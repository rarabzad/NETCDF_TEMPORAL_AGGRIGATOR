library(shiny)
library(shinyjs)
library(DT)
library(zip)
library(ncdf4)
library(fields)    
library(viridisLite)
library(lubridate)
library(progress)

# Source the aggregator function
source("https://raw.githubusercontent.com/rarabzad/RDRS/refs/heads/main/scripts/rdrs_ncdf_aggregator.R")
options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB limit
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(HTML("
    .selectize-control.single .selectize-input {
      overflow: hidden !important;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    .selectize-dropdown {
      position: absolute !important;
      width: auto !important;
      min-width: 100% !important;
      max-width: 600px !important;
      z-index: 2000 !important;
    }
    .shiny-input-container { overflow: visible !important; }

    /* make our matrix-input scrollable inside sidebar */
    .matrix-wrapper {
      overflow-x: auto;
      padding-bottom: 5px;
    }
    table.matrix-input {
      width: auto;
      min-width: 100%;
      border-collapse: collapse;
    }
    table.matrix-input th, table.matrix-input td {
      padding: 4px 8px;
      border: 1px solid #ddd;
      white-space: nowrap;
    }
    table.matrix-input th {
      background: #f8f8f8;
      text-align: left;
    }
  "))),
  
  tags$div(
    style = "display: flex; align-items: center; gap: 15px; margin-bottom: 20px;",
    tags$img(src = "logo.png", width = "100px", style = "border-radius: 20px;"),
    tags$h2("RDRS NetCDF Aggregator", style = "margin: 0;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("nc_zip", "Upload NetCDF ZIP Archive", accept = ".zip"),
      numericInput("n_vars", "Number of Variables to Aggregate", 1, min = 1),
      
      # scrollable wrapper around the matrix
      div(class="matrix-wrapper", uiOutput("var_matrix")),
      
      numericInput("time_shift", "Time Shift (hours)", 0),
      numericInput("agg_length", "Aggregation Length (hours)", 24, min = 1),
      checkboxInput("agg_gph", "Aggregate Geopotential", FALSE),
      actionButton("run", "Run Aggregation", icon = icon("play")),
      br(), br(),
      downloadButton("download_results", "Download Results ZIP")
    ),
    
    mainPanel(
      verbatimTextOutput("log"),
      hr(),
      uiOutput("results_tabs")
    )
  )
)

server <- function(input, output, session) {
  log_txt        <- reactiveVal("")
  temp_dir       <- reactiveVal(NULL)
  result_dir     <- reactiveVal(NULL)
  result_zip     <- reactiveVal(NULL)
  available_vars <- reactiveVal(NULL)
  available_units<- reactiveVal(list())
  index_df       <- reactiveVal(NULL)
  busy           <- reactiveVal(FALSE)
  
  
  append_log <- function(msg) {
    isolate(log_txt(paste0(log_txt(), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " – ", msg, "\n")))
  }
  
  # 1) Unzip + discover
  observeEvent(input$nc_zip, {
    req(input$nc_zip)
    append_log("Unzipping archive…")
    td <- tempfile("ncdir_"); dir.create(td)
    unzip(input$nc_zip$datapath, exdir = td)
    temp_dir(td)
    
    ncs <- list.files(td, "\\.nc$", full.names=TRUE, recursive=TRUE)
    if (!length(ncs)) return(append_log("No NetCDF files found."))
    
    nc <- nc_open(ncs[[1]])
    vars <- names(nc$var)
    available_vars(vars)
    available_units(as.list(sapply(vars, function(v){
      att <- ncatt_get(nc, v, "units")$value
      if (is.null(att)) "" else att
    }, USE.NAMES=TRUE)))
    nc_close(nc)
    append_log(sprintf("Found %d files, %d variables.", length(ncs), length(vars)))
  })
  
  # 2) Input matrix
  output$var_matrix <- renderUI({
    req(available_vars(), available_units())
    n <- input$n_vars; vars <- available_vars()
    
    # header row
    hdr <- tags$tr(
      tags$th(""), lapply(seq_len(n), function(i) tags$th(sprintf("Var #%d", i)))
    )
    # each input row
    var_row <- tags$tr(tags$th("Variable"), lapply(seq_len(n), function(i)
      tags$td(selectizeInput(sprintf("var_%d",i), NULL, choices=vars, selected=vars[1]))
    ))
    fun_row <- tags$tr(tags$th("Function"), lapply(seq_len(n), function(i)
      tags$td(selectInput(sprintf("fun_%d",i), NULL, c("sum","mean","min","max"), "mean"))
    ))
    unit_row <- tags$tr(tags$th("Unit"), lapply(seq_len(n), function(i)
      tags$td(textInput(sprintf("unit_%d",i), NULL, value=available_units()[[vars[1]]]))
    ))
    fac_row  <- tags$tr(tags$th("Factor"), lapply(seq_len(n), function(i)
      tags$td(numericInput(sprintf("factor_%d",i), NULL, value=1, min=0, step=0.01))
    ))
    
    tags$table(class="matrix-input",
               tags$thead(hdr),
               tags$tbody(var_row, fun_row, unit_row, fac_row))
  })
  
  # sync units
  observe({
    req(available_units())
    lapply(seq_len(input$n_vars), function(i) {
      observeEvent(input[[paste0("var_",i)]], {
        u <- available_units()[[ input[[paste0("var_",i)]] ]]
        updateTextInput(session, paste0("unit_",i), value = u)
      }, ignoreInit=TRUE)
    })
  })
  
  # 3) Run, zip, load index
  observeEvent(input$run, {
    req(temp_dir(), available_vars())
    
    outdir <- file.path(temp_dir(), "output")
    dir.create(outdir, recursive=TRUE, showWarnings=FALSE)
    
    n    <- input$n_vars
    vars <- vapply(seq_len(n), function(i) input[[sprintf("var_%d",i)]], "")
    fns  <- vapply(seq_len(n), function(i) input[[sprintf("fun_%d",i)]], "")
    us   <- vapply(seq_len(n), function(i) input[[sprintf("unit_%d",i)]], "")
    fs   <- vapply(seq_len(n), function(i) input[[sprintf("factor_%d",i)]], 1)
    
    withProgress(message = "Please wait, aggregation running…", {
      # you can call incProgress() between major steps if you like:
      incProgress(0.1)
      rdrs_ncdf_aggregator(
        ncdir             = temp_dir(),
        time_shift        = input$time_shift,
        aggregationLength = input$agg_length,
        var               = vars,
        var_units         = us,
        fun               = fns,
        aggregationFactor = fs,
        aggregate_gph     = input$agg_gph
      )
      incProgress(0.8)
      
      # load index, zip, etc.
      idxf <- file.path(outdir,"aggregation_procedure.csv")
      if (file.exists(idxf)) index_df(read.csv(idxf))
      zipf <- file.path(temp_dir(),"rdrs_output.zip")
      zip::zip(zipfile=zipf, files=list.files(outdir, full.names=TRUE, recursive=TRUE),
               mode="cherry-pick", root=outdir)
      result_zip(zipf); result_dir(outdir)
      incProgress(0.1)
    })
    
    append_log("Aggregation & zip complete.")
  })
  
  # 4) Average‐over‐time matrices
  avg_matrices <- reactive({
    req(result_dir())
    ncfile <- file.path(result_dir(), "RavenInput.nc")
    nc     <- nc_open(ncfile)
    vns    <- setdiff(names(nc$var), c("lon","lat"))
    mats   <- lapply(vns, function(vn) apply(ncvar_get(nc, vn), 1:2, mean, na.rm=TRUE))
    nc_close(nc)
    list(names=vns, mats=mats)
  })
  
  # render dynamic tabs
  output$results_tabs <- renderUI({
    am <- avg_matrices()
    req(am)
    
    # generate one tabPanel per variable
    plot_tabs <- lapply(seq_along(am$names), function(i) {
      tabPanel(am$names[i],
               plotOutput(paste0("plot_",i), height="500px")
      )
    })
    # the final “Index” tab
    plot_tabs[[length(plot_tabs)+1]] <- tabPanel("Index", DTOutput("index_table"))
    
    do.call(tabsetPanel, c(id="results_tabs", plot_tabs))
  })
  
  # hook up each plotOutput
  observe({
    am <- avg_matrices()
    req(am)
    for (i in seq_along(am$mats)) {
      local({
        ii <- i
        nm <- am$names[ii]
        output[[paste0("plot_",ii)]] <- renderPlot({
          # draw the field with a color‐key legend
          fields::image.plot(
            am$mats[[ii]],
            col        = magma(100),          # warm, high‑contrast
            main       = nm,
            legend.lab = "Mean value",
            axes       = FALSE
          )
          box()
        })
      })
    }
  })
  
  # final outputs
  output$log         <- renderText(log_txt())
  output$index_table <- renderDT({ req(index_df()); datatable(index_df(), rownames=TRUE, options=list(pageLength=5)) })
  
  output$download_results <- downloadHandler(
    filename    = function() paste0("rdrs_output_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip"),
    contentType = "application/zip",
    content     = function(file) {
      req(result_zip())
      file.copy(result_zip(), file, overwrite=TRUE)
    }
  )
}

shinyApp(ui, server)
