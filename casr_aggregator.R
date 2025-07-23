casr_aggregator <- function(
    ncfile,
    var = NULL,
    fun = NULL,
    time_shift = NULL,
    aggregationLength = NULL,
    aggregationFactor = NULL,
    var_units=NULL,
    aggregate_gph = F)
{
  # Check required packages
  required_pkgs <- c("ncdf4", "abind", "lubridate", "lutz")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "))
  }
  library(ncdf4)
  library(abind)
  library(lubridate)
  library(lutz)
  # Validate input NetCDF file
  if (!file.exists(ncfile)) stop("NetCDF file not found: ", ncfile)
  nc <- tryCatch(nc_open(ncfile), error = function(e) stop("Failed to open NetCDF: ", e$message))
  
  outputfile = "RavenInput.nc"
  
  vars_all <- names(nc$var)
  dims_all <- names(nc$dim)
  
  dims <- nc$dim
  vars_all <- names(nc$var)
  time_dim <- names(dims)[sapply(dims, function(d) grepl("since", d$units, ignore.case = TRUE))]
  space_dims <- unique(c(
    names(dims)[sapply(dims, \(d) grepl("degree", d$units, ignore.case = TRUE))],
    dims_all[grepl("lat|lon", dims_all, ignore.case = TRUE)]
  ))
  if (length(time_dim) != 1 || length(space_dims) < 2) {
    stop("Could not uniquely identify time and at least two spatial dimensions.")
  }
  # Get space dimensions (lat/lon detection)
  if (length(space_dims) < 2) {
    stop("Could not detect two spatial dimensions (lat/lon). Found: ", paste(space_dims, collapse = ", "))
  }
  if(aggregate_gph)
  {
    gp_var<-"CaSR_v3.1_P_GZ_SFC"
    if(!(gp_var %in% names(nc$var))) stop(paste0("No geopotential variable found in the provided NetCDF file: ", gp_var))
  }else{
    gp_var<-NULL
  } 
  # Default aggregation length
  if (missing(aggregationLength) || is.null(aggregationLength)) {
    aggregationLength <- 24
    message("aggregationLength not provided. Defaulting to 24 hours.")
  } else if (!is.numeric(aggregationLength) || aggregationLength <= 0) {
    stop("`aggregationLength` must be a positive numeric value (in hours).")
  }
  
  # Validate time shift
  if(is.null(time_shift)) time_shift<-0
  if (!is.numeric(time_shift) || length(time_shift) != 1) stop("`time_shift` must be a single numeric value (in hours).")

  # Detect all variables with both time and spatial dimensions
  vars_with_time_space <- Filter(function(vname) {
    dims <- sapply(nc$var[[vname]]$dim, function(x) x$name)
    any(grepl("time", dims, ignore.case = TRUE)) &&
      all(sapply(space_dims, function(s) any(grepl(s, dims, ignore.case = TRUE))))
  }, vars_all)
  
  # Default to a  ll vars with time+space if `var` missing
  if (missing(var) || is.null(var)) {
    var <- vars_with_time_space
    message("`var` not provided. Using all variables with time and spatial dimensions: ", paste(var, collapse = ", "))
  }else{
    if(any(!(var %in% vars_with_time_space)))
    {
      message("variables that has no at least one time dimensions and two spatial dimensions are discarded: ", paste(var[!(var %in% vars_with_time_space)], collapse = ", "))
      var<-var[var %in% vars_with_time_space]
    }
  }
  if(is.null(var_units))
  {
    var_units<-unlist(lapply(var,function(v) ncatt_get(nc, v, "units")$value))
    names(var_units)<-var
    message("`var_units` not provided. Using existing units for the selected variables: ", paste0(sprintf("%s [%s]",var,var_units),collapse = ", "))
  }else{
    if(!(is.vector(var_units) & length(var_units)==length(var))) stop("the length of the select variables and the vector of units must be the same!")
    names(var_units)<-var
  }
  
  if(is.null(aggregationFactor))
  {
    aggregationFactor<-rep(1,length(var))
    names(aggregationFactor)<-var
    message("`aggregationFactor` not provided. Using 1 as the factor of aggregation for all variables" )
  }else{
    names(aggregationFactor)<-var
    if(length(aggregationFactor)!=length(var)) stop("the length of the select variables and the vector of aggregation factors must be the same!")
  }
  
  # Ensure provided variables exist
  if (!all(var %in% vars_all)) {
    invalid <- var[!var %in% vars_all]
    stop("The following variables are not in the NetCDF file: ", paste(invalid, collapse = ", "))
  }
  
  # Handle duplicate variables (e.g., "pr", "pr")
  var_table <- table(var)
  if (any(var_table > 1)) {
    dupes <- names(var_table[var_table > 1])
    if (is.null(fun)) {
      stop("Duplicated variables detected (", paste(dupes, collapse = ", "), ") but `fun` not provided.")
    }
  }
  
  # Set up aggregation functions
  if (missing(fun) || is.null(fun))
  {
    if(any(duplicated(var)))
    {
      var<-var[!duplicated(var)]
      message("duplicated variables are discarded when fun is missing!")
    }
    fun<-setNames(rep(list(mean), length(var)), var)
    message("`fun` not provided. Using mean for all variables.")
  } else {
    # Handle unnamed or partial named lists
    if (!any(!is.list(fun) | !is.vector(fun))) stop("`fun` must be a list/vector.")
    # Expand to allow duplicated vars with different functions
    if (length(fun) != length(var)) {
      stop("Length of `fun` list must match `var`. Duplicate `var` entries require corresponding `fun` entries.")
    }
    if(any(!(fun %in% c("min","max","mean","sum")))) stop(sprintf("Invalid function provided: %s", paste0(fun[!(fun %in% c("min","max","mean","sum"))], collapse = ", ")))
    fun<-lapply(fun, match.fun)
    names(fun)<-var
  }
  
  lon_vals <- ncvar_get(nc, ifelse(any(c("lon", "longitude") %in% vars_all), intersect(c("lon","longitude"), vars_all)[1], space_dims[grepl("lon", space_dims, ignore.case=TRUE)][1]))
  lat_vals <- ncvar_get(nc, ifelse(any(c("lat", "latitude") %in% vars_all), intersect(c("lat","latitude"), vars_all)[1], space_dims[grepl("lat", space_dims, ignore.case=TRUE)][1]))
  lon <- mean(lon_vals, na.rm = TRUE); if (lon > 180) lon <- lon - 360
  lat <- mean(lat_vals, na.rm = TRUE)
  timezone <- tryCatch(
    lutz::tz_lookup_coords(lat, lon, method = "accurate"),
    error = function(e) "UTC"
  )
  tvals <- ncvar_get(nc, time_dim)
  tunits <- ncatt_get(nc, time_dim, "units")$value
  mult <- switch(tolower(sub(" since.*", "", tunits)),
                 "seconds" = 1,
                 "minutes" = 60,
                 "hours"   = 3600,
                 "days"    = 86400,
                 stop("Unsupported time unit"))
  origin <- as.POSIXct(sub(".*since ", "", tunits), tz = "UTC")
  times <- as_datetime(origin + tvals * mult - time_shift * 3600, tz = timezone)
  start_time <- floor_date(min(times), unit = paste(aggregationLength, "hours"))
  gidx <- floor(as.numeric(difftime(times, start_time, units = "secs")) / (aggregationLength * 3600))
  times_grouped<-split(times,gidx)
  times_id_grouped<-split(1:length(times),gidx)
  new_times<-rep(times_grouped[[1]][1],length(times_grouped))
  for(t in 2:length(times_grouped)) new_times[t]<-times_grouped[[t]][1]
  group_keys <- sort(unique(gidx))
  new_tvals <- as.numeric(difftime(new_times, new_times[1], units = "secs")) / mult
  if (is.null(var))
  {
    var <- names(nc$var)[sapply(nc$var, function(v) {
      dnames <- sapply(v$dim, `[[`, "name")
      time_dim %in% dnames && all(space_dims %in% dnames)
    })]
  }
  dim_defs <- list()
  for (d in space_dims) dim_defs[[d]] <- ncdim_def(d, dims[[d]]$units, ncvar_get(nc, d))
  dim_defs[[time_dim]] <- ncdim_def(time_dim, tunits, new_tvals, unlim = TRUE)
  var_names<-paste0(sapply(fun, function(f) {
    if (identical(f, min)) "min" else
      if (identical(f, max)) "max" else
        if (identical(f, mean)) "mean" else
          if (identical(f, sum)) "sum" else
            "unknown"}),"_",var)
  names(var_names)<-names(fun)
  var_defs <- lapply(var, function(v)
  {
    vinfo <- nc$var[[v]]
    orig_dim_order <- sapply(vinfo$dim, function(x) x$name)
    out_dims <- lapply(orig_dim_order, function(dn) dim_defs[[dn]])
    chunks <- sapply(orig_dim_order, function(dn)
    {
      dim_len <- length(dim_defs[[dn]]$vals)
      if (dn == time_dim) 1 else min(10, dim_len)
    })
    ncvar_def(
      name = var_names[v],
      units = var_units[v],
      dim = out_dims,
      missval = if (!is.null(vinfo$missval)) vinfo$missval else NA,
      prec = "float",
      chunksizes = chunks,
      compression = 1
    )
  })
  names(var_defs)<-var
  if(!is.null(gp_var))
  {
    gpe_dims<-unlist(lapply(nc$var[[gp_var]]$dim,function(x) x$name))
    gpe_dims<-gpe_dims[!(gpe_dims %in% time_dim)]
    dim_defs_gpe<-dim_defs[match(gpe_dims,names(dim_defs))]
    var_defs$"Geopotential_Elevation" <- ncvar_def(name = "Geopotential_Elevation",
                                                   units = "m",
                                                   longname = "[entire period mean] Geopotential Elevation Above the Sea Levels",
                                                   dim = dim_defs_gpe,
                                                   missval = NaN,
                                                   prec="double")
  }
  
  ncnew <- nc_create(outputfile, var_defs)
  for (i in seq_along(var))
  {
    cat(sprintf("aggregating: %s\n",var[i]))
    var_dims<-unlist(lapply(nc$var[[var[i]]]$dim, function(x) x$name))
    time_dim_id<-match(time_dim,var_dims)
    space_dim_id<-match(space_dims,var_dims)
    names(space_dims)<-space_dim_id
    names(time_dim)<-time_dim_id
    varid<-var_defs[[match(var[i],names(var_defs))]]
    pb_time <- txtProgressBar(min = 0, max = length(times_id_grouped), style = 3)
    for(t in 1:length(times_id_grouped))
    {
      time_start<-min(times_id_grouped[[t]])
      space_start<-c(1,1)
      names(time_start)<-names(time_dim)
      names(space_start)<-names(space_dims)
      start<-c(time_start,space_start)
      start<-start[order(names(start))]
      time_count<-length(times_id_grouped[[t]])
      space_count<-c(nc$dim[[space_dims[1]]]$len,nc$dim[[space_dims[2]]]$len)
      names(time_count)<-names(time_dim)
      names(space_count)<-names(space_dims)
      count<-c(time_count,space_count)
      count<-count[order(names(count))]
      vals<-apply(ncvar_get(nc,var[i],start = start,count = count),1:2,fun[[var[i]]])*aggregationFactor[var[i]]
      count[time_dim_id]<-1
      start[space_dim_id]<-1
      start[time_dim_id]<-t
      ncvar_put(nc = ncnew,varid = varid,vals = vals,start = start,count = count)
      setTxtProgressBar(pb_time, t)  # Update inner progress bar
    }
    close(pb_time)
  }
  if(!is.null(gp_var))
  {
    gp_dims<-unlist(lapply(nc$var[[gp_var]]$dim,function(x) x$name))
    space_dims<-space_dims[order(match(space_dims,gp_dims))]
    gph<-matrix(NA,c(nc$dim[[space_dims[1]]]$len,nc$dim[[space_dims[2]]]$len))
    time_chunks<-round(seq(1,nc$dim[[time_dim]]$len,length.out=floor(prod(nc$var[[gp_var]]$varsize)/10e6)+1))
    var_dims<-unlist(lapply(nc$var[[gp_var]]$dim, function(x) x$name))
    time_dim_id<-match(time_dim,var_dims)
    space_dim_id<-match(space_dims,var_dims)
    names(space_dims)<-space_dim_id
    names(time_dim)<-time_dim_id
    varid<-var_defs[[match(var[i],unlist(lapply(var_defs,function(x) x$name)))]]
    pb_time <- txtProgressBar(min = 0, max = length(time_chunks), style = 3)
    gph_vals<-list()
    cat(sprintf("aggregating: %s\n",gp_var))
    geo2ele<-function(gph)  (gph*10*9.81)*6371000/(9.81*6371000-gph*10*9.81)
    for(t in 1:(length(time_chunks)-1))
    {
      time_start<-time_chunks[t]
      space_start<-c(1,1)
      names(time_start)<-names(time_dim)
      names(space_start)<-names(space_dims)
      start<-c(time_start,space_start)
      start<-start[order(names(start))]
      time_count<-time_chunks[t+1]-time_chunks[t]
      space_count<-c(nc$dim[[space_dims[1]]]$len,nc$dim[[space_dims[2]]]$len)
      names(time_count)<-names(time_dim)
      names(space_count)<-names(space_dims)
      count<-c(time_count,space_count)
      count<-count[order(names(count))]
      gph_vals[[t]]<-apply(ncvar_get(nc,gp_var,start = start,count =count),1:2,mean)
      setTxtProgressBar(pb_time, t)  # Update inner progress bar
    }
    gph<-apply(array(unlist(gph_vals), dim = c(nrow(gph_vals[[1]]), ncol(gph_vals[[1]]), length(gph_vals))),1:2,mean)
    gpe<-geo2ele(gph)
    ncvar_put(nc = ncnew,varid = var_defs$Geopotential_Elevation,vals = gpe)
  }
  ncatt_put(ncnew, 0, "title", "Aggregated NetCDF File")
  ncatt_put(ncnew, 0, "institution", "University of Waterloo")
  ncatt_put(ncnew, 0, "source", "Canadian Surface Reanalysis (CaSR v3.1) from Environment and Climate Change Canada (ECCC)")
  ncatt_put(ncnew, 0, "contact", "rarabzad@uwaterloo.ca")
  ncatt_put(ncnew, 0, "location", "Waterloo, Canada")
  ncatt_put(ncnew, 0, "history", paste("Created on", Sys.time()))
  ncatt_put(ncnew, 0, "Conventions", "CF-1.6")
  nc_close(nc)
  
  writeLines(text = capture.output(ncnew),
             con = file.path(dirname(outputfile), paste0(gsub(".nc","",outputfile),"_content.txt")))
  variableBlocks<-c(":GriddedForcing \t\t\t RavenVarName",
                    "\t:ForcingType \t\t\t RavenForcingType",
                    "\t:FileNameNC \t\t\t netcdf_path",
                    "\t:VarNameNC \t\t\t var_name",
                    "\t:DimNamesNC \t\t\t dims",
                    "\t:ElevationVarNameNC \t\t gpe_var",
                    "\t:RedirectToFile \t\t grid_weights_path",
                    ":EndGriddedForcing\n")
  rvt<-c()
  for(i in 1:(length(var)))
  {
    variableBlocks_tmp<-variableBlocks
    variableBlocks_tmp<-gsub("netcdf_path",outputfile,variableBlocks_tmp)
    variableBlocks_tmp<-gsub("var_name",var[i],variableBlocks_tmp)
    if(!is.null(gp_var))
    {
      variableBlocks_tmp<-gsub("gpe_var","Geopotential_Elevation",variableBlocks_tmp)
    }else{
      variableBlocks_tmp<-variableBlocks_tmp[-grep("gpe_var",variableBlocks_tmp)]
    
    dimNames<-paste0(unlist(lapply(nc$var[[var[i]]]$dim, function(x) x$name)),collapse = " ")
    variableBlocks_tmp<-gsub("dims",dimNames,variableBlocks_tmp)
    rvt<-c(rvt,variableBlocks_tmp)
  }
  writeLines(text = rvt, con = file.path(dirname(outputfile), "model.rvt"), append = file.exists(file.path(dirname(outputfile), "model.rvt")))
  nc_close(ncnew)
  nc_close(nc)
  message("✅ Aggregated NetCDF written to: ", outputfile)
}
