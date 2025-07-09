# RDRS NetCDF Aggregator

![Shiny App](https://img.shields.io/badge/built%20with-shiny-FF69B4.svg)
![R](https://img.shields.io/badge/R-4.3.1-blue)

## Overview

The **RDRS NetCDF Aggregator** is an interactive [Shiny](https://shiny.posit.co/) web application designed to process and aggregate **hourly NetCDF datasets from Environment and Climate Change Canada's Regional Deterministic Reforecast System (RDRS v2.1)**. It allows users to upload zipped NetCDF files, select variables, apply aggregation functions (e.g., mean, sum), and download coarsened time-resolution output NetCDF files along with Raven-compatible `.rvt` files.

This tool was developed to streamline the generation of daily, weekly, or custom temporal aggregations for hydrologic and climate modeling purposes.

---

## Features

- 📦 Upload zipped folders of hourly RDRS NetCDF files
- 📊 Select multiple variables and aggregation functions
- 🕒 Apply custom time shifts and aggregation lengths
- 🧮 Generates coarsened time resolution NetCDF output
- 📥 Download results as a ZIP including `.nc`, `.csv`, `.txt`, and `.rvt` files
- 🌐 View mean field plots interactively inside the browser

---

## Installation

To run this Shiny app locally, make sure you have R (>= 4.1.0) and the following packages installed:

```r
install.packages(c("shiny", "shinyjs", "DT", "zip", "ncdf4", "fields", "viridisLite"))
````

Clone this repository and open `app.R` in RStudio or R console:

```r
git clone https://github.com/rarabzad/RDRS.git
cd RDRS
```

Then run the app:

```r
shiny::runApp("app.R")
```

---

## Usage

### 1. Upload Your Data

* Click "Upload NetCDF ZIP Archive" and select your `.zip` file containing hourly RDRS `.nc` files.
* Files should follow the naming pattern: `YYYYMMDD12.nc` (e.g., `2015060112.nc`)

### 2. Configure Aggregation

* Select the number of variables to aggregate
* For each variable:

  * Choose the variable name
  * Set the aggregation function (`sum`, `mean`, `min`, `max`)
  * Optionally adjust units or scale factor
* Set the time shift (in hours)
* Choose the aggregation length (e.g., 24 for daily)
* Optionally aggregate the geopotential variable

### 3. Run and Download

* Click **"Run Aggregation"**
* Once processing is complete, download the output using **"Download Results ZIP"**

### 4. Visual Inspection

* The main panel will display log messages and mean-field plots for each variable

---

## Outputs

After aggregation, the following files are included in the downloadable zip:

| File                        | Description                                       |
| --------------------------- | ------------------------------------------------- |
| `RavenInput.nc`             | Aggregated NetCDF output                          |
| `aggregation_procedure.csv` | Log of input file processing and time mapping     |
| `model.rvt`                 | Raven forcing configuration for the new variables |
| `RavenInput_content.txt`    | Metadata of NetCDF variables                      |

---

## How It Works

This app wraps around the [`rdrs_ncdf_aggregator()`](https://github.com/rarabzad/RDRS/blob/main/scripts/rdrs_ncdf_aggregator.R) function, which performs the following steps:

1. **Reads and verifies** all NetCDF files in the uploaded archive
2. **Extracts and aggregates** specified variables across time using the desired function
3. **Handles time alignment and shifting** based on the user input
4. **Creates output NetCDF**, including metadata and derived variables
5. **Generates helper files** like `.csv`, `.rvt`, and `.txt` to support further modeling

The aggregation supports multiple variables with different functions and applies a user-defined scaling factor (e.g., convert geopotential to elevation).

---

## Docker Deployment (Optional)

You can run this app in a Docker container (requires Docker installed):

```bash
docker build -t rdrs-aggregator .
docker run -p 3838:3838 rdrs-aggregator
```

Then open your browser at [http://localhost:3838](http://localhost:3838)

---

## Links

* 🔗 [Live Demo](https://raven-rdrs-aggrigator.share.connect.posit.cloud))
* 🔗 [Function Script](https://github.com/rarabzad/RDRS/blob/main/scripts/rdrs_ncdf_aggregator.R)

