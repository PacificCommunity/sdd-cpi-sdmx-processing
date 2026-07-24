# CPI Data wrangling and SDMX Converter

`cpiShiny` is an R package containing a Shiny application for processing Consumer Price Index (CPI) data.

The application calculates:

\- Monthly CPI indexes

\- Monthly month-on-month inflation

\- Quarterly average CPI indexes

\- Quarterly inflation

\- Annual average CPI indexes

\- Annual inflation


The application also adds the relevant statistics office to the observation comment and generates a final SDMX-compatible CPI dataset.



## Installation

Install the package directly from GitHub.

```r

install.packages("remotes")
remotes::install_github("https://github.com/PacificCommunity/sdd-cpi-sdmx-processing", subdir = "cpiShiny")
library(cpiShiny)
cpiShiny::run_cpi_app()

)

