
# Transforming IMTS tables into SDMX tables

The goal of EXCEL2SDMX package is to allow users to be able to generate SDMX files out from published statistical excel files. 

## Excel file preparations

Before using the EXCEL2SDMX R package, you will need to reorganize your published excel tables to the proper standard format:

### IMTS excel reorganisation

The IMTS excel should be re-organised according steps: 

1. Create an excel file
2. Add the following worksheets in the excel file:
    
    1. **bot** – Balance of trade table  
    2. **imports** – Imports table  
    3. **exports** – Domestic exports table  
    4. **reexports** – Re-exports table  
    5. **bot_cty** – Balance of trade by partner countries table  
    6. **trade_reg** – Trade by region table  
    7. **mode_trspt** – Trade by mode of transport table

#### Example

This is a basic example which shows you how to organize your imts excel worksheet tables:

[Download sample IMTS file](https://github.com/trara538/IMTS2SDMX/blob/main/inst/extdata/sample_IMTS.xlsx)
    

## Installation

You can install and execute the IMTS2SDMX package as per the following steps:

``` r
# **************** How to install the package from R console ******************** #

install.packages("remotes") # Ensure remotes package is installed before proceeding
library(remotes) # load the remotes package
remotes::install_github("https://github.com/trara538/IMTS2SDMX") # Install the IMTS2SDMX package
# if "remotes::install_github("https://github.com/trara538/IMTS2SDMX")" does not work. try the following
remotes::install_github("https://github.com/trara538/IMTS2SDMX", force = TRUE)

# **************** How to run the imtshiny app from R console ******************** #
library(IMTS2SDMX) # Load the IMTS2SDMX package
IMTS2SDMX::run_app() # Execute the IMTS2SDMX package 

```