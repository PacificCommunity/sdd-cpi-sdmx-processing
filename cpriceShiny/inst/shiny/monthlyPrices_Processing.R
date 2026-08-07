# Load libraries
library(openxlsx)
library(readxl)

username <- Sys.getenv("USERNAME")

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#### ***************************** Process annual commodity prices ********************************************* ####

mPrices <- function(aData) {

  url <- "https://thedocs.worldbank.org/en/doc/74e8be41ceb20fa0da750cda2f6b9e4e-0050012026/related/CMO-Historical-Data-Monthly.xlsx"
  
  destfile <- "CMO-Historical-Data-Monthly.xlsx"
  download.file(url, destfile, mode = "wb")
  
  mdata <- read_excel(destfile, sheet = "Monthly Prices")
  
  # Remove unwanted row headings (1 to 5)
  mdata <- mdata[-c(1:3), ]
  
  # Replace the column headings with the first record
  colnames(mdata) <- as.character(unlist(mdata[1, ]))
  
  # Remove the first row (now used as column names)
  mdata <- mdata[-1, ]
  
  #Transpose dataframe to have the years in the columns
  mdata_t <- t(mdata)
  
  # Replace the column headings with the first record with is the years record
  colnames(mdata_t) <- as.character(unlist(mdata_t[1, ]))
  
  # Step 2: Remove the first row (now used as column names)
  mdata_t <- mdata_t[-1, ]
  
  # Move row names into the first column
  mdata_t <- cbind(Label = rownames(mdata_t), mdata_t)
  
  # Clean up row names
  rownames(mdata_t) <- NULL
  
  # Rename columns 1 and 2
  colnames(mdata_t)[1:3] <- c("Name", "unit", "code")
  
  
  # Replace "MO" with "-" in column names (skip first 2 columns: "Name", "unit")
  colnames(mdata_t)[-(1:2)] <- gsub("^([0-9]{4})M([0-9]{2})$", "\\1-\\2", colnames(mdata_t)[-(1:2)])
  
  # Write final file to csv file
  mData <- mdata_t
  return(mData)
  
}
