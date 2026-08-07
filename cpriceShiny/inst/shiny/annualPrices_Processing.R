# Load libraries
library(openxlsx)
library(readxl)

username <- Sys.getenv("USERNAME")

#Dynamic directory path mapping
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#### ***************************** Process annual commodity prices ********************************************* ####

aPrices <- function(aData) {

  url <- "https://thedocs.worldbank.org/en/doc/74e8be41ceb20fa0da750cda2f6b9e4e-0050012026/related/CMO-Historical-Data-Annual.xlsx"
  destfile <- "CMO-Historical-Data-Annual.xlsx"
  download.file(url, destfile, mode = "wb")
  
  data <- read_excel(destfile, sheet = "Annual Prices (Nominal)")
  
  # Remove unwanted row headings (1 to 5)
  data <- data[-c(1:5), ]
  
  # Replace the column headings with the first record
  colnames(data) <- as.character(unlist(data[1, ]))
  
  # Remove the first row (now used as column names)
  data <- data[-1, ]
  
  #Transpose dataframe to have the years in the columns
  data_t <- t(data)
  
  # Replace the column headings with the first record with is the years record
  colnames(data_t) <- as.character(unlist(data_t[1, ]))
  
  # Step 2: Remove the first row (now used as column names)
  data_t <- data_t[-1, ]
  
  # Move row names into the first column
  data_t <- cbind(Label = rownames(data_t), data_t)
  
  # Clean up row names
  rownames(data_t) <- NULL
  
  # Rename columns 1 and 2
  colnames(data_t)[1:3] <- c("Name", "unit", "code")
  
  # Write final file to csv file
  aData <- data_t
  return(aData)
  
}
