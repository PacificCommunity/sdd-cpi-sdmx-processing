# ============================================================
# Commodity Price SDMX Shiny Processor
# ============================================================

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(readxl)
library(openxlsx)

# ------------------------------------------------------------
# Load processing functions
# ------------------------------------------------------------

#### **** Process annual commodity prices *************** ####

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

#### ******** Process annual commodity prices ******************* ####

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

# ============================================================
# UI section
# ============================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "SDMX Data Processor"
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Commodity Prices", tabName = "commodity_prices", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
      tabItems(
        tabItem(
          tabName = "commodity_prices",
  
        fluidRow(
            box(
              title = "Commodity Price Processing",
              width = 12,
              status = "primary",
              solidHeader = TRUE,
              actionButton("process", "Process Commodity Prices", icon = icon("play")),
              br(),
              br(),
              downloadButton("download", "Download CSV", icon = icon("download"))
          )
        ),
      
        fluidRow(
            box(
              title = "Processing Log",
              width = 12,
              status = "info",
              solidHeader = TRUE,
              verbatimTextOutput("log")
          )
        ),
        
        fluidRow(
            box(
            title = "Preview",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            DTOutput("preview")
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session){
  result <- reactiveVal(NULL)
  log_text <- reactiveVal("")
  
  add_log <- function(message){
    log_text(
      paste0(
        log_text(),
        message,
        "\n"
      )
    )
  }
  
  observeEvent(input$process, {
    log_text("")
    add_log("Starting commodity price processing...")
    
    tryCatch({
      
      # ------------------------------------------------------
      # Download annual data
      # ------------------------------------------------------
      
      add_log("Downloading World Bank annual commodity prices...")
      annual <- aPrices(NULL)
      
      # ------------------------------------------------------
      # Download monthly data
      # ------------------------------------------------------
      
      add_log("Downloading World Bank monthly commodity prices...")
      monthly <- mPrices(NULL)
      
      # ------------------------------------------------------
      # Commodity mapping file
      # ------------------------------------------------------
      
      if(file.exists("CPriceList.csv")){
          cpriceList <-read.csv("CPriceList.csv")
        
      } else {
         cpriceList <- read.csv("CPriceList.csv")
      }
      
      # ======================================================
      # Annual processing
      # ======================================================
      
      add_log("Processing annual prices...")
      annual <- merge(annual, cpriceList, by="Name")
      annual <- annual[,c(ncol(annual), 1:(ncol(annual)-1))]
      value_columns <- colnames(annual)[!(colnames(annual) %in% c("Id", "Name", "unit", "UNIT_MEASURE", "code"))]
      annual_long <- data.frame()
      
      for(col in value_columns){
          annual_long <- rbind(annual_long, data.frame(
                                                      COMMODITY =annual$Id, 
                                                      UNIT_MEASURE = annual$UNIT_MEASURE,
                                                      TIME_PERIOD = col,
                                                      OBS_VALUE = annual[[col]])
          )
      }
      
      annual_long$OBS_VALUE <- as.numeric(annual_long$OBS_VALUE)
      
      annual_long <- annual_long |>
        filter(!is.na(OBS_VALUE), UNIT_MEASURE != "") |>
        mutate(
          DATAFLOW = "SPC:DF_COMMODITY_PRICES(1.0)",
          FREQ = "A",
          INDICATOR = "COMPRICE",
          UNIT_MULT = "",
          OBS_STATUS = "",
          DATA_SOURCE = "",
          OBS_COMMENT = ""
        )
      
      
      # ======================================================
      # Monthly processing
      # ======================================================
      
      add_log("Processing monthly prices...")
      monthly <- merge(monthly, cpriceList, by="Name")
      monthly <- monthly[,c(ncol(monthly), 1:(ncol(monthly)-1))]
      value_columns <- colnames(monthly)[!(colnames(monthly) %in% c("Id", "Name", "unit", "UNIT_MEASURE", "code"))]
      monthly_long <- data.frame()
      
      for(col in value_columns){
          monthly_long <- rbind(monthly_long, data.frame(
                                                      COMMODITY =monthly$Id, 
                                                      UNIT_MEASURE = monthly$UNIT_MEASURE,
                                                      TIME_PERIOD = col,
                                                      OBS_VALUE = monthly[[col]])
          )
      }
      
      monthly_long$OBS_VALUE <- as.numeric(monthly_long$OBS_VALUE)
      
      monthly_long <- monthly_long |>
        filter(!is.na(OBS_VALUE)) |>
        mutate(
              DATAFLOW = "SPC:DF_COMMODITY_PRICES(1.0)",
              FREQ = "M",
              INDICATOR = "COMPRICE",
              UNIT_MULT = "",
              OBS_STATUS = "",
              DATA_SOURCE = "",
              OBS_COMMENT = "")
      
      # ======================================================
      # Combine
      # ======================================================
      
      add_log("Combining annual and monthly datasets...")
      
      final <- bind_rows(annual_long, monthly_long) |>
        mutate(
              OBS_VALUE = round(OBS_VALUE, 2)) |>
        
        select(DATAFLOW, FREQ, COMMODITY, INDICATOR, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT)
      
      result(final)
      
      add_log(paste0("Completed successfully. Records generated: ", nrow(final)))
      
    },

    error=function(e){
      add_log(paste0("ERROR: ", e$message))
    })
  })

  # ----------------------------------------------------------
  # Log
  # ----------------------------------------------------------
  
  output$log <- renderText({
            log_text()
    })

  # ----------------------------------------------------------
  # Preview
  # ----------------------------------------------------------
  
  output$preview <- renderDT({
      req(result())
      datatable(head(result(), 100),
        options=list(scrollX=TRUE)
      )
    })

  # ----------------------------------------------------------
  # Download
  # ----------------------------------------------------------
  
  output$download <-
    downloadHandler(
        filename=function(){
        paste0("DF_COMMODITY_PRICES_", Sys.Date(),".csv")
      },
      
      content=function(file){
          write.csv(result(), file, row.names=FALSE)
      }
    )
}

# ============================================================
# Run App
# ============================================================

shinyApp(ui,server)