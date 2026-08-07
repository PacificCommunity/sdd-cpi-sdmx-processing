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

source("annualPrices_Processing.R")
source("monthlyPrices_Processing.R")


# ============================================================
# UI
# ============================================================

ui <- dashboardPage(
  
  dashboardHeader(
    title = "SDMX Data Processor"
  ),
  
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Commodity Prices",
        tabName = "commodity_prices",
        icon = icon("chart-line")
      )
      
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
            
            
            actionButton(
              "process",
              "Process Commodity Prices",
              icon = icon("play")
            ),
            
            
            br(),
            br(),
            
            
            downloadButton(
              "download",
              "Download CSV",
              icon = icon("download")
            )
            
            
          )
          
        ),
        
        
        
        fluidRow(
          
          box(
            
            title = "Processing Log",
            
            width = 12,
            
            status = "info",
            
            solidHeader = TRUE,
            
            
            verbatimTextOutput(
              "log"
            )
            
          )
          
        ),
        
        
        
        fluidRow(
          
          box(
            
            title = "Preview",
            
            width = 12,
            
            status = "success",
            
            solidHeader = TRUE,
            
            
            DTOutput(
              "preview"
            )
            
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
    
    add_log(
      "Starting commodity price processing..."
    )
    
    
    tryCatch({
      
      
      # ------------------------------------------------------
      # Download annual data
      # ------------------------------------------------------
      
      add_log(
        "Downloading World Bank annual commodity prices..."
      )
      
      
      annual <- aPrices(NULL)
      
      
      
      # ------------------------------------------------------
      # Download monthly data
      # ------------------------------------------------------
      
      add_log(
        "Downloading World Bank monthly commodity prices..."
      )
      
      
      monthly <- mPrices(NULL)
      
      
      
      # ------------------------------------------------------
      # Commodity mapping file
      # ------------------------------------------------------
      
      if(
        file.exists(
          "raw_data/commodity_price/CPriceList.csv"
        )
      ){
        
        cpriceList <-
          read.csv(
            "raw_data/commodity_price/CPriceList.csv"
          )
        
      } else {
        
        cpriceList <-
          read.csv(
            "CPriceList.csv"
          )
        
      }
      
      
      
      # ======================================================
      # Annual processing
      # ======================================================
      
      
      add_log(
        "Processing annual prices..."
      )
      
      
      annual <-
        merge(
          annual,
          cpriceList,
          by="Name"
        )
      
      
      annual <-
        annual[,c(
          ncol(annual),
          1:(ncol(annual)-1)
        )]
      
      
      
      value_columns <-
        colnames(annual)[
          !(colnames(annual) %in%
              c(
                "Id",
                "Name",
                "unit",
                "UNIT_MEASURE",
                "code"
              ))
        ]
      
      
      
      annual_long <- data.frame()
      
      
      
      for(col in value_columns){
        
        annual_long <-
          rbind(
            
            annual_long,
            
            data.frame(
              
              COMMODITY =
                annual$Id,
              
              UNIT_MEASURE =
                annual$UNIT_MEASURE,
              
              TIME_PERIOD =
                col,
              
              OBS_VALUE =
                annual[[col]]
              
            )
            
          )
        
      }
      
      
      
      annual_long$OBS_VALUE <-
        as.numeric(
          annual_long$OBS_VALUE
        )
      
      
      
      annual_long <-
        
        annual_long |>
        
        filter(
          !is.na(OBS_VALUE),
          UNIT_MEASURE != ""
        ) |>
        
        mutate(
          
          DATAFLOW =
            "SPC:DF_COMMODITY_PRICES(1.0)",
          
          FREQ =
            "A",
          
          INDICATOR =
            "COMPRICE",
          
          UNIT_MULT =
            "",
          
          OBS_STATUS =
            "",
          
          DATA_SOURCE =
            "",
          
          OBS_COMMENT =
            ""
          
        )
      
      
      
      
      # ======================================================
      # Monthly processing
      # ======================================================
      
      
      add_log(
        "Processing monthly prices..."
      )
      
      
      monthly <-
        merge(
          monthly,
          cpriceList,
          by="Name"
        )
      
      
      
      monthly <-
        monthly[,c(
          ncol(monthly),
          1:(ncol(monthly)-1)
        )]
      
      
      
      value_columns <-
        colnames(monthly)[
          !(colnames(monthly) %in%
              c(
                "Id",
                "Name",
                "unit",
                "UNIT_MEASURE",
                "code"
              ))
        ]
      
      
      
      monthly_long <- data.frame()
      
      
      
      for(col in value_columns){
        
        monthly_long <-
          rbind(
            
            monthly_long,
            
            data.frame(
              
              COMMODITY =
                monthly$Id,
              
              UNIT_MEASURE =
                monthly$UNIT_MEASURE,
              
              TIME_PERIOD =
                col,
              
              OBS_VALUE =
                monthly[[col]]
              
            )
            
          )
        
      }
      
      
      
      monthly_long$OBS_VALUE <-
        as.numeric(
          monthly_long$OBS_VALUE
        )
      
      
      
      monthly_long <-
        
        monthly_long |>
        
        filter(
          !is.na(OBS_VALUE)
        ) |>
        
        mutate(
          
          DATAFLOW =
            "SPC:DF_COMMODITY_PRICES(1.0)",
          
          FREQ =
            "M",
          
          INDICATOR =
            "COMPRICE",
          
          UNIT_MULT =
            "",
          
          OBS_STATUS =
            "",
          
          DATA_SOURCE =
            "",
          
          OBS_COMMENT =
            ""
          
        )
      
      
      
      
      # ======================================================
      # Combine
      # ======================================================
      
      
      add_log(
        "Combining annual and monthly datasets..."
      )
      
      
      
      final <-
        
        bind_rows(
          annual_long,
          monthly_long
        ) |>
        
        mutate(
          OBS_VALUE =
            round(
              OBS_VALUE,
              2
            )
        ) |>
        
        select(
          
          DATAFLOW,
          FREQ,
          COMMODITY,
          INDICATOR,
          TIME_PERIOD,
          OBS_VALUE,
          UNIT_MEASURE,
          UNIT_MULT,
          OBS_STATUS,
          DATA_SOURCE,
          OBS_COMMENT
          
        )
      
      
      
      result(final)
      
      
      
      add_log(
        paste0(
          "Completed successfully. Records generated: ",
          nrow(final)
        )
      )
      
      
    },
    
    
    error=function(e){
      
      add_log(
        paste0(
          "ERROR: ",
          e$message
        )
      )
      
    })
    
    
  })
  
  
  
  # ----------------------------------------------------------
  # Log
  # ----------------------------------------------------------
  
  output$log <-
    renderText({
      
      log_text()
      
    })
  
  
  
  # ----------------------------------------------------------
  # Preview
  # ----------------------------------------------------------
  
  output$preview <-
    renderDT({
      
      req(result())
      
      
      datatable(
        
        head(
          result(),
          100
        ),
        
        options=list(
          scrollX=TRUE
        )
        
      )
      
      
    })
  
  
  
  # ----------------------------------------------------------
  # Download
  # ----------------------------------------------------------
  
  output$download <-
    downloadHandler(
      
      
      filename=function(){
        
        paste0(
          "DF_COMMODITY_PRICES_",
          Sys.Date(),
          ".csv"
        )
        
      },
      
      
      content=function(file){
        
        write.csv(
          
          result(),
          
          file,
          
          row.names=FALSE
          
        )
        
      }
      
      
    )
  
  
}



# ============================================================
# Run App
# ============================================================

shinyApp(ui,server)