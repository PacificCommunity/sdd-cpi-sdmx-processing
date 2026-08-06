# --------------------------------
# Load libraries
# --------------------------------
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(shinymanager)

# --------------------------------
# USER CREDENTIALS
# --------------------------------
credentials <- data.frame(
  user = c("user"),
  password = c("password"),
  stringsAsFactors = FALSE
)

# --------------------------------
# ALLOWED WORKSHEETS
# --------------------------------
allowed_sheets <- c(
  "bot",
  "imports",
  "exports",
  "reexports",
  "totexports",
  "bot_cty",
  "trade_reg",
  "mode_trspt"
)

# --------------------------------
# UI
# --------------------------------
ui <- fluidPage(
  titlePanel("IMTS Excel to SDMX Converter"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "excel_file",
        label = "Upload IMTS Excel file",
        accept = c(
          ".xlsx",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        )
      ),
      actionButton("process", "Process file"),
      br(), br(),
      downloadButton("download_csv", "Download SDMX CSV")
    ),
    mainPanel(
      verbatimTextOutput("log"),
      tableOutput("preview")
    )
  )
)

# Protect UI with login
ui <- secure_app(ui)

# --------------------------------
# SERVER
# --------------------------------
server <- function(input, output, session) {
  
  # Enforce authentication
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  log_messages <- reactiveVal("")
  final_data <- reactiveVal(NULL)
  
  add_log <- function(msg) {
    log_messages(paste(log_messages(), msg, sep = "\n"))
  }
  
  observeEvent(input$process, {
    req(input$excel_file)
    
    add_log("Starting processing...")
    
    filePath <- input$excel_file$datapath
    wsheets  <- excel_sheets(filePath)
    
    # --------------------------------
    # VALIDATE WORKSHEET NAMES
    # --------------------------------
    invalid_sheets <- setdiff(wsheets, allowed_sheets)
    
    if (length(invalid_sheets) > 0) {
      
      msg <- paste(
        "The following worksheet(s) are not supported:",
        paste(invalid_sheets, collapse = ", "),
        "\n\nAllowed worksheet names are:",
        paste(allowed_sheets, collapse = ", ")
      )
      
      showNotification(
        msg,
        type = "error",
        duration = NULL
      )
      
      add_log("ERROR: Unsupported worksheet(s) detected")
      add_log(msg)
      
      req(FALSE)  # ⛔ Stop execution safely
    }
    
    add_log(paste(
      "Valid worksheets detected:",
      paste(wsheets, collapse = ", ")
    ))
    
    # --------------------------------
    # PROCESS WORKSHEETS
    # --------------------------------
    final_df <- data.frame()
    
    for (sheetname in wsheets) {
      
      df <- read_excel(filePath, sheet = sheetname)
      
      if (sheetname == "bot") {
        
        table_long <- df |>
          pivot_longer(
            cols = -c(DATAFLOW:OBS_COMMENT),
            names_to = "TRADE_FLOW",
            values_to = "OBS_VALUE"
          ) |>
          mutate(TIME_PERIOD = as.character(TIME_PERIOD))
        
        final_df <- table_long
        
      } else if (sheetname %in% c("imports", "exports", "reexports", "totexports")) {
        
        table_long <- df |>
          pivot_longer(
            cols = -c(DATAFLOW:OBS_COMMENT),
            names_to = "COMMODITY",
            values_to = "OBS_VALUE"
          ) |>
          mutate(TIME_PERIOD = as.character(TIME_PERIOD))
        
        final_df <- bind_rows(final_df, table_long)
        
      } else if (sheetname == "bot_cty") {
        
        table_long <- df |>
          pivot_longer(
            cols = -c(DATAFLOW:OBS_COMMENT),
            names_to = "TIME_PERIOD",
            values_to = "OBS_VALUE"
          ) |>
          mutate(
            FREQ = ifelse(nchar(TIME_PERIOD) > 4, "M", "A"),
            TIME_PERIOD = as.character(TIME_PERIOD)
          )
        
        final_df <- bind_rows(final_df, table_long)
        
      } else if (sheetname == "mode_trspt") {
        
        table_long <- df |>
          pivot_longer(
            cols = -c(DATAFLOW:OBS_COMMENT),
            names_to = "TRANSPORT",
            values_to = "OBS_VALUE"
          ) |>
          mutate(TIME_PERIOD = as.character(TIME_PERIOD))
        
        final_df <- bind_rows(final_df, table_long)
        
      } else if (sheetname == "trade_reg") {
        
        table_long <- df |>
          pivot_longer(
            cols = -c(DATAFLOW:OBS_COMMENT),
            names_to = "TIME_PERIOD",
            values_to = "OBS_VALUE"
          ) |>
          mutate(
            FREQ = ifelse(nchar(TIME_PERIOD) > 4, "M", "A"),
            TIME_PERIOD = as.character(TIME_PERIOD)
          )
        
        final_df <- bind_rows(final_df, table_long)
      }
    }
    
    # --------------------------------
    # FINAL CLEANING
    # --------------------------------
    final_df <- final_df |>
      filter(!is.na(OBS_VALUE))
    
    final_df[is.na(final_df)] <- ""
    
    final_df <- final_df |>
      mutate(OBS_VALUE = round(as.numeric(OBS_VALUE), 0)) |>
      select(
        DATAFLOW, FREQ, TIME_PERIOD, GEO_PICT, INDICATOR,
        TRADE_FLOW, COMMODITY, COUNTERPART, TRANSPORT, CURRENCY,
        OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS,
        DATA_SOURCE, OBS_COMMENT
      )
    
    final_data(final_df)
    add_log("Processing completed successfully")
  })
  
  # --------------------------------
  # DOWNLOAD
  # --------------------------------
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("IMTS_sdmx_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(final_data(), file, row.names = FALSE)
    }
  )
  
  # --------------------------------
  # OUTPUTS
  # --------------------------------
  output$preview <- renderTable({
    req(final_data())
    head(final_data(), 10)
  })
  
  output$log <- renderText({
    log_messages()
  })
}

shinyApp(ui, server)