# ============================================================
# QUARTERLY CPI DATA PROCESSING SHINY APPLICATION
# ============================================================

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
library(DT)

# ============================================================
# Statistics Offices
# ============================================================

stats_office <- data.frame(
  
  GEO_PICT = c(
    "AS","CK","FJ","FM","GU","KI","MH","MP","NC","NR","NU",
    "PF","PG","PN","PW","SB","TK","TO","TV","VU","WF","WS"
  ),
  
  office = c(
    "Data and Statistics American Samoa Department of Commerce",
    "Cook Islands Statistics Office",
    "Fiji Bureau of Statistics",
    "FSM Statistics",
    "The Bureau of Statistics and Plans - Guam",
    "Kiribati national Statistics Office",
    "Marshall Islands Economic Policy, Planning and Statistics Office (EPPSO)",
    "CNMI Department of Commerce",
    "Institut de la Statistique et des Etudes Economiques",
    "Nauru Bureau of Statistics",
    "Niue Statistics Office",
    "Institut de la statistique de la Polynésie française",
    "PNG National Statistics Office",
    "Pitcairn Statistics office",
    "Palau Statistics Office",
    "Solomon Islands National Statistics Office",
    "Tokelau Statistics Office",
    "Tonga Statistics Department",
    "Tuvalu Statistics Office",
    "Vanuatu Bureau of Statistics Office",
    "Wallis and Futuna Statistics Office",
    "Samoa Bureau of Statistics"
  ),
  
  stringsAsFactors = FALSE
)

# ============================================================
# Quarterly Inflation Calculation
# ============================================================

calc_quarterly_inflation <- function(
    df,
    comment
) {
  
  df |>
    
    arrange(GEO_PICT, COMMODITY, TIME_PERIOD) |>
    group_by(GEO_PICT,COMMODITY) |>
    mutate(OBS_VALUE = (OBS_VALUE /lag(OBS_VALUE) - 1) * 100) |>
    ungroup() |>
    filter(!is.na(OBS_VALUE)) |>
    mutate(FREQ = "Q", INDICATOR = "INF", UNIT_MEASURE = "PERCENT", OBS_STATUS = "E", OBS_COMMENT = comment, OBS_VALUE = round(OBS_VALUE, 1)
    )
}

# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  titlePanel("Quarterly CPI Data Processing Application"),
  sidebarLayout(
    
    # ========================================================
    # SIDEBAR
    # ========================================================
    
    sidebarPanel(
      h4("1. Upload CPI Data"),
      fileInput(inputId = "cpi_file", label = "Select quarterly CPI Excel file:", accept = c(".xlsx", ".xls")),
      helpText(
        paste("The Excel file must contain a worksheet", "named 'cpi_data'.")),
      helpText(paste("TIME_PERIOD must be in the format", "'YYYY-Q1', 'YYYY-Q2', 'YYYY-Q3' or 'YYYY-Q4'.")),
      hr(),
      h4("2. Process Data"),
      actionButton(inputId = "process", label = "Process CPI Data", icon = icon("play"), class = "btn-primary"),
      hr(),
      h4("3. Download"),
      downloadButton(outputId = "download_csv", label = "Download Final CSV", class = "btn-success"),
      hr(),
      h4("Processing Status"),
      verbatimTextOutput("status")),
    
    # ========================================================
    # MAIN PANEL
    # ========================================================
    
    mainPanel(
      tabsetPanel(
        
        # ====================================================
        # SUMMARY
        # ====================================================
        
        tabPanel(title = "Summary", br(),
                 fluidRow(
                   column(width = 3,
                          wellPanel(h4("Rows"), textOutput("n_rows"))),
                   column(width = 3,
                          wellPanel(h4("Countries"), textOutput("n_countries"))),
                   column(width = 3,
                          wellPanel(h4("Commodities"), textOutput("n_commodities"))),
                   column(width = 3,
                          wellPanel(h4("Indicators"), textOutput("n_indicators")))),
                 hr(),
                 h4("Indicator Summary"),
                 DTOutput("indicator_summary")
        ),
        
        # ====================================================
        # FINAL DATA
        # ====================================================
        
        tabPanel(
          title = "Final Data",
          br(),
          DTOutput("final_data")
        ),
        
        # ====================================================
        # QUARTERLY CPI
        # ====================================================
        
        tabPanel(
          title ="Quarterly CPI",
          br(),
          DTOutput("quarterly_data")
        ),
        
        # ====================================================
        # QUARTERLY INFLATION
        # ====================================================
        
        tabPanel(
          title = "Quarterly Inflation",
          br(),
          DTOutput("quarterly_inflation_data")
        ),
        
        # ====================================================
        # ANNUAL CPI
        # ====================================================
        
        tabPanel(
          title = "Annual CPI",
          br(),
          DTOutput("annual_data")
        ),
        
        # ====================================================
        # ANNUAL INFLATION
        # ====================================================
        
        tabPanel(
          title = "Annual Inflation",
          br(),
          DTOutput("annual_inflation_data")
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ==========================================================
  # REACTIVE VALUES
  # ==========================================================
  
  processed_data <- reactiveVal(NULL)
  quarterly_data <- reactiveVal(NULL)
  quarterly_inflation_data <- reactiveVal(NULL)
  annual_data <- reactiveVal(NULL)
  annual_inflation_data <- reactiveVal(NULL)
  
  # ==========================================================
  # PROCESS CPI DATA
  # ==========================================================
  
  observeEvent(
    input$process,
    {
      req(input$cpi_file)
      
      # ======================================================
      # PROGRESS
      # ======================================================
      
      withProgress(
        message = "Processing quarterly CPI data...",
        value = 0,
        {
          
          # ==================================================
          # READ EXCEL FILE
          # ==================================================
          
          incProgress(0.10,
                      detail = "Reading Excel file...")
          cpi_data <-
            read_excel(input$cpi_file$datapath, sheet = "cpi_data") |>
            pivot_longer(
              cols = -c(DATAFLOW:OBS_COMMENT),
              names_to = "COMMODITY",
              values_to = "OBS_VALUE"
            )
          
          # ==================================================
          # CHECK REQUIRED COLUMNS
          # ==================================================
          
          incProgress(0.05,
                      detail ="Checking input data...")
          
          required_columns <- c("DATAFLOW", "GEO_PICT", "INDICATOR", "COMMODITY", "TIME_PERIOD", "OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "OBS_STATUS", "BASE_PER", "OBS_COMMENT")
          missing_columns <- setdiff(required_columns, names(cpi_data))
          
          if (length(missing_columns) > 0) {
            stop(paste("The following required columns", "are missing from the Excel file:",
                       paste(missing_columns, collapse = ", ")
            )
            )
          }
          
          # ==================================================
          # PREPARE QUARTERLY CPI DATA
          # ==================================================
          
          incProgress(0.15,
                      detail = "Preparing quarterly CPI data...")
          
          cpi <- cpi_data |>
            mutate(
              OBS_VALUE = as.numeric(OBS_VALUE),
              TIME_PERIOD = as.character(TIME_PERIOD),
              Year = as.numeric(substr(TIME_PERIOD, 1,4)),
              Quarter = as.numeric(substr(TIME_PERIOD, 7, 7)),
              OBS_COMMENT = "Quarterly consumer price indexes sourced from ") |>
            filter(!is.na(OBS_VALUE))
          
          # ==================================================
          # VALIDATE TIME PERIOD
          # ==================================================
          
          invalid_periods <- cpi |>
            filter(is.na(Year) | is.na(Quarter) | !Quarter %in% c(1,2,3,4))
          
          if (nrow(invalid_periods) > 0) {
            stop(paste("Invalid TIME_PERIOD values detected.", "Expected format is YYYY-Q1 to YYYY-Q4."))
          }
          
          # ==================================================
          # QUARTERLY CPI
          # ==================================================
          
          incProgress(0.10, detail = "Preparing quarterly CPI...")
          quarterly_cpi <- cpi |>
            mutate(FREQ ="Q",
                   INDICATOR = "IDX",
                   OBS_STATUS = "E",
                   OBS_COMMENT = "Quarterly consumer price indexes sourced from ") |>
            
            select(DATAFLOW, FREQ, GEO_PICT, INDICATOR, COMMODITY, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, BASE_PER, OBS_COMMENT) |>
            mutate(OBS_VALUE = round(OBS_VALUE, 1))
          
          # ==================================================
          # QUARTERLY INFLATION
          # ==================================================
          
          incProgress(0.15, detail = "Calculating quarterly inflation...")
          quarterly_inflation <- calc_quarterly_inflation(quarterly_cpi, "Quarterly inflation calculated from quarterly indexes sourced from ")
          
          # ==================================================
          # ANNUAL CPI
          # ==================================================
          
          incProgress(0.15, detail = "Calculating annual CPI...")
          
          annual_cpi <- cpi |>
            group_by(DATAFLOW, GEO_PICT, COMMODITY, BASE_PER, UNIT_MEASURE, UNIT_MULT, Year) |>
            summarise(Quarters = n_distinct(Quarter), OBS_VALUE = ifelse(Quarters == 4, mean(OBS_VALUE, na.rm = TRUE), NA_real_),
                      .groups = "drop")|>
            
            filter(Quarters == 4)
          
          # ==================================================
          # ANNUAL CPI OUTPUT
          # ==================================================
          
          incProgress(0.10, detail = "Preparing annual CPI...")
          
          annual_cpi_out <- annual_cpi |>
            mutate(FREQ = "A",
                   INDICATOR = "IDX",
                   OBS_STATUS = "E",
                   OBS_COMMENT = "Annual average indexes calculated from quarterly indexes sourced from ",
                   TIME_PERIOD = as.character(Year)) |>
            select(DATAFLOW, FREQ, GEO_PICT, INDICATOR, COMMODITY, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE,UNIT_MULT, OBS_STATUS, BASE_PER, OBS_COMMENT) |>
            mutate(OBS_VALUE = round(OBS_VALUE, 1))
          
          # ==================================================
          # ANNUAL INFLATION
          # ==================================================
          
          incProgress(0.10, detail = "Calculating annual inflation...")
          
          annual_inflation_out <- annual_cpi_out |>
            arrange(GEO_PICT, COMMODITY, TIME_PERIOD) |>
            group_by(GEO_PICT, COMMODITY) |>
            mutate(OBS_VALUE = (OBS_VALUE / lag(OBS_VALUE) - 1) * 100) |>
            ungroup() |>
            filter(!is.na(OBS_VALUE)) |>
            mutate(FREQ = "A", INDICATOR = "INF", UNIT_MEASURE = "PERCENT", OBS_STATUS = "E", OBS_COMMENT = "Annual inflation calculated from annual average quarterly indexes sourced from ", OBS_VALUE = round(OBS_VALUE, 1)) |>
            select(DATAFLOW, FREQ, GEO_PICT, INDICATOR, COMMODITY, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, BASE_PER, OBS_COMMENT)
          
          # ==================================================
          # COMBINE ALL DATA
          # ==================================================
          
          incProgress(0.05, detail =  "Combining datasets...")
          
          combined <- bind_rows(quarterly_cpi, quarterly_inflation, annual_cpi_out, annual_inflation_out)
          
          # ==================================================
          # ADD STATISTICS OFFICE
          # ==================================================
          
          incProgress(0.05, detail = "Adding statistics office names...")
          
          combined <- combined |>
            left_join(stats_office, by = "GEO_PICT") |>
            filter(!is.na(OBS_VALUE)) |>
            mutate(across(everything(), ~ if_else(is.na(.x),"", as.character(.x))),
                   OBS_VALUE = round(as.numeric(OBS_VALUE), 1),
                   OBS_COMMENT = ifelse(OBS_COMMENT != "", paste0(OBS_COMMENT," ",office), "")) |>
            select(-office, DATAFLOW, FREQ, GEO_PICT, INDICATOR, COMMODITY, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, BASE_PER, OBS_COMMENT)
          
          # ==================================================
          # SAVE REACTIVE DATA
          # ==================================================
          
          processed_data(combined)
          
          quarterly_data(quarterly_cpi)
          
          quarterly_inflation_data(quarterly_inflation)
          
          annual_data(annual_cpi_out)
          
          annual_inflation_data(annual_inflation_out)
          
          incProgress(0.05, detail = "Processing complete!")
        }
      )
    }
  )
  
  # ==========================================================
  # SUMMARY OUTPUTS
  # ==========================================================
  
  output$n_rows <- renderText({
    req(processed_data())
    format(nrow(processed_data()), big.mark = ",")
  })
  
  output$n_countries <- renderText({
    req(processed_data())
    n_distinct(processed_data()$GEO_PICT)
  })
  
  output$n_commodities <- renderText({
    req(processed_data())
    n_distinct(processed_data()$COMMODITY)
  })
  
  output$n_indicators <- renderText({
    req(processed_data())
    n_distinct(processed_data()$INDICATOR)
  })
  
  # ==========================================================
  # INDICATOR SUMMARY
  # ==========================================================
  
  output$indicator_summary <- renderDT({
    req(processed_data())
    processed_data() |>
      count(FREQ, INDICATOR,
            name = "Number of Observations")
    
  },
  
  options = list(pageLength = 10, scrollX = TRUE)
  )
  
  # ==========================================================
  # FINAL DATA TABLE
  # ==========================================================
  
  output$final_data <-    renderDT({
    req(processed_data())
    datatable(processed_data(), filter = "top", extensions = "Buttons", options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
  })
  
  # ==========================================================
  # QUARTERLY CPI TABLE
  # ==========================================================
  
  output$quarterly_data <- renderDT({
    
    req(quarterly_data())
    datatable(quarterly_data(), filter = "top", options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # ==========================================================
  # QUARTERLY INFLATION TABLE
  # ==========================================================
  
  output$quarterly_inflation_data <- renderDT({
    
    req(quarterly_inflation_data())
    datatable(quarterly_inflation_data(), filter =  "top", options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # ==========================================================
  # ANNUAL CPI TABLE
  # ==========================================================
  
  output$annual_data <- renderDT({
    req(annual_data())
    datatable(annual_data(), filter = "top", options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # ==========================================================
  # ANNUAL INFLATION TABLE
  # ==========================================================
  
  output$annual_inflation_data <- renderDT({
    
    req(annual_inflation_data())
    datatable(annual_inflation_data(), filter = "top", options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # ==========================================================
  # STATUS
  # ==========================================================
  
  output$status <- renderText({
    if (is.null(processed_data())) {
      "Waiting for quarterly CPI Excel file..."
    } else {
      paste("Processing completed successfully.", "\n",
            "The final quarterly CPI dataset is ready for download."
      )
    }
  })
  
  # ==========================================================
  # DOWNLOAD CSV
  # ==========================================================
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("DF_CPI-quarterly-data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    
    content = function(file) {
      req(processed_data())
      write.csv(processed_data(), file, row.names = FALSE, na = "")
    }
  )
}

# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(ui = ui, server = server)
