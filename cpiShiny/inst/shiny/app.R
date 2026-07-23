# ============================================================
# CPI DATA PROCESSING SHINY APPLICATION
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
# Inflation Calculation Function
# ============================================================

calc_inflation <- function(df, comment) {
  
  df |>
    arrange(GEO_PICT, COMMODITY, TIME_PERIOD) |>
    group_by(GEO_PICT, COMMODITY) |>
    mutate(
      OBS_VALUE = (OBS_VALUE / lag(OBS_VALUE) - 1) * 100
    ) |>
    ungroup() |>
    filter(!is.na(OBS_VALUE)) |>
    mutate(
      INDICATOR = "INF",
      UNIT_MEASURE = "PERCENT",
      OBS_STATUS = "E",
      OBS_COMMENT = comment,
      OBS_VALUE = round(OBS_VALUE, 1)
    )
}


# ============================================================
# UI
# ============================================================

ui <- fluidPage(
  
  titlePanel("CPI Data Processing Application"),
  
  sidebarLayout(
    
    # --------------------------------------------------------
    # Sidebar
    # --------------------------------------------------------
    
    sidebarPanel(
      
      h4("1. Upload CPI Data"),
      
      fileInput(
        inputId = "cpi_file",
        label = "Select CPI Excel file:",
        accept = c(".xlsx", ".xls")
      ),
      
      helpText(
        "The Excel file must contain a worksheet named 'cpi_data'."
      ),
      
      hr(),
      
      h4("2. Process Data"),
      
      actionButton(
        inputId = "process",
        label = "Process CPI Data",
        icon = icon("play"),
        class = "btn-primary"
      ),
      
      hr(),
      
      h4("3. Download"),
      
      downloadButton(
        outputId = "download_csv",
        label = "Download Final CSV",
        class = "btn-success"
      ),
      
      hr(),
      
      h4("Processing Status"),
      
      verbatimTextOutput("status")
    ),
    
    
    # --------------------------------------------------------
    # Main Panel
    # --------------------------------------------------------
    
    mainPanel(
      
      tabsetPanel(
        
        # ----------------------------------------------------
        # Summary Tab
        # ----------------------------------------------------
        
        tabPanel(
          title = "Summary",
          
          br(),
          
          fluidRow(
            
            column(
              width = 3,
              wellPanel(
                h4("Rows"),
                textOutput("n_rows")
              )
            ),
            
            column(
              width = 3,
              wellPanel(
                h4("Countries"),
                textOutput("n_countries")
              )
            ),
            
            column(
              width = 3,
              wellPanel(
                h4("Commodities"),
                textOutput("n_commodities")
              )
            ),
            
            column(
              width = 3,
              wellPanel(
                h4("Indicators"),
                textOutput("n_indicators")
              )
            )
          ),
          
          hr(),
          
          h4("Indicator Summary"),
          
          DTOutput("indicator_summary")
        ),
        
        
        # ----------------------------------------------------
        # Data Preview
        # ----------------------------------------------------
        
        tabPanel(
          title = "Final Data",
          
          br(),
          
          DTOutput("final_data")
        ),
        
        
        # ----------------------------------------------------
        # Monthly Inflation
        # ----------------------------------------------------
        
        tabPanel(
          title = "Monthly Inflation",
          
          br(),
          
          DTOutput("monthly_data")
        ),
        
        
        # ----------------------------------------------------
        # Quarterly CPI
        # ----------------------------------------------------
        
        tabPanel(
          title = "Quarterly CPI",
          
          br(),
          
          DTOutput("quarterly_data")
        ),
        
        
        # ----------------------------------------------------
        # Annual CPI
        # ----------------------------------------------------
        
        tabPanel(
          title = "Annual CPI",
          
          br(),
          
          DTOutput("annual_data")
        )
      )
    )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Reactive values
  # ----------------------------------------------------------
  
  processed_data <- reactiveVal(NULL)
  
  monthly_data <- reactiveVal(NULL)
  
  quarterly_data <- reactiveVal(NULL)
  
  annual_data <- reactiveVal(NULL)
  
  
  # ----------------------------------------------------------
  # Process CPI data
  # ----------------------------------------------------------
  
  observeEvent(input$process, {
    
    req(input$cpi_file)
    
    
    # --------------------------------------------------------
    # Show progress
    # --------------------------------------------------------
    
    withProgress(
      
      message = "Processing CPI data...",
      value = 0,
      
      {
        
        # ----------------------------------------------------
        # Read Excel file
        # ----------------------------------------------------
        
        incProgress(
          0.10,
          detail = "Reading Excel file..."
        )
        
        cpi_data <- read_excel(
          input$cpi_file$datapath,
          sheet = "cpi_data"
        ) |>
          pivot_longer(
            cols = -c(DATAFLOW:OBS_COMMENT),
            names_to = "COMMODITY",
            values_to = "OBS_VALUE"
          )
        
        
        # ----------------------------------------------------
        # Convert CPI data
        # ----------------------------------------------------
        
        incProgress(
          0.10,
          detail = "Preparing CPI data..."
        )
        
        cpi <- cpi_data |>
          mutate(
            OBS_VALUE = as.numeric(OBS_VALUE),
            Date = as.Date(
              paste0(TIME_PERIOD, "-01")
            ),
            Year = year(Date),
            Quarter = quarter(Date),
            OBS_COMMENT =
              "Monthly consumer price indexes sourced from "
          )
        
        
        # ----------------------------------------------------
        # Monthly Inflation
        # ----------------------------------------------------
        
        incProgress(
          0.15,
          detail = "Calculating monthly inflation..."
        )
        
        monthly_inflation <- calc_inflation(
          cpi,
          "Monthly inflation calculated from average monthly indexes sourced from "
        )
        
        
        # ----------------------------------------------------
        # Quarterly CPI
        # ----------------------------------------------------
        
        incProgress(
          0.15,
          detail = "Calculating quarterly CPI..."
        )
        
        quarterly_cpi <- cpi |>
          group_by(
            DATAFLOW,
            GEO_PICT,
            COMMODITY,
            BASE_PER,
            UNIT_MEASURE,
            UNIT_MULT,
            OBS_STATUS,
            OBS_COMMENT,
            Year,
            Quarter
          ) |>
          summarise(
            Months = sum(!is.na(OBS_VALUE)),
            
            OBS_VALUE = ifelse(
              Months == 3,
              mean(
                OBS_VALUE,
                na.rm = TRUE
              ),
              NA_real_
            ),
            
            .groups = "drop"
          ) |>
          filter(
            Months == 3
          ) |>
          mutate(
            FREQ = "Q",
            INDICATOR = "IDX",
            OBS_STATUS = "E",
            
            OBS_COMMENT =
              "Quarterly average indexes calculated from average monthly indexes sourced from ",
            
            TIME_PERIOD =
              paste0(
                Year,
                "-Q",
                Quarter
              )
          ) |>
          select(
            DATAFLOW,
            FREQ,
            GEO_PICT,
            INDICATOR,
            COMMODITY,
            TIME_PERIOD,
            OBS_VALUE,
            UNIT_MEASURE,
            UNIT_MULT,
            OBS_STATUS,
            BASE_PER,
            OBS_COMMENT
          ) |>
          mutate(
            OBS_VALUE = round(
              OBS_VALUE,
              1
            )
          )
        
        
        # ----------------------------------------------------
        # Quarterly Inflation
        # ----------------------------------------------------
        
        incProgress(
          0.10,
          detail = "Calculating quarterly inflation..."
        )
        
        quarterly_inflation <- calc_inflation(
          quarterly_cpi,
          "Quarterly average inflation calculated from average monthly indexes sourced from "
        )
        
        
        # ----------------------------------------------------
        # Annual CPI
        # ----------------------------------------------------
        
        incProgress(
          0.10,
          detail = "Calculating annual CPI..."
        )
        
        annual_cpi <- quarterly_cpi |>
          mutate(
            Year = substr(
              TIME_PERIOD,
              1,
              4
            )
          ) |>
          group_by(
            DATAFLOW,
            GEO_PICT,
            COMMODITY,
            BASE_PER,
            UNIT_MEASURE,
            UNIT_MULT,
            Year
          ) |>
          summarise(
            
            Quarters = n(),
            
            OBS_VALUE = ifelse(
              Quarters == 4,
              mean(
                OBS_VALUE
              ),
              NA_real_
            ),
            
            .groups = "drop"
          ) |>
          filter(
            Quarters == 4
          )
        
        
        # ----------------------------------------------------
        # Annual CPI Output
        # ----------------------------------------------------
        
        incProgress(
          0.05,
          detail = "Preparing annual CPI..."
        )
        
        annual_cpi_out <- annual_cpi |>
          mutate(
            FREQ = "A",
            INDICATOR = "IDX",
            OBS_STATUS = "E",
            
            OBS_COMMENT =
              "Annual average indexes calculated from average monthly indexes sourced from ",
            
            TIME_PERIOD =
              as.character(
                Year
              )
          ) |>
          select(
            DATAFLOW,
            FREQ,
            GEO_PICT,
            INDICATOR,
            COMMODITY,
            TIME_PERIOD,
            OBS_VALUE,
            UNIT_MEASURE,
            UNIT_MULT,
            OBS_STATUS,
            BASE_PER,
            OBS_COMMENT
          )
        
        
        # ----------------------------------------------------
        # Annual Inflation
        # ----------------------------------------------------
        
        incProgress(
          0.05,
          detail = "Calculating annual inflation..."
        )
        
        annual_inflation_out <- calc_inflation(
          annual_cpi_out,
          "Annual average inflation calculated from quarterly average indexes sourced from "
        )
        
        
        # ----------------------------------------------------
        # Combine all data
        # ----------------------------------------------------
        
        incProgress(
          0.05,
          detail = "Combining datasets..."
        )
        
        combined <- bind_rows(
          
          cpi |>
            select(
              DATAFLOW,
              FREQ,
              GEO_PICT,
              INDICATOR,
              COMMODITY,
              TIME_PERIOD,
              OBS_VALUE,
              UNIT_MEASURE,
              UNIT_MULT,
              OBS_STATUS,
              BASE_PER,
              OBS_COMMENT
            ),
          
          monthly_inflation,
          
          quarterly_cpi,
          
          quarterly_inflation,
          
          annual_cpi_out,
          
          annual_inflation_out
        )
        
        
        # ----------------------------------------------------
        # Add Statistics Office
        # ----------------------------------------------------
        
        incProgress(
          0.05,
          detail = "Adding statistics office names..."
        )
        
        combined <- combined |>
          left_join(
            stats_office,
            by = "GEO_PICT"
          ) |>
          filter(
            !is.na(OBS_VALUE)
          ) |>
          mutate(
            
            across(
              everything(),
              ~ if_else(
                is.na(.x),
                "",
                as.character(.x)
              )
            ),
            
            OBS_VALUE =
              round(
                as.numeric(
                  OBS_VALUE
                ),
                1
              ),
            
            OBS_COMMENT =
              ifelse(
                OBS_COMMENT != "",
                paste0(
                  OBS_COMMENT,
                  " ",
                  office
                ),
                ""
              )
          ) |>
          select(
            -c(
              "Date",
              "Year",
              "Quarter",
              "office"
            ),
            
            DATAFLOW,
            FREQ,
            GEO_PICT,
            INDICATOR,
            COMMODITY,
            TIME_PERIOD,
            OBS_VALUE,
            UNIT_MEASURE,
            UNIT_MULT,
            OBS_STATUS,
            BASE_PER,
            OBS_COMMENT
          )
        
        
        # ----------------------------------------------------
        # Save reactive data
        # ----------------------------------------------------
        
        processed_data(
          combined
        )
        
        monthly_data(
          monthly_inflation
        )
        
        quarterly_data(
          quarterly_cpi
        )
        
        annual_data(
          annual_cpi_out
        )
        
        incProgress(
          0.05,
          detail = "Processing complete!"
        )
      }
    )
  })
  
  
  # ==========================================================
  # Summary Outputs
  # ==========================================================
  
  
  output$n_rows <- renderText({
    
    req(processed_data())
    
    format(
      nrow(
        processed_data()
      ),
      big.mark = ","
    )
  })
  
  
  output$n_countries <- renderText({
    
    req(processed_data())
    
    n_distinct(
      processed_data()$GEO_PICT
    )
  })
  
  
  output$n_commodities <- renderText({
    
    req(processed_data())
    
    n_distinct(
      processed_data()$COMMODITY
    )
  })
  
  
  output$n_indicators <- renderText({
    
    req(processed_data())
    
    n_distinct(
      processed_data()$INDICATOR
    )
  })
  
  
  # ==========================================================
  # Indicator Summary
  # ==========================================================
  
  output$indicator_summary <- renderDT({
    
    req(processed_data())
    
    processed_data() |>
      count(
        FREQ,
        INDICATOR,
        name = "Number of Observations"
      )
    
  },
  
  options = list(
    pageLength = 10,
    scrollX = TRUE
  ))
  
  
  # ==========================================================
  # Final Data Table
  # ==========================================================
  
  output$final_data <- renderDT({
    
    req(processed_data())
    
    datatable(
      processed_data(),
      
      filter = "top",
      
      extensions = "Buttons",
      
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c(
          "copy",
          "csv",
          "excel"
        )
      )
    )
  })
  
  
  # ==========================================================
  # Monthly Inflation Table
  # ==========================================================
  
  output$monthly_data <- renderDT({
    
    req(monthly_data())
    
    datatable(
      monthly_data(),
      
      filter = "top",
      
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # Quarterly CPI Table
  # ==========================================================
  
  output$quarterly_data <- renderDT({
    
    req(quarterly_data())
    
    datatable(
      quarterly_data(),
      
      filter = "top",
      
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # Annual CPI Table
  # ==========================================================
  
  output$annual_data <- renderDT({
    
    req(annual_data())
    
    datatable(
      annual_data(),
      
      filter = "top",
      
      options = list(
        pageLength = 25,
        scrollX = TRUE
      )
    )
  })
  
  
  # ==========================================================
  # Status
  # ==========================================================
  
  output$status <- renderText({
    
    if (
      is.null(
        processed_data()
      )
    ) {
      
      "Waiting for CPI Excel file..."
      
    } else {
      
      paste(
        "Processing completed successfully.",
        "\n",
        "The final dataset is ready for download."
      )
    }
  })
  
  
  # ==========================================================
  # Download CSV
  # ==========================================================
  
  output$download_csv <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "DF_CPI-data_",
        format(
          Sys.time(),
          "%Y%m%d_%H%M%S"
        ),
        ".csv"
      )
    },
    
    content = function(file) {
      
      req(
        processed_data()
      )
      
      write.csv(
        processed_data(),
        file,
        row.names = FALSE,
        na = ""
      )
    }
  )
}


# ============================================================
# Run Application
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
