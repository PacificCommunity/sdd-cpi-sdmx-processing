library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(DT)

allowed_sheets <- c(
  "bot","imports","exports","reexports","totexports",
  "bot_cty","trade_reg","mode_trspt","x_sitc","m_sitc"
)

ui <- fluidPage(
  titlePanel("IMTS Data Processing Application"),
  sidebarLayout(
    sidebarPanel(
      h4("1. Upload IMTS Data"),
      fileInput("excel_file","Select IMTS Excel file",accept=".xlsx"),
      hr(),
      actionButton("process","Process IMTS Data",icon=icon("play"),class="btn-primary"),
      hr(),
      downloadButton("download_csv","Download Final CSV",class="btn-success"),
      hr(),
      h4("Processing Status"),
      verbatimTextOutput("status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 br(),
                 fluidRow(
                   column(3,wellPanel(h4("Rows"),textOutput("n_rows"))),
                   column(3,wellPanel(h4("Indicators"),textOutput("n_indicators"))),
                   column(3,wellPanel(h4("Frequencies"),textOutput("n_freq"))),
                   column(3,wellPanel(h4("Trade Flows"),textOutput("n_tradeflow")))
                 ),
                 hr(),
                 h4("Indicator Summary"),
                 DTOutput("indicator_summary")
        ),
        tabPanel("Final Data",br(),DTOutput("preview")),
        tabPanel("Processing Log",br(),verbatimTextOutput("log"))
      )
    )
  )
)

server <- function(input,output,session){
  
  final_data <- reactiveVal(NULL)
  log_messages <- reactiveVal("")
  
  add_log <- function(msg){
    log_messages(paste(log_messages(),msg,sep="\n"))
  }
  
  observeEvent(input$process,{
    req(input$excel_file)
    
    withProgress(message="Processing IMTS data...",value=0,{
      add_log("Starting processing...")
      filePath <- input$excel_file$datapath
      
      incProgress(.1,"Reading workbook...")
      wsheets <- excel_sheets(filePath)
      
      invalid <- setdiff(wsheets,allowed_sheets)
      if(length(invalid)){
        showNotification(
          paste("Unsupported worksheet(s):",paste(invalid,collapse=", ")),
          type="error",duration=NULL)
        add_log("ERROR: Unsupported worksheets.")
        return()
      }
      
      incProgress(.2,"Processing worksheets...")
      final_df <- data.frame()
      
      for(sheetname in wsheets){
        df <- read_excel(filePath,sheet=sheetname)
        
        if(sheetname=="bot"){
          tmp <- df |>
            pivot_longer(cols=-c(DATAFLOW:OBS_COMMENT),
                         names_to="TRADE_FLOW",
                         values_to="OBS_VALUE") |>
            mutate(TIME_PERIOD=as.character(TIME_PERIOD))
        } else if(sheetname %in% c("imports","exports","reexports","totexports","x_sitc","m_sitc")){
          tmp <- df |>
            pivot_longer(cols=-c(DATAFLOW:OBS_COMMENT),
                         names_to="COMMODITY",
                         values_to="OBS_VALUE") |>
            mutate(TIME_PERIOD=as.character(TIME_PERIOD))
        } else if(sheetname=="bot_cty"){
          tmp <- df |>
            pivot_longer(cols=-c(DATAFLOW:OBS_COMMENT),
                         names_to="TIME_PERIOD",
                         values_to="OBS_VALUE") |>
            mutate(FREQ=ifelse(nchar(TIME_PERIOD)>4,"M","A"),
                   TIME_PERIOD=as.character(TIME_PERIOD))
        } else if(sheetname=="mode_trspt"){
          tmp <- df |>
            pivot_longer(cols=-c(DATAFLOW:OBS_COMMENT),
                         names_to="TRANSPORT",
                         values_to="OBS_VALUE") |>
            mutate(TIME_PERIOD=as.character(TIME_PERIOD))
        } else {
          tmp <- df |>
            pivot_longer(cols=-c(DATAFLOW:OBS_COMMENT),
                         names_to="TIME_PERIOD",
                         values_to="OBS_VALUE") |>
            mutate(FREQ=ifelse(nchar(TIME_PERIOD)>4,"M","A"),
                   TIME_PERIOD=as.character(TIME_PERIOD))
        }
        final_df <- bind_rows(final_df,tmp)
      }
      
      incProgress(.8,"Cleaning data...")
      final_df <- final_df |>
        filter(!is.na(OBS_VALUE))
      final_df[is.na(final_df)] <- ""
      final_df <- final_df |>
        mutate(OBS_VALUE=round(as.numeric(OBS_VALUE),0)) |>
        select(DATAFLOW,FREQ,TIME_PERIOD,GEO_PICT,INDICATOR,
               TRADE_FLOW,COMMODITY,COUNTERPART,TRANSPORT,CURRENCY,
               OBS_VALUE,UNIT_MEASURE,UNIT_MULT,OBS_STATUS,
               DATA_SOURCE,OBS_COMMENT)
      
      final_data(final_df)
      add_log("Processing completed successfully.")
      incProgress(1)
    })
  })
  
  output$n_rows <- renderText({req(final_data());format(nrow(final_data()),big.mark=",")})
  output$n_indicators <- renderText({req(final_data());dplyr::n_distinct(final_data()$INDICATOR)})
  output$n_freq <- renderText({req(final_data());dplyr::n_distinct(final_data()$FREQ)})
  output$n_tradeflow <- renderText({req(final_data());dplyr::n_distinct(final_data()$TRADE_FLOW)})
  
  output$indicator_summary <- renderDT({
    req(final_data())
    datatable(final_data() |> count(FREQ,INDICATOR,name="Observations"))
  })
  
  output$preview <- renderDT({
    req(final_data())
    datatable(final_data(),filter="top",extensions="Buttons",
              options=list(pageLength=25,scrollX=TRUE,dom="Bfrtip",
                           buttons=c("copy","csv","excel")))
  })
  
  output$log <- renderText(log_messages())
  
  output$status <- renderText({
    if(is.null(final_data())) "Waiting for IMTS Excel file..."
    else "Processing completed successfully.\nThe final dataset is ready for download."
  })
  
  output$download_csv <- downloadHandler(
    filename=function(){paste0("DF_IMTS-data_",format(Sys.time(),"%Y%m%d_%H%M%S"),".csv")},
    content=function(file){write.csv(final_data(),file,row.names=FALSE,na="")}
  )
}

shinyApp(ui,server)