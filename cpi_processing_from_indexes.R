library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)

username <- Sys.getenv("USERNAME")

stats_office <- data.frame(
  GEO_PICT = c("AS","CK","FJ","FM","GU","KI","MH","MP","NC","NR","NU","PF","PG","PN","PW","SB","TK","TO","TV","VU","WF","WS"),
  office = c("Data and Statistics American Samoa Department of Commerce",
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
             )
)

exlPath <- paste0("C:/users/", username , "/OneDrive - SPC/DotStat/Dock/DF_CPI/cpi_data.xlsx") 

#-----------------------------------------------------------
# Read Monthly CPI and convert to pivot longer
#-----------------------------------------------------------

cpi_data <- read_excel(exlPath, sheet = "cpi_data") |>
  pivot_longer(
    cols = -c(DATAFLOW:OBS_COMMENT),
    names_to = "COMMODITY",
    values_to = "OBS_VALUE"
)

#-----------------------------------------------------------
# Convert OBS_VALUE to numeric
#-----------------------------------------------------------

cpi <- cpi_data |>
  mutate(
    OBS_VALUE = as.numeric(OBS_VALUE),
    Date = as.Date(paste0(TIME_PERIOD, "-01")),
    Year = year(Date),
    Quarter = quarter(Date),
    OBS_COMMENT = "Monthly consumer price indexes sourced from "
  )

#-----------------------------------------------------------
# Average Inflation rate calculation function
#-----------------------------------------------------------

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


#-----------------------------------------------------------
# Calculate Monthly Inflation (Month-on-Month)
#-----------------------------------------------------------

monthly_inflation <- calc_inflation(
  cpi,
  "Monthly inflation calculated from average monthly indexes sourced from "
)

#-----------------------------------------------------------
# Quarterly Average CPI
# Only calculate where all 3 months exist
#-----------------------------------------------------------

quarterly_cpi <- cpi |>
  group_by(
    DATAFLOW,GEO_PICT,COMMODITY,BASE_PER,UNIT_MEASURE,UNIT_MULT,OBS_STATUS,OBS_COMMENT,Year,Quarter) |>
  summarise(
    Months = sum(!is.na(OBS_VALUE)),
    OBS_VALUE = ifelse(
      Months == 3,
      mean(OBS_VALUE, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) |>
  filter(Months == 3) |>
  mutate(
    FREQ = "Q",
    INDICATOR = "IDX",
    OBS_STATUS = "E",
    OBS_COMMENT = "Quarterly average indexes calculated from average monthly indexes sourced from ",
    TIME_PERIOD = paste0(Year, "-Q", Quarter)
  ) |>
  select(DATAFLOW,FREQ,GEO_PICT,INDICATOR,COMMODITY,TIME_PERIOD,OBS_VALUE,UNIT_MEASURE,UNIT_MULT,OBS_STATUS,BASE_PER,OBS_COMMENT
  )

quarterly_cpi$OBS_VALUE <-
  round(quarterly_cpi$OBS_VALUE, 1)


#-----------------------------------------------------------
# Quarterly Inflation
#-----------------------------------------------------------

quarterly_inflation <- calc_inflation(
  quarterly_cpi,
  paste0("Quarterly average inflation calculated from average monthly indexes sourced from ")
)

#-----------------------------------------------------------
# Calculate Annual Average CPI
# Only where 12 monthly observations exist
#-----------------------------------------------------------

annual_cpi <- quarterly_cpi |>
  mutate(
    Year = substr(TIME_PERIOD, 1, 4)
  ) |>
  group_by(DATAFLOW,GEO_PICT,COMMODITY,BASE_PER,UNIT_MEASURE,UNIT_MULT,Year) |>
  summarise(
    Quarters = n(),
    OBS_VALUE = ifelse(
      Quarters == 4,
      mean(OBS_VALUE),
      NA_real_
    ),
    .groups = "drop"
  ) |>
  filter(Quarters == 4)

#-----------------------------------------------------------
# Calculate Annual Average Inflation
#-----------------------------------------------------------

annual_inflation <- annual_cpi |>
  arrange(GEO_PICT, COMMODITY, Year) |>
  group_by(GEO_PICT, COMMODITY) |>
  mutate(
    OBS_VALUE =
      ((OBS_VALUE / lag(OBS_VALUE)) - 1) * 100
  ) |>
  ungroup()

#-----------------------------------------------------------
# Create Annual CPI dataset
#-----------------------------------------------------------

annual_cpi_out <- annual_cpi |>
  mutate(
    FREQ = "A",
    INDICATOR = "IDX",
    OBS_STATUS = "E",
    OBS_COMMENT = "Annual average indexes calculated from average monthly indexes sourced from ",
    TIME_PERIOD = as.character(Year)
  ) |>
  select(DATAFLOW,FREQ,GEO_PICT,INDICATOR,COMMODITY,TIME_PERIOD,OBS_VALUE,UNIT_MEASURE,UNIT_MULT,OBS_STATUS,BASE_PER,OBS_COMMENT
  )

#-----------------------------------------------------------
# Create Annual Inflation dataset
#-----------------------------------------------------------

annual_inflation_out <- calc_inflation(
  annual_cpi_out,
  "Annual average inflation calculated from quarterly average indexes sourced from "
)

#-----------------------------------------------------------
# Combine all generated dataframes together
#-----------------------------------------------------------

combined <- bind_rows(
  cpi |>
    select(DATAFLOW,FREQ,GEO_PICT,INDICATOR,COMMODITY,TIME_PERIOD,OBS_VALUE,UNIT_MEASURE,UNIT_MULT,OBS_STATUS,BASE_PER,OBS_COMMENT
    ),
  monthly_inflation,
  quarterly_cpi,
  quarterly_inflation,
  annual_cpi_out,
  annual_inflation_out
)

# Merge combined with office dataframe to get office names
combined <- merge(combined, stats_office, by = "GEO_PICT")

combined<- combined |>
  filter(!is.na(OBS_VALUE)) |>
  mutate(
    across(
      everything(),
      ~ if_else(is.na(.x), "", as.character(.x))
    ),
    OBS_VALUE = round(as.numeric(OBS_VALUE), 1),
    OBS_COMMENT = ifelse(!is.na(OBS_COMMENT), paste0(OBS_COMMENT," ",office), "")
  ) |>
  select(-c("Date", "Year", "Quarter", "office"),
         DATAFLOW,FREQ,GEO_PICT,INDICATOR,COMMODITY,TIME_PERIOD,OBS_VALUE,UNIT_MEASURE,UNIT_MULT,OBS_STATUS,BASE_PER,OBS_COMMENT)
  
#-----------------------------------------------------------
# Create folder name using the system date and time
#-----------------------------------------------------------

oneDrivePath <- paste0("C:/Users/", username, "/OneDrive - SPC/DotStat/REFDB/DF_CPI")
myYear <- as.numeric(format(Sys.Date(), "%Y"))
myMonth <- sprintf("%02d", as.numeric(format(Sys.Date(), "%m")))
myDay <- sprintf("%02d", as.numeric(format(Sys.Date(), "%d")))
myHour <- sprintf("%02d", as.numeric(format(Sys.time(), "%H")))
myMin <- sprintf("%02d", as.numeric(format(Sys.time(), "%M")))
mySecond <- sprintf("%02d", as.numeric(format(Sys.time(), "%S")))

folderName <- paste0(myYear,myMonth,myDay,"_",myHour,myMin,mySecond,"_CPI_data_Update")

newFolder <- paste0(oneDrivePath,"/",folderName)
newFile <- paste0(newFolder,"/DF_CPI-data.CSV")

#-----------------------------------------------------------
# Write the final file to the newly created folder
#-----------------------------------------------------------

dir.create(newFolder, recursive = TRUE, showWarnings = FALSE)

write.csv(combined, newFile,  row.names = FALSE)
message("CSV written to: ", newFolder)
cat("Finished successfully.\n")