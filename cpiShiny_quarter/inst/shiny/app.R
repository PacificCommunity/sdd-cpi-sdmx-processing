library(rsdmx)
library(dplyr)

stats_office <- data.frame(
  GEO_PICT = c(
    "ASM","COK","FJI","FSM","GUM","KIR","MHL","MNP","NCL","NRU","NIU",
    "PYF","PNG","PCN","PLW","SLB","TKL","TON","TUV","VUT","WLF","WSM"
  )
)

df <- data.frame()

for (country in stats_office$GEO_PICT) {
  
  cat("Downloading:", country, "\n")
  
  data_revenue <- tryCatch(
    {
      data <- as.data.frame(
        readSDMX(
          providerId = "OECD",
          resource = "data",
          flowRef = "DSD_REV_COMP_ASAP@DF_RSASAP",
          key = list(
            country, NULL, NULL, NULL, NULL, NULL, NULL, NULL
          )
        )
      )
      
      data$GEO_PICT <- country
      
      data
      
    },
    error = function(e) {
      cat("Error downloading", country, ":", e$message, "\n")
      NULL
    }
  )
  
  if (!is.null(data_revenue) && nrow(data_revenue) > 0) {
    df <- bind_rows(df, data_revenue)
  }
}

# Check countries downloaded
unique(df$GEO_PICT)

# Number of observations by country
df %>%
  count(GEO_PICT)