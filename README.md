# CPI Data Processing and Transformation

## Overview

This R script processes monthly Consumer Price Index (CPI) data for Pacific Island Countries and Territories (PICTs) and generates a standardised CPI dataset suitable for dissemination through an SDMX-based statistical data platform.

The script:

* Reads monthly CPI data from an Excel workbook.
* Reshapes CPI commodity data from wide to long format.
* Converts CPI observations to numeric values.
* Generates monthly, quarterly, and annual CPI datasets.
* Calculates monthly, quarterly, and annual inflation rates.
* Applies data completeness rules when calculating quarterly and annual averages.
* Adds official names of national statistical offices and statistical agencies.
* Standardises the output according to the required SDMX data structure.
* Writes the final combined dataset to a timestamped CSV file.

---

## Data Coverage

The script processes CPI data for the following Pacific Island Countries and Territories:

| GEO_PICT | Statistical Office / Data Provider                                       |
| -------- | ------------------------------------------------------------------------ |
| AS       | Data and Statistics American Samoa Department of Commerce                |
| CK       | Cook Islands Statistics Office                                           |
| FJ       | Fiji Bureau of Statistics                                                |
| FM       | FSM Statistics                                                           |
| GU       | The Bureau of Statistics and Plans - Guam                                |
| KI       | Kiribati National Statistics Office                                      |
| MH       | Marshall Islands Economic Policy, Planning and Statistics Office (EPPSO) |
| MP       | CNMI Department of Commerce                                              |
| NC       | Institut de la Statistique et des Etudes Economiques                     |
| NR       | Nauru Bureau of Statistics                                               |
| NU       | Niue Statistics Office                                                   |
| PF       | Institut de la statistique de la Polynésie française                     |
| PG       | PNG National Statistics Office                                           |
| PN       | Pitcairn Statistics Office                                               |
| PW       | Palau Statistics Office                                                  |
| SB       | Solomon Islands National Statistics Office                               |
| TK       | Tokelau Statistics Office                                                |
| TO       | Tonga Statistics Department                                              |
| TV       | Tuvalu Statistics Office                                                 |
| VU       | Vanuatu Bureau of Statistics Office                                      |
| WF       | Wallis and Futuna Statistics Office                                      |
| WS       | Samoa Bureau of Statistics                                               |

---

## Input Data

The script reads CPI data from an Excel workbook using the `readxl` package.

The input workbook is expected to contain a worksheet named:

```text
cpi_data
```

The expected data structure contains SDMX-related dimensions and attributes, including:

* `DATAFLOW`
* `FREQ`
* `GEO_PICT`
* `INDICATOR`
* `TIME_PERIOD`
* `UNIT_MEASURE`
* `UNIT_MULT`
* `OBS_STATUS`
* `BASE_PER`
* `OBS_COMMENT`

Commodity columns are stored in wide format and are converted to long format, with:

* `COMMODITY` representing the CPI commodity or classification.
* `OBS_VALUE` representing the CPI index observation.

The input file is currently referenced using a local OneDrive path and the Windows username obtained from the system environment.

---

## R Packages

The script uses the following R packages:

```r
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(readxl)
```

### Package Usage

| Package     | Purpose                                                       |
| ----------- | ------------------------------------------------------------- |
| `dplyr`     | Data manipulation, grouping, summarisation and transformation |
| `tidyr`     | Reshaping CPI data from wide to long format                   |
| `lubridate` | Extracting year and quarter information from dates            |
| `readr`     | Data import/export functionality                              |
| `readxl`    | Reading CPI data from Excel workbooks                         |

---

# Processing Methodology

## 1. Monthly CPI Data

The source CPI data is initially stored in wide format, where individual CPI commodities are represented as columns.

The script converts these commodity columns into long format using `pivot_longer()`.

Each observation is represented by a combination of:

```text
GEO_PICT
COMMODITY
TIME_PERIOD
OBS_VALUE
```

The monthly CPI index is assigned:

```text
FREQ = M
INDICATOR = IDX
```

The script also creates a date variable from the `TIME_PERIOD` field to support subsequent annual and quarterly calculations.

---

## 2. Monthly Inflation

Monthly inflation is calculated using the percentage change in the CPI index compared with the previous month.

The formula is:

```text
Monthly Inflation (%) =
((CPI_t / CPI_(t-1)) - 1) × 100
```

where:

* `CPI_t` = CPI index for the current month.
* `CPI_(t-1)` = CPI index for the previous month.

Monthly inflation is calculated separately for each:

```text
GEO_PICT
COMMODITY
```

The first available observation for each country and commodity is excluded because there is no previous month available for calculating the percentage change.

The generated monthly inflation dataset uses:

```text
INDICATOR = INF
UNIT_MEASURE = PERCENT
OBS_STATUS = E
```

Inflation rates are rounded to one decimal place.

---

## 3. Quarterly Average CPI

Quarterly CPI indexes are calculated from monthly CPI indexes.

The calculation is performed separately for each:

```text
DATAFLOW
GEO_PICT
COMMODITY
BASE_PER
UNIT_MEASURE
UNIT_MULT
```

A quarterly average is only calculated when **all three months in the quarter are available**.

Therefore:

```text
Quarterly CPI = Mean of 3 monthly CPI observations
```

For example:

```text
2024-Q1 = Mean(January 2024, February 2024, March 2024)
```

If fewer than three monthly observations are available, the quarterly average is not generated.

The resulting quarterly dataset uses:

```text
FREQ = Q
INDICATOR = IDX
```

Quarterly CPI values are rounded to one decimal place.

---

## 4. Quarterly Inflation

Quarterly inflation is calculated from the quarterly average CPI indexes.

The formula is:

```text
Quarterly Inflation (%) =
((CPI_Qt / CPI_Q(t-1)) - 1) × 100
```

where:

* `CPI_Qt` = current quarterly average CPI.
* `CPI_Q(t-1)` = previous quarterly average CPI.

The calculation is performed separately for each country and commodity.

The resulting dataset uses:

```text
FREQ = Q
INDICATOR = INF
UNIT_MEASURE = PERCENT
OBS_STATUS = E
```

---

## 5. Annual Average CPI

Annual CPI indexes are calculated from the quarterly average CPI indexes.

The calculation is based on the four quarterly averages within each calendar year.

An annual CPI average is generated only when **all four quarters are available**.

Therefore:

```text
Annual CPI = Mean(Q1, Q2, Q3, Q4)
```

For example:

```text
2024 Annual CPI =
Mean(2024-Q1, 2024-Q2, 2024-Q3, 2024-Q4)
```

Because each quarterly average is itself calculated only when three monthly observations are available, the annual CPI calculation effectively requires:

```text
4 complete quarters
12 complete months
```

This ensures that annual CPI averages are based on a complete calendar year.

The resulting annual dataset uses:

```text
FREQ = A
INDICATOR = IDX
```

---

## 6. Annual Inflation

Annual inflation is calculated from annual average CPI indexes.

The formula is:

```text
Annual Inflation (%) =
((Annual CPI_t / Annual CPI_(t-1)) - 1) × 100
```

This represents the percentage change in the annual average CPI compared with the previous year's annual average CPI.

The resulting dataset uses:

```text
FREQ = A
INDICATOR = INF
UNIT_MEASURE = PERCENT
OBS_STATUS = E
```

---

# Frequency and Indicator Structure

The script generates six types of observations:

| Frequency       | Indicator | Description                                             |
| --------------- | --------- | ------------------------------------------------------- |
| Monthly (`M`)   | `IDX`     | Monthly CPI index                                       |
| Monthly (`M`)   | `INF`     | Month-on-month inflation rate                           |
| Quarterly (`Q`) | `IDX`     | Quarterly average CPI index                             |
| Quarterly (`Q`) | `INF`     | Quarter-on-quarter inflation rate                       |
| Annual (`A`)    | `IDX`     | Annual average CPI index                                |
| Annual (`A`)    | `INF`     | Year-on-year inflation rate based on annual average CPI |

---

# Data Completeness Rules

The following completeness rules are applied:

### Monthly CPI

Monthly observations are retained when a valid numeric `OBS_VALUE` is available.

### Monthly Inflation

Inflation is calculated only when a previous monthly CPI observation exists.

### Quarterly CPI

A quarterly average is calculated only when:

```text
Number of available months = 3
```

### Annual CPI

An annual average is calculated only when:

```text
Number of available quarters = 4
```

Since each quarter requires three months, this means that annual averages are based on a complete set of 12 monthly observations.

### Annual Inflation

Annual inflation requires two consecutive annual CPI averages.

---

# Output Dataset

All generated datasets are combined into a single dataframe using `bind_rows()`.

The final output contains the following standardised fields:

| Variable       | Description                                         |
| -------------- | --------------------------------------------------- |
| `DATAFLOW`     | SDMX dataflow identifier                            |
| `FREQ`         | Observation frequency: monthly, quarterly or annual |
| `GEO_PICT`     | PICT geographic code                                |
| `INDICATOR`    | CPI index (`IDX`) or inflation (`INF`)              |
| `COMMODITY`    | CPI commodity or classification                     |
| `TIME_PERIOD`  | Observation period                                  |
| `OBS_VALUE`    | CPI index or inflation rate                         |
| `UNIT_MEASURE` | Unit of measurement                                 |
| `UNIT_MULT`    | Unit multiplier                                     |
| `OBS_STATUS`   | Observation status                                  |
| `BASE_PER`     | CPI base period                                     |
| `OBS_COMMENT`  | Observation source and methodology comment          |

---

# Observation Comments

The script adds descriptive comments to generated observations.

Examples include:

```text
Monthly consumer price indexes sourced from [Statistical Office]
```

```text
Average Monthly inflation calculated from average monthly indexes sourced from [Statistical Office]
```

```text
Quarterly average indexes calculated from average monthly indexes sourced from [Statistical Office]
```

```text
Quarterly average inflation rate calculated from calculated average quarterly indexes sourced from [Statistical Office]
```

```text
Annual average indexes calculated from calculated average quarterly indexes sourced from [Statistical Office]
```

```text
Annual average inflation rate calculated from calculated annual average indexes sourced from [Statistical Office]
```

The relevant statistical office is appended to the observation comment based on the `GEO_PICT` code.

---

# Output File

The final dataset is exported as a CSV file named:

```text
DF_CPI-data.CSV
```

The file is saved in a timestamped directory using the following naming convention:

```text
YYYYMMDD_HHMMSS_CPI_data_Update
```

For example:

```text
20260723_083015_CPI_data_Update
```

This approach creates a unique output directory for each processing run and provides a clear record of when the CPI dataset was generated.

---

# Output Directory Structure

The resulting directory structure is:

```text
DF_CPI/
└── YYYYMMDD_HHMMSS_CPI_data_Update/
    └── DF_CPI-data.CSV
```

---

# Data Transformation Workflow

The overall processing workflow can be summarised as:

```text
Excel CPI Data
      │
      ▼
Read Monthly CPI Data
      │
      ▼
Convert Wide Data to Long Format
      │
      ▼
Convert CPI Values to Numeric
      │
      ▼
Generate Monthly CPI Index
      │
      ├───────────────┐
      ▼               ▼
Monthly Inflation   Quarterly CPI
                        │
                        ▼
                  Quarterly Inflation
                        │
                        ▼
                  Annual Average CPI
                        │
                        ▼
                  Annual Inflation
                        │
                        ▼
                  Combine All Data
                        │
                        ▼
              Add Statistical Office
                        │
                        ▼
              Standardise SDMX Fields
                        │
                        ▼
                Export CSV Dataset
```

---

# Data Quality and Validation Considerations

The script applies several basic data quality controls:

1. CPI values are converted to numeric format.
2. Missing CPI observations are excluded from the final output.
3. Quarterly averages require three monthly observations.
4. Annual averages require four quarterly observations.
5. Inflation calculations require a preceding observation.
6. CPI and inflation values are rounded to one decimal place.
7. Statistical office names are mapped using the `GEO_PICT` code.
8. Output variables are arranged according to the expected SDMX data structure.

---

# Important Methodological Note

Quarterly CPI averages are calculated from **monthly CPI indexes**, while annual CPI averages are calculated from the **quarterly CPI averages**.

Therefore, the aggregation hierarchy is:

```text
Monthly CPI
    ↓
Quarterly Average CPI
    ↓
Annual Average CPI
```

Annual CPI is therefore calculated only when four complete quarterly averages are available. This ensures that the annual average is based on a complete calendar year of CPI observations.

Inflation rates are then calculated from the corresponding frequency-specific CPI averages:

```text
Monthly CPI      → Monthly Inflation
Quarterly CPI    → Quarterly Inflation
Annual CPI       → Annual Inflation
```

---

# Reproducibility

The script currently uses a local Windows OneDrive directory and retrieves the Windows username using:

```r
username <- Sys.getenv("USERNAME")
```

The input and output paths therefore depend on the local user's OneDrive folder structure.

For improved portability and reproducibility, it is recommended that future versions of the script use configurable file paths, command-line arguments, environment variables, or project-relative paths rather than hard-coded local directories.

---

# Author and Maintenance

This script is intended for processing and standardising CPI data for Pacific Island Countries and Territories within an SDMX statistical data dissemination workflow.

The script should be updated when:

* New PICTs are added.
* Statistical office names change.
* CPI commodity classifications change.
* The SDMX data structure changes.
* Input Excel file structures change.
* CPI aggregation or inflation methodology is revised.

---