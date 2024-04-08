# Dependencies
library(httr)
library(lubridate)

# Set working directory 
setwd("/Users/shashwatpande/Library/CloudStorage/OneDrive-TriumphMotorcyclesLtd/###R Projects/# Ad-Hoc/## New Build Tracker")

# Run upstream processes
source("./code/2) Modelling.R")

# Get Nationwide Data
GET("https://www.nationwidehousepriceindex.co.uk/download/uk-monthly-index",
    write_disk(tf <- tempfile(fileext = ".xls")))
nationwide <- readxl::read_xls(tf)
nationwide_small <- nationwide %>% 
  rename("date" = "...1", "nationwide_prices" = `Average House Price`, 
         "nationwide_change" = `Monthly % Change (SA)`,
         "annual_nwd_change" = `Year % Change`) %>%
  dplyr::select(date, nationwide_prices, nationwide_change, annual_nwd_change) %>% 
  filter(lubridate::year(date) >= 2022) %>%
  mutate(date = lubridate::date(date))
saveRDS(nationwide_small, paste0("./processed_data/nationwide_sample_data.RDS"))
saveRDS(nationwide, paste0("./processed_data/nationwide_full_data.RDS"))

# Get ONS Data
current <- if_else(
  nchar(month(today() %m-% months(3))) == 1, 
  paste0("0", month(today() %m-% months(3))), 
  paste0(month(today() %m-% months(3)))
  )
file <- glue::glue("house-price-index-data/UK-HPI-full-file-2024-{current}.csv")
uk_hpi <- readr::read_csv(glue::glue("https://publicdata.landregistry.gov.uk/market-trend-data/{file}"))
uk_hpi <- uk_hpi %>%
  filter(RegionName == "United Kingdom") %>%
  select(Date, ONSAveragePrice = AveragePrice, `ONS1m%Change` = `1m%Change`, 
         `ONS12m%Change` = `12m%Change`,
         ONSNewPrice = NewPrice, `ONSNew1m%Change`= `New1m%Change`,
         `ONSNew12m%Change` = `New12m%Change`,
         ONSNewVolume = NewSalesVolume, ONSVolume = OldSalesVolume) %>%
  mutate(Date = as.Date(Date, "%d/%m/%Y"))
saveRDS(uk_hpi, "./processed_data/UK-HPI.RDS")

