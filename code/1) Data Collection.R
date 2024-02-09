# Dependencies
library(grid)
library(gridExtra)
library(tidyverse)
library(tidytext)
library(lubridate)

# Helpers
source("./code/helpers/get_barratt.R")
source("./code/helpers/get_bellway.R")
source("./code/helpers/get_persimmon.R")

# Get raw data
barrat <- get_barratt() %>% 
  mutate_at(vars(contains("price") | contains("rooms")), as.numeric)
persimmon <- get_bellway() %>% 
  mutate_at(vars(contains("price") | contains("rooms")), as.numeric)
bellway <- get_persimmon() %>% 
  mutate_at(vars(contains("price") | contains("rooms")), as.numeric)

# Look ups for geo-coding
ons_params <- c(
  "postcode", "eastings", "northings", "country", "nhs_ha", "longitude", "latitude",
  "european_electoral_region", "primary_care_trust", "lsoa", "msoa", "incode", "outcode",
  "parliamentary_constituency", "admin_district", "admin_ward", "ccg", "nuts"
)

# New data
new <- bind_rows(barrat, persimmon, bellway) %>%
  mutate(across(where(contains("price"))), as.numeric)
  select(Date, Year, Month, url, developer, developement, address, postcode, rooms_min, 
         rooms_max, price_from, price_upto, all_of(ons_params)) %>%
  filter(!is.na(price_from))

# Previous price 
previous <- readRDS("./raw_data/new_build_prices.RDS") 

# Full data frame
df <- bind_rows(previous, new) %>%
  filter(!is.na(rooms_min)) %>%
  group_by(Date, developer, european_electoral_region, primary_care_trust) %>%
  ## Count the no. of unique listings
  mutate(listings = n_distinct(developement),
         room_range = if_else(is.na(rooms_max-rooms_min), 0, rooms_max-rooms_min),
         Month = factor(Month)) %>%
  ungroup() %>%
  ## Fix some problematic values
  mutate(price_from = ifelse(price_from==1, price_upto, price_from),
         price_mean = ifelse(is.na(price_upto), price_from, (price_from+price_upto)/2), 
         rooms_min.f = factor(rooms_min))

# Results
write_rds(df, "./raw_data/new_build_prices.RDS")

