# Dependencies
library(grid)
library(gridExtra)
library(tidyverse)
library(tidytext)
library(lubridate)

# Set working directory 
setwd("/Users/shashwatpande/Library/CloudStorage/OneDrive-TriumphMotorcyclesLtd/###R Projects/# Ad-Hoc/## New Build Tracker")

# Helpers
source("./code/helpers/get_barratt.R")
source("./code/helpers/get_bellway.R")
source("./code/helpers/get_persimmon.R")

# Get raw data
barratt <- get_barratt() %>% 
  mutate_at(vars(contains("price") | contains("rooms")), as.numeric)
bellway <- get_bellway() %>% 
  mutate_at(vars(contains("price") | contains("rooms")), as.numeric)
persimmon <- get_persimmon() %>% 
  mutate_at(vars(contains("price") | contains("rooms")), as.numeric)

# Look ups for geo-coding
ons_params <- c(
  "postcode", "eastings", "northings", "country", "nhs_ha", "longitude", "latitude",
  "european_electoral_region", "primary_care_trust", "lsoa", "msoa", "incode", "outcode",
  "parliamentary_constituency", "admin_district", "admin_ward", "ccg", "nuts"
)

# New data
new <- bind_rows(barratt, persimmon, bellway) %>%
  select(Date, Year, Month, url, developer, developement, address, postcode, rooms_min, 
         rooms_max, price_from, price_upto, all_of(ons_params)) %>%
  filter(!is.na(price_from)) %>% 
  as_tibble()

# Previous price 
previous <- readRDS("./raw_data/new_build_prices.RDS") %>%
  mutate(Date = date(Date))

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

# Clean Data
levels(df$rooms_min.f) <- c("1","2","3","4","5", "6")
# Set factor variable for no. of rooms
df$rooms_min.f <- plyr::mapvalues(
  df$rooms_min.f, from = c("1", "2", "3", "4", "5", "6"), to = c("1","2","3","4+","4+","4+")
)
levels(df$rooms_min.f) <- c("1","2","3","4+")

# Assign geo-codes to NA postcodes
tmp <- filter(df, is.na(european_electoral_region)) %>%
  separate(postcode, c("out", "inc"), sep = " ") %>%
  mutate(out = ifelse(is.na(inc), "DA10", out)) 
postcode_list <- lapply(tmp$out, function(x) PostcodesioR::random_postcode(x)$postcode)
postcode_list[sapply(postcode_list, is.null)] <- NA
tmp$postcode <- unlist(postcode_list)
tmp <- select(tmp, colnames(df)) %>%
  select(-c(european_electoral_region,
            eastings,northings,country,nhs_ha,longitude,latitude,primary_care_trust,
            lsoa,msoa,incode,outcode,parliamentary_constituency,admin_district,admin_ward,
            ccg,nuts)) %>%
  filter(!is.na(postcode))
# Lookup postcodes
postcodes.tmp <- data.frame()
for (i in 1:length(tmp$postcode)) {
  tmp2 <- PostcodesioR::postcode_lookup(tmp$postcode[i])
  postcodes.tmp <- bind_rows(postcodes.tmp, tmp2)
}
tmp <- cbind(tmp, postcodes.tmp)
tmp <- select(tmp, colnames(df))
# Remove tmp developments from Data
df <- filter(df, !developement %in% unique(tmp$developement))
# Return approximate approximate geo-codes to data
df <- bind_rows(df, tmp) %>%
  filter(nchar(price_mean) > 3)

data <- df %>% 
  group_by(developer, rooms_min.f, developement, latitude, longitude) %>%
  ### Compute change in % terms 
  mutate(
    change_from = (c(NA, diff(price_from))/price_from) * 100, 
    change_mean = (c(NA, diff(price_mean))/price_mean) * 100, 
    change_upto = (c(NA, diff(price_upto))/price_upto) * 100
  ) %>%
  filter(nchar(price_mean) > 3)

# Results
write_rds(df, "./raw_data/new_build_prices.RDS")
write_rds(data, "./processed_data/new_build_prices_clean.RDS")

# Clear environment
rm(
  tmp,postcodes.tmp,previous,new,
  barratt,bellway,persimmon,
  df,ons_params,
  get_barratt,get_bellway,
  get_persimmon, postcode_list
)

