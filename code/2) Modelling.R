# Libraries
library(lme4)
library(lmerTest)
library(tidyverse)
library(lubridate)
library(tsibble)
library(httr)

# Set working directory 
setwd("/Users/shashwatpande/Library/CloudStorage/OneDrive-TriumphMotorcyclesLtd/###R Projects/# Ad-Hoc/## New Build Tracker")

# Load data
#source("./code/1) Data Collection")

# Preprocess
df <- read_rds("./processed_data/new_build_prices_clean.RDS") %>% 
  distinct() %>%
  rename("data_scraped_on" = "Date") %>%
  mutate(data_scraped_on = date(data_scraped_on), period = data_scraped_on %m-% months(1)) %>%
  mutate(date=date(tsibble::yearmonth(period))) %>%
  mutate(rooms_min.f = paste0(rooms_min.f, " Bedrooms"),
         price = log(price_mean),
         price.from = log(price_from),
         price.upto = log(price_upto)) %>%
  group_by(Month, developer, european_electoral_region, rooms_min.f) %>%
  mutate(listings = length(unique(developement)),
         Month = yearmonth(date))
df$Month <- factor(df$Month)
levels(df$Month)

# Mixed Effects, monthly average
model0 <- lmer(
  price ~ 
    Month +
    (1|european_electoral_region:primary_care_trust:admin_district), 
  data = df
)
model0.from <- lmer(
  price.from ~ 
    Month +
    (1|european_electoral_region:primary_care_trust:admin_district), 
  data = df
)
model0.upto <- lmer(
  price.upto ~ 
    Month +
    (1|european_electoral_region:primary_care_trust:admin_district), 
  data = df
)
# Mixed Effects, simple slopes
model1 <- lmer(
  price ~ 
    Month + developer + rooms_min.f + 
    european_electoral_region + 
    log(listings) +
    room_range +
    (1|european_electoral_region:primary_care_trust:admin_district), 
  data = df
)
model1.from <- lmer(
  price.from ~ 
    Month + developer + rooms_min.f + 
    european_electoral_region + 
    log(listings) +
    room_range +
    (1|european_electoral_region:primary_care_trust:admin_district), 
  data = df
)
model1.upto <- lmer(
  price.upto ~ 
    Month + developer + rooms_min.f + 
    european_electoral_region + 
    log(listings) +
    room_range +
    (1|european_electoral_region:primary_care_trust:admin_district), 
  data = df
)

# Mixed effects, conditional slopes
model2 <- lmer(
  price ~ 
    Month*developer*rooms_min.f + 
    european_electoral_region + 
    log(listings) + 
    room_range +
    (1|european_electoral_region:primary_care_trust:admin_district), 
  data = df 
)
model2.from <- lmer(
  price.from ~ 
    Month*developer*rooms_min.f + 
    european_electoral_region + 
    log(listings) + 
    room_range +
    (1|european_electoral_region:primary_care_trust:admin_district), 
  data = df 
)
model2.upto <- lmer(
  price.upto ~ 
    Month*developer*rooms_min.f + 
    european_electoral_region + 
    log(listings) + 
    room_range +
    (1|european_electoral_region:primary_care_trust:admin_district), 
  data = df 
)
# Mixed effects, conditional slopes Regions
model3 <- lm(
  price ~ 
    Month*european_electoral_region + 
    rooms_min.f + 
    log(listings) + 
    room_range,
  data = df  %>% filter(european_electoral_region != "London") 
)
model3.from <- lm(
  price.from ~ 
    Month*european_electoral_region + 
    rooms_min.f + 
    log(listings) + 
    room_range,
  data = df  %>% filter(european_electoral_region != "London") 
)
model3.upto <- lm(
  price.upto ~ 
    Month*european_electoral_region + 
    rooms_min.f + 
    log(listings) + 
    room_range,
  data = df  %>% filter(european_electoral_region != "London") 
)


## Save models
saveRDS(model0, file = "./models/model0.RDS")
saveRDS(model1, file = "./models/model1.RDS")
saveRDS(model2, file = "./models/model2.RDS")
saveRDS(model3, file = "./models/model3.RDS")
saveRDS(model0.from, file = "./models/model0.from.RDS")
saveRDS(model1.from, file = "./models/model1.from.RDS")
saveRDS(model2.from, file = "./models/model2.from.RDS")
saveRDS(model3.from, file = "./models/model3.from.RDS")
saveRDS(model0.upto, file = "./models/model0.upto.RDS")
saveRDS(model1.upto, file = "./models/model1.upto.RDS")
saveRDS(model2.upto, file = "./models/model2.upto.RDS")
saveRDS(model3.upto, file = "./models/model3.upto.RDS")



