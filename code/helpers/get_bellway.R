# Get Barratt Developer Listings #
get_bellway <- function() {
  # Dependencies
  library(rvest)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tidyverse)
  library(foreach)
  library(doParallel)
  
  # Extract URLs
  scrape_urls <- "https://www.bellway.co.uk/new-homes" %>%
    read_html() %>%
    html_nodes(css = ".map-point") %>%
    html_attr("href") %>%
    as_tibble_col(column_name = "url") %>%
    mutate(url = paste0("https://www.bellway.co.uk", url)) %>%
    .$url %>% as.character()
  
  # Scraper Logic
  ## Parallel Back-end
  cores <- parallel::detectCores() - 1
  cl <- makeCluster(cores)
  registerDoParallel(cl, cores = cores)
  data <- foreach (i = 1:length(scrape_urls), .errorhandling = "remove", .combine = rbind) %dopar% {
    
    library(rvest)
    library(dplyr)
    library(purrr)
    library(stringr)
    library(tidyverse)
    
    main_page <- scrape_urls[i] %>%
      read_html() %>%
      html_nodes(css = ".tile__content") 
    developement <- main_page %>%
      html_nodes(css = ".heading") %>%
      html_text2() %>%
      str_remove_all(., "\r") %>%
      str_trim()
    address <- main_page %>%
      html_nodes(css = ".description") %>%
      html_text2() %>%
      str_remove_all(., "\r") %>%
      str_trim()
    bedrooms <- main_page %>%
      html_nodes(css = "ul") %>% 
      html_text2() %>% 
      as_tibble() %>% 
      separate(value, into = c("a", "b"), sep = "\n") %>%
      select(a) %>%
      separate(a, into = c("rooms_min", "rooms_max"), sep = "&") %>%
      mutate(rooms_min = str_extract(rooms_min, "^\\d{1}"), 
             rooms_max = str_extract(rooms_max, "[0-9]+"))
    price <- main_page %>%
      html_nodes(css = ".price") %>% 
      html_text2()
    
    tibble(url = scrape_urls[i], developement = developement, address = address, price = price) %>% 
      cbind(., bedrooms) %>%
      mutate(postcode = str_extract_all(address, "[A-Z]{1,2}[0-9][A-Z0-9]? [0-9][ABD-HJLNP-UW-Z]{2}"),
             address = str_trim(str_remove_all(address, "[A-Z]{1,2}[0-9][A-Z0-9]? [0-9][ABD-HJLNP-UW-Z]{2}")),
             address = str_remove_all(address, ",$")) %>%
      separate(price, into = c("price_from", "price_upto"), sep = "to") %>%
      mutate(price_from = str_remove_all(price_from, ",") %>% str_extract(., "[0-9]+"),
             price_upto = str_remove_all(price_upto, ",") %>% str_extract(., "[0-9]+")) %>% 
      mutate(developer = "Bellway-Homes", postcode = as.character(postcode))
  }
  
  # Reconcile
  unique_data <- data %>% 
    select(-url) %>% 
    unique() %>%
    left_join(
      .,
      data %>%
        group_by(developement, address, rooms_min, rooms_max,
                 price_from, price_upto, postcode, developer) %>%
        summarise(url = list(url)[[1]][1])
      )
  
  # Geocode
  geocoded_postcodes <- foreach(i = 1:length(unique(data$postcode)), .combine = rbind) %dopar% {
    # Look ups for geo-coding
    ons_params <- c(
      "postcode", "eastings", "northings", "country", "nhs_ha", "longitude", "latitude",
      "european_electoral_region", "primary_care_trust", "lsoa", "msoa", "incode", "outcode",
      "parliamentary_constituency", "admin_district", "admin_ward", "ccg", "nuts"
    )
    PostcodesioR::postcode_lookup(unique(data$postcode)[i]) %>%
      select(any_of(ons_params))
  }
  
  stopCluster(cl)
  
  # Results
  data_final <- left_join(unique_data, geocoded_postcodes) %>%
    mutate(Date = Sys.Date(), Year = lubridate::year(Date), Month = months(Date))
  
  data_final
  
}