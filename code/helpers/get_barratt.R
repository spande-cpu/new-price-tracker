# Get Barratt Development Listings #
get_barratt <- function() {
  # Dependencies
  library(rvest)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tidyverse)
  library(foreach)
  library(doParallel)

  # Extract URLs
  scrape_urls <- "https://www.barratthomes.co.uk/new-homes/" %>%
    read_html() %>%
    html_nodes(css = ".location-group__item") %>%
    html_nodes(css = "a") %>%
    html_attr("href") %>%
    as_tibble_col(column_name = "url") %>%
    mutate(url = paste0("https://www.barratthomes.co.uk", url)) %>%
    .$url %>%
    as.character()

  ## Scraper Logic
  ### Parallel Backend
  cores <- parallel::detectCores() - 1
  cl <- makeCluster(cores)
  registerDoParallel(cl, cores = cores)
  data <- foreach(i = 1:length(scrape_urls), .errorhandling = "remove", .combine = rbind) %dopar% {
    library(rvest)
    library(dplyr)
    library(purrr)
    library(stringr)
    library(tidyverse)

    main_page <- scrape_urls[i] %>%
      read_html() %>%
      html_nodes(css = ".location-list")
    developement <- main_page %>%
      html_nodes(css = ".location-list-card__heading") %>%
      html_text2() %>%
      str_remove_all(., "\r") %>%
      str_trim()
    address <- main_page %>%
      html_nodes(css = ".location-list-card__address") %>%
      html_text2() %>%
      str_remove_all(., "\r") %>%
      str_trim()
    details <- main_page %>%
      html_nodes(css = ".location-list-card__features") %>%
      html_text2() %>%
      str_remove_all(., "\r|\n") %>%
      str_trim() %>%
      as_tibble() %>%
      separate(value, c("price_from", " ", "price_upto", "rooms"), sep = " ", extra = "merge") %>%
      select(price_from, price_upto, rooms) %>%
      mutate(
        id = row_number(),
        price_from = str_remove_all(price_from, ",|£"),
        price_upto = str_remove_all(price_upto, ",|£")
      ) %>%
      mutate(rooms = str_extract_all(rooms, "[0-9]+")) %>%
      unnest(rooms) %>%
      mutate(
        rooms = as.numeric(rooms),
        rooms = if_else(rooms >= 7, NA, rooms)
      ) %>%
      group_by(id) %>%
      summarise(
        price_from = unique(price_from), price_upto = unique(price_upto),
        rooms_min = min(rooms, na.rm = T), rooms_max = max(rooms, na.rm = T)
      ) %>%
      mutate_if(is.character, as.numeric)

    tibble(url = scrape_urls[i], developement = developement, address = address) %>%
      cbind(., details) %>%
      mutate(
        postcode = str_extract_all(address, "[A-Z]{1,2}[0-9][A-Z0-9]? [0-9][ABD-HJLNP-UW-Z]{2}"),
        address = str_trim(str_remove_all(address, "[A-Z]{1,2}[0-9][A-Z0-9]? [0-9][ABD-HJLNP-UW-Z]{2}")),
        address = str_remove_all(address, ",$"), developer = "Barratt-Homes", postcode = as.character(postcode)
      )
  }

  # Reconcile
  unique_data <- data %>%
    select(-url) %>%
    unique() %>%
    left_join(
      .,
      data %>%
        group_by(
          developement, address, rooms_min, rooms_max,
          price_from, price_upto, postcode, developer
        ) %>%
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
