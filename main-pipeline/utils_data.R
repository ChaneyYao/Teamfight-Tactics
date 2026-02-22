# utils_data.R
library(httr)
library(jsonlite)
library(tidyverse)

scrape_tft_data <- function(patch_version, base_path) {
  url <- "https://api-hc.metatft.com/tft-comps-api/unit_items_processed"
  res <- GET(url)
  raw_data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))
  
  units_tidy <- enframe(raw_data$units, name = "unit", value = "details") %>%
    mutate(
      count   = map_dbl(details, ~ as.numeric(.x[[2]])), 
      place   = map_dbl(details, ~ as.numeric(.x[[3]])), 
      average = map_dbl(details, ~ as.numeric(.x[[4]])), 
      pick    = map_dbl(details, ~ as.numeric(.x[[5]])), 
      items   = map(details, function(df_list) {
        item_df <- df_list[[6]]
        if (!is.data.frame(item_df) || nrow(item_df) == 0) {
          return(setNames(as.list(rep(NA, 10)), paste0("item_", 1:10)))
        }
        setNames(as.list(item_df[[1]]), paste0("item_", seq_len(nrow(item_df))))
      })
    ) %>%
    unnest_wider(items) %>%
    filter(!is.na(item_1)) %>%
    select(-details) %>%
    filter(str_detect(unit, "^TFT16")) %>%
    mutate(unit = str_remove(unit, "TFT16_"))
  
  # Save the file
  current_time <- format(Sys.time(), "%Y-%m-%d-%H%M")
  file_name <- paste0(patch_version, "-", current_time, "-units.csv")
  full_path <- file.path(base_path, file_name)
  
  if (!dir.exists(base_path)) dir.create(base_path, recursive = TRUE)
  write_csv(units_tidy, full_path)
  
  message("CSV Saved: ", file_name)
  return(units_tidy) # Return the data so main.R can use it immediately
}