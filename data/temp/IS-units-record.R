library(httr)
library(jsonlite)
library(tidyverse)

# 1. Scraping data
url <- "https://api-hc.metatft.com/tft-comps-api/unit_items_processed"
res <- GET(url)
data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

unit_list = data$units

# 2. Transpose and Rectangle with Robust Error Handling
units_tidy <- enframe(unit_list, name = "unit", value = "details") %>%
  mutate(
    # Pulling rows safely
    count = map_dbl(details, ~ as.numeric(.x[[2]])), 
    place = map_dbl(details, ~ as.numeric(.x[[3]])), 
    average = map_dbl(details, ~ as.numeric(.x[[4]])), 
    pick = map_dbl(details, ~ as.numeric(.x[[5]])), 
    
    # Robust item handling
    items = map(details, function(df_list) {
      # Safety first: check if index 6 exists and is a dataframe
      item_df <- df_list[[6]]
      
      # The Fix: Handle NA/NULL/Empty values properly
      if (!is.data.frame(item_df) || nrow(item_df) == 0) {
        # Return a named vector of 10 NAs so the columns align
        return(setNames(as.list(rep(NA, 10)), paste0("item_", 1:10)))
      }
      
      # Convert the 10x1 dataframe column to a named list for unnest_wider
      setNames(as.list(item_df[[1]]), paste0("item_", seq_len(nrow(item_df))))
    })
  ) %>%
  # 3. Explode the items list into 10 separate columns
  unnest_wider(items) %>%
  filter(!is.na(item_1)) %>%
  select(-details) %>%
  
  # Only TFT16
  filter(str_detect(unit, "^TFT16")) %>%
  mutate(unit = str_remove(unit, "TFT16_"))

# 1. Define the Patch Version
patch_version <- "16.3"

# 2. Generate the timestamp in your specified format (YYYY-MM-DD-HHMM)
# %H%M will give you the 24-hour time format like 2048
current_time <- format(Sys.time(), "%Y-%m-%d-%H%M")

# Define your path and dynamic filename
saving_route <- "C:/Teamfight-Tactics/Teamfight-Tactics/data/processed/"
patch_version <- "16.3"
current_time <- format(Sys.time(), "%Y-%m-%d-%H%M")
file_name <- paste0(patch_version, "-", current_time, "-units.csv")

# 1. Combine them into a full path
full_path <- file.path(saving_route, file_name)

# 2. Save the data frame
write_csv(units_tidy, full_path)
