rm(list = ls())

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(purrr)

# Trying for all items

all_api = "https://api-hc.metatft.com/tft-stat-api/pbe-items?days=3"
all_res = GET(all_api)
all_data = fromJSON(content(all_res, as = "text", encoding = "UTF-8"))
all_results = all_data$results

all_item_df = all_results %>%
  mutate(ori_name = itemName) %>%
  separate(itemName, into = c("Patch", "Type", "Category", "Name"))

## 道具分類

### To identify Artifacts

for (i in 1:nrow(all_item_df)) {
  
  if (all_item_df$Category[i] != "Artifact"){
    all_item_df$Name[i] = all_item_df$Category[i]
    all_item_df$Category[i] = "Normal_Item"
  }
  
}

### To identify Emblems

for (i in 1:nrow(all_item_df)) {
  
  if (str_ends(all_item_df$Name[i], "EmblemItem")){
    all_item_df$Name[i] = sub("EmblemItem$", "", all_item_df$Name[i])
    all_item_df$Category[i] = "Emblem"
  }
  
}

### To identify Radiant items

for (i in 1:nrow(all_item_df)) {
  
  if (str_ends(all_item_df$Name[i], "Radiant")){
    all_item_df$Category[i] = "Radiant"
  }
  
}

## Helper functions
weighted_games_cal <- function(places) {
  sum((1:8) * places)
}

avg_cal <- function(places) {
  total <- sum(places)
  if (total == 0) return(NA)
  weighted_games_cal(places) / total
}

winrate_cal <- function(places) {
  total <- sum(places)
  if (total == 0) return(NA)
  places[1] / total
}

frequency_cal <- function(places) {
  sum(places) / all_data$games$count
}

# Process each unit
all_data_list <- list()

for (i in 1:nrow(all_item_df)) {
  places <- all_item_df$places[[i]]
  all_data_list[[i]] <- list(
    item = all_item_df$Name[i],
    average_place = avg_cal(places),
    winrate = winrate_cal(places),
    frequency = frequency_cal(places)
  )
}

# Convert to dataframe
processed_data_df <- bind_rows(all_data_list)

# Combining 
final_item_df = all_item_df %>%
  inner_join(processed_data_df, by = c("Name" = "item")) %>%
  relocate(ori_name, .before = Patch)

# ------------------------------------分隔線---------------------------------------

# Silvermere

svm_url <- "https://api-hc.metatft.com/tft-stat-api/pbe-item_detail?days=3&itemName=TFT_Item_Artifact_SilvermereDawn"
svm_res <- GET(svm_url)
svm_data <- fromJSON(content(svm_res, as = "text", encoding = "UTF-8"))

svm_data_all = svm_data[["units_overall"]][-c(1, 8, 35, 54), ]
svm_data_specific <- svm_data[["units"]]
svm_gmaes <- svm_data[["games"]][["count"]]

svm_name = svm_data$item

combined_svm_data = svm_data_all %>%
  inner_join(svm_data_specific, by = "unit", suffix = c("_all", "_svm")) %>%
  mutate(clean_name = sub("^TFT\\d+_", "", unit)) %>%
  relocate(clean_name, .before = places_all) %>%
  select(-unit) %>%
  mutate(
    avg_all = sapply(places_all, avg_cal),
    avg_svm = sapply(places_svm, avg_cal),
    place_change = avg_svm - avg_all,
    svm_games = sapply(places_svm, sum),
    item_name = svm_name
  )

# Sniper's Focus

spf_url = "https://api-hc.metatft.com/tft-stat-api/pbe-item_detail?days=3&itemName=
TFT9_Item_OrnnHorizonFocus"


# ---- Item list from your cleaned item names ----
item_list <- final_item_df$ori_name

# ---- Helper function for each item ----
fetch_item_unit_data <- function(item_name) {
  
  url <- paste0("https://api-hc.metatft.com/tft-stat-api/pbe-item_detail?days=3&itemName=", item_name)
  res <- tryCatch(GET(url), error = function(e) return(NULL))
  
  if (is.null(res) || http_error(res)) return(NULL)
  
  data <- tryCatch(fromJSON(content(res, as = "text", encoding = "UTF-8")), error = function(e) return(NULL))
  
  if (is.null(data$units) || is.null(data$units_overall)) return(NULL)
  
  # Remove some outlier rows as you did (assuming based on bad data?)
  units_all <- data[["units_overall"]][-c(1, 8, 35, 54), ]
  units_svm <- data[["units"]]
  item_name_clean <- data[["item"]]
  games_count <- data[["games"]][["count"]]
  
  df <- inner_join(units_all, units_svm, by = "unit", suffix = c("_all", "_item")) %>%
    mutate(
      clean_name = str_remove(unit, "^TFT\\d+_"),
      avg_all = sapply(places_all, avg_cal),
      avg_item = sapply(places_item, avg_cal),
      place_change = avg_item - avg_all,
      item_name = item_name_clean,
      games = sapply(places_item, sum)
    ) %>%
    relocate(clean_name, .before = places_all) %>%
    select(-unit)
  
  return(df)
}

spf_data = fetch_item_unit_data(item_list[68])

# ---- Map over all items ----
all_item_unit_dfs <- map_dfr(item_list, fetch_item_unit_data)

# Feel free to check

selected_all_item_unit_dfs = all_item_unit_dfs %>%
  select(-c(places_all, places_item)) %>%
  filter(games > 20000) %>%
  filter(place_change < -0.6)

battle_academia = all_item_unit_dfs %>%
  select(-c(places_all, places_item)) %>%
  inner_join(final_item_df, by = c("item_name" = "ori_name")) %>%
  filter(games > 1000) %>%
  filter(Name == "BattleAcademia")
 