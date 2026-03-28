library(rvest)
library(tidyverse)
library(purrr)

# Functions
scraping_pip = function(url_str){
  return(
    url_str %>%
      read_html() %>%
      html_elements("table") %>%
      html_table()
  )
}

# Handy function for processing the percentages
rate_cal = function(input_col){
  x = input_col
  no_percent_x = gsub("%", "", x)
  num_x = as.numeric(no_percent_x) / 100
  return(num_x)
}

###############################################
#                     SEP                     #
###############################################

# Define the URL
url <- "https://liquipedia.net/tft/Lore_%26_Legends/Tacticians_Crown/Statistics"

# Main Pipeline
test = url %>% scraping_pip()

# The main data with some processing
main_dt = test[[13]] %>%
  mutate("Win%" = rate_cal(`Win%`),
         "Top4%" = rate_cal(`Top4%`),
         "8th%" = rate_cal(`8th%`))

table(main_dt$Region, main_dt$`Top4%`)

# Points data

url_pt <- "https://liquipedia.net/tft/Lore_%26_Legends/Tacticians_Crown"
webpage <- read_html(url_pt)

# 1. Locate all rows in the standings panel
rows <- webpage %>% html_elements(".panel-table__row[data-js-battle-royale='row']")

# 2. Extract data from each row
# We use map_df to iterate through rows and create a data frame
standings_data <- map_df(rows, function(row) {
  
  # Extract Name
  name <- row %>% 
    html_element(".cell--team") %>% 
    html_attr("data-sort-val")
  
  # Extract Rank
  rank <- row %>% 
    html_element(".cell--rank") %>% 
    html_attr("data-sort-val")
  
  # Extract Total Points (The green highlighted part in your screenshot)
  points <- row %>% 
    html_element(".cell--total-points") %>% 
    html_attr("data-sort-val")
  
  # Return a single row data frame
  tibble(
    Rank = as.numeric(rank),
    Player = name,
    Total_Points = as.numeric(points)
  )
})

# 3. View the results
print(head(standings_data))