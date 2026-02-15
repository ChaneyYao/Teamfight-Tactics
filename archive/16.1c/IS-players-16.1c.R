library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

url_threshold <- "https://api.metatft.com/public/promotion_thresholds/latest"
threshold = fromJSON(content(GET(url_threshold), as = "text", encoding = "UTF-8"))

url_player_stat = "https://api.metatft.com/tft-leaderboard/v2/global?offset=0&limit=100"
player_stat = fromJSON(content(GET(url_test), as = "text", encoding = "UTF-8"))