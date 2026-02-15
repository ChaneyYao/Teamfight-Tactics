library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

#url_threshold <- "https://api.metatft.com/public/promotion_thresholds/latest"
#threshold = fromJSON(content(GET(url_threshold), as = "text", encoding = "UTF-8"))

# Preparing for multiple regions IS

## Defining the region names
regions <- c("global","euw1", "kr", "na1", "tw2", "pbe")

## Setting the limit
limit_count <- 1000

## 使用 sprintf 格式化字串生成網址
standard_url = "https://api.metatft.com/tft-leaderboard/v2/%s?offset=0&limit=%d"
urls <- setNames(
  sprintf(standard_url, regions, limit_count),
  regions
)

# 定義抓取函數
fetch_data <- function(url) {
  tryCatch({
    # 這裡放入你原本的邏輯
    fromJSON(content(GET(url), as = "text", encoding = "UTF-8"))
  }, error = function(e) {
    message(paste("Error fetching:", url))
    return(NULL)
  })
}

# 執行批量抓取
# 這會回傳一個 List，裡面包含各個地區的 Data Frame
all_regions_data <- lapply(urls, fetch_data)

# 如果只是為了在 R 裡面後續分析，用這個最安全
saveRDS(all_regions_data, "all_regions_tft_data.rds")

?saveRDS

# 下次要用時，一行讀取，結構完全不變
# my_data <- readRDS("all_regions_tft_data.rds")

# 1. 設定存檔資料夾
target_dir <- "C:/Teamfight-Tactics/Teamfight-Tactics/data/processed"
if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)

# 2. 取得今天的日期並轉成 "YYYYMMDD" 格式
today_str <- format(Sys.Date(), "%Y%m%d") # 今天執行會產出 "20251228"

# 3. 使用 paste0 拼接檔名
# 結果會是: 20251228_all_regions_tft_data.rds
file_name <- paste0(today_str, "_all_regions_tft_data.rds")
full_path <- file.path(target_dir, file_name)

# 4. 存檔
saveRDS(all_regions_data, full_path)

message(paste("檔案已成功儲存至：", full_path))