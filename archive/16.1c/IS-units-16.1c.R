library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

url <- "https://api-hc.metatft.com/tft-comps-api/unit_items_processed"
res <- GET(url)
data <- fromJSON(content(res, as = "text", encoding = "UTF-8"))

units_df <- bind_rows(data$units) %>%
  select(unit, avg, pick) %>%
  mutate(unit = sub("^TFT\\d+_", "", unit)) %>%
  distinct(unit, .keep_all = TRUE)

ggplot(units_df, aes(x = pick, y = avg)) + 
  geom_point() + 
  geom_hline(yintercept = 4.5, color = "red") + 
  geom_text(aes(label = unit), hjust = -0.1, vjust = -0.5, size = 2.5)



number_of_all_games = data[["games"]][[1]]
unit_rawdata = data[["results"]]

cleaned_data = unit_rawdata[-c(7, 53), ] %>%
  mutate("clean_name" = sub("^TFT\\d+_", "", unit)) %>%
  relocate("clean_name", .before = places) %>%
  select(-unit)

number_of_champions = nrow(cleaned_data)

weighted_games_cal = function(places){
  weighted_games = 0
  for(i in 1:8){
    weighted_games = weighted_games + i * places[i]
  }
  return(weighted_games)
}

avg_cal = function(places){
  total_games = sum(places)
  result = weighted_games_cal(places) / total_games
  return(result)
}

winrate_cal = function(places){
  result = places[1] / sum(places)
  return(result)
}

frequency_cal = function(places){
  return(sum(places) / number_of_all_games)
}

# Creating the dataframe of our desired outputs

processed_data_list = list()

for (i in 1:number_of_champions){
  
  ## Aatrox_data = unit_rawdata$places[[1]]
  individual_data = cleaned_data$places[[i]]
  
  ## avg_cal(Aatrox_data), winrate_cal(Aatrox_data), frequency_cal(Aatrox_data)
  processed_data_list[[i]] = list(
    "average_place" = avg_cal(individual_data),
    "winrate" = winrate_cal(individual_data),
    "frequency" = frequency_cal(individual_data)
  )
}

processed_data_df <- do.call(rbind, lapply(processed_data_list, as.data.frame)) %>%
  mutate("unit" = cleaned_data$clean_name) %>%
  relocate(unit, .before = average_place)

ggplot(processed_data_df, aes(x = frequency, y = average_place)) + 
  geom_point() + 
  geom_hline(yintercept = 4.5, color = "red") + 
  geom_text(aes(label = unit), hjust = -0.1, vjust = -0.5, size = 2.5)

# Distribution of average places

## Computing confidence interval

### Basic statistics
x_bar <- mean(processed_data_df$average_place)
s <- sd(processed_data_df$average_place)
n <- length(processed_data_df$average_place)

### 99% confidence interval using normal approximation
error_margin <- qnorm(0.995) * s / sqrt(n)

lower_bound <- x_bar - error_margin
upper_bound <- x_bar + error_margin

ggplot(processed_data_df, aes(x = average_place)) + 
  geom_histogram(binwidth = 0.15, fill = "skyblue", color = "black") +
  geom_vline(xintercept = lower_bound, color = "red") +
  geom_vline(xintercept = upper_bound, color = "red") +
  geom_vline(xintercept = x_bar, color = "black") +
  labs(
    x = "Average Place",
    y = "count",
    title = "Average Place of Units"
  ) + 
  theme_minimal()
  