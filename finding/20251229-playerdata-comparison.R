library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyverse)

previous_source = "C:/Teamfight-Tactics/Teamfight-Tactics/data/processed/20251228_all_regions_tft_data.rds"
now_source = "C:/Teamfight-Tactics/Teamfight-Tactics/data/processed/20251229_all_regions_tft_data.rds"

previous <- readRDS(previous_source)
now = readRDS(now_source)

data = now$tw2$data %>%
  mutate(previous_rank = rank) %>%
  inner_join(previous$tw2$data %>%
               mutate(now_rank = rank) %>%
               select(player_id, now_rank), 
             by = "player_id") %>%
  select(riot_id, previous_rank, now_rank) %>%
  mutate(delta = now_rank - previous_rank) %>%
  filter(delta < -100)
