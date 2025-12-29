rm(list = ls())
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyverse)

previous_source = "C:/Teamfight-Tactics/Teamfight-Tactics/data/processed/20251228_all_regions_tft_data.rds"
now_source = "C:/Teamfight-Tactics/Teamfight-Tactics/data/processed/20251229_all_regions_tft_data.rds"

previous <- readRDS(previous_source)
now = readRDS(now_source)

emerging_players = now$tw2$data %>%
  mutate(now_rank = rank) %>%
  inner_join(previous$tw2$data %>%
               mutate(previous_rank = rank) %>%
               select(player_id, previous_rank), 
             by = "player_id") %>%
  select(player_id, riot_id, previous_rank, now_rank) %>%
  mutate(delta = now_rank - previous_rank) %>%
  filter(delta < -100) %>%
  arrange(now_rank)

ori = now$tw2$data

emerging_strat = emerging_players %>%
  inner_join(now$tw2$data %>%
               select(-c(riot_id, puuid, rating, rating_numeric, live, summoner_region, num_played, rank)), by = "player_id") 

# Create a standalone lookup table
carries_lookup <- emerging_strat %>%
  select(riot_id, stats) %>%
  mutate(carries = stats$RecentResult$topCarries) %>% # Dig directly to the list
  select(riot_id, carries) %>%
  unnest(carries)
