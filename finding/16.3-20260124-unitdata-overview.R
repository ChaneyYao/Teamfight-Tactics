rm(list = ls())
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(here)

source = "C:/Teamfight-Tactics/Teamfight-Tactics/data/processed/16.3-2026-01-29-2209-units.csv"

data = read.csv(source)

ggplot(data, aes(x = pick, y = average)) + 
  geom_point() + 
  geom_hline(yintercept = 4.5, color = "red", linetype = "dashed") + 
  geom_text(aes(label = unit), hjust = -0.1, vjust = -0.5, size = 2.5) +
  # Adjust these numbers to "crop" the plot to your cluster
  coord_cartesian(xlim = c(0.01, 0.5), ylim = c(3.6, 5.4)) +
  scale_y_reverse() # Units with average 3.0 will be at the top, 6.0 at the bottom

# 1. Define the Patch Version
patch_version <- "16.3"

# 2. Generate the timestamp in your specified format (YYYY-MM-DD-HHMM)
# %H%M will give you the 24-hour time format like 2048
current_time <- format(Sys.time(), "%Y-%m-%d-%H%M")

# Define your path and dynamic filename
saving_route <- "C:/Teamfight-Tactics/Teamfight-Tactics/data/processed/plots/"
patch_version <- "16.3"
current_time <- format(Sys.time(), "%Y-%m-%d-%H%M")
file_name <- paste0(patch_version, "-", current_time, "-units.png")

ggsave(file_name, plot = get_last_plot(), path = saving_route)
