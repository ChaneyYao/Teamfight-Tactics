# Scraping data for patch control
library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(plotly)

comps_api = "https://api-hc.metatft.com/tft-comps-api/comps_stats?queue=1100&patch=current&days=1&rank=CHALLENGER,DIAMOND,EMERALD,GRANDMASTER,MASTER&permit_filter_adjustment=true"
comps_res = GET(comps_api)
comps_data = fromJSON(content(comps_res, as = "text", encoding = "UTF-8"))

comps_data$results$cluster %>% unique()

# 1. Define your unique cluster IDs
cluster_ids <- comps_data$results$cluster %>% unique()

# 2. Filter out empty strings or invalid markers like "-1"
valid_ids <- cluster_ids[cluster_ids != "" & cluster_ids != "-1"]

# 3. Create a function to fetch and parse data
fetch_comp_data <- function(id) {
  # Construct the URL
  # Note: Extracting the first 3 digits for cluster_id if it varies, 
  # otherwise keeping it static at 404 as per your examples.
  base_cluster <- substr(id, 1, 3)
  url <- paste0("https://api-hc.metatft.com/tft-comps-api/comp_details?comp=", 
                id, "&cluster_id=", base_cluster)
  
  message(paste("Fetching data for ID:", id))
  
  # Perform the GET request
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    return(fromJSON(content(response, as = "text", encoding = "UTF-8")))
  } else {
    warning(paste("Failed to fetch ID:", id, "Status:", status_code(response)))
    return(NULL)
  }
}

# 4. Execute the scrape
# Using set_names ensures the resulting list is indexed by the ID
all_cluster_data <- valid_ids %>%
  set_names() %>%
  map(~{
    Sys.sleep(0.5) # Optional: Be polite to the API to avoid rate limits
    fetch_comp_data(.x)
  })

# Changing the names of the comps in all_cluster_data

# Function to clean "TFT17_Illaoi" -> "Illaoi"
clean_unit_name <- function(name) {
  name %>%
    str_replace_all("TFT17_", "") %>%
    str_replace_all("PVE_", "")
}

# Create a lookup table for Cluster IDs to Champion Names
cluster_names <- imap_dfr(all_cluster_data, function(val, id) {
  # Access the unit stats
  units_df <- val$results$unit_stats
  
  if (!is.null(units_df) && nrow(units_df) > 0) {
    # 1. Clean the names
    # 2. Filter out non-champions (Minions, Summons, etc.)
    # 3. Take the first two
    top_units <- units_df$unit %>%
      clean_unit_name() %>%
      .[!str_detect(., "Minion|Summon|ElderDragon")] %>% 
      head(2)
    
    # Combine them into a single string: "Illaoi & Mordekaiser"
    comp_name <- paste(top_units, collapse = " & ")
    
    return(data.frame(cluster_id = id, comp_display_name = comp_name))
  } else {
    return(NULL)
  }
})

# 1. Combine all trends data frames into one large data frame
# imap_dfr is useful here: it iterates over the list and binds rows, 
# while .y represents the name (the Cluster ID)
plot_data <- imap_dfr(all_cluster_data, function(val, id) {
  # Extract the trends data frame
  trends <- val$results$trends
  
  # Only proceed if trends is a valid data frame and not empty
  if (!is.null(trends) && is.data.frame(trends) && nrow(trends) > 0) {
    trends$cluster_id <- id
    return(trends)
  } else {
    return(NULL)
  }
})

# 2. Pre-process the data
plot_data <- plot_data %>%
  mutate(
    # Convert the ISO 8601 string to a Date object
    date = as.Date(day),
    # Ensure avg is numeric
    avg = as.numeric(avg)
  ) %>%
  # Group by cluster to evaluate the "all days" condition
  group_by(cluster_id) %>%
  filter(
    all(avg < 5.5),      # Condition 1: Never worse than 5.5 avg
    all(pick > 0.01)     # Condition 2: Always > 1% playrate
  ) %>%
  ungroup()

# 1. Clean the data
plot_data_cleaned <- plot_data %>%
  mutate(
    # First, ensure it's a proper Date object for sorting
    date_obj = as.Date(day),
    # If you specifically want a string column with just MM-DD:
    month_day = format(date_obj, "%m-%d")
  ) %>%
  left_join(cluster_names, by = "cluster_id")

# 2. Plot with the cleaned X-axis labels

# Define the patch transition dates
patch_17_1b_date <- as.Date("2026-04-24")
patch_17_2_date <- as.Date("2026-04-29")

# 2. Plot with Vertical Patch Lines
ggplot(plot_data_cleaned, aes(x = date_obj, y = avg, group = cluster_id, color = cluster_id)) +
  geom_line(alpha = 0.4, size = 0.7) +
  
  # Add vertical lines for patch changes
  geom_vline(xintercept = c(patch_17_1b_date, patch_17_2_date), 
             linetype = "dashed", 
             color = "red", 
             size = 0.8) +
  
  # Add text labels for the patches
  annotate("text", x = patch_17_1b_date, y = 5.1, label = "17.1b", 
           color = "red", angle = 90, vjust = -0.5, fontface = "bold") +
  annotate("text", x = patch_17_2_date, y = 5.1, label = "17.2", 
           color = "red", angle = 90, vjust = -0.5, fontface = "bold") +
  
  # Reverse Y-axis (lower placement is better)
  scale_y_reverse() +
  
  # Formatting the labels
  scale_x_date(
    date_labels = "%m-%d", 
    date_breaks = "1 day"
  ) +
  labs(
    title = "AVP Trends by Cluster & Patch Changes",
    subtitle = "Vertical lines indicate patch deployments",
    x = "Date (MM-DD)",
    y = "Average Placement"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

library(plotly)

# 1. Save your plot as an object (p)
p <- ggplot(plot_data_cleaned, aes(x = date_obj, y = avg, group = comp_display_name, color = cluster_id)) +
  geom_line(alpha = 0.7, size = 1) +
  geom_vline(xintercept = c(patch_17_1b_date, patch_17_2_date), 
             linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", x = patch_17_1b_date, y = 5.1, label = "17.1b", 
           color = "red", angle = 90, vjust = -0.5) +
  annotate("text", x = patch_17_2_date, y = 5.1, label = "17.2", 
           color = "red", angle = 90, vjust = -0.5) +
  scale_y_reverse() +
  scale_x_date(date_labels = "%m-%d", date_breaks = "1 day") +
  theme_minimal() +
  theme(legend.position = "none")

# 2. Convert to interactive plot
# Use 'tooltip' to specify exactly what information pops up on hover
ggplotly(p, tooltip = c("group", "x", "y"))