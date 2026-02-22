# utils_viz.R
library(ggplot2)

generate_tft_plot <- function(data, patch_version, plot_path) {
  current_time <- format(Sys.time(), "%Y-%m-%d-%H%M")
  
  p <- ggplot(data, aes(x = pick, y = average)) + 
    geom_point() + 
    geom_hline(yintercept = 4.5, color = "red", linetype = "dashed") + 
    geom_text(aes(label = unit), hjust = -0.1, vjust = -0.5, size = 2.5, check_overlap = TRUE) +
    scale_y_reverse() +
    coord_cartesian(xlim = c(0.01, 0.5), ylim = c(3.6, 5.4)) +
    labs(title = paste("TFT Patch", patch_version), subtitle = current_time) +
    theme_minimal()

  # Save the plot
  file_name <- paste0(patch_version, "-", current_time, "-units.png")
  full_path <- file.path(plot_path, file_name)
  
  if (!dir.exists(plot_path)) dir.create(plot_path, recursive = TRUE)
  ggsave(full_path, plot = p, width = 10, height = 7)
  
  message("Plot Saved: ", file_name)
  return(p)
}