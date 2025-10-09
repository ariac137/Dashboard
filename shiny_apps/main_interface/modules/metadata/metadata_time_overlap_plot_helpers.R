library(ggplot2)
library(dplyr)
library(tidyr)
library(rlang)
library(plotly) 
source("modules/metadata/metadata_processing.R") 

# Combined plot for all omics
plot_metadata_time_overlap <- function(metadata, omics_cols) {
  # Defensive check for minimum metadata structure
  if (is.null(metadata) || ncol(metadata) < 2) return(NULL)
  
  # Define column names with fallback for safety during plot generation
  id_col_name <- names(metadata)[1]
  time_col_name <- names(metadata)[2]
  
  # 1. Prepare base long data and range data using existing helper
  md <- prepare_metadata(metadata, omics_cols)
  if (is.null(md)) return(NULL)
  metadata_long <- md$metadata_long
  
  # 2. Manual Y-Axis Ordering and Offset Calculation (from user's logic)
  
  # a. Order IDs by number of timepoints
  id_order <- metadata_long %>%
    count(id, sort = TRUE) %>%
    pull(id)
  metadata_long$id <- factor(metadata_long$id, levels = id_order, ordered = TRUE)
  
  # b. Create a map for vertical offset based on omics type
  unique_omics <- unique(metadata_long$omics_type)
  offset_map <- setNames(
    seq(-0.3, 0.3, length.out = length(unique_omics)), # Wider range for clear separation
    unique_omics
  )
  
  # c. Calculate the new, manually offset Y position (y_offset)
  metadata_long <- metadata_long %>%
    mutate(y_offset = as.numeric(id) + offset_map[omics_type])
  
  # d. Re-compute line ranges using the new y_offset
  range_data <- metadata_long %>%
    group_by(id, omics_type) %>%
    summarise(
      min_pts = min(Timepoint, na.rm = TRUE),
      max_pts = max(Timepoint, na.rm = TRUE),
      y_offset = mean(y_offset), # Use mean since all points for this group have the same offset
      .groups = "drop"
    )
  
  # 3. Create Custom Tooltip Text for Interactivity (using the final data frames)
  
  # Tooltip for the time points (dots)
  metadata_long <- metadata_long %>%
    mutate(tooltip_text = paste(
      "Subject ID:", id, "\n",
      "Omics Type:", omics_type, "\n",
      "Timepoint:", Timepoint, "\n",
      "Available:", available 
    ))
  
  # Tooltip for the time ranges (segments/lines)
  range_data <- range_data %>%
    mutate(tooltip_text = paste(
      "Subject ID:", id, "\n",
      "Omics Type:", omics_type, "\n",
      "Time Range:", min_pts, "to", max_pts
    ))
  
  # 4. Calculate global time range for consistent X-axis
  min_time <- min(metadata_long$Timepoint, na.rm = TRUE)
  max_time <- max(metadata_long$Timepoint, na.rm = TRUE)
  padding <- (max_time - min_time) * 0.1
  x_limits <- c(min_time - padding, max_time + padding)
  
  
  # 5. Create the ggplot object using y_offset
  p <- ggplot(metadata_long, aes(x = Timepoint, y = y_offset, group = omics_type)) +
    
    # 1. Draw the time range segment (Line/Range)
    geom_segment(data = range_data,
                 aes(x = min_pts, xend = max_pts, 
                     y = y_offset, yend = y_offset, # Use y_offset
                     color = omics_type, 
                     group = omics_type,
                     text = tooltip_text),
                 linewidth = 0.7, 
                 inherit.aes = FALSE) +
    
    # 2. Draw the time points
    geom_point(aes(color = omics_type, shape = omics_type, 
                   group = omics_type,
                   text = tooltip_text),
               size = 1.5, 
               show.legend = TRUE) +
    
    # 3. Custom Y-Axis: Map the numeric y_offset back to the Subject ID factors
    scale_y_continuous(
      breaks = seq_along(levels(metadata_long$id)),
      labels = levels(metadata_long$id),
      # FIX: Reduce default expansion (padding) on the y-axis
      expand = expansion(mult = c(0.05, 0.05)) 
    ) +
    
    # Add padding to the X-axis (time) range
    scale_x_continuous(expand = expansion(mult = c(0.1, 0.1))) + 
    
    # Explicitly use coord_cartesian with global limits for consistent X-axis range
    coord_cartesian(xlim = x_limits) +
    
    scale_color_brewer(palette = "Set1", name = "Omics Type") +
    scale_shape_manual(values = 15:20, name = "Omics Type") +
    theme_minimal(base_size = 15) +
    theme(axis.title = element_blank(),
          axis.text.y = element_text(size = 9, color = "black"),
          legend.position = "right",
          # FIX 1: Reduced top margin
          plot.margin = margin(t = 5, r = 20, b = 10, l = 20, unit = "pt"),
          # FIX 2: Use a negative bottom margin on the title to pull the plot up
          plot.title = element_text(margin = margin(b = -10, unit = "pt"))) +
    # Use the safe time column name
    labs(x = time_col_name) +
    ggtitle("Omics Time Overlap Plot (Combined - Interactive)")
  
  # --- Convert ggplot to plotly ---
  p_interactive <- ggplotly(p, tooltip = "text") %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
  
  return(p_interactive) # Return the interactive plotly object
}


# Time Overlap plot height helper - relies on the number of unique subjects (ID factors)
get_time_overlap_plot_height <- function(metadata, omics_cols) {
  min_height <- 300
  if (is.null(metadata) || ncol(metadata) < 2) return(min_height)
  
  # Define ID column name with fallback for safety
  id_col_name <- names(metadata)[1]
  
  # Count ALL unique subjects for height calculation
  n_subjects <- metadata %>% pull(!!sym(id_col_name)) %>% unique() %>% length()
  
  # Using a slightly larger pixel count per subject to accommodate the manual separation
  px_per_subject <- 25 # Increased slightly for more vertical space per subject
  calculated_height <- n_subjects * px_per_subject + 100 # Reduced fixed padding from 150 to 100
  
  return(max(min_height, calculated_height))
}
