library(ggplot2)
library(dplyr)
library(tidyr)
library(rlang)
library(plotly) 
source("modules/metadata/metadata_processing.R") 

# Combined plot for all omics using facets
plot_metadata_timeline_faceted <- function(metadata, omics_cols) {
  # Defensive check for minimum metadata structure
  if (is.null(metadata) || ncol(metadata) < 2) return(NULL)
  
  # Define column names with fallback for safety
  id_col_name <- names(metadata)[1]
  time_col_name <- names(metadata)[2]
  
  # Prepare the long data (points) and range data (segments)
  md <- prepare_metadata(metadata, omics_cols)
  if (is.null(md)) return(NULL)
  
  # 1. Get ALL unique subject IDs from the ORIGINAL metadata to define the Y-axis
  all_subject_ids <- metadata %>% 
    pull(!!sym(id_col_name)) %>% 
    unique() %>%
    # Ensure IDs are ordered as factors
    factor(levels = .) 
  
  # 2. Apply the full factor levels to both dataframes
  metadata_long_facetted <- md$metadata_long %>%
    mutate(id = factor(id, levels = levels(all_subject_ids))) 
  
  range_data_facetted <- md$range_data %>%
    mutate(id = factor(id, levels = levels(all_subject_ids)))
  
  # --- Create Custom Tooltip Text for Interactivity ---
  # Define the tooltip for the points
  metadata_long_facetted <- metadata_long_facetted %>%
    mutate(tooltip_text = paste(
      "Subject ID:", id, "\n",
      "Omics Type:", omics_type, "\n",
      "Timepoint:", Timepoint, "\n",
      "Available:", available 
    ))
  
  # Define the tooltip for the segments (lines)
  range_data_facetted <- range_data_facetted %>%
    mutate(tooltip_text = paste(
      "Subject ID:", id, "\n",
      "Omics Type:", omics_type, "\n",
      "Time Range:", min_pts, "to", max_pts
    ))
  
  # 1. Calculate global time range across ALL omics types
  min_time <- min(metadata_long_facetted$Timepoint, na.rm = TRUE)
  max_time <- max(metadata_long_facetted$Timepoint, na.rm = TRUE)
  
  # Calculate 10% padding for the global limits
  padding <- (max_time - min_time) * 0.1
  x_limits <- c(min_time - padding, max_time + padding)
  
  # The main combined ggplot using facet_wrap
  p <- ggplot(metadata_long_facetted, aes(x = Timepoint, y = id)) +
    
    # 1. Draw the time range segment (the line)
    geom_segment(data = range_data_facetted,
                 aes(x = min_pts, xend = max_pts, y = id, yend = id, 
                     text = tooltip_text),
                 color = "steelblue",
                 linewidth = 1,
                 inherit.aes = FALSE) + 
    
    # 2. Draw the time points
    geom_point(aes(text = tooltip_text),
               color = "darkred", 
               size = 2) +
    
    # FIX 1: Force the Y-axis to include ALL ID factor levels
    scale_y_discrete(limits = levels(all_subject_ids)) + 
    
    # FIX for X-AXIS: Explicitly use coord_cartesian with global limits.
    # This aggressively forces all facets to share the same X-axis range and applies padding.
    coord_cartesian(xlim = x_limits) +
    
    # Facet by omics_type. 'scales = "fixed"' ensures axes are shared (now strongly enforced by coord_cartesian).
    facet_wrap(~ omics_type, ncol = length(unique(metadata_long_facetted$omics_type)), 
               scales = "fixed") + 
    
    theme_minimal(base_size = 15) +
    theme(axis.title = element_blank(),
          axis.text.y = element_text(size = 9, color = "black"),
          # *** KEY CHANGE: Reduced panel spacing for tighter plots ***
          panel.spacing.x = unit(0.2, "cm"), 
          plot.margin = margin(10, 20, 10, 20)) +
    
    # Use the defined time column name for the label
    labs(x = time_col_name) + 
    ggtitle("Omics Timeline Plots (Faceted Comparison - Interactive)")
  
  # --- KEY FOR INTERACTIVITY: Convert ggplot to plotly ---
  p_interactive <- ggplotly(p, tooltip = "text") %>%
    config(displayModeBar = TRUE, displaylogo = FALSE)
  
  return(p_interactive) # Return the interactive plotly object
}

# --- Height Calculation Helper ---
get_timeline_plot_height <- function(metadata, omics_cols) {
  min_height <- 100
  if (is.null(metadata) || ncol(metadata) < 2) return(min_height)
  
  id_col_name <- names(metadata)[1]
  
  # Count ALL unique subjects for height calculation
  n_subjects <- metadata %>% pull(!!sym(id_col_name)) %>% unique() %>% length()
  
  px_per_subject <- 10
  calculated_height <- n_subjects * px_per_subject + 150
  
  return(max(min_height, calculated_height))
}
