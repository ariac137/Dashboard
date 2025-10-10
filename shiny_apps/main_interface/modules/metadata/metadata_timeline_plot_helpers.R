# metadata_timeline_plot_helpers.R

library(ggplot2)
library(dplyr)
library(tidyr)
library(rlang)
library(plotly) 
library(RColorBrewer)

source("modules/metadata/metadata_processing.R")
source("modules/metadata/metadata_timeline_data_prep.R")
source("modules/metadata/metadata_timeline_plot_geom.R")

# Combined plot for all omics using facets
generate_combined_timeline_plot <- function(metadata, omics_cols, color_by_column = NULL, point_color_by_column = NULL) {
  # Defensive checks
  if (is.null(metadata) || ncol(metadata) < 2) return(NULL)
  
  id_col_name <- names(metadata)[1]
  time_col_name <- names(metadata)[2]
  
  # 1. Prepare the long data and range data
  md <- prepare_metadata(metadata, omics_cols)
  if (is.null(md) || is.null(md$metadata_long) || nrow(md$metadata_long) == 0) return(NULL)
  
  metadata_long_facetted <- md$metadata_long
  range_data_facetted <- md$range_data
  
  # 2. Factor Omics Types (Facets) and determine order
  omics_levels <- metadata_long_facetted$omics_type %>% unique() %>% sort()
  metadata_long_facetted$omics_type <- factor(metadata_long_facetted$omics_type, levels = omics_levels)
  first_omics_type_level <- levels(metadata_long_facetted$omics_type)[1]
  
  # 3. Determine Subject ID Order (Sorted by Omics Data Count)
  id_order <- metadata_long_facetted %>%
    count(id, sort = TRUE) %>%
    pull(id) %>%
    as.character()
  
  # 4. Setup Coloring for STRIP and Y-AXIS (Subject-Level)
  strip_color_setup <- setup_plot_coloring(metadata, id_col_name, id_order, color_by_column)
  
  # 5. Setup Coloring for POINTS (for palette, label, and numeric check)
  point_color_setup <- setup_plot_coloring(metadata, id_col_name, id_order, point_color_by_column)
  
  # 6. Prepare Strip and Anchor Data
  strip_anchor_prep <- prepare_strip_and_anchor_data(
    metadata_long_facetted, 
    strip_color_setup$strip_data_base, 
    first_omics_type_level, 
    omics_levels, 
    id_order
  )
  
  # 7. Combine anchor data with main data
  metadata_long_facetted_anchored <- bind_rows(metadata_long_facetted, strip_anchor_prep$anchor_data) %>%
    mutate(omics_type = factor(omics_type, levels = omics_levels)) # Re-factor
  
  # 8. Add Point Color Group (Joins the RAW column for Timepoint-Level coloring)
  if (!is.null(point_color_by_column)) {
    raw_point_color_data <- metadata %>%
      select(
        id = !!sym(id_col_name), 
        Timepoint = !!sym(time_col_name),
        !!sym(point_color_by_column)
      ) %>%
      rename(point_color_group = !!sym(point_color_by_column)) %>%
      # FIX: Ensure a truly unique set of join keys (id, Timepoint) 
      group_by(id, Timepoint) %>%
      slice_head(n = 1) %>% 
      ungroup()
    
    # Left join using both ID and Timepoint to get timepoint-specific colors
    metadata_long_facetted_anchored <- metadata_long_facetted_anchored %>%
      left_join(raw_point_color_data, by = c("id", "Timepoint"), relationship = "many-to-one") %>% 
      # Ensure factor conversion is possible, even for numeric data
      mutate(point_color_group = factor(point_color_group))
  } else {
    metadata_long_facetted_anchored <- metadata_long_facetted_anchored %>%
      mutate(point_color_group = "None")
  }
  
  # 9. Add Tooltip Text
  metadata_long_facetted_anchored <- add_tooltip_text(
    metadata_long_facetted_anchored, 
    strip_color_setup$strip_data_base, 
    color_by_column, 
    id_col_name, 
    time_col_name, 
    strip_color_setup$color_label
  )
  
  # 10. Build and Return Plot
  p_interactive <- build_interactive_timeline_plot(
    metadata_long_facetted_anchored, 
    range_data_facetted, 
    strip_anchor_prep$strip_data, 
    strip_anchor_prep$strip_x_pos, 
    strip_anchor_prep$strip_width_val, 
    strip_color_setup$fill_scale_layer, # Strip fill layer
    point_color_setup$color_palette,    # Point color palette
    point_color_setup$color_label,      # Point color label
    point_color_by_column,              # Controls point coloring logic
    strip_color_setup$y_axis_colors,    # Y-axis color vector
    id_order, 
    omics_levels, 
    time_col_name,
    point_color_setup$is_numeric_data # Pass the numeric data flag
  )
  
  return(p_interactive)
}


# --- Height calculation helper (unchanged) ---
get_timeline_plot_height <- function(metadata, omics_cols) {
  min_height <- 100
  if (is.null(metadata) || ncol(metadata) < 2) return(min_height)
  
  id_col_name <- names(metadata)[1]
  valid_omics_cols <- omics_cols[omics_cols %in% names(metadata)]
  if (length(valid_omics_cols) == 0) return(min_height)
  
  metadata_long <- metadata %>%
    mutate(id = !!sym(id_col_name)) %>%
    pivot_longer(
      cols = any_of(valid_omics_cols),
      names_to = "omics_type",
      values_to = "available"
    ) %>%
    mutate(available = as.character(available)) %>%
    filter(tolower(available) %in% c("yes", "1", "true"))
  
  if (nrow(metadata_long) == 0) return(min_height)
  
  n_subjects_with_data <- metadata_long %>%
    pull(id) %>%
    unique() %>%
    length()
  
  base_height <- 150 
  height_per_subject <- 25 
  
  calculated_height <- base_height + (n_subjects_with_data * height_per_subject)
  
  return(max(min_height, calculated_height))
}