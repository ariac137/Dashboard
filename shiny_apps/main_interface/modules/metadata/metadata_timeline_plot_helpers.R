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
generate_combined_timeline_plot <- function(metadata, omics_cols, 
                                            color_by_column = NULL, 
                                            point_color_by_column = NULL,
                                            continuous_to_categorical_threshold = 15,
                                            selected_point_palette = "Set1") {
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
  
  # 3. Determine Subject ID Order (Sorted by Omics Data Count) - Fallback
  id_order <- metadata_long_facetted %>%
    count(id, sort = TRUE) %>%
    pull(id) %>%
    as.character()
  
  # Helper to detect missing/empty values
  is_missing <- function(x) {
    is.na(x) | x == "" | tolower(as.character(x)) %in% c("none", "unknown", "missing", "na")
  }
  
  # --- 3.5 Setup Coloring for POINTS (Secondary Dropdown) and calculate ORDER early ---
  point_color_by_column_clean <- NULL
  secondary_group_order <- NULL
  point_color_setup_is_numeric <- FALSE
  
  if (!is.null(point_color_by_column)) {
    
    # NEW BLOCK: Check for numeric data and apply quartile binning
    point_col_data <- metadata %>% pull(!!sym(point_color_by_column))
    point_col_unique_non_na <- point_col_data %>% unique() %>% na.omit()
    
    # Check if data is numeric and has many unique values (e.g., > threshold)
    is_numeric_many_levels <- is.numeric(point_col_data) && length(point_col_unique_non_na) > continuous_to_categorical_threshold # USE NEW PARAMETER
    
    if (is_numeric_many_levels) {
      # Use the qtile function to get cut points based on non-missing data
      cut_points <- quantile(point_col_data, probs = seq(0, 1, 0.25), na.rm = TRUE)
      
      # Ensure unique cut points (important if many values are the same)
      cut_points <- unique(cut_points)
      
      # If less than 2 intervals are possible, treat as is
      if (length(cut_points) < 3) { # 3 unique points gives 2 intervals
        is_numeric_many_levels <- FALSE # Revert to treating as original
      } else {
        # Create quartile groups
        metadata <- metadata %>%
          mutate(
            # Apply cut function, creating a new column with factor labels
            !!paste0(point_color_by_column, "_quartile") := cut(
              !!sym(point_color_by_column), 
              breaks = unique(cut_points), 
              include.lowest = TRUE,
              right = FALSE, 
              dig.lab = 4  
            )
          )
        # Update column name to use the newly created quartile column
        point_color_by_column_actual <- paste0(point_color_by_column, "_quartile")
        point_color_setup_is_numeric <- FALSE # Treat the *new* binned data as categorical for coloring
      }
    }
    
    # If not binned, use the original column name
    if (!is_numeric_many_levels) {
      point_color_by_column_actual <- point_color_by_column
      # Use the same logic as strip_color to detect *actual* continuous data
      point_color_setup_is_numeric <- FALSE 
    }
    
    # FIX: Ensure only ONE point color group is selected per subject ID, 
    # even if the raw metadata has multiple timepoints/rows for that ID.
    id_to_point_group_raw <- metadata %>%
      select(id = !!sym(id_col_name), point_color_group_raw = !!sym(point_color_by_column_actual)) %>% # Use actual column
      # Group by ID and summarise to a single value (e.g., the first non-NA value)
      mutate(id = as.character(id)) %>%
      group_by(id) %>%
      summarise(point_color_group_raw = first(na.omit(point_color_group_raw)), .groups = 'drop')
    id_to_point_group <- id_to_point_group_raw %>%
      mutate(
        point_color_group_clean = as.character(point_color_group_raw),
        point_color_group_clean = gsub(",\\d+([\\)]?)$", "\\1", point_color_group_clean)
      ) %>%
      # Use "None" consistently for missing points
      mutate(point_color_group_clean = ifelse(
        is_missing(point_color_group_clean),
        "None", point_color_group_clean
      ))
    
    # Calculate secondary group order by GLOBAL COUNT (largest to smallest)
    secondary_group_sizes <- id_to_point_group %>%
      select(id, point_color_group_clean) %>% 
      distinct(id, point_color_group_clean) %>% 
      count(point_color_group_clean, name = "n") %>%
      arrange(desc(n))
    
    # --- DEBUG PRINT: Global Secondary Count --- (REMOVED)
    # cat("\n--- Secondary Group Counts (Global, by unique ID) ---\n")
    # print(secondary_group_sizes)
    # cat("---------------------------------------------------\n")
    
    secondary_group_order <- secondary_group_sizes$point_color_group_clean
    point_color_by_column_clean <- "point_color_group_clean"
  }
  
  # 4. Setup Coloring for STRIP and Y-AXIS (Subject-Level)
  strip_color_setup <- setup_plot_coloring(metadata, id_col_name, id_order, color_by_column)
  
  
  if (!is.null(color_by_column)) {
    
    # Count subjects per group, treating missing as "None"
    group_sizes <- metadata %>%
      select(id = !!sym(id_col_name), !!sym(color_by_column)) %>%
      distinct() %>%
      mutate(group_clean = ifelse(is_missing(!!sym(color_by_column)), "None", as.character(!!sym(color_by_column)))) %>%
      # Ensure we only count unique IDs per clean group
      select(id, group_clean) %>% 
      distinct(id, group_clean) %>% 
      count(group_clean, name = "n") %>%
      arrange(desc(n))  # largest → smallest (Primary Order)
    
    # --- DEBUG PRINT: Primary Count --- (REMOVED)
    # cat("\n--- Primary Group Counts (by unique ID) ---\n")
    # print(group_sizes)
    # cat("-------------------------------------------\n")
    
    group_order <- group_sizes$group_clean
    
    # Map IDs to cleaned groups (Primary Group)
    id_to_group <- metadata %>%
      select(id = !!sym(id_col_name), !!sym(color_by_column)) %>%
      mutate(id = as.character(id)) %>%
      distinct() %>%
      mutate(group_clean = ifelse(is_missing(!!sym(color_by_column)), "None", as.character(!!sym(color_by_column)))) %>%
      mutate(group_clean = gsub(",\\d+([\\)]?)$", "\\1", group_clean))
    
    # --- NEW: Reorder subjects by PRIMARY group, then by SECONDARY group (if selected) ---
    ordering_data <- id_to_group %>%
      mutate(group_clean = factor(group_clean, levels = group_order)) %>%
      arrange(group_clean) # Sorts by factor level (largest count first)
    
    if (!is.null(point_color_by_column_clean)) {
      
      # Calculate Secondary counts WITHIN Primary Group (Local Count)
      secondary_counts_by_primary <- id_to_group %>%
        left_join(id_to_point_group %>% select(id, secondary_group = point_color_group_clean), by = "id") %>%
        # Factor primary group by count for print order
        mutate(group_clean = factor(group_clean, levels = group_order)) %>%
        # Group by both cleaned columns
        group_by(group_clean, secondary_group) %>%
        # Count unique IDs in each combination
        summarise(n_subjects = n_distinct(id), .groups = 'drop') %>%
        # Sort first by primary group order, then by secondary count (largest to smallest)
        arrange(group_clean, desc(n_subjects))
      
      # --- DEBUG PRINT: Secondary count within Primary Group --- (REMOVED)
      # cat("\n--- Secondary Group Counts WITHIN Primary Group (by unique ID) ---\n")
      # print(secondary_counts_by_primary)
      # cat("------------------------------------------------------------------\n")
      
      # --- MODIFIED SORTING LOGIC: Use LOCAL COUNT (n_subjects) for tie-breaker ---
      
      # 1. Join the secondary group name (which is now guaranteed to be one per ID) into the ordering data
      ordering_data <- ordering_data %>%
        left_join(id_to_point_group %>% select(id, secondary_group = point_color_group_clean), by = "id") %>%
        
        # 2. Join in the local counts (n_subjects) using BOTH primary (group_clean) and secondary (secondary_group) names.
        left_join(secondary_counts_by_primary %>% select(group_clean, secondary_group, n_subjects), 
                  by = c("group_clean", "secondary_group")) %>%
        
        # 3. Sort by: Primary Group (by count), then LOCAL COUNT (n_subjects, largest first), then SECONDARY GROUP NAME (to group colors), then ID.
        arrange(group_clean, desc(n_subjects), secondary_group, id)
      
    } else {
      # If no secondary group, fall back to sorting by ID
      ordering_data <- ordering_data %>%
        arrange(group_clean, id)
    }
    
    id_order <- ordering_data %>%
      pull(id) %>%
      unique()
    
  } else if (!is.null(point_color_by_column_clean)) { 
    # --- NEW: Scenario B: Only Secondary (Point) Color Selected (No Primary Strip) ---
    
    # Reorder subjects based on secondary group size
    id_order <- id_to_point_group %>%
      mutate(point_color_group_clean = factor(point_color_group_clean, levels = secondary_group_order)) %>%
      arrange(point_color_group_clean, id) %>%
      pull(id) %>%
      unique()
    
  } # End of the ordering logic
  
  # 6. Prepare Strip and Anchor Data
  strip_anchor_prep <- prepare_strip_and_anchor_data(
    metadata_long_facetted, 
    strip_color_setup$strip_data_base, 
    first_omics_type_level, 
    omics_levels, 
    id_order
  )
  
  # --- FIX: Ensure IDs are comparable across datasets ---
  metadata_long_facetted <- metadata_long_facetted %>%
    mutate(id = as.character(id))
  strip_anchor_prep$anchor_data <- strip_anchor_prep$anchor_data %>%
    mutate(id = as.character(id))
  
  # 7. Combine anchor data with main data
  metadata_long_facetted_anchored <- bind_rows(metadata_long_facetted, strip_anchor_prep$anchor_data) %>%
    mutate(omics_type = factor(omics_type, levels = omics_levels)) # Re-factor
  
  # ---- Setup Coloring for Points based on SECOND dropdown ----
  if (!is.null(point_color_by_column_clean)) {
    
    # Determine the global order
    group_order <- secondary_group_order # Use the order calculated for coloring
    
    # Assign global factor levels to ensure consistency across all facets
    id_to_point_group <- id_to_point_group %>%
      mutate(point_color_group_clean = factor(point_color_group_clean, levels = group_order))
    
    # Join the coloring info into the long metadata
    metadata_long_facetted_anchored <- metadata_long_facetted_anchored %>%
      # FIX: Ensure we are joining the subject-level, single-color assignment
      left_join(id_to_point_group %>% select(id, point_color_group_clean), by = "id") %>%
      mutate(point_color_group = point_color_group_clean) %>%
      select(-point_color_group_clean)
    
    # Create color palette for plotting
    point_colors <- colorRampPalette(RColorBrewer::brewer.pal(
      n = min(12, RColorBrewer::brewer.pal.info[selected_point_palette, "maxcolors"]), 
      name = selected_point_palette # Use the new parameter here
    ))(length(group_order))
    names(point_colors) <- group_order
    
    point_color_setup <- list(
      color_palette = point_colors,
      color_label = point_color_by_column,
      is_numeric_data = FALSE
    )
    
  } else {
    # Default coloring if no second dropdown selected
    metadata_long_facetted_anchored <- metadata_long_facetted_anchored %>%
      mutate(point_color_group = factor("None"))
    
    point_color_setup <- list(
      color_palette = c("None" = "gray"),
      color_label = "None",
      is_numeric_data = FALSE
    )
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
  height_per_subject <- 15
  
  calculated_height <- base_height + (n_subjects_with_data * height_per_subject)
  
  return(max(min_height, calculated_height))
}