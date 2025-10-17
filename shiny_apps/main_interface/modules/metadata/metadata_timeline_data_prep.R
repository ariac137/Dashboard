# timeline_data_prep.R

#' Helper function to set up coloring data, palettes, and y-axis colors.
setup_plot_coloring <- function(metadata, id_col_name, id_order, color_by_column) {
  
  y_axis_colors <- NULL
  color_label <- NULL
  color_palette <- NULL
  fill_scale_layer <- NULL
  is_numeric_data <- FALSE # FIX: Initialize variable here
  
  # Data frame to hold the ID and the color group for the vertical strip
  strip_data_base <- metadata %>%
    select(!!sym(id_col_name)) %>%
    distinct() %>%
    rename(id = !!sym(id_col_name)) %>%
    mutate(id = as.character(id))
  
  # Check if a valid column is provided for coloring
  if (!is.null(color_by_column) && color_by_column %in% names(metadata)) {
    
    # 1. Get ALL unique non-NA values from the entire column 
    color_levels_full <- metadata %>% 
      pull(!!sym(color_by_column)) %>% 
      as.character() %>% 
      unique() %>% 
      na.omit() %>% 
      sort()
    
    # Determine if data is numeric (to use viridis scale)
    is_numeric_data <- all(grepl("^-?[0-9,.]+$", color_levels_full)) && length(color_levels_full) > 15
    
    # Limit to 15 levels for the manual palette generation (for categorical data)
    color_levels_manual <- color_levels_full[1:min(length(color_levels_full), 15)]
    n_colors <- length(color_levels_manual)
    
    # *** NEW LOGIC STARTS HERE ***
    
    # 2. Determine the levels and number of colors
    color_levels_final <- color_levels_full # Use ALL levels as there is no 15 limit now
    num_levels <- length(color_levels_final)
    
    # 3. Generate the color palette
    if (!is_numeric_data && num_levels > 0) {
      
      # Use a qualitative palette for small groups (up to 12)
      if (num_levels <= 12) {
        palette_values <- RColorBrewer::brewer.pal(max(3, num_levels), "Set3")[1:num_levels]
      } else {
        # Use viridis for 13 or more levels to ensure 30+ distinct colors are available
        palette_values <- viridisLite::viridis(num_levels, option = "D")
      }
      
      # Assign the palette colors to the final level names
      color_palette <- setNames(palette_values, color_levels_final)
      
    } else {
      # For numeric data or no levels, the palette is handled by the geom file or NULL
      color_palette <- NULL
    }
    
    color_label <- color_by_column
    
    # 4. Get ID-level grouping data for STRIP and Y-AXIS (First Dropdown behavior)
    color_data_strip <- metadata %>%
      select(!!sym(id_col_name), !!sym(color_by_column)) %>%
      rename(id = !!sym(id_col_name), color_group = !!sym(color_by_column)) %>%
      mutate(id = as.character(id)) %>%
      # FIX: Group by ID, then take the FIRST NON-MISSING value for the subject-level strip color.
      group_by(id) %>%
      summarise(color_group = first(na.omit(color_group)), .groups = 'drop') %>% 
      mutate(id = as.character(id)) %>% 
      mutate(color_group = factor(color_group))
    
    # 5. Determine the color vector for y-axis labels
    y_axis_colors <- left_join(data.frame(id = as.character(id_order)), color_data_strip, by = "id") %>%
      pull(color_group) %>%
      as.character()
    
    # Map the Y-axis colors using the manual color palette
    y_axis_colors <- recode(
      y_axis_colors, 
      !!!color_palette,
      .default = "black" 
    )
    
    # 6. Finalize strip data base and fill scale layer
    strip_data_base <- strip_data_base %>%
      left_join(color_data_strip, by = "id")
    
    # guide = "none" ensures the fill legend for the strip is hidden
    fill_scale_layer <- scale_fill_manual(
      values = if (!is.null(color_palette)) color_palette else c("Default" = "white"), 
      name = color_label, 
      guide = "none"
    )
    
  } else {
    # Default white strip when no column is selected
    strip_data_base <- strip_data_base %>%
      mutate(color_group = "None")
    
    fill_scale_layer <- scale_fill_manual(values = c("None" = "white"), guide = "none")
    # is_numeric_data remains FALSE
  }
  
  return(list(
    strip_data_base = strip_data_base,
    y_axis_colors = y_axis_colors,
    color_label = color_label,
    color_palette = color_palette, 
    fill_scale_layer = fill_scale_layer,
    is_numeric_data = is_numeric_data # This is now always defined
  ))
}

#' Helper function to prepare the data for the vertical strip and the X-axis anchor.
prepare_strip_and_anchor_data <- function(metadata_long_facetted, strip_data_base, first_omics_type_level, omics_levels, id_order) {
  
  # Get data for the first (leftmost) facet only
  first_facet_data <- metadata_long_facetted %>%
    filter(omics_type == first_omics_type_level)
  
  # Calculate min time and range for the first facet
  min_time_first_facet <- min(first_facet_data$Timepoint)
  time_range_diff_first_facet <- diff(range(first_facet_data$Timepoint))
  
  # Calculate position and width for the strip
  strip_offset_val <- max(1, time_range_diff_first_facet * 0.05) 
  strip_width_val <- max(0.35, time_range_diff_first_facet * 0.05)
  strip_x_pos <- min_time_first_facet - strip_offset_val
  
  # 1. Create the final strip data: Join base strip data with ONLY the first omics_type.
  strip_data <- strip_data_base %>%
    # Anchor to the first facet only
    mutate(omics_type = first_omics_type_level) %>%
    # Ensure omics_type is a factor with ALL levels defined in the main plot data.
    mutate(omics_type = factor(omics_type, levels = omics_levels)) %>%
    # Ensure ID is factored correctly
    mutate(id = factor(id, levels = id_order))
  
  # 2. Create a **dummy anchor point**
  anchor_data <- data.frame(
    id = unique(metadata_long_facetted$id),
    Timepoint = strip_x_pos - (strip_width_val * 2), # Place it even further left
    omics_type = first_omics_type_level,
    available = "Anchor",
    min_pts = NA_real_,
    max_pts = NA_real_,
    value = NA_real_
  ) %>%
    mutate(
      id = factor(id, levels = id_order),
      omics_type = factor(omics_type, levels = omics_levels)
    )
  
  return(list(
    strip_data = strip_data,
    strip_x_pos = strip_x_pos,
    strip_width_val = strip_width_val,
    anchor_data = anchor_data
  ))
}

#' Helper function to create the tooltip text column.
add_tooltip_text <- function(metadata_long_facetted_anchored, strip_data_base, color_by_column, id_col_name, time_col_name, color_label) {
  
  if (!is.null(color_by_column)) {
    metadata_long_facetted_anchored <- metadata_long_facetted_anchored %>%
      left_join(strip_data_base %>% select(id, color_group), by = "id") %>%
      mutate(plot_tooltip_text = paste(
        id, "<br>",
        time_col_name, ":", Timepoint, "<br>",
        "Omics Type:", omics_type, "<br>",
        color_label, ":", color_group
      ))
  } else {
    metadata_long_facetted_anchored <- metadata_long_facetted_anchored %>%
      mutate(plot_tooltip_text = paste(
        id, "<br>",
        time_col_name, ":", Timepoint, "<br>",
        "Omics Type:", omics_type
      ))
  }
  
  return(metadata_long_facetted_anchored)
}