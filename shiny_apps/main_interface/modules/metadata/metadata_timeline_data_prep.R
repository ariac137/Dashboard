# timeline_data_prep.R

#' Helper function to set up coloring data, palettes, and y-axis colors.
#'
#' @param metadata The input metadata dataframe.
#' @param id_col_name The name of the ID column (first column).
#' @param id_order A vector of subject IDs in the desired plot order.
#' @param color_by_column The column name to use for coloring the strip and points.
#' @param metadata_long_facetted The long format data used for omics types.
#' @return A list containing:
#'         - strip_data_base (data.frame for the color strip base)
#'         - y_axis_colors (vector for y-axis label colors)
#'         - color_label (label for the color legend)
#'         - color_palette (named vector of color values)
#'         - fill_scale_layer (ggplot fill scale for geom_tile)
setup_plot_coloring <- function(metadata, id_col_name, id_order, color_by_column) {
  
  y_axis_colors <- NULL
  color_label <- NULL
  color_palette <- NULL
  fill_scale_layer <- NULL
  
  # Data frame to hold the ID and the color group for the vertical strip
  strip_data_base <- metadata %>%
    select(!!sym(id_col_name)) %>%
    distinct() %>%
    rename(id = !!sym(id_col_name))
  
  # Check if a valid column is provided for coloring
  if (!is.null(color_by_column) && color_by_column %in% names(metadata)) {
    
    # 1. Get the group for each ID
    color_data <- metadata %>%
      select(!!sym(id_col_name), !!sym(color_by_column)) %>%
      rename(id = !!sym(id_col_name), color_group = !!sym(color_by_column)) %>%
      distinct() %>%
      mutate(color_group = factor(color_group))
    
    # 2. Generate the color palette
    color_levels <- color_data$color_group %>%
      as.character() %>%
      na.omit() %>% 
      unique()
    
    n_colors <- length(color_levels)
    
    palette_values <- if (n_colors <= 9) RColorBrewer::brewer.pal(max(3, n_colors), "Set1") else grDevices::rainbow(n_colors)
    
    color_palette <- setNames(palette_values[1:n_colors], color_levels)
    color_label <- color_by_column
    
    # 3. Determine the color vector for y-axis labels
    y_axis_colors <- left_join(data.frame(id = id_order), color_data, by = "id") %>%
      pull(color_group) %>%
      as.character()
    
    y_axis_colors <- recode(
      y_axis_colors, 
      !!!color_palette,
      .default = "black" 
    )
    
    # 4. Finalize strip data base and fill scale layer
    strip_data_base <- strip_data_base %>%
      left_join(color_data, by = "id")
    
    fill_scale_layer <- scale_fill_manual(values = color_palette, name = color_label, guide = "none")
    
  } else {
    # Default white strip when no column is selected
    strip_data_base <- strip_data_base %>%
      mutate(color_group = "None")
    
    fill_scale_layer <- scale_fill_manual(values = c("None" = "white"), guide = "none")
  }
  
  return(list(
    strip_data_base = strip_data_base,
    y_axis_colors = y_axis_colors,
    color_label = color_label,
    color_palette = color_palette,
    fill_scale_layer = fill_scale_layer
  ))
}

#' Helper function to prepare the data for the vertical strip and the X-axis anchor.
#'
#' @param metadata_long_facetted The long format data used for omics types.
#' @param strip_data_base The base data frame for the strip (ID and color group).
#' @param first_omics_type_level The name of the first facet level.
#' @param omics_levels A vector of all unique omics levels (facets).
#' @param id_order A vector of subject IDs in the desired plot order.
#' @return A list containing:
#'         - strip_data (final data for geom_tile)
#'         - strip_x_pos (x-coordinate for the strip)
#'         - strip_width_val (width of the strip)
#'         - anchor_data (dummy data to extend the x-axis)
prepare_strip_and_anchor_data <- function(metadata_long_facetted, strip_data_base, first_omics_type_level, omics_levels, id_order) {
  
  # Get data for the first (leftmost) facet only
  first_facet_data <- metadata_long_facetted %>%
    filter(omics_type == first_omics_type_level)
  
  # Calculate min time and range for the first facet
  min_time_first_facet <- min(first_facet_data$Timepoint)
  time_range_diff_first_facet <- diff(range(first_facet_data$Timepoint))
  
  # Calculate position and width for the strip
  strip_offset_val <- max(1, time_range_diff_first_facet * 0.05) 
  strip_width_val <- max(0.2, time_range_diff_first_facet * 0.01)
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
#'
#' @param metadata_long_facetted_anchored The data with anchor points.
#' @param color_by_column The column name used for coloring.
#' @param id_col_name The ID column name.
#' @param time_col_name The time column name.
#' @param color_label The label for the color variable.
#' @return The data frame with the plot_tooltip_text column added.
add_tooltip_text <- function(metadata_long_facetted_anchored, strip_data_base, color_by_column, id_col_name, time_col_name, color_label) {
  
  if (!is.null(color_by_column)) {
    metadata_long_facetted_anchored <- metadata_long_facetted_anchored %>%
      left_join(strip_data_base %>% select(id, color_group), by = "id") %>%
      mutate(plot_tooltip_text = paste(
        !!sym(id_col_name), "<br>",
        time_col_name, ":", Timepoint, "<br>",
        "Omics Type:", omics_type, "<br>",
        color_label, ":", color_group
      ))
  } else {
    metadata_long_facetted_anchored <- metadata_long_facetted_anchored %>%
      mutate(plot_tooltip_text = paste(
        !!sym(id_col_name), "<br>",
        time_col_name, ":", Timepoint, "<br>",
        "Omics Type:", omics_type
      ))
  }
  
  return(metadata_long_facetted_anchored)
}