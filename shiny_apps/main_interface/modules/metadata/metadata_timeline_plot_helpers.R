library(ggplot2)
library(dplyr)
library(tidyr)
library(rlang)
library(plotly) 
library(RColorBrewer) # Essential for generating a consistent color palette

source("modules/metadata/metadata_processing.R") 

# Combined plot for all omics using facets
plot_metadata_timeline_faceted <- function(metadata, omics_cols, color_by_column = NULL) {
  # Defensive check for minimum metadata structure
  if (is.null(metadata) || ncol(metadata) < 2) return(NULL)
  
  id_col_name <- names(metadata)[1]
  time_col_name <- names(metadata)[2]
  
  # Prepare the long data (points) and range data (segments)
  md <- prepare_metadata(metadata, omics_cols)
  if (is.null(md) || is.null(md$metadata_long) || nrow(md$metadata_long) == 0) return(NULL)
  
  metadata_long_facetted <- md$metadata_long
  range_data_facetted <- md$range_data
  
  # --- 0. Determine Subject ID Order (Sorted by Omics Data Count) ---
  # This order determines both the visual y-axis sequence and the color vector sequence.
  id_order <- metadata_long_facetted %>%
    count(id, sort = TRUE) %>%
    pull(id) %>%
    as.character()
  
  # --- Coloring Logic for Y-Axis Labels and Points ---
  y_axis_colors <- NULL
  color_label <- NULL
  
  # Check if a valid column is provided for coloring
  if (!is.null(color_by_column) && color_by_column %in% names(metadata)) {
    
    # 1. Select relevant data from the original metadata
    color_data <- metadata %>%
      select(id = !!sym(id_col_name), color_group = !!sym(color_by_column)) %>%
      distinct()
    
    # 2. Merge the coloring data into the long format data
    metadata_long_facetted <- metadata_long_facetted %>%
      left_join(color_data, by = "id") %>%
      # Ensure 'color_group' is a factor for consistent coloring
      mutate(color_group = factor(color_group))
    
    # Merge coloring data into range_data_facetted as well
    range_data_facetted <- range_data_facetted %>%
      left_join(color_data, by = "id") %>%
      mutate(color_group = factor(color_group))
    
    # Set the label for the legend
    color_label <- color_by_column
    
  } else {
    # If no column is selected, color_label remains NULL.
    metadata_long_facetted <- metadata_long_facetted
  }
  
  # --- Setup Factor Levels for Y-Axis ---
  all_subject_ids <- metadata_long_facetted %>% 
    pull(id) %>% 
    unique() %>%
    factor(levels = id_order) 
  
  metadata_long_facetted <- metadata_long_facetted %>%
    mutate(id = factor(id, levels = levels(all_subject_ids))) 
  
  range_data_facetted <- range_data_facetted %>%
    mutate(id = factor(id, levels = levels(all_subject_ids)))
  
  # --- Define Custom Tooltip Text BEFORE ggplot uses it ---
  metadata_long_facetted <- metadata_long_facetted %>%
    mutate(tooltip_text = paste(
      "Subject ID:", id, "\n",
      "Omics Type:", omics_type, "\n",
      "Timepoint:", Timepoint, "\n",
      "Available:", available,
      if (!is.null(color_label)) paste0("\n", color_label, ": ", color_group) else ""
    ))
  # --- END Tooltip Definition ---
  
  # --- Custom Color Palette Generation (Fixes RColorBrewer warning) ---
  custom_colors <- NULL
  if (!is.null(color_label)) {
    unique_groups <- unique(metadata_long_facetted$color_group)
    n_groups <- length(unique_groups)
    
    # Use a safe set of 12 colors from the Paired palette
    paired_palette_colors <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", 
                               "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", 
                               "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928")
    
    if (n_groups <= 12) {
      # Safely select colors without calling brewer.pal when n < 3
      custom_colors <- setNames(paired_palette_colors[1:n_groups], unique_groups)
    } else {
      # For >12 groups, generate a custom, sequential palette with lighter tones
      color_func <- colorRampPalette(c("#A6CEE3", "#6A3D9A"))
      generated_colors <- color_func(n_groups)
      custom_colors <- setNames(generated_colors, unique_groups)
    }
    
    # Create the color vector for the Y-axis labels
    id_color_map <- metadata_long_facetted %>%
      select(id, color_group) %>%
      distinct() %>%
      arrange(id) %>%
      # Ensure all IDs are included, even those without points (using NA for color_group)
      right_join(data.frame(id = levels(all_subject_ids)), by = "id") %>%
      mutate(color_code = custom_colors[as.character(color_group)])
    
    # Since all IDs must be in the axis, we need a complete map
    y_axis_colors <- id_color_map %>% 
      pull(color_code)
  }
  # --- END Custom Color Palette Generation ---
  
  # 1. Calculate global time range across ALL omics types
  min_time <- min(metadata_long_facetted$Timepoint, na.rm = TRUE)
  max_time <- max(metadata_long_facetted$Timepoint, na.rm = TRUE)
  
  padding <- (max_time - min_time) * 0.1
  x_limits <- c(min_time - padding, max_time + padding)
  
  # The main combined ggplot using facet_wrap
  p <- ggplot(metadata_long_facetted, aes(x = Timepoint, y = id)) +
    
    # 1. Draw the time range segment (CONDITIONAL LOGIC)
    {if (!is.null(color_label)) {
      # Case 1: Coloring by a variable (Use aesthetic mapping for Plotly toggling)
      # NOTE: The color group mapping here is what allows the line to toggle with the points
      geom_segment(data = range_data_facetted,
                   aes(x = min_pts, xend = max_pts, y = id, yend = id, color = color_group),
                   linewidth = 1.5,
                   alpha = 0.5, # Make the segment slightly lighter than the points
                   inherit.aes = FALSE,
                   show.legend = FALSE) # Do NOT show segment in the legend
    } else {
      # Case 2: No coloring variable selected (Use constant color OUTSIDE aes to suppress legend)
      geom_segment(data = range_data_facetted,
                   aes(x = min_pts, xend = max_pts, y = id, yend = id), 
                   color = "gray60",
                   linewidth = 1,
                   inherit.aes = FALSE)
    }} +
    
    # 2. Draw the time points (CONDITIONAL LOGIC)
    {if (!is.null(color_label)) {
      # Case 1: Coloring by a variable (Use aesthetic mapping for legend)
      geom_point(aes(text = tooltip_text, color = color_group), size = 3)
    } else {
      # Case 2: No coloring variable selected (Use constant color OUTSIDE aes to suppress legend)
      geom_point(aes(text = tooltip_text), color = "cornflowerblue", size = 3)
    }} +
    
    # 3. Apply Color Scale (CONDITIONAL LOGIC: Only apply when coloring by a variable)
    {if (!is.null(color_label)) list(
      # Use custom_colors generated above. This scale applies to BOTH geom_point and geom_segment.
      scale_color_manual(values = custom_colors, name = color_label)
    )} +
    
    # Force the Y-axis to include ALL ID factor levels from the data
    scale_y_discrete(limits = levels(all_subject_ids)) + 
    
    # Explicitly use coord_cartesian with global limits.
    coord_cartesian(xlim = x_limits) +
    
    # Facet by omics_type.
    facet_wrap(~ omics_type, ncol = length(unique(metadata_long_facetted$omics_type)), 
               scales = "fixed") + 
    
    theme_minimal(base_size = 15) +
    theme(axis.title = element_blank(),
          axis.text.y = element_text(
            size = 9, 
            # THIS IS THE KEY LINE: Dynamically set the color vector
            color = if (is.null(y_axis_colors)) "black" else y_axis_colors
          ),
          panel.spacing.x = unit(0.2, "cm"), 
          plot.margin = margin(t = 5, r = 20, b = 10, l = 20, unit = "pt"),
          plot.title = element_text(margin = margin(b = -10, unit = "pt"))) + 
    
    labs(x = time_col_name) + 
    ggtitle("Omics Timeline Plots (Faceted Comparison - Interactive)")
  
  # Convert ggplot to plotly for interactivity
  p_interactive <- ggplotly(p, tooltip = "text") %>%
    # Use `layout` to set tick text colors individually
    layout(yaxis = list(
      tickvals = 1:length(all_subject_ids),
      ticktext = levels(all_subject_ids),
      tickfont = list(color = if (is.null(y_axis_colors)) "black" else y_axis_colors)
    ))
  
  return(p_interactive) 
}

# --- Height Calculation Helper ---\
get_timeline_plot_height <- function(metadata, omics_cols) {
  min_height <- 100
  if (is.null(metadata) || ncol(metadata) < 2) return(min_height)
  
  id_col_name <- names(metadata)[1]
  
  # Count ALL unique subjects for height calculation
  n_subjects <- metadata %>% pull(!!sym(id_col_name)) %>% unique() %>% length()
  
  px_per_subject <- 10
  calculated_height <- n_subjects * px_per_subject + 100
  
  return(max(min_height, calculated_height))
}
