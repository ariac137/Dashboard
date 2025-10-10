# timeline_plot_geom.R

library(viridis) 
library(ggplot2) 
library(plotly) 
library(dplyr) 

#' Constructs the faceted ggplot and converts it to an interactive plotly object.
build_interactive_timeline_plot <- function(
    metadata_long_facetted_anchored, range_data_facetted, 
    strip_data, strip_x_pos, strip_width_val, fill_scale_layer, 
    color_palette, color_label, point_color_by_column, y_axis_colors, 
    id_order, omics_levels, time_col_name, is_numeric_data) { 
  
  point_coloring_active <- !is.null(point_color_by_column)
  use_continuous_scale <- point_coloring_active && is_numeric_data
  
  # --- Data Preparation for Geoms ---
  actual_point_data <- filter(metadata_long_facetted_anchored, available != "Anchor")
  
  valid_segment_combinations <- actual_point_data %>%
    distinct(id, omics_type)
  
  range_data_to_plot <- range_data_facetted %>%
    semi_join(valid_segment_combinations, by = c("id", "omics_type"))
  
  
  # The main combined ggplot using facet_wrap
  p <- ggplot(metadata_long_facetted_anchored, aes(x = Timepoint, y = id)) +
    
    # 0. The Vertical Color Strip (geom_tile) - MUST BE FIRST
    geom_tile(
      data = strip_data,
      aes(x = strip_x_pos, y = id, fill = color_group),
      width = strip_width_val,
      height = 0.9,
      inherit.aes = FALSE
    ) +
    
    # Apply the fill scale
    fill_scale_layer + 
    
    # 1. Draw the time range segment 
    # CRITICAL FIX: Use group = 1 to force all segments into a SINGLE Plotly trace.
    geom_segment(data = range_data_to_plot,
                 aes(x = min_pts, xend = max_pts, y = id, yend = id, 
                     group = 1), # <-- FORCE ALL SEGMENTS TO ONE TRACE GROUP
                 color = "gray60",
                 linewidth = 1,
                 show.legend = FALSE) + # Hide the segment traces from the legend
    
    # 2. Draw the time points
    geom_point(data = actual_point_data,
               aes(color = if (point_coloring_active) {
                 if (use_continuous_scale) {
                   as.numeric(as.character(point_color_group))
                 } else {
                   point_color_group
                 }
               } else {
                 omics_type
               }, 
               text = plot_tooltip_text), 
               size = 3) +
    
    # 3. Color Scale for Points/Omics 
    { if (point_coloring_active) {
      if (use_continuous_scale) {
        list(
          viridis::scale_color_viridis(discrete = FALSE, name = color_label)
        )
      } else {
        list(
          scale_color_manual(values = color_palette, name = color_label),
          guides(color = guide_legend(override.aes = list(size = 5)))
        )
      }
    } else {
      # No custom color - use omics_type for legend
      n_omics <- length(omics_levels)
      omics_palette <- setNames(rep("gray30", n_omics), omics_levels)
      list(
        scale_color_manual(values = omics_palette, name = "Omics Type"),
        guides(color = guide_legend(override.aes = list(size = 5)))
      )
    }} +
    
    # Facet by omics_type to show plots side-by-side
    facet_wrap(~ omics_type, ncol = length(omics_levels),
               scales = "free_x",
               labeller = label_value) + 
    
    # --- Theme and Customization ---
    theme_minimal(base_size = 15) +
    theme(axis.title = element_blank(),
          axis.text.y = element_text(
            size = 9, 
            color = if (is.null(y_axis_colors)) "black" else y_axis_colors
          ),
          panel.spacing.x = unit(0.2, "cm"), 
          plot.margin = margin(t = 5, r = 20, b = 10, l = 20, unit = "pt"),
          plot.title = element_text(margin = margin(b = -10, unit = "pt"))
    ) +
    
    labs(x = time_col_name) + 
    ggtitle("Omics Timeline Plots (Faceted Comparison - Interactive)")
  
  # 4. Convert to Plotly
  p_interactive <- plotly::ggplotly(p, tooltip = "text", originalData = FALSE)
  
  # 5. CRITICAL FIX: Link the single segment trace to the first categorical legend item.
  
  # Determine the name of the first categorical legend item (the master toggle)
  master_group_name <- NULL
  if (point_coloring_active && !use_continuous_scale && length(color_palette) > 0) {
    # Custom color active: use the first color level name
    master_group_name <- names(color_palette)[1]
  } else if (!point_coloring_active && length(omics_levels) > 0) {
    # No custom color: use the first omics type name
    master_group_name <- omics_levels[1]
  }
  
  # Trace 1: geom_tile (Strip). Always hide from legend.
  if (length(p_interactive$x$data) >= 1) {
    p_interactive$x$data[[1]]$showlegend <- FALSE
  }
  
  # The segment trace is now the second trace (index 2) because `group = 1` forced it into a single trace.
  # Check if the trace at index 2 exists and is a line trace (which it should be).
  if (length(p_interactive$x$data) >= 2 && !is.null(p_interactive$x$data[[2]]$mode) && p_interactive$x$data[[2]]$mode == "lines") {
    segment_trace_index <- 2
    
    # Link this single segment trace to the master group name.
    if (!is.null(master_group_name)) {
      p_interactive$x$data[[segment_trace_index]]$legendgroup <- master_group_name
    }
    
    p_interactive$x$data[[segment_trace_index]]$showlegend <- FALSE
  }
  
  # Update Point Traces (starting from index 3)
  omics_name_map <- setNames(omics_levels, 1:length(omics_levels))
  
  for (i in 3:length(p_interactive$x$data)) {
    trace <- p_interactive$x$data[[i]]
    
    # Determine if the trace name is one of Plotly's numerical omics factor names
    is_omics_factor_name <- trace$name %in% names(omics_name_map)
    
    # 5B. Point Traces (mode="markers")
    if (!is.null(trace$mode) && trace$mode == "markers") {
      
      if (!point_coloring_active) {
        # No custom color: Rename and link to the omics name.
        if (is_omics_factor_name) {
          group_name <- omics_name_map[[trace$name]]
          p_interactive$x$data[[i]]$name <- group_name
          p_interactive$x$data[[i]]$legendgroup <- group_name
          p_interactive$x$data[[i]]$showlegend <- TRUE
        }
      } else if (!use_continuous_scale) {
        # Custom color (Categorical): Ensure the legend group is set.
        group_name <- trace$name
        p_interactive$x$data[[i]]$legendgroup <- group_name
        p_interactive$x$data[[i]]$showlegend <- TRUE
      } else {
        # Continuous color: No discrete legend, hide the trace.
        p_interactive$x$data[[i]]$showlegend <- FALSE
      }
    }
  }
  
  # 6. Use plotly::layout to set tick text colors individually
  p_interactive <- plotly::layout(
    p_interactive,
    yaxis = list(
      tickvals = 1:length(id_order),
      ticktext = id_order,
      tickfont = list(color = if (is.null(y_axis_colors)) "black" else y_axis_colors)
    )
  )
  
  return(p_interactive)
}