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
  
  
  # Determine the aesthetic variable that drives the legend
  legend_aesthetic <- if (point_coloring_active && !use_continuous_scale) {
    # Categorical custom color: use point_color_group
    quote(point_color_group)
  } else {
    # Default or continuous color: use omics_type
    quote(omics_type)
  }
  
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
    # Use the same aesthetic to create grouped traces, but use a static `group` aesthetic 
    # to differentiate it from the points if needed, or stick to the shared aesthetic
    # to facilitate automatic grouping.
    geom_segment(data = range_data_to_plot,
                 aes(x = min_pts, xend = max_pts, y = id, yend = id, 
                     color = !!legend_aesthetic), # <--- USE LEGEND AESTHETIC FOR GROUPING
                 color = "gray60", # <--- VISUAL OVERRIDE (outside aes)
                 linewidth = 1,
                 show.legend = FALSE) + # Still hide segments from the legend
    
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
               size = 2) +
    
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
  
  # 5. CRITICAL FIX: Explicit Trace Linking 
  
  # Trace 1: geom_tile (Strip). Always hide from legend.
  if (length(p_interactive$x$data) >= 1) {
    p_interactive$x$data[[1]]$showlegend <- FALSE
  }
  
  omics_name_map <- setNames(omics_levels, 1:length(omics_levels))
  
  # Prepare to map trace name (Plotly's internal name) to the final, visible legend group name
  trace_to_legend_map <- list()
  
  # Step 5A: First pass to identify point traces and map their internal Plotly name to the final legend name.
  for (i in 2:length(p_interactive$x$data)) {
    trace <- p_interactive$x$data[[i]]
    is_point_trace <- !is.null(trace$mode) && trace$mode == "markers"
    
    if (is_point_trace) {
      group_name <- trace$name
      
      if (!point_coloring_active) {
        # No custom color: Legend by Omics Type.
        if (group_name %in% names(omics_name_map)) {
          final_group_name <- omics_name_map[[group_name]]
        } else {
          final_group_name <- group_name # Fallback
        }
      } else if (!use_continuous_scale) {
        # Custom color (Categorical): Legend by point_color_group.
        final_group_name <- group_name
      } else {
        # Continuous color: No discrete legend.
        final_group_name <- NULL
      }
      
      # Store the mapping
      if (!is.null(final_group_name)) {
        trace_to_legend_map[[group_name]] <- final_group_name
      }
      
      # Apply point trace updates (same as before)
      if (!is.null(final_group_name)) {
        p_interactive$x$data[[i]]$name <- final_group_name
        p_interactive$x$data[[i]]$legendgroup <- final_group_name
        p_interactive$x$data[[i]]$showlegend <- TRUE
      } else {
        p_interactive$x$data[[i]]$showlegend <- FALSE
      }
    }
  }
  
  # Step 5B: Second pass to find segment traces and link them to the correct group.
  for (i in 2:length(p_interactive$x$data)) {
    trace <- p_interactive$x$data[[i]]
    is_segment_trace <- !is.null(trace$mode) && trace$mode == "lines"
    
    if (is_segment_trace) {
      # Segment traces inherit the trace name from the aesthetic used:
      segment_internal_name <- trace$name
      
      # Find the final legend name this segment trace should be linked to
      if (segment_internal_name %in% names(trace_to_legend_map)) {
        linked_group_name <- trace_to_legend_map[[segment_internal_name]]
        
        # CRITICAL: Set the segment's legendgroup to match the point's
        p_interactive$x$data[[i]]$legendgroup <- linked_group_name
      }
      
      p_interactive$x$data[[i]]$showlegend <- FALSE # Segments should remain hidden from legend
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