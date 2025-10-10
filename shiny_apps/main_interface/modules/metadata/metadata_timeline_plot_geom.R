# timeline_plot_geom.R

#' Constructs the faceted ggplot and converts it to an interactive plotly object.
#'
#' @param metadata_long_facetted_anchored The long data frame including the x-axis anchor points.
#' @param range_data_facetted The data for the time range segments.
#' @param strip_data The final data frame for the vertical color strip (geom_tile).
#' @param strip_x_pos The x-coordinate for the strip.
#' @param strip_width_val The width of the strip.
#' @param fill_scale_layer The ggplot fill scale layer for the strip.
#' @param color_palette A named vector of color values for points.
#' @param color_label The label for the color legend/variable.
#' @param color_by_column The column name used for coloring (or NULL).
#' @param y_axis_colors The color vector for y-axis labels.
#' @param id_order The ordered vector of subject IDs.
#' @param omics_levels The vector of all omics types (facet levels).
#' @param time_col_name The name of the time column.
#' @return An interactive plotly object.
build_interactive_timeline_plot <- function(
    metadata_long_facetted_anchored, range_data_facetted, 
    strip_data, strip_x_pos, strip_width_val, fill_scale_layer, 
    color_palette, color_label, color_by_column, y_axis_colors, 
    id_order, omics_levels, time_col_name) {
  
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
    geom_segment(data = range_data_facetted,
                 aes(x = min_pts, xend = max_pts, y = id, yend = id),
                 color = "gray60",
                 linewidth = 1) +
    
    # 2. Draw the time points (Filter out the dummy anchor points)
    geom_point(data = filter(metadata_long_facetted_anchored, available != "Anchor"),
               aes(color = if (!is.null(color_by_column)) color_group else NULL, 
                   text = plot_tooltip_text), 
               size = 3) +
    
    # 3. Color Scale for Points (Only apply if coloring is active)
    { if (!is.null(color_by_column)) {
      list(
        scale_color_manual(values = color_palette, name = color_label),
        guides(color = guide_legend(override.aes = list(size = 5)))
      )
    }} +
    
    # Facet by omics_type to show plots side-by-side
    facet_wrap(~ omics_type, ncol = length(omics_levels),
               scales = "free_x") + 
    
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
  
  # Convert to Plotly safely
  p_interactive <- plotly::ggplotly(p, tooltip = "text")
  
  # Use plotly::layout to set tick text colors individually (necessary for Plotly)
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