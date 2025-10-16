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
  
  # --- Data Preparation ---
  actual_point_data <- filter(metadata_long_facetted_anchored, available != "Anchor")
  valid_segment_combinations <- actual_point_data %>%
    distinct(id, omics_type)
  range_data_to_plot <- range_data_facetted %>%
    semi_join(valid_segment_combinations, by = c("id", "omics_type"))
  
  # Legend aesthetic
  legend_aesthetic <- if (point_coloring_active && !use_continuous_scale) {
    quote(point_color_group)
  } else {
    quote(omics_type)
  }
  
  # --- Build ggplot ---
  p <- ggplot(metadata_long_facetted_anchored, aes(x = Timepoint, y = id)) +
    
    # Vertical color strip
    geom_tile(
      data = strip_data,
      aes(x = strip_x_pos, y = id, fill = color_group),
      width = strip_width_val,
      height = 0.9,
      inherit.aes = FALSE
    ) +
    fill_scale_layer +
    
    # Time range segments
    geom_segment(data = range_data_to_plot,
                 aes(x = min_pts, xend = max_pts, y = id, yend = id,
                     color = !!legend_aesthetic),
                 color = "gray60",
                 linewidth = 1,
                 show.legend = FALSE) +
    
    # Points
    geom_point(data = actual_point_data,
               aes(color = if (point_coloring_active) {
                 if (use_continuous_scale) as.numeric(as.character(point_color_group))
                 else point_color_group
               } else {
                 omics_type
               },
               text = plot_tooltip_text),
               size = 2) +
    
    # Color scales
    { if (point_coloring_active) {
      if (use_continuous_scale) {
        list(viridis::scale_color_viridis(discrete = FALSE, name = color_label))
      } else {
        list(
          scale_color_manual(values = color_palette, name = color_label),
          guides(color = guide_legend(override.aes = list(size = 5)))
        )
      }
    } else {
      omics_palette <- setNames(rep("gray30", length(omics_levels)), omics_levels)
      list(
        scale_color_manual(values = omics_palette, name = "Omics Type"),
        guides(color = guide_legend(override.aes = list(size = 5)))
      )
    }} +
    
    # Facets
    facet_wrap(~ omics_type, ncol = length(omics_levels),
               scales = "free_x",
               labeller = label_value) +
    
    # Theme
    theme_minimal(base_size = 15) +
    theme(axis.title = element_blank(),
          axis.text.y = element_text(size = 9,
                                     color = if (is.null(y_axis_colors)) "black" else y_axis_colors),
          panel.spacing.x = unit(0.2, "cm"),
          plot.margin = margin(t = 5, r = 20, b = 10, l = 20, unit = "pt"),
          plot.title = element_text(margin = margin(b = -10, unit = "pt"))) +
    labs(x = time_col_name) +
    ggtitle("Omics Timeline Plots (Faceted Comparison - Interactive)")
  
  # After ggplotly conversion
  p_interactive <- plotly::ggplotly(p, tooltip = "text", originalData = FALSE)
  
  # Hide geom_tile traces
  if (length(p_interactive$x$data) >= 1) {
    p_interactive$x$data[[1]]$showlegend <- FALSE
  }
  
  # Map unique categories to single legend entries
  # metadata_timeline_plot_geom.R (Around line 90)
  
  # Map unique categories to single legend entries
  for (i in seq_along(p_interactive$x$data)) {
    trace <- p_interactive$x$data[[i]]
    
    if (!is.null(trace$mode) && grepl("markers", trace$mode)) {
      # Get the original category
      clean_name <- trace$name
      
      # 1. Remove Plotly's index (e.g., stripping the trailing ", 1" from a Plotly trace name)
      # This handles both (Group,Index), 4 and (OmicsType), 4
      clean_name <- gsub("\\s*,\\s*\\d+$", "", clean_name)
      
      # 2. VITAL FIX: Remove any index that might have originated in the data itself.
      # This targets the index *inside* the name, like (Fullterm,1) or (Epigenetic,1)
      # The brackets are often part of the string when no variable is selected.
      clean_name <- gsub(",\\d+([\\)]?)$", "\\1", clean_name)
      
      # Force name and legendgroup to be exactly the category
      trace$name <- clean_name
      trace$legendgroup <- clean_name
      trace$showlegend <- !(clean_name %in% sapply(p_interactive$x$data[1:(i-1)], function(t) t$legendgroup))
      
      p_interactive$x$data[[i]] <- trace
    }
    
    # Segment traces: never show legend
    if (!is.null(trace$mode) && grepl("lines", trace$mode)) {
      trace$showlegend <- FALSE
      p_interactive$x$data[[i]] <- trace
    }
  }  
  
  # --- Customize y-axis tick colors ---
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