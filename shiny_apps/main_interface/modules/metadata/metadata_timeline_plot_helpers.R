library(ggplot2)
library(dplyr)
library(tidyr)
library(rlang)

plot_metadata_timeline_all <- function(metadata, omics_cols) {
  
  # 1. Basic checks
  if (ncol(metadata) < 2) { 
    stop("Metadata must contain at least 2 columns (Patient ID, Timepoint).")
  }
  
  id_col_name <- names(metadata)[1]
  timepoint_col_name <- names(metadata)[2]
  
  # Keep only valid omics columns
  valid_omics_cols <- omics_cols[omics_cols %in% names(metadata)]
  if (length(valid_omics_cols) == 0) {
    warning("No valid omics columns found in metadata. Plot skipped.")
    return(NULL)
  }
  
  # 2. Prepare data
  metadata_prep <- metadata %>%
    mutate(
      id = !!sym(id_col_name),
      Timepoint = !!sym(timepoint_col_name)
    )
  
  # 3. Convert to long format (no anomalies)
  metadata_long <- metadata_prep %>%
    pivot_longer(
      cols = any_of(valid_omics_cols),
      names_to = "omics_type",
      values_to = "available"
    ) %>%
    mutate(available = as.character(available)) %>%
    filter(tolower(available) %in% c("yes", "1", "true"))   # Only keep available data points
  
  if (nrow(metadata_long) == 0) return(NULL)
  
  # 4. Compute min–max timepoints for grey timelines
  range_data <- metadata_long %>%
    mutate(Timepoint = as.numeric(Timepoint)) %>% 
    group_by(id) %>% 
    summarise(min_pts = min(Timepoint, na.rm = TRUE),
              max_pts = max(Timepoint, na.rm = TRUE), .groups = "drop")
  
  # 5. Order subjects by number of timepoints
  id_order <- metadata_long %>%
    group_by(id) %>% 
    summarise(n_timepoints = n(), .groups = "drop") %>%
    arrange(desc(n_timepoints)) %>% 
    pull(id)
  
  metadata_long$id <- factor(metadata_long$id, levels = id_order, ordered = TRUE)
  range_data$id <- factor(range_data$id, levels = id_order, ordered = TRUE)
  
  # 6. Create plot
  p <- ggplot(metadata_long, aes(x = Timepoint, y = id)) +
    
    # Grey background lines (timelines)
    geom_segment(data = range_data,
                 aes(x = min_pts, xend = max_pts, y = id, yend = id),
                 linewidth = 0.6, color = "grey80") +
    
    # Omics data points (fixed small size, no legend for size)
    geom_point(aes(shape = omics_type),
               size = 1.8,                # smaller fixed point size
               color = "black",
               fill = "grey40",
               position = position_dodge(width = 0.4),
               show.legend = TRUE) +
    
    scale_shape_manual(
      values = 15:20, 
      name = "Omics Type"
    ) +
    
    theme_minimal(base_size = 15) +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_text(size = 9, color = "black"),
      legend.position = "right",
      plot.margin = margin(10, 20, 10, 20)
    ) +
    labs(x = timepoint_col_name) +
    ggtitle("Omics Timeline Plot")
  
  # 7. Return the plot (you’ll control height externally)
  return(p)
}
