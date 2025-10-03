library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbeeswarm) 

plot_metadata_timeline_all <- function(metadata, omics_cols) {
  
  # 1. Reshape into long format
  metadata_long <- metadata %>%
    pivot_longer(
      cols = all_of(omics_cols),
      names_to = "omics_type",
      values_to = "available"
    ) %>%
    filter(available == "Yes")
  
  if (nrow(metadata_long) == 0) return(NULL)
  
  # 2. Compute min–max week per subject (for the grey background segments)
  range_data <- metadata_long %>%
    group_by(id) %>%
    summarise(min_pts = min(Week), max_pts = max(Week), .groups = "drop")
  
  # 3. Order subjects nicely (for the single continuous y-axis)
  id_order <- metadata_long %>%
    group_by(id, Group) %>%
    summarise(n_timepoints = n(), .groups = "drop") %>%
    arrange(Group, desc(n_timepoints)) %>%
    pull(id)
  
  metadata_long$id <- factor(metadata_long$id, levels = id_order, ordered = TRUE)
  range_data$id <- factor(range_data$id, levels = id_order, ordered = TRUE)
  
  # 4. Group-based colors for y-axis text
  id_group <- metadata_long %>%
    select(id, Group) %>%
    distinct() %>%
    arrange(id)
  
  id_colored <- ifelse(id_group$Group == "FMT", "#ff7f0e", "#2ca02c")
  
  # 5. Plot everything in a SINGLE graph (NO FACETING)
  p <- ggplot(metadata_long, aes(x = Week, y = id)) +
    
    # The grey background segments
    geom_segment(data = range_data, aes(x = min_pts, xend = max_pts,
                                        y = id, yend = id),
                 linewidth = 5, color = "grey80") +
    
    # The data points. Color distinguishes omics type.
    # 🌟 FIX: Changed position_dodge2(height = 0.5) to position_dodge(width = 0.5)
    # This separates the points vertically on the discrete y-axis.
    geom_point(aes(color = omics_type, shape = omics_type),
               size = 3, alpha = 0.8,
               position = position_dodge(width = 0.5)) + 
    
    scale_color_brewer(palette = "Set1") +
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_text(color = id_colored),
      legend.position = "right"
    ) +
    
    labs(x = "Timeline (Weeks)") +
    
    ggtitle("Omics Timeline Plots (Combined)")
  
  return(p)
}