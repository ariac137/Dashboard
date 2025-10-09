# Plot each omics type separately
plot_metadata_timeline_per_omics <- function(metadata, omics_cols) {
  md <- prepare_metadata(metadata, omics_cols)
  if (is.null(md)) return(list())
  metadata_long <- md$metadata_long
  range_data <- md$range_data
  
  plots <- list()
  for (omics in unique(metadata_long$omics_type)) {
    md_sub <- metadata_long %>% filter(omics_type == omics)
    range_sub <- range_data %>% filter(omics_type == omics)
    
    p <- ggplot(md_sub, aes(x = Timepoint, y = id)) +
      geom_segment(data = range_sub,
                   aes(x = min_pts, xend = max_pts, y = id, yend = id),
                   color = "steelblue",
                   linewidth = 1) +
      geom_point(color = "steelblue", size = 3) +
      theme_minimal(base_size = 15) +
      theme(axis.title = element_blank(),
            axis.text.y = element_text(size = 9, color = "black"),
            plot.margin = margin(10, 20, 10, 20)) +
      labs(x = names(metadata)[2]) +
      ggtitle(paste("Omics Timeline Plot:", omics))
    
    plots[[omics]] <- p
  }
  
  return(plots)
}