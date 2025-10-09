library(ggplot2)
library(dplyr)
library(tidyr)
library(rlang)

library(ggplot2)
library(dplyr)
library(tidyr)
library(rlang)
source("modules/metadata/metadata_processing.R") 

# Combined plot for all omics
plot_metadata_time_overlap <- function(metadata, omics_cols) {
  md <- prepare_metadata(metadata, omics_cols)
  if (is.null(md)) return(NULL)
  metadata_long <- md$metadata_long
  range_data <- md$range_data
  
  p <- ggplot(metadata_long, aes(x = Timepoint, y = id)) +
    geom_segment(data = range_data,
                 aes(x = min_pts, xend = max_pts, y = id, yend = id, 
                     color = omics_type),
                 linewidth = 1,
                 position = position_dodge(width = 0.9)) +
    geom_point(aes(color = omics_type, shape = omics_type),
               size = 3,
               position = position_dodge(width = 0.9),
               show.legend = TRUE) +
    scale_color_brewer(palette = "Set1", name = "Omics Type") +
    scale_shape_manual(values = 15:20, name = "Omics Type") +
    theme_minimal(base_size = 15) +
    theme(axis.title = element_blank(),
          axis.text.y = element_text(size = 9, color = "black"),
          legend.position = "right",
          plot.margin = margin(10, 20, 10, 20)) +
    labs(x = names(metadata)[2]) +
    ggtitle("Omics Time Overlap Plot (Combined)")
  
  return(p)
}


# Time Overlap plot height helper
get_time_overlap_plot_height <- function(metadata, omics_cols) {
  min_height <- 50
  if (ncol(metadata) < 2) return(min_height)
  
  id_col_name <- names(metadata)[1]
  valid_omics_cols <- omics_cols[omics_cols %in% names(metadata)]
  if (length(valid_omics_cols) == 0) return(min_height)
  
  metadata_long <- metadata %>%
    mutate(id = !!sym(id_col_name)) %>%
    pivot_longer(
      cols = any_of(valid_omics_cols),
      names_to = "omics_type",
      values_to = "available"
    ) %>%
    mutate(available = as.character(available)) %>%
    filter(tolower(available) %in% c("yes", "1", "true"))
  
  if (nrow(metadata_long) == 0) return(min_height)
  
  n_subjects <- metadata_long %>% pull(id) %>% unique() %>% length()
  px_per_subject <- 20
  calculated_height <- n_subjects * px_per_subject + 100 
  
  return(max(min_height, calculated_height))
}

