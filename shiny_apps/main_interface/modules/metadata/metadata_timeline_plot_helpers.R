library(ggplot2)
library(dplyr)
library(tidyr)
library(rlang)
source("modules/metadata/metadata_processing.R") 

# Combined plot for all omics using facets
plot_metadata_timeline_faceted <- function(metadata, omics_cols) {
  # This part is crucial: prepare_metadata must now ensure ALL IDs are present in metadata_long
  # Assuming prepare_metadata handles the initial metadata cleaning/selection. 
  md <- prepare_metadata(metadata, omics_cols)
  if (is.null(md)) return(NULL)
  
  # The 'id' column contains all subjects we want on the Y-axis
  all_subject_ids <- md$metadata_long %>% 
    pull(id) %>% 
    unique() %>%
    # Ensure ID column is a factor with all unique values for consistent ordering
    factor(levels = .) 
  
  # Modify the long data to ensure all subject IDs are present as factors
  metadata_long_facetted <- md$metadata_long %>%
    mutate(id = factor(id, levels = levels(all_subject_ids))) 
  
  # Ensure range data also uses the full factor levels for y-axis consistency
  range_data_facetted <- md$range_data %>%
    mutate(id = factor(id, levels = levels(all_subject_ids)))
  
  # The main combined ggplot using facet_wrap
  p <- ggplot(metadata_long_facetted, aes(x = Timepoint, y = id)) +
    
    # 1. Draw the time range segment (for subjects that have data)
    geom_segment(data = range_data_facetted,
                 aes(x = min_pts, xend = max_pts, y = id, yend = id),
                 color = "steelblue",
                 linewidth = 1) +
    
    # 2. Draw the time points (for subjects that have data)
    geom_point(color = "steelblue", size = 3) +
    
    # Facet by omics_type to show plots side-by-side
    facet_wrap(~ omics_type, ncol = length(unique(metadata_long_facetted$omics_type)), 
               scales = "free_x") + # Use free_x for appropriate time axis per omics
    
    theme_minimal(base_size = 15) +
    theme(axis.title = element_blank(),
          axis.text.y = element_text(size = 9, color = "black"),
          panel.spacing.x = unit(1, "cm"), # Add space between facets
          plot.margin = margin(10, 20, 10, 20)) +
    labs(x = names(metadata)[2]) +
    ggtitle("Omics Timeline Plots (Faceted)")
  
  return(p)
}

# --- Height Calculation Re-purposed for the single combined plot ---
# The logic remains the same to calculate height based on subject count.
get_timeline_plot_height <- function(metadata, omics_cols) {
  min_height <- 50
  if (ncol(metadata) < 2) return(min_height)
  
  id_col_name <- names(metadata)[1]
  
  # Count unique subjects across all relevant omics types
  n_subjects <- metadata %>% pull(!!sym(id_col_name)) %>% unique() %>% length()
  
  px_per_subject <- 8
  calculated_height <- n_subjects * px_per_subject + 100 # Add buffer for title/axes
  
  return(max(min_height, calculated_height))
}