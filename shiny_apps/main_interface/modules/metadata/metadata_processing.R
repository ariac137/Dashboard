library(dplyr)
library(tidyr)
library(rlang)

# Prepare metadata for plotting
prepare_metadata <- function(metadata, omics_cols) {
  if (ncol(metadata) < 2) { 
    stop("Metadata must contain at least 2 columns (Patient ID, Timepoint).")
    return(NULL)
  }
  
  id_col_name <- names(metadata)[1]
  timepoint_col_name <- names(metadata)[2]
  
  valid_omics_cols <- omics_cols[omics_cols %in% names(metadata)]
  if (length(valid_omics_cols) == 0) {
    warning("No valid omics columns found in metadata.")
    return(NULL)
  }
  
  metadata_prep <- metadata %>%
    mutate(
      id = !!sym(id_col_name),
      Timepoint = !!sym(timepoint_col_name)
    )
  
  metadata_long <- metadata_prep %>%
    pivot_longer(
      cols = any_of(valid_omics_cols),
      names_to = "omics_type",
      values_to = "available"
    ) %>%
    mutate(available = as.character(available)) %>%
    filter(tolower(available) %in% c("yes", "1", "true"))
  
  if (nrow(metadata_long) == 0) return(NULL)
  
  # Compute min–max timepoints per patient per omics
  range_data <- metadata_long %>%
    mutate(Timepoint = as.numeric(Timepoint)) %>% 
    group_by(id, omics_type) %>% 
    summarise(min_pts = min(Timepoint, na.rm = TRUE),
              max_pts = max(Timepoint, na.rm = TRUE), .groups = "drop")
  
  # Order patients by number of timepoints
  id_order <- metadata_long %>%
    group_by(id) %>% 
    summarise(n_timepoints = n(), .groups = "drop") %>%
    arrange(desc(n_timepoints)) %>% 
    pull(id)
  
  metadata_long$id <- factor(metadata_long$id, levels = id_order, ordered = TRUE)
  range_data$id <- factor(range_data$id, levels = id_order, ordered = TRUE)
  
  return(list(metadata_long = metadata_long, range_data = range_data))
}

