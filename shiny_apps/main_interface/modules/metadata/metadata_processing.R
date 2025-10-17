library(dplyr)
library(tidyr)
library(rlang)
library(lubridate) # <--- CRITICAL ADDITION

# Prepare metadata for plotting
prepare_metadata <- function(metadata, omics_cols) {
  if (ncol(metadata) < 2) { 
    stop("Metadata must contain at least 2 columns (Patient ID, Timepoint).")
    return(NULL)
  }
  
  # DEBUG 1: Show raw names
  message("DEBUG METADATA (processing.R): Raw column names:")
  print(names(metadata)) 
  
  # CRITICAL FIX 1: Aggressively clean column names to prevent hidden character issues
  original_names <- names(metadata)
  clean_names <- make.names(original_names)
  names(metadata) <- clean_names
  
  # DEBUG 2: Show cleaned names
  message("DEBUG METADATA (processing.R): Cleaned column names:")
  print(names(metadata))
  id_col_name <- names(metadata)[1]
  timepoint_col_name <- names(metadata)[2]
  
  valid_omics_cols <- omics_cols[omics_cols %in% names(metadata)]
  if (length(valid_omics_cols) == 0) {
    warning("No valid omics columns found in metadata.")
    return(NULL)
  }
  
  metadata_prep <- metadata %>%
    mutate(
      id = as.character(!!sym(id_col_name)),
      Timepoint_raw = !!sym(timepoint_col_name) # Store raw value in a new column
    )
  
  # --- CRITICAL DATE/TIME HANDLING LOGIC ---
  
  # Determine if Timepoint_raw is already numeric or looks like simple numbers
  time_col_data <- metadata_prep %>% pull(Timepoint_raw)
  # Check if all non-NA values can be coerced to numeric without introducing NAs
  is_simple_numeric <- all(
    !is.na(suppressWarnings(as.numeric(na.omit(time_col_data))))
  )
  
  time_parsed <- NULL
  parsed_ratio <- 0
  
  # 1. Only attempt to parse if it is NOT a simple numeric column
  if (!is_simple_numeric) {
    time_parsed <- lubridate::parse_date_time(time_col_data, 
                                              orders = c("m/d/y H:M", "mdy HMS", "ymd", "mdy", "dmy", "H:M"))
    
    # Check if a majority of values were successfully parsed as dates
    parsed_ratio <- sum(!is.na(time_parsed)) / length(time_col_data)
  }
  
  if (parsed_ratio > 0.5) { 
    # CASE 1: IS DATE-TIME DATA. Calculate numeric days since start.
    metadata_prep <- metadata_prep %>%
      # Add the parsed Date-Time object
      mutate(Timepoint_parsed = time_parsed) %>%
      group_by(id) %>%
      # Find the earliest time point for each subject
      mutate(Start_Time = min(Timepoint_parsed, na.rm = TRUE)) %>%
      ungroup() %>%
      # Convert Timepoint to a numeric value: Days Since Start
      mutate(Timepoint = as.numeric(difftime(Timepoint_parsed, Start_Time, units = "days")))
    
  } else {
    # CASE 2: NOT DATE-TIME (e.g., "0", "1"). Use original coercion.
    # This prevents the 'NAs introduced by coercion' when the column is correctly numeric
    metadata_prep <- metadata_prep %>%
      mutate(Timepoint = as.numeric(Timepoint_raw)) 
  }
  
  # --- END CRITICAL LOGIC ---
  
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
  # Timepoint is now guaranteed to be numeric here, fixing the min/max warnings
  range_data <- metadata_long %>%
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