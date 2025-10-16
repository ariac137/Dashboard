# metadata_timepoint_stats_helpers.R

library(dplyr)
library(tidyr)
library(rlang)
library(scales) # Needed for percent formatting

#' Calculates the minimum and maximum number of samples (time points) collected 
#' across all individuals for each omics type, including the count and percentage 
#' of individuals achieving those min/max values.
#'
#' @param metadata The primary metadata data frame.
#' @param omics_cols A character vector of column names representing the omics types.
#' @return A data frame with Omics_Name, Min_Samples_Per_Individual, and Max_Samples_Per_Individual, 
#'         where the latter two columns are formatted text strings.
calculate_omics_time_stats <- function(metadata, omics_cols) {
  
  if (is.null(metadata) || ncol(metadata) < 2) return(NULL)
  
  # Determine the Subject ID column (Assumed to be the first column)
  id_col_name <- names(metadata)[1] 
  
  # Filter for omics columns that exist in the metadata
  valid_omics_cols <- omics_cols[omics_cols %in% names(metadata)]
  if (length(valid_omics_cols) == 0) return(NULL)
  
  # 1. Pivot to long format (ID, Omics, and Availability)
  time_data_long <- metadata %>%
    select(ID = !!sym(id_col_name), all_of(valid_omics_cols)) %>%
    pivot_longer(
      cols = all_of(valid_omics_cols),
      names_to = "Omics_Name",
      values_to = "available"
    ) %>%
    mutate(available = as.character(available)) %>%
    filter(tolower(available) %in% c("yes", "1", "true"))
  
  if (nrow(time_data_long) == 0) return(NULL)
  
  # 2. FIRST AGGREGATION: Calculate the sample count (N_Samples) for *each individual*
  individual_sample_counts <- time_data_long %>%
    group_by(Omics_Name, ID) %>%
    summarise(
      N_Samples = n(), # Count of samples/time points for this individual/omics pair
      .groups = 'drop_last' # Keep grouped by Omics_Name for the next step
    )
  
  # 3. SECOND AGGREGATION: Summarize and format the individual counts across all individuals
  omics_time_stats <- individual_sample_counts %>%
    # Calculate the total number of individuals contributing to this omics type
    mutate(Total_Individuals = n_distinct(ID)) %>%
    ungroup() %>%
    group_by(Omics_Name) %>%
    summarise(
      # Find Min and Max values
      Min_Samples = min(N_Samples, na.rm = TRUE),
      Max_Samples = max(N_Samples, na.rm = TRUE),
      
      # Calculate counts of individuals achieving Min/Max
      N_Min = sum(N_Samples == Min_Samples, na.rm = TRUE),
      N_Max = sum(N_Samples == Max_Samples, na.rm = TRUE),
      
      # Retain Total N
      Total_Individuals = first(Total_Individuals),
      .groups = 'drop'
    ) %>%
    
    # 4. FORMATTING STEP: Create the final descriptive text strings
    mutate(
      # Calculate percentages
      Perc_Min = scales::percent(N_Min / Total_Individuals, accuracy = 0.1),
      Perc_Max = scales::percent(N_Max / Total_Individuals, accuracy = 0.1),
      
      # Format Min column: "Count (N_Min Individuals, Perc_Min)"
      Min_Samples_Per_Individual = paste0(
        Min_Samples, 
        " (", N_Min, " | ", Perc_Min, ")"
      ),
      
      # Format Max column: "Count (N_Max Individuals, Perc_Max)"
      Max_Samples_Per_Individual = paste0(
        Max_Samples, 
        " (", N_Max, " | ", Perc_Max, ")"
      )
    ) %>%
    
    # Final Selection (only the requested output columns)
    select(Omics_Name, Min_Samples_Per_Individual, Max_Samples_Per_Individual)
  
  return(omics_time_stats)
}