# metadata_contingency_table_helper.R

library(dplyr)
library(tidyr)
library(rlang)

#' Generates a contingency table of UNIQUE Subject Counts by a Grouping Column.
#'
#' @param metadata_long The long-format metadata with omics columns (1/TRUE/YES)
#' @param group_col_name The name of the grouping column
#' @param omics_cols Character vector of omics columns to count
#' @return Data frame of counts per group and per omics, with unique subject totals
generate_omics_count_table <- function(metadata_long, group_col_name, omics_cols) {
  
  # Debug
  message("DEBUG: Columns in metadata_long: ", paste(names(metadata_long), collapse = ", "))
  message("DEBUG: Requested group column: ", group_col_name)
  message("DEBUG: Omics columns: ", paste(omics_cols, collapse = ", "))
  
  # Check group column exists
  if (!(group_col_name %in% names(metadata_long))) {
    stop("ERROR: Selected group column not found in metadata_long.")
  }
  
  # Replace NA in group column with 'Missing'
  metadata_long <- metadata_long %>%
    mutate(Group = coalesce(as.character(.data[[group_col_name]]), "Missing"))
  
  # Initialize empty table
  count_list <- list()
  
  for (col in omics_cols) {
    if (!(col %in% names(metadata_long))) {
      warning(paste("Omics column not found:", col))
      next
    }
    
    # Count unique IDs per group where omics column is 1/TRUE/YES
    tmp <- metadata_long %>%
      filter(.data[[col]] %in% c(1, TRUE, "1", "TRUE", "Yes", "yes")) %>%
      group_by(Group) %>%
      summarise(!!col := n_distinct(id), .groups = "drop")
    
    count_list[[col]] <- tmp
  }
  
  # Merge all omics counts by Group
  if (length(count_list) == 0) return(NULL)
  
  count_table <- Reduce(function(x, y) full_join(x, y, by = "Group"), count_list)
  
  # Replace NAs with 0
  count_table[omics_cols] <- lapply(count_table[omics_cols], function(x) as.integer(replace_na(x, 0)))
  
  # Total subjects per group (unique IDs across all omics)
  total_per_group <- metadata_long %>%
    group_by(Group) %>%
    summarise(Total_Subjects_with_Omics = n_distinct(id), .groups = "drop")
  
  # Merge totals
  count_table <- left_join(count_table, total_per_group, by = "Group")
  
  # Grand total row
  grand_total <- tibble(
    Group = "Total",
    !!!setNames(
      lapply(omics_cols, function(col) sum(count_table[[col]])),
      omics_cols
    ),
    Total_Subjects_with_Omics = n_distinct(metadata_long$id)
  )
  
  # Bind total row
  count_table <- bind_rows(count_table, grand_total)
  
  return(count_table)
}
