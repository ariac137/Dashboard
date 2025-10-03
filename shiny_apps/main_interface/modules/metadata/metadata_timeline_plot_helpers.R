library(ggplot2)
library(dplyr)
library(tidyr)
library(ggbeeswarm) 
library(rlang) 

plot_metadata_timeline_all <- function(metadata, omics_cols) {
  
  if (ncol(metadata) < 2) { 
    stop("Metadata must contain at least 2 columns after processing (Patient ID, Timepoint).")
    return(NULL)
  }
  
  # Indices are corrected to 1 and 2 because core_process_df removed the original first column.
  id_col_name <- names(metadata)[1]        # Column 1 (Original Col 2): Patient ID (Used for 'id')
  timepoint_col_name <- names(metadata)[2] # Column 2 (Original Col 3): Timepoint (Used for 'Timepoint')
  
  # Filter omics_cols to include ONLY columns that exist in the metadata
  valid_omics_cols <- omics_cols[omics_cols %in% names(metadata)]
  
  if (length(valid_omics_cols) == 0) {
    warning("No valid omics columns found in metadata. Plot skipped.")
    return(NULL)
  }
  
  # 2. Preparation with guaranteed column creation (KEEPING ALL COLUMNS)
  metadata_prep <- metadata %>%
    mutate(
      id = !!sym(id_col_name),             # Create 'id' from Patient ID (Col 1)
      Timepoint = !!sym(timepoint_col_name) # Create 'Timepoint' from Time (Col 2)
    )
  
  # 3. Reshape into long format
  metadata_long <- metadata_prep %>%
    pivot_longer(
      cols = any_of(valid_omics_cols), 
      names_to = "omics_type",
      values_to = "available"
    ) %>%
    mutate(available = as.character(available)) %>%
    # 🌟 NEW FIX: Create highlight column to control shape and size
    mutate(
      # 'Highlight' now defines the visual property of the point
      highlight_type = case_when(
        # Identify the specific anomaly (Proteomic is missing/0)
        tolower(omics_type) == "proteomic" & tolower(available) %in% c("0", "no", "false") ~ "Anomaly",
        # Identify all normally present data points
        tolower(available) %in% c("yes", "1", "true") ~ "Present",
        # All other missing data are excluded from the plot
        TRUE ~ "Absent"
      )
    ) %>%
    # Filter to plot only "Present" points and the "Anomaly" point(s)
    filter(highlight_type != "Absent")
  
  
  if (nrow(metadata_long) == 0) return(NULL)
  
  # 4. Compute min–max Timepoint per subject (Requires numeric timepoints)
  range_data <- metadata_long %>%
    mutate(Timepoint = as.numeric(Timepoint)) %>% 
    group_by(id) %>% 
    summarise(min_pts = min(Timepoint, na.rm = TRUE), max_pts = max(Timepoint, na.rm = TRUE), .groups = "drop")
  
  # 5. Order subjects (by number of timepoints)
  id_order <- metadata_long %>%
    group_by(id) %>% 
    summarise(n_timepoints = n(), .groups = "drop") %>%
    arrange(desc(n_timepoints)) %>% 
    pull(id)
  
  metadata_long$id <- factor(metadata_long$id, levels = id_order, ordered = TRUE)
  range_data$id <- factor(range_data$id, levels = id_order, ordered = TRUE) 
  
  # 6. Plot everything in a SINGLE graph
  p <- ggplot(metadata_long, aes(x = Timepoint, y = id)) +
    
    # The grey timeline background
    geom_segment(data = range_data, aes(x = min_pts, xend = max_pts,
                                        y = id, yend = id),
                 linewidth = 1, color = "grey80") + 
    
    # The omics data points: Use SHAPE for omics_type, but SIZE/ALPHA/FILL to highlight anomaly
    geom_point(aes(shape = omics_type, size = highlight_type, alpha = highlight_type),
               color = "black", # Color is fixed for now (reserved for future use)
               fill = "grey40", 
               position = position_dodge(width = 0.5)) + 
    
    # 🌟 NEW FIX: Manually set shapes for omics_type
    scale_shape_manual(
      values = 15:20, # Use a good set of distinctive shapes (e.g., squares, circles, triangles)
      name = "Omics Type"
    ) +
    
    # 🌟 NEW FIX: Use size to make the anomaly stand out
    scale_size_manual(
      values = c("Anomaly" = 7, "Present" = 3), # Anomaly is large, Present is normal size
      guide = "none" # Hide this legend, as the anomaly should be self-explanatory
    ) +
    
    # 🌟 NEW FIX: Use alpha (transparency) to make the anomaly stand out
    scale_alpha_manual(
      values = c("Anomaly" = 1, "Present" = 0.8), # Anomaly is fully opaque
      guide = "none"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      axis.title = element_blank(),
      axis.text.y = element_text(color = "black"), 
      legend.position = "right"
    ) +
    
    # X-axis label uses the original name of the timepoint column
    labs(x = timepoint_col_name) +
    
    ggtitle("Omics Timeline Plots (Combined with Anomaly Highlight)")
  
  return(p)
}