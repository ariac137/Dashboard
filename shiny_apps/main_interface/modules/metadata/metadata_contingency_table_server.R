library(DT)

source("modules/metadata/metadata_processing.R")
source("modules/metadata/metadata_contingency_table_helper.R")
source("modules/metadata/metadata_contingency_table_ui.R")

metadataContingencyTableServer <- function(id, metadata_reactive, omics_names_reactive, group_col_reactive) {
  moduleServer(id, function(input, output, session) {
    
    selected_group_col <- reactive({
      group_col <- group_col_reactive()
      if (is.null(group_col) || group_col == "None") return(NULL)
      return(group_col)
    })
    
    omics_count_table_data <- reactive({
      req(metadata_reactive())
      group_col_name <- selected_group_col()
      if (is.null(group_col_name)) return(NULL)
      
      message(paste0("DEBUG 1: Starting omics_count_table_data for group column: ", group_col_name))
      
      # Prepare metadata long
      md_prep <- prepare_metadata(metadata_reactive(), omics_names_reactive())
      if (is.null(md_prep$metadata_long) || nrow(md_prep$metadata_long) == 0) return(NULL)
      metadata_long <- md_prep$metadata_long
      
      # NEW: Use the CLEANED metadata object to get the cleaned ID name.
      cleaned_metadata <- md_prep$metadata # metadata_reactive() after cleaning
      cleaned_id_col_name <- names(cleaned_metadata)[1] # e.g., "Participant.ID"
      time_col_name <- names(cleaned_metadata)[2] # e.g., "Month"
      
      # Prepare group_data
      group_data <- cleaned_metadata %>%
        # CRITICAL FIX 1: Use the cleaned_id_col_name for selection/renaming
        select(
          id = !!sym(cleaned_id_col_name), 
          Timepoint, # <--- Standardized numeric time column (Join Key 2)
          !!sym(time_col_name), # <--- Original time column (for completeness/future use)
          Group = !!sym(group_col_name)
        ) %>%
        # CRITICAL FIX 2: Force ID to be a character for clean joining
        mutate(id = as.character(id)) %>% 
        # Only keep unique subject-group combinations
        distinct()
      
      # DEBUG BREAKPOINT A: Check group_data before join
      message(paste0("DEBUG 2: group_data columns: ", paste(names(group_data), collapse = ", ")))
      # browser() 
      
      # Join group to long metadata
      metadata_long <- metadata_long %>%
        left_join(group_data, by = c("id", "Timepoint"))
      
      # DEBUG BREAKPOINT B: Check metadata_long after join
      # message(paste0("DEBUG 3: metadata_long columns after join: ", paste(names(metadata_long), collapse = ", ")))
      # browser()
      
      # Pivot long to wide so omics types become columns
      metadata_wide <- metadata_long %>%
        # FIX: Implement conditional select logic to handle both 'Group' and 'Group.y'
        {
          if ("Group.y" %in% names(.)) {
            # Scenario 1: Collision occurred, use Group.y
            message("Using Group.y for pivot.")
            select(., id, omics_type, available, Group = Group.y)
          } else {
            # Scenario 2: No collision, Group is already named correctly
            message("Using clean Group for pivot.")
            select(., id, omics_type, available, Group)
          }
        } %>% 
        distinct() %>%
        tidyr::pivot_wider(
          id_cols = c(id, Group), # Now 'Group' definitely exists
          names_from = omics_type,
          values_from = available,
          values_fn = list
        )
      
      # DEBUG BREAKPOINT C: Check metadata_wide after pivot
      message(paste0("DEBUG 4: metadata_wide columns after pivot: ", paste(names(metadata_wide), collapse = ", ")))
      message("DEBUG 4.1: metadata_wide head BEFORE list conversion (Omics columns should contain 'list' values):")
      print(head(metadata_wide))
      # browser()
      
      # Convert list-columns to 0/1 (TRUE/1/"Yes" or NON-NA -> 1L else 0L)
      
      # FIX (Part 5: Updated logic for case-insensitivity and non-NA check)
      # Check for any value that indicates availability (1, TRUE, or case-insensitive "yes")
      # OR check for any non-NA character string (assuming file name presence implies availability)
      metadata_wide <- metadata_wide %>%
        mutate(across(any_of(omics_names_reactive()), # Safely select existing omics columns
                      ~ sapply(., function(x) {
                        
                        # 1. Check for explicit TRUE/1/Yes (case-insensitive)
                        explicit_match <- any(tolower(as.character(x)) %in% c("1", "true", "yes"), na.rm = TRUE)
                        
                        # 2. Check for any non-NA non-empty character string (implies file path/name presence)
                        string_present <- is.character(x) && any(nchar(x) > 0 & !is.na(x))
                        
                        # If either condition is met, mark as available (1L), otherwise 0L
                        if (explicit_match || string_present) {
                          return(1L)
                        } else {
                          return(0L)
                        }
                      })))
      
      # DEBUG BREAKPOINT D: Check metadata_wide after 0/1 conversion
      message("DEBUG 5: metadata_wide head AFTER 0/1 conversion. Omics columns must now contain 0s or 1s:")
      print(head(metadata_wide))
      # browser() # Pause execution here to check data structure
      
      # Compute counts per group
      count_table <- metadata_wide %>%
        group_by(Group) %>%
        # FIX (Part 3): Use any_of() here as well for consistency and safety
        summarise(
          across(any_of(omics_names_reactive()), ~ sum(.)),
          Total_Subjects_with_Omics = n_distinct(id),
          .groups = "drop"
        ) %>%
        mutate(Group = as.character(Group))
      
      # DEBUG BREAKPOINT E: Check count_table before adding total row
      message("DEBUG 6: count_table BEFORE adding Total row. Check Group sums:")
      print(count_table)
      
      # Add total row
      total_row <- count_table %>%
        summarise(
          Group = "Total",
          across(any_of(omics_names_reactive()), ~ sum(.)), # FIX (Part 4): Use any_of()
          Total_Subjects_with_Omics = sum(Total_Subjects_with_Omics)
        )
      
      # Combine
      result_table <- bind_rows(count_table, total_row)
      
      message("DEBUG 7: Final result_table being returned:")
      print(result_table)
      # browser() # FINAL INSPECTION POINT: The full table before it goes to DT
      
      return(result_table)
    })
    
    # Render title
    output$table_title_ui <- renderUI({
      table_data <- omics_count_table_data()
      group_col_name <- selected_group_col()
      if (!is.null(table_data) && nrow(table_data) > 0) {
        h4(paste("Unique Subject Counts by Group:", group_col_name))
      } else {
        NULL
      }
    })
    
    # Renders the interactive table
    output$omics_count_table <- DT::renderDataTable({
      data <- omics_count_table_data()
      group_col_name <- selected_group_col() # Get the column name for the title
      
      validate(
        need(!is.null(group_col_name), "Select a 'Group' column to view the table."),
        need(!is.null(data) && nrow(data) > 0, "No subjects could be counted. Check metadata and omics files.")
      )
      
      # Ensure numeric columns are integers (This is custom logic that must be preserved)
      data <- data %>%
        mutate(across(where(is.numeric), as.integer))
      
      # *** APPLY THE COMMON FUNCTION HERE ***
      render_interactive_table(
        df = data,
        title = paste("Unique Subject Counts by Group:", group_col_name) # Use a dynamic title
      )
    })
    
    return(NULL)
  })
}
