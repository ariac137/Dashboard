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
      
      # Identify ID and Time columns
      id_col_name <- names(metadata_reactive())[1]
      time_col_name <- names(metadata_reactive())[2]
      
      # Prepare group_data
      group_data <- metadata_reactive() %>%
        select(all_of(c(id_col_name, time_col_name, group_col_name))) %>%
        distinct() %>%
        rename(
          id = !!sym(id_col_name),
          Timepoint = !!sym(time_col_name),
          Group = !!sym(group_col_name)
        )
      
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
      # browser()
      
      # Convert list-columns to 0/1 (TRUE/1/"Yes" -> 1L else 0L)
      
      # FIX (Part 2): Remove the explicit select() before mutate and change all_of() to any_of()
      # to safely handle cases where omics_names_reactive() contains extraneous names (like 'OmicsType')
      metadata_wide <- metadata_wide %>%
        mutate(across(any_of(omics_names_reactive()), # Safely select existing omics columns
                      ~ sapply(., function(x) if(any(x %in% c(1, TRUE, "Yes"))) 1L else 0L)))
      
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
      
      # Add total row
      total_row <- count_table %>%
        summarise(
          Group = "Total",
          across(any_of(omics_names_reactive()), ~ sum(.)), # FIX (Part 4): Use any_of()
          Total_Subjects_with_Omics = sum(Total_Subjects_with_Omics)
        )
      
      # Combine
      bind_rows(count_table, total_row)
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
    
    # Render interactive table
    output$omics_count_table <- DT::renderDataTable({
      data <- omics_count_table_data()
      validate(
        need(!is.null(selected_group_col()), "Select a 'Group' column to view the table."),
        need(!is.null(data) && nrow(data) > 0, "No subjects could be counted. Check metadata and omics files.")
      )
      
      # Ensure numeric columns are integers
      data <- data %>%
        # Use any_of here too if omics_names_reactive() is used to select columns for type conversion
        mutate(across(where(is.numeric), as.integer))
      
      DT::datatable(
        data,
        rownames = FALSE,
        extensions = c('Buttons', 'ColReorder', 'Scroller'),
        filter = 'top',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          pageLength = 10,
          lengthMenu = c(5, 10, 20, 50)
        )
      )
    })
    
    return(NULL)
  })
}
