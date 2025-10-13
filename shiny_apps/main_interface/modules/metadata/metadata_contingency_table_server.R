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
      
      # Join group to long metadata
      metadata_long <- metadata_long %>%
        left_join(group_data, by = c("id", "Timepoint"))
      
      # Pivot long to wide so omics types become columns
      metadata_wide <- metadata_long %>%
        select(id, omics_type, available, Group) %>%
        distinct() %>%
        tidyr::pivot_wider(
          id_cols = c(id, Group),
          names_from = omics_type,
          values_from = available,
          values_fn = list
        )
      
      # Convert list-columns to 0/1 (TRUE/1/"Yes" -> 1L else 0L)
      metadata_wide <- metadata_wide %>%
        mutate(across(all_of(omics_names_reactive()), 
                      ~ sapply(., function(x) if(any(x %in% c(1, TRUE, "Yes"))) 1L else 0L)))
      
      # Compute counts per group
      count_table <- metadata_wide %>%
        group_by(Group) %>%
        summarise(
          across(all_of(omics_names_reactive()), ~ sum(.)),
          Total_Subjects_with_Omics = n_distinct(id),
          .groups = "drop"
        ) %>%
        mutate(Group = as.character(Group))  # Ensure Group is character
      
      # Add total row
      total_row <- count_table %>%
        summarise(
          Group = "Total",
          across(all_of(omics_names_reactive()), ~ sum(.)),
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
