# metadata_timeline_plot_server.R

source("modules/metadata/metadata_timeline_plot_helpers.R")
source("modules/metadata/metadata_timeline_plot_ui.R")

metadataTimelinePlotsServer <- function(id, metadata_reactive, omics_names_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Determine the Subject ID and Timepoint column names
    id_col_name <- reactive({ names(metadata_reactive())[1] })
    time_col_name <- reactive({ names(metadata_reactive())[2] })
    
    subject_level_cols <- reactive({
      req(metadata_reactive())
      md <- metadata_reactive()
      
      id_col <- id_col_name()
      
      # Exclude ID, Timepoint, and Omics columns
      exclude_cols <- c(id_col, time_col_name(), omics_names_reactive())
      potential_cols <- names(md) %>% setdiff(exclude_cols)
      
      valid_cols <- potential_cols %>%
        purrr::map_chr(function(col) {
          
          # 1. Check for Subject-Level Constancy (Value must not change over time for a subject)
          is_constant_per_id <- md %>%
            select(!!sym(id_col), !!sym(col)) %>%
            group_by(!!sym(id_col)) %>%
            # Check for unique, non-NA values. Max should be 1 across all IDs.
            summarise(n_unique = n_distinct(!!sym(col), na.rm = TRUE), .groups = 'drop') %>%
            pull(n_unique) %>%
            max() <= 1
          
          # 2. Check for at least 2 unique values overall (to be useful for coloring)
          n_unique_overall <- md %>% pull(!!sym(col)) %>% unique() %>% na.omit() %>% length()
          
          if (is_constant_per_id && n_unique_overall >= 2) {
            return(col) 
          } else {
            return(NA_character_)
          }
        }) %>%
        stats::na.omit() %>%
        as.character()
      
      # Add "None" as the default selection option
      c("None", valid_cols)
    })
    
    # 1. Generate UI for Strip Color dropdown
    output$strip_color_column_ui <- renderUI({
      timelineColorDropdownUI(
        session$ns, 
        "strip_color_column", 
        "Primary Outcome", 
        subject_level_cols() # Use limited list
      )
    })
    
    # 2. Generate UI for Point Color dropdown
    output$point_color_column_ui <- renderUI({
      
      # The dropdown UI
      dropdown_ui <- timelineColorDropdownUI(
        session$ns, 
        "point_color_column", 
        "Secondary Variable", 
        subject_level_cols()
      )
      
      # Text to inform the user about the quartile binning
      help_text <- p(
        em("(If you choose a variable that has more than 15 categories such as BMI or Age, it would automatically split into 4 quantiles for comparison)"),
        style = "font-size: 0.85em; color: #555;"
      )
      
      # Combine the dropdown and the help text
      tagList(
        dropdown_ui,
        help_text
      )
    })
    
    # 3. Filtered/Processed metadata (currently no filtering, just pass-through)
    filtered_metadata_reactive <- reactive({
      req(metadata_reactive())
      metadata_reactive()
    })
    
    # Reactive to track the selected strip color column (exported for the table module)
    group_col_reactive <- reactive({
      if (is.null(input$strip_color_column) || input$strip_color_column == "None") NULL else input$strip_color_column
    })
    
    # 4. Reactive to generate the plot
    plots_single <- reactive({
      req(filtered_metadata_reactive())
      req(omics_names_reactive())
      
      # Get the column names from the reactive values
      strip_color_col <- group_col_reactive()
      point_color_col <- if (is.null(input$point_color_column) || input$point_color_column == "None") NULL else input$point_color_column
      
      # NEW: Get selected palette, defaulting to "Set3"
      selected_palette <- input$point_color_palette %||% "Set3"
      
      generate_combined_timeline_plot(
        metadata = filtered_metadata_reactive(), 
        omics_cols = omics_names_reactive(),
        color_by_column = strip_color_col,        # Used for strip fill and y-axis color (Subject-Level)
        point_color_by_column = point_color_col,   # Used for point marker color (Timepoint-Level)
        selected_point_palette = selected_palette
      )
    })
    
    # 5. Reactive to calculate height dynamically
    plot_height <- reactive({
      req(filtered_metadata_reactive())
      req(omics_names_reactive())
      get_timeline_plot_height(filtered_metadata_reactive(), omics_names_reactive())
    })
    
    # 6. Dynamic UI rendering for the single faceted plot
    output$timeline_plot_container <- renderUI({
      final_plot <- plots_single()
      
      if (is.null(final_plot)) {
        return(p("No data points available to generate timeline plots."))
      }
      
      # Use renderPlotly for the interactive output
      output$combined_timeline_plot <- renderPlotly({ final_plot }) 
      
      # Use plotlyOutput with dynamic height
      plotlyOutput(session$ns("combined_timeline_plot"), height = plot_height()) 
    })
    
    # 7. Return the reactive plot object AND the selected group column
    return(list(
      plot = plots_single,
      group_col = group_col_reactive
    ))
  })
}