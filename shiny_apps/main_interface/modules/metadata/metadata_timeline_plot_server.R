source("modules/metadata/metadata_timeline_plot_helpers.R")
source("modules/metadata/metadata_timeline_plot_ui.R")

metadataTimelinePlotsServer <- function(id, metadata_reactive, omics_names_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Determine the Subject ID and Timepoint column names
    id_col_name <- reactive({ names(metadata_reactive())[1] })
    time_col_name <- reactive({ names(metadata_reactive())[2] })
    
    # Reactive to identify suitable columns for coloring/grouping (categorical columns)
    categorical_cols <- reactive({
      req(metadata_reactive())
      md <- metadata_reactive()
      
      # Exclude ID, Timepoint, and Omics columns
      exclude_cols <- c(id_col_name(), time_col_name(), omics_names_reactive())
      
      # Filter for columns that are NOT excluded and have between 2 and 15 unique non-NA values
      valid_cols <- names(md) %>%
        setdiff(exclude_cols) %>%
        lapply(function(col) {
          # Count unique non-NA values
          n_unique <- md %>% pull(!!sym(col)) %>% unique() %>% na.omit() %>% length()
          if (n_unique > 1 && n_unique <= 15) col else NULL
        }) %>%
        unlist()
      
      # Add "None" as the default selection option
      c("None", valid_cols)
    })
    
    # 1. Generate UI for Coloring dropdown
    output$color_column_ui <- renderUI({
      colorColumnUI(session$ns, categorical_cols())
    })
    
    # 2. Filtered/Processed metadata (currently no filtering, just pass-through)
    filtered_metadata_reactive <- reactive({
      req(metadata_reactive())
      metadata_reactive()
    })
    
    # 3. Reactive to generate the SINGLE faceted plot (which now includes the grouping plot)
    plots_single <- reactive({
      req(filtered_metadata_reactive())
      req(omics_names_reactive())
      
      # Pass the selected coloring column name to the helper function
      color_col <- if (is.null(input$color_column) || input$color_column == "None") NULL else input$color_column
      
      # Renamed function call
      generate_combined_timeline_plot(
        metadata = filtered_metadata_reactive(), 
        omics_cols = omics_names_reactive(),
        color_by_column = color_col
      )
    })
    
    # 4. Reactive to calculate height dynamically
    plot_height <- reactive({
      req(filtered_metadata_reactive())
      req(omics_names_reactive())
      get_timeline_plot_height(filtered_metadata_reactive(), omics_names_reactive())
    })
    
    # 5. Dynamic UI rendering for the single faceted plot
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
    
    # Return the reactive single plot object
    return(plots_single)
  })
}
