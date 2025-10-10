source("modules/metadata/metadata_timeline_plot_helpers.R")
source("modules/metadata/metadata_timeline_plot_ui.R")

metadataTimelinePlotsServer <- function(id, metadata_reactive, omics_names_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Determine the Subject ID and Timepoint column names
    id_col_name <- reactive({ names(metadata_reactive())[1] })
    time_col_name <- reactive({ names(metadata_reactive())[2] })
    
    # Reactive to identify SUITABLE CATEGORICAL columns for the STRIP (First Dropdown)
    strip_categorical_cols <- reactive({
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
          # LIMIT TO 15 for the strip color (first dropdown)
          if (n_unique > 1 && n_unique <= 15) col else NULL
        }) %>%
        unlist()
      
      # Add "None" as the default selection option
      c("None", valid_cols)
    })
    
    # Reactive to identify ALL suitable columns for POINT COLOR (Second Dropdown)
    point_color_cols <- reactive({
      req(metadata_reactive())
      md <- metadata_reactive()
      
      # Exclude ID, Timepoint, and Omics columns
      exclude_cols <- c(id_col_name(), time_col_name(), omics_names_reactive())
      
      # Filter for columns that are NOT excluded and have at least 2 unique non-NA values
      valid_cols <- names(md) %>%
        setdiff(exclude_cols) %>%
        lapply(function(col) {
          n_unique <- md %>% pull(!!sym(col)) %>% unique() %>% na.omit() %>% length()
          # NO LIMIT on unique values (removes the <= 15 filter)
          if (n_unique > 1) col else NULL
        }) %>%
        unlist()
      
      # Add "None" as the default selection option
      c("None", valid_cols)
    })
    
    # 1. Generate UI for Strip Color dropdown
    output$strip_color_column_ui <- renderUI({
      timelineColorDropdownUI(
        session$ns, 
        "strip_color_column", 
        "Color Strip & Subject Labels by (Max 15 Categories)", 
        strip_categorical_cols() # Use limited list
      )
    })
    
    # 2. Generate UI for Point Color dropdown
    output$point_color_column_ui <- renderUI({
      timelineColorDropdownUI(
        session$ns, 
        "point_color_column", 
        "Color Timepoints by (All Columns)", 
        point_color_cols() # Use expanded list
      )
    })
    
    # 3. Filtered/Processed metadata (currently no filtering, just pass-through)
    filtered_metadata_reactive <- reactive({
      req(metadata_reactive())
      metadata_reactive()
    })
    
    # 4. Reactive to generate the plot
    plots_single <- reactive({
      req(filtered_metadata_reactive())
      req(omics_names_reactive())
      
      # Read input from the two new dropdowns
      strip_color_col <- if (is.null(input$strip_color_column) || input$strip_color_column == "None") NULL else input$strip_color_column
      point_color_col <- if (is.null(input$point_color_column) || input$point_color_column == "None") NULL else input$point_color_column
      
      generate_combined_timeline_plot(
        metadata = filtered_metadata_reactive(), 
        omics_cols = omics_names_reactive(),
        color_by_column = strip_color_col,        # Used for strip fill and y-axis color (Subject-Level)
        point_color_by_column = point_color_col   # Used for point marker color (Timepoint-Level)
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
    
    # Return the reactive single plot object
    return(plots_single)
  })
}