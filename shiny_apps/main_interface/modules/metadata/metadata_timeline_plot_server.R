# metadata_timeline_plot_server.R

source("modules/metadata/metadata_timeline_plot_helpers.R")
source("modules/metadata/metadata_timeline_plot_ui.R")

metadataTimelinePlotsServer <- function(id, metadata_reactive, omics_names_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Determine the Subject ID and Timepoint column names
    id_col_name <- reactive({ names(metadata_reactive())[1] })
    time_col_name <- reactive({ names(metadata_reactive())[2] })
    
    # ... (strip_categorical_cols and point_color_cols remain the same) ...
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
        "Primary Outcome", 
        strip_categorical_cols() # Use limited list
      )
    })
    
    # 2. Generate UI for Point Color dropdown
    output$point_color_column_ui <- renderUI({
      timelineColorDropdownUI(
        session$ns, 
        "point_color_column", 
        "Secondary Variable", 
        point_color_cols() # Use expanded list
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
    
    # 7. Return the reactive plot object AND the selected group column
    return(list(
      plot = plots_single,
      group_col = group_col_reactive
    ))
  })
}