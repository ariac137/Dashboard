source("modules/metadata/metadata_time_overlap_plot_helpers.R")
source("modules/metadata/metadata_time_overlap_plot_ui.R")
library(plotly) # ADDED: Include plotly library for rendering

metadataTimeOverlapPlotsServer <- function(id, metadata_reactive, omics_names_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Reactive to generate the single interactive plot
    plots <- reactive({
      req(metadata_reactive())
      req(omics_names_reactive())
      
      # Calls the helper function, which now returns an interactive plotly object
      plot_metadata_time_overlap(metadata_reactive(), omics_names_reactive())
    })
    
    # 2. Calculate height dynamically
    plot_height <- reactive({
      req(metadata_reactive())
      req(omics_names_reactive())
      get_time_overlap_plot_height(metadata_reactive(), omics_names_reactive())
    })
    
    # 3. Dynamic UI rendering
    output$time_overlap_plot_container <- renderUI({
      final_plot <- plots() # This is the single plotly object
      
      if (is.null(final_plot)) return(NULL)
      
      # *** FIX 1: Use renderPlotly() ***
      output$combined_plot <- renderPlotly({ final_plot })
      
      # *** FIX 2: Use plotlyOutput() ***
      plotlyOutput(session$ns("combined_plot"), height = plot_height())
    })
    
    return(plots)
  })
}
