source("modules/metadata/metadata_timeline_plot_helpers.R")
source("modules/metadata/metadata_timeline_plot_ui.R")

metadataTimelinePlotsServer <- function(id, metadata_reactive, omics_names_reactive) {
  moduleServer(id, function(input, output, session) {
    
    plots <- reactive({
      req(metadata_reactive())
      req(omics_names_reactive())
      
      # KEY FIX: Calls the helper function ONCE, passing ALL omics names to get a SINGLE ggplot object.
      plot_metadata_timeline_all(metadata_reactive(), omics_names_reactive())
    })
    
    output$plot_container <- renderUI({
      final_plot <- plots() # This is the single ggplot object
      
      if (is.null(final_plot)) return(NULL)
      
      # Renders the SINGLE plot
      output$combined_plot <- renderPlot({ final_plot })
      
      # Outputs only ONE plot container
      plotOutput(session$ns("combined_plot"))
    })
    
    return(plots)
  })
}