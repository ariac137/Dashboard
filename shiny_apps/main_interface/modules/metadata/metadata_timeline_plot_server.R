source("modules/metadata/metadata_timeline_plot_helpers.R")
source("modules/metadata/metadata_timeline_plot_ui.R")

metadataTimelinePlotsServer <- function(id, metadata_reactive, omics_names_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Reactive to generate the SINGLE faceted plot
    plots_single <- reactive({
      req(metadata_reactive())
      req(omics_names_reactive())
      
      # Calls the new helper function to get a SINGLE ggplot object
      plot_metadata_timeline_faceted(metadata_reactive(), omics_names_reactive())
    })
    
    # 2. Reactive to calculate height dynamically for the SINGLE plot
    plot_height <- reactive({
      req(metadata_reactive())
      req(omics_names_reactive())
      # Use the specific timeline height helper
      get_timeline_plot_height(metadata_reactive(), omics_names_reactive())
    })
    
    # 3. Dynamic UI rendering for the single faceted plot
    output$timeline_plot_container <- renderUI({
      final_plot <- plots_single()
      
      if (is.null(final_plot)) {
        return(p("No data points available to generate timeline plots for the selected omics types."))
      }
      
      # Renders the SINGLE combined faceted plot
      output$combined_timeline_plot <- renderPlot({ final_plot })
      
      # Outputs only ONE plot container
      plotOutput(session$ns("combined_timeline_plot"), height = plot_height())
    })
    
    # Return the reactive single plot object
    return(plots_single)
  })
}