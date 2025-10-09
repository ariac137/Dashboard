source("modules/metadata/metadata_timeline_plot_helpers.R")
source("modules/metadata/metadata_timeline_plot_ui.R")
library(plotly)

metadataTimelinePlotsServer <- function(id, metadata_reactive, omics_names_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Reactive to generate the SINGLE faceted plot (Now returns a plotly object)
    single_plot_reactive <- reactive({
      req(metadata_reactive())
      req(omics_names_reactive())
      
      # Calls the helper function, which returns an interactive plotly object
      # The helper function (metadata_timeline_plot_helpers.R) MUST be using ggplotly()
      plot_metadata_timeline_faceted(metadata_reactive(), omics_names_reactive())
    })
    
    # 2. Reactive to calculate height dynamically for the SINGLE plot
    plot_height <- reactive({
      req(metadata_reactive())
      req(omics_names_reactive())
      get_timeline_plot_height(metadata_reactive(), omics_names_reactive())
    })
    
    # 3. Dynamic UI rendering for the single faceted plot
    output$timeline_plot_container <- renderUI({
      final_plot <- single_plot_reactive()
      
      if (is.null(final_plot)) {
        return(p("No data points available to generate timeline plots for the selected omics types."))
      }
      
      plot_h <- plot_height()
      
      # *** FIX 1: Use renderPlotly() instead of renderPlot() ***
      output$combined_timeline_plot <- renderPlotly({ final_plot })
      
      # *** FIX 2: Use plotlyOutput() instead of plotOutput() ***
      plotlyOutput(session$ns("combined_timeline_plot"), height = plot_h)
    })
    
    # Return the reactive single plot object
    return(single_plot_reactive)
  })
}
