# metadata_timepoint_stats_server.R

library(shiny)
library(DT)

# Source the calculation helper and the DT rendering helper
source("modules/metadata/metadata_timepoint_stats_helpers.R")
# Assuming metadata_table_render_helpers.R is where render_interactive_table is defined
source("modules/metadata/metadata_table_render_helpers.R") 

#' Server logic for calculating and rendering the Omics Timepoint Statistics Table.
#'
#' @param id Module ID.
#' @param metadata_reactive Reactive value containing the loaded data frame.
#' @param omics_names_reactive Reactive value containing the vector of omics column names.
#' @param group_col_reactive Reactive value of the selected primary grouping column (required by main_page, but not used here).
metadataTimepointStatsServer <- function(id, metadata_reactive, omics_names_reactive, group_col_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression to perform the required calculations
    omics_stats_reactive <- reactive({
      req(metadata_reactive())
      req(omics_names_reactive())
      
      # Call the helper function to compute the stats
      calculate_omics_time_stats(
        metadata = metadata_reactive(),
        omics_cols = omics_names_reactive()
      )
    })
    
    # Renders the interactive DT table
    output$omics_time_stats_table <- DT::renderDT({
      
      stats_df <- omics_stats_reactive()
      req(stats_df)
      
      # Use the centralized rendering helper for consistency
      render_interactive_table(
        df = stats_df,
        title = "Time Point Summary by Omics Type"
      )
    })
    
  })
}