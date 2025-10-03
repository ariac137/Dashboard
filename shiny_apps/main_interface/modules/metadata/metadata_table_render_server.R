library(shiny)
library(DT) # Required for DT::renderDT

source("modules/metadata/metadata_table_render_helpers.R")

# -----------------------------------------------------------
# Server function for the Metadata Table Preview tab (NOW PURELY RENDERING)
# -----------------------------------------------------------

#' Server logic for rendering the interactive metadata table.
#' It relies on receiving a pre-loaded reactive data frame from the data loader module.
#'
#' @param id Module ID.
#' @param processed_data_reactive Reactive value containing the loaded data frame (df).
renderTableServer <- function(id, processed_data_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # -----------------------------------------------------------
    # Table Rendering
    # -----------------------------------------------------------
    
    # Renders the interactive DT table using the data loaded by the loader module
    output$metadata_table_output <- DT::renderDT({
      
      data_to_render <- processed_data_reactive()
      req(data_to_render)
      
      # Assumes render_interactive_table() is available (sourced in app.R)
      render_interactive_table(
        df = data_to_render,
        title = "Uploaded and Processed Metadata (Sample IDs are Row Names)"
      )
    })
    
  })
}
