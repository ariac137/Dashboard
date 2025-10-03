library(shiny)
library(DT) # Required for DT::renderDT

# -----------------------------------------------------------
# UI function for the Metadata Table Preview tab
# -----------------------------------------------------------

#' UI element for the metadata table preview tab.
#'
#' @param id Module ID.
renderTableUI <- function(id) {
  ns <- NS(id)
  tabPanel("Metadata Table Preview", 
           value = "metadata_preview_tab", 
           # This is the actual output slot for the interactive DT table
           DT::DTOutput(ns("metadata_table_output"))
  )
}
