metadataContingencyTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    # Title is dynamically set in the server
    uiOutput(ns("table_title_ui")),
    # Interactive table output
    DT::dataTableOutput(ns("omics_count_table")),
    br(),
    br() 
  )
}
