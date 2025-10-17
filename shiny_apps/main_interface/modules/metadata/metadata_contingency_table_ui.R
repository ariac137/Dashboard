# metadata_contingency_table_ui.R

metadataContingencyTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    # Title is dynamically set in the server
    uiOutput(ns("table_title_ui")),
    
    # FIX 1: Wrap output for centering (sets 90% width, centers remaining margin)
    div(style = "width: 90%; margin: 0 auto;",
        # Interactive table output
        DT::dataTableOutput(ns("omics_count_table"))
    ),
    br(),
    br() 
  )
}