library(shiny)
source("modules/mod_helpers.R")

metadataUI <- function(id) {
  ns <- NS(id)
  fileInput(ns("meta_file"), "Upload Metadata File",
            multiple = FALSE,
            accept = c(".csv", ".tsv", ".txt", ".xlsx", ".rds"))
}

metadataServer <- function(id, save_dir = tempdir(), rdata_prefix = "metadata") {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(file = NULL, preview = NULL)
    
    observeEvent(input$meta_file, {
      req(input$meta_file)
      rv$file <- input$meta_file$datapath
      df <- tryCatch(read_file(rv$file, tools::file_ext(input$meta_file$name)), error = function(e) NULL)
      if(!is.null(df)) rv$preview <- head(df, 5)
      
      # RESTORED: save immediately upon selection
      process_file(rv$file, input$meta_file$name, rdata_prefix, save_dir)
    })
    
    return(list(file = reactive({ rv$file }),
                preview = reactive({ rv$preview })))
  })
}