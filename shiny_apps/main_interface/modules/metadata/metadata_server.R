# metadata_server.R (MINIMAL SERVER WRAPPER)

library(shiny)
# Source the flow logic
source("modules/metadata/metadata_ui.R") 
source("modules/metadata/metadata_ui_helpers.R") 

metadataServer <- function(id, save_dir = tempdir(), rdata_prefix = "metadata") {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(file = NULL)
    
    # DELEGATES ALL PROCESSING AND ERROR HANDLING TO THE FLOW FILE
    observeEvent(input$meta_file, {
      req(input$meta_file)
      handle_metadata_upload_flow(
        input_file = input$meta_file,
        rdata_prefix = rdata_prefix,
        save_dir = save_dir,
        rv = rv
      )
    })
    
    return(list(
      file = reactive({ rv$file })
    ))
  })
}