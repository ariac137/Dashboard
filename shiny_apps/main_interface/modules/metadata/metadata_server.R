# metadata_server.R (MINIMAL SERVER WRAPPER)

library(shiny)
# Source the flow logic
source("modules/metadata/metadata_ui.R") 
source("modules/metadata/metadata_ui_helpers.R") 

# --- MODIFIED: Added default_file_path argument ---
metadataServer <- function(id, save_dir = tempdir(), rdata_prefix = "metadata", 
                           default_file_path = NULL) {
  moduleServer(id, function(input, output, session) {
    rv <- reactiveValues(file = NULL)
    
    # --- NEW: Default File Loading on Startup ---
    # This logic assumes the default file is already processed or 
    # can be processed in a similar flow to an uploaded file.
    if (!is.null(default_file_path) && file.exists(default_file_path)) {
      # The metadata flow is designed around `input$meta_file`, 
      # so we must create a mock object or call the processing logic directly.
      # Assuming a simple .rds file can be loaded directly as a final processed state:
      if (tolower(tools::file_ext(default_file_path)) == "rds") {
        # Assuming the .rds contains the final object that `rv$file` expects
        rv$file <- list(
          data_path = default_file_path,
          original_name = basename(default_file_path),
          rdata_path = default_file_path, # Since it's already an rds
          processed = TRUE
        )
      } else {
        # If the default file is a raw format (e.g., CSV), you'd need to mock 
        # the `input_file` structure and call `handle_metadata_upload_flow`
        mock_input_file <- list(
          datapath = default_file_path,
          name = basename(default_file_path)
        )
        # Call the full processing flow for a raw default file
        handle_metadata_upload_flow(
          input_file = mock_input_file,
          rdata_prefix = rdata_prefix,
          save_dir = save_dir,
          rv = rv
        )
      }
    }
    # ---------------------------------------------
    
    # DELEGATES ALL PROCESSING AND ERROR HANDLING TO THE FLOW FILE
    observeEvent(input$meta_file, {
      req(input$meta_file)
      handle_metadata_upload_flow(
        input_file = input$meta_file,
        rdata_prefix = rdata_prefix,
        save_dir = save_dir,
        rv = rv
      )
    }, ignoreInit = TRUE) # Ignore initial event if a default file is loaded
    
    return(list(
      file = reactive({ rv$file })
    ))
  })
}