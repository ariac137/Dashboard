# metadata_flow.R (METADATA FRONTEND FLOW)

library(shiny)

# Source the pure backend (for process_file) and state management
source("modules/helper_functions.R") # For the pure process_file function
source("modules/metadata/metadata_backend_helpers.R") # For update_metadata_state

#' @description Handles file processing, error presentation, and state update flow.
handle_metadata_upload_flow <- function(input_file, rdata_prefix, save_dir, rv) {
  
  # BACKEND EXECUTION: Calls the pure process_file from helper_functions.R
  processed_file_info <- tryCatch({
    process_file(
      file_path = input_file$datapath,
      file_name = input_file$name,
      prefix = rdata_prefix,
      save_dir = save_dir
    )
  }, 
  # FRONTEND INTERACTION: Show error modal
  error = function(e) { 
    showModal(modalDialog(
      title = "File Processing Error",
      paste("Could not process file:", conditionMessage(e)),
      easyClose = TRUE
    ))
    return(NULL)
  })
  
  # STATE MANAGEMENT: Update reactive value using the state function
  update_metadata_state(rv, processed_file_info)
}