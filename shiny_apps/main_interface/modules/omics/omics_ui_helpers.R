# omics_helper.R (OMICS REACTIVE FLOW)

library(shiny)
library(future)
library(promises)
library(uuid)
library(DT) 

# IMPORTANT: Source the pure backend files
source("modules/omics/omics_backend_helpers.R") 

# ----------------------------------------------------
# Reactive State Management
# ----------------------------------------------------

#' @description Manages state and calls pure backend file moving utility.
add_and_stage_omics_file <- function(rv, uid, name, tmp_path, save_dir) {
  # Calls the pure backend file system utility
  dest_path <- stage_omics_file(uid, name, tmp_path, save_dir)
  # REACTIVE STATE MANIPULATION
  rv$files[[uid]] <- list(name = name, path = dest_path)
  return(dest_path)
}

#' @description Manages state and calls pure backend file removal utility.
handle_file_removal <- function(rv, uid) {
  fpath <- rv$files[[uid]]$path
  # Calls the pure backend file system utility
  remove_omics_file_from_disk(fpath) 
  # REACTIVE STATE MANIPULATION
  rv$files[[uid]] <- NULL
  rv$preview[[uid]] <- NULL
}

# ----------------------------------------------------
# UI/Flow Control Utilities
# ----------------------------------------------------

#' @description Handles the complex batch file upload, async processing, and error modals.
handle_omics_file_upload <- function(input_files, rv, loader, session, save_dir) {
  
  for (i in seq_len(nrow(input_files))) {
    orig_name <- input_files$name[i]
    tmp_path <- input_files$datapath[i]
    
    if (any(sapply(isolate(rv$files), function(x) x$name == orig_name))) next
    
    uid <- paste0("f", uuid::UUIDgenerate())
    
    # Calls state helper to stage file and update RV
    dest_path <- add_and_stage_omics_file(rv, uid, orig_name, tmp_path, save_dir)
    
    loader$show()
    
    # ASYNC OPERATION: Calls pure backend safe_preview (from helper_functions.R)
    future({
      safe_preview(dest_path, tools::file_ext(orig_name)) 
    }) %...>% (function(preview_df) {
      # Update RV on success
      isolate({
        rv$preview[[uid]] <- preview_df
      })
      loader$hide()
    }) %...!% (function(e) {
      loader$hide()
      # SHINY UI INTERACTION: Show error modal
      showModal(modalDialog(
        title = "Error",
        paste("Error processing", orig_name, ":", e$message),
        easyClose = TRUE
      ))
      # Clean up file and state on error
      handle_file_removal(rv, uid)
    })
  }
  # Reset file input value
  session$sendInputMessage("omics_files", list(value = NULL))
}


#' @description Displays upload summary modal and resets state.
showUploadSummary <- function(rv) {
  fids <- names(rv$files)
  req(length(fids) > 0)
  
  summary_msgs <- sapply(fids, function(uid) {
    paste0("Uploaded '", rv$files[[uid]]$name, "'")
  })
  
  # SHINY UI INTERACTION: Show summary modal
  showModal(modalDialog(
    title = "Upload Summary",
    renderPrint({ cat(paste(summary_msgs, collapse="\n")) }),
    easyClose = TRUE
  ))
  
  # REACTIVE STATE MANIPULATION: Reset state
  rv$files <- list()
  rv$preview <- list()
  removeModal()
}