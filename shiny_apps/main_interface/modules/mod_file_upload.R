library(shiny)
source("modules/mod_helpers.R")
source("modules/mod_description_sample.R")
source("modules/mod_metadata.R")
source("modules/mod_omics_server.R")

# -------------------------------
# Combined File Upload Module
# -------------------------------
fileUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Description / sample downloads ---
    descriptionSampleUI(ns("desc1")),
    hr(),
    
    # --- Metadata upload (single file) ---
    h4("Metadata Upload"),
    metadataUI(ns("meta1")),
    hr(),
    
    # --- Omics upload (modal, multiple files) ---
    h4("Omics Files Upload"),
    omicsModalUI(ns("omics1"))
    # REMOVED: Global actionButton(ns("global_upload_btn"), ...)
  )
}

fileUploadServer <- function(id, save_dir = tempdir()) {
  moduleServer(id, function(input, output, session) {
    
    # --- Description server ---
    descriptionSampleServer("desc1")
    
    # --- Metadata upload server ---
    metadata <- metadataServer("meta1", save_dir = save_dir)
    
    # --- Omics upload server ---
    omics <- omicsModalServer("omics1", save_dir = save_dir)
    
    # Return all reactive outputs
    return(list(
      metadata_file = metadata$file,
      metadata_preview = metadata$preview,
      omics_files = omics$files,
      omics_preview = omics$preview
    ))
  })
}