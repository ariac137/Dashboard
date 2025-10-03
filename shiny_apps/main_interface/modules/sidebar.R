library(shiny)
source("modules/upload_instructions/upload_instructions.R")
source("modules/metadata/metadata_server.R")
source("modules/omics/omics_server.R")
source("modules/omics_name/omics_name_file_upload_server.R")

# -------------------------------
# Combined File Upload Module
# -------------------------------
sidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Description / sample downloads ---
    descriptionSampleUI("desc1"),
    hr(),
    
    omicsFileUploadUI(ns("omics_submit_A")),
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

sidebarServer <- function(id, save_dir = tempdir()) {
  moduleServer(id, function(input, output, session) {
    
    # --- Description server ---
    descriptionSampleServer("desc1")
    
    omics_names_reactive <- omicsFileUploadServer("omics_submit_A")
    
    # --- Metadata upload server ---
    metadata <- metadataServer("meta1", save_dir = save_dir)
    
    # --- Omics upload server ---
    omics <- omicsModalServer("omics1", save_dir = save_dir)
    
    # Return all reactive outputs
    return(list(
      metadata_file = metadata$file,
      metadata_preview = metadata$preview,
      omics_files = omics$files,
      omics_preview = omics$preview,
      omics_names = omics_names_reactive  
    ))
  })
}