# sidebar.R - FINAL CORRECTED VERSION

library(shiny)
source("modules/upload_instructions/upload_instructions.R")
source("modules/metadata/metadata_server.R")
source("modules/omics/omics_server.R")
source("modules/omics_name/omics_name_file_upload_server.R")
source("modules/notepad/notepad_server.R")
source("modules/screenshot/screenshot_server.R")

styled_hr <- function() {
  tags$hr(class = "sidebar-separator")
}

# -------------------------------
# Combined File Upload Module
# -------------------------------
sidebarUI <- function(id) { 
  ns <- NS(id)
  tagList(
    
    # --- SCREENSHOT INTEGRATION ---
    screenshotButtonUI(ns("screenshot")),
    
    # --- NOTEPAD INTEGRATION ---
    notepadUI(ns("project_notes")),   
    styled_hr(),                 
    
    # --- Description / sample downloads ---
    # descriptionSampleUI("desc1"),     
    # styled_hr(),                     
    
    omicsFileUploadUI(ns("omics_submit_A")), 
    styled_hr(),                      
    
    # --- Metadata upload (single file) ---
    h4("Upload Metadata"),           
    metadataUI(ns("meta1")),         
    # styled_hr()                    
    
    # --- Omics upload (modal, multiple files) ---
    #h4("Omics Files Upload"),
    #omicsModalUI(ns("omics1"))
  )
}

sidebarServer <- function(id, save_dir = tempdir(),
                          default_omics_names_path = NULL, 
                          default_metadata_path = NULL,
                          default_omics_names = NULL)  {
  moduleServer(id, function(input, output, session) {
    
    # --- Description server ---
    descriptionSampleServer("desc1")
    
    omics_names_reactive <- omicsFileUploadServer(
      "omics_submit_A", 
      default_names_vector = default_omics_names
    )
    
    # --- Metadata upload server ---
    metadata <- metadataServer(
      "meta1", 
      save_dir = save_dir, 
      default_file_path = default_metadata_path
    )
    
    # --- NOTEPAD SERVER ---
    notepadServer("project_notes")
    
    # --- NEW: SCREENSHOT SERVER ---
    screenshotServer("screenshot")
    
    # Return all reactive outputs
    return(list(
      metadata_file = metadata$file,
      metadata_preview = metadata$preview,
      omics_names = omics_names_reactive  
    ))
  })
}