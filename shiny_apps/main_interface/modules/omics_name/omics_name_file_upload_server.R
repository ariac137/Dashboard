library(data.table)

source("modules/omics_name/omics_name_helpers.R")
source("modules/omics_name/omics_name_file_upload_ui.R")

omicsFileUploadServer <- function(id, default_names_vector = NULL) {
  moduleServer(id, function(input, output, session) {
    omics_names <- reactive({
      # 1. Manual input takes priority
      if (!is.null(input$manual_names) && nzchar(input$manual_names)) {
        # omics_name_helpers.R provides parse_manual_input
        return(parse_manual_input(input$manual_names)) 
      }
      
      # 2. If manual input is empty, use the default vector
      # This replaces all previous file upload/default file path logic.
      if (!is.null(default_names_vector) && length(default_names_vector) > 0) {
        return(default_names_vector) 
      }
      
      # --- Nothing provided yet ---
      return(character(0))
    })
    
    # Render inside the module
    output$names_out <- renderPrint({
      names <- omics_names()
      
      if (length(names) == 0) {
        cat("No omics names provided yet.\n")
      } else {
        # Option 1: comma-separated
        cat(paste(names, collapse = ", "), "\n")
      }
    })
    
    
    return(omics_names)
  })
}