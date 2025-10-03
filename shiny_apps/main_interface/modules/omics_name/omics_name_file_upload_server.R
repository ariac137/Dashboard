library(data.table)

source("modules/omics_name/omics_name_helpers.R")
source("modules/omics_name/omics_name_file_upload_ui.R")

omicsFileUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    omics_names <- reactive({
      # --- Manual input takes priority ---
      if (!is.null(input$manual_names) && nzchar(input$manual_names)) {
        strsplit(input$manual_names, ",")[[1]] %>%
          trimws() %>%
          .[. != ""]
      }
      # --- File input ---
      else if (!is.null(input$omics_file)) {
        req(input$omics_file)
        file_path <- input$omics_file$datapath
        ext <- tolower(tools::file_ext(input$omics_file$name))
        
        names_vec <- tryCatch({
          df <- if (ext %in% c("csv", "tsv")) {
            data.table::fread(file_path,
                              data.table = FALSE,
                              header = !input$skip_header,
                              check.names = FALSE,
                              stringsAsFactors = FALSE)
          } else if (ext == "txt") {
            readLines(file_path) %>% data.frame(names = ., stringsAsFactors = FALSE)
          } else {
            showNotification("Unsupported file type", type = "error")
            return(character(0))
          }
          
          # Skip first row if checked
          if (input$skip_header && nrow(df) > 0) {
            df <- df[-1, , drop = FALSE]
          }
          
          # Flatten and clean
          unlist(df, use.names = FALSE) %>%
            trimws() %>%
            .[. != ""]
        }, error = function(e) {
          showNotification(paste("Error reading file:", e$message), type = "error")
          character(0)
        })
        
        return(names_vec)  # <-- MUST return
      }
      # --- Nothing provided yet ---
      else {
        character(0)
      }
    })
    
    # Render inside the module
    output$names_out <- renderPrint({
      names <- omics_names()
      
      if (length(names) == 0) {
        cat("No omics names provided yet.\n")
      } else {
        # Option 1: comma-separated
        cat(paste(names, collapse = ", "), "\n")
        
        # Option 2: one name per line
        # cat(paste(names, collapse = "\n"), "\n")
      }
    })
    
    
    return(omics_names)
  })
}
