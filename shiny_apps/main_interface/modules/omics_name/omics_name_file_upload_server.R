library(data.table)

source("modules/omics_name/omics_name_helpers.R")
source("modules/omics_name/omics_name_file_upload_ui.R")

omicsFileUploadServer <- function(id, default_file_path = NULL) {
  moduleServer(id, function(input, output, session) {
    omics_names <- reactive({
      # 1. Manual input takes priority
      if (!is.null(input$manual_names) && nzchar(input$manual_names)) {
        return(parse_manual_input(input$manual_names)) 
      }
      
      # 2. Determine file source (UI upload OR Default)
      file_source <- NULL
      file_ext <- NULL
      skip_header_val <- FALSE
      
      
      if (!is.null(input$omics_file)) {
        # Case A: File uploaded via UI
        req(input$omics_file)
        file_source <- input$omics_file$datapath
        file_ext <- tolower(tools::file_ext(input$omics_file$name))
        skip_header_val <- input$skip_header
        
      } else if (!is.null(default_file_path) && file.exists(default_file_path)) {
        # Case B: Default file path provided and exists. 
        # This treats the default file as if it were uploaded.
        file_source <- default_file_path
        file_ext <- tolower(tools::file_ext(default_file_path))
        # Default skip_header is FALSE, as it's not a UI input.
        skip_header_val <- FALSE 
      }
      
      # 3. Read and Process File using unified logic (Case A or B)
      if (!is.null(file_source)) {
        names_vec <- tryCatch({
          
          # FIX 1: Use 'file_ext' instead of 'ext'
          # FIX 2: Use 'file_source' instead of 'file_path'
          # FIX 3: Use 'skip_header_val' instead of 'input$skip_header'
          
          df <- if (file_ext %in% c("csv", "tsv")) {
            data.table::fread(file_source,
                              data.table = FALSE,
                              header = !skip_header_val, 
                              check.names = FALSE,
                              stringsAsFactors = FALSE)
          } 
          else if (file_ext == "txt") {
            readLines(file_source) %>% data.frame(names = ., stringsAsFactors = FALSE)
          } else {
            showNotification("Unsupported file type", type = "error")
            return(character(0))
          }
          
          # Skip first row if checked
          # Only needs to be done manually for TXT when using readLines
          if (file_ext == "txt" && skip_header_val && nrow(df) > 0) {
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
      }
    })
    
    
    return(omics_names)
  })
}