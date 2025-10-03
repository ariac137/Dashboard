library(shiny)

# -----------------------------------------------------------
# Server function for Data Loading/Processing
# -----------------------------------------------------------

#' Server logic for synchronously loading the processed RData file into memory.
#' This acts as an intermediate data provider between the uploader and the renderer.
#'
#' @param id Module ID.
#' @param uploaded_file_reactive Reactive value containing the file path(s) 
#'                               from the metadata upload process.
#' @return A list containing:
#'         - `processed_data`: A reactive value holding the loaded data frame.
dataLoaderServer <- function(id, uploaded_file_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive block that reads the processed RData file when its path changes
    processed_data <- reactive({
      
      # 1. Require that a file has been successfully processed and saved (path exists)
      req(uploaded_file_reactive())
      
      # The 'metadata_file' reactive returns a list of paths 
      file_paths <- uploaded_file_reactive()
      
      # For now, assume the first path holds the primary processed data
      rdata_path <- file_paths[[1]] 
      
      # 2. Load the processed data frame 'df' from the saved RData file
      # Note: We use a safe environment to load data to avoid cluttering the global environment.
      temp_env <- new.env()
      load(rdata_path, envir = temp_env)
      
      # The processed data frame is named 'df' inside the RData file
      data_frame <- temp_env$df
      
      return(data_frame)
    })
    
    # Return the data frame reactive for consumption by other modules
    return(list(
      processed_data = processed_data
    ))
  })
}
