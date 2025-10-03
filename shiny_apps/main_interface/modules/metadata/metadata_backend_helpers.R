# metadata_state.R (METADATA REACTIVE STATE MANAGEMENT)

library(shiny)

#' @description Updates the reactive value (rv$file) with the result of file processing.
update_metadata_state <- function(rv, processed_file_info) {
  # REACTIVE STATE MANIPULATION
  rv$file <- processed_file_info
}

#' @description Clears the metadata file from reactive state.
clear_metadata_state <- function(rv) {
  rv$file <- NULL
}
