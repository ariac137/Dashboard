# omics_backend.R (PURE OMICS BACKEND)

# Imports pure file system utilities
source("modules/helper_functions.R") 
library(tools)

#' @description Moves a file to the permanent location and returns the path.
#' This is a pure file system operation, suitable for staging.
stage_omics_file <- function(uid, name, tmp_path, save_dir) {
  dest_path <- file.path(save_dir, paste0(uid, "_", name))
  # Uses move_file_to_permanent from helper_functions.R
  move_file_to_permanent(tmp_path, dest_path) 
  return(dest_path)
}

#' @description Removes a file from the disk.
remove_omics_file_from_disk <- function(file_path) {
  # Uses remove_file_from_disk from helper_functions.R
  remove_file_from_disk(file_path)
}

# NOTE: safe_preview and process_file are already in helper_functions.R