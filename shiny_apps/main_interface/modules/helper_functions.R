# helper_functions.R (PURE BACKEND UTILITIES)

library(data.table)
library(readxl)
library(tools)
library(dplyr)

# ----------------------------------------------------
# 1. Core File Reading Utilities (Optimized)
# ----------------------------------------------------

# FASTEST Full-File Reading Function (used by process_file)
fast_read_file <- function(file_path, ext) {
  ext <- tolower(ext)
  
  df <- tryCatch({
    if (ext %in% c("csv", "tsv", "txt")) {
      df_read <- data.table::fread(file_path, data.table = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      df_read <- readxl::read_excel(file_path)
    } else if (ext == "rds") {
      df_read <- readRDS(file_path)
    } else {
      stop(paste("Unsupported file type:", ext))
    }
    return(as.data.frame(df_read))
  }, error = function(e) {
    stop(paste("Error reading file:", conditionMessage(e)))
  })
}

# FASTEST Preview Reading Function (used by omics_server for async preview)
safe_preview <- function(path, ext, n = 5) {
  # This function performs a fast, row-limited read and returns a simple list of data frames.
  # The UI rendering logic is elsewhere.
  tryCatch({
    ext <- tolower(ext)
    if (ext %in% c("csv","tsv","txt")) {
      df <- data.table::fread(path, nrows = n, data.table = FALSE)
      list(`Sheet1` = df)  
    } else if (ext %in% c("xlsx","xls")) {
      sheets <- readxl::excel_sheets(path)
      lapply(sheets, function(s) {
        df <- readxl::read_excel(path, sheet = s)
        df[seq_len(min(n, nrow(df))), seq_len(min(n, ncol(df)))]
      }) |> setNames(sheets)
    } else if (ext == "rds") {
      obj <- readRDS(path)
      if (is.data.frame(obj)) list(`Data` = head(obj, n))
      else list(`Data` = data.frame(value = utils::capture.output(head(obj, n))))
    } else {
      list(`Data` = data.frame(note = paste("No preview for .", ext)))
    }
  }, error = function(e) list(`Error` = data.frame(error = conditionMessage(e))))
}

# ----------------------------------------------------
# 2. Core Data Processing & Saving
# ----------------------------------------------------

# Shared function to set rownames and remove ID columns
# Final Corrected Shared function to match your transposed data format
core_process_df <- function(df, prefix) {
  
  # 1. Read IDs from the FIRST column for ANY file (metadata or omics data)
  ids <- df[[1]] 
  
  # 2. Check for Duplicates (The Sample ID must be unique)
  if (any(duplicated(ids))) {
    # Custom error message depending on the file type
    if (prefix == "metadata") {
      stop("Sample ID column contains duplicate values.")
    } else {
      stop("Omics data file primary key (Sample ID) column contains duplicate values.")
    }
  }
  
  # 3. Set Sample IDs as Rownames
  rownames(df) <- ids
  
  # 4. Remove the redundant ID column (now stored in rownames)
  df <- df[,-1, drop = FALSE]
  
  return(df)
}

# Process entire file content and save to RData
process_file <- function(file_path, file_name, prefix, save_dir) {
  ext <- tools::file_ext(file_name)
  # ... (Excel sheet processing is kept, using fast_read_file for delimited files) ...
  base_name <- tools::file_path_sans_ext(file_name)
  
  if (ext %in% c("xlsx", "xls")) {
    sheets <- readxl::excel_sheets(file_path)
    sheet_paths <- list()
    for (s in sheets) {
      df <- readxl::read_excel(file_path, sheet = s) %>% as.data.frame()
      df <- core_process_df(df, prefix)
      rdata_name <- paste0(make.names(prefix), "_", make.names(s), ".RData")
      dest_path <- file.path(save_dir, rdata_name)
      save(df, file = dest_path) 
      sheet_paths[[make.names(s)]] <- dest_path
    }
    return(sheet_paths)
  } else {
    df <- fast_read_file(file_path, ext) 
    df <- core_process_df(df, prefix)
    rdata_name <- paste0(make.names(prefix), "_", make.names(base_name), ".RData")
    dest_path <- file.path(save_dir, rdata_name)
    save(df, file = dest_path)
    return(list(df = dest_path))
  }
}

# ----------------------------------------------------
# 3. Pure File System Utilities
# ----------------------------------------------------

# Moves a temporary file to a permanent location (used by addFile)
move_file_to_permanent <- function(tmp_path, dest_path) {
  file.copy(tmp_path, dest_path, overwrite = TRUE)
  return(dest_path)
}

# Removes a file from disk
remove_file_from_disk <- function(fpath) {
  if (!is.null(fpath) && file.exists(fpath)) file.remove(fpath)
}