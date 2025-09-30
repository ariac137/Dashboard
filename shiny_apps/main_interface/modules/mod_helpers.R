library(readr)
library(readxl)
library(tools)
library(stringr)

# Detect delimiter for txt/csv
detect_delim <- function(path) {
  lines <- readLines(path, n = 5)
  delimiters <- c("," = 0, "\t" = 0, ";" = 0, " " = 0)
  for (d in names(delimiters)) {
    delimiters[d] <- str_count(lines[1], fixed(d))
  }
  names(delimiters)[which.max(delimiters)]
}

# Read file based on extension
read_file <- function(file_path, ext) {
  df <- switch(ext,
               csv = read_csv(file_path, show_col_types = FALSE),
               tsv = read_tsv(file_path, show_col_types = FALSE),
               txt = read_delim(file_path, delim = detect_delim(file_path), show_col_types = FALSE),
               xlsx = read_excel(file_path),
               rds = readRDS(file_path),
               stop(paste("Unsupported file type:", ext))
  )
  as.data.frame(df)
}

# Process and save file to RData (includes logic for metadata vs. omics files)
process_file <- function(file_path, file_name, prefix, save_dir) {
  ext <- tools::file_ext(file_name)
  base_name <- tools::file_path_sans_ext(file_name)
  
  process_df <- function(df, file_name, is_metadata = FALSE) {
    if (is_metadata) {
      # Use column 2 as unique Sample ID for row names 
      sample_ids <- df[[2]]
      if (any(duplicated(sample_ids))) stop("Sample ID column contains duplicate values.")
      rownames(df) <- sample_ids 
      df <- df[,-c(1, 2), drop = FALSE] # Remove Dataset (Col 1) and Sample ID (Col 2)
    } else {
      # Default Omics logic (Col 1 is Feature/Gene ID)
      sample_ids <- df[[1]]
      if (any(duplicated(sample_ids))) stop("Feature ID column contains duplicate values.")
      rownames(df) <- df[[1]] 
      df <- df[,-1, drop = FALSE] # Remove the first column
    }
    return(df)
  }
  
  is_metadata <- prefix == "metadata" 
  
  if (ext == "xlsx") {
    sheets <- readxl::excel_sheets(file_path)
    sheet_list <- list()
    for (s in sheets) {
      df <- read_excel(file_path, sheet = s) %>% as.data.frame()
      df <- process_df(df, file_name, is_metadata = is_metadata)
      rdata_name <- paste0(make.names(prefix), "_", make.names(s), ".RData")
      save(df, file = file.path(save_dir, rdata_name)) 
      sheet_list[[make.names(s)]] <- rdata_name
    }
    return(sheet_list)
  } else {
    df <- read_file(file_path, ext)
    df <- process_df(df, file_name, is_metadata = is_metadata)
    rdata_name <- paste0(make.names(prefix), "_", make.names(base_name), ".RData")
    save(df, file = file.path(save_dir, rdata_name))
    return(list(df = rdata_name))
  }
}