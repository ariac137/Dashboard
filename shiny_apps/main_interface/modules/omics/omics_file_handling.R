safe_preview <- function(path, ext, n = 5) {
  tryCatch({
    ext <- tolower(ext)
    if (ext %in% c("csv","tsv","txt")) {
      if (requireNamespace("data.table", quietly = TRUE)) {
        df <- data.table::fread(path, nrows = n, data.table = FALSE)
      } else if (requireNamespace("readr", quietly = TRUE)) {
        df <- readr::read_delim(path, delim = NULL, n_max = n, show_col_types = FALSE)
      } else {
        firstline <- readLines(path, n = 1)
        sep <- if (grepl("\t", firstline)) "\t" else if (grepl(",", firstline)) "," else ""
        df <- utils::read.table(path, sep = sep, header = TRUE, nrows = n, check.names = FALSE)
      }
      list(`Sheet1` = df)  # wrap in list for consistency
    } else if (ext %in% c("xlsx","xls") && requireNamespace("readxl", quietly = TRUE)) {
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



addFile <- function(rv, uid, name, tmp_path, save_dir) {
  dest_path <- file.path(save_dir, paste0(uid, "_", name))
  file.copy(tmp_path, dest_path, overwrite = TRUE)
  rv$files[[uid]] <- list(name = name, path = dest_path)
  dest_path
}

removeFile <- function(rv, uid) {
  fpath <- rv$files[[uid]]$path
  if (!is.null(fpath) && file.exists(fpath)) file.remove(fpath)
  rv$files[[uid]] <- NULL
  rv$preview[[uid]] <- NULL
}

showUploadSummary <- function(rv) {
  fids <- names(rv$files)
  req(length(fids) > 0)
  
  summary_msgs <- sapply(fids, function(uid) {
    paste0("Uploaded '", rv$files[[uid]]$name, "'")
  })
  
  showModal(modalDialog(
    title = "Upload Summary",
    renderPrint({ cat(paste(summary_msgs, collapse="\n")) }),
    easyClose = TRUE
  ))
  
  rv$files <- list()
  rv$preview <- list()
  removeModal()
}
