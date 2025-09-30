library(plumber)
library(readr)
library(readxl)
library(tools)
library(stringr)

# Helper: detect delimiter for txt/csv-like files
detect_delim <- function(path) {
  lines <- readLines(path, n = 5)
  delimiters <- c("," = 0, "\t" = 0, ";" = 0, " " = 0)
  for (d in names(delimiters)) {
    delimiters[d] <- stringr::str_count(lines[1], fixed(d))
  }
  delim <- names(delimiters)[which.max(delimiters)]
  delim
}

#* Upload a file and save as RData
#* @param file:file The uploaded file
#* @param rdata_name:string The name of the R object
#* @post /upload
function(req, res, file, rdata_name = "mydata") {
  tmp_file <- tempfile(fileext = paste0(".", tools::file_ext(file$name)))
  file.copy(file$datapath, tmp_file)
  
  ext <- tools::file_ext(file$name)
  
  data <- switch(ext,
                 csv = read_csv(tmp_file),
                 tsv = read_tsv(tmp_file),
                 txt = {
                   delim <- detect_delim(tmp_file)
                   read_delim(tmp_file, delim = delim)
                 },
                 xlsx = read_excel(tmp_file),
                 rds = readRDS(tmp_file),
                 stop("Unsupported file type")
  )
  
  rdata_name_safe <- make.names(rdata_name)
  save(list = "data", file = file.path(tempdir(), paste0(rdata_name_safe, ".RData")))
  
  list(
    message = paste0("File saved as ", rdata_name_safe, ".RData"),
    rows = nrow(data),
    columns = ncol(data),
    data_preview = head(data, 10)
  )
}
