library(data.table)

# Read CSV/TSV
read_csv_tsv <- function(file_path, skip_header = FALSE) {
  data.table::fread(
    file_path,
    data.table = FALSE,
    header = !skip_header,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

# Read TXT
read_txt <- function(file_path, skip_header = FALSE) {
  df <- readLines(file_path) %>% data.frame(names = ., stringsAsFactors = FALSE)
  if (skip_header && nrow(df) > 0) df <- df[-1, , drop = FALSE]
  df
}

# Flatten and clean a dataframe or vector
clean_names <- function(df) {
  unlist(df, use.names = FALSE) %>%
    trimws() %>%
    .[. != ""]
}

# Parse manual text input
parse_manual_input <- function(text) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  strsplit(text, ",")[[1]] %>%
    trimws() %>%
    .[. != ""]
}
