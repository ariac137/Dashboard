library(shiny)

omicsFileUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Upload Omics Data Types List"),
    p("Each line (or cell) should contain one omics name."),
    checkboxInput(
      inputId = ns("skip_header"),
      label = "Skip first row (File contains a header)",
      value = FALSE
    ),
    hr(),
    fileInput(ns("omics_file"), "Choose Omics List File (CSV, TXT):",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".txt")),
    h5("Or type omics names manually (comma-separated):"),
    textAreaInput(
      inputId = ns("manual_names"),
      label = NULL,
      placeholder = "e.g. RNAseq, Proteomics, Metabolomics",
      rows = 3
    ),
    h4("Cleaned Names Output"),
    verbatimTextOutput(ns("names_out"))
  )
}
