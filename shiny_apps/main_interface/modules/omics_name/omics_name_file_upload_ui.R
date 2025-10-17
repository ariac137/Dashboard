library(shiny)

omicsFileUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Enter Omics Names"),
    # p("Each line (or cell) should contain one omics name."),
    # fileInput(ns("omics_file"), "Choose File (CSV, TXT):",
    #           multiple = FALSE,
    #           accept = c(".csv", ".tsv", ".txt")),
    # h6("Or type manually (comma-separated):"),
    textAreaInput(
      inputId = ns("manual_names"),
      label = NULL,
      placeholder = "e.g. RNAseq, Proteomics, Metabolomics",
      rows = 3
    ),
    # checkboxInput(
    #   inputId = ns("skip_header"),
    #   label = "Skip Header",
    #   value = FALSE
    # ),
    # h5("Cleaned Names Output"),
    verbatimTextOutput(ns("names_out"))
  )
}
