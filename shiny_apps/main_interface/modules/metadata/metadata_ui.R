# metadata_ui.R

library(shiny)

metadataUI <- function(id) {
  ns <- NS(id)
  tagList(
    p("Please submit a metadata file containing one row per sample. The first column should be the sample ID."),
    p("Metadata can include: sampleID, subjectID, Time, Group, and any other subject characteristics (e.g., age, sex, etc.)."),
    br(),
    fileInput(ns("meta_file"), "Upload Metadata File",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".txt", ".xlsx", ".rds"))
  )
}