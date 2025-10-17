# metadata_ui.R

library(shiny)

metadataUI <- function(id) {
  ns <- NS(id)
  tagList(
    p("Upload a metadata file with the first three columns in this order: Sample ID, Subject ID, Time."),
    p("For columns showing if data exists, use consistent values: 0/1, True/False, or Yes/No."),
    
    downloadButton(
      outputId = ns("download_template"),
      label = "Download Template", 
      class = "btn-primary"        
    ),
                   
    fileInput(ns("meta_file"), 
              label = "",
              multiple = FALSE,
              accept = c(".csv", ".tsv", ".txt", ".xlsx", ".rds"))
  )
}
