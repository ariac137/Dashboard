# metadata_ui.R

library(shiny)

metadataUI <- function(id) {
  ns <- NS(id)
  tagList(
    p("First three columns should be in this order: Sample ID, Subject ID, Time (numeric or datetime)."),
    p("For columns that show whether omics data exists, use 0/1, True/False, or Yes/No consistently."),
    
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
