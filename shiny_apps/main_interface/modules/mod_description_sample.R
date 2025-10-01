# shiny_apps/main_interface/modules/mod_description_sample.R
library(shiny)

# UI for description + sample
descriptionSampleUI <- function(id) {
  ns <- NS(id)
  div(
    id = ns("description_container"),  # wrap everything in a div
    tagList(
      h4("Instructions for Data Upload"),
      
      p("1. Metadata: Please submit a metadata file containing one row per sample. The first column should be the sample ID."),
      p("   Metadata can include: sampleID, subjectID, Time, Group, and any other subject characteristics (e.g., age, sex, etc.)."),
      p("   The metadata does not need to start with the dataset name; it can start directly with the sample IDs."),
      
      p("2. Omics files: You can upload multiple files per session. Each file should have samples in rows and features in columns."),
      p("3. Supported file types for both metadata and omics files: .csv, .tsv, .txt, .xlsx, .rds"),
      p("4. Excel files with multiple sheets are supported; each sheet is processed as a separate dataset."),
      
      br(),
      downloadButton(ns("download_omics"), "Download Sample Omics File", width = "100%"),
      downloadButton(ns("download_metadata"), "Download Sample Metadata File", width = "100%"),
      br()
    )
  )
}



# Server logic for description + sample
descriptionSampleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Temporary folder for sample files
    sample_path <- tempdir()
    
    # Create sample omics and metadata
    omics_sample <- data.frame(
      SampleID = paste0("Sample", 1:5),
      Gene1 = rnorm(5),
      Gene2 = rnorm(5),
      Gene3 = rnorm(5)
    )
    metadata_sample <- data.frame(
      SampleID = paste0("Sample", 1:5),
      Age = c(30, 25, 40, 35, 28),
      Gender = c("F", "M", "F", "M", "F")
    )
    
    # Save sample files
    write.csv(omics_sample, file.path(sample_path, "sample_omics.csv"), row.names = FALSE)
    write.csv(metadata_sample, file.path(sample_path, "sample_metadata.csv"), row.names = FALSE)
    
    # Download handlers
    output$download_omics <- downloadHandler(
      filename = function() { "sample_omics.csv" },
      content = function(file) { file.copy(file.path(sample_path, "sample_omics.csv"), file) }
    )
    
    output$download_metadata <- downloadHandler(
      filename = function() { "sample_metadata.csv" },
      content = function(file) { file.copy(file.path(sample_path, "sample_metadata.csv"), file) }
    )
  })
}
