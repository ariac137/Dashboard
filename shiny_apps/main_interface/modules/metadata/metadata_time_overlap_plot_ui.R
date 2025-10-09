metadataTimeOverlapPlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Omics Time Overlap Plots"),
    uiOutput(ns("time_overlap_plot_container"))
  )
}
