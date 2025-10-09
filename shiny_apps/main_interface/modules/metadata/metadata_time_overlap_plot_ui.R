library(plotly) # ADDED: Include plotly library for UI components

metadataTimeOverlapPlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Omics Time Overlap Plots (Interactive)"),
    uiOutput(ns("time_overlap_plot_container"))
  )
}
