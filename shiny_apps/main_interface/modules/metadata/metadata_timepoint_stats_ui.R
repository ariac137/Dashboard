# metadata_timepoint_stats_ui.R

library(shiny)
library(DT)

#' UI element for the Omics Timepoint Statistics Table.
#'
#' @param id Module ID.
metadataTimepointStatsUI <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    h3("Omics Timepoint Statistics Table"),
    p("Summary of time points collected for each omics type (Count, Min, and Max)."),
    # Output slot for the interactive DT table
    DT::DTOutput(ns("omics_time_stats_table"))
  )
}