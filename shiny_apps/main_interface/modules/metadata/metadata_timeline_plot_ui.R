metadataTimelinePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Omics Timeline Plots"),
    uiOutput(ns("plot_container"))
  )
}
