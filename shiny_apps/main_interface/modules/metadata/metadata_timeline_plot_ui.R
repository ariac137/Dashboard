metadataTimelinePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Omics Timeline Plots (Faceted Comparison)"),
    # This UI output will now contain a single combined plot
    uiOutput(ns("timeline_plot_container"))
  )
}