metadataTimelinePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Omics Timeline Plots (Faceted Comparison)"),
    # Only the Coloring dropdown remains
    fluidRow(
      column(6, # Using column(6) for a centered, medium-width input
             # Dropdown for selecting the categorical column to color by
             uiOutput(ns("color_column_ui")) 
      )
    ),
    # Plot container
    uiOutput(ns("timeline_plot_container"))
  )
}

# The UI that contains the actual dropdown input
colorColumnUI <- function(ns, choices) {
  selectInput(
    ns("color_column"), 
    "Color Points & Subject Labels by", # <--- Updated Label for conciseness
    choices = choices,
    selected = "None" # <--- Ensures input starts as "None", not NULL
  )
}
