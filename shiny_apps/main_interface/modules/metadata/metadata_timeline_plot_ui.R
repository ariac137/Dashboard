# metadata_timeline_plot_ui.R

metadataTimelinePlotsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Omics Timeline Plots"),
    
    # NEW: Color Palette Selection
    fluidRow(
      column(12,
             selectInput(
               ns("point_color_palette"),
               "Point Color Palette",
               choices = c(
                 # Qualitative Palettes
                 "Set3", "Set1", "Set2", "Pastel1", "Pastel2", "Dark2", "Accent", "Paired",
                 # Divergent Palettes (Good for ordered categories)
                 "Spectral", "RdYlBu", "PRGn", "BrBG", "PiYG", "PuOr"
               ),
               selected = "Set1"
             )
      )
    ),
    
    # Container for two side-by-side dropdowns
    fluidRow(
      column(6, 
             # Dropdown for strip color (and y-axis labels)
             uiOutput(ns("strip_color_column_ui")) 
      ),
      column(6,
             # Dropdown for time point marker color
             uiOutput(ns("point_color_column_ui"))
      )
    ),
    # Plot container
    uiOutput(ns("timeline_plot_container")),
    
    # The table UI will be placed below this container in the main server file
    # by calling the new contingency table module UI.
  )
}

# The generic UI that creates a single dropdown input (unchanged)
timelineColorDropdownUI <- function(ns, inputId, label, choices) {
  selectInput(
    ns(inputId), 
    label,
    choices = choices,
    selected = "None"
  )
}