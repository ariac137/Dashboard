library(shiny)

# Source the submodule UI and Server files
source("modules/metadata/metadata_table_render_ui.R")
source("modules/metadata/metadata_data_loader_server.R") # NEW: Data loading logic
source("modules/metadata/metadata_table_render_server.R")
source("modules/metadata/metadata_timeline_plot_server.R")

mainPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("main_tabs"),
      tabPanel(
        "Table View",
        renderTableUI(ns("table_render_logic"))
      ),
      tabPanel(
        "Timeline Plots",
        metadataTimelinePlotsUI(ns("timeline_plots"))
      )
    )
  )
}

mainPageServer <- function(id, uploaded_reactive, omics_names_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Load metadata
    loaded_data <- dataLoaderServer(
      id = "data_loading_logic",
      uploaded_file_reactive = uploaded_reactive$metadata_file
    )
    
    # 2. Render table
    renderTableServer(
      id = "table_render_logic",
      processed_data_reactive = loaded_data$processed_data
    )
    
    # 3. Render dynamic omics timeline plots
    metadataTimelinePlotsServer(
      id = "timeline_plots",
      metadata_reactive = loaded_data$processed_data,
      omics_names_reactive = omics_names_reactive
    )
    
  })
}
