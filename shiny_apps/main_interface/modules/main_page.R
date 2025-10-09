library(shiny)

# Source the submodule UI and Server files
source("modules/metadata/metadata_table_render_ui.R")
source("modules/metadata/metadata_data_loader_server.R") # NEW: Data loading logic
source("modules/metadata/metadata_table_render_server.R")
source("modules/metadata/metadata_time_overlap_plot_server.R")
source("modules/metadata/metadata_timeline_plot_server.R")

mainPageUI <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      id = ns("main_tabs"),
      # tabPanel(
      #   "Table View",
      #   renderTableUI(ns("table_render_logic"))
      # ),
      # 1. Timeline Plots Tab
      tabPanel(
        "Timeline Plots",
        # Calls the UI function for the multi-plot module
        metadataTimelinePlotsUI(ns("timeline_plots"))
      ),
      
      # 2. Time Overlap Plots Tab (NEW)
      tabPanel(
        "Time Overlap Plot",
        # Calls the UI function for the single-plot module
        metadataTimeOverlapPlotsUI(ns("time_overlap_plots"))
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
    
    # # 2. Render table
    # renderTableServer(
    #   id = "table_render_logic",
    #   processed_data_reactive = loaded_data$processed_data
    # )
    
    # 3. Render dynamic omics timeline plots
    timeline_plots_reactive <- metadataTimelinePlotsServer(
      id = "timeline_plots",
      metadata_reactive = loaded_data$processed_data,
      omics_names_reactive = omics_names_reactive
    )
    
    # 4. Render dynamic omics time overlap plot (NEW)
    overlap_plot_reactive <- metadataTimeOverlapPlotsServer(
      id = "time_overlap_plots",
      metadata_reactive = loaded_data$processed_data,
      omics_names_reactive = omics_names_reactive
    )
    
  })
}
