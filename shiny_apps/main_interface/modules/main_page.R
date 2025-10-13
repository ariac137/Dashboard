# main_page.R

library(shiny)

# Source the submodule UI and Server files
# source("modules/metadata/metadata_table_render_ui.R") # Commented out in original
source("modules/metadata/metadata_data_loader_server.R")
# source("modules/metadata/metadata_table_render_server.R") # Commented out in original
source("modules/metadata/metadata_time_overlap_plot_server.R")
source("modules/metadata/metadata_timeline_plot_server.R")

# NEW SOURCES FOR THE TABLE MODULE
source("modules/metadata/metadata_contingency_table_ui.R")
source("modules/metadata/metadata_contingency_table_server.R")

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
        fluidRow(
          column(12, 
                 # Calls the UI function for the multi-plot module
                 metadataTimelinePlotsUI(ns("timeline_plots")),
                 
                 # CRUCIAL: The Contingency Table UI must be placed here, below the plot module UI
                 metadataContingencyTableUI(ns("contingency_table"))
          )
        )
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
    
    # 2. Render dynamic omics timeline plots (returns list(plot, group_col))
    timeline_module_outputs <- metadataTimelinePlotsServer(
      id = "timeline_plots",
      metadata_reactive = loaded_data$processed_data,
      omics_names_reactive = omics_names_reactive
    )
    
    # 3. Render dynamic omics contingency table
    # Pass the selected group column reactive from the timeline plot module
    metadataContingencyTableServer(
      id = "contingency_table",
      metadata_reactive = loaded_data$processed_data,
      omics_names_reactive = omics_names_reactive,
      group_col_reactive = timeline_module_outputs$group_col 
    )
    
    # 4. Render dynamic omics time overlap plot
    overlap_plot_reactive <- metadataTimeOverlapPlotsServer(
      id = "time_overlap_plots",
      metadata_reactive = loaded_data$processed_data,
      omics_names_reactive = omics_names_reactive
    )
    
  })
}