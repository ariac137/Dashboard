library(shiny)
source("modules/sidebar.R")  # existing module
source("modules/main_page.R") 

options(shiny.maxRequestSize = 500 * 1024^2)

DEFAULT_OMICS_NAMES_FILE <- "data/omics_name.csv"
DEFAULT_METADATA_FILE <- "data/metadata_PREMITUR_csv.csv"

ui <- fluidPage(
  #theme = bslib::bs_theme(bootswatch = "cerulean"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
  
  titlePanel("Multi-Omics Data Upload Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Existing file upload module
      sidebarUI("sidebar"),
      width = 3
    ),
    
    mainPanel(
      width = 9, 
      mainPageUI("main_page_logic"),
    )
  )
)

server <- function(input, output, session) {
  
  uploaded <- sidebarServer(
    id = "sidebar",
    default_omics_names_path = DEFAULT_OMICS_NAMES_FILE,
    default_metadata_path = DEFAULT_METADATA_FILE
  ) 
  mainPageServer(
    id = "main_page_logic",
    uploaded_reactive = uploaded,
    omics_names_reactive = uploaded$omics_names
  )

}



shinyApp(ui, server)
