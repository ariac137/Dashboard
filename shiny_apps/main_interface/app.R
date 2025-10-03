library(shiny)
source("modules/sidebar.R")  # existing module
source("modules/main_page.R") 

options(shiny.maxRequestSize = 500 * 1024^2)

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
  
  uploaded <- sidebarServer("sidebar")  # existing module
  mainPageServer(
    id = "main_page_logic",
    uploaded_reactive = uploaded,
    omics_names_reactive = uploaded$omics_names
  )

}



shinyApp(ui, server)
