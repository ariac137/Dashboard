library(shiny)
source("modules/mod_file_upload.R")  # existing module

options(shiny.maxRequestSize = 500 * 1024^2)

ui <- fluidPage(
  #theme = bslib::bs_theme(bootswatch = "cerulean"),
  tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
  
  titlePanel("Multi-Omics Data Upload Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Existing file upload module
      fileUploadUI("uploader"),
      width = 3
    ),
    
    mainPanel(
      width = 9
    )
  )
)

server <- function(input, output, session) {
  uploaded <- fileUploadServer("uploader")  # existing module

}



shinyApp(ui, server)
