# modules/screenshot/screenshot_ui.R

library(shiny)
library(shinyjs)

# UI function for the screenshot button
screenshotButtonUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Use shinyjs for custom JavaScript and a button for triggering the screenshot
    # The 'screenshot_btn' ID will be targeted by the server-side logic
    actionButton(ns("screenshot_btn"), 
                 label = "Full Screenshot", 
                 icon = icon("camera-retro")),
    # Add a custom class for styling (e.g., placing it in the sidebar)
    tags$script(HTML("
      // Load html2canvas from a CDN (or include locally if preferred)
      // This library handles the heavy lifting of rendering the page to a canvas
      if (typeof html2canvas === 'undefined') {
        $.getScript('https://html2canvas.hertzen.com/dist/html2canvas.min.js');
      }
    "))
  )
}