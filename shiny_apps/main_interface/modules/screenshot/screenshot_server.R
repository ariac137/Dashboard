# modules/screenshot/screenshot_server.R

library(shiny)
library(shinyjs)

source("modules/screenshot/screenshot_ui.R")

# Server function to handle the screenshot logic
screenshotServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$screenshot_btn, {
      
      # 1. Prepare the custom JavaScript to execute in the browser
      #    - 'html2canvas' captures the body of the document.
      #    - It then converts the canvas to a data URL (PNG)
      #    - A temporary link is created and clicked to trigger the download.
      js_code <- HTML(
        '
        // Use html2canvas to capture the entire document body
        html2canvas(document.body).then(function(canvas) {
            
            // Convert the canvas to a data URL (PNG)
            var imgData = canvas.toDataURL("image/png");
            
            // Create a temporary link element
            var link = document.createElement("a");
            
            // Set the download filename
            var timestamp = new Date().toISOString().replace(/[:.]/g, "-");
            link.download = "dashboard-screenshot-" + timestamp + ".png";
            
            // Set the data URL as the link\'s href
            link.href = imgData;
            
            // Programmatically click the link to trigger the download
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
            
            console.log("Screenshot download triggered.");
        });
        '
      )
      
      # 2. Execute the JavaScript code on the client side
      runjs(js_code)
      
    })
    
  })
}