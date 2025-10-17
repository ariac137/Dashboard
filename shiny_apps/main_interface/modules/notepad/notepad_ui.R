# modules/notepad/notepad_ui.R

library(shiny)
library(htmltools)
library(bslib)

# Function to generate a unique ID for each new panel
generate_id <- function(prefix = "note-panel-") {
  paste0(prefix, sample.int(100000, 1))
}

# --- UI Definition ---
notepadUI <- function(id) {
  ns <- NS(id)
  tagList(
    # 1. Button to add a new note
    actionButton(
      ns("add_note"),
      label = "Add Note",
      icon = icon("plus"),
      class = "btn-primary"
    ),
    
    # 2. The hidden 'actionButton' is completely removed here to eliminate the 
    #    "transparent window" that was appearing at the bottom of the page.
    
    # 3. JavaScript to handle double-click for editing AND the close button
    tags$script(HTML(paste0("
      // --- DOUBLE-CLICK EDITING ---
      $(document).on('dblclick', '.note-content', function() {
        var currentText = $(this).text();
        var textarea = $('<textarea class=\"form-control note-editor\" style=\"height: 100%; width: 100%;\">').val(currentText);
        $(this).replaceWith(textarea);
        textarea.focus();
      });
      
      // Listener for when editing is finished (blur)
      $(document).on('blur', '.note-editor', function() {
        var newText = $(this).val();
        var contentDiv = $('<div class=\"note-content\" style=\"cursor: text; height: 100%; overflow: auto; background-color: white;\">').text(newText);
        $(this).replaceWith(contentDiv);
      });
      
      // --- CLOSE BUTTON FIX (Using robust Shiny.setInputValue) ---
      $(document).on('click', '.note-close-btn', function() {
        var panel_id = $(this).closest('.note-panel-absolute').attr('id');
        
        // Send the panel ID back to the R server. We'll use 'note_to_remove' 
        // as the new, clean input name.
        Shiny.setInputValue('", ns('note_to_remove'), "', panel_id, {priority: 'event'});
      });
    ")))
  )
}