library(shiny)
library(bslib)
library(htmltools)

# Function to generate a unique ID for each new panel
generate_id <- function(prefix = "note-panel-") {
  paste0(prefix, sample.int(100000, 1))
}

# --- UI Definition ---
ui <- page_fillable(
  # Set the background image
  style = css(
    background_image = "url(https://unsplash.com/photos/XKXGghL7GQc/download?force=true&w=1920)",
    background_repeat = "no-repeat",
    background_size = "cover",
    background_position = "center bottom",
  ),
  
  # Action button to add a new note panel
  tags$div(
    actionButton("add_note", "➕ Add Note", class = "btn-primary"),
    style = "position: absolute; top: 10px; left: 10px; z-index: 1000;"
  ),
  
  # A place to hold all the dynamically generated panels
  tags$div(id = "note_container"),
  
  # JavaScript to handle double-click and text editing
  tags$script(HTML("
    $(document).on('dblclick', '.note-content', function() {
      // Get the current text
      var currentText = $(this).text();
      // Replace the div with a textarea for editing
      var textarea = $('<textarea class=\"form-control note-editor\" style=\"height: 100%; width: 100%;\">').val(currentText);
      $(this).replaceWith(textarea);
      textarea.focus();
    });
    
    // When the textarea loses focus (blur), revert back to a display div
    $(document).on('blur', '.note-editor', function() {
      // Get the new text
      var newText = $(this).val();
      // Replace the textarea with a display div
      var contentDiv = $('<div class=\"note-content\" style=\"cursor: text; height: 100%; overflow: auto;\">').text(newText);
      $(this).replaceWith(contentDiv);
    });
  "))
)

# --------------------------------------------------------------------------------

# --- Server Logic ---
server <- function(input, output, session) {
  
  # Use a reactive value to track the number of notes (optional, but good practice)
  note_count <- reactiveVal(0)
  
  # Observer to handle the "Add Note" button click
  observeEvent(input$add_note, {
    note_count(note_count() + 1)
    note_id <- generate_id()
    
    # 1. Define the UI element to insert
    new_panel <- absolutePanel(
      id = note_id,
      class = "note-panel", # Add a class for potential custom styling
      
      # Use a wellPanel for a clean, contained look
      wellPanel(
        title = tags$strong(paste("Note #", note_count())),
        
        # This is the editable content area
        # The JS above targets the 'note-content' class
        tags$div(
          class = "note-content",
          style = "cursor: text; height: 100px; overflow: auto;",
          "Double-click here to edit this note!"
        )
      ),
      
      # 2. Set initial position and properties
      width = "250px",
      height = "150px",
      top = sample(50:400, 1), # Random initial position
      left = sample(50:800, 1), 
      draggable = TRUE # Make it draggable
    )
    
    # 3. Insert the new panel UI into the 'note_container' div
    insertUI(
      selector = "#note_container",
      where = "beforeEnd",
      ui = new_panel
    )
  })
}

# --------------------------------------------------------------------------------

# Create Shiny app ----
shinyApp(ui = ui, server = server)