# modules/notepad/notepad_server.R

source("modules/notepad/notepad_ui.R") 

# --- Server Logic ---
notepadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    note_count <- reactiveVal(0)
    
    # 1. Observer to handle adding a new note
    observeEvent(input$add_note, {
      note_count(note_count() + 1)
      note_id <- generate_id()
      
      # Retain high Z-INDEX (9999) to keep the panel always on top
      border_style <- "box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.5); border: 1px solid #ccc; z-index: 9999;"
      
      # 1. Define the UI element to insert
      new_panel <- absolutePanel(
        id = note_id,
        class = "note-panel-absolute",
        style = border_style,          
        
        # --- MODIFICATION: Added background-color: #FFFFE0 (Light Yellow) ---
        wellPanel(
          style = "background-color: #FFFFE0; padding: 10px;", # Light Yellow
          
          # Header containing title and close button
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; cursor: move;",
            tags$strong(paste("Note #", note_count())),
            tags$button("❌", class = "btn btn-xs note-close-btn", 
                        style = "padding: 2px 5px; line-height: 1; border: none; background: none;") 
          ),
          
          # Editable content area 
          tags$div(
            class = "note-content",
            # FIX: Removed redundant background-color: white; so it inherits the yellow
            style = "cursor: text; height: 100px; overflow: auto;", 
            "Double-click here to edit this note!"
          )
        ),
        
        # 2. Set initial position and properties
        width = "250px",
        height = "150px",
        top = sample(50:400, 1),
        left = sample(50:800, 1), 
        draggable = TRUE,
        resizable = TRUE
      )
      
      # 3. Insert the new panel UI into the main page body
      insertUI(
        selector = "body", 
        where = "beforeEnd",
        ui = new_panel
      )
    })
    
    # 2. Observer to handle removing a note
    # FIX: Listen to the new, clean input name 'note_to_remove' from the JS
    observeEvent(input$note_to_remove, {
      note_id_to_remove <- input$note_to_remove
      
      if (!is.null(note_id_to_remove) && note_id_to_remove != "") {
        removeUI(
          selector = paste0("#", note_id_to_remove)
        )
      }
    }, ignoreInit = TRUE)
    
    return(NULL)
  })
}