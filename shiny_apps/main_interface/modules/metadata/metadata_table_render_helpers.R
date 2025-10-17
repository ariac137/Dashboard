# -----------------------------------------------------------
# 1. Load Required Library (DT for interactive display)
# -----------------------------------------------------------
# NOTE: You must install this package if you haven't already:
# install.packages("DT")
library(DT)

# -----------------------------------------------------------
# 2. Function Definition: render_interactive_table
# -----------------------------------------------------------
#' Renders a data frame as an interactive, searchable HTML table.
#'
#' @param df The data frame to render (e.g., the output from core_process_df).
#' @param title Optional character string for the table caption.
#' @return An HTML widget that displays the interactive table.
render_interactive_table <- function(df, title = "Processed Data Table") {
  
  # Ensure the input is a data frame (datatable works best with them)
  if (!is.data.frame(df)) {
    warning("Input is not a data frame. Attempting to convert.")
    df <- as.data.frame(df)
  }
  
  # Use the datatable function for interactive rendering
  datatable(
    df,
    extensions = 'Buttons', # Include extensions for buttons
    rownames = FALSE, # Ensure a consistent row name display
    caption = htmltools::tags$caption(
      style = 'caption-side: top; text-align: left; color: #1f3c7a; font-size: 1.2em;',
      title
    ),
    # Add options for a great user experience
    options = list(
      columnDefs = list(list(className = 'dt-center', targets = '_all')),
      
      # CRITICAL CHANGE: Show ALL entries at once
      pageLength = -1,  
      
      # CRITICAL CHANGE: Remove pagination controls ('l' and 'p')
      # B: Buttons, f: filtering input, t: table, r: processing
      dom = 'Bfrt', 
      
      # Add standard buttons (often included via extensions = 'Buttons')
      extensions = 'Buttons',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      
      # Explicitly disable pagination
      paging = FALSE, 
      
      # Ensure column widths adapt automatically
      autoWidth = TRUE,
      
      # Add a hover effect to rows
      rowCallback = JS(
        "function(row, data, index) {",
        "  $(row).hover(",
        "    function() { $(this).css('background-color', '#f0f0f0'); },",
        "    function() { $(this).css('background-color', ''); }",
        "  );",
        "}"
      )
    ) # No trailing comma here
  )
}