loadingOverlayUI <- function(id, text = "Loading...") {
  ns <- NS(id)
  div(
    id = ns("overlay"),
    class = "modal-loading-overlay",
    text
  )
}
