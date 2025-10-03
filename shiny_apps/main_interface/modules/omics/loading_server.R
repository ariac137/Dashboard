loadingOverlayServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    list(
      show = function() {
        insertUI(
          selector = ".modal-content:first",   # inside the modal
          where = "beforeEnd",
          ui = div(
            id = session$ns("loading_overlay"),
            class = "modal-loading-overlay",
            "Currently processing files..."
          )
        )
      },
      hide = function() {
        removeUI(selector = paste0("#", session$ns("loading_overlay")))
      }
    )
  })
}
