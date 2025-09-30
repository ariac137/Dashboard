renderPreviews <- function(rv, output, session) {
  ns <- session$ns
  output$preview_ui <- renderUI({
    if (length(rv$preview) == 0) return(NULL)
    tagList(
      p(em("Preview shows only the first 5 rows and the first 5 columns")),
      lapply(names(rv$preview), function(uid) {
        div(class = "preview-table",
            h5(rv$files[[uid]]$name),
            DT::DTOutput(ns(paste0("tbl_", uid)))
        )
      })
    )
  })
  
  observe({
    lapply(names(rv$preview), function(uid) {
      local({
        id <- uid
        output[[paste0("tbl_", id)]] <- DT::renderDataTable({
          df <- rv$preview[[id]]
          df <- df[seq_len(min(5, nrow(df))), seq_len(min(5, ncol(df)))]
          DT::datatable(df, options = list(paging = FALSE, searching = FALSE, info = FALSE))
        })
      })
    })
  })
}
