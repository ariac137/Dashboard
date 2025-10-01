renderPreviews <- function(rv, output, session) {
  ns <- session$ns
  output$preview_ui <- renderUI({
    if (length(rv$preview) == 0) return(NULL)
    tagList(
      p(em("Preview shows only the first 5 rows and the first 5 columns")),
      lapply(names(rv$preview), function(uid) {
        sheets <- rv$preview[[uid]]
        tab_panels <- lapply(names(sheets), function(sheet_name) {
          bslib::nav_panel(
            title = sheet_name,
            DT::DTOutput(ns(paste0("tbl_", uid, "_", sheet_name)))
          )
        })
        div(
          class = "preview-table",   # <-- wrap each file in the frame
          h5(rv$files[[uid]]$name),
          bslib::navset_tab(!!!tab_panels)
        )
      })
    )
  })
  
  observe({
    lapply(names(rv$preview), function(uid) {
      sheets <- rv$preview[[uid]]
      lapply(names(sheets), function(sheet_name) {
        local({
          df <- sheets[[sheet_name]]
          output[[paste0("tbl_", uid, "_", sheet_name)]] <- DT::renderDataTable({
            df[seq_len(min(5, nrow(df))), seq_len(min(5, ncol(df)))] |> 
              DT::datatable(options = list(paging = FALSE, searching = FALSE, info = FALSE))
          })
        })
      })
    })
  })
}
