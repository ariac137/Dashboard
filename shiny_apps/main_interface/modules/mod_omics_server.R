source("modules/mod_omics_file_handling.R")
source("modules/mod_omics_preview_renderer.R")
source("modules/mod_omics_ui.R")

omicsModalServer <- function(id, save_dir = tempdir()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(files = list(), preview = list())
    
    observeEvent(input$open_modal, { showModal(makeOmicsModal(ns)) })
    
    observeEvent(input$omics_files, {
      files_df <- input$omics_files
      req(files_df)
      
      for (i in seq_len(nrow(files_df))) {
        orig_name <- files_df$name[i]
        tmp_path <- files_df$datapath[i]
        if (any(sapply(isolate(rv$files), function(x) x$name == orig_name))) next
        
        uid <- paste0("f", uuid::UUIDgenerate())
        dest_path <- addFile(rv, uid, orig_name, tmp_path, save_dir)
        rv$preview[[uid]] <- safe_preview(dest_path, tools::file_ext(orig_name))
      }
      
      session$sendInputMessage("omics_files", list(value = NULL))
    })
    
    # File list UI
    output$file_list_ui <- renderUI({
      box_content <- if (length(rv$files) == 0) {
        tags$span("No files uploaded", class = "no-files")
      } else {
        tagList(
          lapply(names(rv$files), function(uid) {
            fname <- rv$files[[uid]]$name
            fluidRow(
              column(8, tags$span(fname)),
              column(4, actionButton(ns(paste0("remove_", uid)), "Remove", class="btn-xs btn-danger"))
            )
          })
        )
      }
      div(class = "file-box", hr(class = "section-separator"), box_content)
    })
    
    # Remove files
    observe({
      lapply(names(rv$files), function(uid) {
        observeEvent(input[[paste0("remove_", uid)]], { removeFile(rv, uid) }, ignoreInit=TRUE)
      })
    })
    
    renderPreviews(rv, output, session)
    observeEvent(input$upload_btn_omics, { showUploadSummary(rv) })
    
    return(list(files = reactive(rv$files), preview = reactive(rv$preview)))
  })
}
