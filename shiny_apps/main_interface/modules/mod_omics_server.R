source("modules/mod_omics_file_handling.R")
source("modules/mod_omics_preview_renderer.R")
source("modules/mod_omics_ui.R")
source("modules/mod_loading_ui.R")
source("modules/mod_loading_server.R")

omicsModalServer <- function(id, save_dir = tempdir()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(files = list(), preview = list())
    
    # Initialize loading module
    loader <- loadingOverlayServer(paste0(ns("loader")))
    
    # Show modal when button clicked
    observeEvent(input$open_modal, {
      showModal(makeOmicsModal(ns))
    })
    
    # Handle file input
    observeEvent(input$omics_files, {
      files_df <- input$omics_files
      req(files_df)
      
      for (i in seq_len(nrow(files_df))) {
        orig_name <- files_df$name[i]
        tmp_path <- files_df$datapath[i]
        
        # Skip duplicates
        if (any(sapply(isolate(rv$files), function(x) x$name == orig_name))) next
        
        uid <- paste0("f", uuid::UUIDgenerate())
        dest_path <- addFile(rv, uid, orig_name, tmp_path, save_dir)
        
        # Show loader for this file inside the modal
        loader$show()
        
        # Generate preview asynchronously
        future({
          safe_preview(dest_path, tools::file_ext(orig_name))
        }) %...>% (function(preview_df) {
          isolate({
            rv$preview[[uid]] <- preview_df
          })
          loader$hide()  # hide overlay when done
        }) %...!% (function(e) {
          loader$hide()
          showModal(modalDialog(
            title = "Error",
            paste("Error processing", orig_name, ":", e$message),
            easyClose = TRUE
          ))
        })
      }
      
      # Reset fileInput
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
        observeEvent(input[[paste0("remove_", uid)]], { removeFile(rv, uid) }, ignoreInit = TRUE)
      })
    })
    
    # Render previews with constrained size
    renderPreviews(rv, output, session)
    
    # Upload & save button
    observeEvent(input$upload_btn_omics, {
      showUploadSummary(rv)
    })
    
    return(list(
      files = reactive(rv$files),
      preview = reactive(rv$preview)
    ))
  })
}
