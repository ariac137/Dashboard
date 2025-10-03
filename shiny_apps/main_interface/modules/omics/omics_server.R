# omics_server.R (MINIMAL SERVER WRAPPER)

library(shiny)

# Source Omics-specific logic (now the flow file)
source("modules/omics/omics_ui.R")
source("modules/omics/omics_ui_helpers.R")
source("modules/omics/loading_server.R") 
source("modules/omics/loading_ui.R")

omicsModalServer <- function(id, save_dir = tempdir()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(files = list(), preview = list())
    loader <- loadingOverlayServer(paste0(ns("loader")))
    
    # UI-driven events
    observeEvent(input$open_modal, { showModal(makeOmicsModal(ns)) })
    
    # 1. DELEGATE: Complex Batch Upload Logic
    observeEvent(input$omics_files, {
      req(input$omics_files)
      handle_omics_file_upload(
        input_files = input$omics_files,
        rv = rv,
        loader = loader,
        session = session,
        save_dir = save_dir
      )
    })
    
    # 2. File List UI Rendering
    output$file_list_ui <- renderUI({ 
      if (length(rv$files) == 0) {
        tags$span("No files uploaded", class = "no-files")
      } else {
        tagList(
          lapply(names(rv$files), function(uid) {
            fluidRow(
              column(8, tags$span(rv$files[[uid]]$name)),
              column(4, actionButton(ns(paste0("remove_", uid)), "Remove", class="btn-xs btn-danger"))
            )
          })
        )
      }
    })
    
    # 3. DELEGATE: File Removal Logic
    observe({
      lapply(names(rv$files), function(uid) {
        observeEvent(input[[paste0("remove_", uid)]], { 
          handle_file_removal(rv, uid) 
        }, ignoreInit = TRUE)
      })
    })
    
    # 4. Render Previews
    renderPreviews(rv, output, session)
    
    # 5. DELEGATE: Final Upload/Summary Logic
    observeEvent(input$upload_btn_omics, {
      showUploadSummary(rv) 
    })
    
    return(list(
      files = reactive(rv$files),
      preview = reactive(rv$preview)
    ))
  })
}