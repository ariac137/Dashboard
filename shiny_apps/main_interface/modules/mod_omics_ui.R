makeOmicsModal <- function(ns) {
  modalDialog(
    title = tags$span("Upload Omics Files", style="color:black; font-weight:bold;"),
    
    # File selection card
    div(class="card",
        div(class="card-header", "Select Omics Files"),
        div(class="card-body",
            fileInput(ns("omics_files"), label = NULL, multiple = TRUE,
                      accept = c(".csv",".tsv",".txt",".xlsx",".rds"))
        )
    ),
    
    # Current files card
    div(class="card",
        div(class="card-header", "Current Files"),
        div(class="card-body", uiOutput(ns("file_list_ui")))
    ),
    
    # Preview card
    div(class="card",
        div(class="card-header", "Previews"),
        div(class="card-body", uiOutput(ns("preview_ui")))
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("upload_btn_omics"), "Upload & Save", class="btn-primary")
    ),
    size = "l",
    easyClose = TRUE
  )
}
