library(shiny)

# Increase max file size (optional)
options(shiny.maxRequestSize = 500 * 1024^2) # 500 MB

ui <- fluidPage(
  titlePanel("Multi-Batch Omics Upload Demo"),
  actionButton("open_modal", "Upload Omics Files"),
  h4("Uploaded Files:"),
  uiOutput("file_list_ui"),
  h4("Previews:"),
  uiOutput("preview_ui")
)

server <- function(input, output, session) {
  rv <- reactiveValues(files = list(), preview = list())
  
  safe_preview <- function(path, ext, n = 5) {
    tryCatch({
      ext <- tolower(ext)
      if (ext == "csv") utils::read.csv(path, nrows = n, check.names = FALSE)
      else if (ext %in% c("tsv", "txt")) utils::read.table(path, sep="\t", header=TRUE, nrows=n)
      else if (ext %in% c("xlsx","xls") && requireNamespace("readxl", quietly=TRUE)) readxl::read_excel(path, n_max=n)
      else if (ext == "rds") { obj <- readRDS(path); if(is.data.frame(obj)) head(obj,n) else data.frame(value=utils::capture.output(head(obj,n))) }
      else data.frame(note=paste("No preview for", ext))
    }, error=function(e) data.frame(error=conditionMessage(e)))
  }
  
  # Modal to select files
  observeEvent(input$open_modal, {
    showModal(modalDialog(
      title = "Upload Omics Files",
      fileInput("omics_files", "Select files", multiple = TRUE,
                accept=c(".csv",".tsv",".txt",".xlsx",".rds")),
      footer = tagList(
        modalButton("Cancel")
      ),
      size="l",
      easyClose = TRUE
    ))
  })
  
  # Handle uploaded files
  observeEvent(input$omics_files, {
    req(input$omics_files)
    files_df <- input$omics_files
    for (i in seq_len(nrow(files_df))) {
      orig <- files_df$name[i]
      tmp  <- files_df$datapath[i]
      uid  <- paste0("f", round(as.numeric(Sys.time())*1000), "_", sample(1:999,1))
      rv$files[[uid]] <- list(name=orig, path=tmp)
      rv$preview[[uid]] <- safe_preview(tmp, tools::file_ext(orig))
    }
    removeModal()
  })
  
  # Render file list with remove buttons
  output$file_list_ui <- renderUI({
    if(!length(rv$files)) return("No files uploaded yet")
    lapply(names(rv$files), function(uid) {
      fluidRow(
        column(8, rv$files[[uid]]$name),
        column(4, actionButton(paste0("rm_",uid), "Remove", class="btn-xs btn-danger"))
      )
    })
  })
  
  # Render previews
  output$preview_ui <- renderUI({
    lapply(names(rv$preview), function(uid) {
      tableOutput(paste0("tbl_", uid))
    })
  })
  
  # Dynamic observers for remove buttons and previews
  observe({
    lapply(names(rv$files), function(uid) {
      # Preview
      output[[paste0("tbl_", uid)]] <- renderTable({
        rv$preview[[uid]]
      }, rownames=TRUE)
      
      # Remove
      observeEvent(input[[paste0("rm_", uid)]], {
        rv$files[[uid]] <- NULL
        rv$preview[[uid]] <- NULL
      }, ignoreInit=TRUE, once=TRUE)
    })
  })
}

shinyApp(ui, server)
