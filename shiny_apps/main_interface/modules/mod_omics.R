omicsModalServer <- function(id, save_dir = tempdir(), rdata_prefix = "omics") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      files = list(),    # uid -> list(name, path)
      preview = list()   # uid -> data.frame preview
    )
    
    safe_preview <- function(path, ext, n = 5) {
      tryCatch({
        ext <- tolower(ext)
        if (ext %in% c("csv","tsv","txt")) {
          # Prefer data.table::fread for speed and auto-detection
          if (requireNamespace("data.table", quietly = TRUE)) {
            data.table::fread(path, nrows = n, data.table = FALSE)
          } else if (requireNamespace("readr", quietly = TRUE)) {
            readr::read_delim(path, delim = NULL, n_max = n, show_col_types = FALSE)
          } else {
            # Fallback: base R (forces sep guess: try tab, then comma, else whitespace)
            firstline <- readLines(path, n = 1)
            sep <- if (grepl("\t", firstline)) "\t"
            else if (grepl(",", firstline)) ","
            else ""
            utils::read.table(path, sep = sep, header = TRUE, nrows = n, check.names = FALSE)
          }
        } else if (ext %in% c("xlsx","xls") && requireNamespace("readxl", quietly = TRUE)) {
          readxl::read_excel(path, n_max = n)
        } else if (ext == "rds") {
          obj <- readRDS(path)
          if (is.data.frame(obj)) head(obj, n)
          else data.frame(value = utils::capture.output(head(obj, n)))
        } else {
          data.frame(note = paste("No preview for .", ext))
        }
      }, error = function(e) data.frame(error = conditionMessage(e)))
    }
    
    
    # Open modal
    observeEvent(input$open_modal, {
      showModal(modalDialog(
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
      ))
    })
    
    # Handle file input
    observeEvent(input$omics_files, {
      files_df <- input$omics_files
      req(files_df)
      
      # Show loading overlay
      insertUI(
        selector = ".modal-content:first",
        where = "beforeEnd",
        ui = div(id = session$ns("loading_overlay"),
                 class = "modal-loading-overlay",
                 "Processing files, please wait...")
      )
      
      # inside your observeEvent(input$omics_files, {...})
      
      for (i in seq_len(nrow(files_df))) {
        orig_name <- files_df$name[i]
        tmp_path <- files_df$datapath[i]
        
        if (any(sapply(isolate(rv$files), function(x) x$name == orig_name))) next
        
        uid <- paste0("f", UUIDgenerate())
        dest_path <- file.path(save_dir, paste0(uid, "_", orig_name))
        file.copy(tmp_path, dest_path, overwrite = TRUE)
        rv$files[[uid]] <- list(name = orig_name, path = dest_path)
        
        # Generate preview asynchronously
        future({
          safe_preview(dest_path, tools::file_ext(orig_name))
        }) %...>% (function(preview) {
          rv$preview[[uid]] <- preview
          
          # Dynamically create renderTable **after preview exists**
          output[[paste0("tbl_", uid)]] <- renderTable({
            req(rv$preview[[uid]])
            rv$preview[[uid]]
          }, rownames = TRUE)
        }) %...!% (function(e) {
          showModal(modalDialog(title="Error", paste("Error processing", orig_name, ":", e$message), easyClose=TRUE))
        })
      }
      
      
      # Remove overlay when all previews are ready
      observe({
        if (length(rv$files) > 0 && length(rv$preview) == length(rv$files)) {
          removeUI(selector = paste0("#", session$ns("loading_overlay")))
        }
      })
      
      # Reset fileInput for next batch
      session$sendInputMessage("omics_files", list(value = NULL))
    })
    
    # File list UI
    output$file_list_ui <- renderUI({
      # Container box for current files
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
      
      div(
        class = "file-box",
        hr(class = "section-separator"),
        box_content
      )
    })
    
    
    # Remove files
    observe({
      lapply(names(rv$files), function(uid) {
        observeEvent(input[[paste0("remove_", uid)]], {
          fpath <- rv$files[[uid]]$path
          if (!is.null(fpath) && file.exists(fpath)) file.remove(fpath)
          rv$files[[uid]] <- NULL
          rv$preview[[uid]] <- NULL
        }, ignoreInit = TRUE)
      })
    })
    
    # Preview UI
    output$preview_ui <- renderUI({
      if (length(rv$preview) == 0) return(NULL)
      tagList(
        p(em("Preview shows only the first 5 rows and the first 5 columns")),
        lapply(names(rv$preview), function(uid) {
          div(class = "preview-table",
              h5(rv$files[[uid]]$name),
              DT::DTOutput(session$ns(paste0("tbl_", uid)))
          )
        })
      )
    })
    
    
    # --- server side preview rendering ---
    observe({
      lapply(names(rv$preview), function(uid) {
        local({
          id <- uid
          output[[paste0("tbl_", id)]] <- DT::renderDataTable({
            df <- rv$preview[[id]]
            df <- df[seq_len(min(5, nrow(df))), seq_len(min(5, ncol(df)))]
            DT::datatable(
              df,
              options = list(paging = FALSE, searching = FALSE, info = FALSE)
            )
          })
        })
      })
    })
    
    
    # Upload button
    observeEvent(input$upload_btn_omics, {
      fids <- names(rv$files)
      req(length(fids) > 0)
      
      summary_msgs <- sapply(fids, function(uid) {
        paste0("Uploaded '", rv$files[[uid]]$name, "'")
      })
      
      showModal(modalDialog(
        title = "Upload Summary",
        renderPrint({ cat(paste(summary_msgs, collapse="\n")) }),
        easyClose = TRUE
      ))
      
      rv$files <- list()
      rv$preview <- list()
      removeModal()
    })
    
    return(list(
      files = reactive({ rv$files }),
      preview = reactive({ rv$preview })
    ))
  })
}
