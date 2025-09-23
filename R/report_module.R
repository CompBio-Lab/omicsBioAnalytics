#' UI of Generate Report page
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
report_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput(ns("title"), "Title", value = "",
                         placeholder = "Title of report..."),
        shiny::textInput(ns("author"), "Author", value = "",
                         placeholder = "Author..."),
        shiny::textInput(ns("aff"), "Affiliation", value = "",
                         placeholder = "Affiliation..."),
        shiny::selectInput(
          inputId = ns("figs"),
          label = "Figures:",
          choices = "None",
          size = 10, selectize = FALSE,
          selected = "None"
        ),
        shiny::fileInput(ns("cFig"), "Upload custom figure"),
        shiny::downloadButton(ns("report"), "Generate report")
      ),
      shiny::mainPanel(
        shiny::h1("Add section"),
        shiny::imageOutput(ns("myImage")),
        shiny::div(
          shiny::h4("Text"),
          shiny::tags$textarea(
            "**Markdown** syntax is allowed.",
            id = ns("markdowninput"),
            rows = 3,
            style = "width:80%;")),
        shiny::actionButton(ns("h1Btn"), "Insert section",
                            color = "primary", style = "height: 20%"),
        shiny::uiOutput(ns("dragAndDrop")),
        shiny::hr(),
        shiny::h4("Files in tempdir()"),
        shiny::actionButton(ns("refresh_tempdir"), "Refresh"),
        DT::dataTableOutput(ns("tempdir_list"))
      )
    )
  )
}

#' Reactive values for report_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{response_var}{reactive vector of categories
#'   for the response variable}
#' }
#' @export
report_ui_vars <- function(input, output, session) {

  return(
    list(
      title = shiny::reactive({
        input$title
      }),
      author = shiny::reactive({
        input$author
      }),
      aff = shiny::reactive({
        input$aff
      }),
      figs = shiny::reactive({
        input$figs
      }),
      cFig = shiny::reactive({
        input$cFig
      }),
      report = shiny::reactive({
        input$report
      }),
      markdowninput = shiny::reactive({
        input$markdowninput
      }),
      h1Btn = shiny::reactive({
        input$h1Btn
      })
    )
  )
}

#' Generate Report module server-side processing
#' Populates the Figures selectInput with .png filenames from tempdir()
#' and auto-refreshes every 3 seconds.
#' @export
report_server <- function(input, output, session, report_ui_vars) {
  ns <- session$ns

  # --- helper: list .png files in tempdir (filenames only) ---
  list_pngs <- function() {
    list.files(tempdir(), pattern = "(?i)\\.png$", full.names = FALSE)
  }

  # --- helper: build a tidy table of files in tempdir() ---
  build_tempdir_df <- function() {
    files <- list.files(tempdir(), full.names = TRUE)
    if (!length(files)) {
      return(data.frame(
        File = character(), Size_KB = numeric(),
        Modified = as.POSIXct(character()), Path = character(),
        stringsAsFactors = FALSE
      ))
    }
    info <- file.info(files)
    out <- data.frame(
      File     = basename(files),
      Size_KB  = round(info$size / 1024, 1),
      Modified = info$mtime,
      Path     = files,
      stringsAsFactors = FALSE,
      row.names = NULL,
      check.names = FALSE
    )
    out[order(out$Modified, decreasing = TRUE), ]
  }

  # --- auto refresh timer (every 3s) ---
  rt <- shiny::reactiveTimer(3000, session)

  # --- keep an ordered list of added sections for drag-n-drop ---
  list_of_elements <- list()
  tracker <- shiny::reactiveValues(section = list(), fig = list())

  # --- auto-refresh: tempdir table + Figures choices (with .png extension) ---
  shiny::observe({
    rt()  # tick

    # 1) Update the tempdir() table
    df <- build_tempdir_df()
    output$tempdir_list <- DT::renderDataTable(
      df,
      options = list(pageLength = 10, order = list(list(2, "desc"))),
      rownames = FALSE
    )

    # 2) Update Figures select with actual .png filenames
    pngs <- list_pngs()
    choices <- c("None", pngs)
    cur <- isolate(input$figs)
    sel <- if (is.null(cur) || !(cur %in% choices)) "None" else cur
    print(sel)
    shiny::updateSelectInput(
      session,
      inputId = "figs",
      label   = "Figures:",
      choices = c("None", pngs),  # << includes .png extensions
      selected = sel
    )
  })

  # --- handle user-uploaded custom figure: copy into tempdir() ---
  shiny::observeEvent(report_ui_vars$cFig(), {
    inFile <- report_ui_vars$cFig(); req(inFile)
    file.copy(inFile$datapath, file.path(tempdir(), inFile$name), overwrite = TRUE)
    # next timer tick will refresh the selectInput automatically
  })

  # --- add a new manuscript section (text + optional figure) ---
  shiny::observeEvent(report_ui_vars$h1Btn(), {
    nr <- report_ui_vars$h1Btn()
    id <- ns(paste0("input", nr))

    tracker$section[[id]]$txt <- report_ui_vars$markdowninput()
    tracker$section[[id]]$fig <- report_ui_vars$figs()  # now stores filename.png or "None"

    element <- shiny::div(
      style = "display:flex; justify-content:space-between; align-items:center; width:100%",
      id = ns(paste0("newInput", nr)),
      report_ui_vars$markdowninput(),
      shiny::br(),
      ifelse(report_ui_vars$figs() == "None", "", paste("Attached Fig:", report_ui_vars$figs())),
      shiny::actionButton(ns(paste0("removeBtn", nr)), "Remove")
    )
    list_of_elements[[id]] <<- element

    # Render the drag-and-drop UI
    output$dragAndDrop <- shiny::renderUI({
      shiny::fluidRow(
        shiny::column(
          width = 12,
          sortable::rank_list(
            text = "Drag the items in any desired order",
            labels = list_of_elements,
            input_id = ns("rank_list_1")
          ),
          shiny::verbatimTextOutput(ns("results"))
        )
      )
    })

    # Remove button for this section
    shiny::observeEvent(input[[paste0("removeBtn", nr)]], {
      shiny::removeUI(selector = paste0("#", ns(paste0("newInput", nr))))
      list_of_elements <<- list_of_elements[names(list_of_elements) != ns(paste0("input", nr))]

      output$dragAndDrop <- shiny::renderUI({
        shiny::fluidRow(
          shiny::column(
            width = 12,
            sortable::rank_list(
              text = "Drag the items in any desired order",
              labels = list_of_elements,
              input_id = ns("rank_list_1")
            )
          )
        )
      })
    })
  })

  # --- keep list_of_elements in user-selected order ---
  shiny::observe({
    ord <- input$rank_list_1
    if (!is.null(ord)) list_of_elements <<- list_of_elements[ord]
  })

  # --- generate and download the Word report via R Markdown ---
  output$report <- shiny::downloadHandler(
    filename = function() sprintf("report_%s.docx", Sys.Date()),
    contentType = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      if (file.exists("HFdiagnosis.bib")) {
        file.copy("HFdiagnosis.bib", file.path(tempdir(), "HFdiagnosis.bib"), overwrite = TRUE)
      }

      params <- list(
        title       = report_ui_vars$title(),
        author      = report_ui_vars$author(),
        affiliation = report_ui_vars$aff(),
        section     = tracker$section,
        ord         = names(list_of_elements)
      )

      shiny::withProgress(message = "Download in progress",
                          detail = "This may take a while...", value = 0, {
                            rmarkdown::render(
                              input         = tempReport,
                              output_file   = file,                      # Shiny's temp path for the docx
                              output_format = rmarkdown::word_document(),# or rely on YAML in the Rmd
                              params        = params,
                              envir         = new.env(parent = globalenv()),
                              quiet         = TRUE
                            )
                          })
    }
  )


  # --- preview the selected image (works if selection includes .png or not) ---
  output$myImage <- shiny::renderImage({
    sel <- report_ui_vars$figs()
    req(sel, sel != "None")
    fname <- if (grepl("(?i)\\.png$", sel)) sel else paste0(sel, ".png")
    list(
      src = file.path(tempdir(), fname),
      contentType = "image/png",
      width = 400, height = 300
    )
  }, deleteFile = FALSE)
}

# handy infix for defaulting NULL
`%||%` <- function(x, y) if (is.null(x)) y else x
