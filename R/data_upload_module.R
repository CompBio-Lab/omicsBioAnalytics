#' UI of Data Upload page
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
data_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12,
        shiny::h2("Web-based analytics"),
        shiny::radioButtons(
          ns("sep"),
          "File type (delimiter):",
          choices = c("Comma-separated (.csv)" = ",", "Tab-separated (.tsv/.txt)" = "\t"),
          selected = ","
        )
        ),
      shiny::column(6,
                    tags$div(style="display:flex; align-items:flex-start;",
                             shiny::fileInput(
                               ns("demo"), label="Metadata",
                               accept=c(".csv",".tsv",".txt")),
                             shiny::actionButton(
                               ns("demo_help"), label="", icon=icon("question"),
                               class="btn btn-link", style="color:gray; margin-left:6px;")
                    )
      ),
      shiny::column(6, shiny::uiOutput(ns("response_var"))),
      shiny::column(6, shiny::uiOutput(ns("ref_var")))
    ),
    shiny::fluidRow(
      shiny::column(6,
                    tags$div(style="display:flex; align-items:flex-start;",
                             shiny::fileInput(
                               ns("omics_data"), label="Omics data (upload one or more files)",
                               multiple = TRUE, accept=c(".csv",".tsv",".txt")),
                             shiny::actionButton(
                               ns("omics_data_help"), label="", icon=icon("question"),
                               class="btn btn-link", style="color:gray; margin-left:6px;")
                    )
        )),
    shiny::fluidRow(
      shiny::column(6,
        shiny::actionButton(ns("run"),
          "Run Analysis",
          icon = shiny::icon("play"),
          style = "color: #fff; background-color:
          #337ab7; border-color: #2e6da4"),
        shiny::uiOutput(ns("uploadErrorMsg_preread")),
        shiny::uiOutput(ns("uploadErrorMsg_postread")),
        shiny::uiOutput(ns("response_levels_check"))
      ),
      shiny::column(6,
        shiny::h3("Or try these example datasets:", align = "left"),
        shiny::fluidRow(shiny::column(6,
          customDownloadButton(ns("heart_failure"),
            label = "Heart Failure", icon = shiny::icon("heart"))),
          shiny::column(6, customDownloadButton(ns("covid19"),
            label = "COVID-19", icon = shiny::icon("chart-line")))),
        shiny::fluidRow(shiny::column(6, "read paper: ",
          shiny::a("CJC 2019",
            href = "https://www.ncbi.nlm.nih.gov/pubmed/30935638")),
          shiny::column(6, "read paper: ",
            shiny::a("Cell 2020",
              href = "https://pubmed.ncbi.nlm.nih.gov/32416070/"))),
        shiny::fluidRow(shiny::column(6,
          shiny::a("watch demo",
            href = "https://www.youtube.com/watch?v=u1zLL4uXZi8")),
          shiny::column(6, shiny::a("watch demo",
            href = "https://www.youtube.com/watch?v=oglZDscpbAU"))))
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h2("Voice-enabled analytics"),
        shiny::actionButton(
          ns("alexa"),
          "Alexa, analyze my data!",
          icon = shiny::icon("user"),
          style = "color:#fff; background-color:#337ab7; border-color:#2e6da4"
        )
      ),
      shiny::uiOutput(ns("errMsgAlexa"))
    )
  )
}

#' Reactive values for data_upload_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{response_var}{reactive vector of categories
#'   for the response variable}
#' }
#' @export
data_upload_ui_vars <- function(input, output, session) {

  return(
    list(
      sep = shiny::reactive({
        input$sep}),
      demo = shiny::reactive({
        input$demo}),
      response_var = shiny::reactive({
        input$response_var}),
      ref_var = shiny::reactive({
        input$ref_var}),
      omics_data = shiny::reactive({
        input$omics_data}),
      run = shiny::reactive({
        input$run}),
      heart_failure = shiny::reactive({
        input$heart_failure}),
      covid19 = shiny::reactive({
        input$covid19}),
      alexa = shiny::reactive({
        input$alexa}),
      modal = shiny::reactive({
        input$modal})
    )
  )
}

#' Data Upload module server-side processing (with extension validation)
#'
#' @param input,output,session standard shiny boilerplate
#' @param heart_failure_data list of data.frames for example download
#' @param covid19_data list of data.frames for example download
#' @param data_upload_ui_vars list of reactives from data_upload_ui_vars()
#' @export
data_upload_server <- function(input, output, session,
                               heart_failure_data, covid19_data, data_upload_ui_vars) {
  ns <- session$ns

  observeEvent(input$demo_help, {
    showModal(modalDialog(
      title = "Metadata example",
      HTML('<img src="metadata_example.png" width="100%" height="300px"">'),
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })

  observeEvent(input$omics_data_help, {
    showModal(modalDialog(
      title = "Omics data example",
      HTML('<img src="omicsfile_example.png" width="100%" height="300px"">'),
      easyClose = TRUE, footer = modalButton("Close")#, size = "m"
    ))
  })

  ## ---- Extension validation helpers --------------------------------------

  # Map the selected delimiter to allowed file extensions
  allowed_ext <- shiny::reactive({
    if (identical(data_upload_ui_vars$sep(), ",")) c("csv") else c("tsv", "txt")
  })

  # Human-readable label for messages: ".csv" or ".tsv or .txt"
  allowed_ext_label <- shiny::reactive({
    paste0(".", allowed_ext(), collapse = " or ")
  })

  # Build extension mismatch messages (if any)
  ext_error_msgs <- shiny::reactive({
    msgs <- character(0)

    # Demo file
    if (!is.null(data_upload_ui_vars$demo())) {
      demo_name <- data_upload_ui_vars$demo()$name
      demo_ext  <- tolower(tools::file_ext(demo_name))
      if (!demo_ext %in% allowed_ext()) {
        msgs <- c(
          msgs,
          sprintf("Metadata file must match the selected type (%s): %s",
                  allowed_ext_label(), demo_name)
        )
      }
    }

    # Omics files (possibly multiple)
    if (!is.null(data_upload_ui_vars$omics_data())) {
      om_names <- data_upload_ui_vars$omics_data()$name
      om_exts  <- tolower(tools::file_ext(om_names))
      bad_idx  <- which(!(om_exts %in% allowed_ext()))
      if (length(bad_idx)) {
        msgs <- c(
          msgs,
          sprintf("These omics files do not match the selected type (%s): %s",
                  allowed_ext_label(), paste(om_names[bad_idx], collapse = ", "))
        )
      }
    }

    if (length(msgs)) msgs else NULL
  })

  # TRUE only when all uploaded files match the chosen delimiter (or none uploaded yet)
  files_ext_ok <- shiny::reactive({
    is_demo_ok <- is.null(data_upload_ui_vars$demo()) ||
      tolower(tools::file_ext(data_upload_ui_vars$demo()$name)) %in% allowed_ext()

    is_omics_ok <- is.null(data_upload_ui_vars$omics_data()) ||
      all(tolower(tools::file_ext(data_upload_ui_vars$omics_data()$name)) %in% allowed_ext())

    is_demo_ok && is_omics_ok
  })


  ## ---- File content check - headers --------------------------------------

  # File header existence
  header_error_msgs <- shiny::reactive({

    if (is.null(data_upload_ui_vars$demo()) || is.null(data_upload_ui_vars$omics_data())) {
      return(NULL)
    } # header checks only run when demo and omics are uploaded +
      # no interfering with warning messages that need to appear before uploads!

    msgs <- character(0)

    # Demo file

    demo_file <- data_upload_ui_vars$demo()
    demo_path <- demo_file$datapath

    first_line <- readLines(demo_path, n = 1)
    demo_header_ok <- all(grepl("[A-Za-z]", strsplit(first_line, ",")[[1]]))

    # Omics files

    omics_files <- data_upload_ui_vars$omics_data()
    omics_paths <- omics_files$datapath

    omics_header_ok <- sapply(omics_paths, function(f) {
      first_line <- readLines(f, n = 1)
      all(grepl("[A-Za-z]", strsplit(first_line, ",")[[1]]))
    })
    bad_headers <- which(!omics_header_ok)


    # Error messages set up

    if (!demo_header_ok)  {
      msgs <- c(msgs, "The metadata file is missing a correct header.")
    }
    if (length(bad_headers)) {
      msgs <- c(msgs, sprintf("These omics files are missing correct headers: %s",
                paste(omics_files$name[bad_headers], collapse = ", ")))
    }

    if (length(msgs)) msgs else NULL
  })

  # TRUE only when all uploaded files have correct header
  files_header_ok <- shiny::reactive({
    is.null(header_error_msgs())
  })


  ## ---- Error / validation UI before file reading ---------------------------------------------

  output$uploadErrorMsg_preread <- shiny::renderUI({
    msgs <- character(0)

    # Required inputs
    if (is.null(data_upload_ui_vars$demo())) {
      msgs <- c(msgs, "Metadata is required with at least 1 categorical variable!")
    }
    if (is.null(data_upload_ui_vars$omics_data())) {
      msgs <- c(msgs, "At least one omics data file is required!")
    }

    # Extension mismatches
    if (!is.null(ext_error_msgs())) msgs <- c(msgs, ext_error_msgs())

    # Header checks
    if (!is.null(header_error_msgs())) msgs <- c(msgs, header_error_msgs())

    if (length(msgs)) {
      shiny::div(
        class = "alert alert-danger",
        shiny::tags$ul(lapply(msgs, shiny::tags$li))
      )
    } else {
      NULL
    }
  })

  ## ---- Readers (guarded by files_ext_ok & files_header_ok) ---------------------------------

  # Demographics data upload
  get_demo_data_raw <- shiny::reactive({
    shiny::req(data_upload_ui_vars$demo(), files_ext_ok(), files_header_ok())
    read.table(
      data_upload_ui_vars$demo()$datapath,
      header = TRUE,
      sep = data_upload_ui_vars$sep()
    )
  })

  # Omics data upload
  get_omics_data_raw <- shiny::reactive({
    shiny::req(data_upload_ui_vars$omics_data(), files_ext_ok(), files_header_ok())
    omics_data <- lapply(
      data_upload_ui_vars$omics_data()$datapath,
      read.table,
      header = TRUE,
      sep = data_upload_ui_vars$sep()
    )
    # Clean list names: strip extension (case-insensitive)
    names(omics_data) <- sub("\\.(csv|tsv|txt)$", "",
                             data_upload_ui_vars$omics_data()$name,
                             ignore.case = TRUE)
    omics_data
  })

  ## ---- File content check - data --------------------------------------

  # Omics file numeric entries check (except header), no NAs accepted
  omics_entries_error_msgs <- shiny::reactive({
    req(data_upload_ui_vars$demo(), data_upload_ui_vars$omics_data(), get_omics_data_raw())
    msgs <- character(0)
    omics_data <- get_omics_data_raw() # assigns the reactive’s value (function outcome) to local variable
    omics_entries_ok <- sapply(omics_data, function(dat) {
      all(sapply(dat, function(col) is.numeric(col) && all(!is.na(col))))
    })

    bad_entries <- which(!omics_entries_ok)

    # Error messages set up
    if (length(bad_entries)) {
      msgs <- c(msgs, sprintf("These omics files contain non-numeric entries: %s",
                paste(names(omics_data)[bad_entries], collapse = ", ")))
    }
    if (length(msgs)) msgs else NULL
    })


  # Metadata and Omics files have to have the same number of samples (rows)
  equal_row_numbers_error_msgs <- shiny::reactive({
    req(data_upload_ui_vars$demo(), data_upload_ui_vars$omics_data(), get_demo_data_raw(), get_omics_data_raw())
    msgs <- character(0)
    demo_data <- get_demo_data_raw()
    demo_rows <- nrow(demo_data)
    omics_data <- get_omics_data_raw()
    omics_rows <- sapply(omics_data, nrow)

    different_row_n <- omics_rows != demo_rows

    # Error messages set up
    if (any(different_row_n)) {
      msgs <- c(msgs, sprintf("These omics files do not have the same amount of rows as the metadata file: %s",
                              paste(names(omics_data)[different_row_n], collapse = ", ")))
    }
    if (length(msgs)) msgs else NULL
  })


  # Metadata file has to contain at least one categorical variable (NAs ignored)
  categoricals_error_msgs <- shiny::reactive({
     req(data_upload_ui_vars$demo(), get_demo_data_raw())
     msgs <- character(0)
     demo_data <- get_demo_data_raw()

     categoricals <- apply(demo_data, 2, function(i) {
       tab <- table(as.character(i))
       length(tab) < 9 && (length(tab) == 0 || min(tab) > 1)
     })

     # Error messages set up
     if (all(!categoricals)) {
       msgs <- c(msgs, sprintf("Metadata file does not contain any categorical variables!
                               Only variables with more than 1 and less than 9 unique values are considered categorical."))
     }
     if (length(msgs)) msgs else NULL
   })


  # TRUE only when all uploaded files pass the content checks
  file_contents_ok <- shiny::reactive({
    is.null(omics_entries_error_msgs()) &&
    is.null(equal_row_numbers_error_msgs()) &&
    is.null(categoricals_error_msgs())
  })


  ## ---- Error / validation UI after file reading ---------------------------------------------

  output$uploadErrorMsg_postread <- shiny::renderUI({
    msgs <- character(0)

    # Omics entries (numeric only)
    if (!is.null(omics_entries_error_msgs())) msgs <- c(msgs, omics_entries_error_msgs())

    # Equal row numbers
    if (!is.null(equal_row_numbers_error_msgs())) msgs <- c(msgs, equal_row_numbers_error_msgs())

    # Metadata contains categorical variables
    if (!is.null(categoricals_error_msgs())) msgs <- c(msgs, categoricals_error_msgs())


    if (length(msgs)) {
      shiny::div(
        class = "alert alert-danger",
        shiny::tags$ul(lapply(msgs, shiny::tags$li))
      )
    } else {
      NULL
    }
  })



  ## ---- Response / reference selection UIs  (guarded by file_contents_ok)--------------------------------

  # Show candidate categorical columns from demo
  output$response_var <- shiny::renderUI({
    shiny::req(get_demo_data_raw(), file_contents_ok())
    keep_cols <- apply(get_demo_data_raw(), 2, function(i) {
      # categorical-ish: < 9 unique and min cell count > 1
      i <- i[!is.na(i) & i != "NA"]
      tab <- table(as.character(i))
      length(tab) >= 2 && length(tab) < 9
    })
    shiny::selectInput(
      ns("response_var"),
      "Select response variable",
      choices = colnames(get_demo_data_raw()[, keep_cols, drop = FALSE])
    )
  })

  # Reference level picker (wait until response_var chosen)
  output$ref_var <- shiny::renderUI({
    shiny::req(get_demo_data_raw(), data_upload_ui_vars$response_var(), file_contents_ok())
    shiny::selectInput(
      ns("ref_var"),
      "Select reference level",
      choices = na.omit(unique(get_demo_data_raw()[, data_upload_ui_vars$response_var(), drop = TRUE]))
    )
  })

  # Factor response with chosen reference
  response_raw <- shiny::reactive({
    shiny::req(get_demo_data_raw(), data_upload_ui_vars$response_var(), data_upload_ui_vars$ref_var())
    shiny::req(data_upload_ui_vars$ref_var() %in% unique(get_demo_data_raw()[, data_upload_ui_vars$response_var()]))
    stats::relevel(
      factor(as.character(get_demo_data_raw()[, data_upload_ui_vars$response_var()])),
      ref = data_upload_ui_vars$ref_var()
    )
  })

  ## ---- Row removal where response is NA --------------------------------

  keep_rows <- shiny::reactive({
    shiny::req(data_upload_ui_vars$response_var())
    demo <- get_demo_data_raw()
    !is.na(demo[, data_upload_ui_vars$response_var()])
  })

  get_demo_data <- shiny::reactive({
    get_demo_data_raw()[keep_rows(), ]
  })

  get_omics_data <- shiny::reactive({
    lapply(get_omics_data_raw(), function(df) df[keep_rows(), ])
  })

  response <- shiny::reactive({
    response_raw()[keep_rows()]
  })

  ## ---- Response variable check --------------------------------

  observe ({
    print("response")
    print(response())
    print("unique response")
    print(unique(response()))
    print("table response")
    print(table(response()))
  })

  output$response_levels_check <- shiny::renderUI({
    msg <- character(0)
    response_levels <- table(response())

    if (any(response_levels < 3)) {
      msg <- "Each level of the response variable must have at least 3 samples."
      shiny::div(
        class = "alert alert-danger",
        shiny::tags$ul(lapply(msg, shiny::tags$li))
      )
    } else {
      NULL
    }
  })

  ## ---- Pathway analysis dataset chooser (robust to missing 'kegg') (only if file_contents_ok) -------
  perform_pathway_analysis <- shiny::reactive({
    shiny::req(get_omics_data(), file_contents_ok())
    shiny::req(!any(table(response()) < 3))
    # Guard if 'kegg' not available
    if (!exists("kegg", inherits = TRUE)) return(character(0))
    kegg_genes <- tryCatch(unlist(kegg), error = function(e) character(0))
    if (!length(kegg_genes)) return(character(0))

    dataset_names <- vapply(names(get_omics_data()), function(nm) {
      length(intersect(colnames(get_omics_data()[[nm]]), kegg_genes)) > 5
    }, logical(1))
    names(dataset_names)[dataset_names]
  })

  ## ---- Example datasets: download handlers -------------------------------

  # Heart failure example bundle (TSVs zipped)
  output$heart_failure <- shiny::downloadHandler(
    filename = "heartFailureDatasets_omicsBioAnalytics.zip",
    contentType = "application/zip",
    content = function(file) {
      withr::with_tempdir({
        files <- character(0)
        for (nm in names(heart_failure_data)) {
          fn <- paste0(nm, ".txt")
          utils::write.table(heart_failure_data[[nm]], file = fn, sep = "\t", row.names = FALSE)
          files <- c(files, fn)
        }
        zip::zipr(zipfile = file, files = files)
      })
    }
  )

  # COVID-19 example bundle (TSVs zipped)
  output$covid19 <- shiny::downloadHandler(
    filename = "COVID19Datasets_omicsBioAnalytics.zip",
    contentType = "application/zip",
    content = function(file) {
      withr::with_tempdir({
        files <- character(0)
        for (nm in names(covid19_data)) {
          fn <- paste0(nm, ".txt")
          utils::write.table(covid19_data[[nm]], file = fn, sep = "\t", row.names = FALSE)
          files <- c(files, fn)
        }
        zip::zipr(zipfile = file, files = files)
      })
    }
  )

  ## ---- Return reactives for downstream modules ---------------------------

  return(list(
    get_demo_data            = get_demo_data,
    response                 = response,
    response_raw             = response_raw,
    get_omics_data           = get_omics_data,
    perform_pathway_analysis = perform_pathway_analysis
  ))
}
