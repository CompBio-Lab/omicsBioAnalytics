#' UI of Differential Expression Analysis page
#' @export
#' @rdname dea
dea <- function() {
  shiny::fluidRow(shiny::uiOutput("dea"))
}

#' Dynamic UI of Differential Expression Analysis page per dataset
#'
#' @param id, character used to specify namespace,
#' see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI element
#' @export
dea_ui <- function(id, datasetName, dataset, response) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(6,
        shiny::radioButtons(ns("comparison"),
          label = "Comparison:",
          choices = paste(levels(response)[1], setdiff(levels(response), levels(response)[1]), sep = " vs. "),
          selected = paste(levels(response)[1], setdiff(levels(response), levels(response)[1]), sep = " vs. ")[1],
          inline = TRUE)
      ),
      shiny::column(4,
        shiny::radioButtons(ns("deTest"), "Test:",
          c("OLS" = "ols", "LIMMA" = "limma", "LIMMA voom" = "vlimma"),
          "limma",
          inline = TRUE)),
    #   shiny::column(
    #     2,
    #     shiny::br(),
    #
    #     # tiny CSS so "?" looks like an icon (no FA/bsicons)
    #     shiny::tags$style(
    #       HTML(sprintf("
    #   #%s {
    #     display:inline-block; width:22px; height:22px; line-height:22px;
    #     text-align:center; border-radius:50%%; border:1px solid #bbb;
    #     color:#666; cursor:pointer; user-select:none;
    #   }
    #   #%s:hover { background:#f7f7f7; color:#333; }
    # ", ns("qhelp"), ns("qhelp")))
    #     ),
    #
    #     bslib::popover(
    #       # target element (must be a real tag)
    #       shiny::tags$span(id = ns("qhelp"), "?", tabindex = "0"),
    #
    #       # popover body as UNNAMED positional arg (works across old bslib versions)
    #       shiny::div(
    #         "OLS: Ordinary Least Squares;",
    #         shiny::tags$br(),
    #         "LIMMA: OLS with eBayes variance correction factor."
    #       ),
    #
    #       title     = "Tests",
    #       placement = "right",
    #       options   = list(trigger = "click")   # keep here to avoid the literal 'click'
    #     )
    #   ),
    shiny::column(2,
                  shiny::actionButton(
                    ns("qhelp"), label = "", icon = shiny::icon("question"),
                    class = "btn btn-link", style = "color:gray; padding-left:6px;"
                  )
    ),
      shiny::column(6, shiny::verbatimTextOutput(ns("selection")), style = 'padding: 15px 10px 0px 10px;'),
      shiny::column(6, align = "center",
        shiny::sliderInput(ns("fdr"), shiny::h3("Select FDR threshold", align = "center"),
          min = 0.05, max = 0.5, value = 0.15))),
    shiny::fluidRow(align = "center",
      shiny::column(6, plotly::plotlyOutput(ns("volcanoPlot"))),
      shiny::column(6,
        shiny::div(style = "display:inline-block;vertical-align:top;",
          shiny::fluidRow(
            shiny::column(8,
              dqshiny::autocomplete_input(ns("variable_name"), "Type variable name:",
                colnames(dataset), max_options = ncol(dataset))),
            shiny::column(4,
              shiny::actionButton(ns("dePlotOps_button"), "Plot options"),
              shiny::actionButton(
                ns("plot_help"), label = "", icon = shiny::icon("question"),
                class = "btn btn-link", style = "color:gray; padding-left:6px;"
              )
            )
          )),
        plotly::plotlyOutput(ns("boxplot"))
      )),
    shiny::fluidRow(shiny::column(8, shiny::h4(shiny::textOutput(ns("statement")))),
      shiny::column(4,
        shiny::actionButton(
          ns("button"),
          "Significant variables",
          icon = shiny::icon("table")
        ),
        shiny::downloadButton(ns("topTable"), label = shiny::HTML("<span style='font-size:1em;'>Download</span>"), style = "color: #fff; background-color: #619CFF; border-color: #2e6da4")

      )),
    shiny::hr(),
    shiny::conditionalPanel(
      condition = paste0("output.performPathwayAnalysis.indexOf('", datasetName, "') != -1"),
      shiny::fluidRow(align = 'center',
        shiny::h1("Geneset Enrichment Analysis"),
        shiny::column(8, shiny::sliderInput(ns("enrichmentSlider"), "Select number of pathways:", min = 0, max = 10, value = 5, step = 2)),
        shiny::column(
          4,
          shiny::actionButton(
            ns("pathwayEnrichmentOps_button"),
            "Plot options"
          )
        ),
        shiny::column(12,
          # sliderInput(paste("enrichmentSlider", i, sep="_"), "Select number of pathways:", min=0, max=10, value=5, step=2),
          # plotly::plotlyOutput(paste("pathwayEnrichment", i, sep="_")),
          shiny::plotOutput(ns("pathwayEnrichment"),
                            hover = shiny::hoverOpts(id = ns("pathwayEnrichment_hover"),
                                                     delay = 100,
                                                     delayType = "debounce",
                                                     clip = TRUE)),
          shiny::verbatimTextOutput(ns("pathwayEnrichment_info")),
          shiny::downloadButton(ns("edgesGsetAll"), label = shiny::HTML("<span style='font-size:1em;'>Download<br/>Enriched Pathways</span>")))),
      shiny::fluidRow(align = 'center',
        shiny::column(12, shiny::h1(paste0("Drugs that reverse expression of up-regulated ", datasetName)),
          shiny::sliderInput(ns("drugEnrichmentUpSlider"), "Select number of compounds:", min = 0, max = 10, value = 5, step = 2),
          plotly::plotlyOutput(ns("drugEnrichmentUp"))),
        shiny::column(12,
          shiny::downloadButton(ns("edgesPertUp"), label = shiny::HTML("<span style='font-size:1em;'>Download compounds </span>"), style = "color: #fff; background-color: #F8766D; border-color: #2e6da4")),
        shiny::column(12, shiny::h1(paste0("Drugs that reverse expression of down-regulated ", datasetName)),
          shiny::sliderInput(ns("drugEnrichmentDownSlider"), "Select number of compounds:", min = 0, max = 10, value = 5, step = 2),
          plotly::plotlyOutput(ns("drugEnrichmentDown"))),
        shiny::column(12,
          shiny::downloadButton(ns("edgesPertDown"), label = shiny::HTML("<span style='font-size:1em;'>Download compounds </span>"), style = "color: #fff; background-color: #619CFF; border-color: #2e6da4"))
      )
    )
  )
}

#' Reactive values for dea_server module server-side processing
#'
#' @param input, output, session standard \code{shiny}
#'
#' @return list with following components
#' \describe{
#'   \item{comparison}{reactive character indicating which comparison to perform}
#' }
#' @export
dea_ui_vars <- function(input, output, session) {

  return(
    list(
      comparison = shiny::reactive({
        input$comparison}),
      deTest = shiny::reactive({
        input$deTest}),
      variable_name = shiny::reactive({
        input$variable_name}),
      fdr = shiny::reactive({
        input$fdr}),
      dePlotOps_button = shiny::reactive({
        input$dePlotOps_button}),
      dePlotOps = shiny::reactive({
        input$dePlotOps}),
      dePlotOps_hjust = shiny::reactive({
        input$dePlotOps_hjust}),
      dePlotOps_vjust = shiny::reactive({
        input$dePlotOps_vjust}),
      dePlotOps_xAngle = shiny::reactive({
        input$dePlotOps_xAngle}),
      dePlotOps_xSize = shiny::reactive({
        input$dePlotOps_xSize}),
      dePlotOps_ySize = shiny::reactive({
        input$dePlotOps_ySize}),
      button = shiny::reactive({
        input$button}),
      modal = shiny::reactive({
        input$modal}),
      enrichmentSlider = shiny::reactive({input$enrichmentSlider}),
      pathwayEnrichmentOps_button = shiny::reactive({input$pathwayEnrichmentOps_button}),
      pathwayEnrichmentOps = shiny::reactive({input$pathwayEnrichmentOps}),
      pathwayEnrichmentOps_hjust = shiny::reactive({input$pathwayEnrichmentOps_hjust}),
      pathwayEnrichmentOps_vjust = shiny::reactive({input$pathwayEnrichmentOps_vjust}),
      pathwayEnrichmentOps_xAngle = shiny::reactive({input$pathwayEnrichmentOps_xAngle}),
      pathwayEnrichmentOps_xSize = shiny::reactive({input$pathwayEnrichmentOps_xSize}),
      pathwayEnrichmentOps_ySize = shiny::reactive({input$pathwayEnrichmentOps_ySize}),
      pathwayEnrichment = shiny::reactive({input$pathwayEnrichment}),
      pathwayEnrichment_hover = shiny::reactive({input$pathwayEnrichment_hover}),
      drugEnrichmentUpSlider = shiny::reactive({input$drugEnrichmentUpSlider}),
      drugEnrichmentDownSlider = shiny::reactive({input$drugEnrichmentDownSlider})
    )
  )
}


#' DEA module server-side processings
#'
#' This module produces the DEA panel for a given dataset
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param demo data frame with metadata
#' @param dataset data frame containing omic variables
#' @param response factor containing cateogories for the response variable
#' @param eda_ui_vars list of one element ncomp(containing number of PCs)
#' @export
dea_server <- function(input, output, session, datasetName, dataset, response,
                       response_var, perform_pathway_analysis, group_colors,
                       dea_ui_vars) {
  ns <- session$ns

  plotOps <- reactiveValues(hjust = 0.5, vjust = 0.5, xAngle = 0, xSize = 7, ySize = 7)
  observeEvent(dea_ui_vars$dePlotOps_button(), {
    showModal(
      modalDialog(
        title = "Plot options",
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close"),
        shiny::sliderInput(ns("dePlotOps_hjust"), "horizontal justification:", min = 0, max = 1, value = 0.5),
        shiny::sliderInput(ns("dePlotOps_vjust"), "vertical justification:",   min = 0, max = 1, value = 0.5),
        shiny::sliderInput(ns("dePlotOps_xAngle"), "x-axis text angle:",       min = 0, max = 180, value = 0),
        shiny::sliderInput(ns("dePlotOps_xSize"),  "x-axis text size:",        min = 5, max = 20, value = 7),
        shiny::sliderInput(ns("dePlotOps_ySize"),  "y-axis text size:",        min = 5, max = 20, value = 7)
      )
    )
  })
  observeEvent(input$applyPlotOps, {
    plotOps$hjust  <- input$dePlotOps_hjust
    plotOps$vjust  <- input$dePlotOps_vjust
    plotOps$xAngle <- input$dePlotOps_xAngle
    plotOps$xSize  <- input$dePlotOps_xSize
    plotOps$ySize  <- input$dePlotOps_ySize
    removeModal()
  })
  observeEvent(input$qhelp, {
    showModal(modalDialog(
      title = "Tests",
      "OLS: Ordinary Least Squares. \n LIMMA: OLS with eBayes variance correction factor.",
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  observeEvent(input$plot_help, {
    showModal(modalDialog(
      title = "Information",
      "The red diamond depicts the mean expression in the group.",
      easyClose = TRUE, footer = modalButton("Close")
    ))
  })
  observeEvent(input$button, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Differentially expressed variables.",
        DT::dataTableOutput(ns("sig")),
        size = "l",   # large
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  observeEvent(input$pathwayEnrichmentOps_button, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Plot options",
        shiny::sliderInput(ns("pathwayEnrichmentOps_hjust"),
                           "horizontal justification:",
                           min = 0, max = 1, value = 0.5
        ),
        shiny::sliderInput(ns("pathwayEnrichmentOps_vjust"),
                           "vertical justification:",
                           min = 0, max = 1, value = 0.5
        ),
        shiny::sliderInput(ns("pathwayEnrichmentOps_xAngle"),
                           "x-axis text angle:",
                           min = 0, max = 180, value = 0
        ),
        shiny::sliderInput(ns("pathwayEnrichmentOps_xSize"),
                           "x-axis text size:",
                           min = 5, max = 20, value = 7
        ),
        shiny::sliderInput(ns("pathwayEnrichmentOps_ySize"),
                           "y-axis text size:",
                           min = 5, max = 20, value = 7
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })

  shiny::observeEvent(dea_ui_vars$fdr(), {
    shiny::observeEvent(dea_ui_vars$comparison(), {
      shiny::observeEvent(dea_ui_vars$deTest(), {
        shiny::req(!is.null(dea_ui_vars$fdr()))
        shiny::req(!is.null(dea_ui_vars$comparison()))
        shiny::req(!is.null(dea_ui_vars$deTest()))

        if (any(log2(dataset) < 0)) {
          shiny::updateRadioButtons(session,
            "deTest",
            label = "Test:",
            choices = c("OLS" = "ols", "LIMMA" = "limma"),
            selected = "limma",
            inline = TRUE
          )
        }

        selectedCoef <- which(levels(response) == sapply(strsplit(dea_ui_vars$comparison(), " vs. "), function(i){ i[[2]]}))
        shiny::req(length(selectedCoef) == 1)

        # removing NA rows
        print("response")
        print(response)
        keep <- !is.na(response)
        response <- droplevels(response[keep])
        print("response")
        print(response)
        dataset <- dataset[keep, ]

        design <- model.matrix(~response)
        print("response")
        print(response)
        print("responseLevels")
        print(levels(response))
        print("coef")
        print(selectedCoef)
        top <- omicsBioAnalytics::generateTopTable(dataset, design, coefNumber = selectedCoef, test = dea_ui_vars$deTest())

        subsetTop <- shiny::reactive({
          top = top %>%
            mutate(Significant = ifelse(adj.P.Val < dea_ui_vars$fdr(),
              paste("FDR < ", dea_ui_vars$fdr()), "Not Sig"))
        })

        output$topTable <- shiny::downloadHandler(
          filename = function() {
            paste("Significant_variables_OmicsBioAnalytics_", datasetName, "_FDR",
              dea_ui_vars$fdr(), "_", Sys.Date(), ".txt", sep="")
          },
          content = function(file) {
            write.table(subsetTop(), file, row.names = FALSE, sep="\t")
          }
        )

        # volcano plot
        output$volcanoPlot <- plotly::renderPlotly({
          options(htmlwidgets.TOJSON_ARGS = NULL) ## for canvasXpress
          plot_ly(subsetTop(), x = ~logFC, y = ~sig,
            key = ~FeatureName, color = ~Significant,
            colors = c("#F8766D", "grey"), marker = list(size = 10),
            type = "scatter", source = "volcanoPlot") %>%
            layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5, y = 1.1),
              xaxis = list(title = "log<sub>2</sub>FC"),
              yaxis = list(title = "-log<sub>10</sub>(P-value)"))
        })

        variable_name <- shiny::reactiveValues(selected = colnames(dataset)[1])
        shiny::observe({
          s <- unlist(plotly::event_data("plotly_click", source = "volcanoPlot")$key)
          if (length(s) > 0) {
            variable_name$selected <- s
          }
        })
        shiny::observeEvent(dea_ui_vars$variable_name(), {
          variable_name$selected <- dea_ui_vars$variable_name()
        })


        # feature plot
        output$boxplot <- plotly::renderPlotly({
          if (variable_name$selected %in% colnames(dataset)) {
            ggplotly(data.frame(x = response, y = dataset[, variable_name$selected]) %>%
                ggplot(aes(x = x, y = y, fill = x)) +
                geom_violin(trim = FALSE) +
                xlab(response_var) +
                ylab(variable_name$selected) +
                geom_jitter(shape = 16, position = position_jitter(0.2)) +
                ggtitle(paste(variable_name$selected, " vs. ", response_var)) +
                theme_classic() +
                theme(legend.position = "none") +
                stat_summary(fun.y=mean, colour="darkred", geom="point",
                               shape=18, size=3,show_guide = FALSE) +
                scale_fill_manual(values = group_colors[1:length(unique(response))]) +
                theme(axis.text.x = element_text(angle = dea_ui_vars$dePlotOps_xAngle(),
                  hjust = dea_ui_vars$dePlotOps_hjust(),
                  vjust = dea_ui_vars$dePlotOps_vjust(),
                  size = dea_ui_vars$dePlotOps_xSize()),
                  axis.text.y = element_text(size = dea_ui_vars$dePlotOps_ySize()))
            )
          } else {
            omicsBioAnalytics::empty_plot("Select point or type variable name.")
          }
        })

        # selected varibales
        shiny::observeEvent(dea_ui_vars$variable_name(),{
          output$selection <- shiny::renderPrint({
            # detect triggers
            if (!(variable_name$selected %in% colnames(dataset))) {
              "Click on a point on the volcano plot"
            } else {
              # cat(paste("You selected:", s$key, "\n\n"));
              # cat(paste("Fold-change = ", signif(s$x, 3), "\n P-value = ", signif(10^-s$y, 3)))
              dplyr::filter(top, FeatureName == variable_name$selected)[, c("FeatureName", "logFC", "AveExpr", "P.Value", "adj.P.Val")]
            }
          })
        })

        # statement
        output$statement <- shiny::renderText({
          paste("There are ", sum(top$adj.P.Val < dea_ui_vars$fdr()), "significant ", datasetName, ". ",
            sum(top$logFC > 0 & top$adj.P.Val < dea_ui_vars$fdr()), datasetName, " were up-regulated whereas",
            sum(top$logFC < 0 & top$adj.P.Val < dea_ui_vars$fdr()), datasetName, " were down-regulated.")
        })

        # table of significant featuers
        output$sig <-  DT::renderDataTable( dplyr::filter(subsetTop(), Significant != "Not Sig") %>%
            mutate(logFC = signif(logFC, 2),
              P.Value = signif(P.Value, 2),
              adj.P.Val = signif(adj.P.Val, 2),
              sig = signif(sig, 2)) %>%
            dplyr::select(FeatureName, logFC, P.Value, adj.P.Val))

        ## Differential pathway analysis
        if (datasetName %in% perform_pathway_analysis) {
          bg_genes <- dplyr::pull(subsetTop(), FeatureName)
          pathwaydbs <- c("Jensen_DISEASES", "KEGG_2019_Human", "WikiPathways_2019_Human")
          sigTable <- dplyr::filter(subsetTop(), Significant != "Not Sig")
          up <- sigTable$logFC[sigTable$logFC > 0 ]
          names(up) <- sigTable$FeatureName[sigTable$logFC > 0 ]
          down <- sigTable$logFC[sigTable$logFC < 0 ]
          names(down) <- sigTable$FeatureName[sigTable$logFC < 0 ]
          all <- c(up, down)
          # dbs <- listEnrichrDbs()

          # Run Pathway Analysis using EnrichR
          # if(length(all) > 1){
          #   # enrichment analysis for all genes/proteins
          #   enrichedAll <- enrichr(genes = names(all), databases = pathwaydbs, background = bg_genes)
          #   edgesGsetAll <- do.call(rbind, enrichedAll) %>%
          #     dplyr::mutate(database = rep(names(enrichedAll), sapply(enrichedAll, nrow))) %>%
          #     dplyr::filter(Adjusted.P.value < dea_ui_vars$fdr())
          #   edgesGsetAll$int <- as.numeric(sapply(strsplit(as.character(edgesGsetAll$Overlap), "/"), function(i) i[1]))
          #
          #   edgesGsetAll_list <- strsplit(edgesGsetAll$Genes, ";")
          #   names(edgesGsetAll_list) <- edgesGsetAll$Term
          #
          #   updateSliderInput(session, "enrichmentSlider", min = 1, max = length(edgesGsetAll_list), value = min(5, round(length(edgesGsetAll_list)/2, 0)))
          #
          # } else {
          #   edgesGsetAll <- data.frame(msg = "No pathways were identified")
          # }

          if (length(all) > 1) {
            # Run enrichment
            enrichedAll <- enrichr(
              genes = names(all),
              databases = pathwaydbs,
              background = bg_genes
            )

            # Combine results; keep database names even if some are empty
            edgesGsetAll <- dplyr::bind_rows(
              purrr::imap(enrichedAll, ~ dplyr::mutate(.x, database = .y))
            )

            # If no rows came back at all, bail early
            if (is.null(edgesGsetAll) || nrow(edgesGsetAll) == 0) {
              edgesGsetAll <- data.frame(msg = "No pathways were identified")
              updateSliderInput(session, "enrichmentSlider", min = 0, max = 0, value = 0)
              return(NULL)
            }

            # Filter by FDR safely; handle the case where nothing passes
            edgesGsetAll <- dplyr::filter(edgesGsetAll, Adjusted.P.value < dea_ui_vars$fdr())

            if (nrow(edgesGsetAll) == 0) {
              edgesGsetAll <- data.frame(msg = "No pathways were identified at this FDR")
              updateSliderInput(session, "enrichmentSlider", min = 0, max = 0, value = 0)
              return(NULL)
            }

            # Parse overlap like "7/150" -> int = 7; be robust to NAs or weird strings
            # edgesGsetAll$int <- purrr::map_dbl(
            #   strsplit(as.character(edgesGsetAll$Overlap %||% ""), "/"),
            #   ~ suppressWarnings(as.numeric(.x[1])) %||% NA_real_
            # )
            edgesGsetAll$int <- sapply(strsplit(edgesGsetAll$Genes, ";"), length)
            head(edgesGsetAll)
            head(edgesGsetAll$Overlap)
            # edgesGsetAll$int <- as.numeric(sapply(strsplit(as.character(edgesGsetAll$Overlap), "/"), function(i) i[1]))

            # Build the named gene-set list (may be used elsewhere in your app)
            edgesGsetAll_list <- strsplit(edgesGsetAll$Genes %||% "", ";", fixed = TRUE)
            names(edgesGsetAll_list) <- edgesGsetAll$Term

            # Update slider based on how many gene sets we have
            n_sets <- length(edgesGsetAll_list)
            updateSliderInput(
              session, "enrichmentSlider",
              min = 1, max = n_sets, value = min(5, max(1, round(n_sets / 2)))
            )

          } else {
            edgesGsetAll <- data.frame(msg = "No pathways were identified (gene list too small).")
            updateSliderInput(session, "enrichmentSlider", min = 0, max = 0, value = 0)
          }



          # plot heatmap of enriched pathway
          # output[[paste("pathwayEnrichment", i, sep="_")]] <- renderPlotly({
          #   if(nrow(edgesGsetAll) > 1 & i %in% performPathwayAnalysis()){
          #     options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
          #     ggplotly(
          #       drugHeatmap(fc = all,
          #         genesetList = edgesGsetAll_list[1:input[[paste("enrichmentSlider", i, sep="_")]]],
          #         col = c("#F8766D", "#619CFF"), datasetName = i, GeneSetName = "Pathways")
          #     )
          #   } else {
          #     omicsBioAnalytics::empty_plot(paste0("No enriched pathways at an FDR = ", input[[paste("fdr", i, sep="_")]]))
          #   }
          # })
          output$pathwayEnrichment <- renderPlot({
            if(nrow(edgesGsetAll) > 1 & datasetName %in% perform_pathway_analysis){
              options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
              print(head(edgesGsetAll))
              edgesGsetAll[1:dea_ui_vars$enrichmentSlider(), ] %>%
                mutate(Term = factor(as.character(Term), as.character(Term))) %>%
                ggplot(aes(x = Term, y = int, color = Term)) + geom_point(size = 5) +
                geom_segment(aes(xend = Term, color = Term), yend = 0, size = 1) +
                scale_y_log10() +
                ylab("Number of genes") +
                theme_classic() +
                theme(legend.position = "none") +
                scale_fill_manual(values = group_colors[1:length(unique(response))]) +
                theme(axis.text.x = element_text(angle = dea_ui_vars$pathwayEnrichmentOps_xAngle(),
                  hjust = dea_ui_vars$pathwayEnrichmentOps_hjust(),
                  vjust = dea_ui_vars$pathwayEnrichmentOps_vjust(),
                  size = dea_ui_vars$pathwayEnrichmentOps_xSize()),
                  axis.text.y = element_text(size = dea_ui_vars$pathwayEnrichmentOps_ySize()))
            } else {
              omicsBioAnalytics::empty_plot(paste0("No enriched pathways at an FDR = ", dea_ui_vars$fdr()))
            }
          })
          # output$pathwayEnrichment_info <- renderPrint({
          #   if(!is.null(dea_ui_vars$pathwayEnrichment_plot_click())){
          #     hover=dea_ui_vars$pathwayEnrichment_plot_click()
          #   }
          # })
          output$pathwayEnrichment_info <- renderPrint({
            hov <- dea_ui_vars$pathwayEnrichment_hover()
            if (is.null(hov)) return(invisible())
            # Show raw hover coords
            # print(hov)

            # Optional: identify the nearest pathway using nearPoints()
            # Works even with categorical x ("Term")
            np <- shiny::nearPoints(
              df = edgesGsetAll,
              coordinfo = hov,
              xvar = "Term",
              yvar = "int",
              threshold = 10,
              maxpoints = 1,
              addDist = TRUE
            )
            if (nrow(np)) {
              cat(paste0("\n", np[, "Term"], ":\n"))
              # print(np[, c("Term","int","Adjusted.P.value","database")])
              print(np[, "Genes"])
            }
          })

          # # Run EnrichR for drug enrichment analysis
          # if(length(up) > 1){
          #   # enrichment analysis for up-regulated genes/proteins
          #   enrichedUp <- enrichr(genes = names(up), databases = "LINCS_L1000_Chem_Pert_down", background = bg_genes)
          #   edgesPertUp <- do.call(rbind, enrichedUp) %>%
          #     dplyr::mutate(database = rep(names(enrichedUp), sapply(enrichedUp, nrow))) %>%
          #     dplyr::filter(Adjusted.P.value < dea_ui_vars$fdr())
          #   edgesPertUp_list <- strsplit(edgesPertUp$Genes, ";")
          #   names(edgesPertUp_list) <- edgesPertUp$Term
          #
          #   updateSliderInput(session, "drugEnrichmentUpSlider", min = 1, max = length(edgesPertUp_list), value = min(5, round(length(edgesPertUp_list)/2, 0)))
          #
          # } else {
          #   edgesPertUp <- data.frame(msg = "No pathways were identified")
          # }
          # if(length(down) > 1){
          #   # enrichment analysis for down-regulated genes/proteins
          #   enrichedDown <- enrichr(genes = names(down), databases = "LINCS_L1000_Chem_Pert_up", background = bg_genes)
          #   edgesPertDown <- do.call(rbind, enrichedDown) %>%
          #     dplyr::mutate(database = rep(names(enrichedDown), sapply(enrichedDown, nrow))) %>%
          #     dplyr::filter(Adjusted.P.value < dea_ui_vars$fdr())
          #
          #   edgesPertDown_list <- strsplit(edgesPertDown$Genes, ";")
          #   names(edgesPertDown_list) <- edgesPertDown$Term
          #
          #   updateSliderInput(session, "drugEnrichmentDownSlider", min = 1, max = length(edgesPertDown_list), value = min(5, round(length(edgesPertDown_list)/2, 0)))
          #
          # } else {
          #   edgesPertDown <- data.frame(msg = "No pathways were identified")
          # }
          # ---- DRUG ENRICHMENT: UP-REGULATED ----
          if (length(up) > 1) {
            enrichedUp <- enrichr(
              genes      = names(up),
              databases  = "LINCS_L1000_Chem_Pert_down",
              background = bg_genes
            )

            # Combine results and tag database, even if some pieces are empty
            edgesPertUp <- tryCatch({
              if (is.null(enrichedUp) || length(enrichedUp) == 0) {
                data.frame()
              } else {
                do.call(rbind, Map(function(df, nm) {
                  if (is.null(df) || nrow(df) == 0) return(df)
                  df$database <- nm
                  df
                }, enrichedUp, names(enrichedUp)))
              }
            }, error = function(e) data.frame())

            # If nothing returned at all
            if (nrow(edgesPertUp) == 0) {
              edgesPertUp <- data.frame(msg = "No pathways were identified")
              edgesPertUp_list <- list()
              updateSliderInput(session, "drugEnrichmentUpSlider", min = 0, max = 0, value = 0)
            } else {
              # FDR filter
              edgesPertUp <- dplyr::filter(edgesPertUp, Adjusted.P.value < dea_ui_vars$fdr())

              if (nrow(edgesPertUp) == 0) {
                edgesPertUp <- data.frame(msg = "No pathways were identified at this FDR")
                edgesPertUp_list <- list()
                updateSliderInput(session, "drugEnrichmentUpSlider", min = 0, max = 0, value = 0)
              } else {
                # Build named list for downstream use
                edgesPertUp_list <- strsplit(as.character(edgesPertUp$Genes), ";", fixed = TRUE)
                names(edgesPertUp_list) <- edgesPertUp$Term

                n_sets_up <- length(edgesPertUp_list)
                updateSliderInput(
                  session, "drugEnrichmentUpSlider",
                  min = 1, max = n_sets_up, value = min(5, max(1, round(n_sets_up / 2)))
                )
              }
            }
          } else {
            edgesPertUp <- data.frame(msg = "No pathways were identified (gene list too small).")
            edgesPertUp_list <- list()
            updateSliderInput(session, "drugEnrichmentUpSlider", min = 0, max = 0, value = 0)
          }

          # ---- DRUG ENRICHMENT: DOWN-REGULATED ----
          if (length(down) > 1) {
            enrichedDown <- enrichr(
              genes      = names(down),
              databases  = "LINCS_L1000_Chem_Pert_up",
              background = bg_genes
            )

            edgesPertDown <- tryCatch({
              if (is.null(enrichedDown) || length(enrichedDown) == 0) {
                data.frame()
              } else {
                do.call(rbind, Map(function(df, nm) {
                  if (is.null(df) || nrow(df) == 0) return(df)
                  df$database <- nm
                  df
                }, enrichedDown, names(enrichedDown)))
              }
            }, error = function(e) data.frame())

            if (nrow(edgesPertDown) == 0) {
              edgesPertDown <- data.frame(msg = "No pathways were identified")
              edgesPertDown_list <- list()
              updateSliderInput(session, "drugEnrichmentDownSlider", min = 0, max = 0, value = 0)
            } else {
              edgesPertDown <- dplyr::filter(edgesPertDown, Adjusted.P.value < dea_ui_vars$fdr())

              if (nrow(edgesPertDown) == 0) {
                edgesPertDown <- data.frame(msg = "No pathways were identified at this FDR")
                edgesPertDown_list <- list()
                updateSliderInput(session, "drugEnrichmentDownSlider", min = 0, max = 0, value = 0)
              } else {
                edgesPertDown_list <- strsplit(as.character(edgesPertDown$Genes), ";", fixed = TRUE)
                names(edgesPertDown_list) <- edgesPertDown$Term

                n_sets_down <- length(edgesPertDown_list)
                updateSliderInput(
                  session, "drugEnrichmentDownSlider",
                  min = 1, max = n_sets_down, value = min(5, max(1, round(n_sets_down / 2)))
                )
              }
            }
          } else {
            edgesPertDown <- data.frame(msg = "No pathways were identified (gene list too small).")
            edgesPertDown_list <- list()
            updateSliderInput(session, "drugEnrichmentDownSlider", min = 0, max = 0, value = 0)
          }

          # plot heatmaps for enriched drugs
          output$drugEnrichmentUp <- renderPlotly({
            if(nrow(edgesPertUp) > 1 & datasetName %in% perform_pathway_analysis){
              options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
              ggplotly(
                drugHeatmap(fc = up,
                  genesetList = edgesPertUp_list[1:dea_ui_vars$drugEnrichmentUpSlider()],
                  col = "#F8766D", datasetName = datasetName, GeneSetName = "LINCS L1000 Chemical Perturbations Down")
              )
            } else {
              omicsBioAnalytics::empty_plot(paste0("No enriched compounds at an FDR = ", dea_ui_vars$fdr()))
            }
          })

          output$drugEnrichmentDown <- renderPlotly({
            if(nrow(edgesPertDown) > 1 & datasetName %in% perform_pathway_analysis){
              options(htmlwidgets.TOJSON_ARGS = NULL) ## import in order to run canvasXpress
              ggplotly(
                drugHeatmap(fc = down,
                  genesetList = edgesPertDown_list[1:dea_ui_vars$drugEnrichmentDownSlider()],
                  col = "#619CFF", datasetName = datasetName, GeneSetName = "LINCS L1000 Chemical Perturbations Up")
              )
            } else {
              omicsBioAnalytics::empty_plot(paste0("No enriched compounds at an FDR = ", dea_ui_vars$fdr()))
            }
          })

          # Download buttons
          output$edgesGsetAll <- downloadHandler(
            filename = function() {
              paste("EnrichmentAnalysis_OmicsBioAnalytics_", datasetName, "_FDR",
                dea_ui_vars$fdr(), "_", Sys.Date(), ".txt", sep="")
            },
            content = function(file) {
              write.table(edgesGsetAll, file, row.names = FALSE, sep="\t")
            }
          )
          output$edgesPertUp <- downloadHandler(
            filename = function() {
              paste("Compounds_that_reverse_expression_of_upregulated_variables_OmicsBioAnalytics_", datasetName, "_FDR",
                dea_ui_vars$fdr(), "_", Sys.Date(), ".txt", sep="")
            },
            content = function(file) {
              write.table(edgesPertUp, file, row.names = FALSE, sep="\t")
            }
          )
          output$edgesPertDown <- downloadHandler(
            filename = function() {
              paste("Compounds_that_reverse_expression_of_downregulated_variables_OmicsBioAnalytics_", datasetName, "_FDR",
                dea_ui_vars$fdr(), "_", Sys.Date(), ".txt", sep="")
            },
            content = function(file) {
              write.table(edgesPertDown, file, row.names = FALSE, sep="\t")
            }
          )

        }

      })
    })
  })


}
