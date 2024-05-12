
function(input, output, session) {
  ns <- session$ns

  # Do not show analysis sidemenu at startup!!
  # output$analysisRan <- reactive({
  #   returnedValue = FALSE
  #   return(returnedValue)
  # })
  # outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)

  ################################################################################
  #
  # Data Upload
  #
  ################################################################################
  # data_upload_ui_vars <- callModule(module = omicsBioAnalytics::data_upload_ui_vars, "data_upload")
  # data_upload_server_vars <- callModule(module = omicsBioAnalytics::data_upload_server,
  #   id = "data_upload",
  #   heart_failure_data = hf_datasets,
  #   covid19_data = covid19,
  #   data_upload_ui_vars = data_upload_ui_vars)

  # Run analysis
  # observeEvent(data_upload_ui_vars$run(), {
  #
  #   withProgress(message = 'Running analysis...',
  #     detail = 'This may take some time...', value = 0, {
        ################################################################################
        #
        # Metadata Analysis
        #
        ################################################################################
        # response_var <- readRDS(here::here("omics_bioanalytics_data", "response_var.rds"))
        # reference <- readRDS(here::here("omics_bioanalytics_data", "reference.rds"))
        # demo <- readRDS(here::here("omics_bioanalytics_data", "demo.rds"))
        # response <- relevel(factor(as.character(demo[,response_var])), ref = reference)
        metadata_ui_vars <- callModule(module = omicsBioAnalytics::metadata_ui_vars, "metadata")
        callModule(module = omicsBioAnalytics::metadata_server,
          id = "metadata",
          response_var = response_var,
          reference = reference,
          demo = demo,
          response = response,
          metadata_ui_vars = metadata_ui_vars)

        ################################################################################
        #
        # Exploratory Data Analysis
        #
        ################################################################################
        ### User interface
        # omics_data <- readRDS(here::here("omics_bioanalytics_data", "omics_data.rds"))
        output$eda = renderUI({
          myTabs <- lapply(names(omics_data), function(datasetName){
            tabPanel(datasetName,
              omicsBioAnalytics::eda_ui(paste0("eda", datasetName, sep = "_"))
            )
          })
          do.call(tabsetPanel, myTabs)
        })

        ### Backend
        # pca <- readRDS(here::here("omics_bioanalytics_data", "pca.rds"))
        lapply(names(omics_data),
          function(datasetName) {
            eda_ui_vars <- callModule(module = omicsBioAnalytics::eda_ui_vars, paste0("eda", datasetName, sep = "_"))
            callModule(module = omicsBioAnalytics::eda_server,
              id = paste0("eda", datasetName, sep = "_"),
              demo = demo,
              dataset = omics_data[[datasetName]],
              response = response,
              pcs = pca[[datasetName]]$pcs,
              var_exp = pca[[datasetName]]$var_exp,
              group_colors = group_colors,
              eda_ui_vars = eda_ui_vars)
          }
        )

        ################################################################################
        #
        # Differential Expression Analysis
        #
        ################################################################################
        # set flag to only run geneset enrichment analysis of datasets with gene symbols
        # output$performPathwayAnalysis <- shiny::reactive({
        #   shiny::req(omics_data)
        #   datasetNames <- sapply(names(omics_data), function(i){
        #     length(intersect(colnames(omics_data[[i]]), unlist(kegg))) > 5
        #   })
        #   names(datasetNames)[datasetNames]
        # })
        # shiny::outputOptions(output, "performPathwayAnalysis", suspendWhenHidden = FALSE)
        perform_pathway_analysis <- shiny::reactive({
          dataset_names <- sapply(names(omics_data), function(i){
            length(intersect(colnames(omics_data[[i]]), unlist(kegg))) > 5
          })
          names(dataset_names)[dataset_names]
        })
        ### User interface
        # de_results <- readRDS(here::here("omics_bioanalytics_data", "de_results.rds"))
        output$dea = renderUI({
          myTabs <- lapply(names(omics_data), function(datasetName){
            tabPanel(datasetName,
              omicsBioAnalytics::dea_ui(paste0("dea", datasetName, sep = "_"),
                datasetName = datasetName,
                dataset = omics_data[[datasetName]],
                response = response,
                tests = de_results[[datasetName]])
            )
          })
          do.call(tabsetPanel, myTabs)
        })

        ### Backend
        lapply(names(omics_data), function(datasetName) {
            dea_ui_vars <- callModule(module = omicsBioAnalytics::dea_ui_vars, paste0("dea", datasetName, sep = "_"))
            callModule(module = omicsBioAnalytics::dea_server,
              id = paste0("dea", datasetName, sep = "_"),
              datasetName = datasetName,
              dataset = omics_data[[datasetName]],
              response = response,
              response_var = response_var,
              perform_pathway_analysis = perform_pathway_analysis(),
              de_results = de_results[[datasetName]],
              group_colors = group_colors,
              dea_ui_vars = dea_ui_vars)
          })

        ################################################################################
        #
        # Biomarker Discovery Analysis
        #
        ################################################################################
        # mods <- readRDS(here::here("omics_bioanalytics_data", "mods.rds"))
        output$biomarker_discovery_analysis = renderUI({
          omicsBioAnalytics::biomarker_discovery_analysis_ui("biomarker_discovery_analysis",
            dataset_names = names(omics_data),
            response = response,
            response_var = response_var)
        })
        biomarker_discovery_analysis_ui_vars <- callModule(module = omicsBioAnalytics::biomarker_discovery_analysis_ui_vars,
          "biomarker_discovery_analysis")
        callModule(module = omicsBioAnalytics::biomarker_discovery_analysis_server,
          id = "biomarker_discovery_analysis",
          datasets = omics_data,
          response = response,
          response_var = response_var,
          mods = mods,
          group_colors = group_colors,
          biomarker_discovery_analysis_ui_vars = biomarker_discovery_analysis_ui_vars)

      # })
  #   observe({
  #     showNotification("Click on the Analysis tab to begin.",
  #       type = "message", duration = 10)
  #   })
  #
  #   # show analysis sidemenu when run analysis button is pressed
  #   output$analysisRan <- reactive({
  #     returnedValue = TRUE
  #     return(returnedValue)
  #   })
  #   outputOptions(output, "analysisRan", suspendWhenHidden = FALSE)
  # })

  ################################################################################
  #
  # Generate a report
  #
  ################################################################################
  # report_ui_vars <- callModule(module = omicsBioAnalytics::report_ui_vars, "report")
  # callModule(module = omicsBioAnalytics::report_server,
  #   id = "report",
  #   report_ui_vars = report_ui_vars)

  ################################################################################
  #
  # Voice-enabled analytics
  #
  ################################################################################
  # if (alexa_skill_exists) {
  #   observeEvent(data_upload_ui_vars$alexa(), {
  #     withProgress(message = 'Alexa is taking a look.',
  #       detail = 'This may take a while...', value = 0, {
  #
  #         demo <- isolate({data_upload_server_vars$get_demo_data()})
  #         responseColumnName <- isolate({data_upload_ui_vars$response_var()})
  #         responseRefLevel <- isolate({data_upload_ui_vars$ref_var()})
  #         response <- isolate({relevel(factor(as.character(demo[, responseColumnName])), ref = responseRefLevel)})
  #         omicsData <- isolate({data_upload_server_vars$get_omics_data()})
  #
  #         dynamodbAttr <- list()
  #         # analyze demo data and save images to S3
  #         dynamodbAttr$ds <- omicsBioAnalytics::alexa_metadata(demo,
  #           group = responseColumnName,
  #           trim = 0.5,
  #           format = "APL",
  #           user_id, s3_bucket)
  #
  #         # EDA and save images to S3
  #         dynamodbAttr$eda <- omicsBioAnalytics::alexaEda(demo,
  #           group = responseColumnName,
  #           omicsData,
  #           user_id, s3_bucket)
  #
  #         # Perform Differential Expression Analysis and save images to S3
  #         dynamodbAttr$dexp <- omicsBioAnalytics::alexaDexp(demo,
  #           group = responseColumnName,
  #           omicsData,
  #           user_id, s3_bucket)
  #
  #         # save dynamodb attributes to dynamodb_table_name (set in global.R) for user_id (set in global.R)
  #         omicsBioAnalytics::put_item(dynamodb_table_name, list(id = user_id, phoneNumber = jsonlite::toJSON(dynamodbAttr)))
  #       })
  #       output$msg <- renderText({
  #         paste0("If you have an Alexa device please say, 'Alexa, start omics bioanalytics' to begin. \n Please use the following id to access your analysis when prompted by Alexa: ", user_id)
  #       })
  #   })
  # } else {
  #   observeEvent(data_upload_ui_vars$alexa(), {
  #     output$msg <- renderText({
  #       "This functionality has been suspended due to cost considerations of S3 and DynamoDB. Please see the Overview tab for a link to the source code (github repo) and step-by-step setup instructions for the complementary Alexa Skill. Sorry for the inconvenience."
  #     })
  #   })
  # }

  # delete temp files
  # session$onSessionEnded(function() {
  #   sapply(list.files(tempdir(), full.names = TRUE), file.remove)
  # })
}
