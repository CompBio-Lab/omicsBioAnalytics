#' prepareData
#'
#' @export
#' @rdname prepareData
prepareData = function(omics_data, demo, response_var, reference){
  ## EDA
  pca <- omicsBioAnalytics::eda_pca(omics_data, demo, response_var, reference)

  ## Differentail expression
  response <- relevel(factor(as.character(demo[,response_var])), ref = reference)
  de_results <- lapply(omics_data, function(omic_data){
    design <- model.matrix(~response)
    omicsBioAnalytics::de_analysis(eset = omic_data, design, test)
  })

  ## biomarker analysis
  mods <- model_training(omics_data, response, alphaMin=0, alphaMax=1,
                         alphalength=5, kfolds=5, n_repeats = 2)
  return(list(response_var=response_var, demo=demo, reference=reference, omics_data=omics_data,
              pca=pca, response=response, de_results=de_results, mods=mods))
}

#' start_app
#'
#' @param ... passed to \link[shiny]{runApp}
#' @export
#' @rdname start_app
start_app <- function(app_data) {
  shinyOptions(app_data = app_data)
  appDir <- system.file("/app", package = "omicsBioAnalytics")
  shiny::runApp(appDir, launch.browser = TRUE)
}
