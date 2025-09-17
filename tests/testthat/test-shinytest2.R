library(testthat)
library(shinytest2)
testthat::local_edition(3)

fixture <- function(...) {
  p <- testthat::test_path("fixtures", ...)
  normalizePath(p, winslash = "/", mustWork = TRUE)
}

test_that("{shinytest2} recording: data upload", {
  skip_on_cran()
  app_dir <- system.file("app", package = "omicsBioAnalytics")
  expect_true(dir.exists(app_dir))

  app <- AppDriver$new(variant = platform_variant(),
                       app_dir = app_dir,
                       name = "data upload",
                       height = 576,
                       width = 511,
                       seed = 1,
                       shiny_args = list(display.mode = "normal"))
  app$set_inputs(sidebarCollapsed = FALSE)
  app$set_window_size(width = 1001, height = 576)
  app$set_inputs(tabs = "data")
  app$set_inputs(`data_upload-sep` = "\t")
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = fixture("demo.txt"))
  app$set_inputs(`data_upload-ref_var` = "")
  app$set_inputs(`data_upload-response_var` = "hospitalizations")
  app$set_inputs(`data_upload-ref_var` = "No")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = c(fixture("cells.txt"),
                                                       fixture("holter.txt"),
                                                               fixture("proteins.txt")))
  app$click("data_upload-run")
  app$expect_screenshot()
})

test_that("{shinytest2} recording: metadata", {
  skip_on_cran()
  app_dir <- system.file("app", package = "omicsBioAnalytics")
  expect_true(dir.exists(app_dir))

  app <- AppDriver$new(variant = platform_variant(),
                       app_dir = app_dir,
                       name = "metadata",
                       height = 576,
                       width = 511,
                       seed = 2,
                       shiny_args = list(display.mode = "normal"))
  app$set_inputs(sidebarCollapsed = FALSE)
  app$set_window_size(width = 939, height = 576)
  app$set_inputs(tabs_before_analysis = "data")
  app$set_inputs(tabs_after_analysis = "data")
  app$set_inputs(`data_upload-sep` = "\t")
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = fixture("demo.txt"))
  app$set_inputs(`data_upload-ref_var` = "")
  app$set_inputs(`data_upload-response_var` = "hospitalizations")
  app$set_inputs(`data_upload-ref_var` = "No")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = c(fixture("cells.txt"),
                                                       fixture("holter.txt"),
                                                               fixture("proteins.txt")))
  app$click("data_upload-run")
  app$set_inputs(sidebarItemExpanded = "Analysis")
  app$set_inputs(tabs_before_analysis = "subitem1")
  app$set_inputs(tabs_after_analysis = "subitem1")
  app$set_inputs(`metadata-hypothesisTests` = FALSE)
  app$click("metadata-lr")
  app$set_inputs(`metadata-cont_var` = "Systolic_Blood_Pressure")
  app$set_inputs(`metadata-cont_var` = "Ejection_Fraction")
  app$set_inputs(`metadata-transform` = "yes")
  app$set_inputs(`metadata-test` = "ks")
  app$set_inputs(`metadata-cat_var` = "Digoxin")
  app$set_inputs(`metadata-cat_var` = "ACEI.ARB")
  app$expect_values()
  app$expect_screenshot()
})

test_that("{shinytest2} recording: exploratory data analysis", {
  skip_on_cran()
  app_dir <- system.file("app", package = "omicsBioAnalytics")
  expect_true(dir.exists(app_dir))

  app <- AppDriver$new(variant = platform_variant(),
                       app_dir = app_dir,
                       name = "exploratory data analysis",
                       height = 576,
                       width = 511,
                       seed = 3,
                       shiny_args = list(display.mode = "normal"))
  app$set_inputs(sidebarCollapsed = FALSE)
  app$set_window_size(width = 824, height = 576)
  app$set_inputs(tabs_before_analysis = "data")
  app$set_inputs(tabs_after_analysis = "data")
  app$set_inputs(`data_upload-sep` = "\t")
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = fixture("demo.txt"))
  app$set_inputs(`data_upload-ref_var` = "")
  app$set_inputs(`data_upload-response_var` = "hospitalizations")
  app$set_inputs(`data_upload-ref_var` = "No")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = c(fixture("cells.txt"),
                                               fixture("holter.txt"),
                                               fixture("proteins.txt")))
  app$click("data_upload-run")
  app$set_inputs(sidebarItemExpanded = "Analysis")
  app$set_inputs(tabs_before_analysis = "subitem2")
  app$set_inputs(tabs_after_analysis = "subitem2")
  app$set_inputs(`edacells_-ncomp` = 2)
  app$set_inputs(`edaholter_-ncomp` = 2)
  app$set_inputs(`edaproteins_-ncomp` = 2)
  app$set_inputs(`edacells_-ncomp` = 3)
  app$set_inputs(`edaholter_-ncomp` = 5)
  app$expect_values()
  app$expect_screenshot()
})

test_that("{shinytest2} recording: differential expression analysis", {
  skip_on_cran()
  app_dir <- system.file("app", package = "omicsBioAnalytics")
  expect_true(dir.exists(app_dir))

  app <- AppDriver$new(variant = platform_variant(),
                       app_dir = app_dir,
                       name = "differential expression analysis",
                       height = 576,
                       width = 511,
                       seed = 4,
                       shiny_args = list(display.mode = "normal"))
  app$set_inputs(sidebarCollapsed = FALSE)
  app$set_window_size(width = 1325, height = 576)
  app$set_inputs(tabs_before_analysis = "data")
  app$set_inputs(tabs_after_analysis = "data")
  app$set_inputs(`data_upload-sep` = "\t")
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = fixture("demo.txt"))
  app$set_inputs(`data_upload-ref_var` = "")
  app$set_inputs(`data_upload-response_var` = "hospitalizations")
  app$set_inputs(`data_upload-ref_var` = "No")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = c(fixture("cells.txt"),
                                                       fixture("holter.txt"),
                                                               fixture("proteins.txt")))
  app$click("data_upload-run")
  app$set_inputs(sidebarItemExpanded = "Analysis")
  app$set_inputs(tabs_before_analysis = "subitem3")
  app$set_inputs(tabs_after_analysis = "subitem3")
  app$wait_for_idle()
  app$set_inputs(`deacells_-variable_name` = "")
  app$set_inputs(`deaholter_-variable_name` = "")
  app$set_inputs(`deaproteins_-variable_name` = "")
  app$set_inputs(`deacells_-dePlotOps` = FALSE)
  app$set_inputs(`deacells_-modal` = FALSE)
  app$set_inputs(`deacells_-pathwayEnrichmentOps` = FALSE)
  app$set_inputs(`deaholter_-dePlotOps` = FALSE)
  app$set_inputs(`deaholter_-modal` = FALSE)
  app$set_inputs(`deaholter_-pathwayEnrichmentOps` = FALSE)
  app$set_inputs(`deaproteins_-dePlotOps` = FALSE)
  app$set_inputs(`deaproteins_-modal` = FALSE)
  app$set_inputs(`deaproteins_-pathwayEnrichmentOps` = FALSE)
  app$click("deacells_-search_button")
  app$click("deacells_-dePlotOps_button")
  app$click("deacells_-plot_help")
  app$click("deacells_-button")
  app$click("deacells_-pathwayEnrichmentOps_button")
  app$click("deaholter_-search_button")
  app$click("deaholter_-dePlotOps_button")
  app$click("deaholter_-plot_help")
  app$click("deaholter_-button")
  app$click("deaholter_-pathwayEnrichmentOps_button")
  app$click("deaproteins_-search_button")
  app$click("deaproteins_-dePlotOps_button")
  app$click("deaproteins_-plot_help")
  app$click("deaproteins_-button")
  app$click("deaproteins_-pathwayEnrichmentOps_button")
  app$set_inputs(`deacells_-fdr` = 0.15)
  app$set_inputs(`deacells_-dePlotOps_hjust` = 0.5)
  app$set_inputs(`deacells_-dePlotOps_vjust` = 0.5)
  app$set_inputs(`deacells_-dePlotOps_xAngle` = 0)
  app$set_inputs(`deacells_-dePlotOps_xSize` = 7)
  app$set_inputs(`deacells_-dePlotOps_ySize` = 7)
  app$set_inputs(`deacells_-enrichmentSlider` = 5)
  app$set_inputs(`deacells_-pathwayEnrichmentOps_hjust` = 0.5)
  app$set_inputs(`deacells_-pathwayEnrichmentOps_vjust` = 0.5)
  app$set_inputs(`deacells_-pathwayEnrichmentOps_xAngle` = 0)
  app$set_inputs(`deacells_-pathwayEnrichmentOps_xSize` = 7)
  app$set_inputs(`deacells_-pathwayEnrichmentOps_ySize` = 7)
  app$set_inputs(`deacells_-drugEnrichmentUpSlider` = 5)
  app$set_inputs(`deacells_-drugEnrichmentDownSlider` = 5)
  app$set_inputs(`deaholter_-fdr` = 0.15)
  app$set_inputs(`deaholter_-dePlotOps_hjust` = 0.5)
  app$set_inputs(`deaholter_-dePlotOps_vjust` = 0.5)
  app$set_inputs(`deaholter_-dePlotOps_xAngle` = 0)
  app$set_inputs(`deaholter_-dePlotOps_xSize` = 7)
  app$set_inputs(`deaholter_-dePlotOps_ySize` = 7)
  app$set_inputs(`deaholter_-enrichmentSlider` = 5)
  app$set_inputs(`deaholter_-pathwayEnrichmentOps_hjust` = 0.5)
  app$set_inputs(`deaholter_-pathwayEnrichmentOps_vjust` = 0.5)
  app$set_inputs(`deaholter_-pathwayEnrichmentOps_xAngle` = 0)
  app$set_inputs(`deaholter_-pathwayEnrichmentOps_xSize` = 7)
  app$set_inputs(`deaholter_-pathwayEnrichmentOps_ySize` = 7)
  app$set_inputs(`deaholter_-drugEnrichmentUpSlider` = 5)
  app$set_inputs(`deaholter_-drugEnrichmentDownSlider` = 5)
  app$set_inputs(`deaproteins_-fdr` = 0.15)
  app$set_inputs(`deaproteins_-dePlotOps_hjust` = 0.5)
  app$set_inputs(`deaproteins_-dePlotOps_vjust` = 0.5)
  app$set_inputs(`deaproteins_-dePlotOps_xAngle` = 0)
  app$set_inputs(`deaproteins_-dePlotOps_xSize` = 7)
  app$set_inputs(`deaproteins_-dePlotOps_ySize` = 7)
  app$set_inputs(`deaproteins_-enrichmentSlider` = 5)
  app$set_inputs(`deaproteins_-pathwayEnrichmentOps_hjust` = 0.5)
  app$set_inputs(`deaproteins_-pathwayEnrichmentOps_vjust` = 0.5)
  app$set_inputs(`deaproteins_-pathwayEnrichmentOps_xAngle` = 0)
  app$set_inputs(`deaproteins_-pathwayEnrichmentOps_xSize` = 7)
  app$set_inputs(`deaproteins_-pathwayEnrichmentOps_ySize` = 7)
  app$set_inputs(`deaproteins_-drugEnrichmentUpSlider` = 5)
  app$set_inputs(`deaproteins_-drugEnrichmentDownSlider` = 5)
  app$set_inputs(`deacells_-comparison` = "No vs. Yes")
  app$set_inputs(`deacells_-deTest` = "limma")
  app$set_inputs(`deaholter_-comparison` = "No vs. Yes")
  app$set_inputs(`deaholter_-deTest` = "limma")
  app$set_inputs(`deaproteins_-comparison` = "No vs. Yes")
  app$set_inputs(`deaproteins_-deTest` = "limma")
  app$click("deacells_-button")
  app$click("deaholter_-dePlotOps_button")
  app$expect_values()
  app$expect_screenshot()
})

test_that("{shinytest2} recording: biomarker analysis", {
  skip_on_cran()
  app_dir <- system.file("app", package = "omicsBioAnalytics")
  expect_true(dir.exists(app_dir))

  app <- AppDriver$new(variant = platform_variant(),
                       app_dir = app_dir,
                       name = "biomarker analysis",
                       height = 576,
                       width = 511,
                       seed = 4,
                       shiny_args = list(display.mode = "normal"))
  app$set_inputs(sidebarCollapsed = FALSE)
  app$set_inputs(tabs_before_analysis = "data")
  app$set_inputs(tabs_after_analysis = "data")
  app$set_inputs(`data_upload-sep` = "\t")
  app$set_inputs(sidebarCollapsed = TRUE)
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = fixture("demo.txt"))
  app$set_inputs(`data_upload-ref_var` = "")
  app$set_inputs(`data_upload-response_var` = "hospitalizations")
  app$set_inputs(`data_upload-ref_var` = "No")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
                     "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = c(fixture("cells.txt"),
                                                       fixture("holter.txt"),
                                                               fixture("proteins.txt")))
  app$set_inputs(sidebarCollapsed = FALSE)
  app$set_window_size(width = 1053, height = 576)
  app$click("data_upload-run")
  app$set_inputs(sidebarItemExpanded = "Analysis")
  app$set_inputs(tabs_before_analysis = "subitem4")
  app$set_inputs(tabs_after_analysis = "subitem4")
  app$set_inputs(`biomarker_discovery_analysis-biomarker_plot-variable_options` = FALSE)
  # Set the required inputs first
  app$set_inputs(`biomarker_discovery_analysis-selectedGroups` = c("No", "Yes"))  # Adjust group names
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_single` = c("cells", "holter", "proteins"))
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_ensemble` = c("cells", "holter", "proteins"))
  # Wait for reactive updates
  app$wait_for_idle()
  # Now click the build button
  app$click("biomarker_discovery_analysis-build")
  app$click("biomarker_discovery_analysis-biomarker_plot-variable_options_button")
  app$set_inputs(`biomarker_discovery_analysis-alpha` = c(0.7, 1))
  app$set_inputs(`biomarker_discovery_analysis-biomarker_plot-hjust` = 0.5)
  app$set_inputs(`biomarker_discovery_analysis-biomarker_plot-vjust` = 0.5)
  app$set_inputs(`biomarker_discovery_analysis-biomarker_plot-xAngle` = 0)
  app$set_inputs(`biomarker_discovery_analysis-biomarker_plot-xSize` = 7)
  app$set_inputs(`biomarker_discovery_analysis-biomarker_plot-ySize` = 7)
  app$set_inputs(`biomarker_discovery_analysis-corCutoff` = 0.5)
  app$set_inputs(`biomarker_discovery_analysis-bioFDR` = 0.05)
  app$set_inputs(`biomarker_discovery_analysis-cvScheme` = "fiveFold")
  app$set_inputs(`biomarker_discovery_analysis-pcaBasePanelRadioButtons` = " ")
  app$set_inputs(`biomarker_discovery_analysis-heatmapBasePanelRadioButtons` = " ")
  app$set_inputs(`biomarker_discovery_analysis-selectedGroups` = c("No", "Yes"))
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_single` = character(0))
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_ensemble` = character(0))
  app$set_inputs(`biomarker_discovery_analysis-alphaGrid` = 5)
  app$set_inputs(`biomarker_discovery_analysis-n_repeats` = 5)
  app$set_inputs(sidebarCollapsed = TRUE)
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_single` = "cells")
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_single` = c("cells", "holter"))
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_single` = c("cells", "holter",
                                                                      "proteins"))
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_ensemble` = "proteins")
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_ensemble` = c("holter",
                                                                        "proteins"))
  app$set_inputs(`biomarker_discovery_analysis-checkGroup_ensemble` = c("cells",
                                                                        "holter", "proteins"))
  app$set_inputs(`biomarker_discovery_analysis-alphaGrid` = 2)
  app$set_inputs(`biomarker_discovery_analysis-n_repeats` = 2)
  app$click("biomarker_discovery_analysis-build")
  app$set_inputs(`biomarker_discovery_analysis-pcaBasePanelRadioButtons` = "holter")
  app$set_inputs(`biomarker_discovery_analysis-pcaBasePanelRadioButtons` = "proteins")
  app$set_inputs(`biomarker_discovery_analysis-heatmapBasePanelRadioButtons` = "holter")
  app$set_inputs(`biomarker_discovery_analysis-heatmapBasePanelRadioButtons` = "proteins")
  app$expect_values()
  app$expect_screenshot()
})
