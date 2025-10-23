library(shinytest2)

test_that("{shinytest2} recording: No_header_each", {
  app <- AppDriver$new(variant = platform_variant(), name = "No_header_each", seed = 1,
      height = 585, width = 979)
  app$set_inputs(tabs_before_analysis = "data")
  app$set_inputs(tabs_after_analysis = "data")
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = "testdata/Hf_no_header_demo.csv")
  app$set_inputs(`data_upload-response_var` = "M")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = "testdata/cells.csv")
  app$expect_screenshot()
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = "testdata/demo.csv")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = "testdata/Hf_no_header_demo.csv")
  app$expect_screenshot()
})


test_that("{shinytest2} recording: NA_val_omics", {
  app <- AppDriver$new(variant = platform_variant(), name = "NA_val_omics", height = 585,
      width = 979)
  app$set_inputs(tabs_before_analysis = "data")
  app$set_inputs(tabs_after_analysis = "data")
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = "testdata/demo.csv")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = "testdata/Hf_NAvalue_holter.csv")
  app$expect_screenshot()
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = "testdata/holter.csv")
  app$set_inputs(`data_upload-response_var` = "Sex")
  app$expect_screenshot()
})


test_that("{shinytest2} recording: Less_rows_omics", {
  app <- AppDriver$new(variant = platform_variant(), name = "Less_rows_omics", height = 585,
      width = 979)
  app$set_inputs(tabs_before_analysis = "data")
  app$set_inputs(tabs_after_analysis = "data")
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = "testdata/demo.csv")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = "testdata/Hf_less_rows_holter.csv")
  app$expect_screenshot()
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = "testdata/holter.csv")
  app$set_inputs(`data_upload-response_var` = "Sex")
  app$expect_screenshot()
})


test_that("{shinytest2} recording: No_categoricals_demo", {
  app <- AppDriver$new(variant = platform_variant(), name = "No_categoricals_demo",
      height = 585, width = 979)
  app$set_inputs(tabs_before_analysis = "data")
  app$set_inputs(tabs_after_analysis = "data")
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = "testdata/demo.csv")
  rlang::warn(paste0("``data_upload-omics_data`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-omics_data` = "testdata/holter.csv")
  app$set_inputs(`data_upload-response_var` = "Sex")
  app$expect_screenshot()
  rlang::warn(paste0("``data_upload-demo`` should be the path to the file, relative to the app's tests/testthat directory.\n",
      "Remove this warning when the file is in the correct location."))
  app$upload_file(`data_upload-demo` = "testdata/Hf_no_categoricals_demo.csv")
  app$expect_screenshot()
})
