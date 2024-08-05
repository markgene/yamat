context("QC metrics on control probes.")
library(minfiDataEPIC)

skip_flag <- FALSE

get_test_control_probe_qc_epic <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  qc <- get_control_probe_qc_metrics(RGsetEPIC)
  message(paste("end", Sys.time()))
  qc
}

test_that("sample number", {
  testthat::skip_if(skip_flag, "Skip")
  control_probe_qc_epic <- get_test_control_probe_qc_epic()
  testthat::expect_equal(nrow(control_probe_qc_epic), 3)
})

test_that("metric number", {
  testthat::skip_if(skip_flag, "Skip")
  control_probe_qc_epic <- get_test_control_probe_qc_epic()
  testthat::expect_equal(ncol(control_probe_qc_epic), 61)
})

test_that("restoration green intensity has the correct value", {
  testthat::skip_if(skip_flag, "Skip")
  control_probe_qc_epic <- get_test_control_probe_qc_epic()
  is_eq <- control_probe_qc_epic$Restoration_Green_Intensity == c(243, 374, 430)
  testthat::expect_true(all(is_eq == TRUE))
})
