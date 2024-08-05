context("Test get_qc_metrics() with EPIC")
library(yamat)
library(minfiDataEPIC)

skip_flag <- FALSE

get_test_qc_metrics_epic <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  qc <- get_qc_metrics(RGsetEPIC)
  message(paste("end", Sys.time()))
  qc
}

test_that("probe x channel number", {
  testthat::skip_if(skip_flag, "Skip")
  qc_epic <- get_test_qc_metrics_epic()
  testthat::expect_equal(nrow(ctrl_probe_epic), 1270 * ncol(RGsetEPIC))
})

test_that("column number", {
  testthat::skip_if(skip_flag, "Skip")
  qc_epic <- get_test_qc_metrics_epic()
  testthat::expect_equal(ncol(ctrl_probe_epic), 10)
})
