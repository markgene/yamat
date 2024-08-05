context("Test control_probe_intensities() with 450K")
library(yamat)
library(minfiData)

skip_flag <- FALSE

get_test_ctrl_probe_450k <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  ctrl_probes <- control_probe_intensities(RGsetEx)
  message(paste("end", Sys.time()))
  ctrl_probes
}

test_that("probe x channel number", {
  testthat::skip_if(skip_flag, "Skip")
  ctrl_probe_450k <- get_test_ctrl_probe_450k()
  testthat::expect_equal(nrow(ctrl_probe_450k), 1696 * ncol(RGsetEx))
})

test_that("column number", {
  testthat::skip_if(skip_flag, "Skip")
  ctrl_probe_450k <- get_test_ctrl_probe_450k()
  testthat::expect_equal(ncol(ctrl_probe_450k), 10)
})
