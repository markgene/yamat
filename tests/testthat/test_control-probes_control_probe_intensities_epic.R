context("Test control_probe_intensities() with EPIC")
library(yamat)
library(minfiDataEPIC)

skip_flag <- FALSE

get_test_ctrl_probe_epic <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  ctrl_probes <- control_probe_intensities(RGsetEPIC)
  message(paste("end", Sys.time()))
  ctrl_probes
}

test_that("probe x channel number", {
  testthat::skip_if(skip_flag, "Skip")
  ctrl_probe_epic <- get_test_ctrl_probe_epic()
  testthat::expect_equal(nrow(ctrl_probe_epic), 1270 * ncol(RGsetEPIC))
})

test_that("column number", {
  testthat::skip_if(skip_flag, "Skip")
  ctrl_probe_epic <- get_test_ctrl_probe_epic()
  testthat::expect_equal(ncol(ctrl_probe_epic), 10)
})
