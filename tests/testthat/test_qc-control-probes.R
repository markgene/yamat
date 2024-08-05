context("QC metrics on control probes.")
library(minfiDataEPIC)

skip_flag <- FALSE

get_test_qc <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  qc <- get_control_probe_qc_metrics(RGsetEPIC)
  message(paste("end", Sys.time()))
  qc
}

test_that("sample number", {
  testthat::skip_if(skip_flag, "Skip")
  qc <- get_test_qc()
  testthat::expect_equal(nrow(qc), 3)
})

test_that("metric number", {
  testthat::skip_if(skip_flag, "Skip")
  qc <- get_control_probe_qc_metrics(RGsetEPIC)
  testthat::expect_equal(ncol(qc), 60)
})
