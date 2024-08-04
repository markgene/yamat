context("QC metrics on control probes.")
library(minfiDataEPIC)

skip_flag <- FALSE

test_that("get_control_probe_qc_metrics", {
  testthat::skip_if(skip_flag, "Skip")
  qc <- get_control_probe_qc_metrics(RGsetEPIC)
  testthat::expect_equal(nrow(qc), 3)
  testthat::expect_equal(ncol(qc), 60)
})
