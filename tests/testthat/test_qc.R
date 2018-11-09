context("Quality control wrapper")
library(yamat)
library(minfiData)

skip_flag <- TRUE
report_dir <- tempdir()

test_that("qc()", {
  testthat::skip_if(skip_flag, "Skip")
  testthat::expect_error(qc(rgset = RGsetEx, report_dir = report_dir), NA)
})
