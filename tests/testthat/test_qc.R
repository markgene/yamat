context("Quality control wrapper")
library(yamat)
library(minfiData)

report_dir <- tempdir()

test_that("qc()", {
  testthat::expect_error(qc(rgset = RGsetEx, report_dir = report_dir), NA)
})
