context("Download data from public archives.")
library(yamat)

skip_flag <- TRUE

test_that("get_gse(): example", {
  testthat::skip_if(skip_flag, "Skip.")
  testthat::expect_error(rgset <- get_gse("GSE113775"), NA)
})
