context("is_annotated()")
library(yamat)
library(minfiData)

skip_flag <- FALSE

test_that("an annotated minfi object returns TRUE", {
  testthat::skip_if(skip_flag, "Skip")
  testthat::expect_true(is_annotated(RGsetEx.sub))
})
