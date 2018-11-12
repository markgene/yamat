context("Copy number")
library(yamat)
library(minfiData)

skip_flag <- TRUE

test_that("copy_number()", {
  testthat::skip_if(skip_flag, "Skip")
  df <- copy_number(MsetEx.sub)
  testthat::expect_equal(dim(df), c(600, 6))
})
