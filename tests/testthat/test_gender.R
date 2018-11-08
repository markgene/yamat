context("Gender")
library(yamat)
library(minfi)
library(minfiData)

skip_flag <- TRUE

test_that("normalize(): raw", {
  testthat::skip_if(skip_flag, "Skip: it takes a while.")
  df <- get_gender(RGsetEx.sub, norm_method = "raw")
  testthat::expect_equal(dim(df), c(6, 16))
})
