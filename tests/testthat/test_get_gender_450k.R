context("Test get_gender() with example 450K data")
library(yamat)
library(minfi)
library(minfiData)

skip_flag <- FALSE

get_test_gender_450k <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  df <- get_gender(RGsetEx.sub, norm_method = "raw")
  message(paste("end", Sys.time()))
  df
}

test_that("output dim", {
  testthat::skip_if(skip_flag, "Skip: it takes a while.")
  gender_450k <- get_test_gender_450k()
  testthat::expect_equal(dim(gender_450k), c(6, 16))
})

test_that("predicted gender", {
  testthat::skip_if(skip_flag, "Skip: it takes a while.")
  gender_450k <- get_test_gender_450k()
  is_eq <- gender_450k$predictedSex == c("M", "F", "M", "F", "F", "F")
  testthat::expect_true(all(is_eq == TRUE))
})
