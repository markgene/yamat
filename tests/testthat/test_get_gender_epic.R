context("Test get_gender() with example EPIC data")
library(yamat)
library(minfi)
library(minfiDataEPIC)

skip_flag <- FALSE

get_test_gender_epic <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  df <- get_gender(RGsetEPIC, norm_method = "raw")
  message(paste("end", Sys.time()))
  df
}

test_that("output dim", {
  testthat::skip_if(skip_flag, "Skip: it takes a while.")
  gender_epic <- get_test_gender_epic()
  testthat::expect_equal(dim(gender_epic), c(3, 12))
})

test_that("predicted gender", {
  testthat::skip_if(skip_flag, "Skip: it takes a while.")
  gender_epic <- get_test_gender_epic()
  is_eq <- gender_epic$predictedSex == c("F", "F", "F")
  testthat::expect_true(all(is_eq == TRUE))
})
