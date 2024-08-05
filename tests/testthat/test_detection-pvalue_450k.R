context("Quality control in terms of detection p-values with 450K test data")
library(yamat)
library(minfi)
library(minfiData)

skip_flag <- FALSE

get_test_detection_p_value_qc_450k <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  detection_p_value_qc_450k <- get_detection_p_value_qc_metrics(RGsetEx.sub)
  message(paste("end", Sys.time()))
  detection_p_value_qc_450k
}

test_that("Dimension", {
  testthat::skip_if(skip_flag, "Skip")
  detection_p_value_qc_450k <- get_test_detection_p_value_qc_450k()
  testthat::expect_equal(dim(detection_p_value_qc_450k), c(6, 7))
})


test_that("Mean_Detection_P_Value has the correct value", {
  testthat::skip_if(skip_flag, "Skip")
  detection_p_value_qc_450k <- get_test_detection_p_value_qc_450k()
  expected <- c(
    3.264937e-08,
    1.950741e-01,
    1.210692e-02,
    2.050539e-01,
    2.351989e-01,
    2.218658e-01
  )
  almost_eq <- abs(detection_p_value_qc_450k$Mean_Detection_P_Value - expected) < 1e-7
  testthat::expect_true(all(almost_eq))
})
