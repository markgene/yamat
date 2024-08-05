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
  testthat::expect_equal(dim(detection_p_value_qc), c(6, 6))
})


test_that("Dimension", {
  testthat::skip_if(skip_flag, "Skip")
  detection_p_value_qc_450k <- get_test_detection_p_value_qc_450k()
  testthat::expect_equal(detection_p_value_qc$Mean_Detection_P_Value)
})
testthat::expect_equal(
  detection_p_value_qc$Mean_Detection_P_Value[1],
  c(
    3.264937e-08,
    1.950741e-01,
    1.210692e-02,
    2.050539e-01,
    2.351989e-01,
    2.218658e-01
  )
)
