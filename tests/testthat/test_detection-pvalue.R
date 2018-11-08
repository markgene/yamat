context("Quality control in terms of detection p-values")
library(yamat)
library(minfi)
library(minfiData)


test_that("summary_detectionP(): dimension of returned data.frame", {
  detP <- minfi::detectionP(RGsetEx.sub)
  df <- summary_detectionP(detP)
  testthat::expect_equal(dim(df), c(6, 6))
})
