context("Test qc_pipe() with EPIC test dataset")
library(yamat)
library(minfi)
library(minfiDataEPIC)

skip_flag <- FALSE

get_test_qc_pipe <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  rgset <- qc_pipe(RGsetEPIC, output = "output/qc_pipe/RGsetEPIC")
  rgset
}

test_that("Excel file exists", {
  testthat::skip_if(skip_flag, "Skip")
  rgset <- get_test_qc_pipe()
  # qc_epic <- as.data.frame(minfi::pData(rgset))
  testthat::expect_true(file.exists("output/qc_pipe/RGsetEPIC/qc.xlsx"))
})
