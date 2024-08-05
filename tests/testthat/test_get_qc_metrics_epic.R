context("Test get_qc_metrics() with EPIC test dataset")
library(yamat)
library(minfi)
library(minfiDataEPIC)

skip_flag <- FALSE

get_test_qc_metrics_epic <- function(env = parent.frame()) {
  message(paste("Start", Sys.time()))
  rgset <- get_qc_metrics(RGsetEPIC)
  message(paste("end", Sys.time()))
  minfi::pData(rgset) %>%
    as.data.frame()
}

test_that("Restoration_Green_Intensity has the correct values", {
  testthat::skip_if(skip_flag, "Skip")
  qc_epic <- get_test_qc_metrics_epic()
  is_eq <- qc_epic$Restoration_Green_Intensity == c(243, 374, 430)
  testthat::expect_true(all(is_eq == TRUE))
})
