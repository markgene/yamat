context("Test plot_beta_value_histogram_fill_by_sample() with EPIC test dataset")
library(yamat)
library(logger)
library(minfi)
library(minfiDataEPIC)

skip_flag <- FALSE

test_that("Restoration_Green_Intensity has the correct values", {
  testthat::skip_if(skip_flag, "Skip")
  output <- "output"
  if (!dir.exists(output)) {
    logger::log_info(glue::glue("Create output directory {output}"))
    dir.create(output, recursive = TRUE)
  }
  output_file <- file.path(output, "RGsetEPIC.png")
  p <- yamat:::plot_beta_value_histogram_fill_by_sample(
    x = RGsetEPIC,
    output_file = output_file,
    height = 7,
    width = 11
  )
  testthat::expect_true(file.exists(output_file))
})
