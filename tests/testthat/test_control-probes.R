context("Quality control in terms of control probes")
library(yamat)
library(minfiData)

skip_flag <- TRUE

test_that("control_probe_intensities()", {
  testthat::skip_if(skip_flag, "Skip")
  testthat::expect_error(df <- control_probe_intensities(RGsetEx), NA)
})

test_that("plot_control_probes()", {
  testthat::skip_if(skip_flag, "Skip")
  testthat::expect_error(plots <- plot_control_probes(RGsetEx, s = 1), NA)
})

if (file.exists("Rplots.pdf"))
  file.remove("Rplots.pdf")

