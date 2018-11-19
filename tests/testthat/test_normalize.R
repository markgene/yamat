context("Normalization methods")
library(yamat)
library(minfi)
library(minfiData)

skip_flag <- TRUE

test_that("normalize(): raw", {
  testthat::skip_if(skip_flag, "Skip: it takes a long time.")
  testthat::expect_error(x <- normalize(
    RGsetEx.sub,
    norm_method = "raw",
    map_to_genome = TRUE
  ),
  NA)
  testthat::expect_false(is.null(x))
})

test_that("normalize(): illumina", {
  testthat::skip_if(skip_flag, "Skip: it takes a long time.")
  testthat::expect_error(x <- normalize(
    RGsetEx.sub,
    norm_method = "illumina",
    map_to_genome = FALSE
  ),
  NA)
  testthat::expect_false(is.null(x))

})

test_that("normalize(): swan", {
  testthat::skip_if(skip_flag, "Skip: it takes a long time.")
  testthat::expect_error(x <- normalize(
    RGsetEx.sub,
    norm_method = "swan",
    map_to_genome = FALSE
  ),
  NA)
  testthat::expect_false(is.null(x))
})

test_that("normalize(): noob", {
  testthat::skip_if(skip_flag, "Skip: it takes a long time.")
  testthat::expect_error(x <- normalize(
    RGsetEx.sub,
    norm_method = "noob",
    map_to_genome = FALSE
  ),
  NA)
  testthat::expect_false(is.null(x))
})

test_that("normalize(): quantile", {
  testthat::skip_if(skip_flag, "Skip: it takes a long time.")
  testthat::expect_error(x <- normalize(
    RGsetEx.sub,
    norm_method = "quantile",
    map_to_genome = FALSE
  ),
  NA)
  testthat::expect_false(is.null(x))
})

test_that("normalize(): funnorm", {
  testthat::skip_if(skip_flag, "Skip: it takes a long time.")
  testthat::expect_error(x <- normalize(
    RGsetEx.sub,
    norm_method = "funnorm",
    map_to_genome = FALSE
  ),
  NA)
  testthat::expect_false(is.null(x))
})

test_that("normalize(): yamat", {
  # testthat::skip_if(skip_flag, "Skip: it takes a long time.")
  testthat::expect_error(x <- normalize(
    RGsetEx.sub,
    norm_method = "yamat",
    map_to_genome = FALSE
  ),
  NA)
  testthat::expect_false(is.null(x))
})

test_that("normalize(): dkfz", {
  testthat::skip_if(skip_flag, "Skip: it takes a long time.")
  testthat::expect_error(x <- normalize(
    RGsetEx.sub,
    norm_method = "dkfz",
    map_to_genome = FALSE
  ),
  NA)
  testthat::expect_false(is.null(x))
})

test_that("normalize(): methylcnv", {
  testthat::skip_if(skip_flag, "Skip: it takes a long time.")
  testthat::expect_error(x <- normalize(
    RGsetEx.sub,
    norm_method = "methylcnv"
  ),
  NA)
  testthat::expect_false(is.null(x))
})
