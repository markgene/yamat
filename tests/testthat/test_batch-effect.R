context("Batch effect")
library(yamat)
library(minfiData)

skip_flag <- TRUE

test_that("remove_batch_effect(): methylation and unmethylation signals", {
  testthat::skip_if(skip_flag, "Skip")
  testthat::expect_error(x <-
                           remove_batch_effect(
                             x = MsetEx.sub,
                             batch = "status",
                             method = "mum",
                             offset = 1
                           ),
                         NA)
  y <- minfi::getMeth(x) - minfi::getMeth(MsetEx.sub)
  testthat::expect_true(max(y) - min(y) > 100)
})

test_that("remove_batch_effect(): copy number", {
  testthat::skip_if(skip_flag, "Skip")
  testthat::expect_error(x <-
                           remove_batch_effect(
                             x = minfi::ratioConvert(MsetEx.sub),
                             batch = "status",
                             method = "cn"
                           ),
                         NA)
  y <- minfi::getCN(x) - minfi::getCN(MsetEx.sub)
  testthat::expect_true(max(y) - min(y) > 0.1)
})

test_that("remove_batch_effect(): beta value", {
  testthat::skip_if(skip_flag, "Skip")
  testthat::expect_error(x <-
                           remove_batch_effect(
                             x = minfi::ratioConvert(MsetEx.sub),
                             batch = "status",
                             method = "beta"
                           ),
                         NA)
  y <- minfi::getBeta(x) - minfi::getBeta(MsetEx.sub)
  testthat::expect_true(max(y) - min(y) > 0.1)
})
