context("set_annotation_epic_v2()")
library(yamat)
library(minfiData)

skip_flag <- FALSE

test_that("an annotated minfi object returns TRUE", {
  testthat::skip_if(skip_flag, "Skip")
  rgset <- set_annotation_epic_v2(RGsetEx.sub)
  testthat::expect_equal(
    rgset@annotation,
    c(array = "IlluminaHumanMethylationEPICv2", annotation = "20a1.hg38")
  )
})
