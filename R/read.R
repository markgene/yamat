# Read methylation data

#' Is an object of minfi classes annotated?
#'
#' See minif classes in [The minfi User's Guide](https://bioconductor.org/packages/release/bioc/vignettes/minfi/inst/doc/minfi.html#2_minfi).
#'
#' @param x An object of minfi classes.
#' @returns A bool vector of length 1.
#' @export
is_annotated <- function(x) {
  if ("Unknown" %in% annotation(x)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' Set annotation for an object of minfi classes of EPIC v2 data.
#'
#' @param x An object of minfi classes.
#' @returns An object of minfi classes.
#' @export
set_annotation_epic_v2 <- function(x) {
  library(IlluminaHumanMethylationEPICv2anno.20a1.hg38)
  library(IlluminaHumanMethylationEPICv2manifest)
  x@annotation <- c(array = "IlluminaHumanMethylationEPICv2", annotation = "20a1.hg38")
  return(x)
}


#' Read methylation array with EPIC v2 support.
#'
#' It is a wrapper of \code{\link[minfi]{read.metharray.exp}}, but with EPIC v2
#' support by setting \code{annotation} attribute.
#' @param ... arguments passed onto \code{\link[minfi]{read.metharray.exp}}.
#' @returns x An object that has \code{sampleNames} method, for example,
#'   \code{\link[minfi]{RGChannelSet-class}}.
#' @export
read_metharray_exp <- function(...) {
  rgset <- minfi::read.metharray.exp(...)
  if (!is_annotated(rgset)) {
    logger::log_warn("annotation is unknown and set it to EPIC v2.")
    rgset <- set_annotation_epic_v2(rgset)
  }
  return(rgset)
}
