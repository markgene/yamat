# Read methylation data

#' Is an object of minfi classes annotated?
#'
#' See minif classes in [The minfi User's Guide](https://bioconductor.org/packages/release/bioc/vignettes/minfi/inst/doc/minfi.html#2_minfi).
#'
#' @param x An object of minfi classes.
#' @return A bool vector of length 1.
#' @export
is_annotated <- function(x) {
  if ("Unknown" %in% annotation(x)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
