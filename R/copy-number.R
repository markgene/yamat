#' Get copy number.
#'
#' The function returns copy number.
#'
#' @param x An object of \code{\link[minfi]{MethylSet-class}} or
#'   \code{\link[minfi]{GenomicMethylSet-class}}.
#' @param log2_transform A logical scalar. Default to FALSE.
#' @param offset A numeric scalar. It is only applicable when log2 transform
#'   is carried out. Default to 1.
#' @return A matrix.
#' @note \code{\link[minfi]{getCN}} returns the log2 value of meth + unmeth. If
#'   the sum is zero, \code{NA} is returned.
#' @export
copy_number <- function(x, log2_transform = FALSE, offset = 1) {
  cn <- minfi::getMeth(x) + minfi::getUnmeth(x)
  if (log2_transform)
    log2(cn + offset)
  else
    cn
}
