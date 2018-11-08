#' Get gender information to \code{RGChannelSet}.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param norm_method A character scalar of method passed to \code{\link{normalize}},
#'   including raw, illumina, swan, quantile, noob, funnorm, yamat, dkfz.
#'   Default to "raw".
#' @return A \code{DataFrame} returned by \code{\link[minfi]{pData}} with
#'   additional columns \code{predictedSex} (a character with values M and F),
#'   \code{xMed} and \code{yMed}, which are the chip-wide medians of
#'   measurements on the two sex chromosomes.
#' @details We need to convert \code{RGChannelSet} object into \code{GenomicMethylSet}
#'   object to \code{\link[minfi]{getSex}}. We create the \code{MethylSet}
#'   without normalization using \code{\link[minfi]{preprocessRaw}}.
#' @export
get_gender <- function(rgset, norm_method = "raw") {
  normalize(rgset, norm_method = norm_method) %>%
    minfi::getSex() %>%
    cbind(minfi::pData(rgset), .)
}
