# Quality Control in Terms of Detection P-values.


#' Summarize detection p-values by sample.
#'
#' @param detP a \code{data.frame} returned by \code{\link[minfi]{detectionP}}.
#' @return A \code{data.frame} of summary statistics.
#' @export
summary_detectionP <- function(detP) {
  do.call(cbind, lapply(as.data.frame(detP), summary)) %>%
    t() %>%
    as.data.frame() -> output
  output
}
