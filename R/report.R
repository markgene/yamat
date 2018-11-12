#' Initiate report directory.
#'
#' Create the report directory and subdirectories of samples if they do not
#' exist. The names of sample directories are specified by \code{Sample_Name}
#' column of phenotype data returned by \code{\link[minfi]{pData}}.
#'
#' @param x An object that has \code{sampleNames} method, for example,
#'   \code{\link[minfi]{RGChannelSet-class}}.
#' @param report_dir A character scalar of reporting directory.
#' @return A character vector of sample directories.
#' @export
init_report <- function(x, report_dir) {
  if (missing(x))
    stop("x is required.")
  if (missing(report_dir))
    stop("report_dir is required.")
  if (!dir.exists(report_dir))
    dir.create(report_dir, recursive = TRUE)
  output <- sapply(seq(ncol(x)),
                   function(i) {
                     sample_dir <- file.path(report_dir, minfi::sampleNames(x)[i])
                     if (!dir.exists(sample_dir))
                       dir.create(sample_dir, recursive = TRUE)
                     sample_dir
                   })
  names(output) <- minfi::sampleNames(x)
  output
}

