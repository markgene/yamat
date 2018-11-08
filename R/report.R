#' Initiate report directory.
#'
#' Create the report directory and subdirectories of samples if they do not
#' exist. The names of sample directories are specified by \code{Sample_Name}
#' column of phenotype data returned by \code{\link[minfi]{pData}}.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param report_dir A character scalar of reporting directory.
#' @return A character vector of sample directories.
#' @export
init_report <- function(rgset, report_dir) {
  if (missing(rgset))
    stop("rgset is required.")
  if (missing(report_dir))
    stop("report_dir is required.")
  if (!dir.exists(report_dir))
    dir.create(report_dir, recursive = TRUE)
  sapply(seq(ncol(rgset)),
         function(i) {
           sample_dir <- file.path(report_dir, minfi::sampleNames(rgset)[i])
           if (!dir.exists(sample_dir))
             dir.create(sample_dir, recursive = TRUE)
           sample_dir
         })
}

