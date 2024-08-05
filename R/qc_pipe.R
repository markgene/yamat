# QC pipeline


#' QC pipeline.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param output output directory.
#' @param qc_csv QC metrics CSV file.
#' @param qc_excel QC metrics Excel file.
#' @param beta_value_distribution_file a plot file for beta value distribution.
#' @param background_offset background correction offset. Default to 3000.
#' @param verbose A logical scalar. Default to TRUE.
#' @returns An object of \code{\link[minfi]{RGChannelSet-class}} with QC metrics
#'   added to phenotype data.
#' @export
qc_pipe <- function(rgset,
                    output,
                    qc_csv = "qc.csv",
                    qc_excel = "qc.xlsx",
                    beta_value_distribution_file = "beta_value_histogram_fill_by_sample.png",
                    background_offset = 3000,
                    verbose = TRUE) {
  if (missing(rgset))
    stop("rgset is required.")
  if (missing(output))
    stop("output is required.")
  if (!dir.exists(output)) {
    logger::log_info(glue::glue("Create output directory {output}"))
    dir.create(output, recursive = TRUE)
  }
  rgset <- get_qc_metrics(rgset)
  qc <- minfi::pData(rgset) %>%
    as.data.frame()
  readr::write_csv(qc, file = file.path(output, qc_file))
  if (verbose) {
    message("Plotting beta-value distribution")
  }
  beta_value_distribution_path <- file.path(output, beta_value_distribution_file)
  beta_value_histogram_fill_by_sample(rgset, output_file_name = beta_value_distribution_path, height = 7, width = 11)
  write_qc_metrics_excel(qc, excel_file = file.path(output, qc_excel_file))
  rgset
}
