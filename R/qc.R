#' Quality control
#'
#' Quality control are performed in terms of observed intensities of control
#' probes with expected value, detection p-values, and gender.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param report_dir A character scalar of reporting directory.
#' @param control_probe_plot_file A character scalar of the filename of control
#'   probe plot. Default to "control_probes.png".
#' @param detP_file A character scalar of the file stored the summary
#'   statistics of detection p-values by sample. Default to "detP.tab".
#' @param pheno_file A character scalar of the file stored the summary
#'   statistics of detection p-values by sample. Default to "pheno.tab".
#' @param overwrite A logical scalar. Default to FALSE.
#' @param verbose A logical scalar. Default to TRUE.
#' @return An object of \code{\link[minfi]{RGChannelSet-class}} with predicted
#'   gender information added. The additional columns are \code{predictedSex}
#'   (a character with values M and F), \code{xMed} and \code{yMed}, which are
#'   the chip-wide medians of measurements on the two sex chromosomes.
#' @export
qc <-
  function(rgset,
           report_dir,
           control_probe_plot_file = "control_probes.png",
           detP_file = "detP.tab",
           pheno_file = "pheno.tab",
           overwrite = FALSE,
           verbose = TRUE) {
    if (missing(rgset))
      stop("rgset is required.")
    if (missing(report_dir))
      stop("report_dir is required.")
    # Initialize report directory.
    init_report(rgset, report_dir)
    # Quality control: control probes.
    if (verbose) {
      message("Control probes...")
      tictoc::tic()
    }
    qc_control_probes(rgset = rgset,
                      report_dir = report_dir,
                      plot_file = control_probe_plot_file)
    if (verbose)
      tictoc::toc()
    # Quality control: detection p-values
    if (verbose) {
      message("Detection p-values...")
      tictoc::tic()
    }
    detP_file <- file.path(report_dir, detP_file)
    if (overwrite | !file.exists(detP_file)) {
      detP_summary <- minfi::detectionP(rgset) %>%
        summary_detectionP()
      rownames(detP_summary) <- minfi::sampleNames(rgset)
      write.table(
        x = detP_summary,
        file = detP_file,
        sep = "\t",
        row.names = TRUE,
        quote = FALSE
      )
    }
    if (verbose)
      tictoc::toc()
    # Quality control: gender
    if (verbose) {
      message("Gender information...")
      tictoc::tic()
    }
    minfi::pData(rgset) <- get_gender(rgset = rgset, norm_method = "raw")
    pheno_file <- file.path(report_dir, pheno_file)
    minfi::pData(rgset) %>%
      as.data.frame() %>%
      write.table(
        x = .,
        file = pheno_file,
        sep = "\t",
        row.names = TRUE,
        quote = FALSE
      )
    if (verbose)
      tictoc::toc()
    invisible(rgset)
  }


#' Quality control
#'
#' Quality control are performed in terms of observed intensities of control
#' probes with expected value, detection p-values, and gender.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param report_dir A character scalar of reporting directory.
#' @param plot_file A character scalar of the filename of control probe plot.
#'   Default to "control_probes.png".
#' @param height A numeric scalar of control probe plot height. Default to 10.
#' @param width A numeric scalar of control probe plot width. Default to 12.
#' @param overwrite A logical scalar. Default to FALSE.
#' @param verbose A logical scalar. Default to TRUE.
#' @param ... Any arguments can be passed to \code{\link[ggplot2]{ggsave}}.
#' @return A list of the return of \code{\link{plot_control_probes}} function
#'   (invisible).
#' @export
qc_control_probes <-
  function(rgset,
           report_dir,
           plot_file = "control_probes.png",
           height = 10,
           width = 12,
           overwrite = FALSE,
           verbose = TRUE,
           ...) {
    if (missing(rgset))
      stop("rgset is required.")
    if (missing(report_dir))
      stop("report_dir is required.")
    lapply(seq(ncol(rgset)),
           function(i) {
             sample_name <- minfi::sampleNames(rgset)[i]
             if (verbose)
               message("Plotting (", i, "/", ncol(rgset), "): ", sample_name)
             plot_file <-
               file.path(report_dir, sample_name, plot_file)
             if (overwrite | !file.exists(plot_file)) {
               plots <- plot_control_probes(rgset, s = i)
               ggplot2::ggsave(
                 plot_file,
                 plot = plots$one_plot,
                 height = height,
                 width = width,
                 ...
               )
               plots
             }
           }) %>%
      invisible(.)
  }
