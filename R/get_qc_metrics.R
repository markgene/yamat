# Get QC metrics

#' Get QC metrics.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param background_offset background correction offset. Default to 3000.
#' @param verbose A logical scalar. Default to TRUE.
#' @returns An object of \code{\link[minfi]{RGChannelSet-class}} with QC metrics
#'   added to phenotype data.
#' @export
get_qc_metrics <- function(rgset,
                           background_offset = 3000,
                           verbose = TRUE) {
  if (missing(rgset))
    stop("rgset is required.")
  output <- minfi::pData(rgset) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var="InternalSampleId")
  # Quality control: control probes.
  if (verbose) {
    logger::log_info("Get control probe QC metrics")
    tictoc::tic()
  }
  ctrl_probe_qc <- get_control_probe_qc_metrics(rgset, background_offset = background_offset)
  output <- dplyr::left_join(output, ctrl_probe_qc, by = "InternalSampleId")
  if (verbose) {
    tictoc::toc()
  }
  # Quality control: detection p-values
  if (verbose) {
    logger::log_info("Get detection p-value QC metrics")
    tictoc::tic()
  }
  detection_p_value_qc <- get_detection_p_value_qc_metrics(rgset)
  output <- dplyr::left_join(output, detection_p_value_qc, by = "InternalSampleId")
  if (verbose) {
    tictoc::toc()
  }
  # Get meth and unmeth median - raw
  mset_raw <- minfi::preprocessRaw(rgset)
  meth_unmeth_median_raw <- minfi::getQC(mset_raw) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "InternalSampleId") %>%
    dplyr::rename(Meth_Median_Raw = mMed, Unmeth_Median_Raw = uMed) %>%
    dplyr::mutate(Sum_Meth_Unmeth_Medians_Raw = Meth_Median_Raw + Unmeth_Median_Raw)
  output <- dplyr::left_join(output, meth_unmeth_median_raw, by = "InternalSampleId")
  # Get meth and unmeth median - normalized by Noob
  mset_noob <- minfi::preprocessNoob(rgset,
                                     dyeCorr = TRUE,
                                     verbose = TRUE,
                                     dyeMethod = "single")
  gmset_noob <- minfi::mapToGenome(mset_noob)
  gmset_noob_flt <- minfi::dropLociWithSnps(gmset_noob)
  meth_unmeth_median_noob <- minfi::getQC(gmset_noob_flt) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "InternalSampleId") %>%
    dplyr::rename(Meth_Median_Normalized = mMed,
                  Unmeth_Median_Normalized = uMed) %>%
    dplyr::mutate(Sum_Meth_Unmeth_Medians_Normalized = Meth_Median_Normalized + Unmeth_Median_Normalized)
  output <- dplyr::left_join(output, meth_unmeth_median_noob, by = "InternalSampleId")
  # Quality control: gender
  if (verbose) {
    logger::log_info("Get gender")
    tictoc::tic()
  }
  gender_df <- get_gender(rgset = rgset, norm_method = "raw") %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "InternalSampleId") %>%
    dplyr::select(predictedSex, xMed, yMed, InternalSampleId) %>%
    dplyr::rename(
      Predicted_Gender = predictedSex,
      ChrX_Median = xMed,
      ChrY_Median = yMed
    )
  output <- dplyr::left_join(output, gender_df, by = "InternalSampleId")
  if (verbose) {
    tictoc::toc()
  }
  rownames(output) <- output$InternalSampleId
  minfi::pData(rgset) <- DataFrame(output)
  invisible(rgset)
}


#' Get Sentrix IDs.
#'
#' @param internal_sample_id internal sample ID
#' @returns a character vector of Sentrix ID
get_sentrix_id <- function(sentrix_id_position) {
  sapply(sentrix_id_position, function(x) {
    strsplit(x, split = "_")[[1]][1]
  })
}


#' Get Sentrix position.
#'
#' @param internal_sample_id internal sample ID
#' @returns a character vector of Sentrix position
get_sentrix_position <- function(sentrix_id_position) {
  sapply(sentrix_id_position, function(x) {
    strsplit(x, split = "_")[[1]][2]
  })
}
