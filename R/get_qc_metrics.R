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
    tibble::rownames_to_column(var = "InternalSampleId")
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
  output <- add_extra_columns(output)
  rownames(output) <- output$InternalSampleId
  minfi::pData(rgset) <- DataFrame(output)
  invisible(rgset)
}


#' Add extra columns to QC metrics and reformat
#'
#' @param qc a \code{data.frame} of QC metrics
#' @returns a \code{data.frame} of QC metrics with extra columns and reorder.
add_extra_columns <- function(qc) {
  # Add extra columns to Metrics and Raw spreadsheets
  if (!"Sentrix_ID" %in% colnames(qc)) {
    qc$Sentrix_ID <- get_sentrix_id(qc$InternalSampleId)
  }
  if (!"Sentrix_Position" %in% colnames(qc)) {
    qc$Sentrix_Position <- get_sentrix_position(qc$InternalSampleId)
  }
  qc_raw <- qc %>%
    dplyr::select(Sentrix_ID, Sentrix_Position, everything())
  # Add extra columns to Metrics spreadsheet
  if (!"Sample_ID" %in% colnames(qc)) {
    qc$Sample_ID <- ""
  }
  if (!"Sample_Description" %in% colnames(qc)) {
    qc$Sample_Description <- ""
  }
  if (!"QC" %in% colnames(qc)) {
    qc$QC <- ifelse(
      qc$Mean_Detection_P_Value > 0.05,
      "Fail",
      ifelse(qc$Mean_Detection_P_Value > 0.01, "To be determined", "")
    )
  }
  if (!"Note" %in% colnames(qc)) {
    qc$Note <- ""
  }
  if (!"Beta_Value_Distribution" %in% colnames(qc)) {
    qc$Beta_Value_Distribution <- ""
  }
  qa <- assess_qc_metrics(qc)
  qc$Tier2_QC_Fail <- rowSums(!qa)
  qc$Tier2_QC_Total <- ncol(qa)
  qc_review <- qc %>%
    dplyr::select(Sentrix_ID,
                  Sentrix_Position,
                  Sample_ID,
                  QC,
                  Note,
                  Beta_Value_Distribution,
                  Tier2_QC_Fail,
                  Tier2_QC_Total,
                  everything())
  return(qc_review)
}


#' Assess QC metrics.
#'
#' @param qc a \code{data.frame} of QC metrics
#' @returns a \code{data.frame}. TRUE if passed, FALSE if not.
assess_qc_metrics <- function(qc) {
  qa <- data.frame(
    Restoration_Green_Background_Ratio = qc$Restoration_Green_Background_Ratio > 0,
    Bisulfite_Conversion_Type_I_Probe_Design_Green_Converted_Unconverted_Ratio = qc$Bisulfite_Conversion_Type_I_Probe_Design_Green_Converted_Unconverted_Ratio > 1,
    Bisulfite_Conversion_Type_I_Probe_Design_Green_Background_Highest_Unconverted_Ratio = qc$Bisulfite_Conversion_Type_I_Probe_Design_Green_Background_Highest_Unconverted_Ratio > 1,
    Bisulfite_Conversion_Type_I_Probe_Design_Red_Converted_Unconverted_Ratio = qc$Bisulfite_Conversion_Type_I_Probe_Design_Red_Converted_Unconverted_Ratio > 1,
    Bisulfite_Conversion_Type_I_Probe_Design_Red_Background_Highest_Unconverted_Ratio = qc$Bisulfite_Conversion_Type_I_Probe_Design_Red_Background_Highest_Unconverted_Ratio > 1,
    Bisulfite_Conversion_Type_II_Probe_Design_Converted_Unconverted_Ratio = qc$Bisulfite_Conversion_Type_II_Probe_Design_Converted_Unconverted_Ratio > 1,
    Bisulfite_Conversion_Type_II_Probe_Design_Background_Highest_Unconverted_Ratio = qc$Bisulfite_Conversion_Type_II_Probe_Design_Background_Highest_Unconverted_Ratio > 1,
    Hybridization_Green_High_Medium_Ratio = qc$Hybridization_Green_High_Medium_Ratio > 1,
    Hybridization_Green_Medium_Low_Ratio = qc$Hybridization_Green_Medium_Low_Ratio > 1,
    Extension_Green_Lowest_CG_Highest_AT_Ratio = qc$Extension_Green_Lowest_CG_Highest_AT_Ratio > 5,
    Extension_Red_Lowest_AT_Highest_CG_Ratio = qc$Extension_Red_Lowest_AT_Highest_CG_Ratio > 5,
    Target_Removal_1_Background_Control_Ratio = qc$Target_Removal_1_Background_Control_Ratio > 1,
    Target_Removal_2_Background_Control_Ratio = qc$Target_Removal_2_Background_Control_Ratio > 1,
    Staining_Green_High_Background_Ratio = qc$Staining_Green_High_Background_Ratio > 5,
    Staining_Red_High_Background_Ratio = qc$Staining_Red_High_Background_Ratio > 5,
    Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio = qc$Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio > 1,
    Specificity_Type_I_Probe_Design_Red_Perfectly_Matched_Mismatched_Ratio = qc$Specificity_Type_I_Probe_Design_Red_Perfectly_Matched_Mismatched_Ratio > 1,
    Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio = qc$Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio > 1,
    Specificity_Type_II_Probe_Design_Background_Highest_Green_Ratio = qc$Specificity_Type_II_Probe_Design_Background_Highest_Green_Ratio > 1,
    Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio = qc$Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio > 5,
    Nonpolymorphic_Red_Lowest_AT_Highest_CG_Ratio = qc$Nonpolymorphic_Red_Lowest_AT_Highest_CG_Ratio > 5,
    Sum_Meth_Unmeth_Medians_Raw = qc$Sum_Meth_Unmeth_Medians_Raw >= 21
  )
  return(qa)
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
