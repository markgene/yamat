# Quality Control in Terms of Detection P-values.


#' Summarize detection p-values by sample.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @return A \code{data.frame} of summarzing detection p-value for QC purpose.
#' @export
get_detection_p_value_qc_metrics <- function(rgset) {
  pheno_df <- minfi::pData(rgset)
  detection_p_values <- minfi::detectionP(rgset)
  probe_total <- colSums(!is.na(detection_p_values))
  probe_le_0_01 <- colSums(detection_p_values <= 0.01)
  probe_le_0_01_pct <- probe_le_0_01 / probe_total
  probe_le_0_05 <- colSums(detection_p_values <= 0.05)
  probe_le_0_05_pct <- probe_le_0_05 / probe_total
  mean_detection_p_values <- colMeans(detection_p_values)
  res <- data.frame(
    Basename = pheno_df$Basename,
    Mean_Detection_P_Value = mean_detection_p_values,
    Probe_Detection_P_Value_Less_than_or_Equal_01_Percent = probe_le_0_01_pct,
    Probe_Detection_P_Value_Less_than_or_Equal_05_Percent = probe_le_0_05_pct,
    Probe_Detection_P_Value_Less_than_or_Equal_05_Count = probe_le_0_01,
    Probe_Detection_P_Value_Less_than_or_Equal_01_Count = probe_le_0_05,
    Probe_Total = probe_total
  )
  res
}
