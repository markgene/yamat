# Write QC metrics


#' Write QC metrics into Excel file.
#'
#' @param qc a \code{data.frame} of QC metrics added to the phenotype data of
#'   the returned \code{\link[minfi]{RGChannelSet-class}} object of
#'   \code{\link{get_qc_metrics}}.
#' @param excel_file output Excel file.
#' @param beta_value_distribution_file the plot file for beta value distribution.
#' @returns a Workbook object.
#' @export
write_qc_metrics_excel <- function(qc,
                                   excel_file,
                                   beta_value_distribution_file = NULL) {
  # Create a new workbook and add a worksheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Metrics")
  openxlsx::addWorksheet(wb, "Raw")
  # Write raw
  openxlsx::writeData(wb, "Raw", qc)
  # Metrics
  metric_column_names <- c(
    "Sentrix_ID",
    "Sentrix_Position",
    "Sample_ID",
    "Sample_Description",
    "QC",
    "Note",
    "Mean_Detection_P_Value",
    "Probe_Detection_P_Value_Less_than_or_Equal_01_Percent",
    "Probe_Detection_P_Value_Less_than_or_Equal_05_Percent",
    "Beta_Value_Distribution",
    "Tier2_QC_Fail",
    "Bisulfite_Conversion_Type_I_Probe_Design_Green_Converted_Unconverted_Ratio",
    "Bisulfite_Conversion_Type_I_Probe_Design_Green_Background_Highest_Unconverted_Ratio",
    "Bisulfite_Conversion_Type_I_Probe_Design_Red_Converted_Unconverted_Ratio",
    "Bisulfite_Conversion_Type_I_Probe_Design_Red_Background_Highest_Unconverted_Ratio",
    "Bisulfite_Conversion_Type_II_Probe_Design_Converted_Unconverted_Ratio",
    "Bisulfite_Conversion_Type_II_Probe_Design_Background_Highest_Unconverted_Ratio",
    "Hybridization_Green_High_Medium_Ratio",
    "Hybridization_Green_Medium_Low_Ratio",
    "Extension_Green_Lowest_CG_Highest_AT_Ratio",
    "Extension_Red_Lowest_AT_Highest_CG_Ratio",
    "Target_Removal_1_Background_Control_Ratio",
    "Target_Removal_2_Background_Control_Ratio",
    "Staining_Green_High_Background_Ratio",
    "Staining_Red_High_Background_Ratio",
    "Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio",
    "Specificity_Type_I_Probe_Design_Red_Perfectly_Matched_Mismatched_Ratio",
    "Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio",
    "Specificity_Type_II_Probe_Design_Background_Highest_Green_Ratio",
    "Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio",
    "Nonpolymorphic_Red_Lowest_AT_Highest_CG_Ratio",
    "Restoration_Green_Background_Ratio",
    "Sum_Meth_Unmeth_Medians_Normalized",
    "Sum_Meth_Unmeth_Medians_Raw",
    "Predicted_Gender"
  )
  # Write data to the worksheet
  metrics <- qc[, metric_column_names]
  openxlsx::writeData(wb, "Metrics", metrics)

  # Apply conditional formatting
  # Green for scores greater than 90
  sample_row_index <- 2:(nrow(qc) + 1)
  fail_style <- openxlsx::createStyle(fontColour = "#e3f9fd", bgFill = "#fc3939")
  warning_style <- openxlsx::createStyle(fontColour = "#e3f9fd", bgFill = "#efa31d")
  QC_index <- which(colnames(metrics) == "QC")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = QC_index,
    rows = sample_row_index,
    rule = '!=""',
    style = warning_style
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = QC_index,
    rows = sample_row_index,
    rule = '=="Fail"',
    style = fail_style
  )
  Beta_Value_Distribution_index <- which(colnames(metrics) == "Beta_Value_Distribution")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Beta_Value_Distribution_index,
    rows = sample_row_index,
    rule = '!=""',
    style = warning_style
  )
  Tier2_QC_Fail_index <- which(colnames(metrics) == "Tier2_QC_Fail")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Tier2_QC_Fail_index,
    rows = sample_row_index,
    rule = '>0',
    style = warning_style
  )
  mean_detection_p_value_col_index <- which(colnames(metrics) == "Mean_Detection_P_Value")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = mean_detection_p_value_col_index,
    rows = sample_row_index,
    rule = '>0.01',
    style = warning_style
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = mean_detection_p_value_col_index,
    rows = sample_row_index,
    rule = ">0.05",
    style = fail_style
  )
  restoration_green_background_ratio_index <- which(colnames(metrics) == "Restoration_Green_Background_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = restoration_green_background_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = restoration_green_background_ratio_index,
    rows = sample_row_index,
    rule = "<=0",
    style = fail_style
  )
  bisulfite_conversion_type_i_probe_design_green_converted_unconverted_ratio_index <- which(
    colnames(metrics) == "Bisulfite_Conversion_Type_I_Probe_Design_Green_Converted_Unconverted_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = bisulfite_conversion_type_i_probe_design_green_converted_unconverted_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  bisulfite_conversion_type_i_probe_design_green_background_highest_unconverted_ratio_index <- which(
    colnames(metrics) == "Bisulfite_Conversion_Type_I_Probe_Design_Green_Background_Highest_Unconverted_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = bisulfite_conversion_type_i_probe_design_green_background_highest_unconverted_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  bisulfite_conversion_type_i_probe_design_red_converted_unconverted_ratio_index <- which(
    colnames(metrics) == "Bisulfite_Conversion_Type_I_Probe_Design_Red_Converted_Unconverted_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = bisulfite_conversion_type_i_probe_design_red_converted_unconverted_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  bisulfite_conversion_type_i_probe_design_red_background_highest_unconverted_ratio_index <- which(
    colnames(metrics) == "Bisulfite_Conversion_Type_I_Probe_Design_Red_Background_Highest_Unconverted_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = bisulfite_conversion_type_i_probe_design_red_background_highest_unconverted_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  bisulfite_conversion_type_ii_probe_design_converted_unconverted_ratio_index <- which(
    colnames(metrics) == "Bisulfite_Conversion_Type_II_Probe_Design_Converted_Unconverted_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = bisulfite_conversion_type_ii_probe_design_converted_unconverted_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  bisulfite_conversion_type_ii_probe_design_background_highest_unconverted_ratio_index <- which(
    colnames(metrics) == "Bisulfite_Conversion_Type_II_Probe_Design_Background_Highest_Unconverted_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = bisulfite_conversion_type_ii_probe_design_background_highest_unconverted_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  hybridization_green_high_medium_ratio_index <- which(colnames(metrics) == "Hybridization_Green_High_Medium_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = hybridization_green_high_medium_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  hybridization_green_medium_low_ratio_index <- which(colnames(metrics) == "Hybridization_Green_Medium_Low_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = hybridization_green_medium_low_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  extension_green_lowest_cg_highest_at_ratio_index <- which(colnames(metrics) == "Extension_Green_Lowest_CG_Highest_AT_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = extension_green_lowest_cg_highest_at_ratio_index,
    rows = sample_row_index,
    rule = "<=5",
    style = warning_style
  )
  extension_red_lowest_at_highest_cg_ratio_index <- which(colnames(metrics) == "Extension_Red_Lowest_AT_Highest_CG_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = extension_red_lowest_at_highest_cg_ratio_index,
    rows = sample_row_index,
    rule = "<=5",
    style = warning_style
  )
  target_removal_1_background_control_ratio_index <- which(colnames(metrics) == "Target_Removal_1_Background_Control_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = target_removal_1_background_control_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  target_removal_2_background_control_ratio_index <- which(colnames(metrics) == "Target_Removal_2_Background_Control_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = target_removal_2_background_control_ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  Staining_Green_High_Background_Ratio_index <- which(colnames(metrics) == "Staining_Green_High_Background_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Staining_Green_High_Background_Ratio_index,
    rows = sample_row_index,
    rule = "<=5",
    style = warning_style
  )
  Staining_Red_High_Background_Ratio_index <- which(colnames(metrics) == "Staining_Red_High_Background_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Staining_Red_High_Background_Ratio_index,
    rows = sample_row_index,
    rule = "<=5",
    style = warning_style
  )
  Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio_index <- which(
    colnames(metrics) == "Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  Specificity_Type_I_Probe_Design_Red_Perfectly_Matched_Mismatched_Ratio_index <- which(
    colnames(metrics) == "Specificity_Type_I_Probe_Design_Red_Perfectly_Matched_Mismatched_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Specificity_Type_I_Probe_Design_Red_Perfectly_Matched_Mismatched_Ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio_index <- which(
    colnames(metrics) == "Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  Specificity_Type_II_Probe_Design_Background_Highest_Green_Ratio_index <- which(
    colnames(metrics) == "Specificity_Type_II_Probe_Design_Background_Highest_Green_Ratio"
  )
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Specificity_Type_II_Probe_Design_Background_Highest_Green_Ratio_index,
    rows = sample_row_index,
    rule = "<=1",
    style = warning_style
  )
  Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio_index <- which(colnames(metrics) == "Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio_index,
    rows = sample_row_index,
    rule = "<=5",
    style = warning_style
  )
  Nonpolymorphic_Red_Lowest_AT_Highest_CG_Ratio_index <- which(colnames(metrics) == "Nonpolymorphic_Red_Lowest_AT_Highest_CG_Ratio")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Nonpolymorphic_Red_Lowest_AT_Highest_CG_Ratio_index,
    rows = sample_row_index,
    rule = "<=5",
    style = warning_style
  )
  Sum_Meth_Unmeth_Medians_Raw_index <- which(colnames(metrics) == "Sum_Meth_Unmeth_Medians_Raw")
  openxlsx::conditionalFormatting(
    wb,
    "Metrics",
    cols = Sum_Meth_Unmeth_Medians_Raw_index,
    rows = sample_row_index,
    rule = "<21",
    style = warning_style
  )
  # Save the workbook
  # Color style description
  fail_style2 <- openxlsx::createStyle(fontColour = "#e3f9fd", fgFill = "#fc3939")
  warning_style2 <- openxlsx::createStyle(fontColour = "#e3f9fd", fgFill = "#efa31d")
  styles <- data.frame(
    Keys = c("Color Key", "Fail", "Warning", "Pass"),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb,
            "Metrics",
            styles,
            startRow = nrow(qc) + 4,
            colNames = FALSE)
  openxlsx::addStyle(
    wb,
    "Metrics",
    cols = 1,
    rows = nrow(qc) + 5,
    style = fail_style2
  )
  openxlsx::addStyle(
    wb,
    "Metrics",
    cols = 1,
    rows = nrow(qc) + 6,
    style = warning_style2
  )
  # Column description
  description <- data.frame(
    Description = c(
      "This spreadsheet contains QC metrics as described below: ",
      "Sentrix_ID: Sentrix ID of the BeadChip",
      "Sentrix_Position: sample position on the BeadChip",
      "Sample_ID: sample ID, e.g. surgical path number",
      "Sample_Description: description of the sample",
      "QC: QC flag. If fail, fill out Fail. The field need to be reviewed. If mean detection p-value > 0.05, fail will be filled.",
      "Note: extra note on the QC or anything else about the sample",
      "Mean_Detection_P_Value: mean detection p-value. If p<=0.01, pass; if 0.01 < p <= 0.05, warning; if p > 0.05, fail. This is the most important metric.",
      "Probe_Detection_P_Value_Less_than_or_Equal_01_Percent: percent of probes with detection p-value <= 0.01. Threshold to be determined.",
      "Probe_Detection_P_Value_Less_than_or_Equal_05_Percent: percent of probes with detection p-value <= 0.05. Threshold to be determined.",
      "Beta_Value_Distribution: if beta value distribution is as expected. The field need to be reviewed based on the beta value distribution plot. If it is okay, leave it empty. If not, put any text on it. A typical sample will have a bi-modal distribution with two peaks close to 0 (unmeth) and 1 (meth) respectively. Note: it depends on the sample. A bi-modal distribution is not always expected. For example, positive control and negative control samples will have a unimodal distribution with the peak close to 1 and 0 respectively.",
      "Tier2_QC_Fail: number of failed tier 2 QC metrics",
      "Tier2_QC_Total: total of tier 2 QC metrics, including QC probe metrics plus sum of meth and unmeth medians",
      "Bisulfite_Conversion_Type_I_Probe_Design_Green_Converted_Unconverted_Ratio: assess the efficiency of bisulfite conversion of the genomic DNA for probes of type I design on green channel. The Infinium Methylation probes query a [C/T] polymorphism created by bisulfite conversion of non-CpG cytosines in the genome. The controls use Infinium I probe design and allele-specific single base extension to monitor efficiency of bisulfite conversion. If the bisulfite conversion reaction was successful, the Converted probes matches the converted sequence and get extended. If the sample has unconverted DNA, the nconverted probes get extended. There are no underlying C bases in the primer landing sites, except for the query site itself. The metric is lowest converted intensity divided by highest unconverted intensity. It should be greater than 1.",
      "Bisulfite_Conversion_Type_I_Probe_Design_Green_Background_Highest_Unconverted_Ratio: assess the efficiency of bisulfite conversion of the genomic DNA for probes of type I design on green channel. The metric is calculated as highest AT of extension green probes. It should be greater than 1. Also see Bisulfite_Conversion_Type_I_Probe_Design_Green_Converted_Unconverted_Ratio.",
      "Bisulfite_Conversion_Type_I_Probe_Design_Red_Converted_Unconverted_Ratio: assess the efficiency of bisulfite conversion of the genomic DNA for probes of type I design on red channel. The Infinium Methylation probes query a [C/T] polymorphism created by bisulfite conversion of non-CpG cytosines in the genome. The controls use Infinium I probe design and allele-specific single base extension to monitor efficiency of bisulfite conversion. If the bisulfite conversion reaction was successful, the Converted probes matches the converted sequence and get extended. If the sample has unconverted DNA, the nconverted probes get extended. There are no underlying C bases in the primer landing sites, except for the query site itself. The metric is lowest converted intensity divided by highest unconverted intensity. It should be greater than 1.",
      "Bisulfite_Conversion_Type_I_Probe_Design_Red_Background_Highest_Unconverted_Ratio: assess the efficiency of bisulfite conversion of the genomic DNA for probes of type I design on red channel. the metric is calculated as highest CG of extension red probes. It should be greater than 1. Also see Bisulfite_Conversion_Type_I_Probe_Design_Red_Converted_Unconverted_Ratio.",
      "Bisulfite_Conversion_Type_II_Probe_Design_Converted_Unconverted_Ratio: assess the efficiency of bisulfite conversion of the genomic DNA for probes of type II design. The Infinium Methylation probes query a [C/T] polymorphism created by bisulfite conversion of non-CpG cytosines in the genome. These controls use Infinium II probe design and single base extension to monitor efficiency of bisulfite conversion. If the bisulfite conversion reaction was successful, the 'A' base gets incorporated and the probe has intensity in the red channel. If the sample has unconverted DNA, the 'G' base gets incorporated across the unconverted cytosine, and the probe has elevated signal in the green channel. The metric is lowest intensity on red channel divided by highest intensity on green channel. It should be greater than 1.",
      "Bisulfite_Conversion_Type_II_Probe_Design_Background_Highest_Unconverted_Ratio: assess the efficiency of bisulfite conversion of the genomic DNA for probes of type II design. the metric is calculated as highest CG of extension red probes. It should be greater than 1. Also see Bisulfite_Conversion_Type_II_Probe_Design_Converted_Unconverted_Ratio.",
      "Hybridization_Green_High_Medium_Ratio: Hybridization controls test the overall performance of the Infinium Assay using synthetic targets instead of amplified DNA. These synthetic targets complement the sequence on the array, allowing the probe to extend on the synthetic target as a template. Synthetic targets are present in the Hybridization Buffer at 3 levels, monitoring the response from high-concentration (5 pM), medium concentration (1 pM), and low concentration (0.2 pM) targets. All bead type IDs result in signals with various intensities, corresponding to the concentrations of the initial synthetic targets. The value for high concentration is always higher than medium and the value for medium concentration is always higher than low. Thus, the value should be greater than 1.",
      "Hybridization_Green_Medium_Low_Ratio: See Hybridization_Green_High_Medium_Ratio.",
      "Extension_Green_Lowest_CG_Highest_AT_Ratio: Extension controls test the extension efficiency of A, T, C, and G nucleotides from a hairpin probe, and are therefore sample independent. In the green channel, the lowest intensity for C or G is always greater than the highest intensity for A or T. Vice versa, in the red channel. The metric is the ratio and is expected to be greater than 5.",
      "Extension_Red_Lowest_AT_Highest_CG_Ratio: similar to Extension_Green_Lowest_CG_Highest_AT_Ratio but on red channel.",
      "Target_Removal_1_Background_Control_Ratio: Target removal controls test the efficiency of the stripping step after the extension reaction. In contrast to allele-specific extension, the control oligos are extended using the probe sequence as a template. This process generates labeled targets. The probe sequences are designed such that extension from the probe does not occur. All target removal controls result in low signal compared to the hybridization controls, indicating that the targets were removed efficiently after extension. Target removal controls are present in the Hybridization Buffer. The value should be greater than 1.",
      "Target_Removal_2_Background_Control_Ratio: see Target_Removal_1_Background_Control_Ratio.",
      "Staining_Green_High_Background_Ratio: Staining controls are used to examine the efficiency of the staining step in both the red and green channels. These controls are independent of the hybridization and extension step. The green channel shows a higher signal for biotin staining when compared to biotin background, whereas the red channel shows higher signal for DNP staining when compared to DNP background. The metric provided for green is the (Biotin High value)/ (Biotin Bkg) and the metric provided for red is (DNP High value)/(DNP Bkg value). The value should be greater than 5.",
      "Staining_Red_High_Background_Ratio: similar to Staining_Green_High_Background_Ratio, but on red channel.",
      "Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio: to monitor potential nonspecific primer extension for Infinium I assay probes on green channel. Specificity controls are designed against nonpolymorphic T sites. These controls are designed to monitor allele-specific extension for Infinium I probes. The methylation status of a particular cytosine is carried out following bisulfite treatment of DNA by using query probes for unmethylated and methylated state of each CpG locus. In assay oligo design, the A/T match corresponds to the unmethylated status of the interrogated C, and G/C match corresponds to the methylated status of C. G/T mismatch controls check for nonspecific detection of methylation signal over unmethylated background. PM controls correspond to A/T perfect match and give high signal. MM controls correspond to G/T mismatch and give low signal. The value should be greater than 1.",
      "Specificity_Type_I_Probe_Design_Red_Perfectly_Matched_Mismatched_Ratio: similar to Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio but on red channel.",
      'Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio: to monitor potential nonspecific primer extension for Infinium II assay probes. Specificity controls are designed against nonpolymorphic T sites. These controls are designed to monitor extension specificity for Infinium II probes and check for potential nonspecific detection of methylation signal over unmethylated background. Specificity II probes incorporate the "A" base across the nonpolymorphic T and have intensity in the Red channel. If there was nonspecific incorporation of the "G" base, the probe has elevated signal in the Green channel. The value should be greater than 1.',
      'Specificity_Type_II_Probe_Design_Background_Highest_Green_Ratio: see Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio.',
      "Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio: Nonpolymorphic controls test the overall performance of the assay, from amplification to detection, by querying a particular base in a nonpolymorphic region of the genome. They let you compare assay performance across different samples. One nonpolymorphic control has been designed for each of the 4 nucleotides (A, T, C, and G). In the green channel, the lowest intensity of C or G is always greater than the highest intensity of A or T. Vice versa, in the red channel. The value should be greater than 5.",
      "Nonpolymorphic_Red_Lowest_AT_Highest_CG_Ratio: similar to Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio but on red channel.",
      "Restoration_Green_Background_Ratio: restoration control probe. The green channel intensity should be higher than background. If FFPE DNA restore kit is used, the value should be greater than 1. If not, it should be greater than 0.",
      "Sum_Meth_Unmeth_Medians_Normalized: sum of the medians of meth and unmeth signals after normalization. Threshold have been determined.",
      "Sum_Meth_Unmeth_Medians_Raw: sum of the medians of meth and unmeth signals without normalization. The threshold is temporarily set as 21.",
      "Predicted_Gender: compare with the true gender if we have the same information."
    ),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb,
            "Metrics",
            description,
            startRow = nrow(qc) + 10,
            colNames = FALSE)
  # Insert the beta value distribution plot
  if (!is.null(beta_value_distribution_file)) {
    logger::log_debug("Insert beta value distribution plot")
    openxlsx::insertImage(
      wb,
      "Metrics",
      beta_value_distribution_file,
      width = 11 * 0.8,
      height = 7 * 0.8,
      startRow = nrow(qc) + 5,
      startCol = 6,
      units = "in",
      dpi = 300
    )
  }
  openxlsx::saveWorkbook(wb, excel_file, overwrite = TRUE)
}
