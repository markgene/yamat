# QC on control probes.
# Based on DRAGEN and Illumina documentations.

#' Get QC metric of staining green probes.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @returns A data frame of QC metrics.
get_staining_green_qc <- function(ctrl_probes) {
  staining_green_probes <- ctrl_probes %>%
    dplyr::filter(Type == "STAINING", Channel == "Green") %>%
    dplyr::filter(!is.na(Expected_Grn)) %>%
    dplyr::group_by(Sample, Expected_Grn) %>%
    dplyr::summarize(Mean_Intensity = mean(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(data = .,
                names_from = "Expected_Grn",
                values_from = "Mean_Intensity") %>%
    dplyr::mutate(Staining_Green_High_Background_Ratio = High / Background) %>%
    dplyr::rename(
      Staining_Green_High_Mean_Intensity = High,
      Staining_Green_Background_Mean_Intensity = Background
    )
  staining_green_probes
}

#' Get QC metric of staining red probes.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @returns A data frame of QC metrics.
get_staining_red_qc <- function(ctrl_probes) {
  staining_red_probes <- ctrl_probes %>%
    dplyr::filter(Type == "STAINING", Channel == "Red") %>%
    dplyr::filter(!is.na(Expected_Red)) %>%
    dplyr::group_by(Sample, Expected_Red) %>%
    dplyr::summarize(Mean_Intensity = mean(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(data = .,
                names_from = "Expected_Red",
                values_from = "Mean_Intensity") %>%
    dplyr::mutate(Staining_Red_High_Background_Ratio = High / Background) %>%
    dplyr::rename(
      Staining_Red_High_Mean_Intensity = High,
      Staining_Red_Background_Mean_Intensity = Background
    )
  staining_red_probes
}


#' Get QC metric of extension green probes.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @returns A data frame of QC metrics.
get_extension_green_qc <- function(ctrl_probes) {
  extension_green_probes <- ctrl_probes %>%
    dplyr::filter(Type == "EXTENSION", Channel == "Green") %>%
    dplyr::mutate(Expected_Grn2 = sapply(Expected_Grn, function(x) {
      ifelse(is.na(x), "Background", as.character(x))
    })) %>%
    dplyr::group_by(Sample, Expected_Grn2) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Expected_Grn2",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(Extension_Green_Lowest_CG_Highest_AT_Ratio = Min_Intensity_High / Max_Intensity_Background) %>%
    dplyr::rename(Extension_Green_Lowest_CG = Min_Intensity_High,
                  Extension_Green_Highest_AT = Max_Intensity_Background) %>%
    dplyr::select(
      Sample,
      Extension_Green_Lowest_CG_Highest_AT_Ratio,
      Extension_Green_Lowest_CG,
      Extension_Green_Highest_AT
    )
  extension_green_probes
}


#' Get QC metric of extension red probes.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @returns A data frame of QC metrics.
get_extension_red_qc <- function(ctrl_probes) {
  extension_red_probes <- ctrl_probes %>%
    dplyr::filter(Type == "EXTENSION", Channel == "Red") %>%
    dplyr::mutate(Expected_Red2 = sapply(Expected_Red, function(x) {
      ifelse(is.na(x), "Background", as.character(x))
    }), ) %>%
    dplyr::group_by(Sample, Expected_Red2) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Expected_Red2",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(Extension_Red_Lowest_AT_Highest_CG_Ratio = Min_Intensity_High / Max_Intensity_Background) %>%
    dplyr::rename(Extension_Red_Lowest_AT = Min_Intensity_High,
                  Extension_Red_Highest_CG = Max_Intensity_Background) %>%
    dplyr::select(
      Sample,
      Extension_Red_Lowest_AT_Highest_CG_Ratio,
      Extension_Red_Lowest_AT,
      Extension_Red_Highest_CG
    )
  extension_red_probes
}


#' Get QC metric of restoration probe.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @param background_offset background correction offset. Default to 3000.
#' @returns A data frame of QC metrics.
get_restoration_qc <- function(ctrl_probes, background_offset = 3000) {
  restoration_green <- ctrl_probes %>%
    dplyr::filter(Type == "RESTORATION", Channel == "Green") %>%
    dplyr::group_by(Sample, ExtendedType) %>%
    dplyr::summarize(Mean_Intensity = mean(Intensity)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-ExtendedType)
  extension_green <- get_extension_green_qc(ctrl_probes)
  restoration_probes <- left_join(restoration_green, extension_green, by = "Sample") %>%
    dplyr::mutate(
      Restoration_Green_Background_Ratio = Mean_Intensity / (Extension_Green_Highest_AT + background_offset)
    ) %>%
    dplyr::rename(Restoration_Green_Intensity = Mean_Intensity) %>%
    dplyr::select(-starts_with("Extension_"))
  restoration_probes
}


#' Get QC metric of hybridization probe.
#'
#' It is only on green channel.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @returns A data frame of QC metrics.
get_hybridization_green_qc <- function(ctrl_probes) {
  hybridization_green_probes <- ctrl_probes %>%
    dplyr::filter(Type == "HYBRIDIZATION", Channel == "Green") %>%
    dplyr::filter(!is.na(Expected_Grn)) %>%
    dplyr::group_by(Sample, Expected_Grn) %>%
    dplyr::summarize(Mean_Intensity = mean(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(data = .,
                names_from = "Expected_Grn",
                values_from = "Mean_Intensity") %>%
    dplyr::mutate(
      Hybridization_Green_High_Medium_Ratio = High / Medium,
      Hybridization_Green_Medium_Low_Ratio = Medium / Low,
    ) %>%
    dplyr::rename(
      Hybridization_Green_High_Intensity = High,
      Hybridization_Green_Medium_Intensity = Medium,
      Hybridization_Green_Low_Intensity = Low,
    )
  hybridization_green_probes
}


#' Get QC metric of target removal probes.
#'
#' It is only on green channel.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @param background_offset background correction offset. Default to 3000.
#' @returns A data frame of QC metrics.
get_target_removal_qc <- function(ctrl_probes, background_offset = 3000) {
  target_removal_probes <- ctrl_probes %>%
    dplyr::filter(Type == "TARGET REMOVAL", Channel == "Green") %>%
    dplyr::group_by(Sample, ExtendedType) %>%
    dplyr::summarize(Mean_Intensity = mean(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(data = .,
                names_from = "ExtendedType",
                values_from = "Mean_Intensity") %>%
    dplyr::rename(
      Target_Removal_1_Control_Intensity = `Target Removal 1`,
      Target_Removal_2_Control_Intensity = `Target Removal 2`,
    )
  extension_green <- get_extension_green_qc(ctrl_probes)
  target_removal <- left_join(target_removal_probes, extension_green, by = "Sample") %>%
    dplyr::mutate(
      Target_Removal_1_Background_Control_Ratio =  (Extension_Green_Highest_AT + background_offset) / Target_Removal_1_Control_Intensity,
      Target_Removal_2_Background_Control_Ratio =  (Extension_Green_Highest_AT + background_offset) / Target_Removal_2_Control_Intensity
    ) %>%
    dplyr::select(-starts_with("Extension_"))
  target_removal
}


#' Get QC metric of bisulfite Conversion of type I probe design green probes.
#'
#' There are conflictings between Illumina Infinium HD Methylation Assay
#' (Document # 15019519 v10) and \href{https://help.dragenarray.illumina.com/product-guides/output-files#methyl_qc_report}{DRAGEN documentation}.
#' The Illumina Infinium HD Methylation Assay said C1-C3 and U1-U3 are for
#' bisulfite conversion I green channel. But based on DRAGEN document, C3 and U3
#' are actually on red channel. I use probes C1-C2 and U1-U2 in this function.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @param background_offset background correction offset. Default to 3000.
#' @returns A data frame of QC metrics.
get_bisulfite_converstion_type_1_green_qc <- function(ctrl_probes, background_offset = 3000) {
  bisulfite_converstion_type_1_green_cu_ratio <- ctrl_probes %>%
    dplyr::filter(Type == "BISULFITE CONVERSION I", Channel == "Green") %>%
    dplyr::mutate(Converted_Unconverted = ifelse(
      ExtendedType %in% c("BS Conversion I-C1", "BS Conversion I-C2"),
      "Converted",
      "Unconverted"
    )) %>%
    dplyr::group_by(Sample, Converted_Unconverted) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Converted_Unconverted",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(
      Bisulfite_Conversion_Type_I_Probe_Design_Green_Converted_Unconverted_Ratio = Min_Intensity_Converted / Max_Intensity_Unconverted
    ) %>%
    dplyr::rename(
      Bisulfite_Conversion_Type_I_Probe_Design_Green_Lowest_Concverted = Min_Intensity_Converted,
      Bisulfite_Conversion_Type_I_Probe_Design_Green_Highest_Unconverted = Max_Intensity_Unconverted
    ) %>%
    dplyr::select(
      Sample,
      Bisulfite_Conversion_Type_I_Probe_Design_Green_Converted_Unconverted_Ratio,
      Bisulfite_Conversion_Type_I_Probe_Design_Green_Lowest_Concverted,
      Bisulfite_Conversion_Type_I_Probe_Design_Green_Highest_Unconverted,
    )
  extension_green <- get_extension_green_qc(ctrl_probes)
  bisulfite_converstion_type_1_green <- left_join(bisulfite_converstion_type_1_green_cu_ratio,
                                                  extension_green,
                                                  by = "Sample") %>%
    dplyr::mutate(
      Bisulfite_Conversion_Type_I_Probe_Design_Green_Background_Highest_Unconverted_Ratio =
        (Extension_Green_Highest_AT + background_offset) / Bisulfite_Conversion_Type_I_Probe_Design_Green_Highest_Unconverted
    ) %>%
    dplyr::select(-starts_with("Extension_"))
  bisulfite_converstion_type_1_green
}


#' Get QC metric of bisulfite Conversion of type I probe design red probes.
#'
#' There are conflictings between Illumina Infinium HD Methylation Assay
#' (Document # 15019519 v10) and [DRAGEN documentation](https://help.dragenarray.illumina.com/product-guides/output-files#methyl_qc_report).
#' The Illumina Infinium HD Methylation Assay said C4-C6 and U4-U6 are for
#' bisulfite conversion I green channel. But based on DRAGEN document, C3 and U3
#' are actually on red channel too. I use probes C3-C6 and U3-U6 in this function.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @param background_offset background correction offset. Default to 3000.
#' @returns A data frame of QC metrics.
get_bisulfite_converstion_type_1_red_qc <- function(ctrl_probes, background_offset = 3000) {
  bisulfite_converstion_type_1_red_cu_ratio <- ctrl_probes %>%
    dplyr::filter(Type == "BISULFITE CONVERSION I", Channel == "Red") %>%
    dplyr::filter(
      ExtendedType %in% c(
        "BS Conversion I-C3",
        "BS Conversion I-C4",
        "BS Conversion I-C5",
        "BS Conversion I-U3",
        "BS Conversion I-U4",
        "BS Conversion I-U5"
      )
    ) %>%
    dplyr::mutate(Converted_Unconverted = ifelse(
      ExtendedType %in% c("BS Conversion I-C3", "BS Conversion I-C4", "BS Conversion I-C5"),
      "Converted",
      "Unconverted"
    )) %>%
    dplyr::group_by(Sample, Converted_Unconverted) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Converted_Unconverted",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(
      Bisulfite_Conversion_Type_I_Probe_Design_Red_Converted_Unconverted_Ratio = Min_Intensity_Converted / Max_Intensity_Unconverted
    ) %>%
    dplyr::rename(
      Bisulfite_Conversion_Type_I_Probe_Design_Red_Lowest_Concverted = Min_Intensity_Converted,
      Bisulfite_Conversion_Type_I_Probe_Design_Red_Highest_Unconverted = Max_Intensity_Unconverted
    ) %>%
    dplyr::select(
      Sample,
      Bisulfite_Conversion_Type_I_Probe_Design_Red_Converted_Unconverted_Ratio,
      Bisulfite_Conversion_Type_I_Probe_Design_Red_Lowest_Concverted,
      Bisulfite_Conversion_Type_I_Probe_Design_Red_Highest_Unconverted,
    )
  extension_red <- get_extension_red_qc(ctrl_probes)
  bisulfite_converstion_type_1_red <- left_join(bisulfite_converstion_type_1_red_cu_ratio,
                                                extension_red,
                                                by = "Sample") %>%
    dplyr::mutate(
      Bisulfite_Conversion_Type_I_Probe_Design_Red_Background_Highest_Unconverted_Ratio = (Extension_Red_Highest_CG + background_offset) / Bisulfite_Conversion_Type_I_Probe_Design_Red_Highest_Unconverted
    ) %>%
    dplyr::select(-starts_with("Extension_"))
  bisulfite_converstion_type_1_red
}


#' Get QC metric of bisulfite Conversion of type II probe design probes.
#'
#' Only on red channel.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @param background_offset background correction offset. Default to 3000.
#' @returns A data frame of QC metrics.
get_bisulfite_converstion_type_2_qc <- function(ctrl_probes, background_offset = 3000) {
  bisulfite_converstion_type_2_cu_ratio <- ctrl_probes %>%
    dplyr::filter(Type == "BISULFITE CONVERSION II") %>%
    dplyr::mutate(Converted_Unconverted = ifelse(Channel == "Red", "Converted", "Unconverted")) %>%
    dplyr::group_by(Sample, Converted_Unconverted) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Converted_Unconverted",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(
      Bisulfite_Conversion_Type_II_Probe_Design_Converted_Unconverted_Ratio = Min_Intensity_Converted / Max_Intensity_Unconverted
    ) %>%
    dplyr::rename(
      Bisulfite_Conversion_Type_II_Probe_Design_Lowest_Concverted = Min_Intensity_Converted,
      Bisulfite_Conversion_Type_II_Probe_Design_Highest_Unconverted = Max_Intensity_Unconverted
    ) %>%
    dplyr::select(
      Sample,
      Bisulfite_Conversion_Type_II_Probe_Design_Converted_Unconverted_Ratio,
      Bisulfite_Conversion_Type_II_Probe_Design_Lowest_Concverted,
      Bisulfite_Conversion_Type_II_Probe_Design_Highest_Unconverted,
    )
  extension_green <- get_extension_green_qc(ctrl_probes)
  bisulfite_converstion_type_2 <- left_join(bisulfite_converstion_type_2_cu_ratio,
                                            extension_green,
                                            by = "Sample") %>%
    dplyr::mutate(
      Bisulfite_Conversion_Type_II_Probe_Design_Background_Highest_Unconverted_Ratio = (Extension_Green_Highest_AT + background_offset) / Bisulfite_Conversion_Type_II_Probe_Design_Highest_Unconverted
    ) %>%
    dplyr::select(-starts_with("Extension_"))
  bisulfite_converstion_type_2
}


#' Get QC metric of specificity of type I probe design green probes.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @returns A data frame of QC metrics.
get_specificity_type_1_green_qc <- function(ctrl_probes) {
  specificity_type_1_green_pm_mm_ratio <- ctrl_probes %>%
    dplyr::filter(
      Type == "SPECIFICITY I",
      Channel == "Green",
      stringr::str_detect(ExtendedType, "^GT Mismatch [1-3]")
    ) %>%
    dplyr::mutate(Matched = ifelse(
      stringr::str_detect(ExtendedType, "^GT Mismatch [1-3] \\(PM\\)$"),
      "Perfectly_Matched",
      "Mismatched"
    )) %>%
    dplyr::group_by(Sample, Matched) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Matched",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(
      Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio = Min_Intensity_Perfectly_Matched / Max_Intensity_Mismatched
    ) %>%
    dplyr::rename(
      Specificity_Type_I_Probe_Design_Green_Lowest_Perfectly_Matched = Min_Intensity_Perfectly_Matched,
      Specificity_Type_I_Probe_Design_Green_Highest_Mismatched = Max_Intensity_Mismatched
    ) %>%
    dplyr::select(
      Sample,
      Specificity_Type_I_Probe_Design_Green_Perfectly_Matched_Mismatched_Ratio,
      Specificity_Type_I_Probe_Design_Green_Lowest_Perfectly_Matched,
      Specificity_Type_I_Probe_Design_Green_Highest_Mismatched,
    )
  specificity_type_1_green_pm_mm_ratio
}


#' Get QC metric of specificity of type I probe design red probes.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @returns A data frame of QC metrics.
get_specificity_type_1_red_qc <- function(ctrl_probes) {
  specificity_type_1_red_pm_mm_ratio <- ctrl_probes %>%
    dplyr::filter(
      Type == "SPECIFICITY I",
      Channel == "Red",
      stringr::str_detect(ExtendedType, "^GT Mismatch [4-6]")
    ) %>%
    dplyr::mutate(Matched = ifelse(
      stringr::str_detect(ExtendedType, "^GT Mismatch [4-6] \\(PM\\)$"),
      "Perfectly_Matched",
      "Mismatched"
    )) %>%
    dplyr::group_by(Sample, Matched) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Matched",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(
      Specificity_Type_I_Probe_Design_Red_Perfectly_Matched_Mismatched_Ratio = Min_Intensity_Perfectly_Matched / Max_Intensity_Mismatched
    ) %>%
    dplyr::rename(
      Specificity_Type_I_Probe_Design_Red_Lowest_Perfectly_Matched = Min_Intensity_Perfectly_Matched,
      Specificity_Type_I_Probe_Design_Red_Highest_Mismatched = Max_Intensity_Mismatched
    ) %>%
    dplyr::select(
      Sample,
      Specificity_Type_I_Probe_Design_Red_Perfectly_Matched_Mismatched_Ratio,
      Specificity_Type_I_Probe_Design_Red_Lowest_Perfectly_Matched,
      Specificity_Type_I_Probe_Design_Red_Highest_Mismatched,
    )
  specificity_type_1_red_pm_mm_ratio
}


#' Get QC metric of specificity of type II probe design probes.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @param background_offset background correction offset. Default to 3000.
#' @returns A data frame of QC metrics.
get_specificity_type_2_qc <- function(ctrl_probes, background_offset = 3000) {
  specificity_type_2_lowest_red_highest_green_ratio <- ctrl_probes %>%
    dplyr::filter(Type == "SPECIFICITY II") %>%
    dplyr::group_by(Sample, Channel) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Channel",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(
      Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio = Min_Intensity_Red / Max_Intensity_Green
    ) %>%
    dplyr::rename(
      Specificity_Type_II_Probe_Design_Lowest_Red = Min_Intensity_Red,
      Specificity_Type_II_Probe_Design_Highest_Green = Max_Intensity_Green
    ) %>%
    dplyr::select(
      Sample,
      Specificity_Type_II_Probe_Design_Lowest_Red_Highest_Green_Ratio,
      Specificity_Type_II_Probe_Design_Lowest_Red,
      Specificity_Type_II_Probe_Design_Highest_Green,
    )
  extension_green <- get_extension_green_qc(ctrl_probes)
  specificity_type_2 <- left_join(specificity_type_2_lowest_red_highest_green_ratio,
                                  extension_green,
                                  by = "Sample") %>%
    dplyr::mutate(
      Specificity_Type_II_Probe_Design_Background_Highest_Green_Ratio = (Extension_Green_Highest_AT + background_offset) / Specificity_Type_II_Probe_Design_Highest_Green
    ) %>%
    dplyr::select(-starts_with("Extension_"))
  specificity_type_2
}


#' Get QC metric of non-polymorphic green probes.
#'
#' There are five extra NP (G) probes of uncertain use. which I do not use
#' for non-polymorphic probe QC metric computation.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @returns A data frame of QC metrics.
get_nonpolymorphic_green_qc <- function(ctrl_probes) {
  nonpolymorphic_green_probes <- ctrl_probes %>%
    dplyr::filter(Type == "NON-POLYMORPHIC", Channel == "Green") %>%
    dplyr::mutate(Expected_Grn2 = sapply(Expected_Grn, function(x) {
      ifelse(is.na(x), "Background", as.character(x))
    })) %>%
    dplyr::group_by(Sample, Expected_Grn2) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Expected_Grn2",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio = Min_Intensity_High / Max_Intensity_Background) %>%
    dplyr::rename(
      Nonpolymorphic_Green_Lowest_CG = Min_Intensity_High,
      Nonpolymorphic_Green_Highest_AT = Max_Intensity_Background
    ) %>%
    dplyr::select(
      Sample,
      Nonpolymorphic_Green_Lowest_CG_Highest_AT_Ratio,
      Nonpolymorphic_Green_Lowest_CG,
      Nonpolymorphic_Green_Highest_AT
    )
  nonpolymorphic_green_probes
}


#' Get QC metric of non-polymorphic red probes.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @returns A data frame of QC metrics.
get_nonpolymorphic_red_qc <- function(ctrl_probes) {
  nonpolymorphic_red_probes <- ctrl_probes %>%
    dplyr::filter(Type == "NON-POLYMORPHIC", Channel == "Red") %>%
    dplyr::mutate(Expected_Red2 = sapply(Expected_Red, function(x) {
      ifelse(is.na(x), "Background", as.character(x))
    }), ) %>%
    dplyr::group_by(Sample, Expected_Red2) %>%
    dplyr::summarize(Min_Intensity = min(Intensity),
                     Max_Intensity = max(Intensity)) %>%
    dplyr::ungroup() %>%
    pivot_wider(
      data = .,
      names_from = "Expected_Red2",
      values_from = c("Min_Intensity", "Max_Intensity")
    ) %>%
    dplyr::mutate(Nonpolymorphic_Red_Lowest_AT_Highest_CG_Ratio = Min_Intensity_High / Max_Intensity_Background) %>%
    dplyr::rename(
      Nonpolymorphic_Red_Lowest_AT = Min_Intensity_High,
      Nonpolymorphic_Red_Highest_CG = Max_Intensity_Background
    ) %>%
    dplyr::select(
      Sample,
      Nonpolymorphic_Red_Lowest_AT_Highest_CG_Ratio,
      Nonpolymorphic_Red_Lowest_AT,
      Nonpolymorphic_Red_Highest_CG
    )
  nonpolymorphic_red_probes
}


#' Get QC metrics of control probes.
#'
#' @param ctrl_probes The object returned by \code{\link{control_probe_intensities}}.
#' @param background_offset background correction offset. Default to 3000.
#' @returns A data frame of QC metrics.
get_control_probe_qc_metrics <- function(rgset, background_offset = 3000) {
  qc_df <- minfi::pData(rgset) %>%
    as.data.frame()
  restoration <- get_restoration_qc(ctrl_probes, background_offset = background_offset)
  qc_df <- dplyr::left_join(qc_df, restoration, by = c("Basename" = "Sample"))
  staining_green <- get_staining_green_qc(ctrl_probes)
  qc_df <- dplyr::left_join(qc_df, staining_green, by = c("Basename" = "Sample"))
  staining_red <- get_staining_red_qc(ctrl_probes)
  qc_df <- dplyr::left_join(qc_df, staining_red, by = c("Basename" = "Sample"))
  extension_green <- get_extension_green_qc(ctrl_probes)
  qc_df <- dplyr::left_join(qc_df, extension_green, by = c("Basename" = "Sample"))
  extension_red <- get_extension_red_qc(ctrl_probes)
  qc_df <- dplyr::left_join(qc_df, extension_red, by = c("Basename" = "Sample"))
  hybridization_green <- get_hybridization_green_qc(ctrl_probes)
  qc_df <- dplyr::left_join(qc_df, hybridization_green, by = c("Basename" = "Sample"))
  target_removal <- get_target_removal_qc(ctrl_probes, background_offset = background_offset)
  qc_df <- dplyr::left_join(qc_df, target_removal, by = c("Basename" = "Sample"))
  bisulfite_converstion_type_1_green_qc <- get_bisulfite_converstion_type_1_green_qc(ctrl_probes, background_offset = background_offset)
  qc_df <- dplyr::left_join(qc_df,
                            bisulfite_converstion_type_1_green_qc,
                            by = c("Basename" = "Sample"))
  bisulfite_converstion_type_1_red_qc <- get_bisulfite_converstion_type_1_red_qc(ctrl_probes, background_offset = background_offset)
  qc_df <- dplyr::left_join(qc_df,
                            bisulfite_converstion_type_1_red_qc,
                            by = c("Basename" = "Sample"))
  bisulfite_converstion_type_2_qc <- get_bisulfite_converstion_type_2_qc(ctrl_probes, background_offset = background_offset)
  qc_df <- dplyr::left_join(qc_df,
                            bisulfite_converstion_type_2_qc,
                            by = c("Basename" = "Sample"))
  specificity_type_1_green <- get_specificity_type_1_green_qc(ctrl_probes)
  qc_df <- dplyr::left_join(qc_df, specificity_type_1_green, by = c("Basename" = "Sample"))
  specificity_type_1_red <- get_specificity_type_1_red_qc(ctrl_probes)
  qc_df <- dplyr::left_join(qc_df, specificity_type_1_red, by = c("Basename" = "Sample"))
  specificity_type_2 <- get_specificity_type_2_qc(ctrl_probes, background_offset = background_offset)
  qc_df <- dplyr::left_join(qc_df, specificity_type_2, by = c("Basename" = "Sample"))
  nonpolymorphic_green <- get_nonpolymorphic_green_qc(ctrl_probes)
  qc_df <- dplyr::left_join(qc_df, nonpolymorphic_green, by = c("Basename" = "Sample"))
  nonpolymorphic_red <- get_nonpolymorphic_red_qc(ctrl_probes)
  qc_df <- dplyr::left_join(qc_df, nonpolymorphic_red, by = c("Basename" = "Sample"))
  qc_df
}
