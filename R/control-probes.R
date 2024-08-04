# Quality Control in Terms of Control Probes


#' Quality control plot of control probes in terms of raw intensities.
#'
#' @param x An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param s An integer scalar. Default to 1.
#' @return A list of \code{df}, \code{red_plot}, \code{green_plot},
#'   \code{norm_plot}, \code{independent_control_plot}.
#'   The {df} is a \code{data.frame} containing the following columns:
#'   Address, Type, Color, ExtendedType, Channel, Expected_Grn, Expected_Red,
#'   Sample, and Intensity.
#'   Each of the \code{red_plot}, \code{green_plot}, \code{norm_plot} and
#'   \code{independent_control_plot} is an object of \code{\link[ggplot2]{ggplot}}
#'   class. They are plots of sample-dependent controls of red and green channel,
#'   normalization probes and sample-independent controls, respectively.
#' @export
plot_control_probes <- function(x, s = 1) {
  xs <- x[, s]
  df <- control_probe_intensities(xs)
  red_plot <- .plot_control_probes_red(df) +
    ggplot2::labs(title = "Red") +
    ggplot2::theme(legend.position = "none")
  green_plot <- .plot_control_probes_green(df) +
    ggplot2::labs(title = "Green") +
    ggplot2::theme(legend.position = "none")
  norm_plot <- .plot_control_probes_norm(df) %>%
    ggpubr::annotate_figure(top = ggpubr::text_grob("Normalization Probes", face = "bold"))
  independent_control_plot <- .plot_sample_independent_control_probes(df)
  dependent_control_plot <-
    ggpubr::ggarrange(red_plot, green_plot, ncol = 2, nrow = 1) %>%
    ggpubr::annotate_figure(top = ggpubr::text_grob("Sample-Dependent Controls", face = "bold"))
  one_plot <-
    ggpubr::ggarrange(independent_control_plot, dependent_control_plot, norm_plot, ncol = 1, nrow = 3) %>%
    ggpubr::annotate_figure(top = ggpubr::text_grob(minfi::sampleNames(xs)[1], face = "bold", size = 14))
  list(
    df = df,
    red_plot = red_plot,
    green_plot = green_plot,
    norm_plot = norm_plot,
    independent_control_plot = independent_control_plot,
    one_plot = one_plot
  )
}


#' Get control probes and their raw intensities.
#'
#' Notes:
#'
#' 1. There are conflictings between Illumina Infinium HD Methylation Assay
#' (Document # 15019519 v10) and \href{https://help.dragenarray.illumina.com/product-guides/output-files#methyl_qc_report}{DRAGEN documentation}.
#' The Illumina Infinium HD Methylation Assay said C1-C3 and U1-U3 are for
#' bisulfite conversion I green channel. But based on DRAGEN document, C3 and U3
#' are actually on red channel. I use probes C1-C2 and U1-U2 in this function.
#'
#' 2. There are conflictings between Illumina Infinium HD Methylation Assay
#' (Document # 15019519 v10) and [DRAGEN documentation](https://help.dragenarray.illumina.com/product-guides/output-files#methyl_qc_report).
#' The Illumina Infinium HD Methylation Assay said C4-C6 and U4-U6 are for
#' bisulfite conversion I green channel. But based on DRAGEN document, C3 and U3
#' are actually on red channel too. I use probes C3-C6 and U3-U6 in this function.
#'
#' 3. There are five extra NP (G) probes of uncertain use, which I do not use
#' for non-polymorphic probe QC metric computation.
#'
#' @param x An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @return A \code{data.frame} containing the following columns: Address, Type,
#'   Color, ExtendedType, Channel, Expected_Grn, Expected_Red, Sample, Group,
#'   Intensity.
#' @export
control_probe_intensities <- function(x) {
  ctrl_probes <- minfi::getProbeInfo(x, type = "Control") %>%
    as.data.frame() %>%
    .expected_intensity()
  ctrl_x <- x[ctrl_probes$Address, ]
  red <- as.data.frame(minfi::getRed(ctrl_x))
  grn <- as.data.frame(minfi::getGreen(ctrl_x))
  red$Address <- row.names(red)
  grn$Address <- row.names(grn)
  red <- ctrl_probes %>%
    dplyr::mutate(Channel = "Red") %>%
    dplyr::left_join(red, by = "Address")
  grn <- ctrl_probes %>%
    dplyr::mutate(Channel = "Green") %>%
    dplyr::left_join(grn, by = "Address")
  rbind(red, grn) %>%
    dplyr::arrange(Type, ExtendedType, Address, Channel) %>%
    tidyr::gather(
      Sample,
      Intensity,
      -Address,
      -Type,
      -Color,
      -ExtendedType,
      -Channel,
      -Expected_Red,
      -Expected_Grn,
      -Group
    )
}


#' Plot control probes evaluated in Red channel.
#'
#' @param df A \code{data.frame} returned by \code{\link{get_control_probe_intensities}}.
#' @return An object of \code{\link[ggplot2]{ggplot}} class.
#' @noRd
.plot_control_probes_red <- function(df, jitter_width = 0.2) {
  df %>%
    dplyr::filter(Channel == "Red") %>%
    dplyr::filter(!is.na(Expected_Red)) %>%
    dplyr::filter(Group == "Sample Dependent") %>%
    ggplot2::ggplot(ggplot2::aes(x = Expected_Red, y = Intensity, colour = Expected_Red)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = jitter_width) +
    ggplot2::facet_grid(Sample ~ Type, scales = "free_x") +
    ggplot2::labs(title = "Evaluate Red (Sample-Dependent Controls)", colour = "Expected") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "right"
    )
}


#' Plot control probes evaluated in Green channel.
#'
#' @param df A \code{data.frame} returned by \code{\link{get_control_probe_intensities}}.
#' @return An object of \code{\link[ggplot2]{ggplot}} class.
#' @noRd
.plot_control_probes_green <- function(df, jitter_width = 0.2) {
  df %>%
    dplyr::filter(Channel == "Green") %>%
    dplyr::filter(!is.na(Expected_Grn)) %>%
    dplyr::filter(Group == "Sample Dependent") %>%
    ggplot2::ggplot(ggplot2::aes(x = Expected_Grn, y = Intensity, colour = Expected_Grn)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = jitter_width) +
    ggplot2::facet_grid(Sample ~ Type, scales = "free_x") +
    ggplot2::labs(title = "Evaluate Green (Sample-Dependent Controls)", colour = "Expected") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "right"
    )
}


#' Plot control probes for normalization.
#'
#' @param df A \code{data.frame} returned by \code{\link{get_control_probe_intensities}}.
#' @return An object of \code{\link[ggplot2]{ggplot}} class.
#' @noRd
.plot_control_probes_norm <- function(df) {
  dplyr::filter(df, stringr::str_detect(Type, "^NORM_[ACGT]")) %>%
    ggplot2::ggplot(ggplot2::aes(x = Channel, y = Intensity, colour = Type)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = 0.2) +
    ggplot2::facet_grid(Sample ~ Type) +
    ggplot2::theme(legend.position = "none")
}


#' Plot sample-independent control probes.
#'
#' @param df A \code{data.frame} returned by \code{\link{get_control_probe_intensities}}.
#' @return An object of \code{\link[ggplot2]{ggplot}} class.
#' @noRd
.plot_sample_independent_control_probes <- function(df) {
  df %>%
    dplyr::filter(Channel == "Green") %>%
    dplyr::filter(!is.na(Expected_Grn)) %>%
    dplyr::filter(Group == "Sample Independent") %>%
    ggplot2::ggplot(ggplot2::aes(x = Expected_Grn, y = Intensity, colour = Expected_Grn)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = 0.2) +
    ggplot2::facet_grid(. ~ Type, scale = "free_x") +
    ggplot2::labs(title = "Green", colour = "Expected") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "right"
    ) -> p_grn
  df %>%
    dplyr::filter(Channel == "Red") %>%
    dplyr::filter(!is.na(Expected_Red)) %>%
    dplyr::filter(Group == "Sample Independent") %>%
    ggplot2::ggplot(ggplot2::aes(x = Expected_Red, y = Intensity, colour = Expected_Red)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_jitter(width = 0.2) +
    ggplot2::facet_grid(. ~ Type, scale = "free_x") +
    ggplot2::labs(title = "Red", colour = "Expected") +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "none"
    ) -> p_red
  ggpubr::ggarrange(p_red, p_grn, ncol = 2, nrow = 1) %>%
    ggpubr::annotate_figure(top = ggpubr::text_grob("Sample-Independent Controls", face = "bold"))
}


#' Get expected intensity.
#'
#' The function adds three columns expected green, expected red, a column of if
#' the probe is sample-dependent or not.
#'
#' @param x A \code{data.frame} containing column \code{ExtendedType}.
#' @return A \code{data.frame} with three additional columns \code{Expected_Grn}
#'   and \code{Expected_Red} and \code{Group}.
#' @noRd
.expected_intensity <- function(x) {
  out <- x %>%
    # Negative probes
    dplyr::mutate(Expected_Grn = ifelse(Type == "NEGATIVE", "Background", NA)) %>%
    dplyr::mutate(Expected_Red = ifelse(Type == "NEGATIVE", "Background", NA)) %>%
    # Norm probes
    dplyr::mutate(Expected_Grn = ifelse(Type %in% c("NORM_C", "NORM_G"),
                                        NA,
                                        Expected_Grn)) %>%
    dplyr::mutate(Expected_Red = ifelse(Type %in% c("NORM_A", "NORM_T"),
                                        NA,
                                        Expected_Red)) %>%
    # Extension
    dplyr::mutate(Expected_Grn = ifelse(
      ExtendedType %in% c("Extension (C)", "Extension (G)"),
      "High",
      Expected_Grn
    )) %>%
    dplyr::mutate(Expected_Red = ifelse(
      ExtendedType %in% c("Extension (A)", "Extension (T)"),
      "High",
      Expected_Red
    )) %>%
    # Non-polymorphic
    dplyr::mutate(Expected_Grn = ifelse(
      # Five extra NP (G) probes are uncertain of their use.
      # ExtendedType == "NP (C)" | stringr::str_detect(ExtendedType, "^NP \\(G\\)"),
      ExtendedType %in% c("NP (C)", "NP (G)"),
      "High",
      Expected_Grn
    )) %>%
    dplyr::mutate(Expected_Red = ifelse(
      ExtendedType %in% c("NP (A)", "NP (T)"),
      "High",
      Expected_Red
    )) %>%
    # Hybridization
    dplyr::mutate(Expected_Grn = ifelse(
      ExtendedType == "Hyb (High)",
      "High",
      Expected_Grn
    )) %>%
    dplyr::mutate(Expected_Grn = ifelse(
      ExtendedType == "Hyb (Low)",
      "Low",
      Expected_Grn
    )) %>%
    dplyr::mutate(Expected_Grn = ifelse(
      ExtendedType == "Hyb (Medium)",
      "Medium",
      Expected_Grn
    )) %>%
    # Specificity I - Green
    dplyr::mutate(Expected_Grn = ifelse(
      stringr::str_detect(ExtendedType, "^GT Mismatch [1-3] \\(PM\\)$"),
      "High",
      ifelse(
        stringr::str_detect(ExtendedType, "^GT Mismatch [1-3] \\(MM\\)$"),
        "Background",
        Expected_Grn
      )
    )) %>%
    # Specificity I - Red
    dplyr::mutate(Expected_Red = ifelse(
      stringr::str_detect(ExtendedType, "^GT Mismatch [4-6] \\(PM\\)$"),
      "High",
      ifelse(
        stringr::str_detect(ExtendedType, "^GT Mismatch [4-6] \\(MM\\)$"),
        "Background",
        Expected_Red
      )
    )) %>%
    # BS I
    dplyr::mutate(Expected_Grn = ifelse(
      stringr::str_detect(ExtendedType, "BS Conversion I-C[123]"),
      "High",
      Expected_Grn
    )) %>%
    dplyr::mutate(Expected_Grn = ifelse(
      stringr::str_detect(ExtendedType, "BS Conversion I-U[123]"),
      "Background",
      Expected_Grn
    )) %>%
    dplyr::mutate(Expected_Red = ifelse(
      stringr::str_detect(ExtendedType, "BS Conversion I-C[456]"),
      "High",
      Expected_Red
    )) %>%
    dplyr::mutate(Expected_Red = ifelse(
      stringr::str_detect(ExtendedType, "BS Conversion I-U[456]"),
      "Background",
      Expected_Red
    )) %>%
    # Specificity II
    dplyr::mutate(Expected_Red = ifelse(
      stringr::str_detect(ExtendedType, "^Specificity [1-3]"),
      "High",
      Expected_Red
    )) %>%
    # BS II
    dplyr::mutate(Expected_Red = ifelse(
      stringr::str_detect(ExtendedType, "BS Conversion II-[1-4]"),
      "High",
      Expected_Red
    )) %>%
    # Target removal
    dplyr::mutate(Expected_Grn = ifelse(
      stringr::str_detect(ExtendedType, "Target Removal [12]"),
      "Low",
      Expected_Grn
    )) %>%
    # Staining
    dplyr::mutate(Expected_Grn = ifelse(
      stringr::str_detect(ExtendedType, "Biotin \\(Bkg\\)"),
      "Background",
      Expected_Grn
    )) %>%
    dplyr::mutate(Expected_Grn = ifelse(
      stringr::str_detect(ExtendedType, "Biotin \\(High\\)"),
      "High",
      Expected_Grn
    )) %>%
    dplyr::mutate(Expected_Grn = ifelse(
      stringr::str_detect(ExtendedType, "Biotin\\(5K\\)"),
      "High",
      Expected_Grn
    )) %>%
    dplyr::mutate(Expected_Red = ifelse(
      stringr::str_detect(ExtendedType, "DNP \\(Bkg\\)"),
      "Background",
      Expected_Red
    )) %>%
    dplyr::mutate(Expected_Red = ifelse(
      stringr::str_detect(ExtendedType, "DNP \\(High\\)"),
      "High",
      Expected_Red
    )) %>%
    dplyr::mutate(Expected_Red = ifelse(
      stringr::str_detect(ExtendedType, "DNP\\(20K\\)"),
      "High",
      Expected_Red
    )) %>%
    # Control groups: sample dependent or sample independent
    dplyr::mutate(Group = .sample_dependent(Type)) %>%
    # Re-level
    dplyr::mutate(
      Expected_Grn = forcats::fct_relevel(Expected_Grn, "High", "Medium"),
      Expected_Red = forcats::fct_relevel(Expected_Red, "High")
    ) %>%
    dplyr::mutate(
      Type = forcats::fct_relevel(
        Type,
        # Sample-independent
        "STAINING",
        "EXTENSION" ,
        "TARGET REMOVAL",
        "HYBRIDIZATION",
        # Sample-dependent
        "BISULFITE CONVERSION I", "BISULFITE CONVERSION II",
        "SPECIFICITY I", "SPECIFICITY II",
        "NON-POLYMORPHIC",
        "NEGATIVE",
        "NORM_C", "NORM_G", "NORM_A", "NORM_T"
      )
    )
  out
}


#' Are controls sample independent or dependent?
#'
#' @param x A character vector.
#' @return A character vector.
#' @noRd
.sample_dependent <- function(x) {
  indep <- c("STAINING", "EXTENSION", "HYBRIDIZATION", "TARGET REMOVAL")
  dep <-
    c(
      "BISULFITE CONVERSION I",
      "BISULFITE CONVERSION II",
      "SPECIFICITY I",
      "SPECIFICITY II",
      "NON-POLYMORPHIC",
      "NORM_C", "NORM_G", "NORM_A", "NORM_T",
      "NEGATIVE"
    )
  out <- NA
  out[x %in% indep] <- "Sample Independent"
  out[x %in% dep] <- "Sample Dependent"
  out
}
