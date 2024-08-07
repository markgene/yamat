# Beta value distribution plots.

#' Beta value distribution histgram fill by sample.
#'
#' @param x An object of minfi classes.
#' @param output_file output plot file.
#' @param ... arguments passed to \code{\link[ggplot2]{ggsave}}.
#' @returns A ggplot object of the plot.
#' @export
plot_beta_value_histogram_fill_by_sample <- function (x, output_file, ...) {
  if (missing(x))
    stop("x is required.")
  if (missing(output_file))
    stop("output_file is required.")
  if (is(x, "RGChannelSet") || is(x, "MethylSet")) {
    b <- getBeta(x)
  } else if (is(x, "matrix")) {
    b <- x
  } else {
    stop("argument 'dat' must be an 'RGChannelSet', a 'MethylSet' or ",
         "matrix.")
  }
  pal <- ggsci_pal_d(ggsci_pal_name = "jco", pal_size = ncol(x))
  as.data.frame(b) %>%
    tidyr::pivot_longer(.,
                        cols = everything(),
                        names_to = "Sample",
                        values_to = "Beta") %>%
    ggplot2::ggplot(., ggplot2::aes(x = Beta, fill = Sample)) +
    ggplot2::geom_histogram(binwidth = 0.01, boundary = 0) +
    ggplot2::scale_fill_manual(values = pal, drop = FALSE) +
    ggplot2::facet_wrap(ggplot2::vars(Sample), ncol = 4, scales = "fixed") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(x = "\u03b2 value", y = "No. of Probes", fill = "\u03b2 value") +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      legend.position = "right",
      # legend.key.height = unit(0.5, units = "in"),
      # legend.key.width = unit(0.3, units = "in"),
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 1),
      strip.text.y.left = ggplot2::element_text(angle = 0, size = 10),
      strip.text.x = ggplot2::element_text(size = 10)
    ) +
    patchwork::plot_annotation(
      title = glue::glue("\u03b2 value distribution"),
      subtitle = glue::glue(
        "A typical sample is expect to have a bi-modal distribution with two peaks close to 0 (unmeth) and 1 (meth), because we expect
        most of the loci should have a certain methylation status. The beta values are computed without any normalization.
        NOTE: this is a typical sample. But a sample is possibly to have different distribution when it passes. For example, a positive
        control sample may have a unimodal with a peak close to 1, and a negative control sample may have a unimodal with a peak close to 0.
        "
      ),
      theme = ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = 9))
    ) -> p
  ggplot2::ggsave(filename = output_file, plot = p, ...)
  invisible(p)
}
