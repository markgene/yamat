# Deal with Batch Effect.

#' Remove batch effect.
#'
#' @param x An object of \code{\link[minfi]{MethylSet-class}} or
#'   \code{\link[minfi]{GenomicMethylSet-class}} if \code{method} is "mum".
#'   An object of \code{\link[minfi]{RatioSet-class}} or
#'   \code{\link[minfi]{GenomicRatioSet-class}} if \code{method} is "cn" or
#'   "beta".
#' @param batch A character scalar of the column name of interested batch in
#'   phenotype \code{data.frame}. Default to \code{NULL}.
#' @param batch2 A character scalar of the column name of interested batch2 in
#'   phenotype \code{data.frame}. Default to \code{NULL}.
#' @param method A character scalar of methods, including "mum" (methylation
#'   and unmethylation separately), "cn" (copy number), "beta" (beta-value).
#'   Default to "mum".
#' @param offset The offset is chosen to avoid log2 transform on zeros.
#'   Default to 1. Only valid for "mum" method.
#' @param verbose A logical scalar. Default to TRUE.
#' @param ... Other arguments can be passed to \code{\link[limma]{removeBatchEffect}}.
#' @return An object of \code{\link[minfi]{GenomicMethylSet-class}}.
#' @details Batch effect is removed by calling \code{\link[limma]{removeBatchEffect}}.
#'   The methylation and unmethylation signals are log2 transformed, removed
#'   batch effect, and transformed back.
#' @export
remove_batch_effect <-
  function(x,
           batch = NULL,
           batch2 = NULL,
           method = c("mum", "cn", "beta"),
           offset = 1,
           verbose = TRUE,
           ...) {
    pheno_df <- as.data.frame(minfi::pData(x))
    if (is.null(batch) & is.null(batch2)) {
      if (verbose)
        message("Both batch and batch2 are null. Return the input object.")
      return(x)
    }
    if (!is.null(batch)) {
      if (!batch %in% colnames(pheno_df))
        stop("batch is not present.")
      batch <- factor(pheno_df[, batch])
    }
    if (!is.null(batch2)) {
      if (!batch2 %in% colnames(pheno_df))
        stop("batch2 is not present.")
      batch2 <- factor(pheno_df[, batch2])
    }
    m <- match.arg(method)
    switch (m,
      mum = remove_batch_effect_mum(x, batch, batch2, offset = offset, ...),
      cn = remove_batch_effect_cn(x, batch, batch2, ...),
      beta = remove_batch_effect_beta(x, batch, batch2, ...)
    )
  }


#' Remove batch effect: methylation and unmethylation signals.
#'
#' Remove batch effect for methylation and unmethylation signals (log2
#' transformed) separately. See \href{https://www.ncbi.nlm.nih.gov/pubmed/29539639}{Capper D. DNA methylation-based classification of central nervous system tumours. Nature (2018)}.
#'
#' @param x An object of \code{\link[minfi]{MethylSet-class}} or
#'   \code{\link[minfi]{GenomicMethylSet-class}}.
#' @param batch factor or vector indicating batches.
#' @param batch2 optional factor or vector indicating a second series of
#'   batches.
#' @param offset The offset is chosen to avoid log2 transform on zeros. Default
#'   to 1.
#' @param ... Other arguments can be passed to \code{\link[limma]{removeBatchEffect}}.
#' @return An object of argument \code{x} class.
#' @details Batch effect is removed by calling \code{\link[limma]{removeBatchEffect}}.
#'   The methylation and unmethylation signals are log2 transformed, removed
#'   batch effect, and transformed back.
remove_batch_effect_mum <-
  function(x,
           batch = NULL,
           batch2 = NULL,
           offset = 1,
           ...) {
    if (!inherits(x, "GenomicMethylSet") & !inherits(x, "MethylSet"))
      stop("x is either MethylSet or GenomicMethylSet class.")
    # Meth
    minfi::getMeth(x) %>%
      ifelse(. < offset, offset, .) %>%
      log2() %>%
      limma::removeBatchEffect(x = .,
                               batch = batch,
                               batch2 = batch2,
                               ...) -> meth_log2_rm_batch
    # Unmeth
    minfi::getUnmeth(x) %>%
      ifelse(. < offset, offset, .) %>%
      log2() %>%
      limma::removeBatchEffect(x = .,
                               batch = batch,
                               batch2 = batch2,
                               ...) -> unmeth_log2_rm_batch
    x_rm_batch <- x
    assay(x_rm_batch, "Meth") <- 2 ^ meth_log2_rm_batch
    assay(x_rm_batch, "Unmeth") <- 2 ^ unmeth_log2_rm_batch
    paste0(
      x_rm_batch@preprocessMethod["rg.norm"],
      ". Corrected batch1=",
      batch,
      ", batch2=",
      batch2,
      " (mum)."
    ) %>%
      unique() %>%
      head(., n = 1) -> x_rm_batch@preprocessMethod["rg.norm"]
    x_rm_batch
  }


#' Remove batch effect: copy number
#'
#' Remove batch effect for copy number: log2 transformed sum of methylation and
#' unmethylation signals.
#'
#' @param x An object of \code{\link[minfi]{RatioSet-class}} or
#'   \code{\link[minfi]{GenomicRatioSet-class}}.
#' @param batch factor or vector indicating batches.
#' @param batch2 optional factor or vector indicating a second series of
#'   batches.
#' @param offset The offset is chosen to avoid log2 transform on zeros. Default
#'   to 1.
#' @param ... Other arguments can be passed to \code{\link[limma]{removeBatchEffect}}.
#' @return An object of argument \code{x} class.
#' @details Batch effect is removed by calling \code{\link[limma]{removeBatchEffect}}.
#'   This is only useful when methylation and unmethylation signals are not
#'   available, such as in objects of \code{\link[minfi]{GenomicRatioSet-class}}.
remove_batch_effect_cn <-
  function(x,
           batch = NULL,
           batch2 = NULL,
           offset = 1,
           ...) {
    if (!inherits(x, "GenomicRatioSet") & !inherits(x, "RatioSet"))
      stop("x is either RatioSet or GenomicRatioSet class.")
    x_rm_batch <- x
    assay(x_rm_batch, "CN") <-
      limma::removeBatchEffect(x = minfi::getCN(x),
                               batch = batch,
                               batch2 = batch2,
                               ...)
    paste0(
      x_rm_batch@preprocessMethod["rg.norm"],
      ". Corrected batch1=",
      batch,
      ", batch2=",
      batch2,
      " (cn)."
    ) %>%
      unique() %>%
      head(., n = 1) -> x_rm_batch@preprocessMethod["rg.norm"]
    x_rm_batch
  }


#' Remove batch effect: beta value.
#'
#' Remove batch effect for beta value.
#'
#' @param x An object of \code{\link[minfi]{RatioSet-class}} or
#'   \code{\link[minfi]{GenomicRatioSet-class}}.
#' @param batch factor or vector indicating batches.
#' @param batch2 optional factor or vector indicating a second series of
#'   batches.
#' @param ... Other arguments can be passed to \code{\link[limma]{removeBatchEffect}}.
#' @return An object of argument \code{x} class.
#' @details Batch effect is removed by calling \code{\link[limma]{removeBatchEffect}}.
#'   This is only useful when methylation and unmethylation signals are not
#'   available, such as in objects of \code{\link[minfi]{GenomicRatioSet-class}}.
remove_batch_effect_beta <-
  function(x,
           batch = NULL,
           batch2 = NULL,
           offset = 100,
           ...) {
    if (!inherits(x, "GenomicRatioSet") & !inherits(x, "RatioSet"))
      stop("x is either RatioSet or GenomicRatioSet class.")
    x_rm_batch <- x
    assay(x_rm_batch, "Beta") <-
      limma::removeBatchEffect(x = minfi::getBeta(x),
                               batch = batch,
                               batch2 = batch2,
                               ...)
    paste0(
      x_rm_batch@preprocessMethod["rg.norm"],
      ". Corrected batch1=",
      batch,
      ", batch2=",
      batch2,
      " (beta)."
    ) %>%
      unique() %>%
      head(., n = 1) -> x_rm_batch@preprocessMethod["rg.norm"]
    x_rm_batch
  }
