# Normalization.


#' Normalization wrapper.
#'
#' Normalize an object of \code{\link[minfi]{RGChannelSet-class}} with different
#' methods.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param norm_method A character scalar of method, including raw, illumina,
#'   swan, quantile, noob, funnorm, yamat, dkfz. Default to "raw".
#' @param map_to_genome A logical scalar if an object of \code{\link[minfi]{MethylSet-class}}
#'   or \code{\link[minfi]{GenomicMethylSet-class}} will be returned. Default
#'   to TRUE.
#' @param ... Any arguments for individual normalization method.
#' @return An object of \code{\link[minfi]{GenomicMethylSet-class}} or
#'   \code{\link[minfi]{MethylSet-class}}. If \code{norm_method} is quantile,
#'   an object of \code{\link[minfi]{GenomicRatioSet-class}} is returned.
#' @export
normalize <- function(rgset,
                      norm_method = c("raw",
                                      "illumina",
                                      "swan",
                                      "quantile",
                                      "noob",
                                      "funnorm",
                                      "yamat",
                                      "dkfz"),
                      map_to_genome = TRUE,
                      ...)  {
  norm_method <- match.arg(norm_method)
  switch(
    norm_method,
    illumina = normalize.illumina(rgset, ...),
    raw      = normalize.raw(rgset, map_to_genome = map_to_genome),
    swan     = normalize.swan(rgset, map_to_genome = map_to_genome, ...),
    quantile = normalize.quantile(rgset, ...),
    noob     = normalize.noob(rgset, map_to_genome = map_to_genome, ...),
    funnorm  = normalize.funnorm(rgset, ...),
    dkfz     = normalize.dkfz(rgset, map_to_genome = map_to_genome),
    yamap    = normalize.yamap(rgset, map_to_genome = map_to_genome, ...)
  )
}


#' Normalization Raw (no normalization, no background correction)
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param map_to_genome A logical scalar if an object of \code{\link[minfi]{MethylSet-class}}
#'   or \code{\link[minfi]{GenomicMethylSet-class}} will be returned. Default
#'   to TRUE.
#' @return An object of \code{\link[minfi]{GenomicMethylSet-class}} or
#'   \code{\link[minfi]{MethylSet-class}}.
#' @noRd
normalize.raw <- function(rgset, map_to_genome = TRUE) {
  if (map_to_genome) {
    minfi::preprocessRaw(rgset) %>%
      minfi::mapToGenome()
  } else {
    minfi::preprocessRaw(rgset)
  }
}


#' Normalization Illumina
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param map_to_genome A logical scalar if an object of \code{\link[minfi]{MethylSet-class}}
#'   or \code{\link[minfi]{GenomicMethylSet-class}} will be returned. Default
#'   to TRUE.
#' @param ... Any arguments for \code{\link[minfi]{preprocessIllumina}}.
#' @return An object of \code{\link[minfi]{GenomicMethylSet-class}} or
#'   \code{\link[minfi]{MethylSet-class}}.
#' @noRd
normalize.illumina <- function(rgset, map_to_genome = TRUE, ...) {
  if (map_to_genome) {
    minfi::preprocessIllumina(rgset, ...) %>%
      minfi::mapToGenome()
  } else {
    minfi::preprocessIllumina(rgset, ...)
  }
}


#' Normalization SWAN
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param map_to_genome A logical scalar if an object of \code{\link[minfi]{MethylSet-class}}
#'   or \code{\link[minfi]{GenomicMethylSet-class}} will be returned. Default
#'   to TRUE.
#' @param ... Any arguments for \code{\link[minfi]{preprocessSWAN}}.
#' @return An object of \code{\link[minfi]{GenomicMethylSet-class}} or
#'   \code{\link[minfi]{MethylSet-class}}.
#' @noRd
normalize.swan <- function(rgset, map_to_genome = TRUE, ...) {
  if (map_to_genome) {
    minfi::preprocessSWAN(rgset, ...) %>%
      minfi::mapToGenome()
  } else {
    minfi::preprocessSWAN(rgset, ...)
  }
}


#' Normalization quantile
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param ... Any arguments for \code{\link[minfi]{preprocessQuantile}}.
#' @return An object of \code{\link[minfi]{GenomicRatioSet-class}}.
#' @noRd
normalize.quantile <- function(rgset, ...) {
  minfi::preprocessQuantile(rgset, ...)
}


#' Normalization noob
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param map_to_genome A logical scalar if an object of \code{\link[minfi]{MethylSet-class}}
#'   or \code{\link[minfi]{GenomicMethylSet-class}} will be returned. Default
#'   to TRUE.
#' @param ... Any arguments for \code{\link[minfi]{preprocessNoob}}.
#' @return An object of \code{\link[minfi]{GenomicMethylSet-class}} or
#'   \code{\link[minfi]{MethylSet-class}}.
#' @noRd
normalize.noob <- function(rgset, map_to_genome = TRUE, ...) {
  if (map_to_genome) {
    minfi::preprocessNoob(rgset, ...) %>%
      minfi::mapToGenome()
  } else {
    minfi::preprocessNoob(rgset, ...)
  }
}


#' Normalization Funnorm
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param ... Any arguments for \code{\link[minfi]{preprocessFunnorm}}.
#' @return An object of \code{\link[minfi]{GenomicMethylSet-class}}.
#' @noRd
normalize.funnorm <- function(rgset, map_to_genome = TRUE, ...) {
  minfi::preprocessFunnorm(rgset, ratioConvert = FALSE, ...)
}


#' Normalization method yamat.
#'
#' Yamat is a wrapper of \code{\link[minfi]{preprocessIllumina}} which normalize
#' the samples individually.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param map_to_genome A logical scalar if an object of \code{\link[minfi]{MethylSet-class}}
#'   or \code{\link[minfi]{GenomicMethylSet-class}} will be returned. Default
#'   to TRUE.
#' @param ... Any arguments for \code{\link[minfi]{preprocessIllumina}}.
#' @return An object of \code{\link[minfi]{GenomicMethylSet-class}} or
#'   \code{\link[minfi]{MethylSet-class}}.
normalize.yamat <- function(rgset, map_to_genome = TRUE, ...) {
  mset_list <- lapply(
    seq(ncol(rgset)),
    function(i) {
      minfi::preprocessIllumina(rgset[, i], ...)
    }
  )
  mset <- do.call(minfi::combine, mset_list)
  mset@preprocessMethod <-
    c(
      rg.norm = "yamap",
      minfi = as.character(packageVersion("minfi")),
      manifest = as.character(packageVersion(minfi:::.getManifestString(rgset@annotation)))
    )
  if (map_to_genome)
    minfi::mapToGenome(mset)
  else
    mset
}


#' Normalization DKFZ.
#'
#' DKFZ implement the normalization method used in the paper
#' \href{https://www.nature.com/articles/nature26000}{Capper et al. DNA methylation-based classification of central nervous system tumours. Nature (2018)}.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param map_to_genome A logical scalar if an object of \code{\link[minfi]{MethylSet-class}}
#'   or \code{\link[minfi]{GenomicMethylSet-class}} will be returned. Default
#'   to TRUE.
#' @return An object of \code{\link[minfi]{GenomicMethylSet-class}} or
#'   \code{\link[minfi]{MethylSet-class}}.
normalize.dkfz <- function(rgset, map_to_genome = TRUE) {
  mset_list <- lapply(
    seq(ncol(rgset)),
    function(i) {
      rg <- minfi::bgcorrect.illumina(rgset[, i])
      rg <- dye_bias_correction(rg)
      mset <- minfi::preprocessRaw(rg)
    }
  )
  mset <- do.call(minfi::combine, mset_list)
  mset@preprocessMethod <-
    c(
      rg.norm = "dkfz",
      minfi = as.character(packageVersion("minfi")),
      manifest = as.character(packageVersion(minfi:::.getManifestString(rgset@annotation)))
    )
  if (map_to_genome)
    minfi::mapToGenome(mset)
  else
    mset
}


#' Dye-bias correction
#'
#' Scale the mean of normalization control probe intensities to specified value.
#'
#' @param rgset An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @param scale_to A numeric value to scale to.
#' @return An object of \code{\link[minfi]{RGChannelSet-class}}.
#' @export
dye_bias_correction <- function(rgset, scale_to = 10000) {
  # Borrow from minfi::normalize.illumina.control()
  Green <- getGreen(rgset)
  Red <- getRed(rgset)
  if (minfi:::.is450k(rgset) || minfi:::.isEPIC(rgset)) {
    AT.controls <-
      minfi::getControlAddress(object = rgset,
                               controlType = c("NORM_A",
                                               "NORM_T"))
    CG.controls <-
      minfi::getControlAddress(object = rgset,
                               controlType = c("NORM_C",
                                               "NORM_G"))
  }
  if (minfi:::.is27k(rgset)) {
    AT.controls <-
      minfi::getControlAddress(object = rgset, controlType = "Normalization-Red")
    CG.controls <-
      minfi::getControlAddress(object = rgset, controlType = "Normalization-Green")
  }
  Green.avg <- colMeans2(Green, rows = match(CG.controls, rownames(Green)))
  Red.avg <- colMeans2(Red, rows = match(AT.controls, rownames(Red)))
  Green.factor <- scale_to/Green.avg
  Red.factor <- scale_to/Red.avg
  Green <- sweep(Green, 2, FUN = "*", Green.factor)
  Red <- sweep(Red, 2, FUN = "*", Red.factor)
  assay(rgset, "Green") <- Green
  assay(rgset, "Red") <- Red
  rgset
}