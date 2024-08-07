% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize}
\alias{normalize}
\title{Normalization wrapper.}
\usage{
normalize(
  rgset,
  norm_method = c("raw", "illumina", "swan", "quantile", "noob", "funnorm", "yamat",
    "dkfz", "methylcnv"),
  map_to_genome = TRUE,
  ...
)
}
\arguments{
\item{rgset}{An object of \code{\link[minfi]{RGChannelSet-class}}.}

\item{norm_method}{A character scalar of method, including raw, illumina,
swan, quantile, noob, funnorm, yamat, dkfz, methylcnv. Default to "raw".}

\item{map_to_genome}{A logical scalar if an object of \code{\link[minfi]{MethylSet-class}}
or \code{\link[minfi]{GenomicMethylSet-class}} will be returned. Default
to TRUE.}

\item{...}{Any arguments for individual normalization method.}
}
\value{
An object of \code{\link[minfi]{GenomicMethylSet-class}} or
  \code{\link[minfi]{MethylSet-class}}. If \code{norm_method} is quantile,
  an object of \code{\link[minfi]{GenomicRatioSet-class}} is returned.
}
\description{
Normalize an object of \code{\link[minfi]{RGChannelSet-class}} with different
methods.
}
