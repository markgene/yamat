% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gender.R
\name{get_gender}
\alias{get_gender}
\title{Get gender information to \code{RGChannelSet}.}
\usage{
get_gender(rgset, norm_method = "raw")
}
\arguments{
\item{rgset}{An object of \code{\link[minfi]{RGChannelSet-class}}.}

\item{norm_method}{A character scalar of method passed to \code{\link{normalize}},
including raw, illumina, swan, quantile, noob, funnorm, yamat, dkfz.
Default to "raw".}
}
\value{
A \code{DataFrame} returned by \code{\link[minfi]{pData}} with
  additional columns \code{predictedSex} (a character with values M and F),
  \code{xMed} and \code{yMed}, which are the chip-wide medians of
  measurements on the two sex chromosomes.
}
\description{
Get gender information to \code{RGChannelSet}.
}
\details{
We need to convert \code{RGChannelSet} object into \code{GenomicMethylSet}
  object to \code{\link[minfi]{getSex}}. We create the \code{MethylSet}
  without normalization using \code{\link[minfi]{preprocessRaw}}.
}
