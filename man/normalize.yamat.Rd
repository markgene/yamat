% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{normalize.yamat}
\alias{normalize.yamat}
\title{Normalization method yamat.}
\usage{
normalize.yamat(rgset, map_to_genome = TRUE, ...)
}
\arguments{
\item{rgset}{An object of \code{\link[minfi]{RGChannelSet-class}}.}

\item{map_to_genome}{A logical scalar if an object of \code{\link[minfi]{MethylSet-class}}
or \code{\link[minfi]{GenomicMethylSet-class}} will be returned. Default
to TRUE.}

\item{...}{Any arguments for \code{\link[minfi]{preprocessIllumina}}.}
}
\value{
An object of \code{\link[minfi]{GenomicMethylSet-class}} or
  \code{\link[minfi]{MethylSet-class}}.
}
\description{
Yamat is a wrapper of \code{\link[minfi]{preprocessIllumina}} which normalize
the samples individually.
}
