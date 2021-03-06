% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize.R
\name{dye_bias_correction}
\alias{dye_bias_correction}
\title{Dye-bias correction}
\usage{
dye_bias_correction(rgset, scale_to = 10000)
}
\arguments{
\item{rgset}{An object of \code{\link[minfi]{RGChannelSet-class}}.}

\item{scale_to}{A numeric value to scale to.}
}
\value{
An object of \code{\link[minfi]{RGChannelSet-class}}.
}
\description{
Scale the mean of normalization control probe intensities to specified value.
}
