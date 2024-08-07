% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc-control-probes.R
\name{get_restoration_qc}
\alias{get_restoration_qc}
\title{Get QC metric of restoration probe.}
\usage{
get_restoration_qc(ctrl_probes, background_offset = 3000)
}
\arguments{
\item{ctrl_probes}{The object returned by \code{\link{control_probe_intensities}}.}

\item{background_offset}{background correction offset. Default to 3000.}
}
\value{
A data frame of QC metrics.
}
\description{
Get QC metric of restoration probe.
}
