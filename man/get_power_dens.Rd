% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_power_dens.R
\name{get_power_dens}
\alias{get_power_dens}
\title{Get power densities}
\usage{
get_power_dens(target_dens, priorities, window)
}
\arguments{
\item{target_dens}{list of target densities}

\item{priorities}{vector of priorities for each of target densities}

\item{window}{owin object}
}
\value{
list of an im object and a ggplot object of power densities
}
\description{
`get_power_dens()` takes the target densities and their priorities
and returns a power density.
}
