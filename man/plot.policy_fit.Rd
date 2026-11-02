\name{plot.policy_fit}
\alias{plot.policy_fit}
\title{Plot policy values or treatment allocations}
\usage{
\method{plot}{policy_fit}(x, type = c("value", "allocation"),
  districts = NULL, district_id = x$specification$unit, data = NULL,
  horizon = length(x$value), stage = NULL, ...)
}
\arguments{
\item{x}{An object returned by \code{get_policy()}.}
\item{type}{\code{"value"} plots values and evaluation intervals by horizon.
\code{"allocation"} compares observed and learned treatment probabilities on a map.}
\item{districts}{An \code{sf} object with one polygon per unit. Required for maps.}
\item{district_id}{Name of the unit identifier column in \code{districts}.
Defaults to the unit column used by \code{get_policy()}.}
\item{data}{The original time-by-unit data passed to \code{get_policy()}.
Required for allocation maps.}
\item{horizon}{Fitted horizon to map. Defaults to the final horizon.}
\item{stage}{Stages to average for the map. \code{NULL} averages all stages.}
\item{...}{Additional arguments. Currently ignored.}
}
\value{A ggplot object. Allocation maps show observed treatment frequency,
mean conditional policy probability, and their difference in percentage points.}
\description{Plot policy values or compare observed and learned treatment allocations.}
\details{Allocation maps use the same observed histories and evaluation windows
for both quantities. They do not simulate histories under the learned policy.
Polygons outside the fitted sample are grey. Identifiers are matched by name.}
\seealso{\code{\link{get_policy}}}
