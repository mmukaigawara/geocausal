#' Print a unit-level propensity-score model
#'
#' @param x an object returned by [get_unit_ps()].
#' @param ... additional arguments. Currently ignored.
#'
#' @returns A one-row data frame summarizing the engine, data dimensions, and
#' probability range.
#'
#' @seealso [get_unit_ps()]
#' @export
print.unit_ps <- function(x, ...) {
  out <- data.frame(
    engine = x$data_info$engine,
    time_periods = length(x$data_info$time_levels),
    units = length(x$data_info$unit_levels),
    probability_min = min(x$fitted),
    probability_max = max(x$fitted),
    stringsAsFactors = FALSE
  )
  print(out, row.names = FALSE)
  invisible(out)
}

#' Summarize a unit-level propensity-score model
#'
#' @param object an object returned by [get_unit_ps()].
#' @param ... additional arguments. Currently ignored.
#'
#' @returns A list containing the model coefficients, probability summary, and
#' balance table.
#'
#' @seealso [get_unit_ps()]
#' @export
summary.unit_ps <- function(object, ...) {
  list(
    coefficients = stats::coef(object$model),
    probability = stats::setNames(
      as.numeric(stats::quantile(object$fitted,
                                 c(0, 0.25, 0.5, 0.75, 1))),
      c("min", "q1", "median", "q3", "max")
    ),
    balance = object$balance
  )
}

#' Plot unit-level propensity scores and balance
#'
#' @param x an object returned by [get_unit_ps()].
#' @param type plot type: `"overlap"` displays propensity-score distributions
#' by observed treatment, and `"balance"` displays absolute standardized
#' differences before and after weighting.
#' @param ... additional arguments. Currently ignored.
#'
#' @returns A `ggplot` object.
#'
#' @seealso [get_unit_ps()]
#' @export
plot.unit_ps <- function(x, type = c("overlap", "balance"), ...) {
  type <- match.arg(type)
  if (type == "overlap") {
    plot_data <- data.frame(
      probability = x$fitted,
      treatment = factor(x$data_info$treatment, levels = c(0, 1),
                         labels = c("untreated", "treated"))
    )
    return(ggplot2::ggplot(
      plot_data, ggplot2::aes(x = probability, fill = treatment)
    ) +
      ggplot2::geom_density(alpha = 0.35) +
      ggplot2::labs(x = "Estimated treatment probability", y = "Density",
                    fill = "Observed treatment") +
      ggplot2::theme_bw())
  }

  if (!nrow(x$balance)) {
    stop("No balance variables are available in this object.", call. = FALSE)
  }
  plot_data <- rbind(
    data.frame(variable = x$balance$variable, sample = "unweighted",
               difference = x$balance$unweighted),
    data.frame(variable = x$balance$variable, sample = "weighted",
               difference = x$balance$weighted)
  )
  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = difference, y = stats::reorder(variable, difference),
                 color = sample)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Absolute standardized difference", y = NULL,
                  color = NULL) +
    ggplot2::theme_bw()
}
