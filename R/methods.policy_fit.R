#' Print a learned unit-level policy
#'
#' @param x an object returned by [get_policy()].
#' @param ... additional arguments. Currently ignored.
#'
#' @returns A data frame containing the selected order, expected outcome,
#' confidence interval, and expected treatment rate at each fitted horizon.
#'
#' @seealso [get_policy()]
#' @export
print.policy_fit <- function(x, ...) {
  out <- .policy_fit_table(x)
  print(out, row.names = FALSE)
  invisible(out)
}

.policy_fit_table <- function(x) {
  data.frame(
    horizon = seq_along(x$value),
    order = x$order,
    expected_outcome = vapply(x$value, function(value) value$estimate, numeric(1)),
    conf_low = vapply(x$value, function(value) value$conf_int[["lower"]], numeric(1)),
    conf_high = vapply(x$value, function(value) value$conf_int[["upper"]], numeric(1)),
    expected_treatment = vapply(
      x$value, function(value) value$expected_treatment$overall, numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}

#' Summarize a learned unit-level policy
#'
#' @param object an object returned by [get_policy()].
#' @param ... additional arguments. Currently ignored.
#'
#' @returns A list containing learned coefficients, fitted-horizon results,
#' order-test details, expected-treatment summaries, and policy-probability
#' summaries.
#'
#' @seealso [get_policy()]
#' @export
summary.policy_fit <- function(object, ...) {
  out <- list(
    coefficients = object$coefficients,
    result = .policy_fit_table(object),
    order_test = object$order_test,
    expected_treatment = do.call(rbind, lapply(
      seq_along(object$value), function(horizon) {
        estimate <- object$value[[horizon]]$expected_treatment$by_stage
        data.frame(
          horizon = horizon,
          stage = seq_along(estimate),
          estimate = as.numeric(estimate),
          stringsAsFactors = FALSE
        )
      }
    )),
    probability = object$probability_summary
  )
  if (!is.null(object$multistart)) out$multistart <- object$multistart
  out
}

#' Plot policy values or treatment allocations
#'
#' @param x an object returned by [get_policy()].
#' @param type `"value"` plots values and evaluation intervals by horizon.
#'   `"allocation"` compares observed and learned treatment probabilities on a map.
#' @param districts an `sf` object with one polygon per unit. Required for maps.
#' @param district_id name of the unit identifier column in `districts`.
#'   Defaults to the unit column used by [get_policy()].
#' @param data the original time-by-unit data passed to [get_policy()].
#'   Required for allocation maps.
#' @param horizon fitted horizon to map. Defaults to the final horizon.
#' @param stage stages to average for the map. `NULL` averages all stages.
#' @param ... additional arguments. Currently ignored.
#'
#' @returns A ggplot object. Allocation maps show observed treatment frequency,
#'   mean conditional policy probability, and their difference in percentage points.
#' @details Allocation maps use the same observed histories and evaluation windows
#'   for both quantities. They do not simulate histories under the learned policy.
#'   Polygons outside the fitted sample are grey. Identifiers are matched by name.
#' @seealso [get_policy()]
#' @export
plot.policy_fit <- function(x, type = c("value", "allocation"), districts = NULL,
                            district_id = x$specification$unit, data = NULL,
                            horizon = length(x$value), stage = NULL, ...) {
  type <- match.arg(type)
  if (type == "value") {
    return(ggplot2::ggplot(.policy_fit_table(x),
      ggplot2::aes(x = horizon, y = expected_outcome)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = conf_low, ymax = conf_high), width = .1) +
      ggplot2::geom_point() +
      ggplot2::scale_x_continuous(breaks = seq_along(x$value)) +
      ggplot2::labs(x = "Horizon", y = "Estimated policy value") +
      ggplot2::theme_classic())
  }
  map_data <- .policy_allocation_data(x, districts, district_id, data, horizon, stage)
  candidate <- any(x$convergence$code[seq_len(horizon)] != 0)
  titles <- c("Observed treatment", if (candidate) "Policy candidate" else "Learned policy",
    "Difference")
  columns <- c("observed", "learned", "difference")
  maps <- lapply(seq_along(columns), function(i) {
    map_data$allocation <- map_data[[columns[i]]]
    p <- ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = allocation), color = "grey65", linewidth = .15) +
      ggplot2::labs(title = titles[i]) + ggplot2::theme_void() +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 12), legend.position = "bottom")
    if (i == 3L) {
      p + ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
        midpoint = 0, limits = c(-100, 100), na.value = "grey90", name = "Percentage points")
    } else {
      p + ggplot2::scale_fill_viridis_c(limits = c(0, 1), na.value = "grey90", name = "Probability")
    }
  })
  probability_maps <- ggpubr::ggarrange(maps[[1]], maps[[2]],
    common.legend = TRUE, legend = "bottom")
  ggpubr::ggarrange(probability_maps, maps[[3]], widths = c(2, 1))
}

.policy_allocation_data <- function(x, districts, district_id, data, horizon, stage) {
  if (!inherits(districts, "sf") || !is.character(district_id) ||
      length(district_id) != 1L || !district_id %in% names(districts)) {
    stop("Supply `districts` as an sf object and name its identifier with `district_id`.", call. = FALSE)
  }
  ids <- as.character(districts[[district_id]])
  if (anyNA(ids) || anyDuplicated(ids)) {
    stop("District identifiers must be nonmissing and unique.", call. = FALSE)
  }
  if (!is.numeric(horizon) || length(horizon) != 1L || !is.finite(horizon) ||
      horizon != as.integer(horizon) || horizon < 1 || horizon > length(x$value)) {
    stop("`horizon` must name a fitted horizon.", call. = FALSE)
  }
  stages <- if (is.null(stage)) seq_len(horizon) else stage
  if (!is.numeric(stages) || !length(stages) || anyNA(stages) ||
      any(!is.finite(stages)) || any(!stages %in% seq_len(horizon)) || anyDuplicated(stages)) {
    stop("`stage` must contain distinct stage numbers within the fitted horizon.", call. = FALSE)
  }
  value <- x$value[[horizon]]
  probability <- value$policy_probability
  units <- dimnames(probability)[[3]]
  panel <- .prepare_unit_data(data, x$specification$time, x$specification$unit)
  if (!setequal(panel$unit_levels, units) ||
      !identical(as.character(panel$time_levels), as.character(value$specification$time_levels))) {
    stop("`data` must contain the same times and units as the fitted policy.", call. = FALSE)
  }
  observed <- .data_matrix(panel$data[[x$specification$treatment]], panel,
    x$specification$treatment, binary = TRUE)[, units, drop = FALSE]
  observed_windows <- lapply(stages, function(s) {
    rows <- (horizon - s + 1L):(nrow(observed) - s + 1L)
    observed[rows, , drop = FALSE]
  })
  observed_rate <- colMeans(do.call(rbind, observed_windows))
  learned_rate <- apply(probability[stages, , , drop = FALSE], 3, mean)
  index <- match(ids, units)
  if (!all(units %in% ids)) {
    stop("`districts` must include every unit in the fitted policy.", call. = FALSE)
  }
  districts$observed <- unname(observed_rate[index])
  districts$learned <- unname(learned_rate[index])
  districts$difference <- 100 * (districts$learned - districts$observed)
  districts
}
