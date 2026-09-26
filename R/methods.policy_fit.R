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
  list(
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
}
