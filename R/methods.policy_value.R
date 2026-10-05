#' Print a unit-level policy value
#'
#' @param x an object returned by [get_policy_value()].
#' @param ... additional arguments. Currently ignored.
#'
#' @returns A one-row data frame with the expected outcome, its standard error
#' and confidence interval, and the expected treatment rate.
#'
#' @seealso [get_policy_value()]
#' @export
print.policy_value <- function(x, ...) {
  out <- data.frame(
    expected_outcome = x$estimate,
    std_error = x$std_error,
    conf_low = unname(x$conf_int["lower"]),
    conf_high = unname(x$conf_int["upper"]),
    expected_treatment = x$expected_treatment$overall,
    stringsAsFactors = FALSE
  )
  print(out, row.names = FALSE)
  invisible(out)
}

#' Summarize a unit-level policy value
#'
#' @param object an object returned by [get_policy_value()].
#' @param ... additional arguments. Currently ignored.
#'
#' @returns A list containing estimates, uncertainty, stabilization diagnostics,
#' and policy-probability summaries.
#'
#' @seealso [get_policy_value()]
#' @export
summary.policy_value <- function(object, ...) {
  probability <- object$policy_probability
  probability_summary <- do.call(rbind, lapply(seq_len(dim(probability)[1]),
    function(stage) {
      x <- as.numeric(probability[stage, , ])
      data.frame(stage = stage, mean = mean(x), minimum = min(x),
                 maximum = max(x), stringsAsFactors = FALSE)
    }))
  rownames(probability_summary) <- NULL
  list(
    expected_outcome = object$estimate,
    stage_outcome = object$horizon_estimate,
    variance = object$variance,
    std_error = object$std_error,
    conf_int = object$conf_int,
    expected_treatment = list(
      overall = object$expected_treatment$overall,
      by_stage = object$expected_treatment$by_stage
    ),
    stabilization = object$stabilization,
    probability = probability_summary
  )
}
