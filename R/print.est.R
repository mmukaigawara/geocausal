#' Print results
#'
#' @description `print` functions take the output and print the summary of it.
#'
#' @param x an output object
#' @param ... arguments passed on to the function
#'
#' @details Currently, observed densities (class: obs) and estimates (class: est) are supported by this function.
#'
#' @export
print.est <- function(x, ...) {

  estimates <- x

  result <- data.frame(
    window = seq(1, length(estimates$windows)),
    point_estimate = estimates$est_causal,
    upper_95 = estimates$est_causal + 1.96 * sqrt(estimates$var_causal),
    lower_95 = estimates$est_causal - 1.96 * sqrt(estimates$var_causal),
    upper_90 = estimates$est_causal + 1.645 * sqrt(estimates$var_causal),
    lower_90 = estimates$est_causal - 1.645 * sqrt(estimates$var_causal)
  )

  return(result)

}
