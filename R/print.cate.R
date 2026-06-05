#' Print results
#'
#' @description Print method for objects of class `cate`, returned by [get_cate()].
#' Builds and returns a data frame of the estimated CATE values at the chosen
#' evaluation points, together with their 90% and 95% confidence intervals.
#'
#' @param x an object of class `cate`, typically the output of [get_cate()].
#' @param ... additional arguments. Currently ignored.
#'
#' @details Currently, observed densities (class: obs), estimates (class: est) and heterogeneity estimates (class: cate) are supported by this function.
#'
#' @returns A data frame with one row per evaluation value, containing the
#' evaluation value, the point estimate, and the lower/upper bounds of the 90%
#' and 95% confidence intervals (rounded to five digits).
#'
#' @seealso [get_cate()]
#'
#' @export
print.cate <- function(x,...) {

  cate <- x
  result_values <- data.frame(
    values = cate$specification$eval_values,
    point_estimate = cate$est_eval,
    upper_95 = cate$est_eval + 1.96 * sqrt(diag(cate$V_eval)),
    lower_95 = cate$est_eval - 1.96 * sqrt(diag(cate$V_eval)),
    upper_90 = cate$est_eval + 1.645 * sqrt(diag(cate$V_eval)),
    lower_90 = cate$est_eval - 1.645 * sqrt(diag(cate$V_eval))
  )
  result_values <- round(result_values,digits = 5)

  return(result_values)
}
