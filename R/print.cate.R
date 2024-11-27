#' Print results
#'
#' @description `print` functions take the output and print the summary of it.
#'
#' @param x an output object
#' @param ... arguments passed on to the function
#'
#' @details Currently, observed densities (class: obs), estimates (class: est) and heterogeneity estimates (class: cate) are supported by this function.
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
