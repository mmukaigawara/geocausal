#' Summarize results
#'
#' @description Summary method for objects of class `cate`, returned by [get_cate()].
#' Summarizes the estimated regression coefficients, the CATE evaluated at the
#' chosen values (both with 90% and 95% confidence intervals), and a chi-square
#' test of whether at least one non-intercept coefficient differs from zero.
#'
#' @param object an object of class `cate`, typically the output of [get_cate()].
#' @param ... additional arguments. Currently ignored.
#' @param significance_level Numeric scalar between 0 and 1, inclusive, representing the significance level for the chi-square test. The test is used to determine whether at least one of the coefficients (except the intercept) is not equal to 0. Default is 0.05
#'
#' @details Currently, observed densities (class: obs), estimates (class: est) and heterogeneity estimates (class: cate) are supported by this function.
#'
#' @returns A named list with three elements: `result_betas`, a data frame of the
#' estimated regression coefficients with 90%/95% confidence intervals;
#' `result_values`, a data frame of the CATE evaluated at the chosen values with
#' 90%/95% confidence intervals; and `chisq_test`, a data frame with the
#' chi-square statistic, its p-value, the significance level, and the test
#' decision.
#'
#' @seealso [get_cate()]
#'
#' @export
summary.cate <- function(object,..., significance_level = 0.05) {
  
  cate <- object
  #1. Summary for the regression coefficients
  intercept <- cate$specification$intercept
  if(intercept==0){
    basis <-  paste0('basis_',1:length(cate$est_beta))
  }else{
    basis <-  c("intercept",paste0('basis_',1:(length(cate$est_beta)-1)))
  }
  
  result_betas <- data.frame(
    point_estimate = cate$est_beta,
    upper_95 = cate$est_beta + 1.96 * sqrt(diag(cate$V_beta)),
    lower_95 = cate$est_beta - 1.96 * sqrt(diag(cate$V_beta)),
    upper_90 = cate$est_beta + 1.645 * sqrt(diag(cate$V_beta)),
    lower_90 = cate$est_beta - 1.645 * sqrt(diag(cate$V_beta))
  )
  row.names(result_betas) <-  basis
  result_betas <-  round(result_betas, digits = 5)
  
  #2. Summary for the cate evaluted at chosen values
  result_values <- data.frame(
    values = cate$specification$eval_values,
    point_estimate = cate$est_eval,
    upper_95 = cate$est_eval + 1.96 * sqrt(diag(cate$V_eval)),
    lower_95 = cate$est_eval - 1.96 * sqrt(diag(cate$V_eval)),
    upper_90 = cate$est_eval + 1.645 * sqrt(diag(cate$V_eval)),
    lower_90 = cate$est_eval - 1.645 * sqrt(diag(cate$V_eval))
  )
  result_values <- round(result_values,digits = 5)
  
  #3. Summary for the chi-square test
  chisq_test <- data.frame(chisq_stat = cate$chisq_stat,
                           p.value = cate$p.value,
                           significance_level = significance_level, reject = cate$p.value<significance_level)
  chisq_test[,1:2] <- round(chisq_test[,1:2],digits = 5)
    
  return(list(result_betas = result_betas, result_values = result_values,chisq_test = chisq_test))
}