#' Generate a Hajek estimator for heterogeneity analysis
#'
#' @description A function that returns a Hajek estimator of causal contrasts
#' @param obs observed density
#' @param cf1 counterfactual density 1
#' @param cf2 counterfactual density 2
#' @param treat column of a hyperframe that summarizes treatment data. In the form of `hyperframe$column`.
#' @param pixel_count_out column of a hyperframe that summarizes the number of outcome events in each pixel
#' @param lag integer that specifies lags to calculate causal estimates.
#' @param trunc_level the level of truncation for the weights (0-1).
#' @param time_after whether to include one unit time difference between treatment and outcome. By default = TRUE
#' @param entire_window owin object (the entire region of interest)
#' @param E_mat optional covariance matrix (excluding the intercept) for the effect modifier. If provided, then the regression model will be based on this matrix. If `intercept = TRUE`, then a column of 1 will be add to `E_mat`.
#' @param E_list a list of real valued images or 2D arrays for effect modifier. It can be NULL if E_mat is provided.
#' @param nbase number of bases for splines
#' @param spline_type type of splines. Either `"ns"` or `"bs"`. 
#' @param intercept whether to include intercept in the regression model. Default is TRUE. 
#' @param eval_values A vector of values of the effect modifier for which CATE will be evaluated. Default is a `seq(a,b,length.out=20)` where `a` and `b` are minimum and maximum values of the effect modifier.
#' @param eval_mat Evaluated spline basis (excluding the intercept) matrix at `eval_values`.  If `intercept = TRUE`, then a column of 1 will be add to `eval_mat`.
#' @param ... arguments passed onto the function 
#' 
#' @returns list of Hajek estimators for each scenario (`est_haj`), 
#' causal contrasts (Hajek estimator) as a matrix (`est_tau_haj_matrix`), and 
#' causal contrast (scenario 2 - scenario 1) as a numeric vector (`est_tau_haj_cf2_vs_cf1`), 
#' along with weights, windows, and smoothed outcomes
#'  
#' @details `get_estimates()` is an internal function to `get_est()` function, 
#' performing the estimation analysis after `get_weighted_surf()` function

get_cate <- function(obs, cf1, cf2, treat, pixel_count_out,lag, trunc_level=0.95, time_after=TRUE,entire_window,
                     E_list = NULL,E_mat = NULL,
                     nbase, spline_type = "ns",intercept = TRUE,
                     eval_values = NULL, eval_mat = NULL,...) {
  
  chisq_stat <- NULL
  p.value <- NULL
  E_mat_provided <- !is.null(E_mat)
  
  
  # --------------------------check the format of the arguments-------------------
  if(is.null(E_list) & is.null(E_mat)){
    stop("Both E_list and E_mat are null.")
  }
  
  if(is.null(eval_mat) & !is.null(E_mat)){
    stop("eval_mat is missing when E_mat is provided")
  }
  
  if(is.null(eval_values) & !is.null(eval_mat)){
    stop("eval_values is missing when eval_mat is provided")
  }
  
  #-------------------------get the weighted surfaces------------------------------------
  cat("Get weighted surfaces... \n")
  # CF1
  estimates_1 <- get_weighted_surf(obs_dens = obs,
                                   cf_dens = cf1,
                                   treatment_data = treat,
                                   smoothed_outcome = pixel_count_out,
                                   mediation = FALSE, cate = TRUE,
                                   obs_med_log_sum_dens = NA,
                                   cf_med_log_sum_dens = NA,
                                   lag = lag, entire_window = entire_window,
                                   time_after,
                                   truncation_level = trunc_level)

  ## CF2
  estimates_2 <- get_weighted_surf(obs_dens = obs,
                                   cf_dens = cf2,
                                   treatment_data = treat,
                                   smoothed_outcome = pixel_count_out,
                                   mediation = FALSE, cate = TRUE,
                                   obs_med_log_sum_dens = NA,
                                   cf_med_log_sum_dens = NA,
                                   lag = lag, entire_window = entire_window,
                                   time_after,
                                   truncation_level = trunc_level)
  
  
  dimyx <- dim(estimates_1$weighted_surface_arr_haj)[c(1,2)]
  time_points <- dim(estimates_1$weighted_surface_arr_haj)[3]
  # dimyx <- dim(estimates$av_surface[[1]])[c(1,2)]
  # time_points <- dim(estimates$av_surface[[1]])[3]
  
  
  
  #------------------------construct the basis matrix if not provided----------------------
  if(is.null(E_mat)){
    cat("Generate spline basis...\n")
    if("im"%in%class(E_list[[1]])){
      E_list <- lapply(1:length(E_list), function(x) as.matrix(E_list[[x]]))
    }else if(!"array"%in%class(E_list[[1]])){
      stop("E_list is not a list of im or 2D arrays")
    }
    
    if(length(E_list)==1){
      E_list <- lapply(1:time_points, function(x) E_list[[1]])
    }else{
      E_list <- E_list[1:time_points]
    }
    
    if(spline_type=="ns"){
      E_mat <- splines::ns(unlist(E_list[1:time_points]),df = nbase-intercept, ...)
    }
    
    if(spline_type=="bs"){
      E_mat <- splines::bs(unlist(E_list[1:time_points]),df = nbase-intercept,...)
    }
    
  }
  knots <- attr(E_mat, "knots")
  
  if(is.null(eval_values)){
    eval_values <- seq(min(unlist(E_list),na.rm = TRUE),max(unlist(E_list),na.rm = TRUE),length.out = 20)
    eval_mat <- predict(E_mat,newx = eval_values)
  }
  
  
  
  if(intercept){
    E_mat <- cbind(1,E_mat)
    eval_mat <- cbind(1,eval_mat)
  }
  
  
  #---------------------------------fit the model-----------------------------------
  cat("Start fit the model...\n")
  # form a dataframe for the estimates and covariates
  df <- cbind.data.frame(E = E_mat,estimates1 = c(estimates_1$weighted_surface_arr_haj),estimates2 = c(estimates_2$weighted_surface_arr_haj))
  weights <- rbind(estimates_1$weights,estimates_2$weights)
  # df <- cbind.data.frame(E = E_mat,estimates1 = c(estimates$av_surface_haj[[1]]),estimates2 = c(estimates$av_surface_haj[[6]]))
  # weights <- rbind(estimates$weights[1,],estimates$weights[6,])
  

  
  df$est <- df$estimates2-df$estimates1
  df$time <- sort(rep(1:time_points,prod(dimyx)))
  df <- na.omit(df) # remove all the na values
  

  valid_time_points <- 0
  p <- ncol(E_mat) # number of covariates

  beta_arr <- array(NA,c(time_points,2*p+2))
  Vt <- array(NA,c(2*p+2,2*p+2,time_points))

  
  for (tt in 1:time_points) {
    df_tt <- df[df$time==tt,-ncol(df)]
    
    # Fit the spline
    tryCatch({
      Z <- as.matrix(df_tt[,-c(ncol(df_tt)-0:2)])
      Q <- solve(t(Z)%*%Z)

      beta_arr[tt,] <- c(Q%*%t(Z)%*%df_tt$estimates1,Q%*%t(Z)%*%df_tt$estimates2,weights[,tt])
      Vt[,,tt] <- beta_arr[tt,]%*%t(beta_arr[tt,])

      valid_time_points <- valid_time_points+1
    }, error=function(e){
      message("Warning: covariate matrix is computationally singular for time period ",tt)
    })
    
  }
  
  Vt <- rowMeans(Vt,dims=2,na.rm = TRUE)

  beta <- colMeans(beta_arr,na.rm=TRUE)
  beta1 <- beta[1:p]
  beta2 <- beta[(p+1):(2*p)]
  
  
  # Jocobian matrix 
  J <- cbind(-diag(p),
             diag(p),
             beta1,
             -beta2)
  
  
  V <- J%*%Vt%*%t(J) # variance for beta_hat
  beta <- beta2-beta1  # beta_hat
  

  tryCatch({
    if(intercept==1){
      chisq_stat <- t(beta[-1])%*%solve(V[-1,-1])%*%beta[-1]*time_points
    }else{
      chisq_stat <- t(beta)%*%solve(V)%*%beta*time_points
    }
    
    p.value <- pchisq(chisq_stat,df = p-1,lower.tail = FALSE)},
    error = function(e) {
      message("Warning: covariance matrix of beta is computationally singular")
    }
  )
    

  
  
  #-----------------------prediction and varaince-------------------------------
  cat("Get the predicted value and variance... \n") 
  
  V_eval <- eval_mat%*%V%*%t(eval_mat)/time_points   # NOTE we divide by T here!

  est_eval <- c(eval_mat%*%beta)
  if(!E_mat_provided){
    specification <- list(eval_values=eval_values,spline_type = spline_type,intercept = intercept, nbase = nbase,knots = knots)
  }else{
    specification <- list(eval_values = eval_values, intercept = intercept)
  }
  
  cate <- list(est_beta = beta,
               V_beta = V/time_points,    # note we divided by T, and this is the variance bound for CI
               chisq_stat = chisq_stat,
               p.value = p.value,
               specification = specification,
               est_eval = est_eval,
               V_eval = V_eval,
               valid_time_points = valid_time_points)
  
  class(cate) <- c("cate","list")
  
  return(cate)
  
}

