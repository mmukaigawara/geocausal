#' Generate a Hajek estimator for heterogeneity analysis
#'
#' @description A function that returns a Hajek estimator of CATE for a spatial or spatio-temporal effect modifier
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
#' @param em treat column of a hyperframe that summarizes the effect modifier data. In the form of `hyperframe$column`. It can be NULL if E_mat is provided.
#' @param nbase number of bases for splines
#' @param spline_type type of splines. Either `"ns"` or `"bs"`. 
#' @param intercept whether to include intercept in the regression model. Default is TRUE. 
#' @param eval_values a vector of values of the effect modifier for which CATE will be evaluated. Default is a `seq(a,b,length.out=20)` where `a` and `b` are minimum and maximum values of the effect modifier.
#' @param eval_mat evaluated spline basis (excluding the intercept) matrix at `eval_values`.  If `intercept = TRUE`, then a column of 1 will be add to `eval_mat`.
#' @param test_beta a vector of integers contain the indices of the coefficients that are included in the hypothesis test. By default, the null hypothesis is that all coefficient  (except the intercept is 0). See details below
#' @param save_weights whether to save weights. Default is `TRUE`
#' @param ... arguments passed onto the function 
#' 
#' @returns list of the following:
#' `est_beta`: estimated regression coefficient
#' `V_beta`: estimated asymptotic covariance matrix of regression coefficient (normalized by total time periods)   
#' `chisq_stat`: observed chi-square statistics for the hypothesis test
#' `p.value`: observed chi-square statistics for the hypothesis test
#' `specification`: information about the specification of the spline basis and the values on which the CATE is estimated
#' `est_eval`: estimated CATE evaluted at chosen values
#' `V_eval`: estimated asymptotic covariance matrix of the estimated CATE values (normalized by total time periods)
#' `mean_effect`: Mean of the pseudo pixel effect
#' `total_effect`: Mean of the pseudo effect for the window `entire_window`. It is equal to mean effect times the total number of pixels inside the chosen window
#'
#' @details `E_mat` should be a matrix or array of dimensions \eqn{n} by \eqn{m} where \eqn{n} is the product of image dimensions and number of time period,
#' and \eqn{m} is `nbase`-`intercept`. If you want to construct your own covariate matrix `E_mat`, you should use `get_em_vec()` to convert
#' the effect modifer(usually a column of a hyperframe) to a vector, and then construct the splines basis based on the vector. The covariate matrix`E_mat` should not 
#' the column for intercept. The function `get_cate()` will conduct a hypothesis testing on whether all the selected coefficients are 0. `test_beta` is a vector of positive integers specifying the indices
#' of the chosen beta. The coefficients (except the intercept) are indexed by `1,2,...,nbase-intercept`. By default, it test whether all the coefficients(except the intercept) are 0, and this is testing the
#' the heterogeneity effect of the effect modifier.

get_cate <- function(obs, cf1, cf2, treat, pixel_count_out,lag, trunc_level=0.95, time_after=TRUE,entire_window = NULL,
                     em = NULL,E_mat = NULL,
                     nbase = 6, spline_type = "ns",intercept = TRUE,
                     eval_values = NULL, eval_mat = NULL,test_beta = NULL,save_weights = TRUE,...) {
  # pixel_ratio <- 8451/(128*128)
  chisq_stat <- NULL
  p.value <- NULL
  total_effect <- NULL
  mean_effect <- NULL
  E_mat_provided <- !is.null(E_mat)
  weights <- NULL
  
  # --------------------------check the format of the arguments-------------------
  is_positive_integer_within_range <- function(x, min_val, max_val) {
    all(x %% 1 == 0 & x > 0 & x >= min_val & x <= max_val)
  }
  
  
  if(is.null(em) & is.null(E_mat)){
    stop("Both em and E_mat are null.")
  }
  
  if(is.null(eval_mat) & !is.null(E_mat)){
    stop("eval_mat is missing when E_mat is provided")
  }
  
  if(is.null(eval_values) & !is.null(eval_mat)){
    stop("eval_values is missing when eval_mat is provided")
  }
  
  if(is.null(nbase)){
    nbase <- ncol(E_mat)+intercept
  }
  
  if(!is.null(test_beta)){
    if(!is_positive_integer_within_range(test_beta,1,nbase-intercept)){
      stop("test_beta need to be a vector of integers between 1 and the number coefficients that are not the intercept")
    }
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
  cat('In the analysis, pixel grid dimension is', dimyx,"\n")
  time_points <- dim(estimates_1$weighted_surface_arr_haj)[3]
  # dimyx <- dim(estimates$av_surface[[1]])[c(1,2)]
  # time_points <- dim(estimates$av_surface[[1]])[3]
  
  
  
  #------------------------construct the basis matrix if not provided----------------------
  if(is.null(E_mat)){
    cat("Generate spline basis...\n")
    if("im"%in%class(em[[1]])){
      em <- lapply(1:length(em), function(x) as.matrix(as.im(em[[x]],dimyx = dimyx)))
    }else if(!"array"%in%class(em[[1]])){
      stop("em is not a list of im or 2D arrays")
    }
    
    if(length(em)==1){
      em <- lapply(1:time_points, function(x) em[[1]])
    }else{
      em <- em[1:time_points]
    }
    
    if(spline_type=="ns"){
      E_mat <- splines::ns(unlist(em[1:time_points]),df = nbase-intercept, ...)
    }
    
    if(spline_type=="bs"){
      E_mat <- splines::bs(unlist(em[1:time_points]),df = nbase-intercept,...)
    }
    
  }
  knots <- attr(E_mat, "knots")
  
  if(is.null(eval_values)){
    eval_values <- seq(min(unlist(em),na.rm = TRUE),max(unlist(em),na.rm = TRUE),length.out = 20)
  }
  if(is.null(eval_mat)){
    eval_mat <- predict(E_mat,newx = eval_values)
  }
  
  
  if(intercept){
    E_mat <- cbind(1,E_mat)
    eval_mat <- cbind(1,eval_mat)
  }
  

  #---------------------------------fit the model-----------------------------------
  cat("Fit the model...\n")
  # form a dataframe for the estimates and covariates
  df <- cbind.data.frame(E = E_mat,estimates1 = c(estimates_1$weighted_surface_arr_haj),estimates2 = c(estimates_2$weighted_surface_arr_haj))
  weights <- rbind(estimates_1$weights,estimates_2$weights)
  # df <- cbind.data.frame(E = E_mat,estimates1 = c(estimates$av_surface_haj[[1]]),estimates2 = c(estimates$av_surface_haj[[6]]))
  # weights <- rbind(estimates$weights[1,],estimates$weights[6,])
  

  df$est <- df$estimates2-df$estimates1
  df$time <- sort(rep(1:time_points,prod(dimyx)))
  df <- na.omit(df) # remove all the na values
  mean_effect <- mean(df$est,na.rm = TRUE)
  total_effect <- mean_effect*sum(df$time==1)


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
             beta1/estimates_1$stabilizer,
             -beta2/estimates_2$stabilizer)
  

  

  
  V <- J%*%Vt%*%t(J) # variance for beta_hat
  beta <- beta2-beta1  # beta_hat
  

  tryCatch({
    
    if(is.null(test_beta)){
      test_beta <- 1:ncol(E_mat)
      if(intercept==1){
        test_beta <- 2:ncol(E_mat)
      }
    }else{
      if(intercept==1){
        test_beta <- test_beta+1
      }
    }
    chisq_stat <- t(beta[test_beta])%*%solve(V[test_beta,test_beta])%*%beta[test_beta]*time_points
    p.value <- pchisq(chisq_stat,df = length(test_beta),lower.tail = FALSE)
    
    },
    error = function(e) {
      message("Warning: covariance matrix of beta in the hypothesis testing is computationally singular")
    }
  )
    

  
  
  #-----------------------prediction and varaince-------------------------------
  cat("Get the predicted value and variance... \n") 
  V_eval <- eval_mat%*%V%*%t(eval_mat)/time_points   # NOTE we divide by T here!

  est_eval <- c(eval_mat%*%beta)
  if(!E_mat_provided){
    specification <- list(eval_values=eval_values,spline_type = spline_type,intercept = intercept, nbase = nbase,knots = knots,test_beta = paste0("basis",test_beta-intercept))
  }else{
    specification <- list(eval_values = eval_values, intercept = intercept,test_beta = paste0("basis",test_beta-intercept))
  }
  
  class(weights) <- weights
  cate <- list(est_beta = beta,
               V_beta = V/time_points,    # note we divided by T, and this is the variance bound for CI
               chisq_stat = chisq_stat,
               p.value = p.value,
               specification = specification,
               est_eval = est_eval,
               V_eval = V_eval,
               total_effect = total_effect,
               mean_effect = mean_effect,
               weights = weights)
  
  class(cate) <- c("cate","list")
  
  return(cate)
  
}

