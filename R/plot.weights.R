#' Plot weights
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param type_weights the type of weights to plot.
#'   - If 'plot_weights' is `standardized`, histogram of standardized weights will be generated.
#'   - If 'plot_weights' is `unstandardized`, histogram of unstandardized weights will be generated.
#'   Default is `standardized`. 
#' @param binwidth bin width of the histogram. Default is NULL
#' 
#' @export 
plot.weights <- function(x,...,type_weights = "standardized",binwidth = NULL){
  
  if(type_weights == "standardized"){
    weights <- weights/rowMeans(weights)
  }
  
  # Convert matrix to data frame
  weights_df <- data.frame(weights = c(weights[1,],weights[2,]),interv = c(rep("Intervention 1",ncol(weights)),rep("Intervention 2",ncol(weights))))
  
  
  
  ggplot(weights_df, aes(x = weights)) +
    ggplot2::geom_histogram(binwidth = binwidth, position = "dodge", color = "black") +
    ggplot2::facet_wrap(~interv, scales = "free") +
    ggplot2::labs(title = paste0("Histogram of ",type_weights," weights"), x = "Weight", y = "Frequency") +
    theme_bw()
}
