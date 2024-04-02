#' Plot estimated CATE
#' 
#' @param cate a cate object returned by `get_cate()` function
#' @param ... arguments passed on to the function
#' @param categorical a vector of boolean values indicating whether each value in `eval_values` should be treated as a categorical (TRUE) or continuous (FALSE).
#' @param xlim a vector of two values specifying the limits of x. Default is NULL
#' @param ylim a vector of two values specifying the limits of y. Default is NULL 
#' @param main title
#' @param xlab label of x-axis
#' 
#' @export 
plot.cate <- function(cate,...,categorical = NULL,xlim = NULL,main = "",xlab = "",ylim = NULL) {

  V <- diag(cate$V_eval)
  x <- cate$specification$eval_values
  m <- cate$est_eval
  ub <- m+qnorm(c(0.975))*sqrt(V)
  lb <- m+qnorm(c(0.025))*sqrt(V)
  
  
  # Create a data frame for plotting
  df_plot <- data.frame(x = x, m = m, ub = ub, lb = lb)
  if(is.null(categorical)){
    categorical <- rep(1,length(x))
  }
  
  group <- cumsum(c(1, diff(categorical) != 0))
  
  # Create a data frame
  df_plot$group <- factor(group)
  
  # Filter the data frame based on xrange if provided
  if (!is.null(xrange)) {
    df_plot <- df_plot[df_plot$x >= xrange[1] & df_plot$x <= xrange[2], ]
  }

    # Create the plot
    # p <- ggplot(df_plot, aes(x = x, y = m)) +
    #   geom_line() +
    #   geom_ribbon(aes(ymin = lb, ymax = ub), fill = "grey75", alpha = 0.5) +
    #   geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    #   labs(x = xlab, y = "CATE", title = main)+theme_classic()
  
  # Create the plot
  p <- ggplot() +
    ggplot2::geom_point(data = subset(df_plot, !(duplicated(group) | duplicated(group, fromLast = TRUE))), aes(x = x, y = m), color = "black") +
    ggplot2::geom_errorbar(data = subset(df_plot, !(duplicated(group) | duplicated(group, fromLast = TRUE))), aes(x = x, ymin = lb, ymax = ub), width = 0.2) +
    ggplot2::geom_line(data = subset(df_plot, (duplicated(group) | duplicated(group, fromLast = TRUE))), aes(x = x, y = m,group=group), color = "black") +
    ggplot2::geom_ribbon(data = subset(df_plot, (duplicated(group) | duplicated(group, fromLast = TRUE))), aes(x = x, ymin = lb, ymax = ub,group = group), fill = "grey", alpha = 0.5) +
    ggplot2::theme_classic()+
    ggplot2::labs(x = xlab, y = "CATE", title = main)
  
  
  # Adjust y-axis limits if specified
  if (!is.null(ylim)) {
    p <- p + ylim(ylim[1], ylim[2])
  }
  
  # Print the plot
  print(p)
  
}
