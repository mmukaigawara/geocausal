#' Plot estimated CATE
#' 
#' @param x input
#' @param ... arguments passed on to the function
#' @param type The type of plot to draw. 
#'   - If 'type' is "p", points with error bars will be drawn.
#'   - If 'type' is "l", lines with shaded region will be drawn.
#'   - If 'type' is a vector of strings, each element specifies the type for the corresponding `eval_values` value.
#' @param xrange an optional vector of two values the range of x shown.
#' @param ylim an optional vector of two values specifying the limits of y
#' @param main title
#' @param xlab label of x-axis
#' @param scale a positive number specifying the scale by which the estimates will be scaled. If provided, the estimates will be scaled by this value. Default is NULL, which means no scaling is applied.
#' 
#' @export 
plot.cate <- function(x,...,type = "l",scale = 1,xrange = NULL,main = "",xlab = "",ylim = NULL) {
  cate <- x
  V <- diag(cate$V_eval)*scale^2
  x <- cate$specification$eval_values
  m <- cate$est_eval*scale
  ub <- m+qnorm(c(0.975))*sqrt(V)
  lb <- m+qnorm(c(0.025))*sqrt(V)
  
  categorical <- ifelse(type=="p",1,0)

  
  
  # Create a data frame for plotting
  df_plot <- data.frame(x = x, m = m, ub = ub, lb = lb)
  if(length(categorical)==1){
    categorical <- rep(categorical,length(x))
  }
  
  if(sum(categorical==0)==0){
    df_plot$x <- as.factor(x)
    p <- ggplot() +
      ggplot2::geom_point(data = df_plot, aes(x = x, y = m), color = "black") +
      ggplot2::geom_errorbar(data = df_plot, aes(x = x, ymin = lb, ymax = ub), width = 0.1) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
      ggplot2::theme_bw()+
      ggplot2::labs(x = xlab, y = "CATE", title = main)
    
  }else{
    group <- cumsum((c(0, diff(categorical) != 0)+categorical)>0)
    
    # Create a data frame
    df_plot$group <- factor(group)
    
    # Filter the data frame based on xrange if provided
    if (!is.null(xrange)) {
      df_plot <- df_plot[df_plot$x >= xrange[1] & df_plot$x <= xrange[2], ]
    }
    
    
    
    # Create the plot
    p <- ggplot() +
      ggplot2::geom_point(data = subset(df_plot, !(duplicated(group) | duplicated(group, fromLast = TRUE))), aes(x = x, y = m), color = "black") +
      ggplot2::geom_errorbar(data = subset(df_plot, !(duplicated(group) | duplicated(group, fromLast = TRUE))), aes(x = x, ymin = lb, ymax = ub), width = 0.1) +
      ggplot2::geom_line(data = subset(df_plot, (duplicated(group) | duplicated(group, fromLast = TRUE))), aes(x = x, y = m,group=group), color = "black") +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red")+
      ggplot2::geom_ribbon(data = subset(df_plot, (duplicated(group) | duplicated(group, fromLast = TRUE))), aes(x = x, ymin = lb, ymax = ub,group = group), fill = "grey", alpha = 0.5) +
      ggplot2::theme_bw()+
      ggplot2::labs(x = xlab, y = "CATE", title = main)
  }
  

  
  
  # Adjust y-axis limits if specified
  if (!is.null(ylim)) {
    p <- p + ylim(ylim[1], ylim[2])
  }

  

  return(p)
  
}
