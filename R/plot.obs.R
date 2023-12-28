#' Plot observed densities
#' 
#' @param actual_data actual data in the form of `hyperframe$column`.
#' @param x input
#' @param ... arguments passed on to the function
#' @param dens_2 density 2 (if any). By default, `NA`.
#' @param dens_3 density 3 (if any). By default, `NA`.
#' @param time_unit x-axis label of the output
#' 
#' @export 
plot.obs <- function (x, ..., dens_2 = NA, dens_3 = NA, actual_data = NA, time_unit = NA) {
  
  color_actual = "darkgrey" 
  color_dens_1 = "#f68f46ff"
  color_dens_2 = "#593d9cff"
  color_dens_3 = "#efe350ff"
  
  dens_1 <- x

  actual_counts <- unlist(purrr::map(actual_data, function(x) x$n))
  
  if (is.na(dens_2)[1]) {
    predicted_counts <- dens_1$estimated_counts
    
    plot_data <- data.frame(time = c(1:length(actual_counts)), 
                            actual_counts = actual_counts, 
                            predicted_counts = predicted_counts)
    
    plot_compare <- plot_data %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_line(aes(x = time, y = actual_counts), color = color_actual, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts), color = color_dens_1, linewidth = 0.6) + theme_bw() + 
      labs(title = "Actual vs. predicted counts", x = time_unit, y = "Count") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    plot_residual <- plot_data %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts - actual_counts), color = color_dens_1, linewidth = 0.6) + theme_bw() + 
      labs(title = "Residual plot", x = time_unit, y = "Predicted - actual counts") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    return(list(plot_data = plot_data, 
                plot_compare = plot_compare, 
                plot_residual = plot_residual))
    
  } else if (is.na(dens_3)[1]) {
    predicted_counts <- dens_1$estimated_counts
    predicted_counts_2 <- dens_2$estimated_counts
    
    plot_data <- data.frame(time = c(1:length(actual_counts)), 
                            actual_counts = actual_counts, 
                            predicted_counts = predicted_counts, 
                            predicted_counts_2 = predicted_counts_2)
    
    plot_compare <- plot_data %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_line(aes(x = time, y = actual_counts), color = color_actual, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts), color = color_dens_1, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts_2), color = color_dens_2, linewidth = 0.6) + 
      theme_bw() + labs(title = "Actual vs. predicted counts", x = time_unit, y = "Count") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    plot_residual <- plot_data %>% 
      ggplot2::ggplot() + ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts - actual_counts), color = color_dens_1, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts_2 - actual_counts), color = color_dens_2, linewidth = 0.6) + 
      theme_bw() + labs(title = "Residual plot", x = time_unit, y = "Predicted - actual counts") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    return(list(plot_data = plot_data, 
                plot_compare = plot_compare, 
                plot_residual = plot_residual))
    
  } else {
    predicted_counts <- dens_1$estimated_counts
    predicted_counts_2 <- dens_2$estimated_counts
    predicted_counts_3 <- dens_3$estimated_counts
    
    plot_data <- data.frame(time = c(1:length(actual_counts)), 
                            actual_counts = actual_counts, 
                            predicted_counts = predicted_counts, 
                            predicted_counts_2 = predicted_counts_2, 
                            predicted_counts_3 = predicted_counts_3)
    
    plot_compare <- plot_data %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_line(aes(x = time, y = actual_counts), color = color_actual, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts), color = color_dens_1, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts_2), color = color_dens_2, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts_3), color = color_dens_3, linewidth = 0.6) + 
      theme_bw() + labs(title = "Actual vs. predicted counts", x = time_unit, y = "Count") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    plot_residual <- plot_data %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts - actual_counts), color = color_dens_1, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts_2 - actual_counts), color = color_dens_2, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts_3 - actual_counts), color = color_dens_3, linewidth = 0.6) + 
      theme_bw() + labs(title = "Residual plot", x = time_unit, y = "Predicted - actual counts") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    return(list(plot_data = plot_data, 
                plot_compare = plot_compare, 
                plot_residual = plot_residual))
  }
  
}