#' Function: vis_obs_density
#'
#' A function that performs the visualization of actual counts and predicted counts
#'
#' @param actual_data
#' @param density_1
#' @param density_2
#' @param density_3
#' @param color_actual
#' @param color_dens_1
#' @param color_dens_2
#' @param color_dens_3
#' @param time_unit The label of the x-axis (time) 


vis_obs_density <- function(actual_data,
                            density_1, density_2 = NA, density_3 = NA,
                            color_actual,
                            color_dens_1, color_dens_2, color_dens_3,
                            time_unit) {
  
  # Obtain the actual counts 
  actual_counts <- unlist(purrr::map(actual_data, function(x) x$n))
  
  if(is.na(density_2)[1]){
    
    # Obtain the predicted counts
    predicted_counts <- density_1$estimated_counts
    
    # Construct dataset
    plot_data <- data.frame(time = c(1:length(actual_counts)), 
                            actual_counts = actual_counts, 
                            predicted_counts = predicted_counts)
    
    plot_compare <- plot_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = time, y = actual_counts), color = color_actual) +
      ggplot2::geom_line(aes(x = time, y = predicted_counts), color = color_dens_1) +
      theme_bw() + labs(title = "Actual vs. predicted counts", x = time_unit, y = "Count")
    
    return(list(plot_data, plot_compare))
    
  } else if (is.na(density_3)[1]) {
    
    # Obtain the predicted counts
    predicted_counts <- density_1$estimated_counts
    predicted_counts_2 <- density_2$estimated_counts
    
    # Construct dataset
    plot_data <- data.frame(time = c(1:length(actual_counts)), 
                            actual_counts = actual_counts, 
                            predicted_counts = predicted_counts,
                            predicted_counts_2 = predicted_counts_2)
    
    plot_compare <- plot_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = time, y = actual_counts), color = color_actual) +
      ggplot2::geom_line(aes(x = time, y = predicted_counts), color = color_dens_1) +
      ggplot2::geom_line(aes(x = time, y = predicted_counts_2), color = color_dens_2) +
      theme_bw() + labs(title = "Actual vs. predicted counts", x = time_unit, y = "Count")
    
    return(list(plot_data, plot_compare))
    
  } else {
    
    # Obtain the predicted counts
    predicted_counts <- density_1$estimated_counts
    predicted_counts_2 <- density_2$estimated_counts
    predicted_counts_3 <- density_3$estimated_counts
    
    # Construct dataset
    plot_data <- data.frame(time = c(1:length(actual_counts)), 
                            actual_counts = actual_counts, 
                            predicted_counts = predicted_counts,
                            predicted_counts_2 = predicted_counts_2,
                            predicted_counts_3 = predicted_counts_3)
    
    plot_compare <- plot_data %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(aes(x = time, y = actual_counts), color = color_actual) +
      ggplot2::geom_line(aes(x = time, y = predicted_counts), color = color_dens_1) +
      ggplot2::geom_line(aes(x = time, y = predicted_counts_2), color = color_dens_2) +
      ggplot2::geom_line(aes(x = time, y = predicted_counts_3), color = color_dens_3) +
      theme_bw() + labs(title = "Actual vs. predicted counts", x = time_unit, y = "Count")
    
    return(list(plot_data, plot_compare))
    
  }
  
}