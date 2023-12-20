#' Plot results
#' 
#' @description `plot` functions take the output and plot it.
#' 
#' @param x an output object
#' @param ... arguments
#' @param actual_data actual data in the form of `hyperframe$column`.
#' @param dens_2 density 2 (if any). By default, `NA`.
#' @param dens_3 density 3 (if any). By default, `NA`.
#' @param time_unit x-axis label of the output
#' 
#' @details Currently, observed densities (class: obs) and estimates (class: est) are supported by this function.

#' @export
plot <- function (x, ...) UseMethod("plot")

#' @rdname plot
#' @method plot im
#' @export 

plot.im <- function (x, ...) spatstat.geom::plot.im(x, ...)

#' @rdname plot
#' @method plot ppp
#' @export 

plot.ppp <- function (x, ...) spatstat.geom::plot.ppp(x, ...)


#' @rdname plot
#' @method plot obs
#' @export 
plot.obs <- function (x, dens_2 = NA, dens_3 = NA, actual_data = NA, time_unit = NA, ...) {
  
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
      labs(title = "Actual vs. Predicted Counts", x = time_unit, y = "Count") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    plot_residual <- plot_data %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts - actual_counts), color = color_dens_1, linewidth = 0.6) + theme_bw() + 
      labs(title = "Residual Plot", x = time_unit, y = "Predicted - Actual Counts") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
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
      theme_bw() + labs(title = "Actual vs. Predicted Counts", x = time_unit, y = "Count") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    plot_residual <- plot_data %>% 
      ggplot2::ggplot() + ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts - actual_counts), color = color_dens_1, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts_2 - actual_counts), color = color_dens_2, linewidth = 0.6) + 
      theme_bw() + labs(title = "Residual Plot", x = time_unit, y = "Predicted - Actual Counts") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
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
      theme_bw() + labs(title = "Actual vs. Predicted Counts", x = time_unit, y = "Count") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    plot_residual <- plot_data %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts - actual_counts), color = color_dens_1, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts_2 - actual_counts), color = color_dens_2, linewidth = 0.6) + 
      ggplot2::geom_line(aes(x = time, y = predicted_counts_3 - actual_counts), color = color_dens_3, linewidth = 0.6) + 
      theme_bw() + labs(title = "Residual Plot", x = time_unit, y = "Predicted - Actual Counts") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    
    return(list(plot_data = plot_data, 
                plot_compare = plot_compare, 
                plot_residual = plot_residual))
  }
  
}


#' @rdname plot
#' @method plot est
#' @export 
plot.est <- function(x, ...) {
  
  estimates <- x
  
  #1. Visualization 1: Plot the difference in weighted surfaces -----
  
  diff_ave_surf <- estimates$cf2_ave_surf - estimates$cf1_ave_surf
  
  df_surf <- as.data.frame(diff_ave_surf, xy = TRUE) #DF
  
  window_sp <- conv_owin_into_sf(estimates$windows[[length(estimates$windows)]])
  polygon_df <- window_sp[[2]] #Convert owin to DF
  
  surf_plot <- ggplot(data = df_surf, aes(x = x, y = y, fill = value)) + #Plot
    ggplot2::geom_tile() +
    ggplot2::coord_quickmap() +
    ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude), 
                          fill = NA, color = "black", size = 0.2) +
    ggplot2::scale_fill_gradientn(
      colors = c("steelblue1", "white", "violetred"),
      limits = c(-max(abs(df_surf$value)), max(abs(df_surf$value)))
    ) +
    ggthemes::theme_map() +
    ggplot2::ggtitle("Differences in average weighted surfaces") + labs(fill = "Density") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  #2. Visualization 2: Causal effects -----
  
  ## Prepare data
  result <- data.frame(
    window = seq(1, length(estimates$windows)),
    point_estimate = estimates$est_causal,
    upper_95 = estimates$est_causal + 1.96 * sqrt(estimates$var_causal),
    lower_95 = estimates$est_causal - 1.96 * sqrt(estimates$var_causal),
    upper_90 = estimates$est_causal + 1.645 * sqrt(estimates$var_causal),
    lower_90 = estimates$est_causal - 1.645 * sqrt(estimates$var_causal)
  )
  
  ## Visualization
  expectation_plot <- ggplot(result, aes(x = as.factor(window), y = point_estimate)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_linerange(aes(ymin = lower_95, ymax = upper_95)) +
    ggplot2::geom_linerange(aes(ymin = lower_90, ymax = upper_90), lwd = 1.1) +
    theme_bw() +
    labs(title = "Causal effects", x = "Windows", y = "Expected outcomes per time period",
         color = "Effect types", fill = "Effect types") +
    theme_bw() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    labs(y = "The expected outcome events\ncovered by the area",
         title = "Causal Effects Per Time Period",
         subtitle = "The Expected Number of Outcome Events") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  
  return(list(surface = surf_plot, expectation = expectation_plot))
  
}