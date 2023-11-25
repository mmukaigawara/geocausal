#' Visualize estimated results
#'
#' @description A function that visualizes estimated results
#'
#' @param estimates an object returned from `get_est()` function
#' 
#' @returns list of two plots: one for differences in average weighted surfaces and another for results
#' 
#' @details The plot for differences in average weighted surfaces is comparing scenario 2 to scenario 1 
#' (i.e., the baseline is scenario 1).

vis_est <- function(estimates) {
  
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
      colors = colorRampPalette(rev(RColorBrewer::brewer.pal(11, "RdBu")))(255),
      values = c(1.0, (0 - min(df_surf$value)) / 
                   (max(df_surf$value) - min(df_surf$value)), 0)) +
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

