#' Plot estimates
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param lim limits of the scale. By default, NA. To set limits manually, provide a vector or max and min
#'
#' @export
plot.est <- function(x, ..., lim = NA) {

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
                          fill = NA, color = "darkgrey", size = 0.2) +
    ggthemes::theme_map() +
    ggplot2::ggtitle("Differences in average weighted surfaces") + labs(fill = "Density") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "bottom")

  if (is.na(lim)) {

    surf_plot <- surf_plot +
      ggplot2::scale_fill_gradientn(
        colors = c("steelblue1", "white", "violetred"),
        limits = c(-max(abs(df_surf$value)), max(abs(df_surf$value)))
      )

  } else {

    surf_plot <- surf_plot +
      ggplot2::scale_fill_gradientn(
        colors = c("steelblue1", "white", "violetred"),
        limits = lim
      )

  }

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
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    labs(y = "Expected outcome events", x = "Windows",
         title = "Causal effects per time period",
         color = "Effect types", fill = "Effect types") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  return(list(surface = surf_plot, expectation = expectation_plot))

}
