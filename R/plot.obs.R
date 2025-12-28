#' Plot observed densities
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param dens_2 density 2 (if any). By default, `NA`.
#' @param dens_3 density 3 (if any). By default, `NA`.
#' @param time_unit x-axis label of the output
#' @param combined whether to combine the two plots. By default, TRUE. If TRUE,
#' then the plot function produces one ggplot object. If FALSE, three objects (two ggplot and one dataframe) will be produced.
#' @param lim limits of the scale for the average residual fields. By default, c(0, -1). To set limits manually, provide a vector of max and min
#' @param window owin object to plot the average residual fields
#'
#' @export
plot.obs <- function (x, ..., dens_2 = NA, dens_3 = NA, lim = c(-1, 1), window,
                      time_unit = NA, combined = TRUE) {

  color_actual = "darkgrey"
  color_dens_1 = "#f68f46ff"
  color_dens_2 = "#593d9cff"
  color_dens_3 = "#efe350ff"

  dens_1 <- x

  actual_counts <- dens_1$actual_counts

  # Average residual fields
  window_sp <- conv_owin_into_sf(window)
  polygon_df <- window_sp[[2]] #Convert owin to DF
  res_df <- x$res_df

  plot_arf <- ggplot(data = res_df, aes(x = x, y = y, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::coord_quickmap() +
    ggplot2::geom_polygon(data = polygon_df, aes(x = longitude, y = latitude),
                          fill = NA, color = "darkgrey", size = 0.2) +
    ggthemes::theme_map() +
    ggplot2::scale_fill_gradientn(
      colors = c("steelblue1", "white", "violetred"),
      limits = lim
    ) +
    labs(fill = "Average residuals") +
    theme(legend.position = "bottom")

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


    if (combined) {

      return(ggpubr::ggarrange(plot_compare, plot_residual, plot_arf, nrow = 1))

    } else {

      return(list(plot_data = plot_data,
                  plot_compare = plot_compare,
                  plot_residual = plot_residual))

    }

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

    if (combined) {

      return(ggpubr::ggarrange(plot_compare, plot_residual))

    } else {

      return(list(plot_data = plot_data,
                  plot_compare = plot_compare,
                  plot_residual = plot_residual,
                  plot_arf = plot_arf))

    }

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

    if (combined) {

      return(ggpubr::ggarrange(plot_compare, plot_residual, plot_arf, nrow = 1))

    } else {

      return(list(plot_data = plot_data,
                  plot_compare = plot_compare,
                  plot_residual = plot_residual,
                  plot_arf = plot_arf))

    }

  }

}
