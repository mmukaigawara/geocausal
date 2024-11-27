#' Plot distance-based expectations
#'
#' @param x input
#' @param ... arguments passed on to the function
#' @param grayscale grayscale or not. By default, FALSE.
#' @param dist_map_unit either `"km"` or `"mile"`
#' @param win_plot whether to plot windows as well. By default, FALSE
#' @param use_raw logical. `use_raw` specifies whether to use the raw value of expectations or percentiles.
#' By default, `FALSE`.
#'
#' @export
plot.distlist <- function(x, ..., dist_map_unit = "km",
                          grayscale = FALSE, win_plot = FALSE,
                          use_raw = FALSE) {

  result_data <- x$result_data
  window_list <- x$window_list
  distance_quantiles <- x$distance_quantiles
  entire_window <- x$entire_window

  x_label_text <- paste0("Distance from the focus (", dist_map_unit, ")")

  if(grayscale) {

    if (use_raw) {

      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
        theme_bw() +
        labs(x = x_label_text,
             y = "The expected treatment events\ncovered by the area",
             title = "The expected number of treatment events",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    } else {

      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
        theme_bw() +
        labs(x = x_label_text,
             y = "The proportion of\nexpected treatment events\ncovered by the area",
             title = "The expected number of treatment events",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        ylim(0, 1) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())

      }

  } else {

    if (use_raw) {

      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
        theme_bw() +
        labs(x = x_label_text,
             y = "The expected treatment events\ncovered by the area",
             title = "The expected number of treatment events",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "PiYG") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    } else {

      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
        theme_bw() +
        labs(x = x_label_text,
             y = "The proportion of\nexpected treatment events\ncovered by the area",
             title = "The expected number of treatment events",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "PiYG") +
        ylim(0, 1) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())

    }

  }

  # Plot for windows
  window_showcase <- lapply(window_list, spatstat.geom::as.polygonal)

  if(grayscale) {

    window_plot_list <- lapply(window_showcase, function(x) {
      gg <- ggplot() +
        ggplot2::geom_polygon(data = ggplot2::fortify(as.data.frame(x)), aes(x = x, y = y), fill = "gray") +
        ggplot2::geom_path(data = ggplot2::fortify(as.data.frame(entire_window)), aes(x = x, y = y)) +
        ggthemes::theme_map()
      return(gg)})

  } else {

    window_plot_list <- lapply(window_showcase, function(x) {
      gg <- ggplot() +
        ggplot2::geom_polygon(data = ggplot2::fortify(as.data.frame(x)), aes(x = x, y = y), fill = "#F1B6DA") +
        ggplot2::geom_path(data = ggplot2::fortify(as.data.frame(entire_window)), aes(x = x, y = y)) +
        ggthemes::theme_map()
      return(gg)})
  }

  window_plot_list[[1]] <- window_plot_list[[1]] +
    ggtitle(paste0(round(as.numeric(distance_quantiles["80%"]), 1), "km \n(", 80, " percentile)")) +
    theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[2]] <- window_plot_list[[2]] +
    ggtitle(paste0(round(as.numeric(distance_quantiles["60%"]), 1), "km \n(", 60, " percentile)")) +
    theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[3]] <- window_plot_list[[3]] +
    ggtitle(paste0(round(as.numeric(distance_quantiles["40%"]), 1), "km \n(", 40, " percentile)")) +
    theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[4]] <- window_plot_list[[4]] +
    ggtitle(paste0(round(as.numeric(distance_quantiles["20%"]), 1), "km \n(", 20, " percentile)")) +
    theme(plot.title = element_text(hjust = 0.5))

  w_plot_list <- list(window_plot_list[[4]], window_plot_list[[3]], window_plot_list[[2]], window_plot_list[[1]])

  # Color and plot
  window_plot <- ggpubr::ggarrange(plotlist = w_plot_list, nrow = 1)
  window_plot <- ggpubr::annotate_figure(window_plot, bottom = ggpubr::text_grob("Areas covered by quantiles"))


  if(win_plot) {

    return(window_plot)

    } else {

    return(expectation_plot)

      }

  }
