#' Plot the results of superthinning tests
#'
#' @param x input
#' @param ... arguments passed on to the function
#'
#' @export
plot.supthin <- function(x, ...) {

  env_k <- x %>% dplyr::filter(fx == "k")
  env_l <- x %>% dplyr::filter(fx == "l")

  plot_k <- ggplot2::ggplot(env_k, aes(x = r)) +
    ggplot2::geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.5) +
    ggplot2::geom_line(aes(y = mmean), color = "red", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_line(aes(y = obs), color = "black", linewidth = 0.5) +
    theme_bw() +
    labs(x = "r", y = expression(K(r))) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  plot_l <- ggplot2::ggplot(env_l, aes(x = r)) +
    ggplot2::geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.5) +
    ggplot2::geom_line(aes(y = mmean), color = "red", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_line(aes(y = obs), color = "black", linewidth = 0.5) +
    theme_bw() +
    labs(x = "r", y = expression(L(r)-r)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  return(list(plot_k = plot_k, plot_l = plot_l))
}


