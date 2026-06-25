#' Plot the results of superthinning tests
#'
#' @description Plot method for objects of class `supthin`, returned by
#' [dx_supthin()]. Visualizes the observed and simulated-envelope summary
#' functions (the K-function and the centered L-function) used to diagnose
#' departures from complete spatial randomness after superthinning.
#'
#' @param x an object of class `supthin`, typically the output of [dx_supthin()].
#' @param ... additional arguments. Currently ignored.
#'
#' @returns A named list with two `ggplot` objects, `plot_k` (the K-function with
#' simulation envelope) and `plot_l` (the centered L-function with simulation
#' envelope).
#'
#' @seealso [dx_supthin()]
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
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  plot_l <- ggplot2::ggplot(env_l, aes(x = r)) +
    ggplot2::geom_ribbon(aes(ymin = lo, ymax = hi), fill = "grey80", alpha = 0.5) +
    ggplot2::geom_line(aes(y = mmean), color = "red", linetype = "dashed", linewidth = 0.5) +
    ggplot2::geom_line(aes(y = obs), color = "black", linewidth = 0.5) +
    theme_bw() +
    labs(x = "r", y = expression(L(r)-r)) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  return(list(plot_k = plot_k, plot_l = plot_l))
}


