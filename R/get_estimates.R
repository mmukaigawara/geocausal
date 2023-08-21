#' Get causal estimates comparing two scenarios
#'
#' @description
#' `get_estimates()` generates causal estimates comparing two counterfactual scenarios.
#'
#' @param obs_dens observed density
#' @param cf_dens counterfactual density
#' @param treatment_data column of a hyperframe that summarizes treatment data. In the form of `hyperframe$column`.
#' @param smoothed_outcome column of a hyperframe that summarizes the smoothed outcome data
#' @param lag integer that specifies lags to calculate causal estimates
#' @param entire_window owin object (the entire region of interest)
#' @param dist_map im object (distance map); notice that the unit (either mile or km) is inherited from the distance map
#' @param dist a vector of distances for which the function calculates the expectations (e.g., `c(50, 100, 150, 200)`)
#' @param dist_map_unit either `"km"` or `"mile"`
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#'
#' @returns list of the following:
#' `average_expected_events`: Hajek estimators (counts, the entire window)
#' `average_expected_events_quantiles`: percentiles of Hajek estimators
#' `weights`: weights
#' `average_weights`: mean of `weights`
#' `plot`: plot showing distance-based expectations
#' `distances`: distances
#' `distances_window`: as window objects
#' `expectation_plot`: plot of expectations
#' `window_plot`: plot of windows

get_estimates <- function(obs_dens,
                          cf_dens,
                          treatment_data,
                          smoothed_outcome,
                          lag,
                          entire_window,
                          dist_map,
                          dist,
                          dist_map_unit = "km",
                          grayscale) {

  # 1. Weight

  message("Calculating weights...\n")

  # 1-1. Log density
  counterfactual_sum_log <- get_cf_sum_log_intens(cf_dens = cf_dens,
                                                  treatment_data = treatment_data)

  observed_sum_log <- obs_dens$sum_log_intens

  # 1-2. Log density ratio (LDR)
  log_density_ratio <- counterfactual_sum_log - observed_sum_log

  # 1-3. Convert LDR to weights (weights for each time period)
  weights <- furrr::future_map_dbl((lag + 1):length(log_density_ratio), function(x) {
    weight <- exp(sum(log_density_ratio[(x - lag + 1): x]))
    return(weight)
  })

  # 2. Weighted smoothed outcome
  smoothed <- smoothed_outcome[(lag + 1):length(smoothed_outcome)] #Just smoothed outcomes

  # 2-1. Convert smoothed outcomes to arrays (pixels, for each time period)
  mat_im <- sapply(smoothed, function(x) spatstat.geom::as.matrix.im(x))
  pixels <- smoothed[[1]]$dim[1]
  mat_im <- array(mat_im, dim = c(pixels, pixels, length(smoothed)))

  # 2-2. Weigh smoothed outcomes for each time period by the weights (smoothed outcome x weight)
  mat_im_weighted <- sweep(mat_im, MARGIN = 3, STATS = weights, FUN = '*') #Weighted smoothed outcomes as matrices

  # 2-3. Get average weighted densities
  # Note: Don't use IPW for output (this functiton does not return IPW)
  weighted_surface_list <- lapply(1:dim(mat_im_weighted)[3], function(x) {
    spatstat.geom::as.im(mat_im_weighted[, , x], W = entire_window)}) #Weighted surfaces for each time frame (eg, each day)

  average_weighted_surface <- spatstat.geom::as.im(apply(mat_im_weighted, c(1, 2), mean),
                                                   W = entire_window) #This is IPW; one pixel image
  average_weighted_surface_haj <- average_weighted_surface / mean(weights) #Hajek; one pixel image

  # 3. Integrate over the window of interest (quantiles from a focus)

  message("Calculating expectations...\n")

  ## Get the range and quantiles of standardized distances
  distance_range <- range(`dist_map`$v, na.rm = TRUE)

  if (max(dist) > distance_range[2]) {
    stop("The max distance should be within the range of the entire window.")
  } #Confirm that max(distances) <= max(distance_range)

  distances <- c(dist, distance_range[2])

  #Note: another idea is to use quantiles, but this could take time when we obtain variances
  #distance_quantiles <- quantile(distance_range, probs = seq(0, 1, by = 0.01))

  ## Convert the distance map to windows
  distance_window <- matrix(`dist_map`$v, nrow = nrow(`dist_map`$v))
  distance_windows <- lapply(distances, function(x) distance_window < x) # A list of binary matrices based on quantiles
  distance_owin <- lapply(distance_windows, function(x) {
    spatstat.geom::owin(mask = x, xrange = `entire_window`$xrange, yrange = `entire_window`$yrange)
  }) # Owin objects with different distances from the focus

  ## Get the expectation depending on distances from focus
  partial_expectations <- lapply(distance_owin, function(x) {
      counter <- average_weighted_surface_haj
      spatstat.geom::Window(counter) <- x
      return(spatstat.geom::integral(counter))
  })

  # 4. Obtain the variance bound
  message("Obtaining the variance bound...\n")

  ## Empty matrix to save output (column = the num of distances of interest; row = timeframe)
  matrix_integrated_weighted_smooth_outcome <- matrix(NA,
                                                      nrow = length(smoothed),
                                                      ncol = length(distance_owin))

  for (i in 1:length(distance_owin)) {
    counts_based_on_distance_for_each_timeframe <-
      furrr::future_map(weighted_surface_list, function(y) {
        counter <- y
        spatstat.geom::Window(counter) <- distance_owin[[i]]
        return(spatstat.geom::integral(counter))
      })
    matrix_integrated_weighted_smooth_outcome[, i] <- unlist(counts_based_on_distance_for_each_timeframe)
    ## This is the matrix of integrated, weighted, and smoothed outcomes (i.e., counts for each time period)
    ## Each column represents different windows (different distances from the focus)
  }

  ## Get the variance upper bound

  ### IPW (don't use this for the final output)
  ### Formula: mean(weighted counts ^ 2)/length(time frame)
  bounds_ipw <- apply(matrix_integrated_weighted_smooth_outcome^2, 2, mean)/
    nrow(matrix_integrated_weighted_smooth_outcome)

  ### Hajek
  bounds_hajek <- bounds_ipw/(mean(weights)^2)

  # 4. Then plot (just one scenario; the remainder basically follows get_distance_based_expectation)

  message("Generating a plot...\n")

  ## Convert partial_expectations to a dataframe
  expectation_results <- as.numeric(unlist(partial_expectations))

  result_data <- data.frame(expectation = expectation_results,
                            distance = distances,
                            var_upper_bound = bounds_hajek)

  ## Plot for distance-based expectations

  x_label_text <- paste0("Distance from the focus (", dist_map_unit, ")")

  if(grayscale) {

      expectation_plot <- ggplot(result_data,
                                 aes(x = distance,
                                     y = expectation,
                                     ymin = expectation - 1.96*sqrt(var_upper_bound),
                                     ymax = expectation + 1.96*sqrt(var_upper_bound))) +
        ggplot2::geom_point() +
        ggplot2::geom_pointrange() +
        theme_bw() + xlim(0, ceiling(result_data$distance[nrow(result_data)])) +
        labs(x = x_label_text,
             y = "The expected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm"))

  } else {

      expectation_plot <- ggplot(result_data,
                                 aes(x = distance,
                                     y = expectation,
                                     ymin = expectation - 1.96*sqrt(bounds_hajek),
                                     ymax = expectation + 1.96*sqrt(bounds_hajek))) +
        ggplot2::geom_point() +
        ggplot2::geom_pointrange() +
        theme_bw() + xlim(0, ceiling(result_data$distance[nrow(result_data)])) +
        labs(x = x_label_text,
             y = "The expected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm"))

  }

  ## Plot for windows
  window_showcase_list <- list(distance_owin[[1]], distance_owin[[length(distance_owin)]])
  window_showcase <- lapply(window_showcase_list, spatstat.geom::as.polygonal)

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
    ggtitle(paste0(round(as.numeric(distances[1]), 1), " ", dist_map_unit)) +
    theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[2]] <- window_plot_list[[2]] +
    ggtitle(paste0(round(as.numeric(distances[length(distances)]), 1), " ", dist_map_unit)) +
    theme(plot.title = element_text(hjust = 0.5))

  w_plot_list <- list(window_plot_list[[1]], window_plot_list[[2]])

  # Color and plot
  window_plot <- ggpubr::ggarrange(plotlist = w_plot_list, nrow = 1)
  window_plot <- ggpubr::annotate_figure(window_plot, bottom = ggpubr::text_grob("Areas covered by quantiles"))

  entire_plot <- ggpubr::ggarrange(expectation_plot, window_plot, nrow = 2, heights = c(0.7, 0.3))
  titletext <- "The Expected Number of Outcome Events and\nDistance from the Focus under a Counterfactual Scenario"
  entire_plot <- ggpubr::annotate_figure(entire_plot, top = ggpubr::text_grob(titletext, face = "bold"))

  return(list(average_weighted_density = average_weighted_surface_haj, #Hajek, average, weighted surface (im)
              average_expected_events = as.numeric(unlist(partial_expectations)[101]), #Hajek, counts, entire window
              average_expected_events_quantiles = as.numeric(unlist(partial_expectations)), #Corresponding to each distance
              weights = weights, #Weights
              average_weights = mean(weights), #Average weights (for Hajek)
              plot = entire_plot, #Distance based expectations
              distances = as.numeric(distances),
              distances_window = distance_owin,
              expectation_plot = expectation_plot,
              window_plot = window_plot))

}
