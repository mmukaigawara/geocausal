#' Function: get_estimates
#'
#' A function that takes a list of power densities
#' and returns simulated counterfactual densities
#'
#' @param counterfactual_simulation_results An output of the simulate_counterfactual_density function
#' @param density_of_interest A density that is being manipulated
#' @param entire_window An owin object (the entire region)
#' @param gray_scale By default, FALSE
#' @param expectation_use_raw Whether to use the actual expectation or proportion for the y-axis. By default, FALSE

get_estimates <- function(observed_density, 
                          counterfactual_density,
                          treatment_data, 
                          smoothed_outcome, 
                          lag,
                          entire_window, 
                          distance_map = dist_from_focus$distance_im,
                          distance_map_unit = "km",
                          grayscale = FALSE,
                          expectation_use_raw = FALSE) {
  
  # 1. Weight
  
  # 1-1. Log density
  counterfactual_sum_log <- get_counterfactual_sum_log_intensity(counterfactual_density = counterfactual_density,
                                                                 treatment_data = treatment_data)
  
  observed_sum_log <- observed_density$sum_log_intensity
  
  # 1-2. Log density ratio (LDR)
  log_density_ratio <- counterfactual_sum_log - observed_sum_log
  
  # 1-3. Convert LDR to weights
  weights <- furrr::future_map_dbl((lag + 1):length(log_density_ratio), function(x) {
    weight <- exp(sum(log_density_ratio[(x - lag + 1): x]))
    return(weight)
  })

  # 2. Weighted smoothed outcome
  smoothed <- smoothed_outcome[(lag + 1):length(smoothed_outcome)]
  
  # 2-1. Convert smoothed outcomes to arrays (pixels, for each time period)
  mat_im <- sapply(smoothed, function(x) spatstat.geom::as.matrix.im(x))
  pixels <- smoothed[[1]]$dim[1]
  mat_im <- array(mat_im, dim = c(pixels, pixels, length(smoothed)))
  
  # 2-2. Weigh each time period by the weights
  mat_im_weighted <- sweep(mat_im, MARGIN = 3, STATS = weights, FUN = '*') #Multiply by weights
  
  # 2-3. Get average weighted densities
  # Note: Don't use IPW for output (this funciton does not return IPW)
  average_weighted_surface <- spatstat.geom::as.im(apply(mat_im_weighted, c(1, 2), mean), 
                                                   W = entire_window) #This is IPW
  average_weighted_surface_haj <- average_weighted_surface / mean(weights) #Hajek
  
  # 3. Integrate over the window of interest (quantiles from a focus)
  
  ## Get the range and quantiles of standardized distances 
  distance_range <- range(`distance_map`$v, na.rm = TRUE)
  distance_quantiles <- quantile(distance_range, probs = seq(0, 1, by = 0.01))
  
  ## Convert the distance map to windows
  distance_window <- matrix(`distance_map`$v, nrow = nrow(`distance_map`$v))
  distance_windows <- lapply(distance_quantiles, function(x) distance_window < x) # A list of binary matrices based on quantiles
  distance_owin <- lapply(distance_windows, function(x) {
    spatstat.geom::owin(mask = x, xrange = `entire_window`$xrange, yrange = `entire_window`$yrange)
  }) # Owin objects
  
  ## Get the expectation depending on distances from focus
  partial_expectations <- lapply(distance_owin, function(x) {
      counter <- average_weighted_surface_haj
      spatstat.geom::Window(counter) <- x
      return(spatstat.geom::integral(counter))
  })
  
  # 4. Then plot (just one scenario; the remainder basically follows get_distance_based_expectation)
  
  ## Convert partial_expectations to a dataframe
  if (expectation_use_raw) {
    expectation_results <- as.numeric(unlist(partial_expectations))
  } else {
    expectation_results <- as.numeric(unlist(partial_expectations))/
      as.numeric(unlist(partial_expectations))[101] #Row = 0 to 100%, Column = Diff value of priorities
  }
  
  result_data <- data.frame(expectation = expectation_results,
                            distance = distance_quantiles)

  ## Plot for distance-based expectations
  
  x_label_text <- paste0("Distance from the focus (", distance_map_unit, ")")
  
  if(grayscale) {
    
    if (expectation_use_raw) {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation)) +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The expected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    } else {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation)) +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The proportion of\nexpected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        ylim(0, 1) + theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    }
    
  } else {
    
    if (expectation_use_raw) {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation)) +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The expected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    } else {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation)) +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The proportion of\nexpected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        ylim(0, 1) + theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    }
    
  }
  
  ## Plot for windows
  window_showcase_list <- list(distance_owin$`80%`, distance_owin$`60%`, 
                               distance_owin$`40%`, distance_owin$`20%`)
  window_showcase <- lapply(window_showcase_list, spatstat.geom::as.polygonal)
  
  if(grayscale) {
    
    window_plot_list <- lapply(window_showcase, function(x) {
      gg <- ggplot() + 
        ggplot2::geom_polygon(data = fortify(as.data.frame(x)), aes(x = x, y = y), fill = "gray") +
        ggplot2::geom_path(data = fortify(as.data.frame(entire_window)), aes(x = x, y = y)) +
        ggthemes::theme_map()
      return(gg)})
    
  } else {
    
    window_plot_list <- lapply(window_showcase, function(x) {
      gg <- ggplot() + 
        ggplot2::geom_polygon(data = fortify(as.data.frame(x)), aes(x = x, y = y), fill = "#F1B6DA") +
        ggplot2::geom_path(data = fortify(as.data.frame(entire_window)), aes(x = x, y = y)) +
        ggthemes::theme_map()
      return(gg)})
  }
  
  window_plot_list[[1]] <- window_plot_list[[1]] + 
    ggtitle(paste0(round(as.numeric(distance_quantiles["80%"]), 1), "km from the focus \n(", 80, " percentile)")) + 
    theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[2]] <- window_plot_list[[2]] + 
    ggtitle(paste0(round(as.numeric(distance_quantiles["60%"]), 1), "km from the focus \n(", 60, " percentile)")) + 
    theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[3]] <- window_plot_list[[3]] + 
    ggtitle(paste0(round(as.numeric(distance_quantiles["40%"]), 1), "km from the focus \n(", 40, " percentile)")) + 
    theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[4]] <- window_plot_list[[4]] + 
    ggtitle(paste0(round(as.numeric(distance_quantiles["20%"]), 1), "km from the focus \n(", 20, " percentile)")) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  w_plot_list <- list(window_plot_list[[4]], window_plot_list[[3]], window_plot_list[[2]], window_plot_list[[1]])
  
  # Color and plot
  window_plot <- ggpubr::ggarrange(plotlist = w_plot_list, nrow = 1)
  window_plot <- ggpubr::annotate_figure(window_plot, bottom = ggpubr::text_grob("Areas covered by quantiles"))
  
  entire_plot <- ggpubr::ggarrange(expectation_plot, window_plot, nrow = 2, heights = c(0.7, 0.3))
  titletext <- "The Expected Number of Outcome Events and\nDistance from the Focus under a Counterfactual Scenario"
  entire_plot <- ggpubr::annotate_figure(entire_plot, top = ggpubr::text_grob(titletext, face = "bold"))
  
  return(entire_plot)
  
  return(list(average_weighted_density = average_weighted_surface_haj, #Hajek, average, weighted surface (im)
              average_expected_events = as.numeric(unlist(partial_expectations)[101]), #Hajek, counts, entire window
              average_expected_events_quantiles = as.numeric(unlist(partial_expectations)), #From 0 to 100% quantiles of areas
              weights = weights, #Weights
              average_weights = mean(weights), #Average weights (for Hajek)
              plot = entire_plot #Distance based expectations
              ))
  
}