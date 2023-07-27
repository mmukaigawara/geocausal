#' Generate a figure with causal contrasts
#'
#' @description A function that returns a figure with causal contrasts
#'
#' @param scenario_1 a counterfactual scenario (the output of `get_estimates()` function)
#' @param scenario_2 another counterfactual scenario
#' @param grayscale whether to grayscale the figures
#' @param dist_map_unit either `"km"` or `"mile"`
#' 
#' @returns a list of ggplot objects and the data
#' 
#' @details `get_causal_cont()` returns a figure with causal contrasts. 
#' A causal contrast is the difference between expected numbers of outcome events of two counterfactual scenarios. 
#' The baseline scenario is `scenario_1`.

get_causal_cont <- function(scenario_1, #Contrast = scenario 2 - scenario 1
                            scenario_2,
                            dist_map_unit = "km",
                            grayscale) {
  
  
  # Create a dataframe for causal contrast
  causal_contrast <- scenario_2$average_expected_events_quantiles - 
    scenario_1$average_expected_events_quantiles
  
  distances <- scenario_1$distances
  
  # Variance upper bound
  bound_tau_hajek <- mean(causal_contrast^2 / length(causal_contrast))
  
  result_data <- data.frame(expectation = causal_contrast,
                            distance = distances,
                            var_upper_bound = bound_tau_hajek)
  
  # Plot for distance-based expectations
  
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
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        labs(x = x_label_text, 
             y = "The expected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 

  } else {
    
      expectation_plot <- ggplot(result_data, 
                                 aes(x = distance, 
                                     y = expectation,
                                     ymin = expectation - 1.96*sqrt(var_upper_bound),
                                     ymax = expectation + 1.96*sqrt(var_upper_bound))) +
        ggplot2::geom_point() +
        ggplot2::geom_pointrange() +
        theme_bw() + xlim(0, ceiling(result_data$distance[nrow(result_data)])) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        labs(x = x_label_text, 
             y = "The expected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
  }
  
  # Color and plot
  window_plot <- scenario_1$window_plot
  
  entire_plot <- ggpubr::ggarrange(expectation_plot, window_plot, nrow = 2, heights = c(0.7, 0.3))
  titletext <- "Causal Effects Per Time Period:\nThe Expected Number of Outcome Events\nComparing Two Counterfactual Scenarios"
  entire_plot <- ggpubr::annotate_figure(entire_plot, top = ggpubr::text_grob(titletext, face = "bold"))
  
  return(list(plot = entire_plot,
              expectation_plot = expectation_plot,
              df = result_data)
         )
  
}