#' Generate a figure with causal contrasts
#'
#' @description A function that returns a figure with causal contrasts
#'
#' @param scenario_1 A counterfactual scenario (the output of get_estimates function)
#' @param scenario_2 Another counterfactual scenario
#' @param grayscale Whether to grayscale
#' @param use_raw If TRUE, the plot displays the raw expectations (if FALSE, max is set to 1)
#' 
#' @returns A list of ggplot objects and the data
 
get_causal_cont <- function(scenario_1, #Contrast = scenario 2 - scenario 1
                            scenario_2,
                            grayscale, 
                            use_raw) {
  
  
  # Create a dataframe for causal contrast
  causal_contrast <- scenario_2$average_expected_events_quantiles - 
    scenario_1$average_expected_events_quantiles
  
  distance_quantiles <- scenario_1$distance_quantiles
  
  result_data <- data.frame(expectation = causal_contrast,
                            distance = distance_quantiles)
  
  # Plot for distance-based expectations
  
  x_label_text <- paste0("Distance from the focus (", distance_map_unit, ")")
  
  if(grayscale) {
    
    if (use_raw) {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The expected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    } else {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The proportion of\nexpected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        ylim(0, 1) + theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    }
    
  } else {
    
    if (use_raw) {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The expected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    } else {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The proportion of\nexpected outcome events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        ylim(0, 1) + theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    }
    
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