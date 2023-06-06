#' Function: get_distance_based_expectation
#'
#' A function that takes a list of power densities
#' and returns simulated counterfactual densities
#'
#' @param counterfactual_simulation_results An output of the simulate_counterfactual_density function
#' @param density_of_interest A density that is being manipulated
#' @param entire_window An owin object (the entire region)
#' @param gray_scale By default, FALSE

get_distance_based_expectation <- function(counterfactual_simulation_results,
                                           entire_window,
                                           density_of_interest,
                                           grayscale = FALSE) {
  
  # Get the range and quantiles of standardized distances 
  distance_range <- range(`density_of_interest`$v, na.rm = TRUE)
  distance_quantiles <- quantile(distance_range, probs = seq(0, 1, by = 0.01))
  
  # Convert the distance map to windows
  distance_window <- matrix(`density_of_interest`$v, nrow = nrow(`density_of_interest`$v))
  distance_windows <- lapply(distance_quantiles, function(x) distance_window > x) # A list of binary matrices based on quantiles
  distance_owin <- lapply(distance_windows, function(x) {
    owin(mask = x, xrange = `entire_window`$xrange, yrange = `entire_window`$yrange)
  }) # Owin objects
  
  # Get the expectation
  partial_expectations <- lapply(distance_owin, function(x) {
    lapply(`counterfactual_simulation_results`$densities, function(y) {
      counter <- y
      spatstat.geom::Window(counter) <- x
      return(integral(counter))
    })
  })
  
  # Convert it to a dataframe
  expectation_results <- data.table::rbindlist(partial_expectations)/
    integral(`counterfactual_simulation_results`$densities[[1]]) #Row = 0 to 100%, Column = Diff value of priorities
  
  result_data <- data.frame(expectation = c(unlist(expectation_results)),
                            alpha = rep(`counterfactual_simulation_results`$priorities, each = 101),
                            distance = 1 - rep(seq(0, 1, by = 0.01), length(`counterfactual_simulation_results`$priorities)))
  result_data$alpha <- factor(result_data$alpha)
  
  # Plot
  if(grayscale) {
    
    expectation_plot <- ggplot(result_data) +
      ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
      theme_bw() +
      labs(x = "Quantiles of Gaussian distances from the focus", 
           y = "The proportion of\nexpected treatment events\ncovered by the area",
           color = latex2exp::TeX("$\\alpha_{focus}$")) +
      ggplot2::scale_color_brewer(palette = "Greys") +
      xlim(0, 1) + ylim(0, 1) + theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
    
  } else {
    
    expectation_plot <- ggplot(result_data) +
      ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
      theme_bw() +
      labs(x = "Quantiles of Gaussian distances from the focus", 
           y = "The proportion of\nexpected treatment events\ncovered by the area",
           color = latex2exp::TeX("$\\alpha_{focus}$")) +
      ggplot2::scale_color_brewer(palette = "PiYG") +
      xlim(0, 1) + ylim(0, 1) + theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
    
  }
  
  
  # Plot for windows
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
    ggtitle("20%") + theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[2]] <- window_plot_list[[2]] + 
    ggtitle("40%") + theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[3]] <- window_plot_list[[3]] + 
    ggtitle("60%") + theme(plot.title = element_text(hjust = 0.5))
  window_plot_list[[4]] <- window_plot_list[[4]] + 
    ggtitle("80%") + theme(plot.title = element_text(hjust = 0.5))
  
  # Color and plot
  window_plot <- ggpubr::ggarrange(plotlist = window_plot_list, nrow = 1)
  window_plot <- ggpubr::annotate_figure(window_plot, bottom = ggpubr::text_grob("Areas covered by quantiles"))
  
  entire_plot <- ggpubr::ggarrange(expectation_plot, window_plot, nrow = 2)
  titletext <- "The Expected Number of Treatment Events and\nDistances from the Focus"
  entire_plot <- ggpubr::annotate_figure(entire_plot, top = ggpubr::text_grob(titletext, face = "bold"))
  
  return(entire_plot)
  
}