#' Function: get_distance_based_expectation
#'
#' A function that takes a list of power densities
#' and returns simulated counterfactual densities
#'
#' @param counterfactual_simulation_results An output of the simulate_counterfactual_density function
#' @param entire_window An owin object (the entire region)
#' @param density_of_interest A density that is being manipulated
#' @param distance_map An im object (map) whose cell values are the distance from the focus (e.g., city)
#' @param distance_map_unit e.g., km, mile
#' @param gray_scale By default, FALSE
#' @param expectation_use_raw Whether to use the actual expectation or proportion for the y-axis. By default, FALSE

get_distance_based_expectation <- function(counterfactual_simulation_results,
                                           entire_window,
                                           density_of_interest,
                                           distance_map,
                                           distance_map_unit = "km",
                                           grayscale = FALSE,
                                           expectation_use_raw = FALSE) {

  # Get the range and quantiles of standardized distances 
  distance_range <- range(`distance_map`$distance_im$v, na.rm = TRUE)
  distance_quantiles <- quantile(distance_range, probs = seq(0, 1, by = 0.01))
  
  # Convert the distance map to windows
  distance_window <- matrix(`distance_map`$distance_im$v, nrow = nrow(`distance_map`$distance_im$v))
  distance_windows <- lapply(distance_quantiles, function(x) distance_window < x) # A list of binary matrices based on quantiles
  distance_owin <- lapply(distance_windows, function(x) {
    spatstat.geom::owin(mask = x, xrange = `entire_window`$xrange, yrange = `entire_window`$yrange)
  }) # Owin objects

  # Get the expectation
  partial_expectations <- lapply(distance_owin, function(x) {
    lapply(`counterfactual_simulation_results`$densities, function(y) {
      counter <- y
      spatstat.geom::Window(counter) <- x
      return(spatstat.geom::integral(counter))
    })
  })

  # Convert it to a dataframe
  if (expectation_use_raw) {
    expectation_results <- data.table::rbindlist(partial_expectations)
  } else {
    expectation_results <- data.table::rbindlist(partial_expectations)/
      spatstat.geom::integral(`counterfactual_simulation_results`$densities[[1]]) #Row = 0 to 100%, Column = Diff value of priorities
  }
  
  result_data <- data.frame(expectation = c(unlist(expectation_results)),
                            alpha = rep(`counterfactual_simulation_results`$priorities, each = 101),
                            distance = distance_quantiles)
  result_data$alpha <- factor(result_data$alpha)

  # Plot
  
  x_label_text <- paste0("Distance from the focus (", distance_map_unit, ")")
  
  if(grayscale) {
    
    if (expectation_use_raw) {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The expected treatment events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    } else {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The proportion of\nexpected treatment events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "Greys") +
        ylim(0, 1) + theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    }
    
  } else {
    
    if (expectation_use_raw) {
      
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The expected treatment events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "PiYG") +
        theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
      
    } else {
    
      expectation_plot <- ggplot(result_data) +
        ggplot2::geom_line(aes(x = distance, y = expectation, group = alpha, color = alpha)) +
        theme_bw() +
        labs(x = x_label_text, 
             y = "The proportion of\nexpected treatment events\ncovered by the area",
             color = latex2exp::TeX("$\\alpha_{focus}$")) +
        ggplot2::scale_color_brewer(palette = "PiYG") +
        ylim(0, 1) + theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm")) 
        
    }
    
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
  titletext <- "The Expected Number of Treatment Events and\nDistance from the Focus"
  entire_plot <- ggpubr::annotate_figure(entire_plot, top = ggpubr::text_grob(titletext, face = "bold"))
  
  return(entire_plot)
  
}