#' Get the expectation of treatment events with arbitrary distances
#'
#' @description
#' `get_distexp()` takes counterfactual densities and
#' and returns the expected number of treatment events based on distances
#' from a user-specified focus.
#'
#' @param cf_sim_results output of `sim_cf_dens()`
#' @param entire_window owin object of the entire region
#' @param dist_map im object whose cell values are the distance from a focus (e.g., city)
#' @param dist_map_unit either `"km"` or `"mile"`
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#' @param use_raw logical. `use_raw` specifies whether to use the raw value of expectations or percentiles.
#' By default, `FALSE`.
#'
#' @returns A list of ggplot objects that summarizes how expectations change over distances from a focus (`expectation_plot`)
#' and summarizes distances and areas (`window_plot`). Note that the second object can not necessarily be well drawn
#' depending on how windows are defined.

get_distexp <- function(cf_sim_results,
                        entire_window,
                        dist_map,
                        dist_map_unit = "km",
                        grayscale = FALSE,
                        use_raw = FALSE) {
  
  # Get the range and quantiles of standardized distances
  distance_range <- range(`dist_map`$v, na.rm = TRUE)
  distance_quantiles <- quantile(distance_range, probs = seq(0, 1, by = 0.01))
  
  # Convert the distance map to windows
  distance_window <- matrix(`dist_map`$v, nrow = nrow(`dist_map`$v))
  distance_windows <- lapply(distance_quantiles, function(x) distance_window < x) # A list of binary matrices based on quantiles
  distance_owin <- lapply(distance_windows, function(x) {
    spatstat.geom::owin(mask = x, xrange = `entire_window`$xrange, yrange = `entire_window`$yrange)
  }) # Owin objects
  
  # Get the expectation
  partial_expectations <- lapply(distance_owin, function(x) {
    lapply(`cf_sim_results`$densities, function(y) {
      counter <- y
      spatstat.geom::Window(counter) <- x
      return(spatstat.geom::integral(counter))
    })
  })
  
  # Convert it to a dataframe
  if (use_raw) {
    expectation_results <- data.table::rbindlist(partial_expectations)
  } else {
    expectation_results <- data.table::rbindlist(partial_expectations)/
      spatstat.geom::integral(`cf_sim_results`$densities[[1]]) #Row = 0 to 100%, Column = Diff value of priorities
  }
  
  result_data <- data.frame(expectation = c(unlist(expectation_results)),
                            alpha = rep(`cf_sim_results`$priorities, each = 101),
                            distance = distance_quantiles)
  result_data$alpha <- factor(result_data$alpha)
  
  # Plot
  
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
      #theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm"))
      
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
      #theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm"))
      
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
      #theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm"))
      
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
      #theme(plot.margin = margin(0.1, 0.1, 1, 0.1, "cm"))
      
    }
    
  }
  
  # Plot for windows
  window_showcase_list <- list(distance_owin$`80%`, distance_owin$`60%`,
                               distance_owin$`40%`, distance_owin$`20%`)
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
  #entire_plot <- ggpubr::ggarrange(expectation_plot, window_plot, nrow = 2, heights = c(0.7, 0.3))
  #titletext <- "The Expected Number of Treatment Events and Distance from the Focus"
  #entire_plot <- ggpubr::annotate_figure(entire_plot, top = ggpubr::text_grob(titletext, face = "bold"))
  
  return(list(expectation_plot = expectation_plot, 
              window_plot = window_plot))
  
}
