#' Function: get_baseline_summary
#'
#' A function that takes a column of dataframe and generates a plot and a summary
#'
#' @param data Data
#' @param time_column The name of the column for time variable 

get_baseline_summary <- function(data, time_column) {

  # Generate a plot of counts over the entire time period
  dat_count <- airstr_base %>%
    dplyr::group_by(.data[[time_column]]) %>% #time_column should be quoted. Otherwise {{time_column}}
    dplyr::summarize(count = dplyr::n())
  
  dat_count_plot <- ggplot2::ggplot(dat_count, aes(x = .data[[time_column]], y = count)) +
    ggplot2::geom_col() + 
    theme_bw() +
    ggplot2::ggtitle("The number of treatment per time period") +
    labs(x = "Time", y = "Count") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Generate a summary
  quantiles <- quantile(dat_count$count, 
                        probs = c(0, 0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 
                                  0.6, 0.7, 0.75, 0.8, 0.9, 1))
  average <- mean(dat_count$count)
  var <- var(dat_count$count)
  
  return(list(plot = dat_count_plot, quantile = quantiles, mean = average, variance = var))
}
