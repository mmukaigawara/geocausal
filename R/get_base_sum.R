#' Summarize baseline densities
#'
#' @description `get_base_sum()` takes dataframe and summarizes it.
#'
#' @param data dataframe
#' @param time_column the name of time variable
#' 
#' @returns list of the following:
#'     * `plot`: ggplot object that shows temporal distribution of data
#'     * `quantile`: quantiles of counts 
#'     * `mean`: the mean
#'     * `variance`: the variance
#'     
#' @examples
#' get_base_sum(data = airstrikes_base, 
#'              time_column = "date")

get_base_sum <- function(data, time_column) {

  # Generate a plot of counts over the entire time period
  dat_count <- data %>%
    dplyr::group_by(.data[[time_column]]) %>% #time_column should be quoted. Otherwise {{time_column}}
    dplyr::summarize(count = dplyr::n())
  
  dat_count_plot <- ggplot2::ggplot(dat_count, aes(x = .data[[time_column]], y = count)) +
    ggplot2::geom_col() + 
    theme_bw() +
    ggplot2::ggtitle("The number of treatment events\nover the entire region per time period") +
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
