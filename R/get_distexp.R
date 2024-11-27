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
#' @param use_raw logical. `use_raw` specifies whether to use the raw value of expectations or percentiles.
#' By default, `FALSE`.
#'
#' @returns A list of resulting dataframe (`result_data`), windows (`window_list`), data for distance quantiles,
#' and a window object for the entire window

get_distexp <- function(cf_sim_results,
                        entire_window,
                        dist_map,
                        dist_map_unit = "km",
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
    lapply(`cf_sim_results`$cf_density_list, function(y) {
      counter <- y
      spatstat.geom::Window(counter) <- x
      return(spatstat.univar::integral(counter))
    })
  })

  # Convert it to a dataframe
  if (use_raw) {
    expectation_results <- data.table::rbindlist(partial_expectations)
  } else {
    expectation_results <- data.table::rbindlist(partial_expectations)/
      spatstat.univar::integral(`cf_sim_results`$cf_density_list[[1]]) #Row = 0 to 100%, Column = Diff value of priorities
  }

  result_data <- data.frame(expectation = c(unlist(expectation_results)),
                            alpha = rep(`cf_sim_results`$powers, each = 101),
                            distance = distance_quantiles)
  result_data$alpha <- factor(result_data$alpha)

  # Window data
  window_showcase_list <- list(distance_owin$`80%`, distance_owin$`60%`,
                               distance_owin$`40%`, distance_owin$`20%`)

  distlist <- list(result_data = result_data,
                   window_list = window_showcase_list,
                   distance_quantiles = distance_quantiles,
                   entire_window = entire_window)
  class(distlist) <- c("distlist", "list")

  return(distlist)

}
