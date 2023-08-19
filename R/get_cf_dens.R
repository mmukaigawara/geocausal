#' Get counterfactual densities
#'
#' @description `get_cf_dens` takes the target (expected) number, baseline density,
#' and power density, and generates a hyperframe with counterfactual densities.
#'
#' @param expected_number the expected number of observations.
#' @param base_dens baseline density (im object)
#' @param power_dens power density (im object)
#' @param window owin object
#' @param grayscale logical. `grayscale` specifies whether to convert plot to grayscale (by default, FALSE).
#'
#' @returns A list of counterfactual density (im object) and a ggplot object
#'
#' @details There are two ways of generating counterfactual densities.
#' First, users can keep the locations of observations as they are and change the expected number of observations.
#' In this case, users do not have to set `power_dens` and simply modify `expected_number`.
#' Alternatively, users can shift the locations as well. In this case, `power_dens` must be specified.
#' To obtain power densities, refer to [get_power_dens()].

get_cf_dens <- function(expected_number,
                        base_dens,
                        power_dens = NA,
                        window,
                        grayscale = FALSE) {

  if (is.na(power_dens[1])) { # counterfactual_type = intensity only -> multiply by the expectation

    counterfactual_density <- expected_number * base_dens

  } else { # counterfactual_type = location as well

    product_power_baseline <- base_dens * power_dens
    counterfactual_density <- product_power_baseline/
      integral(product_power_baseline, W = window) * expected_number

  }

  # ggplot figure

  ## Convert power density to a data frame
  cd_df <- as.data.frame(counterfactual_density)

  ## Pivot the data frame to a long format
  cd_df_long <- tidyr::pivot_longer(cd_df, cols = starts_with("V"), names_to = "variable", values_to = "value")

  if (grayscale) {

    counterfactual_dens <- ggplot() +
      ggplot2::geom_tile(data = cd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_distiller(type = "seq", direction = -1, palette = "Greys") +
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") +
      ggthemes::theme_map() +
      ggplot2::ggtitle("Counterfactual Density",
                       subtitle = paste0("The expected number of treatment events\nover the entire region per time period = ", expected_number)) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))

    } else {

    counterfactual_dens <- ggplot() +
      ggplot2::geom_tile(data = cd_df_long, aes(x = x, y = y, fill = value)) +
      ggplot2::scale_fill_viridis_c(option = "plasma") +
      ggplot2::geom_path(data = as.data.frame(window), aes(x = x, y = y), color = "white") +
      ggthemes::theme_map() +
      ggplot2::ggtitle("Counterfactual Density",
                       subtitle = paste0("The expected number of treatment events\nover the entire region per time period = ", expected_number)) +
      labs(fill = "Density") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5))

    }

  return(list(density = counterfactual_density, plot = counterfactual_dens))

}
