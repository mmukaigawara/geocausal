#' Estimate unit-level propensity scores
#'
#' @description
#' `get_unit_ps()` estimates treatment probabilities from a complete
#' time-by-unit data frame of discrete spatial or administrative units. It
#' supports a pooled logistic
#' regression and, when `geepack` is installed, a generalized estimating
#' equation (GEE). The returned object also contains inverse-probability weights
#' and standardized-difference balance diagnostics.
#'
#' @param formula a two-sided model formula. The left-hand side must be the name
#'   of a binary treatment column in `data`. The right-hand side lists the
#'   covariates used to estimate the treatment probability.
#' @param data a data frame in long format with one row for every combination
#'   of time period and unit. It must form a complete time-by-unit data frame.
#'   It is not an array.
#' @param time a character string giving the name of the time column in `data`.
#' @param unit a character string giving the name of the unit identifier column
#'   in `data`.
#' @param engine estimation engine: `"glm"` (the default) or `"gee"`.
#' @param cluster character string naming the GEE cluster column. If `NULL`,
#'   `time` is used. Ignored when `engine = "glm"`.
#' @param clip numeric vector of length two giving the lower and upper bounds
#'   applied to fitted treatment probabilities.
#' @param balance optional character vector naming variables for which absolute
#'   standardized differences should be computed. By default, variables used in
#'   `formula` are included.
#' @param ... additional arguments passed to [stats::glm()] or
#'   [geepack::geeglm()].
#'
#' @returns An object of class `unit_ps`, a list containing:
#' * `model`: fitted propensity-score model.
#' * `fitted`: clipped probabilities in time-unit order.
#' * `propensity`: a time by unit probability matrix.
#' * `observed_weight`: inverse probability of the observed treatment.
#' * `balance`: tidy unweighted and weighted balance diagnostics.
#' * `data_info` and `call`: model metadata.
#'
#' @details
#' The input must contain one row for each `time` and `unit` combination.
#' Numeric balance variables are compared using standardized mean differences.
#' Factors and character variables are expanded into level indicators whose
#' labels appear in the `variable` column, such as `region: north`.
#'
#' @seealso [get_policy_value()], [get_policy()]
#' @family unit-level policy functions
#'
#' @examples
#' data <- expand.grid(time = 1:5, unit = letters[1:4])
#' data$x <- rep(seq(-1, 1, length.out = 4), 5)
#' data$treatment <- as.integer(data$x + data$time / 10 > 0)
#' ps <- get_unit_ps(treatment ~ x, data, time = "time", unit = "unit")
#' print(ps)
#' plot(ps, type = "overlap")
#'
#' @export
get_unit_ps <- function(formula, data, time, unit,
                        engine = c("glm", "gee"), cluster = NULL,
                        clip = c(0.01, 0.99), balance = NULL, ...) {
  engine <- match.arg(engine)
  model_data <- data
  data_index <- .prepare_unit_data(data, time, unit)
  if (!inherits(formula, "formula") || length(formula) != 3L) {
    stop("`formula` must be a two-sided model formula.", call. = FALSE)
  }
  response <- all.vars(formula[[2]])
  if (length(response) != 1L || !response %in% names(data_index$data)) {
    stop("The formula response must name one column in `data`.", call. = FALSE)
  }
  treatment <- data_index$data[[response]]
  if (anyNA(treatment) || any(!treatment %in% c(0, 1))) {
    stop("The propensity-model response must contain only 0 and 1.",
         call. = FALSE)
  }
  if (!is.numeric(clip) || length(clip) != 2L || anyNA(clip) ||
      clip[1] < 0 || clip[2] > 1 || clip[1] >= clip[2]) {
    stop("`clip` must be increasing probability bounds between 0 and 1.",
         call. = FALSE)
  }

  if (engine == "glm") {
    model <- stats::glm(formula, data = model_data,
                        family = stats::binomial(), ...)
  } else {
    if (!requireNamespace("geepack", quietly = TRUE)) {
      stop("Package `geepack` is required for `engine = \"gee\"`.",
           call. = FALSE)
    }
    if (is.null(cluster)) {
      cluster <- time
    }
    if (!is.character(cluster) || length(cluster) != 1L ||
        !cluster %in% names(data_index$data)) {
      stop("`cluster` must name a column in `data`.", call. = FALSE)
    }
    model_data$.geocausal_cluster <- model_data[[cluster]]
    model <- geepack::geeglm(formula, data = model_data,
                             id = .geocausal_cluster,
                             family = stats::binomial(), ...)
  }

  fitted_model_order <- as.numeric(stats::predict(model, type = "response"))
  fitted_model_order <- pmin(pmax(fitted_model_order, clip[1]), clip[2])
  fitted <- fitted_model_order[data_index$order]
  observed_weight <- 1 / ifelse(treatment == 1, fitted, 1 - fitted)
  propensity <- .data_matrix(fitted, data_index, "fitted propensity")

  if (is.null(balance)) {
    balance <- setdiff(all.vars(formula), response)
  }
  balance_result <- .unit_balance(data_index$data, treatment, observed_weight, balance)

  data_info <- list(
    time = time,
    unit = unit,
    time_levels = data_index$time_levels,
    unit_levels = data_index$unit_levels,
    row_order = data_index$order,
    response = response,
    treatment = treatment,
    engine = engine,
    cluster = if (engine == "gee") cluster else NULL
  )
  out <- list(
    model = model,
    fitted = fitted,
    propensity = propensity,
    observed_weight = observed_weight,
    balance = balance_result,
    data_info = data_info,
    call = match.call()
  )
  class(out) <- c("unit_ps", "list")
  out
}

.unit_balance <- function(data, treatment, weight, variables) {
  empty <- data.frame(variable = character(), type = character(),
                      unweighted = numeric(),
                      weighted = numeric(), stringsAsFactors = FALSE)
  if (!length(variables)) {
    return(empty)
  }
  if (!is.character(variables) || any(!variables %in% names(data))) {
    stop("`balance` must name columns in `data`.", call. = FALSE)
  }
  rows <- lapply(variables, function(variable) {
    x <- data[[variable]]
    if (is.numeric(x) || is.logical(x)) {
      return(data.frame(
        variable = variable,
        type = "numeric",
        unweighted = .absolute_smd(as.numeric(x), treatment),
        weighted = .absolute_smd(as.numeric(x), treatment, weight),
        stringsAsFactors = FALSE
      ))
    }
    x <- factor(x)
    do.call(rbind, lapply(levels(x), function(level) {
      indicator <- as.numeric(x == level)
      data.frame(
        variable = paste0(variable, ": ", level),
        type = "categorical",
        unweighted = .absolute_smd(indicator, treatment),
        weighted = .absolute_smd(indicator, treatment, weight),
        stringsAsFactors = FALSE
      )
    }))
  })
  rownames(empty) <- NULL
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.absolute_smd <- function(x, treatment, weight = rep(1, length(x))) {
  if (anyNA(x)) {
    return(NA_real_)
  }
  mean_and_var <- function(group) {
    use <- treatment == group
    w <- weight[use]
    values <- x[use]
    w <- w / sum(w)
    mean_value <- sum(w * values)
    variance <- sum(w * (values - mean_value)^2)
    c(mean = mean_value, variance = variance)
  }
  treated <- mean_and_var(1)
  untreated <- mean_and_var(0)
  scale <- sqrt((treated["variance"] + untreated["variance"]) / 2)
  difference <- abs(treated["mean"] - untreated["mean"])
  if (scale < .Machine$double.eps) {
    return(if (difference < .Machine$double.eps) 0 else Inf)
  }
  unname(difference / scale)
}
