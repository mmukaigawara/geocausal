#' Evaluate a stochastic policy for unit-level data
#'
#' @description
#' `get_policy_value()` estimates the value of a stochastic policy for a
#' complete time-by-unit data frame. It uses a first- or second-order
#' linearized inverse-probability weight, allowing the number of units to be
#' large without enumerating treatment assignments.
#'
#' @param data a data frame in long format with one row for every combination
#'   of time period and unit. It must form a complete time-by-unit data frame.
#'   It is not an array.
#' @param outcome a character string giving the name of the numeric outcome
#'   column in `data`. Missing outcomes are allowed, but every evaluated time
#'   period must contain at least one observed outcome.
#' @param treatment a character string giving the name of the treatment column
#'   in `data`. The column must contain only 0 and 1.
#' @param propensity the probability of each observed treatment assignment.
#'   Supply either the name of a numeric column in `data`, a numeric vector in
#'   the original row order of `data`, a matrix with one row per time period and
#'   one column per unit, or an object returned by [get_unit_ps()]. Every
#'   probability must be strictly between 0 and 1.
#' @param time a character string giving the name of the time column in `data`.
#' @param unit a character string giving the name of the unit identifier column
#'   in `data`.
#' @param policy a one-sided formula defining the policy design matrix. History
#'   or lagged variables must already be present in `data`.
#' @param policy_coef a numeric vector for a one-stage policy or a matrix with
#'   one row per policy stage. Named columns are matched to the columns produced
#'   by [stats::model.matrix()].
#' @param order order of the linearized inverse-probability weight, either 1 or
#'   2.
#' @param stabilize weight stabilization method: `"mixed"` reproduces the
#'   smooth mixture of normalized and recentered weights, `"normalize"` divides
#'   by the mean time weight, and `"none"` leaves weights unchanged.
#' @param fixed_prob optional numeric vector of policy probabilities for units
#'   whose treatment probability is fixed. A named vector may contain a subset
#'   of units. An unnamed vector must contain one value per unit.
#' @param outcome_units optional character vector of unit IDs whose outcomes
#'   should be averaged. Policy weights are still constructed from every unit.
#'   The default uses all units. This is useful for evaluating a learned policy
#'   for one unit or a group of units without changing the policy weights.
#' @param conf_level confidence level for the normal approximation interval.
#' @param keep amount of intermediate information to retain. `"standard"`
#'   omits unit likelihood ratios. `"all"` retains them for diagnostics and
#'   exact replication work.
#'
#' @returns An object of class `policy_value`, a list containing the overall and
#' stage-specific values, variance and confidence interval, a `unit_outcome`
#' data frame with each unit's expected outcome and interval, expected
#' treatment summaries by stage and unit, time-level influence contributions,
#' policy probabilities, optional unit likelihood ratios, stabilized time
#' weights, stabilization diagnostics, and the complete specification needed
#' to interpret the estimate.
#'
#' @details
#' Let `r` be the likelihood ratio for a unit: `q / p` for a treated unit and
#' `(1 - q) / (1 - p)` otherwise, where `p` is its observed propensity and `q`
#' is its probability under the policy. With `z = r - 1`, the first-order time
#' weight is `1 + sum(z)`. The second-order weight additionally includes every
#' pair product and is evaluated as
#' `0.5 * ((sum(z))^2 - sum(z^2))`.
#'
#' For a multi-stage policy, row 1 of `policy_coef` describes the latest stage
#' and the last row describes the earliest stage in each evaluation window.
#' Stage weights are multiplied recursively to represent the policy history.
#'
#' @seealso [get_unit_ps()], [get_policy()]
#' @family unit-level policy functions
#'
#' @examples
#' data <- expand.grid(time = 1:6, unit = letters[1:4])
#' data$x <- rep(seq(-1, 1, length.out = 4), 6)
#' data$treatment <- rep(c(0, 1, 0, 1), 6)
#' data$ps <- 0.5
#' data$outcome <- data$treatment + data$x + data$time / 10
#'
#' value <- get_policy_value(
#'   data = data, outcome = "outcome", treatment = "treatment",
#'   propensity = "ps", time = "time", unit = "unit", policy = ~ x,
#'   policy_coef = c("(Intercept)" = 0, x = 0.5), order = 1
#' )
#' print(value)
#'
#' @export
get_policy_value <- function(data, outcome, treatment, propensity, time, unit,
                             policy, policy_coef, order = 1L,
                             stabilize = c("mixed", "normalize", "none"),
                             fixed_prob = NULL, outcome_units = NULL,
                             conf_level = 0.95,
                             keep = c("standard", "all")) {
  stabilize <- match.arg(stabilize)
  keep <- match.arg(keep)
  if (length(order) != 1L || !order %in% c(1, 2)) {
    stop("`order` must be 1 or 2.", call. = FALSE)
  }
  order <- as.integer(order)
  if (!is.numeric(conf_level) || length(conf_level) != 1L ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a number strictly between 0 and 1.",
         call. = FALSE)
  }
  data_info <- .prepare_unit_data(data, time, unit)
  for (column in c(outcome, treatment)) {
    if (!is.character(column) || length(column) != 1L ||
        !column %in% names(data_info$data)) {
      stop("`outcome` and `treatment` must name columns in `data`.",
           call. = FALSE)
    }
  }
  treatment_matrix <- .data_matrix(data_info$data[[treatment]], data_info, treatment,
                                    binary = TRUE)
  outcome_matrix <- .data_matrix(data_info$data[[outcome]], data_info, outcome,
                                  allow_na = TRUE)
  if (any(!is.finite(outcome_matrix[!is.na(outcome_matrix)]))) {
    stop("The outcome must contain only finite values or `NA`.", call. = FALSE)
  }
  propensity_matrix <- .resolve_propensity(propensity, data_info)
  design <- .policy_design(policy, data_info)
  coefficient <- .policy_coef_matrix(policy_coef, colnames(design))
  horizon <- nrow(coefficient)
  if (horizon > data_info$n_time) {
    stop("The policy horizon cannot exceed the number of time periods.",
         call. = FALSE)
  }
  fixed <- .fixed_policy_prob(fixed_prob, data_info$unit_levels)
  outcome_index <- .resolve_outcome_units(outcome_units, data_info$unit_levels)

  evaluated <- .evaluate_unit_policy(
    data_info = data_info,
    outcome = outcome_matrix,
    treatment = treatment_matrix,
    propensity = propensity_matrix,
    design = design,
    coefficient = coefficient,
    order = order,
    stabilize = stabilize,
    fixed_prob = fixed,
    outcome_index = outcome_index
  )

  critical <- stats::qnorm(1 - (1 - conf_level) / 2)
  std_error <- sqrt(evaluated$variance)
  unit_std_error <- sqrt(evaluated$unit_variance)
  unit_outcome <- data.frame(
    unit = data_info$unit_levels,
    estimate = evaluated$unit_estimate,
    std_error = unit_std_error,
    conf_low = evaluated$unit_estimate - critical * unit_std_error,
    conf_high = evaluated$unit_estimate + critical * unit_std_error,
    stringsAsFactors = FALSE
  )
  specification <- list(
    outcome = outcome,
    treatment = treatment,
    propensity = if (is.character(propensity)) propensity else class(propensity)[1],
    time = time,
    unit = unit,
    policy = policy,
    coefficient = coefficient,
    order = order,
    stabilize = stabilize,
    fixed_prob = fixed,
    outcome_units = data_info$unit_levels[outcome_index],
    conf_level = conf_level,
    keep = keep,
    time_levels = data_info$time_levels,
    unit_levels = data_info$unit_levels
  )
  out <- list(
    estimate = sum(evaluated$horizon_estimate),
    horizon_estimate = evaluated$horizon_estimate,
    variance = evaluated$variance,
    std_error = std_error,
    conf_int = c(lower = sum(evaluated$horizon_estimate) - critical * std_error,
                 upper = sum(evaluated$horizon_estimate) + critical * std_error),
    influence = evaluated$influence,
    unit_outcome = unit_outcome,
    expected_treatment = evaluated$expected_treatment,
    policy_probability = evaluated$policy_probability,
    likelihood_ratio = if (keep == "all") evaluated$likelihood_ratio else NULL,
    time_weight = evaluated$time_weight,
    stabilization = evaluated$stabilization,
    specification = specification,
    call = match.call()
  )
  class(out) <- c("policy_value", "list")
  out
}

.evaluate_unit_policy <- function(data_info, outcome, treatment, propensity, design,
                                  coefficient, order, stabilize, fixed_prob,
                                  outcome_index) {
  horizon <- nrow(coefficient)
  n_eval <- data_info$n_time - horizon + 1L
  probability <- array(
    NA_real_, dim = c(horizon, n_eval, data_info$n_unit),
    dimnames = list(paste0("stage", seq_len(horizon)),
                    as.character(data_info$time_levels[seq_len(n_eval)]),
                    data_info$unit_levels)
  )
  likelihood <- probability
  raw_stage_weight <- matrix(
    NA_real_, nrow = horizon, ncol = n_eval,
    dimnames = list(paste0("stage", seq_len(horizon)),
                    as.character(data_info$time_levels[seq_len(n_eval)]))
  )

  for (stage in seq_len(horizon)) {
    policy_probability <- matrix(
      NA_real_, nrow = data_info$n_time, ncol = data_info$n_unit
    )
    for (time_index in seq_len(data_info$n_time)) {
      rows <- (time_index - 1L) * data_info$n_unit + seq_len(data_info$n_unit)
      linear_predictor <- as.numeric(
        design[rows, , drop = FALSE] %*% coefficient[stage, ]
      )
      policy_probability[time_index, ] <-
        (1 + exp(-linear_predictor))^(-1)
    }
    fixed_units <- which(!is.na(fixed_prob))
    if (length(fixed_units)) {
      policy_probability[, fixed_units] <- rep(fixed_prob[fixed_units],
                                               each = data_info$n_time)
    }
    start <- horizon - stage + 1L
    rows <- start:(data_info$n_time - stage + 1L)
    q <- policy_probability[rows, , drop = FALSE]
    w <- treatment[rows, , drop = FALSE]
    p <- propensity[rows, , drop = FALSE]
    ratio <- ifelse(w == 1, q / p, (1 - q) / (1 - p))
    probability[stage, , ] <- q
    likelihood[stage, , ] <- ratio
    raw_stage_weight[stage, ] <- .linearized_ipw(ratio, order)
  }

  cumulative_weight <- raw_stage_weight
  if (horizon > 1L) {
    for (stage in seq.int(horizon - 1L, 1L)) {
      cumulative_weight[stage, ] <- raw_stage_weight[stage, ] *
        cumulative_weight[stage + 1L, ]
    }
  }
  stabilized <- .stabilize_ipw(cumulative_weight, stabilize)
  time_weight <- stabilized$weight

  outcome_by_stage_unit <- array(
    NA_real_, dim = c(horizon, n_eval, data_info$n_unit),
    dimnames = c(dimnames(time_weight), list(data_info$unit_levels))
  )
  for (stage in seq_len(horizon)) {
    rows <- (horizon - stage + 1L):(data_info$n_time - stage + 1L)
    outcome_by_stage_unit[stage, , ] <- outcome[rows, , drop = FALSE]
  }
  outcome_by_stage <- apply(
    outcome_by_stage_unit[, , outcome_index, drop = FALSE],
    c(1, 2), mean, na.rm = TRUE
  )
  outcome_by_stage <- matrix(
    outcome_by_stage, nrow = horizon, ncol = n_eval,
    dimnames = dimnames(time_weight)
  )
  if (any(!is.finite(outcome_by_stage))) {
    stop("Every evaluated time period must contain at least one observed outcome.",
         call. = FALSE)
  }

  overall <- .policy_outcome_moments(
    outcome_by_stage, time_weight, stabilized$mixing, stabilize
  )
  unit_estimate <- unit_variance <- rep(NA_real_, data_info$n_unit)
  for (unit_index in seq_len(data_info$n_unit)) {
    unit_outcome <- matrix(
      outcome_by_stage_unit[, , unit_index, drop = FALSE],
      nrow = horizon, ncol = n_eval,
      dimnames = dimnames(time_weight)
    )
    if (all(is.finite(unit_outcome))) {
      unit_moments <- .policy_outcome_moments(
        unit_outcome, time_weight, stabilized$mixing, stabilize
      )
      unit_estimate[unit_index] <- sum(unit_moments$horizon_estimate)
      unit_variance[unit_index] <- unit_moments$variance
    }
  }
  names(unit_estimate) <- names(unit_variance) <- data_info$unit_levels

  names(overall$horizon_estimate) <- paste0("stage", seq_len(horizon))
  names(overall$influence) <- colnames(time_weight)
  expected_treatment <- .expected_policy_treatment(probability, time_weight)
  list(
    horizon_estimate = overall$horizon_estimate,
    variance = overall$variance,
    influence = overall$influence,
    influence_matrix = overall$influence_matrix,
    unit_estimate = unit_estimate,
    unit_variance = unit_variance,
    expected_treatment = expected_treatment,
    policy_probability = probability,
    likelihood_ratio = likelihood,
    time_weight = time_weight,
    outcome = outcome_by_stage,
    stabilization = list(
      method = stabilize,
      normalized_weight_fraction = stats::setNames(
        stabilized$mixing, rownames(time_weight)
      ),
      raw_weight_mean = stats::setNames(
        stabilized$raw_mean, rownames(time_weight)
      )
    )
  )
}

.policy_outcome_moments <- function(outcome, time_weight, mixing, stabilize) {
  weighted_outcome <- time_weight * outcome
  horizon_estimate <- rowMeans(weighted_outcome)
  if (stabilize == "mixed") {
    center <- mixing * horizon_estimate +
      (1 - mixing) * rowMeans(outcome)
  } else {
    center <- horizon_estimate
  }
  influence_matrix <- weighted_outcome - time_weight * center
  influence <- colSums(influence_matrix)
  variance <- mean(influence^2) / ncol(outcome)
  list(
    horizon_estimate = horizon_estimate,
    variance = variance,
    influence = influence,
    influence_matrix = influence_matrix
  )
}

.expected_policy_treatment <- function(probability, time_weight) {
  weighted <- probability
  horizon <- dim(probability)[1]
  if (horizon > 1L) {
    for (stage in seq_len(horizon - 1L)) {
      weighted[stage, , ] <- probability[stage, , ] *
        as.numeric(time_weight[stage + 1L, ])
    }
  }
  stage_by_unit <- matrix(
    NA_real_, nrow = horizon, ncol = dim(probability)[3]
  )
  for (stage in seq_len(horizon)) {
    stage_probability <- matrix(
      weighted[stage, , , drop = FALSE],
      nrow = dim(probability)[2], ncol = dim(probability)[3]
    )
    stage_by_unit[stage, ] <- colMeans(stage_probability)
  }
  rownames(stage_by_unit) <- dimnames(probability)[[1]]
  colnames(stage_by_unit) <- dimnames(probability)[[3]]
  by_stage <- stats::setNames(rowMeans(stage_by_unit), rownames(stage_by_unit))
  by_unit <- stats::setNames(colMeans(stage_by_unit), colnames(stage_by_unit))
  list(
    overall = mean(by_stage),
    by_stage = by_stage,
    by_unit = by_unit,
    stage_by_unit = stage_by_unit
  )
}
