#' Learn a stochastic policy for unit-level data
#'
#' @description
#' `get_policy()` learns a sequence of logistic stochastic policies by
#' optimizing the value estimated by [get_policy_value()]. Policies are fitted
#' sequentially from horizon one through the requested horizon. The user must
#' state whether a larger or smaller outcome is preferred.
#'
#' @param data a data frame in long format with one row for every combination
#'   of time period and unit. It must form a complete time-by-unit data frame
#'   and contain the
#'   outcome, treatment, time, unit, and policy-predictor columns named in the
#'   other arguments.
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
#' @param policy a one-sided formula such as `~ previous_treatment + x`. It
#'   defines the predictors in the logistic policy. Any histories or lagged
#'   variables used by the policy must already be columns in `data`.
#' @param horizon positive integer giving the largest policy horizon.
#' @param order either 1, 2, or `"auto"`. Automatic selection compares the two
#'   linearizations with a multiplier-bootstrap test at each horizon.
#' @param direction whether the policy should `"minimize"` or `"maximize"` the
#'   estimated outcome.
#' @param lower lower bounds for the policy coefficients. Supply one number to
#'   use the same bound for every coefficient, or one number per coefficient.
#' @param upper upper bounds for the policy coefficients. Supply one number to
#'   use the same bound for every coefficient, or one number per coefficient.
#' @param initial optional initial coefficients. Supply a vector to reuse the
#'   same start at every stage, a matrix with one row per stage, or a list of
#'   stage-specific vectors. The default uses -1 for the intercept and 0 for
#'   other coefficients.
#' @param stabilize the time-weight stabilization method passed to
#'   [get_policy_value()]. Choose `"mixed"`, `"normalize"`, or `"none"`.
#' @param fixed_prob optional policy probabilities for units that should not be
#'   optimized. A named numeric vector can specify any subset of unit IDs. An
#'   unnamed vector must contain one probability for every unit. Values must be
#'   between 0 and 1. These fixed probabilities are used when optimizing and
#'   evaluating the learned policy. They are not used by the automatic order
#'   diagnostic, which compares the two estimators before unit-specific policy
#'   constraints are imposed.
#' @param conf_level confidence level used for the normal approximation
#'   intervals returned with each fitted policy value.
#' @param value_lower optional lower bound for every stage-specific value. A
#'   candidate below the bound receives a quadratic optimization penalty.
#' @param optimizer_control list passed to [stats::optim()].
#' @param test_control list controlling `order = "auto"`: `total_points` is the
#'   number of coefficient vectors used to compare the orders, `B` is the number
#'   of multiplier-bootstrap draws, `alpha` is the test level, and `seed`
#'   controls reproducibility.
#'
#' @returns An object of class `policy_fit`, containing:
#' * `coefficients`: learned coefficient matrix.
#' * `value`: one `policy_value` object for each fitted horizon.
#' * `order` and `order_test`: selected linearization orders and test details.
#' * `convergence`: optimizer codes and messages.
#' * `probability_summary`: policy-probability summaries. Expected treatment
#'   rates by stage and unit are stored in each element of `value`.
#' * `specification` and `call`: the fitted policy specification.
#'
#' @details
#' At horizon `m`, coefficients learned for stages `1, ..., m - 1` remain fixed
#' and only the new stage is optimized. The underlying logistic policy is
#' `plogis(X beta)`. This function does not construct histories automatically.
#' Lagged treatments, outcomes, or covariates required by `policy` should be
#' created before calling the function.
#'
#' The automatic order test evaluates both approximations at randomly selected
#' coefficient vectors and uses time-level influence contributions to construct
#' an integrated squared multiplier-bootstrap statistic. Increase
#' `total_points` and `B` for final analyses. Small values are useful for quick
#' checks.
#'
#' @seealso [get_policy_value()], [get_unit_ps()]
#' @family unit-level policy functions
#'
#' @examples
#' data <- expand.grid(time = 1:7, unit = letters[1:4])
#' data$x <- rep(seq(-1, 1, length.out = 4), 7)
#' data$treatment <- rep(c(0, 1, 0, 1), 7)
#' data$ps <- 0.5
#' data$outcome <- data$treatment + data$x + data$time / 10
#'
#' fit <- get_policy(
#'   data = data, outcome = "outcome", treatment = "treatment",
#'   propensity = "ps", time = "time", unit = "unit", policy = ~ x,
#'   horizon = 2, order = 1, direction = "minimize",
#'   lower = -3, upper = 3, optimizer_control = list(maxit = 20)
#' )
#' print(fit)
#'
#' @export
get_policy <- function(data, outcome, treatment, propensity, time, unit, policy,
                       horizon = 1L, order = 1L,
                       direction = c("minimize", "maximize"),
                       lower = -100, upper = 100, initial = NULL,
                       stabilize = c("mixed", "normalize", "none"),
                       fixed_prob = NULL, conf_level = 0.95,
                       value_lower = -Inf,
                       optimizer_control = list(),
                       test_control = list(total_points = 100L, B = 1000L,
                                           alpha = 0.05, seed = NULL)) {
  direction <- match.arg(direction)
  stabilize <- match.arg(stabilize)
  if (!is.numeric(horizon) || length(horizon) != 1L || !is.finite(horizon) ||
      horizon < 1 || horizon != as.integer(horizon)) {
    stop("`horizon` must be a positive integer.", call. = FALSE)
  }
  horizon <- as.integer(horizon)
  automatic <- is.character(order) && length(order) == 1L && order == "auto"
  if (!automatic && (length(order) != 1L || !order %in% c(1, 2))) {
    stop("`order` must be 1, 2, or \"auto\".", call. = FALSE)
  }
  if (!is.numeric(value_lower) || length(value_lower) != 1L ||
      is.na(value_lower)) {
    stop("`value_lower` must be numeric.", call. = FALSE)
  }

  data_info <- .prepare_unit_data(data, time, unit)
  if (horizon > data_info$n_time) {
    stop("`horizon` cannot exceed the number of time periods.", call. = FALSE)
  }
  design <- .policy_design(policy, data_info)
  n_coef <- ncol(design)
  coefficient_names <- colnames(design)
  lower <- .policy_bound(lower, n_coef, coefficient_names, "lower")
  upper <- .policy_bound(upper, n_coef, coefficient_names, "upper")
  if (any(lower >= upper)) {
    stop("Every `lower` bound must be less than its `upper` bound.",
         call. = FALSE)
  }
  initial <- .policy_initial(initial, horizon, coefficient_names, lower, upper)
  test_control <- .policy_test_control(test_control)
  if (automatic && (any(!is.finite(lower)) || any(!is.finite(upper)))) {
    stop("Automatic order selection requires finite coefficient bounds.",
         call. = FALSE)
  }

  coefficient <- matrix(NA_real_, nrow = horizon, ncol = n_coef,
                        dimnames = list(paste0("stage", seq_len(horizon)),
                                        coefficient_names))
  values <- vector("list", horizon)
  selected_order <- integer(horizon)
  order_test <- vector("list", horizon)
  convergence <- data.frame(
    horizon = seq_len(horizon), code = NA_integer_, message = NA_character_,
    objective = NA_real_, stringsAsFactors = FALSE
  )

  for (stage in seq_len(horizon)) {
    previous <- if (stage == 1L) coefficient[FALSE, , drop = FALSE] else
      coefficient[seq_len(stage - 1L), , drop = FALSE]
    if (automatic) {
      stage_seed <- test_control$seed
      test <- .select_policy_order(
        data = data, outcome = outcome, treatment = treatment,
        propensity = propensity, time = time, unit = unit, policy = policy,
        previous = previous, lower = lower, upper = upper,
        stabilize = stabilize, fixed_prob = NULL,
        conf_level = conf_level, total_points = test_control$total_points,
        B = test_control$B, alpha = test_control$alpha, seed = stage_seed
      )
      selected_order[stage] <- test$selected_order
      order_test[[stage]] <- test
    } else {
      selected_order[stage] <- as.integer(order)
      order_test[[stage]] <- NULL
    }

    objective <- function(theta) {
      candidate <- rbind(previous, theta)
      colnames(candidate) <- coefficient_names
      value <- get_policy_value(
        data = data, outcome = outcome, treatment = treatment,
        propensity = propensity, time = time, unit = unit, policy = policy,
        policy_coef = candidate, order = selected_order[stage],
        stabilize = stabilize, fixed_prob = fixed_prob,
        conf_level = conf_level, keep = "standard"
      )
      target <- if (direction == "maximize") -value$estimate else value$estimate
      if (is.finite(value_lower)) {
        deficit <- pmin(value$horizon_estimate - value_lower, 0)
        target <- target + 1e5 * sum(deficit^2)
      }
      target
    }

    optimized <- stats::optim(
      par = unname(initial[stage, ]), fn = objective, method = "L-BFGS-B",
      lower = lower, upper = upper, control = optimizer_control
    )
    coefficient[stage, ] <- optimized$par
    current <- coefficient[seq_len(stage), , drop = FALSE]
    values[[stage]] <- get_policy_value(
      data = data, outcome = outcome, treatment = treatment,
      propensity = propensity, time = time, unit = unit, policy = policy,
      policy_coef = current, order = selected_order[stage],
      stabilize = stabilize, fixed_prob = fixed_prob,
      conf_level = conf_level, keep = "all"
    )
    convergence$code[stage] <- optimized$convergence
    convergence$message[stage] <- if (is.null(optimized$message)) NA_character_ else
      optimized$message
    convergence$objective[stage] <- values[[stage]]$estimate
  }

  probability_summary <- .policy_probability_summary(values)
  specification <- list(
    outcome = outcome, treatment = treatment,
    propensity = if (is.character(propensity)) propensity else class(propensity)[1],
    time = time, unit = unit, policy = policy, horizon = horizon,
    requested_order = order, direction = direction, lower = lower, upper = upper,
    initial = initial, stabilize = stabilize, fixed_prob = fixed_prob,
    conf_level = conf_level, value_lower = value_lower,
    optimizer_control = optimizer_control, test_control = test_control
  )
  out <- list(
    coefficients = coefficient,
    value = values,
    order = selected_order,
    order_test = order_test,
    convergence = convergence,
    probability_summary = probability_summary,
    specification = specification,
    call = match.call()
  )
  class(out) <- c("policy_fit", "list")
  out
}

.policy_bound <- function(x, n_coef, coefficient_names, argument) {
  if (!is.numeric(x) || anyNA(x) || length(x) == 0L ||
      !length(x) %in% c(1L, n_coef)) {
    stop(sprintf("`%s` must be numeric and have length 1 or %d.",
                 argument, n_coef), call. = FALSE)
  }
  if (length(x) == 1L) {
    x <- rep(x, n_coef)
  }
  stats::setNames(x, coefficient_names)
}

.policy_initial <- function(initial, horizon, coefficient_names, lower, upper) {
  n_coef <- length(coefficient_names)
  if (is.null(initial)) {
    one <- rep(0, n_coef)
    if ("(Intercept)" %in% coefficient_names) {
      one[match("(Intercept)", coefficient_names)] <- -1
    }
    out <- matrix(rep(one, each = horizon), nrow = horizon)
  } else if (is.list(initial) && !is.data.frame(initial)) {
    if (length(initial) != horizon) {
      stop("A list supplied to `initial` must have one element per horizon.",
           call. = FALSE)
    }
    out <- do.call(rbind, lapply(initial, as.numeric))
  } else if (is.numeric(initial) && is.null(dim(initial))) {
    if (length(initial) != n_coef) {
      stop("An initial vector must have one value per policy coefficient.",
           call. = FALSE)
    }
    out <- matrix(rep(initial, each = horizon), nrow = horizon)
  } else {
    out <- as.matrix(initial)
  }
  if (!is.numeric(out) || !identical(dim(out), c(horizon, n_coef)) ||
      anyNA(out) || any(!is.finite(out))) {
    stop("`initial` must provide a finite coefficient vector for every horizon.",
         call. = FALSE)
  }
  colnames(out) <- coefficient_names
  rownames(out) <- paste0("stage", seq_len(horizon))
  sweep(sweep(out, 2, lower, pmax), 2, upper, pmin)
}

.policy_test_control <- function(control) {
  defaults <- list(total_points = 100L, B = 1000L, alpha = 0.05, seed = NULL)
  if (!is.list(control) || any(!names(control) %in% names(defaults))) {
    stop("`test_control` contains an unknown component.", call. = FALSE)
  }
  out <- utils::modifyList(defaults, control)
  for (name in c("total_points", "B")) {
    if (!is.numeric(out[[name]]) || length(out[[name]]) != 1L ||
        !is.finite(out[[name]]) || out[[name]] < 1 ||
        out[[name]] != as.integer(out[[name]])) {
      stop(sprintf("`test_control$%s` must be a positive integer.", name),
           call. = FALSE)
    }
    out[[name]] <- as.integer(out[[name]])
  }
  if (!is.numeric(out$alpha) || length(out$alpha) != 1L ||
      !is.finite(out$alpha) || out$alpha <= 0 || out$alpha >= 1) {
    stop("`test_control$alpha` must be strictly between 0 and 1.",
         call. = FALSE)
  }
  if (!is.null(out$seed) && (!is.numeric(out$seed) || length(out$seed) != 1L ||
      !is.finite(out$seed))) {
    stop("`test_control$seed` must be `NULL` or one finite number.",
         call. = FALSE)
  }
  out
}

.select_policy_order <- function(data, outcome, treatment, propensity, time,
                                 unit, policy, previous, lower, upper, stabilize,
                                 fixed_prob, conf_level, total_points, B, alpha,
                                 seed) {
  result <- .with_local_seed(seed, {
    candidates <- matrix(
      stats::runif(total_points * length(lower),
                   min = rep(lower, each = total_points),
                   max = rep(upper, each = total_points)),
      nrow = total_points
    )
    estimate_difference <- numeric(total_points)
    influence_difference <- NULL
    for (point in seq_len(total_points)) {
      candidate <- rbind(previous, candidates[point, ])
      colnames(candidate) <- names(lower)
      first <- get_policy_value(
        data, outcome, treatment, propensity, time, unit, policy, candidate,
        order = 1L, stabilize = stabilize, fixed_prob = fixed_prob,
        conf_level = conf_level, keep = "standard"
      )
      second <- get_policy_value(
        data, outcome, treatment, propensity, time, unit, policy, candidate,
        order = 2L, stabilize = stabilize, fixed_prob = fixed_prob,
        conf_level = conf_level, keep = "standard"
      )
      estimate_difference[point] <- first$estimate - second$estimate
      if (is.null(influence_difference)) {
        influence_difference <- matrix(NA_real_, nrow = total_points,
                                       ncol = length(first$influence))
      }
      influence_difference[point, ] <- first$influence - second$influence
    }
    n_eval <- ncol(influence_difference)
    observed <- sqrt(n_eval) * estimate_difference
    statistic <- mean(observed^2)
    multiplier <- matrix(stats::rnorm(n_eval * B), nrow = n_eval, ncol = B)
    simulated <- influence_difference %*% multiplier / sqrt(n_eval)
    bootstrap <- colMeans(simulated^2)
    critical_value <- as.numeric(stats::quantile(bootstrap, 1 - alpha,
                                                 names = FALSE))
    p_value <- mean(bootstrap >= statistic)
    selected_order <- if (statistic > critical_value) 2L else 1L
    list(statistic = statistic, critical_value = critical_value,
         p_value = p_value, selected_order = selected_order,
         total_points = total_points, B = B, alpha = alpha, seed = seed)
  })
  result
}

.policy_probability_summary <- function(values) {
  rows <- lapply(seq_along(values), function(horizon) {
    probability <- values[[horizon]]$policy_probability
    do.call(rbind, lapply(seq_len(dim(probability)[1]), function(stage) {
      x <- as.numeric(probability[stage, , ])
      data.frame(horizon = horizon, stage = stage, mean = mean(x),
                 minimum = min(x), maximum = max(x),
                 stringsAsFactors = FALSE)
    }))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
