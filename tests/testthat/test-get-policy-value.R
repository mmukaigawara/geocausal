test_that("optimization evaluation preserves full multi-stage values", {
  data <- make_unit_data(n_time = 8, n_unit = 4)
  data$outcome[2] <- NA_real_
  info <- .prepare_unit_data(data, "time", "unit")
  beta <- rbind(c(-.2, .3), c(.1, -.25), c(.4, .2), c(-.3, -.1))
  colnames(beta) <- c("(Intercept)", "x")
  for (order in 1:2) for (stabilize in c("mixed", "normalize", "none")) {
    full <- get_policy_value(data, "outcome", "treatment", "ps", "time", "unit",
      ~ x, beta, order = order, stabilize = stabilize, fixed_prob = c(u2 = .7))
    fast <- .evaluate_unit_policy(info,
      .data_matrix(info$data$outcome, info, "outcome", allow_na = TRUE),
      .data_matrix(info$data$treatment, info, "treatment", binary = TRUE),
      .resolve_propensity("ps", info), .policy_design(~ x, info), beta,
      order, stabilize, .fixed_policy_prob(c(u2 = .7), info$unit_levels),
      seq_len(info$n_unit), unit_summary = FALSE)
    expect_equal(fast$horizon_estimate, full$horizon_estimate, tolerance = 1e-12)
    expect_equal(fast$variance, full$variance, tolerance = 1e-12)
    expect_equal(fast$influence, full$influence, tolerance = 1e-12)
    expect_equal(fast$policy_probability, full$policy_probability, tolerance = 1e-12)
  }
})

test_that("first-order policy value matches a hand calculation", {
  data <- make_unit_data(n_time = 5, n_unit = 4)
  beta <- c("(Intercept)" = -0.1, x = 0.4)

  value <- get_policy_value(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    policy_coef = beta,
    order = 1,
    stabilize = "none",
    keep = "all"
  )

  q <- stats::plogis(beta[1] + beta[2] * data$x)
  ratio <- ifelse(data$treatment == 1, q / data$ps,
                  (1 - q) / (1 - data$ps))
  ratio <- matrix(ratio, nrow = 5, byrow = TRUE)
  time_weight <- 1 + rowSums(ratio - 1)
  y <- matrix(data$outcome, nrow = 5, byrow = TRUE)
  expected <- mean(time_weight * rowMeans(y))

  expect_s3_class(value, "policy_value")
  expect_equal(value$estimate, expected, tolerance = 1e-12)
  expect_equal(as.numeric(value$time_weight), time_weight, tolerance = 1e-12)
  expect_equal(dim(value$policy_probability), c(1, 5, 4))
  expect_named(
    value,
    c("estimate", "horizon_estimate", "variance", "std_error", "conf_int",
      "influence", "unit_outcome", "expected_treatment", "policy_probability",
      "likelihood_ratio", "time_weight", "stabilization", "specification",
      "call")
  )
  expect_equal(value$expected_treatment$overall, mean(q), tolerance = 1e-12)
})

test_that("second-order linearization and stage alignment are correct", {
  data <- make_unit_data(n_time = 6, n_unit = 4)
  beta <- rbind(
    c("(Intercept)" = -0.2, x = 0.3),
    c("(Intercept)" = 0.1, x = -0.25)
  )

  value <- get_policy_value(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    policy_coef = beta,
    order = 2,
    stabilize = "none",
    keep = "all"
  )

  expect_equal(dim(value$time_weight), c(2, 5))
  expect_equal(dim(value$likelihood_ratio), c(2, 5, 4))

  z <- value$likelihood_ratio[2, 1, ] - 1
  expected_stage <- 1 + sum(z) + 0.5 * (sum(z)^2 - sum(z^2))
  expect_equal(value$time_weight[2, 1], expected_stage, tolerance = 1e-12)
})

test_that("fixed unit probabilities override the stochastic policy", {
  data <- make_unit_data(n_time = 5, n_unit = 4)
  value <- get_policy_value(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    policy_coef = c(0, 0),
    fixed_prob = c(u1 = 0, u4 = 1),
    keep = "all"
  )

  expect_true(all(value$policy_probability[, , "u1"] == 0))
  expect_true(all(value$policy_probability[, , "u4"] == 1))
  expect_equal(value$expected_treatment$by_unit[c("u1", "u4")], c(u1 = 0, u4 = 1))
})

test_that("unit order follows first appearance in the supplied data", {
  data <- make_unit_data(n_time = 5, n_unit = 4)
  unit_order <- c("u3", "u1", "u4", "u2")
  data <- do.call(rbind, lapply(split(data, data$time), function(slice) {
    slice[match(unit_order, slice$unit), , drop = FALSE]
  }))
  rownames(data) <- NULL

  value <- get_policy_value(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    policy_coef = c(0, 0),
    keep = "all"
  )

  expect_identical(value$specification$unit_levels, unit_order)
  expect_identical(dimnames(value$policy_probability)[[3]], unit_order)
})

test_that("outcome_units changes outcomes without changing policy weights", {
  data <- make_unit_data(n_time = 5, n_unit = 4)
  beta <- c("(Intercept)" = -0.1, x = 0.4)
  all_units <- get_policy_value(
    data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ x,
    policy_coef = beta, stabilize = "none", keep = "all"
  )
  one_unit <- get_policy_value(
    data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ x,
    policy_coef = beta, stabilize = "none", outcome_units = "u2",
    keep = "all"
  )

  y <- matrix(data$outcome, nrow = 5, byrow = TRUE)
  expected <- mean(as.numeric(all_units$time_weight) * y[, 2])
  expect_equal(one_unit$estimate, expected, tolerance = 1e-12)
  expect_equal(one_unit$time_weight, all_units$time_weight)
  expect_equal(one_unit$expected_treatment, all_units$expected_treatment)
  expect_identical(one_unit$specification$outcome_units, "u2")
  expect_equal(
    one_unit$estimate,
    all_units$unit_outcome$estimate[all_units$unit_outcome$unit == "u2"]
  )
})

test_that("print and summary expose named policy quantities without placeholder NA", {
  data <- make_unit_data(n_time = 5, n_unit = 4)
  value <- get_policy_value(
    data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ x,
    policy_coef = c(0, 0)
  )

  printed <- print(value)
  expect_named(printed, c("expected_outcome", "std_error", "conf_low",
                          "conf_high", "expected_treatment"))
  expect_false(anyNA(printed))
  summarized <- summary(value)
  expect_named(
    summarized$stabilization,
    c("method", "normalized_weight_fraction", "raw_weight_mean")
  )
})

test_that("mean-one mixed weights preserve the original adaptive fraction", {
  weight <- matrix(rep(c(-1.4, -0.4, 1.1, 2.1, 3.6), 20), nrow = 1)
  outcome <- matrix(rep(c(3, 2, 1, 4, 5), 20), nrow = 1)
  stabilized <- .stabilize_ipw(weight, "mixed", outcome)

  # The original ratio is 4.05 / (1.5 * 3.14).
  expected_fraction <- 135 / 157
  expected_weight <- weight
  expect_equal(stabilized$mixing, expected_fraction, tolerance = 1e-12)
  expect_equal(stabilized$weight, expected_weight, tolerance = 1e-12)
  expect_equal(mean(stabilized$weight), 1, tolerance = 1e-12)

  different <- .stabilize_ipw(weight, "mixed", matrix(rep(c(5, 3, 4, 1, 2), 20), nrow = 1))
  expect_gt(abs(different$mixing - stabilized$mixing), 1e-4)
  shifted <- .stabilize_ipw(weight, "mixed", outcome + 10)
  scaled <- .stabilize_ipw(weight, "mixed", outcome * 2)
  expect_equal(shifted$weight, stabilized$weight, tolerance = 1e-12)
  expect_equal(scaled$weight, stabilized$weight, tolerance = 1e-12)

  # At mean weight .8, numerator=2.1375 and denominator=2.862.
  different_mean <- .stabilize_ipw(weight - .2, "mixed", outcome)
  expect_equal(different_mean$mixing, 161 / 636, tolerance = 1e-12)
  expect_equal(different_mean$weight, (161 / 636) * (weight - .2) / .8 +
    (1 - 161 / 636) * weight, tolerance = 1e-12)
})

test_that("smooth projection joins continuously with matching derivatives", {
  eps <- 0.01
  input <- c(-1, 0, eps / 2, eps, 0.5, 1 - eps, 1 - eps / 2, 1, 2)
  expected <- c(0, 0, 0.00375, 0.01, 0.5, 0.99, 0.99625, 1, 1)
  expect_equal(.smooth_unit_interval(input, eps), expected, tolerance = 1e-12)
  grid <- .smooth_unit_interval(seq(-0.1, 1.1, length.out = 10001), eps)
  expect_true(all(diff(grid) >= 0))
  expect_true(all(grid >= 0 & grid <= 1))

  boundary <- c(0, eps, 1 - eps, 1)
  step <- 1e-8
  center <- .smooth_unit_interval(boundary, eps)
  left <- .smooth_unit_interval(boundary - step, eps)
  right <- .smooth_unit_interval(boundary + step, eps)
  expect_lt(max(abs(center - left)), 2 * step)
  expect_lt(max(abs(right - center)), 2 * step)
  expect_equal((center - left) / step, c(0, 1, 1, 0), tolerance = 1e-5)
  expect_equal((right - center) / step, c(0, 1, 1, 0), tolerance = 1e-5)
})

test_that("adaptive mixing handles an undefined ratio without nonfinite weights", {
  outcome <- matrix(rep(c(1, 2, 3, 4), 5), nrow = 1)
  for (constant in c(1, 2)) {
    result <- .stabilize_ipw(matrix(constant, nrow = 1, ncol = 20), "mixed", outcome)
    expect_equal(result$mixing, 1)
    expect_equal(as.numeric(result$weight), rep(1, 20))
  }
  result <- .stabilize_ipw(matrix(rep(c(-1, 0, 2, 3), 5), nrow = 1),
                            "mixed", matrix(2, nrow = 1, ncol = 20))
  expect_equal(result$mixing, 1)
  expect_true(all(is.finite(result$weight)))
})

test_that("mixed weights handle zero and signed means", {
  weight <- rbind(zero = c(-2, -1, 1, 2), near_zero = c(-1.9, -.9, 1.1, 2.1),
                  negative_near_zero = c(-2.1, -1.1, .9, 1.9),
                  large_mean = c(-100, 0, 200, 300))
  weight <- weight[, rep(seq_len(ncol(weight)), 4)]
  outcome <- matrix(rep(c(1, 3, 2, 4), 16), nrow = 4, byrow = TRUE)
  result <- .stabilize_ipw(weight, "mixed", outcome)
  expect_true(all(is.finite(result$weight)))
  expect_equal(result$mixing[1:3], c(0, 1, 0))
  expect_equal(result$weight[c(1, 3), ], weight[c(1, 3), ] + 1 - rowMeans(weight[c(1, 3), ]))
  expect_equal(result$weight[2, ], weight[2, ] / mean(weight[2, ]))
  expect_equal(rowMeans(result$weight), setNames(rep(1, 4), rownames(weight)), tolerance = 1e-12)
  expect_gte(result$mixing[4], .99)
  expect_error(.stabilize_ipw(weight, "normalize", outcome), "mean is zero")
  expect_identical(.stabilize_ipw(weight, "none", outcome)$weight, weight)
})

test_that("multi-stage adaptive mixing uses aligned outcomes and common unit weights", {
  data <- make_unit_data(n_time = 6, n_unit = 4)
  beta <- rbind(c("(Intercept)" = -0.2, x = 0.3),
                  c("(Intercept)" = 0.1, x = -0.25))
  value <- get_policy_value(data, "outcome", "treatment", "ps", "time", "unit",
                              ~ x, beta, stabilize = "mixed", keep = "all")
  raw <- rbind(1 + rowSums(value$likelihood_ratio[1, , ] - 1),
                 1 + rowSums(value$likelihood_ratio[2, , ] - 1))
  raw[1, ] <- raw[1, ] * raw[2, ]
  y <- rowMeans(matrix(data$outcome, nrow = 6, byrow = TRUE))
  aligned <- rbind(y[2:6], y[1:5])
  for (stage in 1:2) {
    rho <- raw[stage, ]
    outcome <- aligned[stage, ]
    C <- mean(rho)
    expected_fraction <- 0
    expected_weight <- rho + 1 - C
    if (abs(C) >= .Machine$double.eps) {
      value_h <- mean(rho * outcome) / C
      value_ipw <- mean(rho * outcome)
      numerator <- -mean(rho * (outcome - value_h) * (rho - 1))
      denominator <- (value_ipw - mean(outcome)) * mean((rho - 1)^2)
      fraction <- if (denominator == 0) 0 else numerator / denominator
      expected_fraction <- 1 - .smooth_unit_interval(fraction, .01)
      expected_weight <- expected_fraction * rho / C + (1 - expected_fraction) * (rho + 1 - C)
    }
    expect_equal(unname(value$stabilization$normalized_weight_fraction[stage]),
                   expected_fraction, tolerance = 1e-12)
    expect_equal(as.numeric(value$time_weight[stage, ]), unname(expected_weight), tolerance = 1e-12)
  }
  one_unit <- get_policy_value(data, "outcome", "treatment", "ps", "time", "unit",
                                 ~ x, beta, stabilize = "mixed", outcome_units = "u2")
  expect_equal(one_unit$time_weight, value$time_weight)
  expect_equal(one_unit$estimate, value$unit_outcome$estimate[value$unit_outcome$unit == "u2"])
})
