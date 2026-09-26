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
