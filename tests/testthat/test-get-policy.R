test_that("get_policy learns bounded policies in either direction", {
  data <- make_unit_data(n_time = 6, n_unit = 4)

  fit_min <- get_policy(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    horizon = 1,
    order = 1,
    direction = "minimize",
    lower = -2,
    upper = 2,
    optimizer_control = list(maxit = 20)
  )
  fit_max <- get_policy(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    horizon = 1,
    order = 1,
    direction = "maximize",
    lower = -2,
    upper = 2,
    optimizer_control = list(maxit = 20)
  )

  expect_s3_class(fit_min, "policy_fit")
  expect_equal(dim(fit_min$coefficients), c(1, 2))
  expect_true(all(fit_min$coefficients >= -2 & fit_min$coefficients <= 2))
  expect_lte(fit_min$value[[1]]$estimate, fit_max$value[[1]]$estimate + 1e-7)
  expect_named(
    fit_min,
    c("coefficients", "value", "order", "order_test", "convergence",
      "probability_summary", "specification", "call")
  )
  printed <- print(fit_min)
  expect_named(printed, c("horizon", "order", "expected_outcome", "conf_low",
                          "conf_high", "expected_treatment"))
  expect_false("convergence" %in% names(summary(fit_min)))
})

test_that("automatic order selection records a reproducible test", {
  data <- make_unit_data(n_time = 6, n_unit = 4)
  control <- list(total_points = 4, B = 20, alpha = 0.05, seed = 42)

  fit1 <- get_policy(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    horizon = 1,
    order = "auto",
    direction = "minimize",
    lower = -1,
    upper = 1,
    test_control = control,
    optimizer_control = list(maxit = 5)
  )
  fit2 <- get_policy(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    horizon = 1,
    order = "auto",
    direction = "minimize",
    lower = -1,
    upper = 1,
    test_control = control,
    optimizer_control = list(maxit = 5)
  )

  expect_true(fit1$order %in% c(1L, 2L))
  expect_named(fit1$order_test[[1]],
               c("statistic", "critical_value", "p_value", "selected_order",
                 "total_points", "B", "alpha", "seed"))
  expect_equal(fit1$order_test, fit2$order_test)
})

test_that("automatic order diagnostics precede fixed-unit constraints", {
  data <- make_unit_data(n_time = 6, n_unit = 4)
  control <- list(total_points = 4, B = 20, alpha = 0.05, seed = 42)

  unconstrained <- get_policy(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    horizon = 1,
    order = "auto",
    direction = "minimize",
    lower = -1,
    upper = 1,
    test_control = control,
    optimizer_control = list(maxit = 5)
  )
  constrained <- get_policy(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    horizon = 1,
    order = "auto",
    direction = "minimize",
    lower = -1,
    upper = 1,
    fixed_prob = c(u1 = 0, u4 = 1),
    test_control = control,
    optimizer_control = list(maxit = 5)
  )

  expect_equal(constrained$order_test, unconstrained$order_test)
})

test_that("automatic order diagnostics reuse the requested seed by horizon", {
  data <- make_unit_data(n_time = 6, n_unit = 4)
  fit <- get_policy(
    data = data,
    outcome = "outcome",
    treatment = "treatment",
    propensity = "ps",
    time = "time",
    unit = "unit",
    policy = ~ x,
    horizon = 2,
    order = "auto",
    direction = "minimize",
    lower = -1,
    upper = 1,
    test_control = list(total_points = 4, B = 20, alpha = 0.05, seed = 42),
    optimizer_control = list(maxit = 5)
  )

  expect_equal(vapply(fit$order_test, `[[`, numeric(1), "seed"), c(42, 42))
})

test_that("zero hard bounds use the internal margin without a soft penalty", {
  skip_if_not_installed("nloptr")
  data <- data.frame(time = 1:8, unit = "u1", ps = .5,
    treatment = rep(c(0, 1), 4), outcome = rep(c(1, -1), 4))
  arguments <- list(data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ 1,
    lower = -3, upper = 3, stabilize = "none")
  mean_fit <- do.call(get_policy, c(arguments, list(mean_lower = 0)))
  # Mean constraint reaches the internal margin.
  expect_lt(abs(mean_fit$value[[1]]$estimate - 1e-6), 2e-8)
  expect_true(grepl("NLOPT", mean_fit$convergence$message))
  unconstrained <- do.call(get_policy, arguments)
  expect_lt(unconstrained$value[[1]]$estimate, 0)
  expect_false(grepl("NLOPT", unconstrained$convergence$message))
})

test_that("infeasible mean constraints are not returned as fitted policies", {
  skip_if_not_installed("nloptr")
  data <- data.frame(time = 1:8, unit = "u1", ps = .5,
                      treatment = rep(c(0, 1), 4), outcome = 0)
  expect_error(get_policy(data, "outcome", "treatment", "ps", "time", "unit", ~ 1,
                           lower = -3, upper = 3, mean_lower = 1e-6,
                           optimizer_control = list(maxeval = 50)),
                 "No policy satisfying `mean_lower` was returned", fixed = TRUE)
})

test_that("hard bounds apply at all four fitted horizons", {
  skip_if_not_installed("nloptr")
  data <- data.frame(time = 1:8, unit = "u1", ps = .5,
                      treatment = rep(c(0, 1), 4), outcome = 1)
  fit <- get_policy(data, "outcome", "treatment", "ps", "time", "unit", ~ 1,
                      horizon = 4, lower = -2, upper = 2, stabilize = "normalize",
                      mean_lower = .1)
  expect_equal(dim(fit$coefficients), c(4, 1))
  expect_true(all(fit$convergence$code == 0))
  expect_equal(vapply(fit$value, `[[`, numeric(1), "estimate"), as.numeric(1:4))
  expect_true(all(vapply(fit$value, function(x) all(x$horizon_estimate >= .1), logical(1))))
})

test_that("mean bounds reject invalid options", {
  data <- make_unit_data()
  for (bound in list(NA_real_, Inf, -Inf, c(0, 1), "0")) {
    expect_error(get_policy(data, "outcome", "treatment", "ps", "time", "unit",
                             ~ x, mean_lower = bound),
                   "`mean_lower` must be NULL or a finite number", fixed = TRUE)
  }
})

test_that("hard stage bounds match an analytical optimum without a penalty", {
  skip_if_not_installed("nloptr")
  data <- data.frame(time = 1:8, unit = "u1", ps = .5,
    treatment = rep(c(0, 1), 4), outcome = rep(c(1, -1), 4))
  for (algorithm in c("NLOPT_LN_COBYLA", "NLOPT_LD_SLSQP")) {
    fit <- get_policy(data, "outcome", "treatment", "ps", "time", "unit", ~ 1,
      lower = -3, upper = 3, stabilize = "none", mean_lower = .1,
      optimizer_control = list(algorithm = algorithm))
    expect_equal(fit$convergence$code, 0L)
    expect_gte(unname(fit$value[[1]]$horizon_estimate), .1)
    expect_equal(fit$value[[1]]$estimate, .1, tolerance = 1e-5)
    expect_equal(unname(fit$coefficients[1, 1]), qlogis((1 - .100001) / 2),
      tolerance = 1e-5)
    expect_equal(fit$specification$mean_lower, .1)
  }
})

test_that("stage bounds are checked for every component at each horizon", {
  skip_if_not_installed("nloptr")
  data <- make_unit_data(n_time = 8)
  fit <- get_policy(data, "outcome", "treatment", "ps", "time", "unit", ~ x,
    horizon = 4, lower = -1, upper = 1, stabilize = "normalize",
    mean_lower = 0)
  for (M in 1:4) {
    expect_length(fit$value[[M]]$horizon_estimate, M)
    expect_true(all(fit$value[[M]]$horizon_estimate >= 0))
  }
  # A zero outcome cannot satisfy a positive stage lower bound.
  data$outcome <- 0
  expect_error(get_policy(data, "outcome", "treatment", "ps", "time", "unit", ~ x,
    lower = -1, upper = 1, mean_lower = .1,
    optimizer_control = list(maxeval = 50)),
    "No policy satisfying `mean_lower` was returned", fixed = TRUE)
})

test_that("mean bounds apply to each stage rather than their positive sum", {
  skip_if_not_installed("nloptr")
  data <- data.frame(time = 1:3, unit = "u1", treatment = 0, ps = .5,
    outcome = c(-5, -5, 30))
  arguments <- list(data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ 1,
    horizon = 2, lower = -1, upper = 1, stabilize = "normalize")
  fit <- do.call(get_policy, arguments)
  expect_gt(fit$value[[2]]$estimate, 0)
  expect_lt(min(fit$value[[2]]$horizon_estimate), 0)
  expect_error(do.call(get_policy, c(arguments, list(mean_lower = 0,
    optimizer_control = list(maxeval = 50)))),
    "No policy satisfying `mean_lower` was returned at horizon 2", fixed = TRUE)
})
