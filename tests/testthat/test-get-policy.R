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
