test_that("multistart preserves the original path and selects the best complete fit", {
  data <- make_unit_data(n_time = 8, n_unit = 4)
  arguments <- list(data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ x,
    horizon = 2, order = 1, lower = -2, upper = 2, stabilize = "none")
  for (direction in c("minimize", "maximize")) {
    single <- do.call(get_policy, c(arguments, list(direction = direction)))
    multiple <- do.call(get_policy, c(arguments, list(direction = direction,
      n_starts = 3, start_control = list(seed = 12))))
    starts <- multiple$multistart$initial
    expect_equal(starts[[1]], single$specification$initial)
    expect_true(all(vapply(starts, function(x) all(x >= -2 & x <= 2), logical(1))))
    independent <- lapply(starts, function(initial) {
      do.call(get_policy, c(arguments, list(direction = direction, initial = initial)))
    })
    estimates <- vapply(independent, function(fit) fit$value[[2]]$estimate, numeric(1))
    selected <- if (direction == "minimize") which.min(estimates) else which.max(estimates)
    expect_equal(multiple$multistart$results$estimate, estimates)
    expect_equal(multiple$multistart$selected_start, selected)
    expect_equal(multiple$coefficients, independent[[selected]]$coefficients)
    expect_equal(multiple$value, independent[[selected]]$value)
    expect_equal(summary(multiple)$multistart, multiple$multistart)
  }
})

test_that("seeded starts are reproducible and preserve the caller's random state", {
  data <- make_unit_data(n_time = 6, n_unit = 4)
  arguments <- list(data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ x,
    lower = -2, upper = 2, stabilize = "none", n_starts = 3,
    start_control = list(seed = 13, scale = .5))
  set.seed(24)
  before <- .Random.seed
  first <- do.call(get_policy, arguments)
  expect_identical(.Random.seed, before)
  second <- do.call(get_policy, arguments)
  expect_equal(first$multistart, second$multistart)
  expect_equal(first$coefficients, second$coefficients)
})

test_that("multistart enforces hard bounds and reports infeasible paths", {
  skip_if_not_installed("nloptr")
  data <- data.frame(time = 1:8, unit = "u1", ps = .5,
    treatment = rep(c(0, 1), 4), outcome = rep(c(1, -1), 4))
  arguments <- list(data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ 1,
    lower = -3, upper = 3, stabilize = "none", mean_lower = .1,
    n_starts = 3, start_control = list(seed = 14),
    optimizer_control = list(algorithm = "NLOPT_LD_SLSQP"))
  fit <- do.call(get_policy, arguments)
  expect_true(all(fit$convergence$code == 0))
  expect_gte(fit$value[[1]]$horizon_estimate[1], .1)
  expect_equal(fit$value[[1]]$estimate, .100001, tolerance = 1e-5)
  arguments$data$outcome <- 0
  arguments$optimizer_control$maxeval <- 50
  expect_error(do.call(get_policy, arguments), "No successful optimization path")
})

test_that("multistart rejects invalid controls", {
  data <- make_unit_data()
  arguments <- list(data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ x)
  for (n in list(0, 1.5, NA_real_, Inf, "2")) {
    expect_error(do.call(get_policy, c(arguments, list(n_starts = n))),
      "`n_starts` must be a positive integer", fixed = TRUE)
  }
  for (control in list(1, list(foo = 2), list(2), list(scale = 0), list(seed = -1))) {
    expect_error(do.call(get_policy, c(arguments, list(start_control = control))),
      "start_control", fixed = TRUE)
  }
})

test_that("a failed first start does not prevent another path from succeeding", {
  data <- expand.grid(time = 1:4, unit = letters[1:4], stringsAsFactors = FALSE)
  data$treatment <- 1
  data$ps <- 2 / 3
  data$outcome <- 1
  arguments <- list(data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ 1,
    initial = 0, lower = -2, upper = 2, stabilize = "normalize")
  # At intercept zero, the additive IPW mean is exactly zero.
  expect_error(do.call(get_policy, arguments), "time-weight mean is zero")
  fit <- do.call(get_policy, c(arguments, list(n_starts = 3,
    start_control = list(seed = 15))))
  expect_false(fit$multistart$results$successful[1])
  expect_true(grepl("time-weight mean is zero", fit$multistart$results$message[1]))
  expect_gt(fit$multistart$selected_start, 1)
  expect_equal(fit$value[[1]]$estimate, 1)
})

test_that("finite values from incomplete optimizer runs are not selected", {
  data <- make_unit_data(n_time = 6, n_unit = 4)
  arguments <- list(data = data, outcome = "outcome", treatment = "treatment",
    propensity = "ps", time = "time", unit = "unit", policy = ~ x,
    stabilize = "none", lower = -2, upper = 2,
    optimizer_control = list(maxit = 0))
  single <- do.call(get_policy, arguments)
  expect_true(is.finite(single$value[[1]]$estimate))
  expect_equal(single$convergence$code, 1L)
  expect_error(do.call(get_policy, c(arguments, list(n_starts = 3,
    start_control = list(seed = 16)))), "No successful optimization path")
})
