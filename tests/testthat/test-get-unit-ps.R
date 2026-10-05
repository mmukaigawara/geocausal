test_that("get_unit_ps returns aligned probabilities and diagnostics", {
  data <- make_unit_data()
  data <- data[sample(seq_len(nrow(data))), ]

  fit <- get_unit_ps(
    treatment ~ x + factor(z),
    data = data,
    time = "time",
    unit = "unit",
    clip = c(0.05, 0.95),
    balance = c("x", "z")
  )

  expect_s3_class(fit, "unit_ps")
  expect_equal(dim(fit$propensity), c(7, 4))
  expect_true(all(fit$fitted >= 0.05 & fit$fitted <= 0.95))
  expect_length(fit$observed_weight, nrow(data))
  expect_named(
    fit,
    c("model", "fitted", "propensity", "observed_weight", "balance",
      "data_info", "call")
  )
  expect_named(fit$balance, c("variable", "type", "unweighted", "weighted"))
  expect_named(
    print(fit),
    c("engine", "time_periods", "units", "probability_min", "probability_max")
  )
  expect_named(summary(fit), c("coefficients", "probability", "balance"))
  expect_s3_class(plot(fit, type = "overlap"), "ggplot")
  expect_s3_class(plot(fit, type = "balance"), "ggplot")
})

test_that("get_unit_ps fits models in the supplied row order", {
  data <- make_unit_data()
  data <- data[c(seq(2, nrow(data), by = 2),
                   seq(1, nrow(data), by = 2)), ]
  expected <- stats::glm(
    treatment ~ x + z,
    data = data,
    family = stats::binomial()
  )

  fit <- get_unit_ps(
    treatment ~ x + z,
    data = data,
    time = "time",
    unit = "unit"
  )

  expect_identical(stats::coef(fit$model), stats::coef(expected))
  expect_equal(dim(fit$propensity), c(7, 4))
})

test_that("get_unit_ps supports the optional GEE engine", {
  skip_if_not_installed("geepack")
  data <- make_unit_data()

  fit <- get_unit_ps(
    treatment ~ x + z,
    data = data,
    time = "time",
    unit = "unit",
    engine = "gee",
    cluster = "time"
  )

  expect_s3_class(fit, "unit_ps")
  expect_identical(fit$data_info$engine, "gee")
  expect_equal(dim(fit$propensity), c(7, 4))
})
