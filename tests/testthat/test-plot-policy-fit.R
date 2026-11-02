test_that("allocation maps align stages, weeks and polygon identifiers", {
  data <- make_unit_data(n_time = 4, n_unit = 4)
  fixed <- setNames(c(.2, .4, .6, .8), paste0("u", 1:4))
  fit <- get_policy(data, "outcome", "treatment", "ps", "time", "unit", ~ 1,
    horizon = 2, stabilize = "normalize", fixed_prob = fixed)
  polygons <- lapply(1:5, function(i) sf::st_polygon(list(matrix(
    c(i, 0, i+1, 0, i+1, 1, i, 1, i, 0), ncol = 2, byrow = TRUE))))
  districts <- sf::st_sf(id = c("u3", "u1", "outside", "u4", "u2"),
    geometry = sf::st_sfc(polygons, crs = 4326))
  observed <- matrix(data$treatment, ncol = 4, byrow = TRUE)
  mapped <- .policy_allocation_data(fit, districts, "id", data[rev(seq_len(nrow(data))), ], 2, NULL)
  expected <- colMeans(rbind(observed[2:4, ], observed[1:3, ]))
  index <- match(districts$id, names(fixed))
  expect_equal(mapped$observed, unname(expected[index]))
  expect_equal(mapped$learned, unname(fixed[index]))
  expect_equal(mapped$difference, 100 * (unname(fixed[index]) - unname(expected[index])))
  expect_true(is.na(mapped$learned[districts$id == "outside"]))
  first <- .policy_allocation_data(fit, districts, "id", data, 2, 1)
  second <- .policy_allocation_data(fit, districts, "id", data, 2, 2)
  expect_equal(first$observed, unname(colMeans(observed[2:4, ])[index]))
  expect_equal(second$observed, unname(colMeans(observed[1:3, ])[index]))
  expect_s3_class(plot(fit), "ggplot")
  expect_s3_class(plot(fit, type = "allocation", districts = districts,
    district_id = "id", data = data), "ggplot")
  expect_error(.policy_allocation_data(fit, districts[-2, ], "id", data, 2, NULL),
    "include every unit", fixed = TRUE)
  expect_error(.policy_allocation_data(fit, districts, "id", data[data$time > 1, ], 2, NULL),
    "same times and units", fixed = TRUE)
  expect_error(.policy_allocation_data(fit, districts, "id", data, 2, 3), "stage", fixed = TRUE)
  expect_error(.policy_allocation_data(fit, districts, "id", data, 3, NULL), "horizon", fixed = TRUE)
})
