.prepare_unit_data <- function(data, time, unit) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  for (column in c(time, unit)) {
    if (!is.character(column) || length(column) != 1L || !column %in% names(data)) {
      stop("`time` and `unit` must name columns in `data`.", call. = FALSE)
    }
  }
  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }
  if (anyNA(data[[time]]) || anyNA(data[[unit]])) {
    stop("The time and unit columns cannot contain missing values.", call. = FALSE)
  }

  time_order <- order(data[[time]])
  time_levels <- unique(data[[time]][time_order])
  unit_levels <- unique(as.character(data[[unit]]))
  row_order <- order(
    match(data[[time]], time_levels),
    match(as.character(data[[unit]]), unit_levels)
  )
  sorted <- data[row_order, , drop = FALSE]
  key <- paste(sorted[[time]], sorted[[unit]], sep = "\r")
  expected <- length(time_levels) * length(unit_levels)

  if (anyDuplicated(key) || nrow(sorted) != expected) {
    stop("`data` must have one row per time-unit combination.",
         call. = FALSE)
  }
  counts <- table(factor(sorted[[time]], levels = time_levels),
                  factor(as.character(sorted[[unit]]), levels = unit_levels))
  if (any(counts != 1L)) {
    stop("`data` must have one row per time-unit combination.",
         call. = FALSE)
  }

  list(
    data = sorted,
    order = row_order,
    time_levels = time_levels,
    unit_levels = unit_levels,
    n_time = length(time_levels),
    n_unit = length(unit_levels),
    time = time,
    unit = unit
  )
}

.data_matrix <- function(x, data_info, name, binary = FALSE, allow_na = FALSE) {
  if (length(x) != data_info$n_time * data_info$n_unit) {
    stop(sprintf("`%s` must have one value per data row.", name), call. = FALSE)
  }
  if (!is.numeric(x) || (!allow_na && anyNA(x))) {
    qualifier <- if (allow_na) "numeric" else "numeric and cannot contain missing values"
    stop(sprintf("`%s` must be %s.", name, qualifier), call. = FALSE)
  }
  if (binary && any(!x %in% c(0, 1))) {
    stop(sprintf("`%s` must contain only 0 and 1.", name), call. = FALSE)
  }
  out <- matrix(x, nrow = data_info$n_time, ncol = data_info$n_unit, byrow = TRUE,
                dimnames = list(as.character(data_info$time_levels), data_info$unit_levels))
  out
}

.resolve_propensity <- function(propensity, data_info) {
  if (inherits(propensity, "unit_ps")) {
    out <- propensity$propensity
    if (!identical(dim(out), c(data_info$n_time, data_info$n_unit))) {
      stop("The `unit_ps` object does not match the dimensions of `data`.",
           call. = FALSE)
    }
    if (!is.null(dimnames(out))) {
      if (!all(as.character(data_info$time_levels) %in% rownames(out)) ||
          !all(data_info$unit_levels %in% colnames(out))) {
        stop("The `unit_ps` object does not contain the same time and unit levels.",
             call. = FALSE)
      }
      out <- out[as.character(data_info$time_levels), data_info$unit_levels, drop = FALSE]
    }
  } else if (is.character(propensity) && length(propensity) == 1L) {
    if (!propensity %in% names(data_info$data)) {
      stop("`propensity` does not name a column in `data`.", call. = FALSE)
    }
    out <- .data_matrix(data_info$data[[propensity]], data_info, propensity)
  } else if (is.matrix(propensity)) {
    if (!identical(dim(propensity), c(data_info$n_time, data_info$n_unit))) {
      stop("A propensity matrix must have one row per time and one column per unit.",
           call. = FALSE)
    }
    out <- propensity
    if (!is.null(dimnames(out))) {
      if (!all(as.character(data_info$time_levels) %in% rownames(out)) ||
          !all(data_info$unit_levels %in% colnames(out))) {
        stop("The propensity matrix has incompatible dimnames.", call. = FALSE)
      }
      out <- out[as.character(data_info$time_levels), data_info$unit_levels, drop = FALSE]
    }
  } else if (is.numeric(propensity) && length(propensity) == nrow(data_info$data)) {
    out <- .data_matrix(propensity[data_info$order], data_info, "propensity")
  } else {
    stop(paste0("`propensity` must be a column name, numeric vector, T by N ",
                "matrix, or `unit_ps` object."), call. = FALSE)
  }

  if (anyNA(out) || any(!is.finite(out)) || any(out <= 0 | out >= 1)) {
    stop("Propensity scores must be finite and strictly between 0 and 1.",
         call. = FALSE)
  }
  out
}

.policy_design <- function(policy, data_info) {
  if (!inherits(policy, "formula")) {
    stop("`policy` must be a formula.", call. = FALSE)
  }
  if (length(policy) != 2L) {
    stop("`policy` must be a one-sided formula such as `~ x1 + x2`.",
         call. = FALSE)
  }
  design <- stats::model.matrix(policy, data = data_info$data)
  if (nrow(design) != nrow(data_info$data) || anyNA(design)) {
    stop("The policy design matrix cannot contain missing values.", call. = FALSE)
  }
  design
}

.policy_coef_matrix <- function(policy_coef, design_names) {
  if (is.data.frame(policy_coef)) {
    policy_coef <- as.matrix(policy_coef)
  }
  if (is.vector(policy_coef) && is.numeric(policy_coef)) {
    policy_coef <- matrix(policy_coef, nrow = 1L,
                          dimnames = list(NULL, names(policy_coef)))
  }
  if (!is.matrix(policy_coef) || !is.numeric(policy_coef) || nrow(policy_coef) < 1L) {
    stop("`policy_coef` must be a numeric vector or matrix.", call. = FALSE)
  }
  if (!is.null(colnames(policy_coef))) {
    missing_coef <- setdiff(design_names, colnames(policy_coef))
    extra_coef <- setdiff(colnames(policy_coef), design_names)
    if (length(missing_coef) || length(extra_coef)) {
      stop("Coefficient names must exactly match the policy design columns.",
           call. = FALSE)
    }
    policy_coef <- policy_coef[, design_names, drop = FALSE]
  } else if (ncol(policy_coef) != length(design_names)) {
    stop("`policy_coef` has the wrong number of columns for `policy`.",
         call. = FALSE)
  } else {
    colnames(policy_coef) <- design_names
  }
  if (anyNA(policy_coef) || any(!is.finite(policy_coef))) {
    stop("`policy_coef` must contain only finite values.", call. = FALSE)
  }
  rownames(policy_coef) <- paste0("stage", seq_len(nrow(policy_coef)))
  policy_coef
}

.fixed_policy_prob <- function(fixed_prob, unit_levels) {
  out <- rep(NA_real_, length(unit_levels))
  names(out) <- unit_levels
  if (is.null(fixed_prob)) {
    return(out)
  }
  if (!is.numeric(fixed_prob) || anyNA(fixed_prob) ||
      any(!is.finite(fixed_prob)) || any(fixed_prob < 0 | fixed_prob > 1)) {
    stop("`fixed_prob` must contain finite probabilities between 0 and 1.",
         call. = FALSE)
  }
  if (is.null(names(fixed_prob))) {
    if (length(fixed_prob) != length(unit_levels)) {
      stop("An unnamed `fixed_prob` must have one value per unit.", call. = FALSE)
    }
    out[] <- fixed_prob
  } else {
    unknown <- setdiff(names(fixed_prob), unit_levels)
    if (length(unknown)) {
      stop("Names in `fixed_prob` must identify units in `data`.", call. = FALSE)
    }
    out[names(fixed_prob)] <- fixed_prob
  }
  out
}

.resolve_outcome_units <- function(outcome_units, unit_levels) {
  if (is.null(outcome_units)) {
    return(seq_along(unit_levels))
  }
  if (!is.character(outcome_units) || !length(outcome_units) ||
      anyNA(outcome_units) || anyDuplicated(outcome_units)) {
    stop("`outcome_units` must be a nonempty character vector of unique unit IDs.",
         call. = FALSE)
  }
  unknown <- setdiff(outcome_units, unit_levels)
  if (length(unknown)) {
    stop("Every value in `outcome_units` must identify a unit in `data`.",
         call. = FALSE)
  }
  match(outcome_units, unit_levels)
}

.linearized_ipw <- function(ratio, order) {
  z <- ratio - 1
  first <- vapply(seq_len(nrow(z)), function(row) {
    sum(z[row, ], na.rm = TRUE)
  }, numeric(1))
  if (order == 1L) {
    return(first + 1)
  }
  if (ncol(z) < 2L) {
    return(first + 1)
  }
  pairs <- utils::combn(seq_len(ncol(z)), 2L, simplify = TRUE)
  second <- vapply(seq_len(nrow(z)), function(row) {
    sum(z[row, pairs[1L, ]] * z[row, pairs[2L, ]])
  }, numeric(1))
  first + second + 1
}

.stabilize_ipw <- function(weight, method, mix_center = 1, mix_scale = 0.1) {
  means <- apply(weight, 1L, mean)
  if (method == "none") {
    return(list(weight = weight, mixing = rep(NA_real_, nrow(weight)),
                raw_mean = means))
  }
  if (any(abs(means) < .Machine$double.eps)) {
    stop("A time-weight mean is zero, so it cannot be stabilized.", call. = FALSE)
  }
  normalized <- t(vapply(seq_along(means), function(stage) {
    weight[stage, ] / means[[stage]]
  }, numeric(ncol(weight))))
  if (method == "normalize") {
    return(list(weight = normalized, mixing = rep(1, nrow(weight)),
                raw_mean = means))
  }
  mixing <- (1 + exp(-(means^2 - mix_center) / mix_scale))^(-1)
  recentered <- t(vapply(seq_along(means), function(stage) {
    weight[stage, ] + 1 - means[[stage]]
  }, numeric(ncol(weight))))
  mixed <- t(vapply(seq_along(means), function(stage) {
    mixing[[stage]] * normalized[stage, ] +
      (1 - mixing[[stage]]) * recentered[stage, ]
  }, numeric(ncol(weight))))
  list(weight = mixed, mixing = mixing, raw_mean = means)
}

.with_local_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(force(code))
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}
