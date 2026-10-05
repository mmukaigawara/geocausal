make_unit_data <- function(n_time = 7L, n_unit = 4L) {
  out <- expand.grid(
    time = seq_len(n_time),
    unit = paste0("u", seq_len(n_unit)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$time, out$unit), ]
  out$x <- rep(seq(-1, 1, length.out = n_unit), n_time) +
    rep(seq(0, 0.6, length.out = n_time), each = n_unit)
  out$z <- rep(c(0, 1, 0, 1), length.out = nrow(out))
  out$ps <- stats::plogis(-0.2 + 0.35 * out$x - 0.1 * out$z)
  out$treatment <- as.integer((seq_len(nrow(out)) %% 3L) != 0L)
  out$outcome <- 1 + 0.5 * out$treatment + out$x +
    rep(seq_len(n_time), each = n_unit) / 10
  rownames(out) <- NULL
  out
}
