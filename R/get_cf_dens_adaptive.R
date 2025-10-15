get_cf_dens_adaptive <- function(baseline_den, scale_factor) {
  if (!all(c("intens_grid_cells", "estimated_counts", "sum_log_intens", "actual_counts") %in% names(baseline_den))) {
    stop("Input baseline_denect must include intens_grid_cells, estimated_counts, sum_log_intens, and actual_counts.")
  }
  
  message("Scaling intensity fields by factor = ", scale_factor, " ...")
  
  # 1. Scale each intensity image by the constant
  scaled_cif <- lapply(baseline_den$intens_grid_cells, function(img) img * scale_factor)
  
  # 2. Update estimated counts 
  scaled_est_counts <- baseline_den$estimated_counts * scale_factor
  
  # 3. Update sum of log intensities 
  #    Each term increases by n_t * log(scale_factor)
  scaled_sum_log <- baseline_den$sum_log_intens + baseline_den$actual_counts * log(scale_factor)
  
  # 4. Return a new list (drop model)
  out_scaled <- list(
    indep_var         = baseline_den$indep_var,
    coef              = baseline_den$coef,
    intens_grid_cells = scaled_cif,
    estimated_counts  = scaled_est_counts,
    sum_log_intens    = scaled_sum_log,
    actual_counts     = baseline_den$actual_counts
  )

  return(out_scaled)
}

