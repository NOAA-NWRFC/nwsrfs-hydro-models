# %%
# Cross-package comparison test: R vs Python NWSRFS baselines
# Run from repo root: Rscript tests/cross_package_test.R

library(nwsrfsr)

py_data = file.path("nwsrfs_py", "nwsrfs_py", "data")
pass = TRUE

# %%
# --- NRKW1 ---
cat("=== NRKW1 ===\n")
run_nrkw1 = load_example("NRKW1")

baseline_nrkw1 = read.csv(file.path(py_data, "NRKW1", "results_por_02", "optimal_6hr_inst.csv"))
n = min(length(run_nrkw1$sim), nrow(baseline_nrkw1))
diff_nrkw1 = abs(run_nrkw1$sim[1:n] - baseline_nrkw1$sim_flow_cfs[1:n])
total_diff_nrkw1 = sum(diff_nrkw1, na.rm = TRUE)
max_diff_nrkw1 = max(diff_nrkw1, na.rm = TRUE)

cat(sprintf("  R sim length: %d | Python baseline length: %d\n", length(run_nrkw1$sim), nrow(baseline_nrkw1)))
cat(sprintf("  Total abs diff: %.6e\n", total_diff_nrkw1))
cat(sprintf("  Max abs diff:   %.6e\n", max_diff_nrkw1))

threshold_nrkw1 = 2.0
if (total_diff_nrkw1 < threshold_nrkw1) {
  cat(sprintf("  PASS (< %.1f cfs total)\n", threshold_nrkw1))
} else {
  cat(sprintf("  FAIL (>= %.1f cfs total)\n", threshold_nrkw1))
  pass = FALSE
}

# %%
# --- SFLN2 ---
cat("\n=== SFLN2 ===\n")
run_sfln2 = load_example("SFLN2")

baseline_sfln2 = read.csv(file.path(py_data, "SFLN2", "results_por_01", "optimal_6hr_inst.csv"))
n2 = min(length(run_sfln2$sim), nrow(baseline_sfln2))
diff_sfln2 = abs(run_sfln2$sim[1:n2] - baseline_sfln2$sim_flow_cfs[1:n2])
total_diff_sfln2 = sum(diff_sfln2, na.rm = TRUE)
max_diff_sfln2 = max(diff_sfln2, na.rm = TRUE)

cat(sprintf("  R sim length: %d | Python baseline length: %d\n", length(run_sfln2$sim), nrow(baseline_sfln2)))
cat(sprintf("  Total abs diff: %.6e\n", total_diff_sfln2))
cat(sprintf("  Max abs diff:   %.6e\n", max_diff_sfln2))

threshold_sfln2 = 0.25
if (total_diff_sfln2 < threshold_sfln2) {
  cat(sprintf("  PASS (< %.2f cfs total)\n", threshold_sfln2))
} else {
  cat(sprintf("  FAIL (>= %.2f cfs total)\n", threshold_sfln2))
  pass = FALSE
}

# %%
# --- Summary ---
cat("\n=== Summary ===\n")
if (pass) {
  cat("All cross-package comparisons PASSED.\n")
} else {
  cat("Some comparisons FAILED.\n")
  quit(status = 1)
}
