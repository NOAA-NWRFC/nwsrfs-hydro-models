# %%
# Cross-package R vs Python comparisons (dev only)
#
# These compare live R model output against Python-generated baselines
# bundled in nwsrfs_py. They live outside the package test suite (rather than
# nwsrfs_r/tests/testthat/) because they depend on another package's example
# data, and the simulation comparisons each run a full ~43-year simulation
# that is too slow for routine `R CMD check`/CRAN runs.
#
# Run with cwd nwsrfs_r so .Rprofile activates renv:
#   Rscript ../dev/test-cross-package.R
# or via:
#   pixi run test-cross
#
# A failing expectation causes testthat to signal an error, which halts the
# script with a nonzero exit code.

library(testthat)
library(nwsrfsr)

repo_root = normalizePath(file.path(getwd(), ".."))
py_data_dir = file.path(repo_root, "nwsrfs_py", "nwsrfs_py", "data")

if (!dir.exists(py_data_dir)) {
  stop(
    "Python package data directory not found at ", py_data_dir,
    ". Run this script with cwd set to nwsrfs_r (e.g. `pixi run test-cross`)."
  )
}

read_py_baseline = function(path) {
  if (!file.exists(path)) {
    stop("Python baseline CSV not found: ", path)
  }
  read.csv(path)
}


# %%
# Strict simulation baselines (moved verbatim from test-nwsrfs-run.R)

test_that("NRKW1 simulation matches Python baseline within tolerance", {
  run = load_example("NRKW1")
  py_baseline = read_py_baseline(
    file.path(py_data_dir, "NRKW1", "results_por_02", "optimal_6hr_inst.csv")
  )

  n = min(length(run$sim), nrow(py_baseline))
  total_diff = sum(abs(run$sim[1:n] - py_baseline$sim_flow_cfs[1:n]), na.rm = TRUE)
  cat(sprintf("NRKW1 sim total abs diff: %.4f cfs over %d timesteps\n", total_diff, n))
  expect_lt(total_diff, 2) # < 2 cfs total absolute difference
})


test_that("SFLN2 simulation matches Python baseline within tolerance", {
  run = load_example("SFLN2")
  py_baseline = read_py_baseline(
    file.path(py_data_dir, "SFLN2", "results_por_01", "optimal_6hr_inst.csv")
  )

  n = min(length(run$sim), nrow(py_baseline))
  total_diff = sum(abs(run$sim[1:n] - py_baseline$sim_flow_cfs[1:n]), na.rm = TRUE)
  cat(sprintf("SFLN2 sim total abs diff: %.4f cfs over %d timesteps\n", total_diff, n))
  expect_lt(total_diff, 0.25)
})


# %%
# AdjustQ baselines (regenerated via dev/regenerate-adjustq-baselines.py)

test_that("adjustq with sim (canned results_por_02) matches Python baseline", {
  # Mirror how Python's AdjustQ.load_example(sim=True) sources its
  # sim_flow_dir: feed the bundled results_por_02 6hr simulation output
  # directly into adjustq(), rather than re-running a live 43-year R
  # simulation inside this test (that comparison is already covered above).
  sim_csv = read_py_baseline(
    file.path(py_data_dir, "NRKW1", "results_por_02", "optimal_6hr_inst.csv")
  )
  sim_df = data.frame(
    sim_csv[, c("year", "month", "day", "hour")],
    flow_cfs = sim_csv$sim_flow_cfs
  )

  r_result = adjustq(daily_flow = nrkw1_daily_flow, inst_flow = nrkw1_inst_flow, sim = sim_df)
  r_result$datetime = as.POSIXct(r_result$datetime, tz = "UTC")

  py = read_py_baseline(file.path(py_data_dir, "Adjustq_check", "NRKW1_w_sim.csv"))
  py$datetime = as.POSIXct(py$datetime_local_tz, tz = "UTC")

  cmp = merge(r_result, py, by = "datetime")
  abs_diff = abs(cmp$flow_cfs - cmp$Inst_Streamflow_cfs)
  total_diff = sum(abs_diff)
  cat(sprintf(
    "adjustq w_sim total abs diff: %.4f cfs over %d matched rows (py has %d)\n",
    total_diff,
    nrow(cmp),
    nrow(py)
  ))

  expect_equal(nrow(cmp), nrow(py))
  expect_lt(total_diff, 2)
})


test_that("adjustq without sim matches Python baseline", {
  r_result = adjustq_load_example(sim = FALSE)
  r_result$datetime = as.POSIXct(r_result$datetime, tz = "UTC")

  py = read_py_baseline(file.path(py_data_dir, "Adjustq_check", "NRKW1_wout_sim.csv"))
  py$datetime = as.POSIXct(py$datetime_local_tz, tz = "UTC")

  cmp = merge(r_result, py, by = "datetime")
  abs_diff = abs(cmp$flow_cfs - cmp$Inst_Streamflow_cfs)
  total_diff = sum(abs_diff)
  cat(sprintf(
    "adjustq wout_sim total abs diff: %.4f cfs over %d matched rows (py has %d)\n",
    total_diff,
    nrow(cmp),
    nrow(py)
  ))

  expect_equal(nrow(cmp), nrow(py))
  expect_lt(total_diff, 50)
})

cat("All cross-package comparisons passed.\n")
