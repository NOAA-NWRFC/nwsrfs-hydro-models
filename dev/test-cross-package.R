# %%
# Cross-package R vs Python comparisons (dev only)
#
# These compare live R model output against Python-generated baselines
# bundled in nwsrfs_py. They live outside the package test suite (rather than
# nwsrfsr/tests/testthat/) because they depend on another package's example
# data, and the simulation comparisons each run a full ~43-year simulation
# that is too slow for routine `R CMD check`/CRAN runs.
#
# Run with cwd nwsrfsr so .Rprofile activates rv:
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
    ". Run this script with cwd set to nwsrfsr (e.g. `pixi run test-cross`)."
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


# %%
# Argument matrix: R vs live Python across non-default load_example() arguments
#
# The strict baselines above only exercise default arguments, which is how
# `forcing_adj` being ignored by the R package went unnoticed. dev/python-sim-matrix.py
# owns the case list and generates the Python side into a temporary directory,
# so the two languages cannot drift apart on which cases are covered.

parse_forcing_adj = function(x) {
  if (x == "TRUE") {
    TRUE
  } else if (x == "FALSE") {
    FALSE
  } else {
    strsplit(x, "|", fixed = TRUE)[[1]]
  }
}

generate_python_matrix = function() {
  outdir = file.path(tempdir(), "py-sim-matrix")
  script = file.path(repo_root, "dev", "python-sim-matrix.py")

  cat("Generating Python simulations for the argument matrix ...\n")
  status = system2("python", c(shQuote(script), shQuote(outdir)), stdout = FALSE)
  if (!identical(status, 0L)) {
    stop(
      "dev/python-sim-matrix.py failed (exit ", status, "). ",
      "Run this script inside the pixi environment, e.g. `pixi run test-cross`."
    )
  }

  outdir
}

matrix_dir = generate_python_matrix()
cases = read.csv(file.path(matrix_dir, "cases.csv"), stringsAsFactors = FALSE)

for (i in seq_len(nrow(cases))) {
  case = cases[i, ]

  test_that(paste("simulation matches Python for case", case$case_id), {
    forcing_adj = parse_forcing_adj(case$forcing_adj)
    run = load_example(
      case$lid,
      forcing_adj = forcing_adj,
      shift_sf = as.logical(case$shift_sf),
      return_inst = as.logical(case$return_inst)
    )

    py = read_py_baseline(file.path(matrix_dir, paste0("sim_", case$case_id, ".csv")))$sim
    expect_equal(length(run$sim), length(py))

    abs_diff = abs(run$sim - py)
    # Normalized so one threshold covers both sites and every case. The two
    # wrappers differ only in floating point noise, which lands near 1e-8 here;
    # the forcing_adj defect this matrix was written to catch was 6e-2.
    rel_total = sum(abs_diff) / sum(abs(py))
    max_rel = max(abs_diff / pmax(abs(py), 1e-9))
    cat(sprintf(
      "%-24s total abs diff %8.4f cfs, normalized %.2e, max relative %.2e\n",
      case$case_id,
      sum(abs_diff),
      rel_total,
      max_rel
    ))

    expect_lt(rel_total, 1e-6)
    expect_lt(max_rel, 1e-4)
  })
}


# %%
# Parameter table parity

for (lid in c("NRKW1", "SFLN2")) {
  test_that(paste("pars matches the Python parameter table for", lid), {
    r_pars = load_example(lid)$pars
    py_pars = read_py_baseline(file.path(matrix_dir, paste0("pars_", lid, ".csv")))

    # Same parameters, same order, same values. The "name" and "zone" columns
    # are deliberately not compared: Python rewrites chanloss rows to
    # name "cl_factor" / zone "<LID>_CL01" where R keeps name "cl_factor_01" /
    # zone "<LID>", and the R chanloss() implementation looks parameters up by
    # the latter convention.
    expect_equal(nrow(r_pars), nrow(py_pars))
    expect_setequal(r_pars$p_name, py_pars$p_name)

    merged = merge(r_pars, py_pars, by = "p_name", suffixes = c("_r", "_py"))
    expect_equal(nrow(merged), nrow(py_pars))
    expect_equal(merged$value_r, merged$value_py)

    cat(sprintf("%s pars: %d rows, values identical\n", lid, nrow(merged)))
  })
}


# %%
# v1.1.0 feature parity: lookup tables, ordinate UH + PCHIP, shared UH, RSnwElev, 12-hour runs

generate_v110_parity = function() {
  outdir = file.path(tempdir(), "py-v110-parity")
  script = file.path(repo_root, "dev", "python-v110-parity.py")

  cat("Generating Python v1.1.0 parity baselines ...\n")
  status = system2("python", c(shQuote(script), shQuote(outdir)), stdout = FALSE)
  if (!identical(status, 0L)) {
    stop("dev/python-v110-parity.py failed (exit ", status, ").")
  }
  outdir
}

v110_dir = generate_v110_parity()

test_that("lookup tables (uh, peadj_table, chanloss_table) match Python", {
  r_nrkw1 = load_example("NRKW1")
  r_sfln2 = load_example("SFLN2")

  py_uh_n = read.csv(file.path(v110_dir, "uh_NRKW1.csv"), check.names = FALSE)
  py_uh_s = read.csv(file.path(v110_dir, "uh_SFLN2.csv"), check.names = FALSE)
  expect_equal(r_nrkw1$uh, py_uh_n, tolerance = 1e-6)
  expect_equal(r_sfln2$uh, py_uh_s, tolerance = 1e-6)

  py_pe_n = read.csv(file.path(v110_dir, "peadj_NRKW1.csv"), check.names = FALSE)
  py_pe_s = read.csv(file.path(v110_dir, "peadj_SFLN2.csv"), check.names = FALSE)
  expect_equal(unname(as.matrix(r_nrkw1$peadj_table[, -1L])), unname(as.matrix(py_pe_n[, -1L])), tolerance = 1e-12)
  expect_equal(unname(as.matrix(r_sfln2$peadj_table[, -1L])), unname(as.matrix(py_pe_s[, -1L])), tolerance = 1e-12)

  py_cl_s = read.csv(file.path(v110_dir, "chanloss_SFLN2.csv"))
  expect_equal(r_sfln2$chanloss_table$chanloss_adj, py_cl_s$Chanloss_Adj, tolerance = 1e-12)
})

test_that("ordinate UH routing and PCHIP timestep resampling match Python", {
  r_ord = nwsrfs_run(file.path(v110_dir, "case_ordinate_uh"), "results_por_01")
  py_sim = read_py_baseline(file.path(v110_dir, "sim_ordinate_uh.csv"))$sim

  expect_equal(r_ord$uh_type, "ordinate")
  expect_false(r_ord$shared_uh_logic)
  expect_lt(sum(abs(r_ord$sim - py_sim)) / sum(abs(py_sim)), 1e-6)

  r_uh3 = uh_ordinates(r_ord, dt_hours = 3L)
  py_uh3 = read.csv(file.path(v110_dir, "uh_ordinate_3hr.csv"), check.names = FALSE)
  expect_equal(r_uh3, py_uh3, tolerance = 1e-10)

  r_uh12 = uh_ordinates(r_ord, dt_hours = 12L)
  py_uh12 = read.csv(file.path(v110_dir, "uh_ordinate_12hr.csv"), check.names = FALSE)
  expect_equal(r_uh12, py_uh12, tolerance = 1e-10)
})

test_that("shared basin UH routing matches Python", {
  r_sh = nwsrfs_run(file.path(v110_dir, "case_shared_uh"), "results_por_02")
  py_sim = read_py_baseline(file.path(v110_dir, "sim_shared_uh.csv"))$sim
  py_uh = read.csv(file.path(v110_dir, "uh_shared.csv"), check.names = FALSE)

  expect_true(r_sh$shared_uh_logic)
  expect_equal(r_sh$uh_zone_names, "NRKW1-UH")
  expect_equal(r_sh$uh, py_uh, tolerance = 1e-6)
  expect_lt(sum(abs(r_sh$sim - py_sim)) / sum(abs(py_sim)), 1e-6)
})

test_that("area_elev + RSnwElev precipitation typing and update_pars match Python", {
  r_rs = nwsrfs_run(file.path(v110_dir, "case_rsnwelev"), "results_por_01")
  py_init = read_py_baseline(file.path(v110_dir, "sim_rsnwelev_init.csv"))$sim

  expect_false(r_rs$ptps_logic)
  expect_true(r_rs$area_elev_logic)
  expect_lt(sum(abs(r_rs$sim - py_init)) / sum(abs(py_init)), 1e-6)

  r_rs2 = update_pars(
    r_rs,
    data.frame(p_name = c("pxtemp_SFLN2-1", "talr_SFLN2-2"), value = c(2.5, 0.75))
  )
  py_upd = read_py_baseline(file.path(v110_dir, "sim_rsnwelev_updated.csv"))$sim
  expect_lt(sum(abs(r_rs2$sim - py_upd)) / sum(abs(py_upd)), 1e-6)
})

test_that("multi-timestep (12-hour) NRKW1 and SFLN2 simulations match Python", {
  r_n12 = nwsrfs_run(file.path(v110_dir, "case_nrkw1_12hr"), "results_por_02")
  py_n12 = read_py_baseline(file.path(v110_dir, "sim_nrkw1_12hr.csv"))$sim
  expect_equal(r_n12$dt_hours, 12L)
  expect_lt(sum(abs(r_n12$sim - py_n12)) / sum(abs(py_n12)), 1e-6)

  r_s12 = nwsrfs_run(file.path(v110_dir, "case_sfln2_12hr"), "results_por_01")
  py_s12 = read_py_baseline(file.path(v110_dir, "sim_sfln2_12hr.csv"))$sim
  expect_equal(r_s12$dt_hours, 12L)
  expect_lt(sum(abs(r_s12$sim - py_s12)) / sum(abs(py_s12)), 1e-6)
})

cat("All cross-package comparisons passed.\n")
