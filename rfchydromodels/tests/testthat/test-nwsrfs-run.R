test_that("load_example NRKW1 runs and returns expected structure", {
  run = load_example("NRKW1")

  expect_s3_class(run, "nwsrfs_run")
  expect_true(run$localflow_logic)
  expect_true(run$upflow_logic)
  expect_false(run$chanloss_logic)
  expect_false(run$consuse_logic)
  expect_equal(run$zone_names, c("NRKW1-1", "NRKW1-2"))
  expect_equal(run$upflow_names, c("MFNW1", "NFNW1", "NSSW1"))
  expect_true(length(run$sim) > 0)
  expect_true(all(run$sim > 0))
})


test_that("load_example SFLN2 runs and returns expected structure", {
  run = load_example("SFLN2")

  expect_s3_class(run, "nwsrfs_run")
  expect_true(run$localflow_logic)
  expect_false(run$upflow_logic)
  expect_true(run$chanloss_logic)
  expect_true(run$consuse_logic)
  expect_true("SFLN2-CU" %in% run$zone_names)
  expect_true(length(run$sim) > 0)
})


test_that("NRKW1 simulation matches Python baseline within tolerance", {
  skip_on_cran()
  run = load_example("NRKW1")
  baseline_path = system.file(
    package = "rfchydromodels"
  )

  # Read Python baseline from nwsrfs_py data directory
  # This test uses relative path from repo root
  py_baseline = tryCatch(
    read.csv(file.path(
      "..", "..", "..", "nwsrfs_py", "nwsrfs_py", "data",
      "NRKW1", "results_por_02", "optimal_6hr_inst.csv"
    )),
    error = function(e) NULL
  )

  if (!is.null(py_baseline)) {
    n = min(length(run$sim), nrow(py_baseline))
    total_diff = sum(abs(run$sim[1:n] - py_baseline$sim_flow_cfs[1:n]), na.rm = TRUE)
    expect_lt(total_diff, 2)  # < 2 cfs total absolute difference
  } else {
    skip("Python baseline CSV not found")
  }
})


test_that("SFLN2 simulation matches Python baseline within tolerance", {
  skip_on_cran()
  run = load_example("SFLN2")

  py_baseline = tryCatch(
    read.csv(file.path(
      "..", "..", "..", "nwsrfs_py", "nwsrfs_py", "data",
      "SFLN2", "results_por_01", "optimal_6hr_inst.csv"
    )),
    error = function(e) NULL
  )

  if (!is.null(py_baseline)) {
    n = min(length(run$sim), nrow(py_baseline))
    total_diff = sum(abs(run$sim[1:n] - py_baseline$sim_flow_cfs[1:n]), na.rm = TRUE)
    expect_lt(total_diff, 0.25)
  } else {
    skip("Python baseline CSV not found")
  }
})


test_that("update_pars modifies parameters and re-runs", {
  run = load_example("NRKW1")
  orig_sim = run$sim

  # Increase a SAC parameter slightly
  new_pars = data.frame(
    p_name = "uztwm_NRKW1-1",
    value = run$pars$value[run$pars$p_name == "uztwm_NRKW1-1"] * 1.01
  )
  run2 = update_pars(run, new_pars)

  expect_s3_class(run2, "nwsrfs_run")
  # Sim should be different after parameter change
  expect_false(identical(run2$sim, orig_sim))
})
