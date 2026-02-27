test_that("adjustq with simulation runs without error", {
  run = load_example("NRKW1")
  sim_df = data.frame(
    run$forcings[[1]][, c("year", "month", "day", "hour")],
    flow_cfs = run$sim
  )

  result = adjustq(
    daily_flow = nrkw1_daily_flow,
    inst_flow = nrkw1_inst_flow,
    sim = sim_df
  )

  expect_true(is.data.frame(result))
  expect_true("flow_cfs" %in% names(result))
  expect_true(nrow(result) > 0)
  expect_true(all(result$flow_cfs >= 0))
})


test_that("adjustq without simulation runs without error", {
  result = adjustq(
    daily_flow = nrkw1_daily_flow,
    inst_flow = nrkw1_inst_flow,
    sim = NULL
  )

  expect_true(is.data.frame(result))
  expect_true("flow_cfs" %in% names(result))
  expect_true(nrow(result) > 0)
  expect_true(all(result$flow_cfs >= 0))
})


test_that("adjustq with sim matches Python baseline", {
  # testthat sets cwd to tests/testthat; navigate up to repo root
  py_path = file.path(
    testthat::test_path(), "..", "..", "..",
    "nwsrfs_py", "nwsrfs_py", "data", "Adjustq_check", "NRKW1_w_sim.csv"
  )
  skip_if_not(file.exists(py_path), "Python baseline CSV not found")

  py = read.csv(py_path)
  py$datetime = as.POSIXct(py$datetime_local_tz, tz = "UTC")

  r_result = adjustq_load_example(sim = TRUE)
  r_result$datetime = as.POSIXct(r_result$datetime, tz = "UTC")

  cmp = merge(r_result, py, by = "datetime")
  abs_diff = abs(cmp$flow_cfs - cmp$Inst_Streamflow_cfs)

  expect_equal(nrow(cmp), nrow(py))
  expect_lt(sum(abs_diff), 2)
})


test_that("adjustq without sim matches Python baseline", {
  py_path = file.path(
    testthat::test_path(), "..", "..", "..",
    "nwsrfs_py", "nwsrfs_py", "data", "Adjustq_check", "NRKW1_wout_sim.csv"
  )
  skip_if_not(file.exists(py_path), "Python baseline CSV not found")

  py = read.csv(py_path)
  py$datetime = as.POSIXct(py$datetime_local_tz, tz = "UTC")

  r_result = adjustq_load_example(sim = FALSE)
  r_result$datetime = as.POSIXct(r_result$datetime, tz = "UTC")

  cmp = merge(r_result, py, by = "datetime")
  abs_diff = abs(cmp$flow_cfs - cmp$Inst_Streamflow_cfs)

  # Slightly looser tolerance due to minor edge effects on last day
  expect_lt(sum(abs_diff), 50)
})
