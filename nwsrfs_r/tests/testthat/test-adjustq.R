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
