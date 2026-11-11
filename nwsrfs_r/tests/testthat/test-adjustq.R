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


test_that("adjustq handles zero simulated flow at small gap endpoints and blend = 0", {
  dts = seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = "6 hours", length.out = 20)
  sim_df = data.frame(
    year = as.integer(format(dts, "%Y")),
    month = as.integer(format(dts, "%m")),
    day = as.integer(format(dts, "%d")),
    hour = as.integer(format(dts, "%H")),
    flow_cfs = c(rep(0, 6), rep(10, 14))
  )
  # Small gap across indices 2..4 where simulated == 0 at both endpoints
  inst_keep = c(1, 5, 6:20)
  inst_df = data.frame(
    year = sim_df$year[inst_keep],
    month = sim_df$month[inst_keep],
    day = sim_df$day[inst_keep],
    hour = sim_df$hour[inst_keep],
    flow_cfs = c(0, 5, rep(12, 15))
  )
  daily_dates = unique(as.Date(dts))
  daily_df = data.frame(
    year = as.integer(format(daily_dates, "%Y")),
    month = as.integer(format(daily_dates, "%m")),
    day = as.integer(format(daily_dates, "%d")),
    flow_cfs = rep(10, length(daily_dates))
  )

  res_zero_sim = adjustq(daily_flow = daily_df, inst_flow = inst_df, sim = sim_df, blend = 10L)
  expect_false(anyNA(res_zero_sim$flow_cfs))
  expect_true(all(is.finite(res_zero_sim$flow_cfs)))

  # Large gap with blend = 0 preserves observed endpoints rather than zeroing end_diff
  inst_large_keep = c(1, 15:20)
  inst_large = data.frame(
    year = sim_df$year[inst_large_keep],
    month = sim_df$month[inst_large_keep],
    day = sim_df$day[inst_large_keep],
    hour = sim_df$hour[inst_large_keep],
    flow_cfs = rep(25, length(inst_large_keep))
  )
  res_blend0 = adjustq(
    daily_flow = daily_df,
    inst_flow = inst_large,
    sim = sim_df,
    blend = 0L,
    max_iterations = 0L
  )
  expect_equal(res_blend0$flow_cfs[15], 25)
  expect_error(adjustq(daily_flow = daily_df, inst_flow = inst_large[0, ]), "inst_flow")
})
