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


test_that("adjustq supports 1, 2, 3, 4, 6, 8, 12, and 24 hour timesteps with and without sim", {
  supported_dts = c(1L, 2L, 3L, 4L, 6L, 8L, 12L, 24L)
  daily_dates = seq(as.Date("2020-01-01"), as.Date("2020-01-12"), by = "1 day")
  n_days = length(daily_dates)
  target_daily = 100 + 25 * sin(seq_len(n_days) * 0.6)
  daily_df = data.frame(
    year = as.integer(format(daily_dates, "%Y")),
    month = as.integer(format(daily_dates, "%m")),
    day = as.integer(format(daily_dates, "%d")),
    flow_cfs = target_daily
  )

  # High-resolution (hourly) instantaneous observations with both a small gap and a large gap
  inst_dts_full = seq(
    as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
    as.POSIXct("2020-01-13 00:00:00", tz = "UTC"),
    by = 3600
  )
  n_hr = length(inst_dts_full)
  inst_vals = 95 + 30 * sin(seq_len(n_hr) * 2 * pi / 24)
  # Create a 12-hour small/medium gap on day 3 and a 48-hour large gap on days 7-8
  drop_small = inst_dts_full >= as.POSIXct("2020-01-03 06:00:00", tz = "UTC") &
    inst_dts_full <= as.POSIXct("2020-01-03 18:00:00", tz = "UTC")
  drop_large = inst_dts_full >= as.POSIXct("2020-01-07 00:00:00", tz = "UTC") &
    inst_dts_full <= as.POSIXct("2020-01-08 18:00:00", tz = "UTC")
  inst_keep = !(drop_small | drop_large)
  inst_dts = inst_dts_full[inst_keep]
  inst_df = data.frame(
    year = as.integer(format(inst_dts, "%Y")),
    month = as.integer(format(inst_dts, "%m")),
    day = as.integer(format(inst_dts, "%d")),
    hour = as.integer(format(inst_dts, "%H")),
    flow_cfs = inst_vals[inst_keep]
  )

  daily_volume_check = function(res_df, dt_hr) {
    if (dt_hr == 24L) {
      return(data.frame(date = as.Date(res_df$datetime), daily_mean = res_df$flow_cfs))
    }
    k = 24L %/% dt_hr
    t_sec = as.numeric(res_df$datetime)
    hrs = as.integer(format(res_df$datetime, "%H"))
    dts = as.Date(res_df$datetime)
    idx0 = which(hrs == 0L)
    valid = idx0[idx0 + k <= nrow(res_df) & (t_sec[pmin(idx0 + k, nrow(res_df))] - t_sec[idx0] == 86400)]
    means = vapply(valid, \(i0) {
      (0.5 * res_df$flow_cfs[i0] +
        sum(res_df$flow_cfs[(i0 + 1L):(i0 + k - 1L)]) +
        0.5 * res_df$flow_cfs[i0 + k]) / k
    }, numeric(1L))
    data.frame(date = dts[valid], daily_mean = means)
  }

  for (dt_hr in supported_dts) {
    sim_dts = seq(
      as.POSIXct("2020-01-01 00:00:00", tz = "UTC"),
      as.POSIXct("2020-01-12 24:00:00", tz = "UTC") - dt_hr * 3600,
      by = dt_hr * 3600
    )
    sim_df = data.frame(
      year = as.integer(format(sim_dts, "%Y")),
      month = as.integer(format(sim_dts, "%m")),
      day = as.integer(format(sim_dts, "%d")),
      hour = as.integer(format(sim_dts, "%H")),
      flow_cfs = 80 + 20 * cos(seq_along(sim_dts) * 2 * pi / (24 / dt_hr))
    )

    # With sim: infer dt_hours automatically and verify explicit dt_hours matches
    res_sim = adjustq(
      daily_flow = daily_df,
      inst_flow = inst_df,
      sim = sim_df,
      blend = max(2L, 24L %/% dt_hr),
      error_tol = 0.01,
      max_iterations = 25L
    )
    res_sim_explicit = adjustq(
      daily_flow = daily_df,
      inst_flow = inst_df,
      sim = sim_df,
      blend = max(2L, 24L %/% dt_hr),
      error_tol = 0.01,
      max_iterations = 25L,
      dt_hours = dt_hr
    )
    expect_equal(res_sim, res_sim_explicit)
    expect_equal(unique(as.numeric(diff(res_sim$datetime), units = "hours")), dt_hr)
    expect_false(anyNA(res_sim$flow_cfs))
    expect_true(all(res_sim$flow_cfs >= 0))

    td_sim = merge(daily_volume_check(res_sim, dt_hr), data.frame(date = daily_dates, obs = target_daily), by = "date")
    rel_err_sim = abs(td_sim$daily_mean - td_sim$obs) / td_sim$obs
    expect_lt(max(rel_err_sim), 0.02)

    # Without sim: explicit dt_hours
    res_nosim = adjustq(
      daily_flow = daily_df,
      inst_flow = inst_df,
      sim = NULL,
      error_tol = 0.01,
      max_iterations = 25L,
      dt_hours = dt_hr
    )
    expect_equal(unique(as.numeric(diff(res_nosim$datetime), units = "hours")), dt_hr)
    expect_false(anyNA(res_nosim$flow_cfs))
    expect_true(all(res_nosim$flow_cfs >= 0))

    td_nosim = merge(daily_volume_check(res_nosim, dt_hr), data.frame(date = daily_dates, obs = target_daily), by = "date")
    rel_err_nosim = abs(td_nosim$daily_mean - td_nosim$obs) / td_nosim$obs
    expect_lt(max(rel_err_nosim), 0.02)
  }

  # Phase-shifted 12-hour simulation (06:00 and 18:00)
  sim_12_shift_dts = seq(
    as.POSIXct("2020-01-01 06:00:00", tz = "UTC"),
    as.POSIXct("2020-01-12 18:00:00", tz = "UTC"),
    by = 12 * 3600
  )
  sim_12_shift = data.frame(
    year = as.integer(format(sim_12_shift_dts, "%Y")),
    month = as.integer(format(sim_12_shift_dts, "%m")),
    day = as.integer(format(sim_12_shift_dts, "%d")),
    hour = as.integer(format(sim_12_shift_dts, "%H")),
    flow_cfs = rep(90, length(sim_12_shift_dts))
  )
  res_12_shift = adjustq(daily_df, inst_df, sim = sim_12_shift, blend = 2L)
  expect_equal(unique(as.numeric(diff(res_12_shift$datetime), units = "hours")), 12)
  expect_false(anyNA(res_12_shift$flow_cfs))

  # Validation checks
  expect_error(adjustq(daily_df, inst_df, sim = NULL, dt_hours = 5L), "Unsupported dt_hours")
  expect_error(adjustq(daily_df, inst_df, sim = NULL, dt_hours = 0L), "Unsupported|positive integer")

  sim_6h_dts = seq(as.POSIXct("2020-01-01 00:00:00", tz = "UTC"), by = 6 * 3600, length.out = 12)
  sim_6h = data.frame(
    year = as.integer(format(sim_6h_dts, "%Y")),
    month = as.integer(format(sim_6h_dts, "%m")),
    day = as.integer(format(sim_6h_dts, "%d")),
    hour = as.integer(format(sim_6h_dts, "%H")),
    flow_cfs = rep(100, length(sim_6h_dts))
  )
  expect_error(adjustq(daily_df, inst_df, sim = sim_6h, dt_hours = 12L), "step by exactly 12 hour")
})
