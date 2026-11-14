# %%
# Tests for hydrological alignment, validation metrics, and ggplot2 diagnostics

test_that("sim_to_daily aligns 6-hour runs and removes spin-up water years", {
  run_nrkw1 = load_example("NRKW1")

  d_post = sim_to_daily(run_nrkw1, spinup_years = 1L)
  d_all = sim_to_daily(run_nrkw1, spinup_years = 0L)

  expect_true(all(c("year", "month", "day", "date", "wy", "sim_cfs", "obs_cfs", "Source", "local_cfs", "lagk_cfs") %in% names(d_post)))
  expect_equal(min(d_all$wy), 1980L)
  expect_equal(min(d_post$wy), 1981L)
  expect_false(anyNA(d_post$sim_cfs))

  # At NRKW1 (no chanloss/consuse), daily outlet flow equals local + routed Lag-K
  expect_equal(d_post$sim_cfs, d_post$local_cfs + d_post$lagk_cfs, tolerance = 1e-6)

  # Numeric vector interface matches nwsrfs_run interface
  d_vec = sim_to_daily(run_nrkw1$sim, obs = FALSE, ts = run_nrkw1$forcings[[1L]], spinup_years = 1L)
  expect_equal(d_vec$sim_cfs, d_post$sim_cfs)
  expect_false("obs_cfs" %in% names(d_vec))
})

test_that("flow_metrics computes overall, monthly, quantile, water-year, and multi-run tables", {
  run_nrkw1 = load_example("NRKW1")

  m_all = flow_metrics(run_nrkw1)
  expect_equal(nrow(m_all), 1L)
  expect_true(all(c("n", "NSE", "logNSE", "KGE", "r", "alpha", "beta", "PBIAS", "RMSE", "MAE") %in% names(m_all)))
  expect_gt(m_all$NSE, 0.75)
  expect_gt(m_all$KGE, 0.80)

  m_usgs = flow_metrics(run_nrkw1, source = "USGS_12210700")
  expect_lt(m_usgs$n, m_all$n)
  expect_gt(m_usgs$NSE, 0.80)

  m_mon = flow_metrics(run_nrkw1, by = "month", source = "USGS_12210700")
  expect_equal(nrow(m_mon), 12L)
  expect_equal(as.character(m_mon$month_label[1L]), "Oct")
  expect_equal(as.character(m_mon$month_label[12L]), "Sep")

  m_q = flow_metrics(run_nrkw1, by = "quantile", source = "USGS_12210700")
  expect_equal(nrow(m_q), 5L)
  expect_equal(sum(m_q$n), m_usgs$n)

  m_wy = flow_metrics(run_nrkw1, by = "water_year")
  expect_equal(nrow(m_wy), 42L)

  run_pert = update_pars(run_nrkw1, data.frame(p_name = "mfmax_NRKW1-2", value = 1.5))
  m_multi = flow_metrics(list(Baseline = run_nrkw1, Perturbed = run_pert), source = "USGS_12210700")
  expect_equal(m_multi$run, c("Baseline", "Perturbed"))
})

test_that("all plotting utilities build valid ggplot objects on NRKW1 and SFLN2", {
  skip_if_not_installed("ggplot2")

  run_nrkw1 = load_example("NRKW1")
  run_sfln2 = load_example("SFLN2")
  run_pert = update_pars(run_nrkw1, data.frame(p_name = "mfmax_NRKW1-2", value = 1.5))

  assert_builds = function(p) {
    expect_s3_class(p, "gg")
    built = ggplot2::ggplot_build(p)
    expect_gt(length(built$data), 0L)
  }

  # Parameter, routing, and forcing plots
  assert_builds(plot_uh(run_nrkw1))
  assert_builds(plot_adc(run_nrkw1))
  assert_builds(plot_lagk(run_nrkw1))
  expect_error(plot_lagk(run_sfln2), "No Lag-K")
  assert_builds(plot_forcing_adjust(run_nrkw1, zone = "NRKW1-2"))
  assert_builds(plot_forcing_adjust(run_nrkw1))

  # Internal state, runoff pathway, and water-balance plots
  assert_builds(plot_states(run_nrkw1, water_year = 2011L))
  assert_builds(plot_runoff_regime(run_nrkw1, normalize = TRUE, by_zone = TRUE))
  assert_builds(plot_water_balance(run_nrkw1, type = "budyko"))
  assert_builds(plot_water_balance(run_sfln2, type = "bars"))

  # Streamflow and validation report plots
  assert_builds(plot(run_nrkw1, water_year = 2011L, decompose = TRUE))
  assert_builds(plot_hydrograph(run_nrkw1, start = "2003-10-01", end = "2005-09-30", shade_source = "Data_Ranger"))
  assert_builds(plot_hydrograph(
    list(Baseline = run_nrkw1, Perturbed = run_pert),
    start = "2011-03-01",
    end = "2011-09-30",
    component = "local",
    obs = FALSE
  ))
  assert_builds(plot_water_year(run_nrkw1, log = TRUE))
  assert_builds(plot_water_year(run_sfln2, stages = TRUE))
  assert_builds(plot_monthly_bias(run_nrkw1, source = "USGS_12210700"))
  assert_builds(plot_fdc(list(Baseline = run_nrkw1, Perturbed = run_pert), source = "USGS_12210700"))
  assert_builds(plot_scatter(run_nrkw1, quantile_min = 0.95, source = "USGS_12210700"))
  assert_builds(plot_cumulative(run_nrkw1, type = "flow"))
  assert_builds(plot_cumulative(run_nrkw1, type = "error"))
  assert_builds(plot_water_supply(run_nrkw1, months = 4L:9L, type = "scatter"))
  assert_builds(plot_water_supply(run_nrkw1, months = 4L:9L, type = "timeseries"))
})
