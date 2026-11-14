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


test_that("forcing_adj = FALSE turns off climatological forcing adjustments", {
  adj = load_example("NRKW1")
  noadj = load_example("NRKW1", forcing_adj = FALSE)

  # Matches Python's forcing_adj=False: FA still runs (it computes pet and etd)
  # but with neutral parameters, so the simulation must change.
  expect_false(isTRUE(all.equal(noadj$sim, adj$sim)))
  expect_equal(noadj$.forcing_adj, FALSE)

  # Equivalent to building the chain by hand with a dry-run FA
  fa = fa_nwrfc(6L, nrkw1_forcing, nrkw1_pars, dry_run = TRUE)
  tci = sac_snow(6L, fa, nrkw1_pars)
  expected = uh(6L, tci, nrkw1_pars, sum_zones = TRUE) +
    lagk(6L, nrkw1_upflow, nrkw1_pars, sum_routes = TRUE)
  expect_equal(noadj$sim, expected)
})


test_that("forcing_adj accepts a subset of forcing types", {
  all_adj = load_example("SFLN2")
  none_adj = load_example("SFLN2", forcing_adj = FALSE)
  some_adj = load_example("SFLN2", forcing_adj = c("map", "ptps"))

  # Adjusting only map and ptps must differ from both extremes
  expect_false(isTRUE(all.equal(some_adj$sim, all_adj$sim)))
  expect_false(isTRUE(all.equal(some_adj$sim, none_adj$sim)))

  # Case insensitive and order independent, as in Python
  expect_equal(load_example("SFLN2", forcing_adj = c("PTPS", "Map"))$sim, some_adj$sim)

  # Naming all four types is the same as TRUE
  expect_equal(
    load_example("SFLN2", forcing_adj = c("map", "mat", "ptps", "pet"))$sim,
    all_adj$sim
  )

  expect_error(load_example("SFLN2", forcing_adj = "rain"), "map, mat, ptps, pet")
})


test_that("return_inst = FALSE returns period-average local flow", {
  inst = load_example("NRKW1")
  peravg = load_example("NRKW1", return_inst = FALSE)

  expect_false(isTRUE(all.equal(peravg$sim, inst$sim)))
  expect_equal(peravg$.return_inst, FALSE)

  # Python averages the UH zone flow before the shift_sf shift and before
  # Lag-K is added, so only the local flow component changes.
  expect_equal(peravg$lagk_route, inst$lagk_route)
  expect_equal(peravg$sacsnow_tci, inst$sacsnow_tci)

  fa = fa_nwrfc(6L, nrkw1_forcing, nrkw1_pars)
  tci = sac_snow(6L, fa, nrkw1_pars)
  expected_sf = uh(6L, tci, nrkw1_pars, sum_zones = TRUE, return_inst = FALSE)
  expect_equal(peravg$sacsnow_sf, expected_sf)
  expect_equal(peravg$sim, expected_sf + peravg$lagk_route)
})


test_that("uh return_inst averages adjacent timesteps before shifting", {
  fa = fa_nwrfc(6L, nrkw1_forcing, nrkw1_pars)
  tci = sac_snow(6L, fa, nrkw1_pars)

  inst = uh(6L, tci, nrkw1_pars, sum_zones = TRUE, start_of_timestep = FALSE)
  peravg = uh(
    6L,
    tci,
    nrkw1_pars,
    sum_zones = TRUE,
    start_of_timestep = FALSE,
    return_inst = FALSE
  )

  # (x[i] + x[i + 1]) / 2 with the final value carried forward
  n = length(inst)
  expect_equal(peravg, (inst + c(inst[-1], inst[n])) / 2)

  # Works per zone as well
  peravg_zones = uh(
    6L,
    tci,
    nrkw1_pars,
    sum_zones = FALSE,
    start_of_timestep = FALSE,
    return_inst = FALSE
  )
  expect_equal(rowSums(peravg_zones), peravg)
})


test_that("pars drops the n_clmods bookkeeping row, as Python does", {
  nrkw1 = load_example("NRKW1")
  sfln2 = load_example("SFLN2")

  expect_false("n_clmods" %in% nrkw1$pars$name)
  expect_false("n_clmods" %in% sfln2$pars$name)

  # One row fewer than the bundled parameter tables
  expect_equal(nrow(nrkw1$pars), nrow(nrkw1_pars) - 1L)
  expect_equal(nrow(sfln2$pars), nrow(sfln2_pars) - 1L)

  # Chanloss detection must not depend on that row
  expect_false(nrkw1$chanloss_logic)
  expect_true(sfln2$chanloss_logic)
})


test_that("chanloss survives a pars round trip through update_pars", {
  run = load_example("SFLN2")

  # A no-op parameter update must reproduce the simulation exactly. If dropping
  # n_clmods silently disabled chanloss or consuse, the flow would change.
  noop = data.frame(
    p_name = "uztwm_SFLN2-1",
    value = run$pars$value[run$pars$p_name == "uztwm_SFLN2-1"]
  )
  run2 = update_pars(run, noop)

  expect_true(run2$chanloss_logic)
  expect_true(run2$consuse_logic)
  expect_equal(run2$sim, run$sim)
})


test_that("chanloss infers the module count when n_clmods is absent", {
  flow = load_example("SFLN2", forcing_adj = FALSE)$sacsnow_sf
  forcing = sfln2_forcing["SFLN2-1"]

  with_row = chanloss(flow, forcing, 6L, sfln2_pars)
  without_row = chanloss(flow, forcing, 6L, sfln2_pars[sfln2_pars$name != "n_clmods", ])

  expect_equal(without_row, with_row)
  expect_false(isTRUE(all.equal(with_row, flow)))
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


test_that("nwsrfs_run exposes UH metadata, lookup tables, and chanloss wraparound/overlap", {
  nrkw1 = load_example("NRKW1")
  sfln2 = load_example("SFLN2")

  expect_equal(nrkw1$uh_type, "gamma")
  expect_equal(nrkw1$uh_zone_names, c("NRKW1-1", "NRKW1-2"))
  expect_false(nrkw1$shared_uh_logic)
  expect_true(nrkw1$ptps_logic)
  expect_false(nrkw1$area_elev_logic)
  expect_null(nrkw1$area_elev)
  expect_true(is.data.frame(nrkw1$uh))
  expect_equal(names(nrkw1$uh), c("hours", "NRKW1-1", "NRKW1-2"))
  expect_equal(nrkw1$peadj_table$month, 1L:12L)
  expect_null(nrkw1$chanloss_table)

  expect_true(is.data.frame(sfln2$chanloss_table))
  expect_equal(sfln2$chanloss_table$month, 1L:12L)
  expect_true(is.data.frame(sfln2$consuse_peadj_table))
  expect_equal(names(sfln2$consuse_peadj_table), c("month", "SFLN2-CU"))

  # Verify year-boundary wraparound (11..2) and overlapping month averaging in .build_chanloss_table()
  syn_cl = data.frame(
    p_name = c(
      "cl_type", "cl_min_q",
      "cl_period_start_01", "cl_period_end_01", "cl_factor_01",
      "cl_period_start_02", "cl_period_end_02", "cl_factor_02"
    ),
    name = c(
      "cl_type", "cl_min_q",
      "cl_period_start_01", "cl_period_end_01", "cl_factor_01",
      "cl_period_start_02", "cl_period_end_02", "cl_factor_02"
    ),
    type = "chanloss",
    zone = "SYN",
    value = c(1, 0, 11, 2, 0.6, 2, 4, 0.8)
  )
  tbl = nwsrfsr:::.build_chanloss_table(syn_cl)
  expect_equal(tbl$chanloss_adj[1L], 0.6)
  expect_equal(tbl$chanloss_adj[2L], 0.7)
  expect_equal(tbl$chanloss_adj[3L], 0.8)
  expect_equal(tbl$chanloss_adj[7L], 1.0)
})


test_that("nwsrfs_run supports area_elev + RSnwElev, ordinate/shared UH, and 12-hour timesteps", {
  data("sfln2_pars", package = "nwsrfsr")
  data("sfln2_forcing", package = "nwsrfsr")
  data("sfln2_daily_flow", package = "nwsrfsr")
  data("area_elev_curve", package = "nwsrfsr")

  td = tempfile("autocalb_rsnw_")
  dir.create(file.path(td, "results_por_01"), recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)

  # Subset 2 full years of SFLN2 (all 12 calendar months) for fast directory tests
  n_sub = 2924L
  for (z in names(sfln2_forcing)) {
    df = sfln2_forcing[[z]][seq_len(n_sub), c("year", "month", "day", "hour", "map_mm", "mat_degc")]
    write.csv(df, file.path(td, paste0("forcing_por_", z, ".csv")), row.names = FALSE)
  }
  write.csv(sfln2_daily_flow[1:731, ], file.path(td, "flow_daily_SFLN2.csv"), row.names = FALSE)

  # Build 3-zone area_elev CSV (including SFLN2-CU)
  ae3 = area_elev_curve
  ae3[["SFLN2-CU"]] = ae3[["SFLN2-1"]]
  write.csv(ae3, file.path(td, "area_elev_SFLN2.csv"), row.names = FALSE)

  # Remove ptps_lower_* / ptps_upper_* limit rows to verify RSnwElev runs don't require them,
  # and add pxtemp + talr rows for all 3 zones
  rs_pars = sfln2_pars[!grepl("^ptps_(lower|upper)_", sfln2_pars$name), ]
  zones = c("SFLN2-1", "SFLN2-2", "SFLN2-CU")
  rs_add = data.frame(
    p_name = c(paste0("pxtemp_", zones), paste0("talr_", zones)),
    name = rep(c("pxtemp", "talr"), each = length(zones)),
    type = "rsnwelev",
    zone = rep(zones, 2L),
    value = c(rep(0.5, 3L), rep(0.55, 3L))
  )
  rs_pars = rbind(rs_pars, rs_add)
  write.csv(rs_pars, file.path(td, "results_por_01", "pars_optimal.csv"), row.names = FALSE)

  run_rs = nwsrfs_run(td, "results_por_01")
  expect_false(run_rs$ptps_logic)
  expect_true(run_rs$area_elev_logic)
  expect_true(is.data.frame(run_rs$area_elev))
  expect_false("ptps" %in% names(run_rs$forcing_raw[[1L]]))
  expect_true("ptps" %in% names(run_rs$forcings[[1L]]))

  # Updating pxtemp via update_pars recalculates RSnwElev ptps and changes simulated flow
  run_rs2 = update_pars(
    run_rs,
    data.frame(p_name = c("pxtemp_SFLN2-1", "pxtemp_SFLN2-2"), value = c(3.5, 3.5))
  )
  expect_false(identical(run_rs2$forcings[["SFLN2-1"]]$ptps, run_rs$forcings[["SFLN2-1"]]$ptps))
  expect_false(identical(run_rs2$sim, run_rs$sim))

  # Ambiguous: both ptps in forcing CSVs and area_elev CSV present
  td_amb = tempfile("autocalb_amb_")
  dir.create(file.path(td_amb, "results_por_01"), recursive = TRUE)
  on.exit(unlink(td_amb, recursive = TRUE), add = TRUE)
  for (z in names(sfln2_forcing)) {
    write.csv(
      sfln2_forcing[[z]][seq_len(n_sub), ],
      file.path(td_amb, paste0("forcing_por_", z, ".csv")),
      row.names = FALSE
    )
  }
  write.csv(sfln2_daily_flow[1:731, ], file.path(td_amb, "flow_daily_SFLN2.csv"), row.names = FALSE)
  write.csv(ae3, file.path(td_amb, "area_elev_SFLN2.csv"), row.names = FALSE)
  write.csv(sfln2_pars, file.path(td_amb, "results_por_01", "pars_optimal.csv"), row.names = FALSE)
  expect_error(nwsrfs_run(td_amb, "results_por_01"), "Ambiguous precipitation typing")

  # Missing both ptps and area_elev CSV
  unlink(file.path(td, "area_elev_SFLN2.csv"))
  expect_error(nwsrfs_run(td, "results_por_01"), "Precipitation typing requires")

  # Ambiguous multiple flow_instantaneous_*.csv files
  unlink(file.path(td_amb, "area_elev_SFLN2.csv"))
  inst_stub = data.frame(year = 1979L, month = 10L, day = 1L, hour = 0L, flow_cfs = 10)
  write.csv(inst_stub, file.path(td_amb, "flow_instantaneous_a.csv"), row.names = FALSE)
  write.csv(inst_stub, file.path(td_amb, "flow_instantaneous_b.csv"), row.names = FALSE)
  expect_error(nwsrfs_run(td_amb, "results_por_01"), "Instantaneous flow csv file is ambiguous")
  unlink(file.path(td_amb, c("flow_instantaneous_a.csv", "flow_instantaneous_b.csv")))

  # Ordinate UH and shared UH via nwsrfs_run() + 12-hour simulation with Consuse
  run_g = nwsrfs_run(td_amb, "results_por_01")
  uh_g = run_g$uh
  non_uh = sfln2_pars[!(sfln2_pars$type == "uh" & sfln2_pars$name != "zone_area"), ]
  ord_rows = do.call(
    rbind,
    lapply(zones, \(z) {
      v = uh_g[[z]]
      v = v[!is.na(v)]
      data.frame(
        p_name = paste0("uh_", seq_along(v) - 1L, "_", z),
        name = paste0("uh_", seq_along(v) - 1L),
        type = "uh",
        zone = z,
        value = v
      )
    })
  )
  write.csv(rbind(non_uh, ord_rows), file.path(td_amb, "results_por_01", "pars_optimal.csv"), row.names = FALSE)
  run_o = nwsrfs_run(td_amb, "results_por_01")
  expect_equal(run_o$uh_type, "ordinate")
  expect_equal(run_o$sim, run_g$sim, tolerance = 1e-3)

  # Shared gamma UH on SFLN2 at 12-hour timestep
  idx_12 = seq(1L, n_sub, by = 2L)
  for (z in names(sfln2_forcing)) {
    df12 = sfln2_forcing[[z]][idx_12, ]
    df12$map_mm = df12$map_mm * 2
    write.csv(df12, file.path(td_amb, paste0("forcing_por_", z, ".csv")), row.names = FALSE)
  }
  shared_uh = rbind(
    non_uh,
    data.frame(
      p_name = c("unit_shape_SFLN2-UH", "unit_toc_SFLN2-UH", "unit_toc_adj_SFLN2-UH"),
      name = c("unit_shape", "unit_toc", "unit_toc_adj"),
      type = "uh",
      zone = "SFLN2-UH",
      value = c(2.0, 24.0, 1.0)
    )
  )
  write.csv(shared_uh, file.path(td_amb, "results_por_01", "pars_optimal.csv"), row.names = FALSE)
  run_12 = nwsrfs_run(td_amb, "results_por_01")
  expect_equal(run_12$dt_hours, 12L)
  expect_true(run_12$shared_uh_logic)
  expect_equal(run_12$uh_zone_names, "SFLN2-UH")
  expect_equal(length(run_12$sim), length(idx_12))
  expect_true(all(is.finite(run_12$sim)))
})


test_that("update_pars validates inputs and preserves run on error or NA values", {
  run = load_example("NRKW1")
  orig_sim = run$sim

  # Empty new_pars is a no-op
  run_empty = update_pars(run, data.frame(p_name = character(0), value = numeric(0)))
  expect_equal(run_empty$sim, orig_sim)

  # NA in value retains existing parameter value
  run_na = update_pars(run, data.frame(p_name = "uztwm_NRKW1-1", value = NA_real_))
  expect_equal(run_na$sim, orig_sim)

  # Input validation checks
  expect_error(update_pars(run, list(p_name = "uztwm_NRKW1-1", value = 50)), "data.frame")
  dup_cols = data.frame(p_name = "uztwm_NRKW1-1", value = 50, value = 60, check.names = FALSE)
  expect_error(update_pars(run, dup_cols), "duplicate column")
  expect_error(update_pars(run, data.frame(p_name = NA_character_, value = 50)), "missing")
  expect_error(
    update_pars(run, data.frame(p_name = c("uztwm_NRKW1-1", "uztwm_NRKW1-1"), value = c(50, 60))),
    "Duplicate p_name"
  )
  expect_error(update_pars(run, data.frame(p_name = "not_a_par", value = 1)), "unknown")

  # Invalid model parameter value fails validation without mutating 'run'
  expect_error(update_pars(run, data.frame(p_name = "uztwm_NRKW1-1", value = -10)), "must be >= 0")
  expect_equal(run$sim, orig_sim)
})


test_that("high-level and low-level model pipelines compose with R native pipe |>", {
  # High-level nwsrfs_run pipeline: load -> update_pars -> sim_to_daily -> flow_metrics
  metrics_piped =
    load_example("NRKW1") |>
    update_pars(data.frame(p_name = "uztwm_NRKW1-1", value = 80)) |>
    sim_to_daily(spinup_years = 1L) |>
    flow_metrics(source = "USGS_12210700")

  run_direct = update_pars(
    load_example("NRKW1"),
    data.frame(p_name = "uztwm_NRKW1-1", value = 80)
  )
  metrics_direct = flow_metrics(run_direct, source = "USGS_12210700", spinup_years = 1L)
  expect_equal(metrics_piped, metrics_direct)

  # Piping run into uh_ordinates and adjustq(sim = run)
  uh_piped =
    run_direct |>
    uh_ordinates(dt_hours = 3L)
  expect_equal(uh_piped, uh_ordinates(run_direct, dt_hours = 3L))

  aq_from_run = adjustq(nrkw1_daily_flow, nrkw1_inst_flow, sim = run_direct)
  sim_df_direct = data.frame(
    run_direct$forcings[[1L]][, c("year", "month", "day", "hour")],
    flow_cfs = run_direct$sim
  )
  aq_from_df = adjustq(nrkw1_daily_flow, nrkw1_inst_flow, sim = sim_df_direct)
  expect_equal(aq_from_run, aq_from_df)

  # Low-level wrapper pipeline on a 1-year subset using named dt_hours and pars
  sub_idx = seq_len(1464L)
  f_sub = lapply(nrkw1_forcing, \(df) df[sub_idx, ])
  uf_sub = lapply(nrkw1_upflow, \(df) df[sub_idx, ])

  local_piped =
    f_sub |>
    fa_nwrfc(dt_hours = 6L, pars = nrkw1_pars) |>
    sac_snow(dt_hours = 6L, pars = nrkw1_pars) |>
    uh(dt_hours = 6L, pars = nrkw1_pars) |>
    chanloss(forcing = f_sub, dt_hours = 6L, pars = nrkw1_pars)

  routed_piped =
    uf_sub |>
    lagk(dt_hours = 6L, pars = nrkw1_pars)

  direct_total = sac_snow_uh_lagk(
    6L,
    fa_nwrfc(6L, f_sub, nrkw1_pars),
    uf_sub,
    nrkw1_pars
  )
  expect_equal(local_piped + routed_piped, direct_total)
})
