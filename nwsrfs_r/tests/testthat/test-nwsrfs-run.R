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
