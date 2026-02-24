test_that("sac_snow + uh chain produces non-zero flow", {
  data("nrkw1_forcing", package = "nwsrfsr")
  data("nrkw1_pars", package = "nwsrfsr")

  adj = fa_nwrfc(6, nrkw1_forcing, nrkw1_pars)
  tci = sac_snow(6, adj, nrkw1_pars)

  expect_true(is.numeric(tci))
  expect_true(any(tci > 0))

  sf = uh(6, tci, nrkw1_pars, sum_zones = TRUE)
  expect_true(is.numeric(sf))
  expect_true(any(sf > 0))
  expect_equal(length(sf), nrow(nrkw1_forcing[[1]]))
})


test_that("lagk with return_states works (bug fix: uptribs reference)", {
  data("nrkw1_upflow", package = "nwsrfsr")
  data("nrkw1_pars", package = "nwsrfsr")

  result = lagk(6, nrkw1_upflow, nrkw1_pars, sum_routes = FALSE, return_states = TRUE)

  expect_true(is.data.frame(result))
  expect_true("year" %in% names(result))
  expect_true(any(grepl("routed", names(result))))
  expect_true(nrow(result) > 0)
})


test_that("chanloss returns same length as input", {
  data("nrkw1_forcing", package = "nwsrfsr")
  data("nrkw1_pars", package = "nwsrfsr")

  n = 100
  flow = rep(500, n)
  forcing = list(`zone1` = nrkw1_forcing[[1]][1:n, ])

  result = chanloss(flow, forcing, 6, nrkw1_pars)
  expect_equal(length(result), n)
})


test_that("chanloss period parameter passed as integer (SFLN2 regression)", {
  data("sfln2_pars", package = "nwsrfsr")
  data("sfln2_forcing", package = "nwsrfsr")

  forcing = sfln2_forcing[c("SFLN2-1", "SFLN2-2")]
  adj = fa_nwrfc(6, forcing, sfln2_pars)
  tci = sac_snow(6, adj, sfln2_pars)
  sf = uh(6, tci, sfln2_pars, sum_zones = TRUE, start_of_timestep = TRUE, backfill = TRUE)

  sf_cl = chanloss(sf, sfln2_forcing["SFLN2-1"], 6, sfln2_pars)

  # Chanloss should significantly reduce flow for SFLN2 (factors 0.29-0.69)
  expect_true(mean(sf_cl) < mean(sf) * 0.7)
})
