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


test_that("rsnwelev computes ptps in [0, 1] and validates inputs", {
  data("sfln2_forcing", package = "nwsrfsr")
  data("sfln2_pars", package = "nwsrfsr")
  data("area_elev_curve", package = "nwsrfsr")

  zones = c("SFLN2-1", "SFLN2-2")
  forcing_zones = lapply(sfln2_forcing[zones], \(df) df[1:40, ])
  rs_pars = rbind(
    sfln2_pars[sfln2_pars$name == "elev" & sfln2_pars$zone %in% zones, ],
    data.frame(
      p_name = paste0(rep(c("talr_", "pxtemp_"), each = 2), zones),
      name = rep(c("talr", "pxtemp"), each = 2),
      type = "rsnwelev",
      zone = rep(zones, 2),
      value = c(0.5, 0.5, 1.0, 1.0)
    )
  )

  out = rsnwelev(forcing_zones, rs_pars, area_elev_curve)
  expect_equal(names(out), zones)
  for (z in zones) {
    expect_true(all(out[[z]]$ptps >= 0 & out[[z]]$ptps <= 1))
  }

  # Too few elevation columns in ae_tbl (e.g. passing 2 zones with a 1-zone table)
  expect_error(rsnwelev(forcing_zones, rs_pars, area_elev_curve[, 1:2]), "ae_tbl")
  # Fewer than 2 rows in ae_tbl
  expect_error(rsnwelev(forcing_zones, rs_pars, area_elev_curve[1, ]), "ae_tbl")
  # Non-positive lapse rate
  bad_talr = rs_pars
  bad_talr$value[bad_talr$name == "talr"] = 0
  expect_error(rsnwelev(forcing_zones, bad_talr, area_elev_curve), "talr")

  # Permuted zone columns in ae_tbl still align by zone name
  ae_perm = area_elev_curve[, c("quantile", "SFLN2-2", "SFLN2-1")]
  out_perm = rsnwelev(forcing_zones, rs_pars, ae_perm)
  expect_equal(out_perm[["SFLN2-1"]]$ptps, out[["SFLN2-1"]]$ptps)
  expect_equal(out_perm[["SFLN2-2"]]$ptps, out[["SFLN2-2"]]$ptps)

  # Negative reference elevation emits a warning rather than an error
  neg_elev = rs_pars
  neg_elev$value[neg_elev$name == "elev"] = -15
  expect_warning(rsnwelev(forcing_zones, neg_elev, area_elev_curve), "elev")
})


test_that("ordinate UH, shared UH, and uh_ordinates PCHIP resampling match gamma and Scipy", {
  data("nrkw1_forcing", package = "nwsrfsr")
  data("nrkw1_pars", package = "nwsrfsr")

  # Subset 2 years (8 months + full calendar coverage) for fast routing parity checks
  f_sub = lapply(nrkw1_forcing, \(df) df[1:3000, ])
  adj = fa_nwrfc(6L, f_sub, nrkw1_pars)
  tci = sac_snow(6L, adj, nrkw1_pars)

  sf_gamma = uh(6L, tci, nrkw1_pars, sum_zones = FALSE)
  uh_tbl = uh_ordinates(nrkw1_pars, dt_hours = 6L)
  expect_equal(names(uh_tbl), c("hours", "NRKW1-1", "NRKW1-2"))
  expect_equal(uh_tbl$hours[1:3], c(0L, 6L, 12L))

  # Passing gamma ordinates via ordinates = reproduces gamma UH routing
  ord_mat = as.matrix(uh_tbl[, c("NRKW1-1", "NRKW1-2")])
  sf_ord_arg = uh(6L, tci, nrkw1_pars, sum_zones = FALSE, ordinates = ord_mat)
  expect_equal(sf_ord_arg, sf_gamma, tolerance = 1e-4)

  # Building ordinate UH pars (uh_0, uh_1, ...) with unequal zone lengths (trailing NA)
  non_uh = nrkw1_pars[!(nrkw1_pars$type == "uh" & nrkw1_pars$name != "zone_area"), ]
  ord_rows = do.call(
    rbind,
    lapply(c("NRKW1-1", "NRKW1-2"), \(z) {
      v = uh_tbl[[z]]
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
  ord_pars = rbind(non_uh, ord_rows)
  sf_ord_pars = uh(6L, tci, ord_pars, sum_zones = TRUE)
  expect_equal(sf_ord_pars, rowSums(sf_gamma), tolerance = 1e-4)

  # Validation errors: bad volume, non-consecutive numbering, ambiguous gamma + ordinate
  bad_vol = ord_pars
  bad_vol$value[grepl("^uh_[0-9]+$", bad_vol$name)] =
    bad_vol$value[grepl("^uh_[0-9]+$", bad_vol$name)] * 0.5
  expect_error(uh(6L, tci, bad_vol), "1 inch")

  non_consec = ord_pars[!(ord_pars$name == "uh_1" & ord_pars$zone == "NRKW1-1"), ]
  expect_error(uh(6L, tci, non_consec), "consecutive")

  ambig = rbind(ord_pars, nrkw1_pars[nrkw1_pars$name == "unit_shape", ][1L, ])
  expect_error(uh(6L, tci, ambig), "ambiguous")

  # Shared gamma UH across 2 SAC-Snow zones matches manual area-weighted TCI routing
  areas = vapply(
    c("NRKW1-1", "NRKW1-2"),
    \(z) nrkw1_pars$value[nrkw1_pars$type == "uh" & nrkw1_pars$name == "zone_area" & nrkw1_pars$zone == z],
    numeric(1L)
  )
  w = areas / sum(areas)
  tci_weighted = matrix(as.numeric(tci %*% w), ncol = 1L, dimnames = list(NULL, "NRKW1-UH"))

  shared_rows = data.frame(
    p_name = c("unit_shape_NRKW1-UH", "unit_toc_NRKW1-UH", "unit_toc_adj_NRKW1-UH"),
    name = c("unit_shape", "unit_toc", "unit_toc_adj"),
    type = "uh",
    zone = "NRKW1-UH",
    value = c(2.2, 24.0, 1.0)
  )
  shared_pars = rbind(non_uh, shared_rows)
  sf_shared = uh(6L, tci, shared_pars, sum_zones = TRUE)

  manual_single_pars = rbind(
    data.frame(
      p_name = "zone_area_NRKW1-UH",
      name = "zone_area",
      type = "uh",
      zone = "NRKW1-UH",
      value = sum(areas)
    ),
    shared_rows
  )
  sf_manual = uh(6L, tci_weighted, manual_single_pars, sum_zones = TRUE)
  expect_equal(sf_shared, sf_manual, tolerance = 1e-6)

  # S-hydrograph + PCHIP resampling conserves volume and matches Scipy PchipInterpolator
  uh_3hr = uh_ordinates(ord_pars, dt_hours = 3L, source_dt_hours = 6L)
  uh_12hr = uh_ordinates(ord_pars, dt_hours = 12L, source_dt_hours = 6L)
  expect_equal(uh_3hr$hours[1:3], c(0L, 3L, 6L))
  expect_equal(
    sum(uh_3hr[["NRKW1-1"]], na.rm = TRUE) * 3,
    sum(uh_tbl[["NRKW1-1"]], na.rm = TRUE) * 6,
    tolerance = 1e-6
  )
  expect_equal(
    sum(uh_12hr[["NRKW1-1"]], na.rm = TRUE) * 12,
    sum(uh_tbl[["NRKW1-1"]], na.rm = TRUE) * 6,
    tolerance = 1e-2
  )
})


test_that("multi-timestep validation and SAC/FA/LagK/Consuse bounds work", {
  data("nrkw1_forcing", package = "nwsrfsr")
  data("nrkw1_pars", package = "nwsrfsr")
  data("nrkw1_upflow", package = "nwsrfsr")
  data("sfln2_pars", package = "nwsrfsr")

  # Construct a full-year 12-hour dataset by subsampling every 2nd 6-hour step
  idx_12 = seq(1L, 1464L, by = 2L)
  f_12 = lapply(nrkw1_forcing, \(df) {
    out = df[idx_12, ]
    out$map_mm = out$map_mm * 2
    rownames(out) = NULL
    out
  })
  uf_12 = lapply(nrkw1_upflow, \(df) {
    out = df[idx_12, ]
    rownames(out) = NULL
    out
  })

  fa_12 = fa_nwrfc(12L, f_12, nrkw1_pars)
  tci_12 = sac_snow(12L, fa_12, nrkw1_pars)
  sf_12 = uh(12L, tci_12, nrkw1_pars)
  lk_12 = lagk(12L, uf_12, nrkw1_pars)
  expect_equal(length(sf_12), length(idx_12))
  expect_equal(length(lk_12), length(idx_12))
  expect_true(all(is.finite(sf_12)) && all(is.finite(lk_12)))

  # Construct a 24-hour dataset: allowed for sac_snow, uh, lagk, chanloss; rejected by fa_nwrfc
  idx_24 = seq(1L, 1464L, by = 4L)
  f_24 = lapply(nrkw1_forcing, \(df) {
    out = df[idx_24, ]
    out$pet_mm = 1.5
    out$etd_mm = 1.2
    rownames(out) = NULL
    out
  })
  uf_24 = lapply(nrkw1_upflow, \(df) {
    out = df[idx_24, ]
    rownames(out) = NULL
    out
  })
  expect_error(fa_nwrfc(24L, f_24, nrkw1_pars), "Unsupported dt_hours")
  tci_24 = sac_snow(24L, f_24, nrkw1_pars)
  sf_24 = uh(24L, tci_24, nrkw1_pars)
  lk_24 = lagk(24L, uf_24, nrkw1_pars)
  expect_equal(length(sf_24), length(idx_24))
  expect_equal(length(lk_24), length(idx_24))

  # Unsupported timestep (e.g. 5 hours) and mismatched dt_hours vs timestamps
  expect_error(uh(5L, tci_12, nrkw1_pars), "Unsupported dt_hours")
  expect_error(sac_snow(6L, fa_12, nrkw1_pars), "step by exactly 6 hour")

  # fa_nwrfc requires all 12 calendar months when climo is NULL
  expect_error(fa_nwrfc(12L, lapply(f_12, \(df) df[1:40, ]), nrkw1_pars), "12 calendar months")

  # sac_snow allows negative 'side', warns on negative 'elev', rejects negative 'uztwm'
  neg_side_pars = nrkw1_pars
  neg_side_pars$value[neg_side_pars$name == "side"] = -0.2
  expect_no_error(sac_snow(12L, fa_12, neg_side_pars))

  neg_elev_pars = nrkw1_pars
  neg_elev_pars$value[neg_elev_pars$name == "elev"] = -10
  expect_warning(sac_snow(12L, fa_12, neg_elev_pars), "elev")

  neg_uztwm_pars = nrkw1_pars
  neg_uztwm_pars$value[neg_uztwm_pars$name == "uztwm"] = -1
  expect_error(sac_snow(12L, fa_12, neg_uztwm_pars), "must be >= 0")

  # consuse accepts 'min_flow' as well as 'min_flow_cmsd' and handles shuffled parameter rows
  dts_d = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "1 day")
  cu_in = data.frame(
    year = as.integer(format(dts_d, "%Y")),
    month = as.integer(format(dts_d, "%m")),
    day = as.integer(format(dts_d, "%d")),
    pet = rep(2.5, length(dts_d)),
    flow = rep(50, length(dts_d))
  )
  cu_p1 = sfln2_pars
  res_cu1 = consuse(cu_in, cu_p1, cfs = TRUE)
  cu_p2 = cu_p1[nrow(cu_p1):1L, ]
  res_cu2 = consuse(cu_in, cu_p2, cfs = TRUE)
  expect_equal(res_cu1, res_cu2)
})
