# %%
# Hydrological data alignment, validation metrics, and ggplot2 diagnostics

utils::globalVariables(".data")

.check_ggplot2 = function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plotting functions. ",
      "Install it with install.packages('ggplot2').",
      call. = FALSE
    )
  }
}

.okabe_ito = c(
  "#0072B2",
  "#D55E00",
  "#009E73",
  "#E69F00",
  "#CC79A7",
  "#56B4E9",
  "#F0E442"
)

.wy_months = c(10L:12L, 1L:9L)
.wy_labels = month.abb[.wy_months]
.wy_month_starts = c(1L, 32L, 62L, 93L, 124L, 152L, 183L, 213L, 244L, 274L, 305L, 336L)

.extract_pars = function(x, pars = NULL) {
  if (inherits(x, "nwsrfs_run")) {
    return(as.data.frame(x$pars))
  }
  if (!is.null(pars)) {
    return(as.data.frame(pars))
  }
  if (is.data.frame(x) && all(c("name", "zone", "value") %in% names(x))) {
    return(as.data.frame(x))
  }
  stop("Supply an 'nwsrfs_run' object or a parameter data.frame.", call. = FALSE)
}

.get_par_val = function(pars, zone, par_name) {
  vals = pars$value[pars$zone == zone & pars$name == par_name]
  if (length(vals) == 0L) NA_real_ else vals[1L]
}

.runoff_zones = function(pars, include_cu = FALSE) {
  if ("type" %in% names(pars) && any(pars$type %in% c("sac", "snow"))) {
    zones = sort(unique(pars$zone[!is.na(pars$zone) & grepl("-", pars$zone) & pars$type %in% c("sac", "snow")]))
  } else {
    zones = sort(unique(pars$zone[!is.na(pars$zone) & grepl("-", pars$zone)]))
  }
  if (!include_cu) {
    zones = zones[!grepl("-CU$", zones)]
  }
  zones
}

.zone_labels = function(pars, zones) {
  vapply(
    zones,
    \(z) {
      elev = .get_par_val(pars, z, "elev")
      if (is.na(elev)) z else sprintf("%s (%.0f m)", z, elev)
    },
    character(1L),
    USE.NAMES = FALSE
  )
}

.is_run_list = function(x) {
  is.list(x) && !inherits(x, "nwsrfs_run") && !is.data.frame(x) &&
    length(x) > 0L && all(vapply(x, \(el) inherits(el, "nwsrfs_run"), logical(1L)))
}

.as_named_runs = function(x, default_name = "Simulated") {
  if (inherits(x, "nwsrfs_run")) {
    out = list(x)
    names(out) = default_name
    return(out)
  }
  if (.is_run_list(x)) {
    if (is.null(names(x)) || any(!nzchar(names(x)))) {
      nms = if (is.null(names(x))) rep("", length(x)) else names(x)
      empty = !nzchar(nms)
      nms[empty] = paste0("Run ", which(empty))
      names(x) = nms
    }
    return(x)
  }
  stop("'x' must be an 'nwsrfs_run' object or a named list of 'nwsrfs_run' objects.", call. = FALSE)
}

.period_avg_daily = function(vec, ts_df, is_inst = TRUE) {
  n = length(vec)
  per_avg = if (is_inst) {
    c((vec[-n] + vec[-1L]) / 2, NA_real_)
  } else {
    vec
  }
  aggregate(
    list(flow_cfs = per_avg),
    by = list(year = ts_df$year, month = ts_df$month, day = ts_df$day),
    FUN = \(v) mean(v, na.rm = FALSE)
  )
}

.wy_day_index = function(date, year, month, day, wy) {
  wy_start = as.Date(sprintf("%04d-10-01", wy - 1L))
  leap_after_feb = ((wy %% 4L == 0L & wy %% 100L != 0L) | (wy %% 400L == 0L)) &
    month >= 3L & month <= 9L
  as.integer(date - wy_start) + 1L - as.integer(leap_after_feb)
}

.comma_labels = function(x) {
  format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
}

.extract_states = function(x, pars = NULL) {
  if (inherits(x, "nwsrfs_run")) {
    if (!isTRUE(x$localflow_logic)) {
      stop("Model run has no SAC-SMA / SNOW-17 zones.", call. = FALSE)
    }
    forcings = if (!is.null(x$forcings)) {
      x$forcings
    } else {
      f_in = x$forcing_raw[x$zone_names]
      fa_arg = x$.forcing_adj
      if (identical(x$ptps_logic, FALSE)) {
        f_in = rsnwelev(f_in, x$pars, x$area_elev)
        fa_types = setdiff(.fa_adjust_types(fa_arg), "ptps")
        fa_arg = if (length(fa_types) == 0L) FALSE else fa_types
      }
      fa_nwrfc(x$dt_hours, f_in, x$pars, adjust = fa_arg)
    }
    st = sac_snow_states(x$dt_hours, forcings, x$pars)
    return(list(states = st, pars = x$pars))
  }
  if (is.data.frame(x) && !is.null(pars)) {
    return(list(states = x, pars = as.data.frame(pars)))
  }
  stop(
    "Supply either an 'nwsrfs_run' object or a 'sac_snow_states()' data.frame with 'pars'.",
    call. = FALSE
  )
}


# %%
# Exported data alignment and validation metric utilities

#' Convert 6-hour simulated flow to period-averaged daily discharge
#'
#' Aligns beginning-of-timestep 6-hour instantaneous discharge from NWSRFS
#' simulations with daily mean gauge records by computing the trapezoidal
#' 6-hour period average `(sim[t] + sim[t + 1]) / 2` before averaging to
#' calendar days. Optionally merges observed daily flow and removes initial
#' spin-up water years.
#'
#' @param x An `"nwsrfs_run"` object from [nwsrfs_run()] or [load_example()],
#'   or a numeric vector of 6-hour simulated flow (in cfs) when `ts` is
#'   supplied.
#' @param obs Optional daily observed flow `data.frame` with columns `year`,
#'   `month`, `day`, and `flow_cfs` (plus optional `Source`). When `x` is an
#'   `"nwsrfs_run"` object and `obs = NULL`, `x$daily_flow` is used if present;
#'   pass `obs = FALSE` to omit observations.
#' @param ts Timestamp `data.frame` with columns `year`, `month`, and `day`
#'   matching `x` when `x` is a numeric vector. Ignored when `x` is an
#'   `"nwsrfs_run"` object.
#' @param spinup_years Integer number of initial water years (Oct 1 - Sep 30) to
#'   discard as model spin-up. Default `1L`; set `0L` to retain the full record.
#'
#' @return A daily `data.frame` ordered by `date` with columns `year`, `month`,
#'   `day`, `date`, `wy` (water year), `sim_cfs`, and when available `obs_cfs`,
#'   `Source`, `local_cfs` (`sacsnow_sf`), and `lagk_cfs` (`lagk_route`).
#'
#' @importFrom stats aggregate setNames
#' @export
#'
#' @examples
#' run = load_example("NRKW1")
#' daily = sim_to_daily(run, spinup_years = 1L)
#' head(daily)
sim_to_daily = function(x, obs = NULL, ts = NULL, spinup_years = 1L) {
  spinup_years = as.integer(spinup_years)
  if (is.na(spinup_years) || spinup_years < 0L) {
    stop("'spinup_years' must be a non-negative integer.", call. = FALSE)
  }

  if (inherits(x, "nwsrfs_run")) {
    ts_ref = if (!is.null(x$forcings)) {
      x$forcings[[1L]]
    } else if (!is.null(x$forcing_raw) && length(x$forcing_raw) > 0L) {
      x$forcing_raw[[1L]]
    } else {
      x$upflow[[1L]]
    }
    is_inst = !identical(x$.return_inst, FALSE)
    out = .period_avg_daily(x$sim, ts_ref, is_inst = is_inst)
    names(out)[names(out) == "flow_cfs"] = "sim_cfs"

    if (!is.null(x$sacsnow_sf)) {
      loc = .period_avg_daily(x$sacsnow_sf, ts_ref, is_inst = is_inst)
      names(loc)[names(loc) == "flow_cfs"] = "local_cfs"
      out = merge(out, loc, by = c("year", "month", "day"), all.x = TRUE)
    }
    if (!is.null(x$lagk_route)) {
      # lagk() returns instantaneous outflow regardless of UH return_inst
      lk = .period_avg_daily(x$lagk_route, ts_ref, is_inst = TRUE)
      names(lk)[names(lk) == "flow_cfs"] = "lagk_cfs"
      out = merge(out, lk, by = c("year", "month", "day"), all.x = TRUE)
    }
    if (is.null(obs)) {
      obs = x$daily_flow
    }
  } else if (is.numeric(x)) {
    if (!is.data.frame(ts) || !all(c("year", "month", "day") %in% names(ts))) {
      stop("When 'x' is a numeric vector, 'ts' must be a data.frame with year, month, day.", call. = FALSE)
    }
    if (length(x) != nrow(ts)) {
      stop("Length of 'x' must match nrow(ts).", call. = FALSE)
    }
    out = .period_avg_daily(x, ts, is_inst = TRUE)
    names(out)[names(out) == "flow_cfs"] = "sim_cfs"
  } else {
    stop("'x' must be an 'nwsrfs_run' object or a numeric vector.", call. = FALSE)
  }

  if (!identical(obs, FALSE) && !is.null(obs)) {
    obs_df = as.data.frame(obs)
    if (!all(c("year", "month", "day", "flow_cfs") %in% names(obs_df))) {
      stop("'obs' must contain columns year, month, day, and flow_cfs.", call. = FALSE)
    }
    keep_cols = intersect(c("year", "month", "day", "flow_cfs", "Source"), names(obs_df))
    obs_sub = obs_df[, keep_cols, drop = FALSE]
    names(obs_sub)[names(obs_sub) == "flow_cfs"] = "obs_cfs"
    out = merge(out, obs_sub, by = c("year", "month", "day"), all.x = TRUE)
  }

  out$date = as.Date(sprintf("%04d-%02d-%02d", out$year, out$month, out$day))
  out$wy = ifelse(out$month >= 10L, out$year + 1L, out$year)
  out = out[order(out$date), ]
  out = out[!is.na(out$sim_cfs), ]

  if (spinup_years > 0L && nrow(out) > 0L) {
    min_wy = min(out$wy, na.rm = TRUE)
    out = out[out$wy >= (min_wy + spinup_years), ]
  }

  col_order = intersect(
    c("year", "month", "day", "date", "wy", "sim_cfs", "obs_cfs", "Source", "local_cfs", "lagk_cfs"),
    names(out)
  )
  out = out[, col_order, drop = FALSE]
  rownames(out) = NULL
  out
}


.gof_vec = function(sim, obs) {
  valid = !is.na(sim) & !is.na(obs)
  sim = sim[valid]
  obs = obs[valid]
  n = length(obs)
  if (n < 2L) {
    return(data.frame(
      n = n,
      NSE = NA_real_,
      logNSE = NA_real_,
      KGE = NA_real_,
      r = NA_real_,
      alpha = NA_real_,
      beta = NA_real_,
      PBIAS = NA_real_,
      RMSE = NA_real_,
      MAE = NA_real_
    ))
  }

  obs_mean = mean(obs)
  sim_mean = mean(sim)
  obs_sd = sd(obs)
  sim_sd = sd(sim)
  denom = sum((obs - obs_mean)^2)
  nse = if (denom > 0) 1 - sum((sim - obs)^2) / denom else NA_real_

  ln_obs = log(pmax(obs, 1))
  ln_sim = log(pmax(sim, 1))
  ln_denom = sum((ln_obs - mean(ln_obs))^2)
  log_nse = if (ln_denom > 0) 1 - sum((ln_sim - ln_obs)^2) / ln_denom else NA_real_

  r = if (obs_sd > 0 && sim_sd > 0) cor(obs, sim) else NA_real_
  alpha = if (obs_sd > 0) sim_sd / obs_sd else NA_real_
  beta = if (obs_mean != 0) sim_mean / obs_mean else NA_real_
  kge = if (!anyNA(c(r, alpha, beta))) {
    1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
  } else {
    NA_real_
  }

  pbias = if (sum(obs) != 0) 100 * sum(sim - obs) / sum(obs) else NA_real_
  rmse = sqrt(mean((sim - obs)^2))
  mae = mean(abs(sim - obs))

  data.frame(
    n = n,
    NSE = nse,
    logNSE = log_nse,
    KGE = kge,
    r = r,
    alpha = alpha,
    beta = beta,
    PBIAS = pbias,
    RMSE = rmse,
    MAE = mae
  )
}


#' Compute hydrological goodness-of-fit metrics
#'
#' Calculates standard streamflow validation statistics (Nash-Sutcliffe
#' efficiency, log-transformed NSE, Kling-Gupta efficiency and its components,
#' percent volume bias, RMSE, and MAE) overall, by water-year month, by
#' observed flow quantile bin, or by water year.
#'
#' @param x An `"nwsrfs_run"` object, a named list of `"nwsrfs_run"` objects, a
#'   daily `data.frame` from [sim_to_daily()], or a numeric vector of simulated
#'   flows.
#' @param obs Optional observed flow `data.frame` (for `"nwsrfs_run"` inputs) or
#'   numeric vector of observed flows (when `x` is a numeric vector).
#' @param by Aggregation grouping: `"overall"` (default), `"month"` (water-year
#'   months Oct-Sep), `"quantile"` (observed flow regimes `q0-q50`, `q50-q80`,
#'   `q80-q90`, `q90-q95`, `q95-q100`), or `"water_year"`.
#' @param source Optional character vector filtering rows to matching values of
#'   the daily `Source` column (e.g., `"USGS_12210700"`).
#' @param spinup_years Integer number of initial spin-up water years to discard
#'   when `x` is an `"nwsrfs_run"` object or list of runs. Default `1L`.
#'
#' @return A `data.frame` containing `n`, `NSE`, `logNSE`, `KGE`, `r`, `alpha`,
#'   `beta`, `PBIAS`, `RMSE`, and `MAE`, along with any grouping columns.
#'
#' @importFrom stats cor quantile sd
#' @export
#'
#' @examples
#' run = load_example("NRKW1")
#' flow_metrics(run)
#' flow_metrics(run, source = "USGS_12210700")
#' flow_metrics(run, by = "quantile", source = "USGS_12210700")
flow_metrics = function(
  x,
  obs = NULL,
  by = c("overall", "month", "quantile", "water_year"),
  source = NULL,
  spinup_years = 1L
) {
  by = match.arg(by)

  if (.is_run_list(x)) {
    runs = .as_named_runs(x)
    res = lapply(names(runs), \(nm) {
      m = flow_metrics(
        runs[[nm]],
        obs = obs,
        by = by,
        source = source,
        spinup_years = spinup_years
      )
      cbind(data.frame(run = nm, stringsAsFactors = FALSE), m)
    })
    out = do.call(rbind, res)
    rownames(out) = NULL
    return(out)
  }

  if (inherits(x, "nwsrfs_run")) {
    df = sim_to_daily(x, obs = obs, spinup_years = spinup_years)
  } else if (is.data.frame(x)) {
    if (!all(c("sim_cfs", "obs_cfs") %in% names(x))) {
      stop("Data.frame 'x' must contain 'sim_cfs' and 'obs_cfs' columns.", call. = FALSE)
    }
    df = x
  } else if (is.numeric(x) && is.numeric(obs)) {
    if (length(x) != length(obs)) {
      stop("Numeric 'x' and 'obs' must have the same length.", call. = FALSE)
    }
    if (by %in% c("month", "water_year")) {
      stop("Grouping by '", by, "' requires date metadata (an 'nwsrfs_run' or 'sim_to_daily()' data.frame).", call. = FALSE)
    }
    df = data.frame(sim_cfs = x, obs_cfs = obs)
  } else {
    stop("Unsupported input to 'flow_metrics()'.", call. = FALSE)
  }

  if (!"obs_cfs" %in% names(df)) {
    stop("Observed flows are required to compute validation metrics.", call. = FALSE)
  }
  if (!is.null(source)) {
    if (!"Source" %in% names(df)) {
      stop("'Source' column not found in daily data.", call. = FALSE)
    }
    df = df[df$Source %in% source, , drop = FALSE]
  }
  df = df[!is.na(df$sim_cfs) & !is.na(df$obs_cfs), , drop = FALSE]

  if (by == "overall") {
    out = .gof_vec(df$sim_cfs, df$obs_cfs)
  } else if (by == "month") {
    res = lapply(.wy_months, \(m) {
      sub_df = df[df$month == m, , drop = FALSE]
      cbind(
        data.frame(
          month = m,
          month_label = factor(month.abb[m], levels = .wy_labels)
        ),
        .gof_vec(sub_df$sim_cfs, sub_df$obs_cfs)
      )
    })
    out = do.call(rbind, res)
  } else if (by == "quantile") {
    probs_lo = c(0, 0.50, 0.80, 0.90, 0.95)
    probs_hi = c(0.50, 0.80, 0.90, 0.95, 1.00)
    bin_names = c("q0-q50", "q50-q80", "q80-q90", "q90-q95", "q95-q100")
    qs_lo = quantile(df$obs_cfs, probs = probs_lo, na.rm = TRUE, names = FALSE)
    qs_hi = quantile(df$obs_cfs, probs = probs_hi, na.rm = TRUE, names = FALSE)
    res = lapply(seq_along(bin_names), \(i) {
      keep = if (i == 1L) {
        df$obs_cfs >= qs_lo[i] & df$obs_cfs <= qs_hi[i]
      } else {
        df$obs_cfs > qs_lo[i] & df$obs_cfs <= qs_hi[i]
      }
      cbind(
        data.frame(quantile_bin = factor(bin_names[i], levels = bin_names)),
        .gof_vec(df$sim_cfs[keep], df$obs_cfs[keep])
      )
    })
    out = do.call(rbind, res)
  } else {
    wys = sort(unique(df$wy))
    res = lapply(wys, \(w) {
      sub_df = df[df$wy == w, , drop = FALSE]
      cbind(data.frame(wy = w), .gof_vec(sub_df$sim_cfs, sub_df$obs_cfs))
    })
    out = do.call(rbind, res)
  }

  rownames(out) = NULL
  out
}


# %%
# ggplot2 theme and S3 plot method

#' Clean ggplot2 theme for NWSRFS hydrological graphics
#'
#' Minimal theme with an opaque white background (for legibility across light
#' and dark HTML themes), subtle major gridlines, left-aligned bold facet
#' labels, and a compact top legend.
#'
#' @param base_size Base font size in points. Default `10.5`.
#'
#' @return A `ggplot2` theme object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_uh(run) + theme_nwsrfs(base_size = 11)
#' }
theme_nwsrfs = function(base_size = 10.5) {
  .check_ggplot2()
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA),
      panel.background = ggplot2::element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#EAEAEA", linewidth = 0.35),
      panel.spacing.x = ggplot2::unit(1.3, "lines"),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = base_size),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1),
      plot.subtitle = ggplot2::element_text(color = "#4A4A4A", size = base_size - 0.5),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      legend.key.height = ggplot2::unit(0.75, "lines"),
      legend.key.width = ggplot2::unit(1.3, "lines"),
      legend.margin = ggplot2::margin(b = 2),
      plot.margin = ggplot2::margin(8, 12, 8, 8)
    )
}


#' Plot diagnostics for an nwsrfs_run model simulation
#'
#' S3 `plot()` method for `"nwsrfs_run"` objects dispatching to package
#' hydrological diagnostic plots.
#'
#' @param x An `"nwsrfs_run"` object from [nwsrfs_run()] or [load_example()].
#' @param type Diagnostic plot to produce: `"hydrograph"` (default),
#'   `"water_year"`, `"fdc"`, `"monthly_bias"`, `"cumulative"`, `"scatter"`,
#'   `"water_supply"`, `"states"`, `"runoff_regime"`, `"water_balance"`,
#'   `"forcing"`, `"uh"`, `"adc"`, or `"lagk"`.
#' @param ... Additional arguments passed to the selected `plot_*()` function.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot(run, water_year = 2011)
#'   plot(run, type = "fdc")
#' }
plot.nwsrfs_run = function(
  x,
  type = c(
    "hydrograph",
    "water_year",
    "fdc",
    "monthly_bias",
    "cumulative",
    "scatter",
    "water_supply",
    "states",
    "runoff_regime",
    "water_balance",
    "forcing",
    "uh",
    "adc",
    "lagk"
  ),
  ...
) {
  type = match.arg(type)
  switch(
    type,
    hydrograph = plot_hydrograph(x, ...),
    water_year = plot_water_year(x, ...),
    fdc = plot_fdc(x, ...),
    monthly_bias = plot_monthly_bias(x, ...),
    cumulative = plot_cumulative(x, ...),
    scatter = plot_scatter(x, ...),
    water_supply = plot_water_supply(x, ...),
    states = plot_states(x, ...),
    runoff_regime = plot_runoff_regime(x, ...),
    water_balance = plot_water_balance(x, ...),
    forcing = plot_forcing_adjust(x, ...),
    uh = plot_uh(x, ...),
    adc = plot_adc(x, ...),
    lagk = plot_lagk(x, ...)
  )
}


# %%
# Basin parameter, routing, and forcing adjustment plots

#' Plot unit hydrographs (gamma or ordinate, per-zone or shared)
#'
#' Computes (or resamples) and plots unit hydrograph ordinates in `cfs / in`
#' via [uh_ordinates()] for each UH zone (or shared basin UH).
#'
#' @param x An `"nwsrfs_run"` object or parameter `data.frame` (`pars`).
#' @param dt_hours Unit hydrograph timestep in hours (`1`, `2`, `3`, `4`, `6`,
#'   `8`, `12`, or `24`). Defaults to `x$dt_hours` for `"nwsrfs_run"` objects,
#'   or `6L` for parameter `data.frame`s.
#' @param include_cu Logical; include `-CU` consumptive-use zones when present.
#'   Default `FALSE`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_uh(nrkw1_pars)
#' }
plot_uh = function(x, dt_hours = NULL, include_cu = FALSE) {
  .check_ggplot2()
  pars = .extract_pars(x)
  uh_wide = if (inherits(x, "nwsrfs_run")) {
    uh_ordinates(x, dt_hours = dt_hours)
  } else {
    uh_ordinates(pars, dt_hours = if (is.null(dt_hours)) 6L else dt_hours)
  }
  if (is.null(uh_wide) || ncol(uh_wide) < 2L) {
    stop("No runoff zones found in 'pars'.", call. = FALSE)
  }

  is_shared = if (inherits(x, "nwsrfs_run")) {
    isTRUE(x$shared_uh_logic)
  } else {
    isTRUE(.detect_uh_spec(pars)$shared_uh_logic)
  }

  uh_zones = names(uh_wide)[-1L]
  if (!include_cu && !is_shared) {
    uh_zones = uh_zones[!grepl("-CU$", uh_zones)]
  }
  if (length(uh_zones) == 0L) {
    stop("No runoff zones found in 'pars'.", call. = FALSE)
  }
  z_labs = .zone_labels(pars, uh_zones)

  uh_df = do.call(
    rbind,
    lapply(seq_along(uh_zones), \(i) {
      ord = uh_wide[[uh_zones[i]]]
      keep = !is.na(ord)
      data.frame(
        zone = factor(z_labs[i], levels = z_labs),
        hour = uh_wide$hours[keep],
        cfs_per_in = ord[keep]
      )
    })
  )

  cols = setNames(rep_len(.okabe_ito, length(z_labs)), z_labs)

  ggplot2::ggplot(
    uh_df,
    ggplot2::aes(
      x = .data[["hour"]],
      y = .data[["cfs_per_in"]],
      color = .data[["zone"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = .comma_labels) +
    ggplot2::labs(
      x = "Time since excess runoff (hours)",
      y = "Unit response (cfs / in)"
    ) +
    theme_nwsrfs()
}


#' Plot SNOW-17 areal snow-cover depletion curves by runoff zone
#'
#' Evaluates `adc3(adc_a, adc_b, adc_c)` for each runoff zone to show how
#' normalized snowpack (`SWE / AI`) varies with areal snow-cover fraction
#' (`AESC`).
#'
#' @param x An `"nwsrfs_run"` object or parameter `data.frame` (`pars`).
#' @param include_cu Logical; include `-CU` consumptive-use zones when present.
#'   Default `FALSE`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_adc(nrkw1_pars)
#' }
plot_adc = function(x, include_cu = FALSE) {
  .check_ggplot2()
  pars = .extract_pars(x)
  zones = .runoff_zones(pars, include_cu = include_cu)
  if (length(zones) == 0L) {
    stop("No SNOW-17 zones found in 'pars'.", call. = FALSE)
  }
  z_labs = .zone_labels(pars, zones)
  aesc_grid = c(0, seq(0.05, 1.0, by = 0.025))

  adc_df = do.call(
    rbind,
    lapply(seq_along(zones), \(i) {
      z = zones[i]
      a = .get_par_val(pars, z, "adc_a")
      b = .get_par_val(pars, z, "adc_b")
      c_par = .get_par_val(pars, z, "adc_c")
      we_ai = c(0, a * aesc_grid[-1L]^b + (1 - a) * aesc_grid[-1L]^c_par)
      data.frame(
        zone = factor(z_labs[i], levels = z_labs),
        aesc = aesc_grid,
        we_ai = we_ai
      )
    })
  )

  cols = setNames(rep_len(.okabe_ito, length(z_labs)), z_labs)

  ggplot2::ggplot(
    adc_df,
    ggplot2::aes(
      x = .data[["aesc"]],
      y = .data[["we_ai"]],
      color = .data[["zone"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
    ggplot2::labs(
      x = "Areal snow-cover fraction (AESC)",
      y = "Normalized snowpack (SWE / AI)"
    ) +
    theme_nwsrfs()
}


#' Plot variable Lag and K attenuation routing curves for upstream reaches
#'
#' Reconstructs the 11-point discharge-dependent Lag (hours) and K attenuation
#' (hours) routing lookup tables parameterized by `lagtbl_*` and `ktbl_*` for
#' each upstream tributary reach.
#'
#' @param x An `"nwsrfs_run"` object or parameter `data.frame` (`pars`)
#'   containing `type == "lagk"` rows.
#'
#' @return A two-panel `ggplot` object comparing Lag time and K attenuation vs
#'   inflow discharge across reaches.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_lagk(nrkw1_pars)
#' }
plot_lagk = function(x) {
  .check_ggplot2()
  pars = .extract_pars(x)
  reaches = sort(unique(pars$zone[pars$type == "lagk"]))
  if (length(reaches) == 0L) {
    stop("No Lag-K routing reaches ('type == \"lagk\"') found in 'pars'.", call. = FALSE)
  }

  ndq = seq(0, 1, by = 0.1)
  tbl_df = do.call(
    rbind,
    lapply(reaches, \(u) {
      la = .get_par_val(pars, u, "lagtbl_a")
      lb = .get_par_val(pars, u, "lagtbl_b")
      lc = .get_par_val(pars, u, "lagtbl_c")
      ld = .get_par_val(pars, u, "lagtbl_d")
      ka = .get_par_val(pars, u, "ktbl_a")
      kb = .get_par_val(pars, u, "ktbl_b")
      kc = .get_par_val(pars, u, "ktbl_c")
      kd = .get_par_val(pars, u, "ktbl_d")
      qmin = .get_par_val(pars, u, "lagk_qmin")
      qmax = .get_par_val(pars, u, "lagk_qmax")
      lagmin = .get_par_val(pars, u, "lagk_lagmin")
      lagmax = .get_par_val(pars, u, "lagk_lagmax")
      kmin = .get_par_val(pars, u, "lagk_kmin")
      kmax = .get_par_val(pars, u, "lagk_kmax")

      q_vec = ndq * (qmax - qmin) + qmin
      lag_e = pmin(pmax(la * (ndq - ld)^2 + lb * ndq + lc, 0), 1)
      k_e = pmin(pmax(ka * (ndq - kd)^2 + kb * ndq + kc, 0), 1)
      lag_hr = lag_e * (lagmax - lagmin) + lagmin
      k_hr = k_e * (kmax - kmin) + kmin

      rbind(
        data.frame(
          reach = factor(u, levels = reaches),
          panel = "A. Variable Lag Time (hours)",
          flow_cfs = q_vec,
          hours = lag_hr
        ),
        data.frame(
          reach = factor(u, levels = reaches),
          panel = "B. Variable K Attenuation (hours)",
          flow_cfs = q_vec,
          hours = k_hr
        )
      )
    })
  )
  tbl_df$panel = factor(tbl_df$panel, levels = unique(tbl_df$panel))
  cols = setNames(rep_len(.okabe_ito, length(reaches)), reaches)

  ggplot2::ggplot(
    tbl_df,
    ggplot2::aes(
      x = .data[["flow_cfs"]],
      y = .data[["hours"]],
      color = .data[["reach"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::geom_point(size = 1.5) +
    ggplot2::facet_wrap(~panel, nrow = 1L, scales = "free_y") +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_x_continuous(labels = .comma_labels) +
    ggplot2::labs(
      x = "Tributary inflow discharge (cfs)",
      y = "Routing time (hours)"
    ) +
    theme_nwsrfs()
}


#' Plot raw vs FA-adjusted water-year forcing climatologies
#'
#' Compares monthly water-year (`Oct`-`Sep`) climatologies of precipitation
#' (`mm/day`), air temperature (`deg C`), potential evapotranspiration (`PET`,
#' `mm/day`), and canopy evapotranspiration demand (`ETD`, `mm/day`) before and
#' after [fa_nwrfc()].
#'
#' @param x An `"nwsrfs_run"` object, or a named list of raw forcing
#'   `data.frame`s when `pars` is supplied.
#' @param pars Parameter `data.frame` (required only when `x` is a forcing
#'   list).
#' @param zone Optional character name of a specific zone to display (e.g.,
#'   `"NRKW1-2"`). When `NULL` (default), plots the area-weighted basin mean
#'   across local runoff zones.
#' @param dt_hours Forcing timestep in hours. Default `6L`.
#' @param include_cu Logical; include `-CU` zones when computing basin mean.
#'   Default `FALSE`.
#'
#' @return A three-panel `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_forcing_adjust(run, zone = "NRKW1-2")
#' }
plot_forcing_adjust = function(
  x,
  pars = NULL,
  zone = NULL,
  dt_hours = 6L,
  include_cu = FALSE
) {
  .check_ggplot2()
  if (inherits(x, "nwsrfs_run")) {
    pars = x$pars
    dt_hours = x$dt_hours
    all_zones = x$zone_names[x$zone_names %in% names(x$forcing_raw)]
    f_raw_in = x$forcing_raw[all_zones]
    if (identical(x$ptps_logic, FALSE)) {
      f_raw_in = rsnwelev(f_raw_in, pars, x$area_elev)
    }
    f_unadj = fa_nwrfc(dt_hours, f_raw_in, pars, adjust = FALSE)
    f_adj = if (isTRUE(x$.forcing_adj) && !is.null(x$forcings)) {
      x$forcings
    } else {
      fa_arg = if (identical(x$ptps_logic, FALSE)) c("map", "mat", "pet") else TRUE
      fa_nwrfc(dt_hours, f_raw_in, pars, adjust = fa_arg)
    }
  } else if (is.list(x) && !is.null(pars)) {
    pars = as.data.frame(pars)
    f_unadj = fa_nwrfc(dt_hours, x, pars, adjust = FALSE)
    f_adj = fa_nwrfc(dt_hours, x, pars, adjust = TRUE)
  } else {
    stop("Supply an 'nwsrfs_run' object or a forcing list with 'pars'.", call. = FALSE)
  }

  steps_per_day = 24 / dt_hours

  if (!is.null(zone)) {
    if (!zone %in% names(f_unadj)) {
      stop("Zone '", zone, "' not found in forcing list.", call. = FALSE)
    }
    target_zones = zone
    weights = 1
  } else {
    target_zones = intersect(.runoff_zones(pars, include_cu = include_cu), names(f_unadj))
    if (length(target_zones) == 0L) {
      target_zones = names(f_unadj)
    }
    areas = vapply(target_zones, \(z) .get_par_val(pars, z, "zone_area"), numeric(1L))
    if (anyNA(areas) || sum(areas) <= 0) {
      weights = rep(1 / length(target_zones), length(target_zones))
    } else {
      weights = areas / sum(areas)
    }
  }

  weighted_month = function(flist) {
    per_z = lapply(seq_along(target_zones), \(i) {
      df = flist[[target_zones[i]]]
      agg = aggregate(
        df[, c("map_mm", "mat_degc", "pet_mm", "etd_mm")],
        by = list(month = df$month),
        FUN = \(v) mean(v, na.rm = TRUE)
      )
      agg[, -1L] = agg[, -1L] * weights[i]
      agg
    })
    out = per_z[[1L]]
    if (length(per_z) > 1L) {
      for (i in 2L:length(per_z)) {
        out[, -1L] = out[, -1L] + per_z[[i]][, -1L]
      }
    }
    out$wy_pos = match(out$month, .wy_months)
    out[order(out$wy_pos), ]
  }

  m_raw = weighted_month(f_unadj)
  m_cal = weighted_month(f_adj)

  panels = c(
    "A. Mean Precipitation (mm/day)",
    "B. Mean Air Temperature (deg C)",
    "C. PET and Canopy ETD (mm/day)"
  )
  series_levels = c("Raw forcing", "FA-adjusted forcing", "Canopy demand (ETD)")

  climo_long = rbind(
    data.frame(
      wy_pos = m_raw$wy_pos,
      panel = panels[1L],
      series = series_levels[1L],
      value = m_raw$map_mm * steps_per_day
    ),
    data.frame(
      wy_pos = m_cal$wy_pos,
      panel = panels[1L],
      series = series_levels[2L],
      value = m_cal$map_mm * steps_per_day
    ),
    data.frame(
      wy_pos = m_raw$wy_pos,
      panel = panels[2L],
      series = series_levels[1L],
      value = m_raw$mat_degc
    ),
    data.frame(
      wy_pos = m_cal$wy_pos,
      panel = panels[2L],
      series = series_levels[2L],
      value = m_cal$mat_degc
    ),
    data.frame(
      wy_pos = m_raw$wy_pos,
      panel = panels[3L],
      series = series_levels[1L],
      value = m_raw$pet_mm * steps_per_day
    ),
    data.frame(
      wy_pos = m_cal$wy_pos,
      panel = panels[3L],
      series = series_levels[2L],
      value = m_cal$pet_mm * steps_per_day
    ),
    data.frame(
      wy_pos = m_cal$wy_pos,
      panel = panels[3L],
      series = series_levels[3L],
      value = m_cal$etd_mm * steps_per_day
    )
  )
  climo_long$panel = factor(climo_long$panel, levels = panels)
  climo_long$series = factor(climo_long$series, levels = series_levels)

  zero_line = data.frame(
    panel = factor(panels[2L], levels = panels),
    yint = 0
  )

  ggplot2::ggplot(
    climo_long,
    ggplot2::aes(
      x = .data[["wy_pos"]],
      y = .data[["value"]],
      color = .data[["series"]],
      linetype = .data[["series"]]
    )
  ) +
    ggplot2::geom_hline(
      data = zero_line,
      ggplot2::aes(yintercept = .data[["yint"]]),
      color = "#888888",
      linetype = "dotted"
    ) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::facet_wrap(~panel, ncol = 1L, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = .wy_labels) +
    ggplot2::scale_color_manual(
      values = c(
        "Raw forcing" = "#7F7F7F",
        "FA-adjusted forcing" = "#0072B2",
        "Canopy demand (ETD)" = "#009E73"
      )
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Raw forcing" = "dashed",
        "FA-adjusted forcing" = "solid",
        "Canopy demand (ETD)" = "solid"
      )
    ) +
    ggplot2::labs(x = "Water-year month", y = NULL) +
    theme_nwsrfs()
}


# %%
# Internal SNOW-17 / SAC-SMA state, runoff pathway, and water-balance plots

#' Plot internal SNOW-17 snowpack, SAC-SMA soil saturation, and runoff pathways
#'
#' Produces a three-panel daily diagnostic of (A) snow water equivalent (`SWE`,
#' mm) by elevation zone, (B) basin-average upper- and lower-zone tension water
#' saturation (`%` of capacity), and (C) stacked SAC-SMA runoff pathways
#' (`mm/day`).
#'
#' @param x An `"nwsrfs_run"` object or a `data.frame` returned by
#'   [sac_snow_states()].
#' @param pars Parameter `data.frame` (required only when `x` is a
#'   [sac_snow_states()] `data.frame`).
#' @param water_year Optional integer water year(s) to plot (e.g., `2011`). If
#'   `water_year`, `start`, and `end` are all `NULL`, defaults to the final
#'   complete post-spinup water year.
#' @param start,end Optional start and end dates (`Date` or `"YYYY-MM-DD"`)
#'   defining a custom plotting window.
#' @param include_cu Logical; include `-CU` zones when present. Default `FALSE`.
#' @param spinup_years Integer spin-up water years to discard when selecting a
#'   default water year. Default `1L`.
#'
#' @return A three-panel `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_states(run, water_year = 2011)
#' }
plot_states = function(
  x,
  pars = NULL,
  water_year = NULL,
  start = NULL,
  end = NULL,
  include_cu = FALSE,
  spinup_years = 1L
) {
  .check_ggplot2()
  sp = .extract_states(x, pars)
  st = sp$states
  pars = sp$pars

  all_zones = .runoff_zones(pars, include_cu = TRUE)
  keep_idx = if (include_cu) seq_along(all_zones) else which(!grepl("-CU$", all_zones))
  zones = all_zones[keep_idx]
  z_labs = .zone_labels(pars, zones)

  areas = vapply(zones, \(z) .get_par_val(pars, z, "zone_area"), numeric(1L))
  w_zone = if (anyNA(areas) || sum(areas) <= 0) {
    rep(1 / length(zones), length(zones))
  } else {
    areas / sum(areas)
  }

  st$date = as.Date(sprintf("%04d-%02d-%02d", st$year, st$month, st$day))
  st$wy = ifelse(st$month >= 10L, st$year + 1L, st$year)
  if (spinup_years > 0L) {
    st = st[st$wy >= (min(st$wy, na.rm = TRUE) + spinup_years), , drop = FALSE]
  }

  steps_per_day = if (inherits(x, "nwsrfs_run")) {
    24L / x$dt_hours
  } else {
    max(1L, as.integer(round(nrow(st) / max(1L, length(unique(st$date))))))
  }

  if (!is.null(water_year)) {
    st = st[st$wy %in% as.integer(water_year), , drop = FALSE]
  } else if (!is.null(start) || !is.null(end)) {
    if (!is.null(start)) st = st[st$date >= as.Date(start), , drop = FALSE]
    if (!is.null(end)) st = st[st$date <= as.Date(end), , drop = FALSE]
  } else {
    wy_counts = table(st$wy)
    full_wys = as.integer(names(wy_counts)[wy_counts >= 360L * steps_per_day])
    target_wy = if (length(full_wys) > 0L) max(full_wys) else max(st$wy)
    st = st[st$wy == target_wy, , drop = FALSE]
  }
  if (nrow(st) == 0L) {
    stop("No state timesteps remain in the requested date/water-year window.", call. = FALSE)
  }

  panels = c(
    "A. Zone Snow Water Equivalent (mm)",
    "B. SAC-SMA Tension Water Saturation (%)",
    "C. SAC-SMA Runoff Pathways (mm/day)"
  )

  # Panel A: SWE per zone (daily mean)
  swe_list = lapply(seq_along(keep_idx), \(k) {
    col_nm = paste0("swe_", keep_idx[k])
    d_swe = aggregate(list(val = st[[col_nm]]), by = list(date = st$date), FUN = \(v) mean(v, na.rm = TRUE))
    data.frame(
      date = d_swe$date,
      panel = panels[1L],
      series = z_labs[k],
      value = d_swe$val
    )
  })
  swe_df = do.call(rbind, swe_list)

  # Panel B: Area-weighted UZTWC and LZTWC % of capacity
  uz_num = Reduce("+", lapply(seq_along(keep_idx), \(k) st[[paste0("uztwc_", keep_idx[k])]] * w_zone[k]))
  lz_num = Reduce("+", lapply(seq_along(keep_idx), \(k) st[[paste0("lztwc_", keep_idx[k])]] * w_zone[k]))
  uz_cap = sum(vapply(zones, \(z) .get_par_val(pars, z, "uztwm"), numeric(1L)) * w_zone)
  lz_cap = sum(vapply(zones, \(z) .get_par_val(pars, z, "lztwm"), numeric(1L)) * w_zone)

  d_soil = aggregate(
    list(uz = 100 * uz_num / uz_cap, lz = 100 * lz_num / lz_cap),
    by = list(date = st$date),
    FUN = \(v) mean(v, na.rm = TRUE)
  )
  soil_labs = c("Upper-zone tension (UZTWC)", "Lower-zone tension (LZTWC)")
  soil_df = rbind(
    data.frame(date = d_soil$date, panel = panels[2L], series = soil_labs[1L], value = d_soil$uz),
    data.frame(date = d_soil$date, panel = panels[2L], series = soil_labs[2L], value = d_soil$lz)
  )

  line_levels = c(rev(z_labs), soil_labs)
  line_df = rbind(swe_df, soil_df)
  line_df$panel = factor(line_df$panel, levels = panels)
  line_df$series = factor(line_df$series, levels = line_levels)

  z_palette = setNames(rep_len(c("#D55E00", "#0072B2", "#E69F00", "#56B4E9"), length(z_labs)), z_labs)
  line_cols = c(
    z_palette,
    "Upper-zone tension (UZTWC)" = "#009E73",
    "Lower-zone tension (LZTWC)" = "#CC79A7"
  )

  # Panel C: Stacked runoff pathways (daily sum in mm/day)
  fast_6 = Reduce(
    "+",
    lapply(seq_along(keep_idx), \(k) {
      idx = keep_idx[k]
      (st[[paste0("roimp_", idx)]] + st[[paste0("sdro_", idx)]] + st[[paste0("ssur_", idx)]]) * w_zone[k]
    })
  )
  sif_6 = Reduce("+", lapply(seq_along(keep_idx), \(k) st[[paste0("sif_", keep_idx[k])]] * w_zone[k]))
  bfs_6 = Reduce("+", lapply(seq_along(keep_idx), \(k) st[[paste0("bfs_", keep_idx[k])]] * w_zone[k]))
  bfp_6 = Reduce("+", lapply(seq_along(keep_idx), \(k) st[[paste0("bfp_", keep_idx[k])]] * w_zone[k]))

  d_flux = aggregate(
    list(fast = fast_6, sif = sif_6, bfs = bfs_6, bfp = bfp_6),
    by = list(date = st$date),
    FUN = \(v) sum(v, na.rm = TRUE)
  )
  path_levels = c(
    "Surface + direct + impervious",
    "Interflow (SIF)",
    "Supplemental baseflow (BFS)",
    "Primary baseflow (BFP)"
  )
  flux_df = rbind(
    data.frame(date = d_flux$date, panel = panels[3L], pathway = path_levels[1L], value = d_flux$fast),
    data.frame(date = d_flux$date, panel = panels[3L], pathway = path_levels[2L], value = d_flux$sif),
    data.frame(date = d_flux$date, panel = panels[3L], pathway = path_levels[3L], value = d_flux$bfs),
    data.frame(date = d_flux$date, panel = panels[3L], pathway = path_levels[4L], value = d_flux$bfp)
  )
  flux_df$panel = factor(flux_df$panel, levels = panels)
  flux_df$pathway = factor(flux_df$pathway, levels = path_levels)

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = line_df,
      ggplot2::aes(
        x = .data[["date"]],
        y = .data[["value"]],
        color = .data[["series"]]
      ),
      linewidth = 0.85
    ) +
    ggplot2::geom_area(
      data = flux_df,
      ggplot2::aes(
        x = .data[["date"]],
        y = .data[["value"]],
        fill = .data[["pathway"]]
      ),
      alpha = 0.9
    ) +
    ggplot2::facet_wrap(~panel, ncol = 1L, scales = "free_y") +
    ggplot2::scale_color_manual(values = line_cols) +
    ggplot2::scale_fill_manual(
      values = c(
        "Surface + direct + impervious" = "#D55E00",
        "Interflow (SIF)" = "#E69F00",
        "Supplemental baseflow (BFS)" = "#56B4E9",
        "Primary baseflow (BFP)" = "#0072B2"
      )
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(nrow = 1L, order = 1L),
      fill = ggplot2::guide_legend(nrow = 1L, order = 2L)
    ) +
    ggplot2::labs(x = "Date", y = NULL) +
    theme_nwsrfs() +
    ggplot2::theme(
      legend.box = "vertical",
      legend.spacing.y = ggplot2::unit(1, "pt"),
      legend.key.width = ggplot2::unit(0.9, "lines")
    )
}


#' Plot water-year cyclical SAC-SMA runoff pathway contributions
#'
#' Computes the mean seasonal cycle (`Oct`-`Sep`) of SAC-SMA runoff generation
#' pathways (`Surface + direct + impervious`, `Interflow`, `Supplemental
#' baseflow`, and `Primary baseflow`), displayed either in `mm/day` or as
#' percentage shares (`0`-`100%`) of daily channel inflow.
#'
#' @param x An `"nwsrfs_run"` object or a `data.frame` from [sac_snow_states()].
#' @param pars Parameter `data.frame` (required only when `x` is a
#'   [sac_snow_states()] `data.frame`).
#' @param normalize Logical; when `TRUE`, normalizes daily pathway fluxes to
#'   percent (`0`-`100%`) of total channel inflow. Default `FALSE` (`mm/day`).
#' @param by_zone Logical; facet by elevation runoff zone instead of computing
#'   the area-weighted basin total. Default `FALSE`.
#' @param include_cu Logical; include `-CU` zones when present. Default `FALSE`.
#' @param spinup_years Integer spin-up water years to discard. Default `1L`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_runoff_regime(run, normalize = TRUE)
#' }
plot_runoff_regime = function(
  x,
  pars = NULL,
  normalize = FALSE,
  by_zone = FALSE,
  include_cu = FALSE,
  spinup_years = 1L
) {
  .check_ggplot2()
  sp = .extract_states(x, pars)
  st = sp$states
  pars = sp$pars

  all_zones = .runoff_zones(pars, include_cu = TRUE)
  keep_idx = if (include_cu) seq_along(all_zones) else which(!grepl("-CU$", all_zones))
  zones = all_zones[keep_idx]
  z_labs = .zone_labels(pars, zones)

  st$date = as.Date(sprintf("%04d-%02d-%02d", st$year, st$month, st$day))
  st$wy = ifelse(st$month >= 10L, st$year + 1L, st$year)
  if (spinup_years > 0L) {
    st = st[st$wy >= (min(st$wy, na.rm = TRUE) + spinup_years), , drop = FALSE]
  }
  st = st[!(st$month == 2L & st$day == 29L), , drop = FALSE]
  st$wy_day = .wy_day_index(st$date, st$year, st$month, st$day, st$wy)

  steps_per_day = nrow(st) / length(unique(st$date))
  path_levels = c(
    "Surface + direct + impervious",
    "Interflow (SIF)",
    "Supplemental baseflow (BFS)",
    "Primary baseflow (BFP)"
  )

  build_zone_cyc = function(fast_v, sif_v, bfs_v, bfp_v, label) {
    m_fast = as.numeric(tapply(fast_v, st$wy_day, \(v) mean(v, na.rm = TRUE))) * steps_per_day
    m_sif = as.numeric(tapply(sif_v, st$wy_day, \(v) mean(v, na.rm = TRUE))) * steps_per_day
    m_bfs = as.numeric(tapply(bfs_v, st$wy_day, \(v) mean(v, na.rm = TRUE))) * steps_per_day
    m_bfp = as.numeric(tapply(bfp_v, st$wy_day, \(v) mean(v, na.rm = TRUE))) * steps_per_day
    wy_days = sort(unique(st$wy_day))

    if (normalize) {
      tot = pmax(m_fast + m_sif + m_bfs + m_bfp, 1e-12)
      m_fast = 100 * m_fast / tot
      m_sif = 100 * m_sif / tot
      m_bfs = 100 * m_bfs / tot
      m_bfp = 100 * m_bfp / tot
    }

    rbind(
      data.frame(wy_day = wy_days, group = label, pathway = path_levels[1L], value = m_fast),
      data.frame(wy_day = wy_days, group = label, pathway = path_levels[2L], value = m_sif),
      data.frame(wy_day = wy_days, group = label, pathway = path_levels[3L], value = m_bfs),
      data.frame(wy_day = wy_days, group = label, pathway = path_levels[4L], value = m_bfp)
    )
  }

  if (by_zone) {
    cyc_df = do.call(
      rbind,
      lapply(seq_along(keep_idx), \(k) {
        idx = keep_idx[k]
        build_zone_cyc(
          st[[paste0("roimp_", idx)]] + st[[paste0("sdro_", idx)]] + st[[paste0("ssur_", idx)]],
          st[[paste0("sif_", idx)]],
          st[[paste0("bfs_", idx)]],
          st[[paste0("bfp_", idx)]],
          z_labs[k]
        )
      })
    )
    cyc_df$group = factor(cyc_df$group, levels = z_labs)
  } else {
    areas = vapply(zones, \(z) .get_par_val(pars, z, "zone_area"), numeric(1L))
    w_zone = if (anyNA(areas) || sum(areas) <= 0) {
      rep(1 / length(zones), length(zones))
    } else {
      areas / sum(areas)
    }
    fast_w = Reduce(
      "+",
      lapply(seq_along(keep_idx), \(k) {
        idx = keep_idx[k]
        (st[[paste0("roimp_", idx)]] + st[[paste0("sdro_", idx)]] + st[[paste0("ssur_", idx)]]) * w_zone[k]
      })
    )
    sif_w = Reduce("+", lapply(seq_along(keep_idx), \(k) st[[paste0("sif_", keep_idx[k])]] * w_zone[k]))
    bfs_w = Reduce("+", lapply(seq_along(keep_idx), \(k) st[[paste0("bfs_", keep_idx[k])]] * w_zone[k]))
    bfp_w = Reduce("+", lapply(seq_along(keep_idx), \(k) st[[paste0("bfp_", keep_idx[k])]] * w_zone[k]))
    cyc_df = build_zone_cyc(fast_w, sif_w, bfs_w, bfp_w, "Basin area-weighted")
  }
  cyc_df$pathway = factor(cyc_df$pathway, levels = path_levels)

  p = ggplot2::ggplot(
    cyc_df,
    ggplot2::aes(
      x = .data[["wy_day"]],
      y = .data[["value"]],
      fill = .data[["pathway"]]
    )
  ) +
    ggplot2::geom_area(alpha = 0.92) +
    ggplot2::scale_x_continuous(breaks = .wy_month_starts, labels = .wy_labels, expand = c(0.01, 0.01)) +
    ggplot2::scale_fill_manual(
      values = c(
        "Surface + direct + impervious" = "#D55E00",
        "Interflow (SIF)" = "#E69F00",
        "Supplemental baseflow (BFS)" = "#56B4E9",
        "Primary baseflow (BFP)" = "#0072B2"
      )
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2L)) +
    ggplot2::labs(
      x = "Water-year month",
      y = if (normalize) "Share of channel inflow (%)" else "Mean channel inflow (mm/day)"
    ) +
    theme_nwsrfs() +
    ggplot2::theme(legend.key.width = ggplot2::unit(0.9, "lines"))

  if (by_zone) {
    p = p + ggplot2::facet_wrap(~group, ncol = 1L, scales = if (normalize) "fixed" else "free_y")
  }
  p
}


#' Plot zone and basin water balance (Budyko diagram or annual mass-balance bars)
#'
#' Summarizes post-spinup SNOW-17 and SAC-SMA water balance using effective
#' precipitation (`psfall + prain`, which includes SNOW-17 snowfall catch
#' correction `scf` and `pxadj`). Supports a Budyko aridity-evaporation diagram
#' (`type = "budyko"`) or an annual mass-balance bar comparison (`type = "bars"`).
#'
#' @param x An `"nwsrfs_run"` object or a `data.frame` from [sac_snow_states()].
#' @param pars Parameter `data.frame` (required only when `x` is a
#'   [sac_snow_states()] `data.frame`).
#' @param type `"budyko"` (default) for `PET / P` vs `AET / P` against energy
#'   and water limits and the Budyko (1974) curve, or `"bars"` for mean annual
#'   `Effective P` vs stacked `Channel inflow (TCI)` + `Actual ET (AET)` in
#'   `mm/yr`.
#' @param include_cu Logical; include `-CU` zones when present. Default `FALSE`.
#' @param spinup_years Integer spin-up water years to discard. Default `1L`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_water_balance(run, type = "budyko")
#'   plot_water_balance(run, type = "bars")
#' }
plot_water_balance = function(
  x,
  pars = NULL,
  type = c("budyko", "bars"),
  include_cu = FALSE,
  spinup_years = 1L
) {
  .check_ggplot2()
  type = match.arg(type)
  sp = .extract_states(x, pars)
  st = sp$states
  pars = sp$pars

  all_zones = .runoff_zones(pars, include_cu = TRUE)
  keep_idx = if (include_cu) seq_along(all_zones) else which(!grepl("-CU$", all_zones))
  zones = all_zones[keep_idx]
  z_labs = .zone_labels(pars, zones)

  st$wy = ifelse(st$month >= 10L, st$year + 1L, st$year)
  if (spinup_years > 0L) {
    st = st[st$wy >= (min(st$wy, na.rm = TRUE) + spinup_years), , drop = FALSE]
  }
  steps_per_day = if (inherits(x, "nwsrfs_run")) {
    24L / x$dt_hours
  } else {
    n_days_u = length(unique(sprintf("%04d-%02d-%02d", st$year, st$month, st$day)))
    max(1L, as.integer(round(nrow(st) / max(1L, n_days_u))))
  }
  wy_counts = table(st$wy)
  full_wys = as.integer(names(wy_counts)[wy_counts >= 360L * steps_per_day])
  if (length(full_wys) > 0L) {
    st = st[st$wy %in% full_wys, , drop = FALSE]
  }
  n_wy = length(unique(st$wy))

  areas = vapply(zones, \(z) .get_par_val(pars, z, "zone_area"), numeric(1L))
  w_zone = if (anyNA(areas) || sum(areas) <= 0) {
    rep(1 / length(zones), length(zones))
  } else {
    areas / sum(areas)
  }

  zone_wb = do.call(
    rbind,
    lapply(seq_along(keep_idx), \(k) {
      idx = keep_idx[k]
      p_eff = sum(st[[paste0("psfall_", idx)]] + st[[paste0("prain_", idx)]], na.rm = TRUE) / n_wy
      tci = sum(st[[paste0("tci_", idx)]], na.rm = TRUE) / n_wy
      aet = sum(pmax(st[[paste0("aet_", idx)]], 0), na.rm = TRUE) / n_wy
      pet_col = paste0("pet_", idx)
      pet = if (pet_col %in% names(st)) {
        sum(st[[pet_col]], na.rm = TRUE) / n_wy
      } else {
        sum(st[[paste0("etd_", idx)]], na.rm = TRUE) / n_wy
      }
      data.frame(
        unit = z_labs[k],
        p_eff_mm = p_eff,
        pet_mm = pet,
        aet_mm = aet,
        tci_mm = tci,
        stringsAsFactors = FALSE
      )
    })
  )

  if (length(keep_idx) > 1L) {
    basin_row = data.frame(
      unit = "Basin area-weighted",
      p_eff_mm = sum(zone_wb$p_eff_mm * w_zone),
      pet_mm = sum(zone_wb$pet_mm * w_zone),
      aet_mm = sum(zone_wb$aet_mm * w_zone),
      tci_mm = sum(zone_wb$tci_mm * w_zone),
      stringsAsFactors = FALSE
    )
    wb_df = rbind(zone_wb, basin_row)
  } else {
    wb_df = zone_wb
  }
  wb_df$unit = factor(wb_df$unit, levels = wb_df$unit)

  if (type == "budyko") {
    wb_df$dryness = wb_df$pet_mm / wb_df$p_eff_mm
    wb_df$evap_idx = wb_df$aet_mm / wb_df$p_eff_mm
    x_max = max(2.2, max(wb_df$dryness, na.rm = TRUE) * 1.15)
    phi = seq(0.01, x_max, by = 0.02)
    budyko_curve = data.frame(
      x = c(0, phi),
      y = c(0, sqrt(phi * tanh(1 / phi) * (1 - exp(-phi))))
    )
    limits_df = data.frame(x = c(0, 1, x_max), y = c(0, 1, 1))
    cols = setNames(c(rep_len(.okabe_ito, nrow(zone_wb)), "#1B1B1B")[seq_len(nrow(wb_df))], levels(wb_df$unit))

    return(
      ggplot2::ggplot() +
        ggplot2::geom_line(
          data = limits_df,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
          color = "#888888",
          linewidth = 0.6
        ) +
        ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "#BBBBBB", linewidth = 0.5) +
        ggplot2::geom_line(
          data = budyko_curve,
          ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
          color = "#4A4A4A",
          linetype = "dotted",
          linewidth = 0.75
        ) +
        ggplot2::geom_point(
          data = wb_df,
          ggplot2::aes(
            x = .data[["dryness"]],
            y = .data[["evap_idx"]],
            color = .data[["unit"]]
          ),
          size = 3.2
        ) +
        ggplot2::scale_color_manual(values = cols) +
        ggplot2::scale_x_continuous(limits = c(0, x_max), expand = c(0.02, 0)) +
        ggplot2::scale_y_continuous(limits = c(0, 1.08), breaks = seq(0, 1, by = 0.2)) +
        ggplot2::labs(
          x = "Dryness index (PET / Effective P)",
          y = "Evaporative index (AET / Effective P)"
        ) +
        theme_nwsrfs()
    )
  }

  bars_df = do.call(
    rbind,
    lapply(seq_len(nrow(wb_df)), \(i) {
      u = wb_df$unit[i]
      rbind(
        data.frame(unit = u, side = "Input", component = "Effective P (rain + SCF snow)", mm_yr = wb_df$p_eff_mm[i]),
        data.frame(unit = u, side = "Output", component = "Actual ET (AET)", mm_yr = wb_df$aet_mm[i]),
        data.frame(unit = u, side = "Output", component = "Channel inflow (TCI)", mm_yr = wb_df$tci_mm[i])
      )
    })
  )
  comp_levels = c("Effective P (rain + SCF snow)", "Actual ET (AET)", "Channel inflow (TCI)")
  bars_df$component = factor(bars_df$component, levels = comp_levels)
  bars_df$side = factor(bars_df$side, levels = c("Input", "Output"))

  ggplot2::ggplot(
    bars_df,
    ggplot2::aes(
      x = .data[["side"]],
      y = .data[["mm_yr"]],
      fill = .data[["component"]]
    )
  ) +
    ggplot2::geom_col(width = 0.62) +
    ggplot2::facet_wrap(~unit, nrow = 1L) +
    ggplot2::scale_fill_manual(
      values = c(
        "Effective P (rain + SCF snow)" = "#56B4E9",
        "Actual ET (AET)" = "#009E73",
        "Channel inflow (TCI)" = "#0072B2"
      )
    ) +
    ggplot2::scale_y_continuous(labels = .comma_labels) +
    ggplot2::labs(x = NULL, y = "Mean annual depth (mm/yr)") +
    theme_nwsrfs()
}


# %%
# Streamflow hydrograph and validation report plots

#' Plot daily streamflow hydrographs, routing decompositions, or run comparisons
#'
#' Plots period-averaged daily streamflow for a single `"nwsrfs_run"` object
#' (with optional observed gauge overlay, local vs Lag-K routing decomposition,
#' and shaded imputed-data intervals) or compares multiple `"nwsrfs_run"`
#' objects (e.g., parameter sensitivity runs from [update_pars()]).
#'
#' @param x An `"nwsrfs_run"` object or a named list of `"nwsrfs_run"` objects.
#' @param obs Optional observed daily flow `data.frame`, `NULL` (uses
#'   `x$daily_flow` when available), or `FALSE` to omit observed flows.
#' @param water_year Optional integer water year(s) to subset (e.g., `2011`).
#' @param start,end Optional start and end dates (`Date` or `"YYYY-MM-DD"`).
#' @param component Flow series to extract per run: `"sim"` (outlet discharge,
#'   default), `"local"` (`sacsnow_sf`), or `"lagk"` (`lagk_route`).
#' @param decompose Logical; for a single `"nwsrfs_run"`, also overlay routed
#'   upstream (`Lag-K`) and local zone (`SAC-SMA + UH`) contributions when
#'   present. Default `FALSE`.
#' @param shade_source Optional character vector of `Source` values (e.g.,
#'   `"Data_Ranger"`) to highlight with a light grey background band indicating
#'   reconstructed or imputed observations.
#' @param log Logical; use a base-10 logarithmic discharge axis. Default `FALSE`.
#' @param spinup_years Integer spin-up water years to discard. Default `1L`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_hydrograph(run, water_year = 2011, decompose = TRUE)
#' }
plot_hydrograph = function(
  x,
  obs = NULL,
  water_year = NULL,
  start = NULL,
  end = NULL,
  component = c("sim", "local", "lagk"),
  decompose = FALSE,
  shade_source = NULL,
  log = FALSE,
  spinup_years = 1L
) {
  .check_ggplot2()
  component = match.arg(component)
  col_map = c(sim = "sim_cfs", local = "local_cfs", lagk = "lagk_cfs")
  target_col = col_map[[component]]

  filter_window = function(df) {
    if (!is.null(water_year)) {
      df = df[df$wy %in% as.integer(water_year), , drop = FALSE]
    }
    if (!is.null(start)) {
      df = df[df$date >= as.Date(start), , drop = FALSE]
    }
    if (!is.null(end)) {
      df = df[df$date <= as.Date(end), , drop = FALSE]
    }
    df
  }

  single_run = inherits(x, "nwsrfs_run")
  runs = .as_named_runs(x, default_name = if (decompose) "Total simulated" else "Simulated")

  d_first = filter_window(sim_to_daily(runs[[1L]], obs = obs, spinup_years = spinup_years))
  if (nrow(d_first) == 0L) {
    stop("No daily observations/simulations remain in the selected date window.", call. = FALSE)
  }

  series_rows = list()
  if (!identical(obs, FALSE) && "obs_cfs" %in% names(d_first) && component == "sim") {
    series_rows[["Observed"]] = data.frame(
      date = d_first$date,
      series = "Observed",
      flow_cfs = d_first$obs_cfs
    )
  }

  for (nm in names(runs)) {
    d_run = if (identical(nm, names(runs)[1L])) {
      d_first
    } else {
      filter_window(sim_to_daily(runs[[nm]], obs = FALSE, spinup_years = spinup_years))
    }
    if (!target_col %in% names(d_run)) {
      stop("Component '", component, "' is not present in run '", nm, "'.", call. = FALSE)
    }
    series_rows[[nm]] = data.frame(
      date = d_run$date,
      series = nm,
      flow_cfs = d_run[[target_col]]
    )
  }

  if (single_run && decompose && component == "sim") {
    if ("lagk_cfs" %in% names(d_first)) {
      series_rows[["Routed upstream (Lag-K)"]] = data.frame(
        date = d_first$date,
        series = "Routed upstream (Lag-K)",
        flow_cfs = d_first$lagk_cfs
      )
    }
    if ("local_cfs" %in% names(d_first)) {
      series_rows[["Local zones (SAC-SMA + UH)"]] = data.frame(
        date = d_first$date,
        series = "Local zones (SAC-SMA + UH)",
        flow_cfs = d_first$local_cfs
      )
    }
  }

  long_df = do.call(rbind, series_rows)
  levs = names(series_rows)
  long_df$series = factor(long_df$series, levels = levs)
  if (log) {
    long_df$flow_cfs = pmax(long_df$flow_cfs, 0.1)
  }

  # Assign colorblind-safe colors
  if (single_run && decompose && component == "sim") {
    pal = c(
      "Observed" = "#1B1B1B",
      "Total simulated" = "#0072B2",
      "Routed upstream (Lag-K)" = "#009E73",
      "Local zones (SAC-SMA + UH)" = "#D55E00"
    )
    cols = pal[levs]
  } else {
    has_obs = "Observed" %in% levs
    other_levs = setdiff(levs, "Observed")
    run_pal = if (!has_obs && length(other_levs) > 1L) {
      c("#1B1B1B", .okabe_ito[c(2L, 3L, 1L, 4L, 5L, 6L)])
    } else {
      .okabe_ito
    }
    other_cols = setNames(rep_len(run_pal, length(other_levs)), other_levs)
    cols = if (has_obs) c("Observed" = "#1B1B1B", other_cols) else other_cols
  }

  p = ggplot2::ggplot()

  if (!is.null(shade_source) && "Source" %in% names(d_first)) {
    flag = !is.na(d_first$Source) & d_first$Source %in% shade_source
    if (any(flag)) {
      rle_f = rle(flag)
      ends = cumsum(rle_f$lengths)
      starts = ends - rle_f$lengths + 1L
      idx_true = which(rle_f$values)
      rects = data.frame(
        xmin = d_first$date[starts[idx_true]] - 0.5,
        xmax = d_first$date[ends[idx_true]] + 0.5
      )
      p = p +
        ggplot2::geom_rect(
          data = rects,
          ggplot2::aes(
            xmin = .data[["xmin"]],
            xmax = .data[["xmax"]],
            ymin = if (log) 0.1 else -Inf,
            ymax = Inf
          ),
          fill = "#E5E5E5",
          alpha = 0.55
        )
    }
  }

  p = p +
    ggplot2::geom_line(
      data = long_df,
      ggplot2::aes(
        x = .data[["date"]],
        y = .data[["flow_cfs"]],
        color = .data[["series"]]
      ),
      linewidth = 0.75
    ) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::labs(
      x = "Date",
      y = if (log) "Discharge (cfs, log scale)" else "Discharge (cfs)"
    ) +
    theme_nwsrfs()

  if (length(levs) > 3L) {
    p = p + ggplot2::guides(color = ggplot2::guide_legend(nrow = 2L))
  }
  if (log) {
    p = p + ggplot2::scale_y_log10(labels = .comma_labels)
  } else {
    p = p + ggplot2::scale_y_continuous(labels = .comma_labels)
  }
  p
}


#' Plot water-year daily discharge climatology and stage adjustments
#'
#' Computes the 365-day water-year (`Oct`-`Sep`, excluding Feb 29) daily mean
#' discharge and optional interannual percentile envelope (`quantiles`),
#' comparing simulated output against observations, comparing multiple runs, or
#' decomposing intermediate channel-loss and consumptive-use stages (`stages =
#' TRUE`).
#'
#' @param x An `"nwsrfs_run"` object or a named list of `"nwsrfs_run"` objects.
#' @param obs Optional observed daily flow `data.frame`, `NULL` (uses
#'   `x$daily_flow`), or `FALSE` to omit observations.
#' @param quantiles Numeric length-2 vector of lower and upper probabilities for
#'   shaded interannual ribbons (default `c(0.1, 0.9)`), or `NULL` to omit
#'   ribbons. Automatically omitted when `stages = TRUE`.
#' @param log Logical; use a base-10 logarithmic y-axis. Default `FALSE`.
#' @param stages Logical; for a single `"nwsrfs_run"`, decompose water-year mean
#'   discharge across active post-routing loss/diversion modules (`SAC-SMA +
#'   UH`, `chanloss()`, and `consuse()`). Default `FALSE`.
#' @param spinup_years Integer spin-up water years to discard. Default `1L`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run_nrkw1 = load_example("NRKW1")
#'   plot_water_year(run_nrkw1, log = TRUE)
#'
#'   run_sfln2 = load_example("SFLN2")
#'   plot_water_year(run_sfln2, stages = TRUE)
#' }
plot_water_year = function(
  x,
  obs = NULL,
  quantiles = c(0.1, 0.9),
  log = FALSE,
  stages = FALSE,
  spinup_years = 1L
) {
  .check_ggplot2()

  summarize_wy = function(vals, wy_day, label, q_probs) {
    d_mean = tapply(vals, wy_day, \(v) mean(v, na.rm = TRUE))
    out = data.frame(
      wy_day = as.integer(names(d_mean)),
      series = label,
      mean_cfs = as.numeric(d_mean)
    )
    if (!is.null(q_probs)) {
      out$q_lo = as.numeric(tapply(vals, wy_day, \(v) quantile(v, probs = q_probs[1L], na.rm = TRUE, names = FALSE)))
      out$q_hi = as.numeric(tapply(vals, wy_day, \(v) quantile(v, probs = q_probs[2L], na.rm = TRUE, names = FALSE)))
    }
    out
  }

  if (stages) {
    if (!inherits(x, "nwsrfs_run")) {
      stop("'stages = TRUE' requires a single 'nwsrfs_run' object.", call. = FALSE)
    }
    ts_ref = if (!is.null(x$forcings)) x$forcings[[1L]] else x$forcing_raw[[1L]]
    d_base = sim_to_daily(x, obs = obs, spinup_years = spinup_years)
    d_base = d_base[!(d_base$month == 2L & d_base$day == 29L), , drop = FALSE]
    d_base$wy_day = .wy_day_index(d_base$date, d_base$year, d_base$month, d_base$day, d_base$wy)

    stage_list = list()
    inflow_6 = numeric(length(x$sim))
    if (!is.null(x$sacsnow_sf)) inflow_6 = inflow_6 + x$sacsnow_sf
    if (!is.null(x$lagk_route)) inflow_6 = inflow_6 + x$lagk_route

    inflow_label = if (isTRUE(x$upflow_logic)) {
      "1. Unattenuated inflow (SAC-SMA + UH + Lag-K)"
    } else {
      "1. Local snowmelt runoff (SAC-SMA + UH)"
    }
    d_in = sim_to_daily(inflow_6, obs = FALSE, ts = ts_ref, spinup_years = spinup_years)
    d_in = d_in[!(d_in$month == 2L & d_in$day == 29L), , drop = FALSE]
    d_in$wy_day = .wy_day_index(d_in$date, d_in$year, d_in$month, d_in$day, d_in$wy)
    stage_list[[inflow_label]] = summarize_wy(d_in$sim_cfs, d_in$wy_day, inflow_label, NULL)

    if (isTRUE(x$chanloss_logic)) {
      after_cl = chanloss(inflow_6, x$forcing_raw[x$zone_names[1L]], x$dt_hours, x$pars)
      d_cl = sim_to_daily(after_cl, obs = FALSE, ts = ts_ref, spinup_years = spinup_years)
      d_cl = d_cl[!(d_cl$month == 2L & d_cl$day == 29L), , drop = FALSE]
      d_cl$wy_day = .wy_day_index(d_cl$date, d_cl$year, d_cl$month, d_cl$day, d_cl$wy)
      cl_label = if (isTRUE(x$consuse_logic)) {
        "2. After channel seepage (chanloss)"
      } else {
        "2. Net simulated (after chanloss)"
      }
      stage_list[[cl_label]] = summarize_wy(d_cl$sim_cfs, d_cl$wy_day, cl_label, NULL)
    }

    if (isTRUE(x$consuse_logic) || !isTRUE(x$chanloss_logic)) {
      fin_idx = length(stage_list) + 1L
      fin_label = if (isTRUE(x$consuse_logic)) {
        sprintf("%d. Net simulated (after consuse)", fin_idx)
      } else {
        "Net simulated"
      }
      stage_list[[fin_label]] = summarize_wy(d_base$sim_cfs, d_base$wy_day, fin_label, NULL)
    }

    if (!identical(obs, FALSE) && "obs_cfs" %in% names(d_base)) {
      stage_list[["Observed"]] = summarize_wy(d_base$obs_cfs, d_base$wy_day, "Observed", NULL)
    }

    cyc_df = do.call(rbind, stage_list)
    levs = names(stage_list)
    cyc_df$series = factor(cyc_df$series, levels = levs)
    if (log) cyc_df$mean_cfs = pmax(cyc_df$mean_cfs, 0.1)

    stage_cols = setNames(rep_len(c("#56B4E9", "#E69F00", "#0072B2", "#009E73"), length(levs)), levs)
    stage_ltys = setNames(rep_len(c("dashed", "dotdash", "solid", "solid"), length(levs)), levs)
    if ("Observed" %in% levs) {
      stage_cols[["Observed"]] = "#1B1B1B"
      stage_ltys[["Observed"]] = "solid"
    }

    p = ggplot2::ggplot(
      cyc_df,
      ggplot2::aes(
        x = .data[["wy_day"]],
        y = .data[["mean_cfs"]],
        color = .data[["series"]],
        linetype = .data[["series"]]
      )
    ) +
      ggplot2::geom_line(linewidth = 0.85) +
      ggplot2::scale_x_continuous(breaks = .wy_month_starts, labels = .wy_labels, expand = c(0.01, 0.01)) +
      ggplot2::scale_color_manual(values = stage_cols) +
      ggplot2::scale_linetype_manual(values = stage_ltys) +
      ggplot2::guides(
        color = ggplot2::guide_legend(nrow = 2L),
        linetype = ggplot2::guide_legend(nrow = 2L)
      ) +
      ggplot2::labs(
        x = "Water-year month",
        y = if (log) "Mean daily discharge (cfs, log scale)" else "Mean daily discharge (cfs)"
      ) +
      theme_nwsrfs()

    return(if (log) p + ggplot2::scale_y_log10(labels = .comma_labels) else p + ggplot2::scale_y_continuous(labels = .comma_labels))
  }

  runs = .as_named_runs(x)
  d_first = sim_to_daily(runs[[1L]], obs = obs, spinup_years = spinup_years)
  d_first = d_first[!(d_first$month == 2L & d_first$day == 29L), , drop = FALSE]
  d_first$wy_day = .wy_day_index(d_first$date, d_first$year, d_first$month, d_first$day, d_first$wy)

  cyc_rows = list()
  if (!identical(obs, FALSE) && "obs_cfs" %in% names(d_first)) {
    cyc_rows[["Observed"]] = summarize_wy(d_first$obs_cfs, d_first$wy_day, "Observed", quantiles)
  }
  for (nm in names(runs)) {
    d_r = if (identical(nm, names(runs)[1L])) {
      d_first
    } else {
      tmp = sim_to_daily(runs[[nm]], obs = FALSE, spinup_years = spinup_years)
      tmp = tmp[!(tmp$month == 2L & tmp$day == 29L), , drop = FALSE]
      tmp$wy_day = .wy_day_index(tmp$date, tmp$year, tmp$month, tmp$day, tmp$wy)
      tmp
    }
    cyc_rows[[nm]] = summarize_wy(d_r$sim_cfs, d_r$wy_day, nm, quantiles)
  }

  cyc_df = do.call(rbind, cyc_rows)
  levs = names(cyc_rows)
  cyc_df$series = factor(cyc_df$series, levels = levs)

  if (log) {
    cyc_df$mean_cfs = pmax(cyc_df$mean_cfs, 0.1)
    if (!is.null(quantiles)) {
      cyc_df$q_lo = pmax(cyc_df$q_lo, 0.1)
      cyc_df$q_hi = pmax(cyc_df$q_hi, 0.1)
    }
  }

  other_levs = setdiff(levs, "Observed")
  other_cols = setNames(rep_len(.okabe_ito, length(other_levs)), other_levs)
  cols = if ("Observed" %in% levs) c("Observed" = "#1B1B1B", other_cols) else other_cols

  p = ggplot2::ggplot(
    cyc_df,
    ggplot2::aes(
      x = .data[["wy_day"]],
      color = .data[["series"]],
      fill = .data[["series"]]
    )
  )
  if (!is.null(quantiles)) {
    p = p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[["q_lo"]], ymax = .data[["q_hi"]]),
        alpha = 0.16,
        color = NA
      )
  }
  p = p +
    ggplot2::geom_line(ggplot2::aes(y = .data[["mean_cfs"]]), linewidth = 0.85) +
    ggplot2::scale_x_continuous(breaks = .wy_month_starts, labels = .wy_labels, expand = c(0.01, 0.01)) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::labs(
      x = "Water-year month",
      y = if (log) "Discharge (cfs, log scale)" else "Mean daily discharge (cfs)"
    ) +
    theme_nwsrfs()

  if (log) {
    p + ggplot2::scale_y_log10(labels = .comma_labels)
  } else {
    p + ggplot2::scale_y_continuous(labels = .comma_labels)
  }
}


#' Plot monthly mean hydrograph and percent volume bias across the water year
#'
#' Two-panel validation diagnostic showing (A) observed vs simulated monthly
#' mean discharge (`cfs`) and (B) monthly percent volume bias (`PBIAS`, `%`)
#' ordered from October through September.
#'
#' @param x An `"nwsrfs_run"` object or a named list of `"nwsrfs_run"` objects.
#' @param obs Optional observed daily flow `data.frame` (defaults to
#'   `x$daily_flow`).
#' @param source Optional character vector filtering rows to matching `Source`
#'   values.
#' @param spinup_years Integer spin-up water years to discard. Default `1L`.
#'
#' @return A two-panel `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_monthly_bias(run, source = "USGS_12210700")
#' }
plot_monthly_bias = function(x, obs = NULL, source = NULL, spinup_years = 1L) {
  .check_ggplot2()
  runs = .as_named_runs(x)
  d_first = sim_to_daily(runs[[1L]], obs = obs, spinup_years = spinup_years)
  if (!"obs_cfs" %in% names(d_first)) {
    stop("Observed daily flow is required for 'plot_monthly_bias()'.", call. = FALSE)
  }
  if (!is.null(source)) {
    d_first = d_first[d_first$Source %in% source, , drop = FALSE]
  }
  d_first = d_first[!is.na(d_first$obs_cfs), , drop = FALSE]

  panels = c("A. Monthly Mean Discharge (cfs)", "B. Monthly Percent Bias (%)")

  obs_m = aggregate(list(cfs = d_first$obs_cfs), by = list(month = d_first$month), FUN = \(v) mean(v, na.rm = TRUE))
  obs_m$wy_pos = match(obs_m$month, .wy_months)

  line_rows = list(
    data.frame(
      wy_pos = obs_m$wy_pos,
      panel = panels[1L],
      series = "Observed",
      value = obs_m$cfs
    )
  )
  bar_rows = list()

  for (nm in names(runs)) {
    d_r = if (identical(nm, names(runs)[1L])) {
      d_first
    } else {
      tmp = sim_to_daily(runs[[nm]], obs = obs, spinup_years = spinup_years)
      if (!is.null(source) && "Source" %in% names(tmp)) {
        tmp = tmp[tmp$Source %in% source, , drop = FALSE]
      }
      tmp[!is.na(tmp$obs_cfs), , drop = FALSE]
    }
    agg = aggregate(
      list(sim = d_r$sim_cfs, obs = d_r$obs_cfs),
      by = list(month = d_r$month),
      FUN = \(v) mean(v, na.rm = TRUE)
    )
    agg$wy_pos = match(agg$month, .wy_months)
    agg$pbias = ifelse(agg$obs > 0, 100 * (agg$sim - agg$obs) / agg$obs, NA_real_)

    line_rows[[nm]] = data.frame(
      wy_pos = agg$wy_pos,
      panel = panels[1L],
      series = nm,
      value = agg$sim
    )
    bar_rows[[nm]] = data.frame(
      wy_pos = agg$wy_pos,
      panel = panels[2L],
      run = nm,
      value = agg$pbias
    )
  }

  line_df = do.call(rbind, line_rows)
  bar_df = do.call(rbind, bar_rows)
  line_levs = c("Observed", names(runs))
  line_df$panel = factor(line_df$panel, levels = panels)
  bar_df$panel = factor(bar_df$panel, levels = panels)
  line_df$series = factor(line_df$series, levels = line_levs)
  bar_df$run = factor(bar_df$run, levels = names(runs))

  run_cols = setNames(rep_len(.okabe_ito, length(runs)), names(runs))
  line_cols = c("Observed" = "#1B1B1B", run_cols)
  zero_df = data.frame(panel = factor(panels[2L], levels = panels), yint = 0)

  ggplot2::ggplot() +
    ggplot2::geom_hline(
      data = zero_df,
      ggplot2::aes(yintercept = .data[["yint"]]),
      color = "#666666",
      linewidth = 0.45
    ) +
    ggplot2::geom_col(
      data = bar_df,
      ggplot2::aes(
        x = .data[["wy_pos"]],
        y = .data[["value"]],
        fill = .data[["run"]]
      ),
      position = "dodge",
      width = 0.65,
      alpha = 0.88,
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      data = line_df,
      ggplot2::aes(
        x = .data[["wy_pos"]],
        y = .data[["value"]],
        color = .data[["series"]]
      ),
      linewidth = 0.85
    ) +
    ggplot2::geom_point(
      data = line_df,
      ggplot2::aes(
        x = .data[["wy_pos"]],
        y = .data[["value"]],
        color = .data[["series"]]
      ),
      size = 1.6
    ) +
    ggplot2::facet_wrap(~panel, ncol = 1L, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = .wy_labels) +
    ggplot2::scale_color_manual(values = line_cols) +
    ggplot2::scale_fill_manual(values = run_cols) +
    ggplot2::labs(x = "Water-year month", y = NULL) +
    theme_nwsrfs()
}


#' Plot daily flow duration curve (exceedance or non-exceedance probability)
#'
#' Compares empirical flow distributions between observed and simulated daily
#' discharge across high-, mid-, and low-flow regimes.
#'
#' @param x An `"nwsrfs_run"` object or a named list of `"nwsrfs_run"` objects.
#' @param obs Optional observed daily flow `data.frame`, `NULL` (uses
#'   `x$daily_flow` when available), or `FALSE` to omit observations.
#' @param log Logical; plot discharge on a base-10 logarithmic y-axis. Default
#'   `TRUE`.
#' @param exceed Logical; plot exceedance probability (`100 * (1 - F(q))`,
#'   default `TRUE`) or non-exceedance probability (`100 * F(q)`, `FALSE`).
#' @param source Optional character vector filtering rows to matching `Source`
#'   values.
#' @param spinup_years Integer spin-up water years to discard. Default `1L`.
#'
#' @return A `ggplot` object.
#' @importFrom stats ecdf
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_fdc(run, source = "USGS_12210700")
#' }
plot_fdc = function(
  x,
  obs = NULL,
  log = TRUE,
  exceed = TRUE,
  source = NULL,
  spinup_years = 1L
) {
  .check_ggplot2()
  runs = .as_named_runs(x)
  d_first = sim_to_daily(runs[[1L]], obs = obs, spinup_years = spinup_years)
  if (!is.null(source) && "Source" %in% names(d_first)) {
    d_first = d_first[d_first$Source %in% source, , drop = FALSE]
  }

  build_fdc = function(vals, label) {
    vals = sort(vals[!is.na(vals)], decreasing = exceed)
    n = length(vals)
    # Weibull plotting position in percent
    prob = 100 * seq_len(n) / (n + 1L)
    if (!exceed) {
      vals = sort(vals, decreasing = FALSE)
      prob = 100 * seq_len(n) / (n + 1L)
    }
    data.frame(prob = prob, flow_cfs = vals, series = label)
  }

  fdc_rows = list()
  if (!identical(obs, FALSE) && "obs_cfs" %in% names(d_first)) {
    fdc_rows[["Observed"]] = build_fdc(d_first$obs_cfs, "Observed")
  }
  for (nm in names(runs)) {
    d_r = if (identical(nm, names(runs)[1L])) {
      d_first
    } else {
      tmp = sim_to_daily(runs[[nm]], obs = obs, spinup_years = spinup_years)
      if (!is.null(source) && "Source" %in% names(tmp)) {
        tmp = tmp[tmp$Source %in% source, , drop = FALSE]
      }
      tmp
    }
    fdc_rows[[nm]] = build_fdc(d_r$sim_cfs, nm)
  }

  fdc_df = do.call(rbind, fdc_rows)
  levs = names(fdc_rows)
  fdc_df$series = factor(fdc_df$series, levels = levs)
  if (log) {
    fdc_df$flow_cfs = pmax(fdc_df$flow_cfs, 0.1)
  }

  other_levs = setdiff(levs, "Observed")
  other_cols = setNames(rep_len(.okabe_ito, length(other_levs)), other_levs)
  cols = if ("Observed" %in% levs) c("Observed" = "#1B1B1B", other_cols) else other_cols

  p = ggplot2::ggplot(
    fdc_df,
    ggplot2::aes(
      x = .data[["prob"]],
      y = .data[["flow_cfs"]],
      color = .data[["series"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::labs(
      x = if (exceed) "Exceedance probability (%)" else "Non-exceedance probability (%)",
      y = if (log) "Daily discharge (cfs, log scale)" else "Daily discharge (cfs)"
    ) +
    theme_nwsrfs()

  if (log) {
    p + ggplot2::scale_y_log10(labels = .comma_labels)
  } else {
    p + ggplot2::scale_y_continuous(labels = .comma_labels)
  }
}


#' Plot observed vs simulated daily discharge scatter (full record or peaks)
#'
#' Scatter plot of paired daily observed and simulated discharge against a `1:1`
#' line. Set `quantile_min = 0.95` to evaluate top-5% peak flows.
#'
#' @param x An `"nwsrfs_run"` object or a named list of `"nwsrfs_run"` objects.
#' @param obs Optional observed daily flow `data.frame` (defaults to
#'   `x$daily_flow`).
#' @param quantile_min Optional lower observed-flow quantile threshold in
#'   `[0, 1)` (e.g., `0.95` for peak-flow validation above the 95th percentile).
#' @param log Logical; use base-10 logarithmic axes. Default `FALSE`.
#' @param source Optional character vector filtering rows to matching `Source`
#'   values.
#' @param spinup_years Integer spin-up water years to discard. Default `1L`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_scatter(run, quantile_min = 0.95, source = "USGS_12210700")
#' }
plot_scatter = function(
  x,
  obs = NULL,
  quantile_min = NULL,
  log = FALSE,
  source = NULL,
  spinup_years = 1L
) {
  .check_ggplot2()
  runs = .as_named_runs(x)

  sc_rows = lapply(names(runs), \(nm) {
    df = sim_to_daily(runs[[nm]], obs = obs, spinup_years = spinup_years)
    if (!"obs_cfs" %in% names(df)) {
      stop("Observed daily flow is required for 'plot_scatter()'.", call. = FALSE)
    }
    if (!is.null(source) && "Source" %in% names(df)) {
      df = df[df$Source %in% source, , drop = FALSE]
    }
    df = df[!is.na(df$obs_cfs) & !is.na(df$sim_cfs), , drop = FALSE]
    if (!is.null(quantile_min)) {
      q_thr = quantile(df$obs_cfs, probs = quantile_min, na.rm = TRUE, names = FALSE)
      df = df[df$obs_cfs >= q_thr, , drop = FALSE]
    }
    data.frame(run = nm, obs_cfs = df$obs_cfs, sim_cfs = df$sim_cfs)
  })

  sc_df = do.call(rbind, sc_rows)
  sc_df$run = factor(sc_df$run, levels = names(runs))
  if (log) {
    sc_df$obs_cfs = pmax(sc_df$obs_cfs, 0.1)
    sc_df$sim_cfs = pmax(sc_df$sim_cfs, 0.1)
  }

  cols = setNames(rep_len(.okabe_ito, length(runs)), names(runs))

  p = ggplot2::ggplot(
    sc_df,
    ggplot2::aes(
      x = .data[["obs_cfs"]],
      y = .data[["sim_cfs"]],
      color = .data[["run"]]
    )
  ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#4A4A4A", linewidth = 0.65) +
    ggplot2::geom_point(alpha = 0.45, size = 1.4) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::labs(
      x = "Observed daily discharge (cfs)",
      y = "Simulated daily discharge (cfs)"
    ) +
    theme_nwsrfs()

  if (length(runs) == 1L) {
    p = p + ggplot2::theme(legend.position = "none")
  }
  lim_lo = min(c(sc_df$obs_cfs, sc_df$sim_cfs), na.rm = TRUE) * 0.95
  lim_hi = max(c(sc_df$obs_cfs, sc_df$sim_cfs), na.rm = TRUE) * 1.04
  if (log) {
    p +
      ggplot2::scale_x_log10(limits = c(lim_lo, lim_hi), labels = .comma_labels) +
      ggplot2::scale_y_log10(limits = c(lim_lo, lim_hi), labels = .comma_labels)
  } else {
    p +
      ggplot2::scale_x_continuous(limits = c(lim_lo, lim_hi), labels = .comma_labels) +
      ggplot2::scale_y_continuous(limits = c(lim_lo, lim_hi), labels = .comma_labels)
  }
}


#' Plot cumulative runoff volume or cumulative volume error over time
#'
#' Diagnoses multi-year volume bias, step changes in gauge records, or drift by
#' accumulating paired daily discharge in thousand acre-feet (`KAF`, where `1
#' cfs-day = 0.001983471 KAF`).
#'
#' @param x An `"nwsrfs_run"` object or a named list of `"nwsrfs_run"` objects.
#' @param obs Optional observed daily flow `data.frame` (defaults to
#'   `x$daily_flow`).
#' @param type `"flow"` (default) to compare cumulative observed and simulated
#'   runoff volumes, or `"error"` to plot cumulative `Simulated - Observed`
#'   volume error (`KAF`).
#' @param source Optional character vector filtering rows to matching `Source`
#'   values.
#' @param spinup_years Integer spin-up water years to discard. Default `1L`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_cumulative(run, type = "flow")
#'   plot_cumulative(run, type = "error")
#' }
plot_cumulative = function(
  x,
  obs = NULL,
  type = c("flow", "error"),
  source = NULL,
  spinup_years = 1L
) {
  .check_ggplot2()
  type = match.arg(type)
  cfsd_to_kaf = 86400 / 43560 / 1000
  runs = .as_named_runs(x)

  d_first = sim_to_daily(runs[[1L]], obs = obs, spinup_years = spinup_years)
  if (!"obs_cfs" %in% names(d_first)) {
    stop("Observed daily flow is required for 'plot_cumulative()'.", call. = FALSE)
  }
  if (!is.null(source) && "Source" %in% names(d_first)) {
    d_first = d_first[d_first$Source %in% source, , drop = FALSE]
  }
  d_first = d_first[!is.na(d_first$obs_cfs) & !is.na(d_first$sim_cfs), , drop = FALSE]

  rows = list()
  if (type == "flow") {
    rows[["Observed"]] = data.frame(
      date = d_first$date,
      series = "Observed",
      kaf = cumsum(d_first$obs_cfs) * cfsd_to_kaf
    )
  }

  for (nm in names(runs)) {
    d_r = if (identical(nm, names(runs)[1L])) {
      d_first
    } else {
      tmp = sim_to_daily(runs[[nm]], obs = obs, spinup_years = spinup_years)
      if (!is.null(source) && "Source" %in% names(tmp)) {
        tmp = tmp[tmp$Source %in% source, , drop = FALSE]
      }
      tmp[!is.na(tmp$obs_cfs) & !is.na(tmp$sim_cfs), , drop = FALSE]
    }
    vals = if (type == "flow") {
      cumsum(d_r$sim_cfs) * cfsd_to_kaf
    } else {
      cumsum(d_r$sim_cfs - d_r$obs_cfs) * cfsd_to_kaf
    }
    rows[[nm]] = data.frame(date = d_r$date, series = nm, kaf = vals)
  }

  cum_df = do.call(rbind, rows)
  levs = names(rows)
  cum_df$series = factor(cum_df$series, levels = levs)

  other_levs = setdiff(levs, "Observed")
  other_cols = setNames(rep_len(.okabe_ito, length(other_levs)), other_levs)
  cols = if ("Observed" %in% levs) c("Observed" = "#1B1B1B", other_cols) else other_cols

  p = ggplot2::ggplot(
    cum_df,
    ggplot2::aes(
      x = .data[["date"]],
      y = .data[["kaf"]],
      color = .data[["series"]]
    )
  )
  if (type == "error") {
    p = p + ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#666666", linewidth = 0.5)
  }
  p = p +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = .comma_labels) +
    ggplot2::labs(
      x = "Date",
      y = if (type == "flow") {
        "Cumulative runoff volume (KAF)"
      } else {
        "Cumulative volume error, Sim - Obs (KAF)"
      }
    ) +
    theme_nwsrfs()

  if (type == "error" && length(runs) == 1L) {
    p = p + ggplot2::theme(legend.position = "none")
  }
  p
}


#' Plot seasonal water-supply runoff volume validation
#'
#' Aggregates paired daily flows over a target water-supply forecast season
#' (default `April`-`September`, `months = 4:9`) for each complete water year in
#' thousand acre-feet (`KAF`) and plots either an observed-vs-simulated `1:1`
#' comparison (`type = "scatter"`) or an interannual time series (`type =
#' "timeseries"`).
#'
#' @param x An `"nwsrfs_run"` object or a named list of `"nwsrfs_run"` objects.
#' @param obs Optional observed daily flow `data.frame` (defaults to
#'   `x$daily_flow`).
#' @param months Integer vector of calendar months defining the water-supply
#'   season within each water year. Default `4L:9L` (`Apr`-`Sep`).
#' @param type `"scatter"` (default) for paired annual seasonal volumes against
#'   the `1:1` line, or `"timeseries"` for annual volumes by water year.
#' @param source Optional character vector filtering rows to matching `Source`
#'   values.
#' @param spinup_years Integer spin-up water years to discard. Default `1L`.
#'
#' @return A `ggplot` object.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   run = load_example("NRKW1")
#'   plot_water_supply(run, months = 4:9)
#' }
plot_water_supply = function(
  x,
  obs = NULL,
  months = 4L:9L,
  type = c("scatter", "timeseries"),
  source = NULL,
  spinup_years = 1L
) {
  .check_ggplot2()
  type = match.arg(type)
  months = as.integer(months)
  cfsd_to_kaf = 86400 / 43560 / 1000
  min_days = length(months) * 27L
  runs = .as_named_runs(x)

  season_tag = if (length(months) == 1L) {
    month.abb[months]
  } else {
    paste0(month.abb[months[1L]], "-", month.abb[months[length(months)]])
  }

  ws_rows = lapply(names(runs), \(nm) {
    df = sim_to_daily(runs[[nm]], obs = obs, spinup_years = spinup_years)
    if (!"obs_cfs" %in% names(df)) {
      stop("Observed daily flow is required for 'plot_water_supply()'.", call. = FALSE)
    }
    if (!is.null(source) && "Source" %in% names(df)) {
      df = df[df$Source %in% source, , drop = FALSE]
    }
    df = df[df$month %in% months & !is.na(df$obs_cfs) & !is.na(df$sim_cfs), , drop = FALSE]
    if (nrow(df) == 0L) {
      stop("No valid seasonal observations remain.", call. = FALSE)
    }
    cnt = tapply(df$obs_cfs, df$wy, length)
    keep_wy = as.integer(names(cnt)[cnt >= min_days])
    df = df[df$wy %in% keep_wy, , drop = FALSE]

    agg = aggregate(
      list(obs_kaf = df$obs_cfs * cfsd_to_kaf, sim_kaf = df$sim_cfs * cfsd_to_kaf),
      by = list(wy = df$wy),
      FUN = sum
    )
    agg$run = nm
    agg
  })

  ws_df = do.call(rbind, ws_rows)
  ws_df$run = factor(ws_df$run, levels = names(runs))

  if (type == "scatter") {
    cols = setNames(rep_len(.okabe_ito, length(runs)), names(runs))
    lim_hi = max(c(ws_df$obs_kaf, ws_df$sim_kaf), na.rm = TRUE) * 1.05
    lim_lo = min(c(ws_df$obs_kaf, ws_df$sim_kaf), na.rm = TRUE) * 0.92

    p = ggplot2::ggplot(
      ws_df,
      ggplot2::aes(
        x = .data[["obs_kaf"]],
        y = .data[["sim_kaf"]],
        color = .data[["run"]]
      )
    ) +
      ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#4A4A4A", linewidth = 0.65) +
      ggplot2::geom_point(size = 2.4, alpha = 0.85) +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::scale_x_continuous(limits = c(lim_lo, lim_hi), labels = .comma_labels) +
      ggplot2::scale_y_continuous(limits = c(lim_lo, lim_hi), labels = .comma_labels) +
      ggplot2::labs(
        x = paste0("Observed ", season_tag, " volume (KAF)"),
        y = paste0("Simulated ", season_tag, " volume (KAF)")
      ) +
      theme_nwsrfs()

    if (length(runs) == 1L) {
      p = p + ggplot2::theme(legend.position = "none")
    }
    return(p)
  }

  first_ws = ws_rows[[1L]]
  ts_rows = c(
    list(data.frame(wy = first_ws$wy, series = "Observed", kaf = first_ws$obs_kaf)),
    lapply(names(runs), \(nm) {
      sub_w = ws_df[ws_df$run == nm, , drop = FALSE]
      data.frame(wy = sub_w$wy, series = nm, kaf = sub_w$sim_kaf)
    })
  )
  ts_df = do.call(rbind, ts_rows)
  levs = c("Observed", names(runs))
  ts_df$series = factor(ts_df$series, levels = levs)
  cols = c("Observed" = "#1B1B1B", setNames(rep_len(.okabe_ito, length(runs)), names(runs)))

  ggplot2::ggplot(
    ts_df,
    ggplot2::aes(
      x = .data[["wy"]],
      y = .data[["kaf"]],
      color = .data[["series"]]
    )
  ) +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::geom_point(size = 1.7) +
    ggplot2::scale_color_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = .comma_labels) +
    ggplot2::labs(
      x = "Water year",
      y = paste0(season_tag, " runoff volume (KAF)")
    ) +
    theme_nwsrfs()
}
