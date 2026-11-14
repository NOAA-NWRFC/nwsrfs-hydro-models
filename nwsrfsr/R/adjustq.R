# %%
# AdjustQ: Replicates the CHPS FEWS AdjustQ tool
# Used as preprocessing step to create upstream flow for Lag-K reaches

#' AdjustQ: Create upstream flow timeseries from observations and simulation
#'
#' Replicates the CHPS FEWS AdjustQ tool. Adjusts observed mean daily discharges
#' using observed instantaneous and simulated discharges. If both observed mean
#' daily and instantaneous data are missing, simulated discharge is used.
#' Supports model timesteps of `1`, `2`, `3`, `4`, `6`, `8`, `12`, or `24` hours.
#'
#' @param daily_flow Data.frame with columns `year`, `month`, `day`, `flow_cfs`
#'   (daily mean observations).
#' @param inst_flow Data.frame with columns `year`, `month`, `day`, `hour`,
#'   `flow_cfs` (instantaneous observations).
#' @param sim Data.frame with columns `year`, `month`, `day`, `hour`, `flow_cfs`
#'   (simulated flow), an `"nwsrfs_run"` object, or `NULL`. If provided,
#'   small/large gap filling uses simulation as fallback.
#' @param blend Integer; threshold for small vs large gap in timesteps. Default `10L`.
#' @param interp_type `"ratio"` or `"difference"`. Default `"ratio"`.
#' @param error_tol Numeric; daily volume matching tolerance (fraction). Default `0.01`.
#' @param max_iterations Integer; max daily correction iterations. Default `15L`.
#' @param dt_hours Optional integer model timestep in hours (`1`, `2`, `3`, `4`,
#'   `6`, `8`, `12`, or `24`). When `sim` is supplied and `dt_hours = NULL`,
#'   `dt_hours` is inferred from `sim` (and validated against `sim` when
#'   supplied). When `sim = NULL` and `dt_hours = NULL`, defaults to `6L`.
#'
#' @return A `data.frame` with columns `datetime` (`POSIXct`, UTC) and
#'   `flow_cfs` (adjusted flow in cfs).
#' @importFrom stats approx
#' @export
adjustq = function(
  daily_flow,
  inst_flow,
  sim = NULL,
  blend = 10L,
  interp_type = "ratio",
  error_tol = 0.01,
  max_iterations = 15L,
  dt_hours = NULL
) {
  if (
    is.logical(blend) ||
      !is.numeric(blend) ||
      length(blend) != 1L ||
      !is.finite(blend) ||
      blend < 0 ||
      blend != floor(blend)
  ) {
    stop("adjustq(): 'blend' must be a non-negative integer.", call. = FALSE)
  }
  if (
    is.logical(max_iterations) ||
      !is.numeric(max_iterations) ||
      length(max_iterations) != 1L ||
      !is.finite(max_iterations) ||
      max_iterations < 0 ||
      max_iterations != floor(max_iterations)
  ) {
    stop("adjustq(): 'max_iterations' must be a non-negative integer.", call. = FALSE)
  }
  if (is.logical(error_tol) || !is.numeric(error_tol) || length(error_tol) != 1L || !is.finite(error_tol) || error_tol < 0) {
    stop("adjustq(): 'error_tol' must be a non-negative finite number.", call. = FALSE)
  }
  blend = as.integer(blend)
  max_iterations = as.integer(max_iterations)
  if (!is.character(interp_type) || length(interp_type) != 1L || !interp_type %in% c("ratio", "difference")) {
    stop("interp_type must be 'ratio' or 'difference'")
  }
  if (nrow(daily_flow) == 0L || nrow(inst_flow) == 0L) {
    stop("adjustq(): 'daily_flow' and 'inst_flow' must contain at least one row.", call. = FALSE)
  }
  if (!is.null(sim) && !inherits(sim, "nwsrfs_run") && nrow(sim) == 0L) {
    stop("adjustq(): 'sim' must contain at least one row when supplied.", call. = FALSE)
  }

  # --- Build datetime indexes ---
  obs_daily_dates = as.Date(sprintf("%04d-%02d-%02d", daily_flow$year, daily_flow$month, daily_flow$day))
  obs_daily = data.frame(date = obs_daily_dates, flow_cfs = as.double(daily_flow$flow_cfs))

  inst_dt = as.POSIXct(
    sprintf(
      "%04d-%02d-%02d %02d:00:00",
      inst_flow$year,
      inst_flow$month,
      inst_flow$day,
      inst_flow$hour
    ),
    tz = "UTC"
  )
  obs_inst = data.frame(datetime = inst_dt, flow_cfs = as.double(inst_flow$flow_cfs))

  if (!is.null(sim)) {
    if (inherits(sim, "nwsrfs_run")) {
      ts_src = if (!is.null(sim$forcings) && length(sim$forcings) > 0L) {
        sim$forcings[[1L]]
      } else if (!is.null(sim$upflow) && length(sim$upflow) > 0L) {
        sim$upflow[[1L]]
      } else {
        stop("adjustq(): 'nwsrfs_run' object contains no timestamp source.", call. = FALSE)
      }
      sim = data.frame(
        ts_src[, c("year", "month", "day", "hour")],
        flow_cfs = as.double(sim$sim)
      )
    }
    if (!"flow_cfs" %in% names(sim)) {
      stop("adjustq(): 'sim' must contain column 'flow_cfs'.", call. = FALSE)
    }
    sim_ts = .infer_and_validate_ts(
      sim,
      dt_hours = dt_hours,
      require_hour = TRUE,
      supported = .supported_dt_hours,
      label = "sim"
    )
    dt_hours = sim_ts$dt_hours
    sim_dt = as.POSIXct(sim_ts$ts_sec, origin = "1970-01-01", tz = "UTC")
    sim_df = data.frame(datetime = sim_dt, simulated = as.double(sim$flow_cfs))
    result = .adjustq_with_sim(
      obs_daily = obs_daily,
      obs_inst = obs_inst,
      sim_df = sim_df,
      blend = blend,
      interp_type = interp_type,
      error_tol = error_tol,
      max_iterations = max_iterations,
      dt_hours = dt_hours
    )
  } else {
    dt_hours = if (is.null(dt_hours)) {
      6L
    } else {
      .integer_hours(dt_hours, name = "dt_hours", supported = .supported_dt_hours)
    }
    result = .inst_mean_q_merge_internal(
      obs_daily = obs_daily,
      obs_inst = obs_inst,
      error_tol = error_tol,
      max_iterations = max_iterations,
      dt_hours = dt_hours
    )
  }

  result
}


#' Load AdjustQ example
#'
#' Runs AdjustQ using bundled NRKW1 data.
#'
#' @param sim Logical; include simulation in AdjustQ. Default `TRUE`.
#' @param ... Additional arguments passed to [adjustq()].
#'
#' @return A `data.frame` (see [adjustq()]).
#' @export
adjustq_load_example = function(sim = TRUE, ...) {
  if (sim) {
    run = load_example("NRKW1")
    sim_df = data.frame(
      run$forcings[[1L]][, c("year", "month", "day", "hour")],
      flow_cfs = run$sim
    )
  } else {
    sim_df = NULL
  }

  adjustq(
    daily_flow = nrkw1_daily_flow,
    inst_flow = nrkw1_inst_flow,
    sim = sim_df,
    ...
  )
}


# %%
# Internal: snap instantaneous observations to regular grid (within 15 minutes)

.snap_inst_to_grid = function(obs_inst, grid, tol_sec = 900) {
  n_grid = length(grid)
  if (n_grid == 0L || nrow(obs_inst) == 0L) {
    return(rep(NA_real_, n_grid))
  }

  t_obs = as.numeric(obs_inst$datetime)
  f_obs = obs_inst$flow_cfs
  valid_t = !is.na(t_obs)
  t_obs = t_obs[valid_t]
  f_obs = f_obs[valid_t]
  if (length(t_obs) == 0L) {
    return(rep(NA_real_, n_grid))
  }

  ord = order(t_obs)
  t_obs = t_obs[ord]
  f_obs = f_obs[ord]
  keep = !duplicated(t_obs)
  t_obs = t_obs[keep]
  f_obs = f_obs[keep]

  m = length(t_obs)
  t_grid = as.numeric(grid)
  idx_l = findInterval(t_grid, t_obs)
  idx_l_safe = pmax(idx_l, 1L)
  idx_r = pmin(idx_l + 1L, m)

  d_l = ifelse(idx_l >= 1L, abs(t_grid - t_obs[idx_l_safe]), Inf)
  d_r = ifelse(idx_l < m, abs(t_obs[idx_r] - t_grid), Inf)
  use_l = d_l <= d_r
  closest = ifelse(use_l, idx_l_safe, idx_r)
  min_d = ifelse(use_l, d_l, d_r)

  val = f_obs[closest]
  ifelse(min_d <= tol_sec & !is.na(val) & val > 0, val, NA_real_)
}


# %%
# Internal: AdjustQ with simulation

.adjustq_with_sim = function(
  obs_daily,
  obs_inst,
  sim_df,
  blend,
  interp_type,
  error_tol,
  max_iterations,
  dt_hours = 6L
) {
  dt_sec = dt_hours * 3600
  phase_sec = as.numeric(sim_df$datetime[1L]) %% dt_sec

  # Snap instantaneous obs to nearest dt_hours grid (within 15 min)
  obs_begin = as.POSIXct(format(min(obs_inst$datetime), "%Y-%m-%d"), tz = "UTC") + phase_sec
  if (obs_begin > min(obs_inst$datetime)) {
    obs_begin = obs_begin - dt_sec
  }
  obs_end = if (dt_hours == 24L && phase_sec == 0) {
    as.POSIXct(format(max(obs_inst$datetime), "%Y-%m-%d"), tz = "UTC")
  } else {
    as.POSIXct(format(max(obs_inst$datetime) + 86400, "%Y-%m-%d"), tz = "UTC") + phase_sec
  }
  obs_grid = seq(obs_begin, obs_end, by = dt_sec)
  obs_step = data.frame(
    datetime = obs_grid,
    observed = .snap_inst_to_grid(obs_inst, obs_grid, tol_sec = 900)
  )

  # Extend simulated data to span complete days
  sim_begin = as.POSIXct(format(min(sim_df$datetime), "%Y-%m-%d"), tz = "UTC") + phase_sec
  if (sim_begin > min(sim_df$datetime)) {
    sim_begin = sim_begin - dt_sec
  }
  sim_end = if (dt_hours == 24L) {
    max(sim_df$datetime)
  } else {
    end_cand = as.POSIXct(format(max(sim_df$datetime) + 86400, "%Y-%m-%d"), tz = "UTC") + phase_sec
    if (end_cand < max(sim_df$datetime)) end_cand + dt_sec else end_cand
  }
  sim_grid = seq(sim_begin, sim_end, by = dt_sec)
  sim_ext = data.frame(datetime = sim_grid)
  sim_ext = merge(sim_ext, sim_df, by = "datetime", all.x = TRUE)
  # Interpolate to fill edges
  non_na = which(!is.na(sim_ext$simulated))
  if (length(non_na) == 1L) {
    sim_ext$simulated = rep(sim_ext$simulated[non_na], nrow(sim_ext))
  } else {
    sim_ext$simulated = approx(
      x = non_na,
      y = sim_ext$simulated[non_na],
      xout = seq_len(nrow(sim_ext)),
      rule = 2
    )$y
  }

  # Merge obs and sim
  working = merge(obs_step, sim_ext, by = "datetime", all = TRUE)
  working = working[order(working$datetime), ]

  # Compute ratio and difference
  working$Inst_Ratio = working$observed / working$simulated
  working$Inst_Difference = working$observed - working$simulated
  working$AdjustQ_Inst = working$observed

  # --- Find gaps in observed period ---
  has_obs = which(!is.na(working$observed))
  if (length(has_obs) > 1L) {
    gap_periods = diff(has_obs)
    gap_end_idx = has_obs[-1L][gap_periods > 1L]
    gap_sizes = gap_periods[gap_periods > 1L]

    if (length(gap_end_idx) > 0L) {
      small_gaps = which(gap_sizes < blend)
      large_gaps = which(gap_sizes >= blend)

      # Fill small gaps
      if (length(small_gaps) > 0L) {
        for (g in small_gaps) {
          end_i = gap_end_idx[g]
          period = gap_sizes[g]
          start_i = end_i - period

          interp_range = start_i:end_i
          start_ratio = working$Inst_Ratio[start_i]
          end_ratio = working$Inst_Ratio[end_i]

          ratio_check1 = abs(start_ratio - end_ratio) > 2
          ratio_check2 = max(start_ratio, end_ratio, na.rm = TRUE) > 5

          if (
            interp_type == "difference" ||
              !is.finite(start_ratio) ||
              !is.finite(end_ratio) ||
              isTRUE(ratio_check1) ||
              isTRUE(ratio_check2)
          ) {
            diffs_interp = approx(
              x = c(1L, length(interp_range)),
              y = working$Inst_Difference[c(start_i, end_i)],
              xout = seq_along(interp_range)
            )$y
            working$AdjustQ_Inst[interp_range] = working$simulated[interp_range] + diffs_interp
          } else {
            ratios_interp = approx(
              x = c(1L, length(interp_range)),
              y = working$Inst_Ratio[c(start_i, end_i)],
              xout = seq_along(interp_range)
            )$y
            working$AdjustQ_Inst[interp_range] = working$simulated[interp_range] * ratios_interp
          }
        }
      }

      # Fill large gaps
      if (length(large_gaps) > 0L && blend > 0L) {
        for (g in large_gaps) {
          end_i = gap_end_idx[g]
          period = gap_sizes[g]
          start_i = end_i - period

          # Back end blend
          end_blend_start = max(end_i - blend, 1L)
          end_blend_range = end_blend_start:end_i
          end_diff = working$Inst_Difference[end_i]
          if (!is.na(end_diff)) {
            blend_frac = 1 - (end_i - end_blend_range) / blend
            working$AdjustQ_Inst[end_blend_range] =
              working$simulated[end_blend_range] + blend_frac * end_diff
          }

          # Front end blend
          start_blend_end = min(start_i + blend, nrow(working))
          start_blend_range = start_i:start_blend_end
          start_diff = working$Inst_Difference[start_i]
          if (!is.na(start_diff)) {
            blend_frac = 1 - (start_blend_range - start_i) / blend
            working$AdjustQ_Inst[start_blend_range] =
              working$simulated[start_blend_range] + blend_frac * start_diff
          }
        }
      }
    }
  }

  # Condense to simulation period
  working = working[!is.na(working$simulated), ]
  # Fill remaining NAs with simulation
  na_adj = is.na(working$AdjustQ_Inst)
  working$AdjustQ_Inst[na_adj] = working$simulated[na_adj]

  # --- Daily volume correction ---
  working = .adjustq_daily(working, obs_daily, error_tol, max_iterations, dt_hours = dt_hours)

  # Clip negatives
  working$AdjustQ_Inst = pmax(working$AdjustQ_Inst, 0)

  data.frame(datetime = working$datetime, flow_cfs = working$AdjustQ_Inst)
}


# %%
# Internal: inst_mean_q_merge (no simulation)

.inst_mean_q_merge_internal = function(
  obs_daily,
  obs_inst,
  error_tol,
  max_iterations,
  dt_hours = 6L
) {
  dt_sec = dt_hours * 3600
  daily_dates = seq(min(obs_daily$date), max(obs_daily$date), by = "day")
  daily_flows = obs_daily$flow_cfs[match(daily_dates, obs_daily$date)]

  if (dt_hours == 24L) {
    daily_grid = data.frame(
      datetime = as.POSIXct(daily_dates, tz = "UTC"),
      daily_flow = daily_flows
    )
  } else {
    # Resample daily to dt_hours with forward fill, shifted forward by one step
    # so 00:00 belongs to the boundary between adjacent days
    k = 24L %/% dt_hours
    offsets_sec = seq_len(k) * dt_sec
    base_sec = as.numeric(as.POSIXct(daily_dates, tz = "UTC"))
    dts_sec = as.vector(outer(offsets_sec, base_sec, `+`))
    daily_grid = data.frame(
      datetime = as.POSIXct(dts_sec, origin = "1970-01-01", tz = "UTC"),
      daily_flow = rep(daily_flows, each = k)
    )
  }

  # Snap instantaneous to dt_hours grid
  grid_begin = min(daily_grid$datetime)
  grid_end = max(daily_grid$datetime)
  grid = seq(grid_begin, grid_end, by = dt_sec)
  inst_step = data.frame(
    datetime = grid,
    inst_flow = .snap_inst_to_grid(obs_inst, grid, tol_sec = 900)
  )

  # Merge: use inst where available, daily otherwise
  merged = merge(inst_step, daily_grid, by = "datetime", all.x = TRUE)
  merged = merged[order(merged$datetime), ]
  merged$AdjustQ_Inst = ifelse(!is.na(merged$inst_flow), merged$inst_flow, merged$daily_flow)

  # Remove rows with no data
  merged = merged[!is.na(merged$AdjustQ_Inst), ]

  # Daily volume correction
  merged = .adjustq_daily(merged, obs_daily, error_tol, max_iterations, dt_hours = dt_hours)

  # Clip negatives
  merged$AdjustQ_Inst = pmax(merged$AdjustQ_Inst, 0)

  data.frame(datetime = merged$datetime, flow_cfs = merged$AdjustQ_Inst)
}


# %%
# Internal: daily volume matching (iterative)

.adjustq_daily = function(working, obs_daily, error_tol, max_iterations, dt_hours = 6L) {
  n = nrow(working)
  if (n == 0L || max_iterations <= 0L) {
    return(working)
  }

  dt_sec = dt_hours * 3600
  k = 24L %/% dt_hours
  t_sec = as.numeric(working$datetime)
  hours = as.integer(format(working$datetime, "%H"))
  dates = as.Date(working$datetime)

  # Daily (24-hour) timestep or phase-shifted subdaily grid without 00:00 boundaries
  if (dt_hours == 24L || !any(hours == 0L)) {
    day_counts = table(dates)
    full_date_strs = names(day_counts[day_counts == k])
    if (length(full_date_strs) == 0L) {
      return(working)
    }
    full_dates = as.Date(full_date_strs)
    obs_matched = obs_daily$flow_cfs[match(full_dates, obs_daily$date)]
    day_idx = match(dates, full_dates)

    iter = 1L
    max_error = error_tol + 1
    while (iter <= max_iterations && max_error > error_tol) {
      daily_sim = vapply(
        seq_along(full_dates),
        \(m) mean(working$AdjustQ_Inst[day_idx == m], na.rm = FALSE),
        numeric(1L)
      )
      ratio = ifelse(!is.na(daily_sim) & daily_sim > 0, obs_matched / daily_sim, NA_real_)
      pbias = ifelse(!is.na(daily_sim) & daily_sim > 0, abs((daily_sim - obs_matched) / daily_sim), NA_real_)
      if (all(is.na(ratio))) {
        break
      }
      row_ratios = ratio[day_idx]
      valid_r = !is.na(row_ratios)
      working$AdjustQ_Inst[valid_r] = working$AdjustQ_Inst[valid_r] * row_ratios[valid_r]
      max_error = if (all(is.na(pbias))) 0 else max(pbias, na.rm = TRUE)
      iter = iter + 1L
    }
    return(working)
  }

  # Subdaily midnight-aligned grid (k >= 2: dt_hours in 1, 2, 3, 4, 6, 8, 12)
  idx_00 = which(hours == 0L)
  has_span = idx_00 + k <= n & (t_sec[pmin(idx_00 + k, n)] - t_sec[idx_00] == 86400)
  cand_i0 = idx_00[has_span]
  if (length(cand_i0) == 0L) {
    return(working)
  }

  cand_dates = dates[cand_i0]
  obs_matched = obs_daily$flow_cfs[match(cand_dates, obs_daily$date)]
  interior_date_idx = match(dates, cand_dates)
  prev_date_idx = match(dates - 1L, cand_dates)

  has_left_step = c(FALSE, diff(t_sec) == dt_sec)
  has_right_step = c(diff(t_sec) == dt_sec, FALSE)
  is_midnight = hours == 0L

  iter = 1L
  max_error = error_tol + 1

  while (iter <= max_iterations && max_error > error_tol) {
    q = working$AdjustQ_Inst
    daily_sim = vapply(
      cand_i0,
      \(i0) {
        seg = q[i0:(i0 + k)]
        if (anyNA(seg)) {
          NA_real_
        } else {
          (0.5 * seg[1L] + sum(seg[2L:k]) + 0.5 * seg[k + 1L]) / k
        }
      },
      numeric(1L)
    )

    valid_sim = !is.na(daily_sim) & daily_sim > 0 & !is.na(obs_matched)
    if (!any(valid_sim)) {
      break
    }

    day_ratio = rep(NA_real_, length(cand_i0))
    day_pbias = rep(NA_real_, length(cand_i0))
    day_ratio[valid_sim] = obs_matched[valid_sim] / daily_sim[valid_sim]
    day_pbias[valid_sim] = abs((daily_sim[valid_sim] - obs_matched[valid_sim]) / daily_sim[valid_sim])

    ratios = rep(NA_real_, n)
    # Interior hours (0 < hour < 24) receive their calendar day's ratio
    int_mask = !is_midnight & !is.na(interior_date_idx)
    ratios[int_mask] = day_ratio[interior_date_idx[int_mask]]

    # Midnight boundaries (hour == 0) average adjacent day ratios when contiguous
    r_prev = ifelse(
      has_left_step[is_midnight] & !is.na(prev_date_idx[is_midnight]),
      day_ratio[prev_date_idx[is_midnight]],
      NA_real_
    )
    r_curr = ifelse(
      has_right_step[is_midnight] & !is.na(interior_date_idx[is_midnight]),
      day_ratio[interior_date_idx[is_midnight]],
      NA_real_
    )
    ratios[is_midnight] = ifelse(
      !is.na(r_prev) & !is.na(r_curr),
      0.5 * (r_prev + r_curr),
      ifelse(!is.na(r_prev), r_prev, r_curr)
    )

    valid_ratio = !is.na(ratios)
    working$AdjustQ_Inst[valid_ratio] = working$AdjustQ_Inst[valid_ratio] * ratios[valid_ratio]

    max_error = if (all(is.na(day_pbias))) 0 else max(day_pbias, na.rm = TRUE)
    iter = iter + 1L
  }

  working
}
