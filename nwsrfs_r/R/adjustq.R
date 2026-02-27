# %%
# AdjustQ: Replicates the CHPS FEWS AdjustQ tool
# Used as preprocessing step to create upstream flow for Lag-K reaches

#' AdjustQ: Create upstream flow timeseries from observations and simulation
#'
#' Replicates the CHPS FEWS AdjustQ tool. Adjusts observed mean daily discharges
#' using observed instantaneous and simulated discharges. If both observed mean
#' daily and instantaneous data are missing, simulated discharge is used.
#'
#' @param daily_flow Data.frame with columns year, month, day, flow_cfs (daily obs)
#' @param inst_flow Data.frame with columns year, month, day, hour, flow_cfs (instantaneous obs)
#' @param sim Data.frame with columns year, month, day, hour, flow_cfs
#'   (simulated 6hr flow), or NULL. If provided, small/large gap filling uses
#'   simulation as fallback.
#' @param blend Integer; threshold for small vs large gap in timesteps. Default 10.
#' @param interp_type "ratio" or "difference". Default "ratio".
#' @param error_tol Numeric; daily volume matching tolerance (fraction). Default 0.01.
#' @param max_iterations Integer; max daily correction iterations. Default 15.
#'
#' @return A data.frame with columns: datetime (POSIXct), flow_cfs (adjusted flow)
#' @importFrom stats approx
#' @export
adjustq = function(daily_flow, inst_flow, sim = NULL,
                   blend = 10L, interp_type = "ratio",
                   error_tol = 0.01, max_iterations = 15L) {

  blend = as.integer(blend)
  max_iterations = as.integer(max_iterations)
  if (!interp_type %in% c("ratio", "difference")) {
    stop("interp_type must be 'ratio' or 'difference'")
  }

  # --- Build datetime indexes ---
  obs_daily_dates = as.Date(paste(daily_flow$year, daily_flow$month, daily_flow$day, sep = "-"))
  obs_daily = data.frame(date = obs_daily_dates, flow_cfs = daily_flow$flow_cfs)

  inst_dt = as.POSIXct(
    paste0(inst_flow$year, "-", sprintf("%02d", inst_flow$month), "-",
           sprintf("%02d", inst_flow$day), " ", sprintf("%02d", inst_flow$hour), ":00:00"),
    tz = "UTC"
  )
  obs_inst = data.frame(datetime = inst_dt, flow_cfs = inst_flow$flow_cfs)

  if (!is.null(sim)) {
    sim_dt = as.POSIXct(
      paste0(sim$year, "-", sprintf("%02d", sim$month), "-",
             sprintf("%02d", sim$day), " ", sprintf("%02d", sim$hour), ":00:00"),
      tz = "UTC"
    )
    sim_df = data.frame(datetime = sim_dt, simulated = sim$flow_cfs)
    result = .adjustq_with_sim(obs_daily, obs_inst, sim_df, blend, interp_type,
                               error_tol, max_iterations)
  } else {
    result = .inst_mean_q_merge_internal(obs_daily, obs_inst, error_tol, max_iterations)
  }

  result
}


#' Load AdjustQ example
#'
#' Runs AdjustQ using bundled NRKW1 data.
#'
#' @param sim Logical; include simulation in AdjustQ. Default TRUE.
#' @param ... Additional arguments passed to [adjustq()]
#'
#' @return A data.frame (see [adjustq()])
#' @export
adjustq_load_example = function(sim = TRUE, ...) {
  if (sim) {
    # Run the full NRKW1 simulation to get sim flow
    run = load_example("NRKW1")
    sim_df = data.frame(
      run$forcings[[1]][, c("year", "month", "day", "hour")],
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
# Internal: AdjustQ with simulation

.adjustq_with_sim = function(obs_daily, obs_inst, sim_df, blend, interp_type,
                             error_tol, max_iterations) {

  # Snap instantaneous obs to nearest 6hr grid (within 15min)
  obs_begin = as.POSIXct(format(min(obs_inst$datetime), "%Y-%m-%d"), tz = "UTC")
  obs_end = as.POSIXct(format(max(obs_inst$datetime) + 86400, "%Y-%m-%d"), tz = "UTC")
  grid_6h = seq(obs_begin, obs_end, by = "6 hours")

  obs_6h = data.frame(datetime = grid_6h, observed = NA_real_)
  for (i in seq_along(grid_6h)) {
    diffs = abs(as.numeric(difftime(obs_inst$datetime, grid_6h[i], units = "mins")))
    closest = which.min(diffs)
    if (diffs[closest] <= 15 && !is.na(obs_inst$flow_cfs[closest]) && obs_inst$flow_cfs[closest] > 0) {
      obs_6h$observed[i] = obs_inst$flow_cfs[closest]
    }
  }

  # Extend simulated data to span complete days
  sim_begin = as.POSIXct(format(min(sim_df$datetime), "%Y-%m-%d"), tz = "UTC")
  sim_end = as.POSIXct(format(max(sim_df$datetime) + 86400, "%Y-%m-%d"), tz = "UTC")
  sim_grid = seq(sim_begin, sim_end, by = "6 hours")
  sim_ext = data.frame(datetime = sim_grid)
  sim_ext = merge(sim_ext, sim_df, by = "datetime", all.x = TRUE)
  # Interpolate to fill edges
  sim_ext$simulated = approx(
    x = which(!is.na(sim_ext$simulated)),
    y = sim_ext$simulated[!is.na(sim_ext$simulated)],
    xout = seq_len(nrow(sim_ext)),
    rule = 2
  )$y

  # Merge obs and sim
  working = merge(obs_6h, sim_ext, by = "datetime", all = TRUE)
  working = working[order(working$datetime), ]

  # Compute ratio and difference
  working$Inst_Ratio = working$observed / working$simulated
  working$Inst_Difference = working$observed - working$simulated
  working$AdjustQ_Inst = working$observed

  # --- Find gaps in observed period ---
  has_obs = which(!is.na(working$observed))
  if (length(has_obs) > 1) {
    gap_periods = diff(has_obs)
    gap_end_idx = has_obs[-1][gap_periods > 1]
    gap_sizes = gap_periods[gap_periods > 1]

    if (length(gap_end_idx) > 0) {
      small_gaps = which(gap_sizes < blend)
      large_gaps = which(gap_sizes >= blend)

      # Fill small gaps
      if (length(small_gaps) > 0) {
        for (g in small_gaps) {
          end_i = gap_end_idx[g]
          period = gap_sizes[g]
          start_i = end_i - period

          interp_range = start_i:end_i
          start_ratio = working$Inst_Ratio[start_i]
          end_ratio = working$Inst_Ratio[end_i]

          ratio_check1 = abs(start_ratio - end_ratio) > 2
          ratio_check2 = max(start_ratio, end_ratio, na.rm = TRUE) > 5

          if (interp_type == "difference" || isTRUE(ratio_check1) || isTRUE(ratio_check2)) {
            # Difference interpolation
            diffs_interp = approx(
              x = c(1, length(interp_range)),
              y = working$Inst_Difference[c(start_i, end_i)],
              xout = seq_along(interp_range)
            )$y
            working$AdjustQ_Inst[interp_range] = working$simulated[interp_range] + diffs_interp
          } else {
            # Ratio interpolation
            ratios_interp = approx(
              x = c(1, length(interp_range)),
              y = working$Inst_Ratio[c(start_i, end_i)],
              xout = seq_along(interp_range)
            )$y
            working$AdjustQ_Inst[interp_range] = working$simulated[interp_range] * ratios_interp
          }
        }
      }

      # Fill large gaps
      if (length(large_gaps) > 0) {
        for (g in large_gaps) {
          end_i = gap_end_idx[g]
          period = gap_sizes[g]
          start_i = end_i - period

          # Back end blend
          end_blend_start = max(end_i - blend, 1)
          end_blend_range = end_blend_start:end_i
          end_diff = working$Inst_Difference[end_i]
          if (!is.na(end_diff)) {
            blend_frac = seq(0, 1, length.out = length(end_blend_range))
            working$AdjustQ_Inst[end_blend_range] =
              working$simulated[end_blend_range] + blend_frac * end_diff
          }

          # Front end blend
          start_blend_end = min(start_i + blend, nrow(working))
          start_blend_range = start_i:start_blend_end
          start_diff = working$Inst_Difference[start_i]
          if (!is.na(start_diff)) {
            blend_frac = seq(1, 0, length.out = length(start_blend_range))
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
  working$AdjustQ_Inst[is.na(working$AdjustQ_Inst)] =
    working$simulated[is.na(working$AdjustQ_Inst)]

  # --- Daily volume correction ---
  working = .adjustq_daily(working, obs_daily, error_tol, max_iterations)

  # Clip negatives
  working$AdjustQ_Inst = pmax(working$AdjustQ_Inst, 0)

  data.frame(datetime = working$datetime, flow_cfs = working$AdjustQ_Inst)
}


# %%
# Internal: inst_mean_q_merge (no simulation)

.inst_mean_q_merge_internal = function(obs_daily, obs_inst, error_tol, max_iterations) {

  # Resample daily to 6hr with forward fill, shifted by 6hr
  daily_6h_dates = seq(min(obs_daily$date), max(obs_daily$date), by = "day")
  daily_6h_list = lapply(daily_6h_dates, \(d) {
    flow = obs_daily$flow_cfs[obs_daily$date == d]
    if (length(flow) == 0) flow = NA_real_
    # Create 4 timesteps: 06:00, 12:00, 18:00, 00:00+1day (shifted by 6hr)
    dts = as.POSIXct(d, tz = "UTC") + c(6, 12, 18, 24) * 3600
    data.frame(datetime = dts, daily_flow = flow)
  })
  daily_6h = do.call(rbind, daily_6h_list)

  # Snap instantaneous to 6hr grid
  grid_begin = min(daily_6h$datetime)
  grid_end = max(daily_6h$datetime)
  grid_6h = seq(grid_begin, grid_end, by = "6 hours")
  inst_6h = data.frame(datetime = grid_6h, inst_flow = NA_real_)
  for (i in seq_along(grid_6h)) {
    diffs = abs(as.numeric(difftime(obs_inst$datetime, grid_6h[i], units = "mins")))
    closest = which.min(diffs)
    if (length(closest) > 0 && diffs[closest] <= 15 &&
        !is.na(obs_inst$flow_cfs[closest]) && obs_inst$flow_cfs[closest] > 0) {
      inst_6h$inst_flow[i] = obs_inst$flow_cfs[closest]
    }
  }

  # Merge: use inst where available, daily otherwise
  merged = merge(inst_6h, daily_6h, by = "datetime", all.x = TRUE)
  merged = merged[order(merged$datetime), ]
  merged$AdjustQ_Inst = ifelse(!is.na(merged$inst_flow), merged$inst_flow, merged$daily_flow)

  # Remove rows with no data
  merged = merged[!is.na(merged$AdjustQ_Inst), ]

  # Daily volume correction
  merged = .adjustq_daily(merged, obs_daily, error_tol, max_iterations)

  # Clip negatives
  merged$AdjustQ_Inst = pmax(merged$AdjustQ_Inst, 0)

  data.frame(datetime = merged$datetime, flow_cfs = merged$AdjustQ_Inst)
}


# %%
# Internal: daily volume matching (iterative)

.adjustq_daily = function(working, obs_daily, error_tol, max_iterations) {

  iter = 1L
  max_error = error_tol + 1

  while (iter <= max_iterations && max_error > error_tol) {
    # Compute daily average using weighted rolling window
    # Python: (rolling(5,center=True).sum() + rolling(3,center=True).sum()) / 8
    n = nrow(working)
    daily_avg = numeric(n)
    for (i in seq_len(n)) {
      # rolling(5, center=TRUE) = i-2 to i+2
      r5_idx = max(1, i - 2):min(n, i + 2)
      r3_idx = max(1, i - 1):min(n, i + 1)
      if (length(r5_idx) == 5 && length(r3_idx) == 3) {
        daily_avg[i] = (sum(working$AdjustQ_Inst[r5_idx]) + sum(working$AdjustQ_Inst[r3_idx])) / 8
      } else {
        daily_avg[i] = NA_real_
      }
    }

    # Extract 12:00 values (hour == 12)
    hours = as.integer(format(working$datetime, "%H"))
    is_12z = hours == 12 & !is.na(daily_avg)

    if (sum(is_12z) == 0) break

    # Build daily ratio table
    daily_dates_12z = as.Date(working$datetime[is_12z])
    daily_sim_12z = daily_avg[is_12z]

    daily_ratio_df = data.frame(
      date = daily_dates_12z,
      daily_sim = daily_sim_12z
    )
    daily_ratio_df = merge(daily_ratio_df, obs_daily, by = "date", all.x = TRUE)
    daily_ratio_df$ratio = daily_ratio_df$flow_cfs / daily_ratio_df$daily_sim
    daily_ratio_df$pbias = abs((daily_ratio_df$daily_sim - daily_ratio_df$flow_cfs) / daily_ratio_df$daily_sim)

    # Map daily ratio to 6hr timesteps using Python's two-step interpolation:
    # 1. Assign ratio at 12:00 timestamps
    # 2. Nearest-neighbor limit=1 -> fills 06:00 and 18:00 adjacent to each 12:00
    # 3. Linear limit=2 -> fills 00:00 as average of neighbors
    ratios = rep(NA_real_, n)

    # Step 1: assign at 12:00
    ratio_lookup = daily_ratio_df$ratio
    names(ratio_lookup) = as.character(daily_ratio_df$date)
    for (i in which(is_12z)) {
      d = as.character(as.Date(working$datetime[i]))
      if (d %in% names(ratio_lookup)) {
        ratios[i] = ratio_lookup[d]
      }
    }

    known = which(!is.na(ratios))
    if (length(known) < 1) break

    # Step 2: nearest with limit=1 (fill 1 step in each direction)
    for (k in known) {
      if (k > 1 && is.na(ratios[k - 1])) ratios[k - 1] = ratios[k]
      if (k < n && is.na(ratios[k + 1])) ratios[k + 1] = ratios[k]
    }

    # Step 3: linear with limit=2 (fill remaining NAs from neighbors)
    still_na = which(is.na(ratios))
    for (j in still_na) {
      left = if (j > 1 && !is.na(ratios[j - 1])) ratios[j - 1] else NA_real_
      right = if (j < n && !is.na(ratios[j + 1])) ratios[j + 1] else NA_real_
      if (!is.na(left) && !is.na(right)) {
        ratios[j] = (left + right) / 2
      } else if (!is.na(left)) {
        ratios[j] = left
      } else if (!is.na(right)) {
        ratios[j] = right
      }
    }

    # Apply ratio
    valid_ratio = !is.na(ratios)
    working$AdjustQ_Inst[valid_ratio] =
      working$AdjustQ_Inst[valid_ratio] * ratios[valid_ratio]

    max_error = max(daily_ratio_df$pbias, na.rm = TRUE)
    iter = iter + 1L
  }

  working
}
