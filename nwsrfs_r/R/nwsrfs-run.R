# %%
# High-level orchestration for NWSRFS model chain
# Mirrors Python's NwsrfsRun class

#' Run the full NWSRFS model chain
#'
#' Reads parameter, forcing, flow, and upflow CSVs from an NWRFC autocalibration
#' directory, auto-detects which model components are present, and runs the full
#' chain: FA -> SAC-SMA/SNOW17 -> UH -> Lag-K -> Chanloss -> Consuse.
#'
#' @param autocalb_dir Path to an NWRFC autocalibration directory
#' @param run_dir Name of results subdirectory (e.g. "results_por_02"). If NULL,
#'   uses the first results_* directory found.
#' @param forcing_adj Logical; apply monthly climatological forcing adjustments.
#'   Default TRUE.
#' @param shift_sf Logical; shift UH-derived streamflow forward one timestep
#'   (required for NWRFC calibrations). Default TRUE.
#'
#' @return A list with class "nwsrfs_run" containing model results and metadata.
#'   See Details.
#'
#' @details The returned list contains:
#' \describe{
#'   \item{sim}{Numeric vector of simulated flow (cfs)}
#'   \item{sacsnow_sf}{Numeric vector of SAC-SMA/SNOW17/UH flow per zone (cfs), or NULL}
#'   \item{sacsnow_tci}{Matrix of total channel inflow per zone (mm), or NULL}
#'   \item{lagk_route}{Numeric vector of Lag-K routed flow (cfs), or NULL}
#'   \item{forcings}{List of adjusted forcing data.frames, or NULL}
#'   \item{pars}{Parameter data.frame}
#'   \item{forcing_raw}{List of raw (unadjusted) forcing data.frames}
#'   \item{upflow}{List of upstream flow data.frames, or NULL}
#'   \item{daily_flow}{Daily observed flow data.frame}
#'   \item{inst_flow}{Instantaneous observed flow data.frame, or NULL}
#'   \item{dt_hours}{Model timestep in hours}
#'   \item{zone_names}{Character vector of zone names}
#'   \item{upflow_names}{Character vector of upstream reach names, or NULL}
#'   \item{localflow_logic}{Logical; SAC-SMA/SNOW17/UH present}
#'   \item{upflow_logic}{Logical; Lag-K routing present}
#'   \item{chanloss_logic}{Logical; chanloss present}
#'   \item{consuse_logic}{Logical; consuse present}
#' }
#'
#' @importFrom utils read.csv
#' @export
nwsrfs_run = function(autocalb_dir, run_dir = NULL, forcing_adj = TRUE, shift_sf = TRUE) {
  # Validate directory
  if (!dir.exists(autocalb_dir)) {
    stop(autocalb_dir, " is not a directory.")
  }

  # Find run_dir if not specified
  if (is.null(run_dir)) {
    dirs = list.dirs(autocalb_dir, recursive = FALSE, full.names = FALSE)
    results_dirs = sort(dirs[grepl("^results", dirs)])
    if (length(results_dirs) == 0) {
      stop(autocalb_dir, " contains no results_* folder.")
    }
    run_dir = results_dirs[1]
    message("Defaulting to using the following results directory: ", run_dir)
  }

  results_path = file.path(autocalb_dir, run_dir)
  if (!dir.exists(results_path)) {
    stop(results_path, " is not a directory.")
  }

  # Read pars
  pars_path = file.path(results_path, "pars_optimal.csv")
  if (!file.exists(pars_path)) {
    stop(pars_path, " is missing")
  }
  pars = read.csv(pars_path, stringsAsFactors = FALSE)

  # Read forcings
  forcing_files = sort(list.files(
    autocalb_dir,
    pattern = "^forcing_por_.*\\.csv$",
    full.names = TRUE
  ))
  if (length(forcing_files) == 0) {
    stop("POR forcing csv files are missing.")
  }
  forcing_raw = lapply(forcing_files, read.csv)
  # Name by zone extracted from filename
  zone_from_file = sub("forcing_por_", "", sub("\\.csv$", "", basename(forcing_files)))
  names(forcing_raw) = zone_from_file

  # Read daily flow
  daily_flow_files = list.files(autocalb_dir, pattern = "^flow_daily_.*\\.csv$", full.names = TRUE)
  if (length(daily_flow_files) != 1) {
    stop("Expected exactly one flow_daily_*.csv file, found ", length(daily_flow_files))
  }
  daily_flow = read.csv(daily_flow_files[1], stringsAsFactors = FALSE)

  # Read instantaneous flow (optional)
  inst_flow_files = list.files(
    autocalb_dir,
    pattern = "^flow_instantaneous_.*\\.csv$",
    full.names = TRUE
  )
  inst_flow = if (length(inst_flow_files) == 1) {
    read.csv(inst_flow_files[1], stringsAsFactors = FALSE)
  } else {
    NULL
  }

  # Read upflow files (optional)
  upflow_files = sort(list.files(autocalb_dir, pattern = "^upflow_.*\\.csv$", full.names = TRUE))
  upflow = if (length(upflow_files) > 0) {
    uf = lapply(upflow_files, read.csv)
    names(uf) = sub("upflow_", "", sub("\\.csv$", "", basename(upflow_files)))
    uf
  } else {
    NULL
  }

  # Run model chain with loaded data
  .run_model_chain(
    pars = pars,
    forcing_raw = forcing_raw,
    upflow = upflow,
    daily_flow = daily_flow,
    inst_flow = inst_flow,
    forcing_adj = forcing_adj,
    shift_sf = shift_sf
  )
}


#' Load and run a bundled example dataset
#'
#' Loads NRKW1 or SFLN2 bundled example data and runs the full NWSRFS model chain.
#'
#' @param lid Station identifier: "NRKW1" or "SFLN2"
#' @param forcing_adj Logical; apply forcing adjustments. Default TRUE.
#' @param shift_sf Logical; shift UH flow forward one timestep. Default TRUE.
#'
#' @return A list with class "nwsrfs_run" (see [nwsrfs_run()])
#' @export
#'
#' @examples
#' \donttest{
#' run = load_example("NRKW1")
#' plot(run$sim, type = "l")
#' }
load_example = function(lid = "NRKW1", forcing_adj = TRUE, shift_sf = TRUE) {
  lid = toupper(lid)

  if (lid == "NRKW1") {
    pars = nrkw1_pars
    # Separate CU forcing from local forcing
    forcing_raw = nrkw1_forcing
    upflow = nrkw1_upflow
    daily_flow = nrkw1_daily_flow
    inst_flow = nrkw1_inst_flow
  } else if (lid == "SFLN2") {
    pars = sfln2_pars
    forcing_raw = sfln2_forcing
    upflow = NULL
    daily_flow = sfln2_daily_flow
    inst_flow = sfln2_inst_flow
  } else {
    stop("Station ID '", lid, "' not recognized. Available: NRKW1, SFLN2")
  }

  .run_model_chain(
    pars = pars,
    forcing_raw = forcing_raw,
    upflow = upflow,
    daily_flow = daily_flow,
    inst_flow = inst_flow,
    forcing_adj = forcing_adj,
    shift_sf = shift_sf
  )
}


#' Update parameters and re-run model chain
#'
#' @param run An "nwsrfs_run" object from [nwsrfs_run()] or [load_example()]
#' @param new_pars A data.frame with columns "p_name" and "value" containing
#'   parameters to update. All p_name values must exist in the current pars.
#'
#' @return A new "nwsrfs_run" object with updated parameters
#' @export
update_pars = function(run, new_pars) {
  if (!inherits(run, "nwsrfs_run")) {
    stop("'run' must be an nwsrfs_run object")
  }
  if (!all(c("p_name", "value") %in% names(new_pars))) {
    stop("new_pars must have 'p_name' and 'value' columns")
  }
  if (!all(new_pars$p_name %in% run$pars$p_name)) {
    stop("All p_name values in new_pars must exist in the current pars")
  }

  pars = run$pars
  # Update matching rows
  for (i in seq_len(nrow(new_pars))) {
    idx = which(pars$p_name == new_pars$p_name[i])
    pars$value[idx] = new_pars$value[i]
  }

  .run_model_chain(
    pars = pars,
    forcing_raw = run$forcing_raw,
    upflow = run$upflow,
    daily_flow = run$daily_flow,
    inst_flow = run$inst_flow,
    forcing_adj = run$.forcing_adj,
    shift_sf = run$.shift_sf
  )
}


# %%
# Internal: core model chain

.run_model_chain = function(
  pars,
  forcing_raw,
  upflow,
  daily_flow,
  inst_flow,
  forcing_adj = TRUE,
  shift_sf = TRUE
) {
  dt_hours = 6L

  # --- Interrogate pars to detect model components ---
  pars = as.data.frame(pars)

  # Zone names: ALL zones with "-" in name (including CU zones)
  # Python's _interrogate_pars: pars.zone.str.contains('-')
  all_zones = sort(unique(pars$zone[grepl("-", pars$zone)]))
  zone_names = all_zones
  n_zones = length(zone_names)
  localflow_logic = n_zones > 0

  # Upflow / Lag-K logic
  upflow_logic = !is.null(upflow) && length(upflow) > 0
  upflow_names = if (upflow_logic) {
    sort(unique(pars$zone[pars$type == "lagk"]))
  } else {
    NULL
  }

  # Chanloss logic: present only if n_clmods > 0
  n_clmods_row = pars[pars$name == "n_clmods", ]
  chanloss_logic = nrow(n_clmods_row) > 0 && n_clmods_row$value[1] > 0

  # Consuse logic: zones ending in "-CU"
  cu_zones = sort(all_zones[grepl("-CU$", all_zones)])
  consuse_logic = length(cu_zones) > 0

  # All forcing zones (ALL zone_names that have matching forcing data)
  forcing_local = if (localflow_logic) {
    forcing_raw[zone_names[zone_names %in% names(forcing_raw)]]
  } else {
    NULL
  }

  sim_length = if (localflow_logic) {
    nrow(forcing_local[[1]])
  } else if (upflow_logic) {
    nrow(upflow[[1]])
  } else {
    stop("No forcing or upflow data available")
  }

  # --- Initialize simulation ---
  sim_flow = numeric(sim_length)

  # --- Forcing adjustments + SAC-SMA/SNOW17 + UH ---
  sacsnow_sf = NULL
  sacsnow_tci = NULL
  forcings_adj = NULL

  if (localflow_logic) {
    # Run forcing adjustments (all zones including CU)
    forcings_adj = fa_nwrfc(dt_hours, forcing_local, pars)

    # Run SAC-SMA / SNOW17
    sacsnow_tci = sac_snow(dt_hours, forcings_adj, pars)

    # Route through UH
    sacsnow_sf = uh(
      dt_hours,
      sacsnow_tci,
      pars,
      sum_zones = TRUE,
      start_of_timestep = shift_sf,
      backfill = TRUE
    )

    sim_flow = sim_flow + sacsnow_sf
  }

  # --- Lag-K routing ---
  lagk_route = NULL
  if (upflow_logic) {
    lagk_route = lagk(dt_hours, upflow, pars, sum_routes = TRUE)
    sim_flow = sim_flow + lagk_route
  }

  # --- Chanloss ---
  if (chanloss_logic) {
    sim_flow = chanloss(sim_flow, forcing_raw[zone_names[1]], dt_hours, pars)
  }

  # --- Consuse ---
  if (consuse_logic) {
    # Convert 6hr instantaneous sim to period average then to daily mean
    # Python: inst_to_ave then resample('1D').mean()
    sim_peravg = (sim_flow + c(sim_flow[-1], sim_flow[length(sim_flow)])) / 2
    ts_ref = forcing_local[[1]]
    day_idx = .make_date_index(ts_ref)
    daily_sim = tapply(sim_peravg, day_idx, mean)
    daily_dates = as.Date(names(daily_sim))

    # Get PET from FA-adjusted forcings for each CU zone
    # Python: pet_shift = self.forcings['pet'].shift(periods=-1, freq='h')
    # then pet_daily = pet_shift.resample('1D').sum()
    # CU zone PET is in forcings_adj columns
    # Find which index corresponds to CU zones
    adj_names = names(forcings_adj)
    cu_zone_idx = which(adj_names %in% cu_zones)

    # PET is in pet_mm column after FA
    pet_6hr = forcings_adj[[cu_zone_idx[1]]]$pet_mm
    # Shift back 1 timestep to match CHPS convention (00:00 goes with previous day)
    pet_shifted = c(pet_6hr[-1], pet_6hr[length(pet_6hr)])
    daily_pet = tapply(pet_shifted, day_idx, sum)

    # Build consuse input
    cu_input = data.frame(
      year = as.integer(format(daily_dates, "%Y")),
      month = as.integer(format(daily_dates, "%m")),
      day = as.integer(format(daily_dates, "%d")),
      pet = as.numeric(daily_pet),
      flow = as.numeric(daily_sim)
    )

    # Handle min_flow naming difference (min_flow vs min_flow_cmsd)
    cu_pars = pars
    if (any(cu_pars$name == "min_flow" & cu_pars$type == "consuse")) {
      cu_pars$name[cu_pars$name == "min_flow" & cu_pars$type == "consuse"] = "min_flow_cmsd"
    }

    cu_result = consuse(cu_input, cu_pars, cfs = TRUE)

    # Get daily diversions: sum(qdiv) - sum(qrfout)
    if (is.data.frame(cu_result)) {
      cu_adj_daily = cu_result$qdiv - cu_result$qrfout
    } else {
      cu_adj_daily = Reduce("+", lapply(cu_result, \(x) x$qdiv - x$qrfout))
    }

    # Map daily adjustment back to 6hr timestep
    # Python pattern: shift daily +1 day, reindex to 6hr with bfill.
    # Result: 00:00 gets previous day's adj, 06:00-18:00 get current day's adj.
    n_per_day = 24L / dt_hours
    n_days = length(cu_adj_daily)
    cu_adj_6hr = numeric(sim_length)
    for (d in seq_len(n_days)) {
      start_i = (d - 1L) * n_per_day + 1L
      if (d == 1L) {
        # First day: 00:00 = 0 (no previous day), 06:00-18:00 = day 1 adj
        cu_adj_6hr[start_i] = cu_adj_daily[1]
        if (n_per_day > 1 && start_i + 1 <= sim_length) {
          end_i = min(start_i + n_per_day - 1L, sim_length)
          cu_adj_6hr[(start_i + 1):end_i] = cu_adj_daily[1]
        }
      } else {
        # 00:00 = previous day's adj, 06:00-18:00 = current day's adj
        if (start_i <= sim_length) {
          cu_adj_6hr[start_i] = cu_adj_daily[d - 1L]
        }
        if (n_per_day > 1 && start_i + 1 <= sim_length) {
          end_i = min(start_i + n_per_day - 1L, sim_length)
          cu_adj_6hr[(start_i + 1):end_i] = cu_adj_daily[d]
        }
      }
    }
    # Fill any remaining timesteps beyond the daily data with last value
    filled = n_days * n_per_day
    if (filled < sim_length) {
      cu_adj_6hr[(filled + 1):sim_length] = cu_adj_daily[n_days]
    }

    sim_flow = pmax(sim_flow - cu_adj_6hr, 0)
  }

  # --- Build result ---
  result = list(
    sim = sim_flow,
    sacsnow_sf = sacsnow_sf,
    sacsnow_tci = sacsnow_tci,
    lagk_route = lagk_route,
    forcings = forcings_adj,
    pars = pars,
    forcing_raw = forcing_raw,
    upflow = upflow,
    daily_flow = daily_flow,
    inst_flow = inst_flow,
    dt_hours = dt_hours,
    zone_names = zone_names,
    upflow_names = upflow_names,
    localflow_logic = localflow_logic,
    upflow_logic = upflow_logic,
    chanloss_logic = chanloss_logic,
    consuse_logic = consuse_logic,
    .forcing_adj = forcing_adj,
    .shift_sf = shift_sf
  )
  class(result) = "nwsrfs_run"
  result
}


# %%
# Internal: helper to make date index from a forcing data.frame

.make_date_index = function(df) {
  as.Date(paste(df$year, df$month, df$day, sep = "-"))
}


#' Print method for nwsrfs_run objects
#'
#' @param x An nwsrfs_run object
#' @param ... Ignored
#' @return x (invisibly)
#' @export
print.nwsrfs_run = function(x, ...) {
  cat("NWSRFS Model Run\n")
  cat("  Zones:    ", paste(x$zone_names, collapse = ", "), "\n")
  cat("  Timestep: ", x$dt_hours, "hours\n")
  cat("  Length:   ", length(x$sim), "timesteps\n")
  cat("  Components:\n")
  cat("    SAC-SMA/SNOW17/UH:", x$localflow_logic, "\n")
  cat("    Lag-K:            ", x$upflow_logic, "\n")
  cat("    Chanloss:         ", x$chanloss_logic, "\n")
  cat("    Consuse:          ", x$consuse_logic, "\n")
  invisible(x)
}
