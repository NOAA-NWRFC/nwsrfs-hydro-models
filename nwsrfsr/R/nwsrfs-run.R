# %%
# High-level orchestration for NWSRFS model chain
# Mirrors Python's NwsrfsRun class

#' Run the full NWSRFS model chain
#'
#' Reads parameter, forcing, flow, upstream flow, and optional area-elevation
#' CSVs from an NWRFC autocalibration directory, auto-detects which model
#' components are present, infers the simulation timestep, and runs the full
#' chain: `(RSnwElev) -> FA -> SAC-SMA/SNOW17 -> UH -> Lag-K -> Chanloss -> Consuse`.
#'
#' @param autocalb_dir Path to an NWRFC autocalibration directory
#' @param run_dir Name of results subdirectory (e.g. `"results_por_02"`). If `NULL`,
#'   uses the first `results_*` directory found.
#' @param forcing_adj Apply monthly climatological forcing adjustments. `TRUE`
#'   adjusts all forcings, `FALSE` adjusts none, and a character vector naming a
#'   subset of `"map"`, `"mat"`, `"ptps"` and `"pet"` adjusts only those.
#'   When `ptps` is derived via [rsnwelev()] from an `area_elev*` CSV, `"ptps"`
#'   is automatically excluded from climatological forcing adjustments.
#'   Default `TRUE`.
#' @param shift_sf Logical; shift UH-derived streamflow forward one timestep
#'   (required for NWRFC calibrations). Default `TRUE`.
#' @param return_inst Logical; return instantaneous streamflow rather than
#'   period average. Default `TRUE`.
#'
#' @return A list with class `"nwsrfs_run"` containing model results, lookup
#'   tables, and metadata. See Details.
#'
#' @details
#' ## Precipitation typing (`ptps` vs `area_elev*`)
#' Either every `forcing_por_*.csv` file must include a `ptps` column
#' (`ptps_logic = TRUE`, `area_elev_logic = FALSE`), or none may include `ptps`
#' and a single `area_elev*.csv` file must be present (`ptps_logic = FALSE`,
#' `area_elev_logic = TRUE`), in which case [rsnwelev()] calculates `ptps` from
#' unadjusted air temperature and `pars` (`pxtemp`, `talr` with
#' `type == "rsnwelev"`, and `elev` with `type == "snow"`). Providing both
#' complete `ptps` columns and an `area_elev*.csv` file is rejected as ambiguous.
#'
#' ## Unit hydrograph selection
#' Considering rows in `pars` where `type == "uh"`:
#' - `unit_shape` selects a two-parameter gamma unit hydrograph (`uh_type = "gamma"`).
#' - Consecutive `uh_0`, `uh_1`, ... entries select an ordinate unit hydrograph
#'   (`uh_type = "ordinate"`), in `cfs/in` at `dt_hours`.
#' - Either provide one UH parameter set per SAC-Snow zone, or one shared UH
#'   parameter set across multiple SAC-Snow zones (`shared_uh_logic = TRUE`),
#'   in which case `zone_area` (`type == "uh"`) is still supplied for every
#'   SAC-Snow zone and channel inflow is area-weighted before routing.
#'
#' ## Returned list elements
#' \describe{
#'   \item{sim}{Numeric vector of simulated outlet flow (cfs)}
#'   \item{sacsnow_sf}{Numeric vector of SAC-SMA/SNOW17/UH flow summed across zones (cfs), or `NULL`}
#'   \item{sacsnow_tci}{Matrix of total channel inflow per SAC-Snow zone (mm), or `NULL`}
#'   \item{lagk_route}{Numeric vector of Lag-K routed flow (cfs), or `NULL`}
#'   \item{forcings}{Named list of adjusted forcing `data.frame`s, or `NULL`}
#'   \item{uh}{Unit hydrograph `data.frame` (`hours` and per-UH-zone `cfs/in`, see [uh_ordinates()]), or `NULL`}
#'   \item{area_elev}{Area-elevation `data.frame` (`quantile` and per-zone elevations in m), or `NULL`}
#'   \item{peadj_table}{Monthly SAC/FA crop-coefficient `data.frame` (`month = 1:12` and per-zone `peadj`), or `NULL`}
#'   \item{consuse_peadj_table}{Monthly consumptive-use crop-coefficient `data.frame` (`month = 1:12` and per-CU-zone `peadj_cu`), or `NULL`}
#'   \item{chanloss_table}{Monthly channel-loss interpolation factor `data.frame` (`month = 1:12` and `chanloss_adj`), or `NULL`}
#'   \item{pars}{Parameter `data.frame` (with `n_clmods` bookkeeping row removed)}
#'   \item{forcing_raw}{Named list of raw (unadjusted) forcing `data.frame`s, or `NULL`}
#'   \item{upflow}{Named list of upstream flow `data.frame`s, or `NULL`}
#'   \item{daily_flow}{Daily observed flow `data.frame`}
#'   \item{inst_flow}{Instantaneous observed flow `data.frame`, or `NULL`}
#'   \item{dt_hours}{Model timestep in hours (`1`, `2`, `3`, `4`, `6`, `8`, `12`, or `24`)}
#'   \item{zone_names}{Character vector of SAC-Snow zone names}
#'   \item{uh_type}{`"gamma"`, `"ordinate"`, or `NULL`}
#'   \item{uh_zone_names}{Character vector of UH parameter-set zone names, or `NULL`}
#'   \item{shared_uh_logic}{Logical (`TRUE` when multiple SAC-Snow zones share one UH), or `NULL`}
#'   \item{ptps_logic}{Logical; `TRUE` when `ptps` was supplied in forcing inputs, `FALSE` when calculated via [rsnwelev()]}
#'   \item{area_elev_logic}{Logical; `TRUE` when area-elevation data is used}
#'   \item{upflow_names}{Character vector of upstream reach names, or `NULL`}
#'   \item{localflow_logic}{Logical; SAC-SMA/SNOW17/UH present}
#'   \item{upflow_logic}{Logical; Lag-K routing present}
#'   \item{chanloss_logic}{Logical; chanloss present}
#'   \item{consuse_logic}{Logical; consuse present}
#' }
#'
#' @importFrom utils read.csv
#' @export
nwsrfs_run = function(
  autocalb_dir,
  run_dir = NULL,
  forcing_adj = TRUE,
  shift_sf = TRUE,
  return_inst = TRUE
) {
  if (!dir.exists(autocalb_dir)) {
    stop(autocalb_dir, " is not a directory.", call. = FALSE)
  }

  if (is.null(run_dir)) {
    dirs = list.dirs(autocalb_dir, recursive = FALSE, full.names = FALSE)
    results_dirs = sort(dirs[grepl("^results", dirs)])
    if (length(results_dirs) == 0L) {
      stop(autocalb_dir, " contains no results_* folder.", call. = FALSE)
    }
    run_dir = results_dirs[1L]
    message("Defaulting to using the following results directory: ", run_dir)
  }

  results_path = file.path(autocalb_dir, run_dir)
  if (!dir.exists(results_path)) {
    stop(results_path, " is not a directory.", call. = FALSE)
  }

  pars_path = file.path(results_path, "pars_optimal.csv")
  if (!file.exists(pars_path)) {
    stop(pars_path, " is missing", call. = FALSE)
  }
  pars = read.csv(pars_path, stringsAsFactors = FALSE)

  search_dirs = unique(normalizePath(c(autocalb_dir, results_path), mustWork = TRUE))
  find_csvs = function(pattern) {
    found = unlist(
      lapply(search_dirs, \(d) list.files(d, pattern = pattern, full.names = TRUE)),
      use.names = FALSE
    )
    if (length(found) == 0L) {
      return(character(0))
    }
    found[order(basename(found))]
  }

  forcing_files = find_csvs("^forcing_por_.*\\.csv$")
  if (length(forcing_files) == 0L) {
    stop("POR forcing csv files are missing. Files must start with forcing_por_*.", call. = FALSE)
  }
  forcing_raw = lapply(forcing_files, \(f) read.csv(f, stringsAsFactors = FALSE))
  names(forcing_raw) = sub("^forcing_por_", "", sub("\\.csv$", "", basename(forcing_files)))

  daily_flow_files = find_csvs("^flow_daily_.*\\.csv$")
  if (length(daily_flow_files) == 0L) {
    stop("Daily flow csv file is missing. File must start with flow_daily_*.", call. = FALSE)
  }
  if (length(daily_flow_files) > 1L) {
    stop(
      "Daily flow csv file is ambiguous. Only one csv file must start with flow_daily_*.",
      call. = FALSE
    )
  }
  daily_flow = read.csv(daily_flow_files[1L], stringsAsFactors = FALSE)

  inst_flow_files = find_csvs("^flow_instantaneous_.*\\.csv$")
  if (length(inst_flow_files) > 1L) {
    stop(
      "Instantaneous flow csv file is ambiguous. Only one csv file must start with flow_instantaneous_*.",
      call. = FALSE
    )
  }
  inst_flow = if (length(inst_flow_files) == 1L) {
    read.csv(inst_flow_files[1L], stringsAsFactors = FALSE)
  } else {
    NULL
  }

  area_elev_files = find_csvs("^area_elev.*\\.csv$")
  if (length(area_elev_files) > 1L) {
    stop(
      "Area-elevation csv file is ambiguous. Only one csv file must start with area_elev*.",
      call. = FALSE
    )
  }
  area_elev_logic = length(area_elev_files) == 1L

  ptps_logic = all(vapply(forcing_raw, \(df) "ptps" %in% names(df), logical(1L)))
  area_elev = NULL
  if (ptps_logic && area_elev_logic) {
    stop(
      "Ambiguous precipitation typing inputs: every forcing file contains a ptps column, ",
      "but an area_elev* CSV is also present. Remove the area_elev* CSV to use supplied PTPS, ",
      "or remove the ptps columns from the forcing files to calculate PTPS with RSnwElev.",
      call. = FALSE
    )
  } else if (!ptps_logic) {
    if (!area_elev_logic) {
      stop(
        "Precipitation typing requires a ptps column in every forcing file or a single area_elev* csv file.",
        call. = FALSE
      )
    }
    forcing_raw = lapply(forcing_raw, \(df) {
      df[["ptps"]] = NULL
      df
    })
    ae_raw = read.csv(area_elev_files[1L], stringsAsFactors = FALSE, check.names = FALSE)
    if (ncol(ae_raw) < 2L) {
      stop("Area-elevation CSV must contain a quantile column and at least one zone column.", call. = FALSE)
    }
    names(ae_raw)[1L] = "quantile"
    zone_cols = sort(names(ae_raw)[-1L])
    area_elev = ae_raw[, c("quantile", zone_cols), drop = FALSE]
  }

  upflow_files = find_csvs("^upflow_.*\\.csv$")
  upflow = if (length(upflow_files) > 0L) {
    uf = lapply(upflow_files, \(f) read.csv(f, stringsAsFactors = FALSE))
    names(uf) = sub("^upflow_", "", sub("\\.csv$", "", basename(upflow_files)))
    uf
  } else {
    NULL
  }

  .run_model_chain(
    pars = pars,
    forcing_raw = forcing_raw,
    upflow = upflow,
    daily_flow = daily_flow,
    inst_flow = inst_flow,
    forcing_adj = forcing_adj,
    shift_sf = shift_sf,
    return_inst = return_inst,
    area_elev = area_elev,
    area_elev_logic = area_elev_logic,
    ptps_logic = ptps_logic
  )
}


#' Load and run a bundled example dataset
#'
#' Loads NRKW1 or SFLN2 bundled example data and runs the full NWSRFS model chain.
#'
#' @param lid Station identifier: `"NRKW1"` or `"SFLN2"`
#' @param forcing_adj Apply forcing adjustments. `TRUE` adjusts all forcings,
#'   `FALSE` adjusts none, and a character vector naming a subset of `"map"`, `"mat"`,
#'   `"ptps"` and `"pet"` adjusts only those. Default `TRUE`.
#' @param shift_sf Logical; shift UH flow forward one timestep. Default `TRUE`.
#' @param return_inst Logical; return instantaneous streamflow rather than
#'   period average. Default `TRUE`.
#'
#' @return A list with class `"nwsrfs_run"` (see [nwsrfs_run()])
#' @export
#'
#' @examples
#' run = load_example("NRKW1")
#' plot(run$sim, type = "l")
load_example = function(
  lid = "NRKW1",
  forcing_adj = TRUE,
  shift_sf = TRUE,
  return_inst = TRUE
) {
  lid = toupper(lid)

  if (lid == "NRKW1") {
    pars = nrkw1_pars
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
    stop("Station ID '", lid, "' not recognized. Available: NRKW1, SFLN2", call. = FALSE)
  }

  .run_model_chain(
    pars = pars,
    forcing_raw = forcing_raw,
    upflow = upflow,
    daily_flow = daily_flow,
    inst_flow = inst_flow,
    forcing_adj = forcing_adj,
    shift_sf = shift_sf,
    return_inst = return_inst,
    area_elev = NULL,
    area_elev_logic = FALSE,
    ptps_logic = TRUE
  )
}


#' Update parameters and re-run model chain atomically
#'
#' Updates parameter values in `run$pars` and rebuilds the model chain.
#' `new_pars` must be a `data.frame` containing non-duplicated columns
#' `"p_name"` and `"value"`, with non-missing, unique `"p_name"` entries that
#' already exist in `run$pars$p_name`. Missing (`NA`) entries in `"value"`
#' retain the existing parameter value. If validation fails, `run` is unchanged.
#'
#' @param run An `"nwsrfs_run"` object from [nwsrfs_run()] or [load_example()]
#' @param new_pars A `data.frame` with columns `"p_name"` and `"value"` containing
#'   parameters to update.
#'
#' @return A new `"nwsrfs_run"` object with updated parameters
#' @export
update_pars = function(run, new_pars) {
  if (!inherits(run, "nwsrfs_run")) {
    stop("'run' must be an nwsrfs_run object", call. = FALSE)
  }
  if (!is.data.frame(new_pars)) {
    stop("new_pars must be a data.frame.", call. = FALSE)
  }
  if (anyDuplicated(names(new_pars)) > 0L) {
    stop("new_pars must not have duplicate column names.", call. = FALSE)
  }
  if (!all(c("p_name", "value") %in% names(new_pars))) {
    stop("new_pars must have 'p_name' and 'value' columns.", call. = FALSE)
  }
  if (nrow(new_pars) == 0L) {
    return(run)
  }

  p_names = new_pars$p_name
  if (anyNA(p_names)) {
    stop("new_pars$p_name must not contain missing values.", call. = FALSE)
  }
  p_names = as.character(p_names)
  if (anyDuplicated(p_names) > 0L) {
    dups = unique(p_names[duplicated(p_names)])
    stop("Duplicate p_name values in new_pars: ", paste(dups, collapse = ", "), call. = FALSE)
  }
  if (anyNA(run$pars$p_name) || anyDuplicated(run$pars$p_name) > 0L) {
    stop("run$pars$p_name must contain non-missing unique identifiers.", call. = FALSE)
  }
  unknown = setdiff(p_names, run$pars$p_name)
  if (length(unknown) > 0L) {
    stop(
      "All p_name values in new_pars must exist in the current pars; unknown: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  if (is.logical(new_pars$value) && !all(is.na(new_pars$value))) {
    stop("new_pars$value must be numeric.", call. = FALSE)
  }
  vals = suppressWarnings(as.double(new_pars$value))
  bad_num = !is.na(new_pars$value) & (is.na(vals) | is.infinite(vals))
  if (any(bad_num)) {
    stop("new_pars$value must contain finite numeric values (or NA to retain existing values).", call. = FALSE)
  }

  pars = run$pars
  idx = match(p_names, pars$p_name)
  keep_update = !is.na(vals)
  pars$value[idx[keep_update]] = vals[keep_update]

  .run_model_chain(
    pars = pars,
    forcing_raw = run$forcing_raw,
    upflow = run$upflow,
    daily_flow = run$daily_flow,
    inst_flow = run$inst_flow,
    forcing_adj = run$.forcing_adj,
    shift_sf = run$.shift_sf,
    return_inst = run$.return_inst,
    area_elev = run$area_elev,
    area_elev_logic = isTRUE(run$area_elev_logic),
    ptps_logic = run$ptps_logic
  )
}


# %%
# Internal helpers: strict parameter array extraction and lookup tables

.parameter_arrays = function(pars, par_type, names, zones, model = par_type) {
  zones = as.character(zones)
  if (length(zones) == 0L) {
    stop(model, " requires at least one basin, zone, reach, or period.", call. = FALSE)
  }
  sub_p = pars[!is.na(pars$type) & pars$type == par_type, , drop = FALSE]
  res = list()
  errs = character(0)
  for (nm in names) {
    rows = sub_p[
      !is.na(sub_p$name) & sub_p$name == nm & !is.na(sub_p$zone) & sub_p$zone %in% zones,
      ,
      drop = FALSE
    ]
    cnts = vapply(zones, \(z) sum(rows$zone == z), integer(1L))
    bad = which(cnts != 1L)
    if (length(bad) > 0L) {
      details = paste(sprintf("%s (found %d)", zones[bad], cnts[bad]), collapse = ", ")
      errs = c(errs, paste0(nm, ": ", details))
    } else {
      res[[nm]] = as.double(rows$value[match(zones, rows$zone)])
    }
  }
  if (length(errs) > 0L) {
    stop(
      sprintf(
        "%s requires exactly one value for each required parameter with type == '%s'; %s",
        model,
        par_type,
        paste(errs, collapse = "; ")
      ),
      call. = FALSE
    )
  }
  res
}

.build_peadj_table = function(pars, zones, par_type = "sac", prefix = "peadj_") {
  m_names = sprintf("%s%02d", prefix, 1:12)
  arrs = .parameter_arrays(pars, par_type, m_names, zones, "PEADJ")
  mat = do.call(rbind, arrs)
  out = data.frame(month = 1L:12L, check.names = FALSE)
  for (j in seq_along(zones)) {
    out[[zones[j]]] = as.double(mat[, j])
  }
  out
}

.build_chanloss_table = function(pars) {
  n_clmods = .n_clmods(pars)
  cl_type = pars$value[pars$name == "cl_type" & pars$type == "chanloss"][1L]
  if (is.na(cl_type)) {
    cl_type = 1L
  }
  default_factor = if (as.integer(cl_type) == 1L) 1.0 else 0.0

  if (is.na(n_clmods) || n_clmods <= 0L) {
    return(data.frame(month = 1L:12L, chanloss_adj = rep(default_factor, 12L)))
  }

  month_vals = vector("list", 12L)
  for (i in seq_len(n_clmods)) {
    s_mon = as.integer(pars$value[pars$name == sprintf("cl_period_start_%02d", i)][1L])
    e_mon = as.integer(pars$value[pars$name == sprintf("cl_period_end_%02d", i)][1L])
    fac = as.double(pars$value[pars$name == sprintf("cl_factor_%02d", i)][1L])
    n_span = ((e_mon - s_mon) %% 12L) + 1L
    mons = ((s_mon - 1L + seq.int(0L, n_span - 1L)) %% 12L) + 1L
    for (m in mons) {
      month_vals[[m]] = c(month_vals[[m]], fac)
    }
  }

  adj = vapply(
    1:12,
    \(m) if (length(month_vals[[m]]) == 0L) default_factor else mean(month_vals[[m]]),
    numeric(1L)
  )
  data.frame(month = 1L:12L, chanloss_adj = adj)
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
  shift_sf = TRUE,
  return_inst = TRUE,
  area_elev = NULL,
  area_elev_logic = !is.null(area_elev),
  ptps_logic = NULL
) {
  pars = as.data.frame(pars)

  # SAC-Snow zone names: zones containing "-" with type == "sac"
  # (Filtering on type == "sac" prevents a shared UH zone like "LID-UH" from
  # being treated as a SAC-Snow forcing zone.)
  zone_names = sort(unique(pars$zone[!is.na(pars$zone) & grepl("-", pars$zone) & pars$type == "sac"]))
  n_zones = length(zone_names)
  localflow_logic = n_zones > 0L

  if (localflow_logic) {
    uh_spec = .detect_uh_spec(pars)
    uh_type = uh_spec$uh_type
    uh_zone_names = uh_spec$uh_zone_names
    shared_uh_logic = uh_spec$shared_uh_logic
  } else {
    uh_type = NULL
    uh_zone_names = NULL
    shared_uh_logic = NULL
  }

  upflow_logic = !is.null(upflow) && length(upflow) > 0L
  upflow_names = if (upflow_logic) {
    sort(unique(pars$zone[!is.na(pars$zone) & pars$type == "lagk"]))
  } else {
    NULL
  }

  chanloss_logic = .n_clmods(pars) > 0
  pars = pars[pars$name != "n_clmods", , drop = FALSE]
  rownames(pars) = NULL

  cu_zones = if (any(!is.na(pars$zone) & grepl("-CU$", pars$zone))) {
    sort(unique(pars$zone[!is.na(pars$zone) & pars$type == "consuse"]))
  } else {
    character(0)
  }
  consuse_logic = length(cu_zones) > 0L

  forcing_local = NULL
  if (localflow_logic) {
    if (is.null(forcing_raw) || length(forcing_raw) == 0L) {
      stop("Local flow zones are present in pars, but forcing_raw is missing.", call. = FALSE)
    }
    miss_fz = setdiff(zone_names, names(forcing_raw))
    if (length(miss_fz) > 0L) {
      stop("Forcing data missing for SAC-Snow zone(s): ", paste(miss_fz, collapse = ", "), call. = FALSE)
    }
    forcing_local = forcing_raw[zone_names]
    ts_local = .validate_ts_list(
      forcing_local,
      dt_hours = NULL,
      supported = .supported_fa_dt_hours,
      label = "forcing"
    )
    dt_hours = ts_local$dt_hours
  } else {
    forcing_raw = NULL
  }

  upflow_aligned = NULL
  if (upflow_logic) {
    if (length(upflow_names) == 0L) {
      stop("Upflow data is present, but no type == 'lagk' reaches were found in pars.", call. = FALSE)
    }
    miss_uf = setdiff(upflow_names, names(upflow))
    if (length(miss_uf) > 0L) {
      stop("Upflow data missing for Lag-K reach(es): ", paste(miss_uf, collapse = ", "), call. = FALSE)
    }
    upflow_aligned = upflow[upflow_names]
    ts_uf = .validate_ts_list(
      upflow_aligned,
      dt_hours = if (localflow_logic) dt_hours else NULL,
      supported = .supported_dt_hours,
      label = "upflow"
    )
    if (!localflow_logic) {
      dt_hours = ts_uf$dt_hours
    } else if (!identical(ts_uf$epoch_hours, ts_local$epoch_hours)) {
      stop("Forcing and upflow timestamps must represent identical date-time ranges.", call. = FALSE)
    }
  }

  if (!localflow_logic && !upflow_logic) {
    stop("No forcing or upflow data available", call. = FALSE)
  }

  if (is.null(ptps_logic)) {
    ptps_logic = if (localflow_logic) {
      all(vapply(forcing_local, \(df) "ptps" %in% names(df), logical(1L)))
    } else {
      TRUE
    }
  }

  sim_length = if (localflow_logic) nrow(forcing_local[[1L]]) else nrow(upflow_aligned[[1L]])
  sim_flow = numeric(sim_length)

  sacsnow_sf = NULL
  sacsnow_tci = NULL
  forcings_adj = NULL
  uh_df = NULL
  peadj_table = NULL

  if (localflow_logic) {
    fa_types = .fa_adjust_types(forcing_adj)

    if (!isTRUE(ptps_logic)) {
      .parameter_arrays(pars, "rsnwelev", c("pxtemp", "talr"), zone_names, "RSnwElev")
      .parameter_arrays(pars, "snow", "elev", zone_names, "RSnwElev")
      if (is.null(area_elev)) {
        stop("RSnwElev requires area-elevation data.", call. = FALSE)
      }
      ae_cols = names(area_elev)[-1L]
      miss_ae = setdiff(zone_names, ae_cols)
      if (length(miss_ae) > 0L && !all(make.names(zone_names) %in% ae_cols)) {
        stop(
          "Area-elevation CSV is missing required columns: ",
          paste(miss_ae, collapse = ", "),
          ".",
          call. = FALSE
        )
      }
      forcing_local = rsnwelev(forcing_local, pars, area_elev)
      fa_types = setdiff(fa_types, "ptps")
    }

    # Validate required parameter counts for FA, SacSnow, and UH
    basin_name = sub("-.*$", "", zone_names[1L])
    for (f in fa_types) {
      .parameter_arrays(pars, "fa", paste0(f, c("_scale", "_p_redist", "_std", "_shift")), basin_name, "FA")
      .parameter_arrays(
        pars,
        "fa_limit",
        c(sprintf("%s_lower_%02d", f, 1:12), sprintf("%s_upper_%02d", f, 1:12)),
        zone_names[1L],
        "FA"
      )
    }
    .parameter_arrays(pars, "sac", sprintf("peadj_%02d", 1:12), zone_names, "FA")
    .parameter_arrays(pars, "snow", "alat", zone_names, "FA")
    .parameter_arrays(pars, "uh", "zone_area", zone_names, "FA")

    .parameter_arrays(
      pars,
      "sac",
      c(
        "uztwm", "uzfwm", "lztwm", "lzfsm", "lzfpm", "adimp",
        "uzk", "lzsk", "lzpk", "zperc", "rexp",
        "pctim", "pfree", "riva", "side", "rserv", "efc",
        "pxadj", "peadj"
      ),
      zone_names,
      "SacSnow"
    )
    .parameter_arrays(
      pars,
      "snow",
      c(
        "scf", "mfmax", "mfmin", "uadj", "si",
        "nmf", "tipm", "mbase", "plwhc", "daygm",
        "adc_a", "adc_b", "adc_c", "alat", "elev"
      ),
      zone_names,
      "SacSnow"
    )

    if (uh_type == "gamma") {
      .parameter_arrays(pars, "uh", "unit_shape", uh_zone_names, "GammaUh")
      uh_sub = pars[pars$type == "uh", , drop = FALSE]
      if (any(uh_sub$name == "unit_scale") && !any(uh_sub$name == "unit_toc")) {
        .parameter_arrays(pars, "uh", "unit_scale", uh_zone_names, "GammaUh")
      } else {
        .parameter_arrays(pars, "uh", c("unit_toc", "unit_toc_adj"), uh_zone_names, "GammaUh")
      }
    } else {
      for (uz in uh_zone_names) {
        uz_names = pars$name[pars$type == "uh" & pars$zone == uz & grepl("^uh_[0-9]+$", pars$name)]
        .parameter_arrays(pars, "uh", unique(uz_names), uz, "OrdinatesUh")
      }
    }

    fa_arg_call = if (length(fa_types) == 0L) FALSE else fa_types
    forcings_adj = fa_nwrfc(dt_hours, forcing_local, pars, adjust = fa_arg_call)
    sacsnow_tci = sac_snow(dt_hours, forcings_adj, pars)
    sacsnow_sf = uh(
      dt_hours,
      sacsnow_tci,
      pars,
      sum_zones = TRUE,
      start_of_timestep = shift_sf,
      backfill = TRUE,
      return_inst = return_inst
    )
    uh_df = uh_ordinates(pars, dt_hours = dt_hours, source_dt_hours = dt_hours)
    peadj_table = .build_peadj_table(pars, zone_names, par_type = "sac", prefix = "peadj_")

    sim_flow = sim_flow + sacsnow_sf
  }

  lagk_route = NULL
  if (upflow_logic) {
    .parameter_arrays(
      pars,
      "lagk",
      c(
        "lagtbl_a", "lagtbl_b", "lagtbl_c", "lagtbl_d",
        "ktbl_a", "ktbl_b", "ktbl_c", "ktbl_d",
        "lagk_lagmax", "lagk_lagmin", "lagk_kmax", "lagk_kmin",
        "lagk_qmax", "lagk_qmin", "init_co", "init_stor", "init_if", "init_of"
      ),
      upflow_names,
      "LagK"
    )
    lagk_route = lagk(dt_hours, upflow_aligned, pars, sum_routes = TRUE)
    sim_flow = sim_flow + lagk_route
  }

  chanloss_table = NULL
  if (chanloss_logic) {
    cl_basin = unique(pars$zone[pars$type == "chanloss" & pars$name %in% c("cl_type", "cl_min_q")])
    if (length(cl_basin) != 1L) {
      stop("Chanloss requires a single basin zone for cl_type and cl_min_q.", call. = FALSE)
    }
    .parameter_arrays(pars, "chanloss", c("cl_type", "cl_min_q"), cl_basin, "Chanloss")
    ts_cl = if (localflow_logic) forcing_local[1L] else upflow_aligned[1L]
    sim_flow = chanloss(sim_flow, ts_cl, dt_hours, pars)
    chanloss_table = .build_chanloss_table(pars)
  }

  consuse_peadj_table = NULL
  if (consuse_logic) {
    if (!localflow_logic || is.null(forcings_adj)) {
      stop("Consuse requires local forcing adjustments (PET) for CU zones.", call. = FALSE)
    }
    miss_cu_f = setdiff(cu_zones, names(forcings_adj))
    if (length(miss_cu_f) > 0L) {
      stop("Forcing data missing for Consuse zone(s): ", paste(miss_cu_f, collapse = ", "), call. = FALSE)
    }
    min_flow_nm = if (any(pars$type == "consuse" & pars$name == "min_flow")) "min_flow" else "min_flow_cmsd"
    .parameter_arrays(
      pars,
      "consuse",
      c("area_km2", "irr_eff", min_flow_nm, "rf_accum_rate", "rf_decay_rate", sprintf("peadj_cu_%02d", 1:12)),
      cu_zones,
      "Consuse"
    )
    .parameter_arrays(pars, "sac", "peadj", cu_zones, "Consuse")
    consuse_peadj_table = .build_peadj_table(pars, cu_zones, par_type = "consuse", prefix = "peadj_cu_")

    ts_ref = forcings_adj[[1L]]
    dates_utc = as.Date(sprintf("%04d-%02d-%02d", ts_ref$year, ts_ref$month, ts_ref$day))
    day_int = as.integer(dates_utc)
    daily_days = unique(day_int)
    day_fac = factor(day_int, levels = daily_days)

    sim_peravg = (sim_flow + c(sim_flow[-1L], sim_flow[sim_length])) / 2
    daily_sim = as.numeric(tapply(sim_peravg, day_fac, mean))
    daily_dates = as.Date(daily_days, origin = "1970-01-01")
    d_year = as.integer(format(daily_dates, "%Y"))
    d_month = as.integer(format(daily_dates, "%m"))
    d_day = as.integer(format(daily_dates, "%d"))

    # Shift PET timestamps back 1 hour (00:00 belongs to previous day in CHPS)
    # and repeat the final value at the final timestamp, matching Python.
    ts_sec = as.numeric(dates_utc) * 86400 + ts_ref$hour * 3600
    pet_sec = c(ts_sec - 3600, ts_sec[sim_length])
    pet_day = as.integer(floor(pet_sec / 86400))
    keep_p = pet_day %in% daily_days
    pet_fac = factor(pet_day[keep_p], levels = daily_days)

    cu_adj_daily = numeric(length(daily_days))
    for (cz in cu_zones) {
      pet_sub = forcings_adj[[cz]]$pet_mm
      pet_vec = c(pet_sub, pet_sub[sim_length])
      daily_pet = as.numeric(tapply(pet_vec[keep_p], pet_fac, sum))
      valid_d = !is.na(daily_pet) & !is.na(daily_sim)

      cu_input = data.frame(
        year = d_year[valid_d],
        month = d_month[valid_d],
        day = d_day[valid_d],
        pet = daily_pet[valid_d],
        flow = daily_sim[valid_d]
      )
      cu_pars_z = pars[
        (pars$type == "consuse" & pars$zone == cz) |
          (pars$type == "sac" & pars$name == "peadj" & pars$zone == cz),
        ,
        drop = FALSE
      ]
      cu_res_z = consuse(cu_input, cu_pars_z, cfs = TRUE)
      cu_adj_z = numeric(length(daily_days))
      cu_adj_z[valid_d] = cu_res_z$qdiv - cu_res_z$qrfout
      cu_adj_daily = cu_adj_daily + cu_adj_z
    }

    # Shift daily adjustment forward +1 day, reindex to subdaily timestamps with
    # backfill, and carry the final daily adjustment into trailing hours.
    cu_idx = pmin(findInterval(ts_sec - 1, (daily_days + 1L) * 86400) + 1L, length(daily_days))
    cu_adj_sub = cu_adj_daily[cu_idx]
    sim_flow = pmax(sim_flow - cu_adj_sub, 0)
  }

  result = list(
    sim = sim_flow,
    sacsnow_sf = sacsnow_sf,
    sacsnow_tci = sacsnow_tci,
    lagk_route = lagk_route,
    forcings = forcings_adj,
    uh = uh_df,
    area_elev = area_elev,
    peadj_table = peadj_table,
    consuse_peadj_table = consuse_peadj_table,
    chanloss_table = chanloss_table,
    pars = pars,
    forcing_raw = forcing_raw,
    upflow = upflow,
    daily_flow = daily_flow,
    inst_flow = inst_flow,
    dt_hours = dt_hours,
    zone_names = zone_names,
    uh_type = uh_type,
    uh_zone_names = uh_zone_names,
    shared_uh_logic = shared_uh_logic,
    ptps_logic = ptps_logic,
    area_elev_logic = isTRUE(area_elev_logic),
    upflow_names = upflow_names,
    localflow_logic = localflow_logic,
    upflow_logic = upflow_logic,
    chanloss_logic = chanloss_logic,
    consuse_logic = consuse_logic,
    .forcing_adj = forcing_adj,
    .shift_sf = shift_sf,
    .return_inst = return_inst
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
  cat("  Zones:    ", if (length(x$zone_names) > 0L) paste(x$zone_names, collapse = ", ") else "none", "\n")
  if (!is.null(x$uh_type)) {
    uh_desc = paste0(
      x$uh_type,
      if (isTRUE(x$shared_uh_logic)) paste0(" (shared: ", x$uh_zone_names, ")") else ""
    )
    cat("  UH type:  ", uh_desc, "\n")
  }
  cat("  Timestep: ", x$dt_hours, "hours\n")
  cat("  Length:   ", length(x$sim), "timesteps\n")
  cat("  Components:\n")
  cat("    SAC-SMA/SNOW17/UH:", x$localflow_logic, "\n")
  if (isTRUE(x$localflow_logic)) {
    cat("    RSnwElev (PTPS):  ", !isTRUE(x$ptps_logic), "\n")
  }
  cat("    Lag-K:            ", x$upflow_logic, "\n")
  cat("    Chanloss:         ", x$chanloss_logic, "\n")
  cat("    Consuse:          ", x$consuse_logic, "\n")
  invisible(x)
}
