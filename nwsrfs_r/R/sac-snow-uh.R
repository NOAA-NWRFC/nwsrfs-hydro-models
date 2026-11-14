#' Execute SAC-SMA, SNOW17 and UH with given parameters
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with columns for forcing inputs
#' @param pars sac parameters
#' @return Vector of routed flow in cfs
#' @export
#'
#' @examples
#' # Simple: full model chain via the bundled example
#' run <- load_example("NRKW1")
#' head(run$sim)
#'
#' # Low-level: populate etd_mm / pet_mm via fa_nwrfc first,
#' # then call the combined SAC-SMA / SNOW17 / UH wrapper.
#' data(nrkw1_forcing)
#' data(nrkw1_pars)
#' dt_hours <- 6
#' forcing_adj <- fa_nwrfc(dt_hours, nrkw1_forcing, nrkw1_pars)
#' flow_cfs <- sac_snow_uh(dt_hours, forcing_adj, nrkw1_pars)
sac_snow_uh <- function(dt_hours, forcing, pars) {
  tci <- sac_snow(dt_hours, forcing, pars)
  flow_cfs <- uh(dt_hours, tci, pars)
  flow_cfs <- chanloss(flow_cfs, forcing, dt_hours, pars)
  flow_cfs
}

#' Execute SAC-SMA, SNOW17, UH and LAG-K with given parameters
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with columns for forcing inputs
#' @param uptribs data frame with columns for upstream flow data
#' @param pars sac parameters
#' @return Vector of routed flow in cfs
#' @export
#'
#' @examples
#' # Simple: full model chain via the bundled example (NRKW1 has upstream tribs)
#' run <- load_example("NRKW1")
#' head(run$sim)
#'
#' # Low-level: populate etd_mm / pet_mm via fa_nwrfc first, then chain
#' # SAC-SMA / SNOW17 / UH with Lag-K routing of upstream tributaries.
#' data(nrkw1_forcing)
#' data(nrkw1_pars)
#' data(nrkw1_upflow)
#' dt_hours <- 6
#' forcing_adj <- fa_nwrfc(dt_hours, nrkw1_forcing, nrkw1_pars)
#' flow_cfs <- sac_snow_uh_lagk(dt_hours, forcing_adj, nrkw1_upflow, nrkw1_pars)
sac_snow_uh_lagk <- function(dt_hours, forcing, uptribs, pars) {
  tci <- sac_snow(dt_hours, forcing, pars)
  flow_cfs <- uh(dt_hours, tci, pars)
  lagk_flow_cfs <- lagk(dt_hours, uptribs, pars)
  total_flow_cfs <- chanloss(flow_cfs + lagk_flow_cfs, forcing, dt_hours, pars)
  total_flow_cfs
}

#' Execute SAC-SMA, SNOW17, return total channel inflow per zone, and model states
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with with columns for forcing inputs
#' @param pars sac parameters
#' @return data.frame (1 column per zone) of unrouted channel inflow (tci), sac states
#' uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc, and snow water equivalent (swe),
#' and adjusted forcing data.
#' @export
#'
#' @examples
#' # Simple: full model chain via the bundled example, tci and states returned together
#' run <- load_example("NRKW1")
#' head(run$sacsnow_tci)
#'
#' # Low-level: populate etd_mm / pet_mm via fa_nwrfc first, then
#' # get per-zone tci and states directly.
#' data(nrkw1_forcing)
#' data(nrkw1_pars)
#' dt_hours <- 6
#' forcing_adj <- fa_nwrfc(dt_hours, nrkw1_forcing, nrkw1_pars)
#' states <- sac_snow_states(dt_hours, forcing_adj, nrkw1_pars)
sac_snow_states <- function(dt_hours, forcing, pars) {
  sac_snow(dt_hours, forcing, pars, return_states = TRUE)
}

.supported_dt_hours = c(1L, 2L, 3L, 4L, 6L, 8L, 12L, 24L)
.supported_fa_dt_hours = c(1L, 2L, 3L, 4L, 6L, 8L, 12L)

.integer_hours = function(value, name = "dt_hours", supported = .supported_dt_hours) {
  if (
    is.logical(value) ||
      !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value != floor(value)
  ) {
    stop(
      name,
      " must be a positive integer number of hours that divides 24 (supported: ",
      paste(supported, collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  val = as.integer(value)
  if (!val %in% supported) {
    stop(
      "Unsupported ",
      name,
      ": ",
      val,
      ". Supported timesteps are ",
      paste(supported, collapse = ", "),
      " hours.",
      call. = FALSE
    )
  }
  val
}

.infer_and_validate_ts = function(
  df,
  dt_hours = NULL,
  require_hour = TRUE,
  supported = .supported_dt_hours,
  label = "timeseries"
) {
  df = as.data.frame(df)
  req_cols = if (require_hour) c("year", "month", "day", "hour") else c("year", "month", "day")
  missing_cols = setdiff(req_cols, names(df))
  if (length(missing_cols) > 0L) {
    stop(
      label,
      " is missing required time column(s): ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (nrow(df) < 2L) {
    stop(label, " requires at least two timestamps.", call. = FALSE)
  }
  for (col in req_cols) {
    vals = df[[col]]
    if (is.logical(vals) || !is.numeric(vals) || any(!is.finite(vals)) || any(vals != floor(vals))) {
      stop(label, " column '", col, "' must contain finite integer values.", call. = FALSE)
    }
  }

  yr = as.integer(df$year)
  mo = as.integer(df$month)
  dy = as.integer(df$day)
  hr = if (require_hour) as.integer(df$hour) else integer(nrow(df))

  if (any(mo < 1L | mo > 12L) || any(dy < 1L | dy > 31L) || any(hr < 0L | hr > 23L)) {
    stop(label, " contains out-of-range calendar date/hour values.", call. = FALSE)
  }
  dates = as.Date(sprintf("%04d-%02d-%02d", yr, mo, dy))
  if (anyNA(dates)) {
    stop(label, " contains invalid calendar dates.", call. = FALSE)
  }

  ts_sec = as.numeric(dates) * 86400 + hr * 3600
  diffs = diff(ts_sec)

  if (is.null(dt_hours)) {
    dt_sec_mean = mean(diffs)
    dt_hr_cand = dt_sec_mean / 3600
    dt_hours = .integer_hours(dt_hr_cand, name = "dt_hours", supported = supported)
  } else {
    dt_hours = .integer_hours(dt_hours, name = "dt_hours", supported = supported)
  }

  dt_sec = dt_hours * 3600
  if (!all(diffs == dt_sec)) {
    stop(
      "Time columns (",
      paste(req_cols, collapse = ", "),
      ") in ",
      label,
      " must step by exactly ",
      dt_hours,
      " hour(s) with no gaps, duplicates, or reversed timestamps.",
      call. = FALSE
    )
  }

  list(dt_hours = dt_hours, dates = dates, ts_sec = ts_sec)
}

.validate_ts_list = function(
  ts_list,
  dt_hours = NULL,
  supported = .supported_dt_hours,
  label = "forcing"
) {
  if (!is.list(ts_list) || is.data.frame(ts_list) || length(ts_list) == 0L) {
    stop(label, " must be a non-empty list of data.frames.", call. = FALSE)
  }
  ref = .infer_and_validate_ts(
    ts_list[[1L]],
    dt_hours = dt_hours,
    require_hour = TRUE,
    supported = supported,
    label = label
  )
  if (length(ts_list) > 1L) {
    for (i in 2L:length(ts_list)) {
      cur = .infer_and_validate_ts(
        ts_list[[i]],
        dt_hours = ref$dt_hours,
        require_hour = TRUE,
        supported = supported,
        label = label
      )
      if (length(cur$ts_sec) != length(ref$ts_sec) || !all(cur$ts_sec == ref$ts_sec)) {
        stop("All ", label, " series must share identical timestamps.", call. = FALSE)
      }
    }
  }
  ref
}

.align_zone_values = function(pars, par_name, n_expected, zones = NULL, par_type = NULL, label = "model") {
  rows = pars[pars$name == par_name, , drop = FALSE]
  if (!is.null(par_type) && "type" %in% names(rows) && any(rows$type == par_type)) {
    rows = rows[rows$type == par_type, , drop = FALSE]
  }
  if (!is.null(zones) && "zone" %in% names(rows) && all(zones %in% rows$zone)) {
    counts = vapply(zones, \(z) sum(rows$zone == z), integer(1L))
    if (any(counts != 1L)) {
      bad = zones[counts != 1L]
      stop(
        label,
        ": parameter '",
        par_name,
        "' must appear exactly once per zone (",
        paste(sprintf("%s: %d", bad, counts[counts != 1L]), collapse = ", "),
        ").",
        call. = FALSE
      )
    }
    vals = rows$value[match(zones, rows$zone)]
  } else {
    vals = rows$value
  }
  if (length(vals) != n_expected || any(!is.finite(vals))) {
    stop(
      label,
      ": parameter '",
      par_name,
      "' must supply ",
      n_expected,
      " finite value(s).",
      call. = FALSE
    )
  }
  as.double(vals)
}

#' Execute SAC-SMA, SNOW17, return total channel inflow per zone, and model states
#'
#' @param dt_hours timestep in hours (1, 2, 3, 4, 6, 8, 12, or 24)
#' @param forcing data frame list with columns for forcing inputs
#' @param pars sac and snow parameters
#' @param return_states logical value indicating if the states should be output as well as the tci
#' @return Matrix (1 column per zone) of unrouted channel inflow (`tci`), or a
#'   `data.frame` of SAC-SMA / SNOW-17 states and adjusted forcings when
#'   `return_states = TRUE`.
#' @export
#'
#' @examples
#' # Simple: full model chain via the bundled example
#' run <- load_example("NRKW1")
#' head(run$sacsnow_tci)
#'
#' # Low-level: populate etd_mm / pet_mm via fa_nwrfc first,
#' # then call the SAC-SMA / SNOW17 wrapper directly. sac_snow() requires
#' # forcing data frames that already contain etd_mm.
#' data(nrkw1_forcing)
#' data(nrkw1_pars)
#' dt_hours <- 6
#' forcing_adj <- fa_nwrfc(dt_hours, nrkw1_forcing, nrkw1_pars)
#' tci <- sac_snow(dt_hours, forcing_adj, nrkw1_pars)
#' @useDynLib nwsrfsr sacsnow_
sac_snow = function(dt_hours, forcing, pars, return_states = FALSE) {
  pars = as.data.frame(pars)
  ts_info = .validate_ts_list(forcing, dt_hours = dt_hours, supported = .supported_dt_hours, label = "forcing")
  dt_hours = ts_info$dt_hours
  dt_seconds = dt_hours * 3600L

  n_zones = length(forcing)
  sim_length = nrow(forcing[[1L]])
  zone_names = names(forcing)

  sac_names = c(
    "uztwm", "uzfwm", "lztwm", "lzfpm", "lzfsm", "adimp",
    "uzk", "lzpk", "lzsk", "zperc", "rexp", "pctim",
    "pfree", "riva", "side", "rserv", "efc"
  )
  snow_names = c(
    "scf", "mfmax", "mfmin", "uadj", "si", "nmf", "tipm",
    "mbase", "plwhc", "daygm", "adc_a", "adc_b", "adc_c"
  )

  sac_pars = do.call(
    rbind,
    lapply(sac_names, \(nm) .align_zone_values(pars, nm, n_zones, zone_names, "sac", "sac_snow()"))
  )
  snow_pars = do.call(
    rbind,
    lapply(snow_names, \(nm) .align_zone_values(pars, nm, n_zones, zone_names, "snow", "sac_snow()"))
  )
  latitude = .align_zone_values(pars, "alat", n_zones, zone_names, "snow", "sac_snow()")
  elev = .align_zone_values(pars, "elev", n_zones, zone_names, "snow", "sac_snow()")
  peadj = .align_zone_values(pars, "peadj", n_zones, zone_names, "sac", "sac_snow()")
  pxadj = .align_zone_values(pars, "pxadj", n_zones, zone_names, "sac", "sac_snow()")
  init_swe = .align_zone_values(pars, "init_swe", n_zones, zone_names, "snow", "sac_snow()")

  nonneg_vals = c(
    init_swe,
    peadj,
    pxadj,
    as.vector(sac_pars[-match("side", sac_names), , drop = FALSE]),
    as.vector(snow_pars)
  )
  if (any(nonneg_vals < 0)) {
    stop(
      "SAC-SMA and SNOW-17 parameters (excluding elev, alat, and side) must be >= 0.",
      call. = FALSE
    )
  }
  if (any(elev < 0) || any(latitude < 0)) {
    warning("elev and/or alat values are below zero, verify this is correct.", call. = FALSE)
  }

  for (z in seq_len(n_zones)) {
    req_f = c("map_mm", "ptps", "mat_degc", "etd_mm")
    miss_f = setdiff(req_f, names(forcing[[z]]))
    if (length(miss_f) > 0L) {
      stop(
        "sac_snow(): forcing zone ",
        z,
        " is missing column(s): ",
        paste(miss_f, collapse = ", "),
        ".",
        call. = FALSE
      )
    }
  }

  map_mat = do.call("cbind", lapply(forcing, \(df) as.double(df[["map_mm"]])))
  ptps_mat = do.call("cbind", lapply(forcing, \(df) as.double(df[["ptps"]])))
  mat_mat = do.call("cbind", lapply(forcing, \(df) as.double(df[["mat_degc"]])))
  etd_mat = do.call("cbind", lapply(forcing, \(df) as.double(df[["etd_mm"]])))

  if (any(!is.finite(map_mat)) || any(!is.finite(ptps_mat)) || any(!is.finite(mat_mat)) || any(!is.finite(etd_mat))) {
    stop("sac_snow(): all forcing columns (map_mm, ptps, mat_degc, etd_mm) must contain finite values.", call. = FALSE)
  }
  if (any(map_mat < 0)) {
    stop("sac_snow(): map_mm values must be >= 0.", call. = FALSE)
  }
  if (any(etd_mat < 0)) {
    stop("sac_snow(): etd_mm values must be >= 0.", call. = FALSE)
  }
  if (any(ptps_mat < 0 | ptps_mat > 1)) {
    stop("sac_snow(): ptps values must be between 0 and 1.", call. = FALSE)
  }

  output_matrix = matrix(0, nrow = sim_length, ncol = n_zones)

  x = .Fortran(
    "sacsnow",
    n_hrus = as.integer(n_zones),
    dt = as.integer(dt_seconds),
    sim_length = as.integer(sim_length),
    year = as.integer(forcing[[1L]]$year)[1:sim_length],
    month = as.integer(forcing[[1L]]$month)[1:sim_length],
    day = as.integer(forcing[[1L]]$day)[1:sim_length],
    hour = as.integer(forcing[[1L]]$hour)[1:sim_length],
    latitude = latitude,
    elev = elev,
    sac_pars = sac_pars,
    peadj = peadj,
    pxadj = pxadj,
    snow_pars = snow_pars,
    init_swe = init_swe,
    map = map_mat,
    ptps = ptps_mat,
    mat = mat_mat,
    etd = etd_mat,
    return_states = as.logical(return_states),
    tci = output_matrix,
    aet = output_matrix,
    uztwc = output_matrix,
    uzfwc = output_matrix,
    lztwc = output_matrix,
    lzfsc = output_matrix,
    lzfpc = output_matrix,
    adimc = output_matrix,
    roimp = output_matrix,
    sdro = output_matrix,
    ssur = output_matrix,
    sif = output_matrix,
    bfs = output_matrix,
    bfp = output_matrix,
    swe = output_matrix,
    aesc = output_matrix,
    neghs = output_matrix,
    liqw = output_matrix,
    raim = output_matrix,
    psfall = output_matrix,
    prain = output_matrix
  )

  if (return_states) {
    return_vars = c(
      "year", "month", "day", "hour",
      "map", "mat", "ptps", "etd",
      "tci", "aet",
      "uztwc", "uzfwc", "lztwc", "lzfsc", "lzfpc", "adimc",
      "roimp", "sdro", "ssur", "sif", "bfs", "bfp",
      "swe", "aesc", "neghs", "liqw", "raim", "psfall", "prain"
    )

    if (!is.null(forcing[[1L]]$pet_mm)) {
      x[["pet"]] = do.call("cbind", lapply(forcing, \(df) as.double(df[["pet_mm"]])))
      return_vars = c(return_vars, "pet")
    }

    return(format_states(x[return_vars]))
  }

  tci_out = x$tci
  if (!is.null(zone_names)) {
    colnames(tci_out) = zone_names
  }
  tci_out
}


#' Format state output from sac_snow_states
#'
#' @param x output list from sac_snow_states
#'
#' @return a data.frame with formatted output
#'
format_states <- function(x) {
  df <- data.frame(year = x$year, month = x$month, day = x$day, hour = x$hour)
  n_zones <- ncol(x$tci)
  for (i in 1:n_zones) {
    for (name in names(x)[-(1:4)]) {
      df[[paste0(name, "_", i)]] <- x[[name]][, i]
    }
  }
  df
}


#' R port of the nwsrfs UH fortran code
#'
#' Returns ordinates for a 2 parameter (shape,scale) gamma unit hydrograph. The ordinates are
#' based on the given timestep (in hours). To match the Fortran code, a max length is used.
#' A warning is issued if the UH does not terminate before the given max length.
#'
#' @param shape gamma shape parameter
#' @param scale gamma scale parameter
#' @param timestep timestep in hours
#' @param max_len max length of the uh
#'
#' @return vector of ordinates for a 2 parameter (shape,scale) gamma unit hydrograph
#' @export
#'
#' @examples
#' dt <- 6
#' shape <- 2
#' scale <- 1
#' y <- uh2p(2, 1, 6)
#' x <- seq(dt, dt * length(y), by = dt)
#' plot(x, y, t = "l")
uh2p <- function(shape, scale, timestep, max_len = 1000) {
  # this code needs timestep in days
  timestep <- timestep / 24
  uh <- numeric(max_len)
  toc <- log(gamma(shape) * scale)
  # print(toc)
  for (i in 1:max_len) {
    top <- i * timestep / scale
    tor <- (shape - 1) * log(top) - top - toc
    uh[i] <- 0
    if (tor > -8.0) {
      uh[i] <- exp(tor)
    } else {
      if (i > 1) {
        uh[i] <- 0.0
        max_len <- i
        break
      }
    }
  }
  s <- sum(uh)
  s <- ifelse(s == 0, 1.0e-5, s)
  # turn it into a unit hydrograph (sums to 1)
  uh <- uh / s
  # dont return all the trailing zero values
  first0 <- which(uh == 0)[1]
  if (is.na(first0)) {
    warning("UH may have been truncated, increase max_len.")
    return(uh)
  }
  return(uh[1:(first0 - 1)])
}

#' Create a 2 parameter gamma unit hydrograph with units cfs/in
#'
#' @param shape gamma shape parameter
#' @param scale gamma scale parameter
#' @param timestep timestep in hours
#' @param area basin area in square miles
#'
#' @return stuff
#' @export
#'
#' @examples
#' dt <- 6
#' shape <- 2
#' scale <- 1
#' y <- uh2p_cfs_in(2, 1, 6, 1000)
#' x <- seq(dt, dt * length(y), by = dt)
#' plot(x, y, t = "l")
uh2p_cfs_in <- function(shape, scale, timestep, area) {
  # convert to cfs/in
  # timestep in hours
  # area in sq mi
  uh2p(shape, scale, timestep) * 5280^2 * 24 / 12 / 86400 / timestep * area
}

#' Get the scale parameter from a 2 parameter gamma unit hydrograph given
#' the shape parameter and time of concentration.
#'
#' @param shape gamma shape parameter
#' @param toc time of concentration (hours)
#' @param dt_hours UH timestep (hours)
#'
#' @return stuff
#' @export
#'
#' @examples
#' uh2p_get_scale_r(2, 50, 1)
#' @importFrom stats optimize
uh2p_get_scale_r <- function(shape, toc, dt_hours) {
  # find a reasonable upper limit for scale, some values are unstable
  scale_lim <- scale_uplimit(shape, dt_hours)
  # optimization to find scale given shape and toc
  scale <- optimize(uh2p_seek, shape, dt_hours, toc, interval = c(.01, scale_lim))$minimum
  # bump up very small or negative values to prevent 0 length UH
  max(as.numeric(scale), 0.02)
}

#' Get the scale parameter from a 2 parameter gamma unit hydrograph given
#' the shape parameter and time of concentration.
#'
#' @param shape gamma shape parameter
#' @param toc time of concentration (hours)
#' @param dt_hours UH timestep (hours)
#'
#' @return scalar scale parameter
#' @export
#'
#' @examples
#' uh2p_get_scale(2, 50, 1)
#' @useDynLib nwsrfsr uh2p_get_scale_root_
uh2p_get_scale <- function(shape, toc, dt_hours) {
  scale <- .Fortran("uh2p_get_scale_root", shape = shape, toc = toc, dt_hours = dt_hours, scale = 0)
  scale$scale
}


#' Find a reasonable scale upper limit for optimization
#'
#' @param shape shape parameter
#' @param dt_hours timestep
#'
#' @return numeric value indicating the upper limit for the scale
scale_uplimit <- function(shape, dt_hours) {
  scale <- 0.1
  len_1 <- 0
  len_2 <- length(uh2p(shape, scale, dt_hours))
  while ((len_1 <= len_2) & (scale < 5)) {
    len_1 <- len_2
    scale <- scale + .1
    len_2 <- length(uh2p(shape, scale, dt_hours))
  }
  return(scale - .1)
}

#' Objective function for finding a scale parameter given shape and toc
#'
#' @param scale blah
#' @param shape blah
#' @param dt_hours blah
#' @param toc blah
#'
#' @return objective function value
uh2p_seek <- function(scale, shape, dt_hours, toc) {
  # add one to the length because the first ordinate is at time 0
  uh_len <- round(toc / dt_hours, 0) + 1
  len_dif <- abs(length(uh2p(shape, scale, dt_hours)) - uh_len)
  return(len_dif)
}

#' Objective function for finding a scale parameter given shape and toc
#'
#' @param scale blah
#' @param shape blah
#' @param dt_hours blah
#' @param toc blah
#'
#' @return function value for root finding
#' @useDynLib nwsrfsr uh2p_len_obj_root_test_
uh2p_seek2 <- function(scale, shape, dt_hours, toc) {
  obj <- .Fortran(
    "uh2p_len_obj_root_test",
    scale = scale,
    shape = shape,
    toc = toc,
    dt_hours = dt_hours,
    obj = 0
  )
  obj$obj
}


#' root finding function for finding a scale parameter given shape and toc
#'
#' @param scale scale parameter
#' @param shape shape parameter
#' @param dt_hours timestep
#' @param toc time of concentration
#'
#' @return function value for root finding
uh2p_root <- function(scale, shape, dt_hours, toc) {
  # add one to the length because the first ordinate is at time 0
  uh_len <- round(toc / dt_hours, 0) + 1
  len_dif <- length(uh2p(shape, scale, dt_hours)) - uh_len
  return(len_dif)
}


.pchip_interp = function(x, y, xout) {
  n = length(x)
  if (n == 2L) {
    return(stats::approx(x, y, xout = xout, rule = 2)$y)
  }
  hk = diff(x)
  mk = diff(y) / hk

  edge_deriv = function(h0, h1, m0, m1) {
    d = ((2 * h0 + h1) * m0 - h0 * m1) / (h0 + h1)
    if (sign(d) != sign(m0)) {
      d = 0
    } else if (sign(m0) != sign(m1) && abs(d) > abs(3 * m0)) {
      d = 3 * m0
    }
    d
  }

  dk = numeric(n)
  dk[1L] = edge_deriv(hk[1L], hk[2L], mk[1L], mk[2L])
  dk[n] = edge_deriv(hk[n - 1L], hk[n - 2L], mk[n - 1L], mk[n - 2L])

  m1 = mk[1L:(n - 2L)]
  m2 = mk[2L:(n - 1L)]
  h1 = hk[1L:(n - 2L)]
  h2 = hk[2L:(n - 1L)]
  same_sign = (m1 > 0 & m2 > 0) | (m1 < 0 & m2 < 0)
  w1 = 2 * h1 + h2
  w2 = h1 + 2 * h2
  dk_int = numeric(n - 2L)
  dk_int[same_sign] = (w1[same_sign] + w2[same_sign]) /
    (w1[same_sign] / m1[same_sign] + w2[same_sign] / m2[same_sign])
  dk[2L:(n - 1L)] = dk_int

  stats::splinefunH(x, y, dk)(xout)
}

.detect_uh_spec = function(pars) {
  pars = as.data.frame(pars)
  uh_rows = if ("type" %in% names(pars) && any(pars$type == "uh")) {
    pars[pars$type == "uh", , drop = FALSE]
  } else {
    pars
  }
  has_shape = any(uh_rows$name == "unit_shape")
  has_ordinates = any(grepl("^uh_[0-9]+$", uh_rows$name))

  if (has_shape && has_ordinates) {
    stop(
      "UH parameters are ambiguous: unit_shape and uh_<number> entries cannot both be provided in rows where type == 'uh'.",
      call. = FALSE
    )
  }
  if (has_shape) {
    uh_type = "gamma"
    marker = "unit_shape"
  } else if (any(uh_rows$name == "uh_0")) {
    uh_type = "ordinate"
    marker = "uh_0"
  } else {
    stop(
      "UH parameters (type == 'uh') must include unit_shape for a gamma unit hydrograph or uh_<number> entries starting with uh_0 for an ordinate unit hydrograph.",
      call. = FALSE
    )
  }

  sac_zones = if (all(c("zone", "type") %in% names(pars))) {
    sort(unique(pars$zone[grepl("-", pars$zone) & pars$type == "sac"]))
  } else {
    character(0)
  }
  uh_zone_names = if ("zone" %in% names(uh_rows)) {
    sort(unique(uh_rows$zone[uh_rows$name == marker]))
  } else {
    character(0)
  }
  shared_uh_logic = (length(uh_zone_names) == 1L) && (length(sac_zones) > 1L)
  if (!shared_uh_logic && length(sac_zones) > 0L && "zone" %in% names(uh_rows)) {
    if (!setequal(uh_zone_names, sac_zones)) {
      stop("Provide one shared UH parameter set or one set per SAC-Snow zone.", call. = FALSE)
    }
  }

  list(
    uh_type = uh_type,
    marker = marker,
    uh_zone_names = uh_zone_names,
    sac_zones = sac_zones,
    shared_uh_logic = shared_uh_logic,
    uh_rows = uh_rows
  )
}

.extract_ordinate_matrix = function(pars, uh_zone_names = NULL) {
  pars = as.data.frame(pars)
  uh_rows = if ("type" %in% names(pars) && any(pars$type == "uh")) {
    pars[pars$type == "uh", , drop = FALSE]
  } else {
    pars
  }
  ord_rows = uh_rows[grepl("^uh_[0-9]+$", uh_rows$name), , drop = FALSE]
  if (nrow(ord_rows) == 0L) {
    stop("Ordinate UH requires uh_0, uh_1, ... parameters.", call. = FALSE)
  }

  if (is.null(uh_zone_names) || length(uh_zone_names) == 0L) {
    if ("zone" %in% names(ord_rows)) {
      uh_zone_names = sort(unique(ord_rows$zone[ord_rows$name == "uh_0"]))
    }
  }

  extract_one = function(z_rows, z_label) {
    if (anyDuplicated(z_rows$name) > 0L) {
      stop("Duplicate ordinate UH parameter names found for zone ", z_label, ".", call. = FALSE)
    }
    idx = as.integer(sub("^uh_", "", z_rows$name))
    ord = order(idx)
    idx_sorted = idx[ord]
    expected = seq.int(0L, length(idx_sorted) - 1L)
    if (length(idx_sorted) == 0L || !identical(idx_sorted, expected)) {
      stop(
        "Ordinate UH requires consecutive uh_0, uh_1, ... parameters for zone ",
        z_label,
        "; found: ",
        if (length(z_rows$name) > 0L) paste(z_rows$name[ord], collapse = ", ") else "none",
        ".",
        call. = FALSE
      )
    }
    as.double(z_rows$value[ord])
  }

  if (length(uh_zone_names) > 0L) {
    zone_vecs = lapply(uh_zone_names, \(z) extract_one(ord_rows[ord_rows$zone == z, , drop = FALSE], z))
  } else {
    zone_vecs = list(extract_one(ord_rows, "1"))
  }

  max_len = max(lengths(zone_vecs))
  ord_mat = matrix(NA_real_, nrow = max_len, ncol = length(zone_vecs))
  for (i in seq_along(zone_vecs)) {
    v = zone_vecs[[i]]
    ord_mat[seq_along(v), i] = v
  }
  if (length(uh_zone_names) == ncol(ord_mat)) {
    colnames(ord_mat) = uh_zone_names
  }
  ord_mat
}

.validate_ordinate_matrix = function(ord_mat, areas_km2, dt_hours) {
  dt_hours = .integer_hours(dt_hours, "dt_hours")
  if (!is.numeric(areas_km2) || length(areas_km2) == 0L || any(!is.finite(areas_km2)) || any(areas_km2 <= 0)) {
    stop("Ordinate UH zone areas must be finite positive values.", call. = FALSE)
  }
  if (!is.matrix(ord_mat) || !is.numeric(ord_mat) || nrow(ord_mat) == 0L) {
    stop("Ordinate UH ordinates must be a non-empty numeric 2D matrix.", call. = FALSE)
  }
  if (ncol(ord_mat) != length(areas_km2)) {
    stop(
      "Ordinate UH matrix column count (",
      ncol(ord_mat),
      ") must match number of UH zones (",
      length(areas_km2),
      ").",
      call. = FALSE
    )
  }

  for (i in seq_len(ncol(ord_mat))) {
    u = ord_mat[, i]
    valid_idx = which(!is.na(u))
    if (length(valid_idx) == 0L) {
      stop(sprintf("ordinates[, %d] must contain at least a single value.", i), call. = FALSE)
    }
    trimmed = u[seq_len(valid_idx[length(valid_idx)])]
    if (!all(is.finite(trimmed))) {
      stop(
        sprintf("ordinates[, %d] must contain only finite values, except for trailing NA values.", i),
        call. = FALSE
      )
    }
    if (any(trimmed < 0)) {
      stop(sprintf("ordinates[, %d] must be nonnegative.", i), call. = FALSE)
    }
    if (!any(trimmed > 0)) {
      stop(sprintf("ordinates[, %d] must contain a positive value.", i), call. = FALSE)
    }

    one_inch_vol_ft3 = areas_km2[i] * 0.386102 * 5280^2 / 12
    depth_in = sum(trimmed) * dt_hours * 3600 / one_inch_vol_ft3
    if (abs(depth_in - 1.0) > 0.1) {
      stop(
        sprintf(
          "ordinates[, %d] must represent 1 inch of runoff within 0.1 inch (got %.3f inches).",
          i,
          depth_in
        ),
        call. = FALSE
      )
    }
  }
  invisible(TRUE)
}

.resample_ordinates_pchip = function(ord_mat, source_dt_hours, target_dt_hours) {
  tstep_old = .integer_hours(source_dt_hours, "source_dt_hours")
  tstep = .integer_hours(target_dt_hours, "dt_hours")
  padding = as.integer(ceiling(tstep / tstep_old))

  new_cols = lapply(seq_len(ncol(ord_mat)), \(i) {
    u = ord_mat[, i]
    u = u[!is.na(u)]
    sh = cumsum(u)
    sh_pad = c(sh, rep(sh[length(sh)], padding))
    old_hours = (seq_along(sh_pad) - 1L) * tstep_old
    stop_hour = old_hours[length(old_hours)]
    nindex = seq.int(0L, stop_hour - 1L, by = tstep)
    combined = sort(unique(c(old_hours, nindex)))
    sh_interp = .pchip_interp(old_hours, sh_pad, combined)
    shift_k = match(tstep, combined) - 1L
    sh_shift = c(rep(0, shift_k), sh_interp[seq_len(length(sh_interp) - shift_k)])
    uh_new = (sh_interp - sh_shift) * (tstep_old / tstep)
    uh_new[match(nindex, combined)]
  })

  max_len = max(lengths(new_cols))
  out_mat = matrix(NA_real_, nrow = max_len, ncol = length(new_cols))
  for (i in seq_along(new_cols)) {
    v = new_cols[[i]]
    out_mat[seq_along(v), i] = v
  }
  colnames(out_mat) = colnames(ord_mat)
  out_mat
}

.gamma_scale_for_zone = function(uh_rows, zone_idx_or_name, by_zone = TRUE) {
  row_sub = if (by_zone) {
    uh_rows[uh_rows$zone == zone_idx_or_name, , drop = FALSE]
  } else {
    uh_rows
  }
  get1 = function(nm, idx = 1L) {
    vals = row_sub$value[row_sub$name == nm]
    if (length(vals) < idx) NA_real_ else as.double(vals[idx])
  }
  idx = if (by_zone) 1L else zone_idx_or_name
  shape = get1("unit_shape", idx)
  toc_gis = get1("unit_toc", idx)
  toc_adj = get1("unit_toc_adj", idx)
  scale_val = get1("unit_scale", idx)

  if (!is.finite(shape) || shape <= 0) {
    stop("Gamma UH 'unit_shape' must be a finite value > 0.", call. = FALSE)
  }
  if (!is.na(toc_gis) && !is.na(toc_adj)) {
    if (!is.finite(toc_gis) || !is.finite(toc_adj) || (toc_gis * toc_adj) <= 0) {
      stop("Gamma UH 'unit_toc * unit_toc_adj' must be a finite value > 0.", call. = FALSE)
    }
    scale = uh2p_get_scale(shape, toc_gis * toc_adj, 1)
  } else if (!is.na(scale_val)) {
    if (!is.finite(scale_val) || scale_val <= 0) {
      stop("Gamma UH 'unit_scale' must be a finite value > 0.", call. = FALSE)
    }
    scale = scale_val
  } else {
    stop("Gamma UH requires either ('unit_toc' and 'unit_toc_adj') or 'unit_scale'.", call. = FALSE)
  }
  list(shape = shape, scale = scale)
}

#' Compute or resample unit hydrograph ordinates in cfs/in
#'
#' Returns unit hydrograph ordinates (`cfs/in`) for each UH parameter set in an
#' `"nwsrfs_run"` object or parameter `data.frame` (`pars`), supporting both
#' two-parameter gamma unit hydrographs (`unit_shape`) and supplied ordinate
#' unit hydrographs (`uh_0`, `uh_1`, ...), as well as shared unit hydrographs
#' across multiple SAC-Snow zones. For ordinate unit hydrographs, requesting a
#' `dt_hours` different from `source_dt_hours` converts the hydrograph using
#' S-hydrograph accumulation and monotone cubic Hermite (PCHIP) interpolation.
#'
#' @param x An `"nwsrfs_run"` object or a parameter `data.frame` (`pars`).
#' @param dt_hours Desired unit hydrograph timestep in hours (`1`, `2`, `3`,
#'   `4`, `6`, `8`, `12`, or `24`). Defaults to `x$dt_hours` for `"nwsrfs_run"`
#'   objects, or `source_dt_hours` (default `6L`) for parameter `data.frame`s.
#' @param source_dt_hours Timestep in hours of supplied `uh_0, uh_1, ...`
#'   ordinates when `x` is a parameter `data.frame`. Defaults to `6L` (ignored
#'   when `x` is an `"nwsrfs_run"` object, where `x$dt_hours` is used).
#' @param validate Logical; validate ordinate volumes and parameter bounds.
#'   Default `TRUE`.
#'
#' @return A `data.frame` with integer column `hours` (`0, dt_hours, ...`) and
#'   one numeric column (`cfs/in`) per UH zone (shorter hydrographs padded with
#'   `NA`), or `NULL` if `x` is a routing-only `"nwsrfs_run"` without local flow.
#'
#' @importFrom stats approx splinefunH
#' @useDynLib nwsrfsr uh2p_call_
#' @export
#'
#' @examples
#' run = load_example("NRKW1")
#' head(uh_ordinates(run))
#' head(uh_ordinates(run, dt_hours = 3L))
uh_ordinates = function(x, dt_hours = NULL, source_dt_hours = NULL, validate = TRUE) {
  if (inherits(x, "nwsrfs_run")) {
    if (!isTRUE(x$localflow_logic)) {
      return(NULL)
    }
    src_dt = x$dt_hours
    target_dt = if (is.null(dt_hours)) src_dt else .integer_hours(dt_hours, "dt_hours")
    if (target_dt == src_dt && !is.null(x$uh)) {
      return(x$uh)
    }
    pars = x$pars
  } else if (is.data.frame(x)) {
    pars = as.data.frame(x)
    src_dt = if (is.null(source_dt_hours)) 6L else .integer_hours(source_dt_hours, "source_dt_hours")
    target_dt = if (is.null(dt_hours)) src_dt else .integer_hours(dt_hours, "dt_hours")
  } else {
    stop("'x' must be an 'nwsrfs_run' object or a parameter data.frame.", call. = FALSE)
  }

  spec = .detect_uh_spec(pars)
  uh_rows = spec$uh_rows
  uh_zones = spec$uh_zone_names

  area_zones = if (length(spec$sac_zones) > 0L) spec$sac_zones else uh_zones
  raw_areas = if (length(area_zones) > 0L) {
    .align_zone_values(uh_rows, "zone_area", length(area_zones), area_zones, "uh", "uh_ordinates()")
  } else {
    as.double(uh_rows$value[uh_rows$name == "zone_area"])
  }
  if (length(raw_areas) == 0L || any(!is.finite(raw_areas)) || any(raw_areas <= 0)) {
    stop("UH 'zone_area' must contain finite values > 0.", call. = FALSE)
  }
  uh_areas = if (spec$shared_uh_logic) sum(raw_areas) else raw_areas
  col_labels = if (length(uh_zones) == length(uh_areas)) {
    uh_zones
  } else {
    paste0("uh_", seq_along(uh_areas) - 1L)
  }

  if (spec$uh_type == "gamma") {
    by_z = length(uh_zones) == length(uh_areas)
    ord_list = lapply(seq_along(uh_areas), \(i) {
      ss = .gamma_scale_for_zone(uh_rows, if (by_z) uh_zones[i] else i, by_zone = by_z)
      uh_dl = .Fortran(
        "uh2p_call",
        shape = as.double(ss$shape),
        scale = as.double(ss$scale),
        timestep = as.double(target_dt),
        max_len = 1000L,
        uh = double(1000L)
      )$uh
      if (!all(is.finite(uh_dl))) {
        stop(sprintf("Zone %s produced nonfinite UH ordinates.", col_labels[i]), call. = FALSE)
      }
      nz = which(uh_dl != 0)
      if (length(nz) == 0L) {
        stop(
          sprintf("Zone %s produced an all-zero UH; check shape, scale, and timestep.", col_labels[i]),
          call. = FALSE
        )
      }
      uh_dl = uh_dl[seq_len(nz[length(nz)])]
      total_uh_vol = uh_areas[i] * 0.386102 * 5280^2 / 12
      uh_dl * total_uh_vol / (target_dt * 3600)
    })
    max_len = max(lengths(ord_list))
    ord_mat = matrix(NA_real_, nrow = max_len, ncol = length(ord_list))
    for (i in seq_along(ord_list)) {
      v = ord_list[[i]]
      ord_mat[seq_along(v), i] = v
    }
    colnames(ord_mat) = col_labels
  } else {
    ord_mat = .extract_ordinate_matrix(pars, uh_zones)
    if (validate) {
      .validate_ordinate_matrix(ord_mat, uh_areas, src_dt)
    }
    if (target_dt != src_dt) {
      ord_mat = .resample_ordinates_pchip(ord_mat, src_dt, target_dt)
    }
    colnames(ord_mat) = col_labels
  }

  hours = (seq_len(nrow(ord_mat)) - 1L) * target_dt
  out = data.frame(hours = as.integer(hours), check.names = FALSE)
  for (i in seq_len(ncol(ord_mat))) {
    out[[col_labels[i]]] = ord_mat[, i]
  }
  out
}


#' Unit hydrograph routing (gamma or ordinate, per-zone or shared)
#'
#' Routes total channel inflow (`tci`, mm) using either a two-parameter gamma
#' unit hydrograph (`unit_shape` in `pars`) or a supplied ordinate unit
#' hydrograph (`uh_0`, `uh_1`, ... in `pars`, or explicit `ordinates`),
#' supporting both per-zone hydrographs and a single shared unit hydrograph
#' across multiple SAC-Snow zones.
#'
#' @param dt_hours timestep in hours (`1`, `2`, `3`, `4`, `6`, `8`, `12`, or `24`)
#' @param tci channel inflow matrix (`mm`), one column per zone
#' @param pars parameter `data.frame`
#' @param sum_zones should routed flows from multiple zones be added and returned as a vector, or
#'                  kept separate and returned as a matrix
#' @param start_of_timestep should the output flow data be shifted by one timestep to account for
#'                          forcing data that uses beginning of timestep labeling
#' @param backfill when start_of_timestep is TRUE, should the first value be duplicated
#' @param return_inst should instantaneous flow be returned rather than period average.
#'                    Period average is the mean of a timestep and the one after it, with
#'                    the final value carried forward. Applied before the
#'                    start_of_timestep shift.
#' @param ordinates optional numeric vector or matrix (`ordinates x zones`) of
#'   unit hydrograph ordinates in `cfs / in` at `dt_hours`, overriding UH rows
#'   in `pars` when supplied.
#' @return Vector (when `sum_zones = TRUE`) or matrix (when `sum_zones = FALSE`)
#'   of routed flow in cfs
#' @export
#'
#' @examples
#' # Simple: full model chain via the bundled example; routed flow per zone
#' run <- load_example("NRKW1")
#' head(run$sacsnow_sf)
#'
#' # Low-level: run FA -> SAC-SMA/SNOW17 to get TCI, then route it with UH.
#' data(nrkw1_forcing)
#' data(nrkw1_pars)
#' dt_hours <- 6
#' forcing_adj <- fa_nwrfc(dt_hours, nrkw1_forcing, nrkw1_pars)
#' tci <- sac_snow(dt_hours, forcing_adj, nrkw1_pars)
#' flow_cfs <- uh(dt_hours, tci, nrkw1_pars)
#' @useDynLib nwsrfsr duamel_
uh = function(
  dt_hours,
  tci,
  pars,
  sum_zones = TRUE,
  start_of_timestep = TRUE,
  backfill = TRUE,
  return_inst = TRUE,
  ordinates = NULL
) {
  dt_hours = .integer_hours(dt_hours, "dt_hours")
  sec_per_day = 86400
  dt_seconds = dt_hours * 3600
  dt_days = dt_hours / 24

  if (is.null(dim(tci))) {
    tci = matrix(as.double(tci), ncol = 1L)
  } else {
    tci_cols = colnames(tci)
    tci = as.matrix(tci)
    storage.mode(tci) = "double"
    colnames(tci) = tci_cols
  }
  if (any(!is.finite(tci))) {
    stop("uh(): 'tci' must contain only finite numeric values.", call. = FALSE)
  }

  pars = as.data.frame(pars)
  n_tci_zones = ncol(tci)
  sim_length = nrow(tci)
  tci_names = colnames(tci)

  uh_rows = if ("type" %in% names(pars) && any(pars$type == "uh")) {
    pars[pars$type == "uh", , drop = FALSE]
  } else {
    pars
  }

  get_tci_areas = function() {
    sac_zones = if (all(c("zone", "type") %in% names(pars))) {
      sort(unique(pars$zone[grepl("-", pars$zone) & pars$type == "sac"]))
    } else {
      character(0)
    }
    if (!is.null(tci_names) && "zone" %in% names(uh_rows) && all(tci_names %in% uh_rows$zone)) {
      if (length(sac_zones) == n_tci_zones && !setequal(tci_names, sac_zones)) {
        stop("tci must contain exactly one column per SAC-Snow zone.", call. = FALSE)
      }
      .align_zone_values(uh_rows, "zone_area", n_tci_zones, tci_names, "uh", "uh()")
    } else if (length(sac_zones) == n_tci_zones && "zone" %in% names(uh_rows)) {
      .align_zone_values(uh_rows, "zone_area", n_tci_zones, sac_zones, "uh", "uh()")
    } else {
      vals = as.double(uh_rows$value[uh_rows$name == "zone_area"])
      if (length(vals) != n_tci_zones || any(!is.finite(vals))) {
        stop("uh(): 'zone_area' must supply one finite value per tci zone.", call. = FALSE)
      }
      vals
    }
  }

  if (!is.null(ordinates)) {
    uh_type = "ordinate"
    ord_mat = if (is.null(dim(ordinates))) {
      matrix(as.double(ordinates), ncol = 1L)
    } else {
      m_tmp = as.matrix(ordinates)
      storage.mode(m_tmp) = "double"
      m_tmp
    }
    tci_areas = get_tci_areas()
    if (any(tci_areas <= 0)) {
      stop("uh(): 'zone_area' must contain values > 0.", call. = FALSE)
    }
    if (ncol(ord_mat) == 1L && n_tci_zones > 1L) {
      weights = tci_areas / sum(tci_areas)
      tci_route = matrix(as.numeric(tci %*% weights), ncol = 1L)
      uh_areas = sum(tci_areas)
      route_names = colnames(ord_mat)
    } else if (ncol(ord_mat) == n_tci_zones) {
      tci_route = tci
      uh_areas = tci_areas
      route_names = if (!is.null(colnames(ord_mat))) colnames(ord_mat) else tci_names
    } else {
      stop(
        "uh(): 'ordinates' must have either 1 shared column or ",
        n_tci_zones,
        " zone columns.",
        call. = FALSE
      )
    }
    .validate_ordinate_matrix(ord_mat, uh_areas, dt_hours)
  } else {
    spec = .detect_uh_spec(pars)
    uh_type = spec$uh_type
    uh_zones = spec$uh_zone_names
    tci_areas = get_tci_areas()
    if (any(tci_areas <= 0)) {
      stop("uh(): 'zone_area' must contain values > 0.", call. = FALSE)
    }

    is_shared = (length(uh_zones) == 1L) && (n_tci_zones > 1L)
    if (is_shared) {
      if (length(spec$sac_zones) > 0L && n_tci_zones != length(spec$sac_zones)) {
        stop("tci must contain exactly one column per SAC-Snow zone.", call. = FALSE)
      }
      weights = tci_areas / sum(tci_areas)
      tci_route = matrix(as.numeric(tci %*% weights), ncol = 1L)
      uh_areas = sum(tci_areas)
      route_names = uh_zones
    } else {
      tci_route = tci
      uh_areas = tci_areas
      if (!is.null(tci_names) && length(uh_zones) > 0L && all(tci_names %in% uh_zones)) {
        route_names = tci_names
      } else if (length(uh_zones) == n_tci_zones) {
        route_names = uh_zones
      } else if (length(uh_zones) == 0L) {
        route_names = NULL
      } else {
        stop(
          "tci must have ",
          length(uh_zones),
          " zone columns matching UH parameter sets.",
          call. = FALSE
        )
      }
    }

    if (uh_type == "ordinate") {
      ord_mat = .extract_ordinate_matrix(pars, route_names)
      .validate_ordinate_matrix(ord_mat, uh_areas, dt_hours)
    }
  }

  n_route = ncol(tci_route)
  flow_cfs = if (sum_zones) {
    numeric(sim_length)
  } else {
    matrix(0, nrow = sim_length, ncol = n_route, dimnames = list(NULL, route_names))
  }

  if (uh_type == "gamma") {
    m = 1000L
    n = sim_length + m
    u1_dummy = single(m)
    by_z = !is.null(route_names) && length(route_names) == n_route
    for (i in seq_len(n_route)) {
      ss = .gamma_scale_for_zone(uh_rows, if (by_z) route_names[i] else i, by_zone = by_z)
      routed = .Fortran(
        "duamel",
        tci = as.single(tci_route[, i]),
        u1 = u1_dummy,
        as.single(ss$shape),
        as.single(ss$scale),
        as.single(dt_days),
        as.integer(n),
        as.integer(m),
        1L,
        0L,
        qr = single(n)
      )
      zone_flow = routed$qr[seq_len(sim_length)] *
        1000 *
        3.28084^3 /
        dt_seconds *
        uh_areas[i]
      if (sum_zones) {
        flow_cfs = flow_cfs + zone_flow
      } else {
        flow_cfs[, i] = zone_flow
      }
    }
  } else {
    for (i in seq_len(n_route)) {
      u1 = ord_mat[, i]
      u1 = u1[!is.na(u1)]
      m_i = length(u1)
      n_i = sim_length + m_i
      routed = .Fortran(
        "duamel",
        tci = as.single(tci_route[, i]),
        u1 = as.single(u1),
        as.single(0),
        as.single(0),
        as.single(dt_days),
        as.integer(n_i),
        as.integer(m_i),
        0L,
        0L,
        qr = single(n_i)
      )
      zone_flow = routed$qr[seq_len(sim_length)] / 25.4
      if (sum_zones) {
        flow_cfs = flow_cfs + zone_flow
      } else {
        flow_cfs[, i] = zone_flow
      }
    }
  }

  if (!return_inst && sim_length > 0L) {
    if (sum_zones) {
      flow_cfs = (flow_cfs + c(flow_cfs[-1L], flow_cfs[sim_length])) / 2
    } else {
      flow_cfs = (flow_cfs + rbind(flow_cfs[-1L, , drop = FALSE], flow_cfs[sim_length, , drop = FALSE])) / 2
    }
  }

  if (start_of_timestep && sim_length > 0L) {
    if (sum_zones) {
      c(
        if (backfill) flow_cfs[1L] else NA_real_,
        if (sim_length > 1L) flow_cfs[1L:(sim_length - 1L)] else numeric(0)
      )
    } else {
      first_row = if (backfill) flow_cfs[1L, , drop = FALSE] else matrix(NA_real_, 1L, n_route, dimnames = list(NULL, route_names))
      out_mat = if (sim_length > 1L) {
        rbind(first_row, flow_cfs[1L:(sim_length - 1L), , drop = FALSE])
      } else {
        first_row
      }
      rownames(out_mat) = NULL
      out_mat
    }
  } else {
    flow_cfs
  }
}

# Internal: number of chanloss modules described by pars. The "n_clmods" row is
# the authoritative count when present, but it is dropped from the pars carried
# on an "nwsrfs_run" object (Python drops it too), so fall back to counting the
# cl_factor_## rows.
.n_clmods = function(pars) {
  n = pars[pars$name == "n_clmods", ]$value[1L]
  if (!is.na(n)) {
    return(n)
  }
  sum(grepl("^cl_factor_[0-9]+$", pars$name))
}


#' Seasonal chanloss
#'
#' @param flow streamflow vector
#' @param forcing forcing data
#' @param dt_hours timestep in hours (1, 2, 3, 4, 6, 8, 12, or 24)
#' @param pars parameters
#' @return Vector of flow modified by the chanloss pattern
#' @export
#'
#' @useDynLib nwsrfsr chanloss_
chanloss = function(flow, forcing, dt_hours, pars) {
  pars = as.data.frame(pars)
  ts_info = .validate_ts_list(forcing, dt_hours = dt_hours, supported = .supported_dt_hours, label = "forcing")
  dt_hours = ts_info$dt_hours
  sim_length = nrow(forcing[[1L]])

  flow = as.double(flow)
  if (length(flow) < sim_length || any(!is.finite(flow[seq_len(sim_length)]))) {
    stop("chanloss(): 'flow' must contain ", sim_length, " finite values.", call. = FALSE)
  }
  if (any(flow[seq_len(sim_length)] < 0)) {
    stop("chanloss(): 'flow' values must be >= 0.", call. = FALSE)
  }

  n_clmods = .n_clmods(pars)
  if (is.na(n_clmods) || n_clmods <= 0) {
    return(flow[seq_len(sim_length)])
  }

  cl_type = pars[pars$name == "cl_type", ]$value[1L]
  cl_min_q = pars[pars$name == "cl_min_q", ]$value[1L]
  if (is.na(cl_type)) {
    cl_type = 1L
  }
  if (!is.finite(cl_type) || !(cl_type %in% c(1, 2))) {
    stop("chanloss(): 'cl_type' must be 1 or 2.", call. = FALSE)
  }
  if (!is.finite(cl_min_q)) {
    stop("chanloss(): 'cl_min_q' must be a finite numeric value.", call. = FALSE)
  }

  cl_factors = numeric(n_clmods)
  cl_periods = matrix(NA_integer_, n_clmods, 2L)
  for (i in seq_len(n_clmods)) {
    p_start = pars[pars$name == sprintf("cl_period_start_%02d", i), ]$value
    p_end = pars[pars$name == sprintf("cl_period_end_%02d", i), ]$value
    fac = pars[pars$name == sprintf("cl_factor_%02d", i), ]$value
    if (length(p_start) != 1L || length(p_end) != 1L || length(fac) != 1L ||
        !is.finite(p_start) || !is.finite(p_end) || !is.finite(fac) ||
        p_start != floor(p_start) || p_end != floor(p_end)) {
      stop(sprintf("chanloss(): missing, duplicate, or invalid parameters for module %02d.", i), call. = FALSE)
    }
    cl_periods[i, 1L] = as.integer(p_start)
    cl_periods[i, 2L] = as.integer(p_end)
    cl_factors[i] = as.double(fac)
  }
  if (any(cl_periods < 1L | cl_periods > 12L)) {
    stop("chanloss(): period start/end months must be integers from 1 to 12.", call. = FALSE)
  }

  cl_flow = .Fortran(
    "chanloss",
    n_clmods = as.integer(n_clmods),
    dt = as.integer(dt_hours * 3600L),
    sim_length = as.integer(sim_length),
    year = as.integer(forcing[[1L]]$year)[seq_len(sim_length)],
    month = as.integer(forcing[[1L]]$month)[seq_len(sim_length)],
    day = as.integer(forcing[[1L]]$day)[seq_len(sim_length)],
    factor = as.double(cl_factors),
    period = as.integer(cl_periods),
    cl_type = as.integer(cl_type),
    min_q = as.double(cl_min_q),
    sim = as.double(flow[seq_len(sim_length)]),
    sim_adj = double(sim_length)
  )

  cl_flow$sim_adj
}

#' Daily consuse model
#'
#' @param input A data frame (or matrix with col names), must have
#' columns: flow, pet (units of mm), year, month, day
#' @param pars model parameters in the same format as the sac and snow models, with type=='consuse'
#' @param cfs if TRUE, then flow units of cfs are expected, if FALSE then cms are expected.
#'
#' @return data frame with consuse variables (or named list of data frames for multiple CU zones)
#' @export
#'
#' @useDynLib nwsrfsr consuse_
consuse = function(input, pars, cfs = TRUE) {
  input = as.data.frame(input)
  pars = as.data.frame(pars)
  .infer_and_validate_ts(input, dt_hours = 24L, require_hour = FALSE, supported = 24L, label = "consuse input")

  if (!all(c("flow", "pet") %in% names(input))) {
    stop("consuse(): 'input' must contain 'flow' and 'pet' columns.", call. = FALSE)
  }
  pet_in = as.double(input$pet)
  qnat_in = as.double(input$flow * ifelse(cfs, 1, 0.028316847))
  if (any(!is.finite(pet_in)) || any(!is.finite(qnat_in))) {
    stop("consuse(): 'flow' and 'pet' must contain finite values.", call. = FALSE)
  }
  if (any(pet_in < 0) || any(qnat_in < 0)) {
    stop("consuse(): 'flow' and 'pet' values must be >= 0.", call. = FALSE)
  }

  cu_pars = pars[pars$type == "consuse", , drop = FALSE]
  cu_zones = sort(unique(cu_pars$zone[grepl("-CU", cu_pars$zone)]))
  if (length(cu_zones) == 0L) {
    cu_zones = sort(unique(pars$zone[grepl("-CU", pars$zone)]))
  }
  sim_length = as.integer(nrow(input))

  cu_out = list()
  for (cu_zone in cu_zones) {
    z_cu = cu_pars[cu_pars$zone == cu_zone, , drop = FALSE]
    cu_month_names = sprintf("peadj_cu_%02d", 1L:12L)
    if (all(cu_month_names %in% z_cu$name)) {
      peadj_m = as.double(z_cu$value[match(cu_month_names, z_cu$name)])
    } else {
      peadj_m = as.double(z_cu$value[substr(z_cu$name, 1L, 5L) == "peadj"])
    }
    if (length(peadj_m) != 12L || any(!is.finite(peadj_m)) || any(peadj_m < 0)) {
      stop("consuse(): 'peadj_m' must contain 12 finite values >= 0 for zone ", cu_zone, ".", call. = FALSE)
    }

    get_cu_scalar = function(nms, src = z_cu) {
      vals = src$value[src$name %in% nms]
      if (length(vals) != 1L || !is.finite(vals)) {
        stop(
          "consuse(): expected 1 finite value for ",
          paste(nms, collapse = "/"),
          " in zone ",
          cu_zone,
          ".",
          call. = FALSE
        )
      }
      as.double(vals)
    }

    area_in = get_cu_scalar("area_km2")
    eff_in = get_cu_scalar("irr_eff")
    mflow_in = get_cu_scalar(c("min_flow_cmsd", "min_flow"))
    accum_in = get_cu_scalar("rf_accum_rate")
    decay_in = get_cu_scalar("rf_decay_rate")
    peadj_in = get_cu_scalar("peadj", pars[pars$zone == cu_zone & pars$name == "peadj", , drop = FALSE])

    if (any(c(area_in, eff_in, accum_in, decay_in, peadj_in) <= 0)) {
      stop("consuse(): area_km2, irr_eff, rf_accum_rate, rf_decay_rate, and peadj must be > 0.", call. = FALSE)
    }
    if (mflow_in < 0) {
      stop("consuse(): min_flow must be >= 0.", call. = FALSE)
    }
    if (max(eff_in, accum_in, decay_in) > 1) {
      stop("consuse(): irr_eff, rf_accum_rate, and rf_decay_rate must be <= 1.", call. = FALSE)
    }

    x = .Fortran(
      "consuse",
      sim_length = sim_length,
      year = as.integer(input$year),
      month = as.integer(input$month),
      day = as.integer(input$day),
      AREA_in = area_in,
      EFF_in = eff_in,
      MFLOW_in = mflow_in,
      ACCUM_in = accum_in,
      DECAY_in = decay_in,
      peadj_m = peadj_m,
      peadj = peadj_in,
      PET_in = pet_in,
      QNAT_in = qnat_in,
      QADJ_out = numeric(sim_length),
      QDIV_out = numeric(sim_length),
      QRFIN_out = numeric(sim_length),
      QRFOUT_out = numeric(sim_length),
      QOL_out = numeric(sim_length),
      QCD_out = numeric(sim_length),
      CE_out = numeric(sim_length),
      RFSTOR_out = numeric(sim_length)
    )
    cu_out[[cu_zone]] = data.frame(
      year = x$year,
      month = x$month,
      day = x$day,
      qnat = x$QNAT_in,
      qadj = x$QADJ_out,
      qdiv = x$QDIV_out,
      qrfin = x$QRFIN_out,
      qrfout = x$QRFOUT_out,
      qol = x$QOL_out,
      qcd = x$QCD_out,
      ce = x$CE_out,
      rfstor = x$RFSTOR_out
    )
  }
  if (length(cu_zones) == 1L) {
    cu_out[[1L]]
  } else {
    cu_out
  }
}


#' Lag-K Routing for any number of upstream points
#'
#' @param dt_hours timestep in hours (1, 2, 3, 4, 6, 8, 12, or 24)
#' @param uptribs a list of data.frames where each element contains `year`, `month`, `day`, `hour`, and `flow_cfs` for an upstream point
#' @param pars parameters
#' @param sum_routes add all routed values together or leave separate
#' @param return_states return the lagk states
#'
#' @return vector, matrix, or data.frame of routed flows
#' @export
#'
#' @examples NULL
#' @useDynLib nwsrfsr lagk_
lagk = function(dt_hours, uptribs, pars, sum_routes = TRUE, return_states = FALSE) {
  pars = as.data.frame(pars)
  ts_info = .validate_ts_list(uptribs, dt_hours = dt_hours, supported = .supported_dt_hours, label = "uptribs")
  dt_hours = ts_info$dt_hours

  n_uptribs = length(uptribs)
  sim_length = nrow(uptribs[[1L]])
  reach_names = names(uptribs)

  get_lk = function(nm) {
    .align_zone_values(pars, nm, n_uptribs, reach_names, "lagk", "lagk()")
  }

  lagtbl_a = get_lk("lagtbl_a")
  lagtbl_b = get_lk("lagtbl_b")
  lagtbl_c = get_lk("lagtbl_c")
  lagtbl_d = get_lk("lagtbl_d")
  ktbl_a = get_lk("ktbl_a")
  ktbl_b = get_lk("ktbl_b")
  ktbl_c = get_lk("ktbl_c")
  ktbl_d = get_lk("ktbl_d")
  lagmax = get_lk("lagk_lagmax")
  lagmin = get_lk("lagk_lagmin")
  kmax = get_lk("lagk_kmax")
  kmin = get_lk("lagk_kmin")
  qmax = get_lk("lagk_qmax")
  qmin = get_lk("lagk_qmin")
  ico = get_lk("init_co")
  iinfl = get_lk("init_if")
  ioutfl = get_lk("init_of")
  istor = get_lk("init_stor")

  if (any(c(lagmax, lagmin, kmax, kmin, qmax, qmin) < 0)) {
    stop("lagk(): table min/max limits for lag, k, and q must be >= 0.", call. = FALSE)
  }
  if (any(lagmax < lagmin)) {
    stop("lagk(): lagk_lagmax must be >= lagk_lagmin.", call. = FALSE)
  }
  if (any(kmax < kmin)) {
    stop("lagk(): lagk_kmax must be >= lagk_kmin.", call. = FALSE)
  }
  if (any(qmax <= qmin)) {
    stop("lagk(): lagk_qmax must be strictly greater than lagk_qmin.", call. = FALSE)
  }
  if (any(c(ico, iinfl, ioutfl, istor) < 0)) {
    stop("lagk(): initial states (init_co, init_if, init_of, init_stor) must be >= 0.", call. = FALSE)
  }

  qa_in = do.call("cbind", lapply(uptribs, \(x) as.double(x[["flow_cfs"]])))
  if (any(!is.finite(qa_in))) {
    stop("lagk(): upstream 'flow_cfs' must contain only finite values.", call. = FALSE)
  }
  if (any(qa_in < 0)) {
    stop("lagk(): upstream 'flow_cfs' values must be >= 0.", call. = FALSE)
  }

  lagk_out = matrix(0, sim_length, n_uptribs)

  routed = .Fortran(
    "lagk",
    n_hrus = as.integer(n_uptribs),
    ita = as.integer(dt_hours),
    itb = as.integer(dt_hours),
    lagtbl_a_in = lagtbl_a,
    lagtbl_b_in = lagtbl_b,
    lagtbl_c_in = lagtbl_c,
    lagtbl_d_in = lagtbl_d,
    ktbl_a_in = ktbl_a,
    ktbl_b_in = ktbl_b,
    ktbl_c_in = ktbl_c,
    ktbl_d_in = ktbl_d,
    lagk_lagmax_in = lagmax,
    lagk_kmax_in = kmax,
    lagk_qmax_in = qmax,
    lagk_lagmin_in = lagmin,
    lagk_kmin_in = kmin,
    lagk_qmin_in = qmin,
    ico_in = ico,
    iinfl_in = iinfl,
    ioutfl_in = ioutfl,
    istor_in = istor,
    qa_in = qa_in,
    sim_length = as.integer(sim_length),
    return_states = as.logical(return_states),
    lagk_out = lagk_out,
    co_st_out = lagk_out,
    inflow_st_out = lagk_out,
    storage_st_out = lagk_out
  )

  if (isTRUE(return_states)) {
    return_vars = c(
      "lagk_out" = "routed",
      "co_st_out" = "lag_time",
      "inflow_st_out" = "k_inflow",
      "storage_st_out" = "k_storage"
    )
    df = uptribs[[1L]][, c("year", "month", "day", "hour")]
    for (i in seq_len(n_uptribs)) {
      for (name in names(return_vars)) {
        df[[paste0(return_vars[name], "_", i)]] = routed[[name]][, i]
      }
    }
    return(df)
  }

  if (sum_routes && n_uptribs > 1L) {
    rowSums(routed$lagk_out)
  } else if (n_uptribs > 1L) {
    out_mat = routed$lagk_out
    if (!is.null(reach_names)) {
      colnames(out_mat) = reach_names
    }
    out_mat
  } else {
    as.vector(routed$lagk_out)
  }
}


#' Conputes surface pressure in hPa from a given elevation
#'
#' @param elev surface elevation in meters
#'
#' @return Surface pressure in hPa
#' @export
#'
#' @examples
#' sp <- sfc_pressure(0)
sfc_pressure <- function(elev) {
  a <- 33.86
  b <- 29.9
  c <- 0.335
  d <- 0.00022
  e <- 2.4
  # sfc pres in hPa
  a * (b - (c * (elev / 100)) + (d * ((elev / 100)^e)))
}


#' Daily Potential Evapotranspiration using Hargreaves-Semani equations
#'
#' @param lat Latitude in decimal degrees
#' @param jday Julian day (Day of year since Jan 1)
#' @param tave Average daily temperature (C)
#' @param tmax Max daily temperature (C)
#' @param tmin Min daily temerature (C)
#'
#' @return Daily PET (vectorized over all inputs)
#' @export
#'
#' @examples
#' pet <- pet_hs(42, 200, 20, 25, 15)
pet_hs <- function(lat, jday, tave, tmax, tmin) {
  # Calculate extraterrestrial radiation
  # Inverse Relative Distance Earth to Sun
  d_r <- 1 + 0.033 * cos((2 * pi) / 365 * jday)
  # Solar Declination
  rho <- 0.409 * sin((2 * pi) / 365 * jday - 1.39)
  # Sunset Hour
  omega_s <- acos(-tan(lat * pi / 180) * tan(rho))
  # Extraterrestrial Radiation (MJm^-2*day^-1)
  r_e <- (24 * 60) /
    pi *
    0.0820 *
    d_r *
    (omega_s * sin(lat * pi / 180) * sin(rho) + cos(lat * pi / 180) * cos(rho) * sin(omega_s))
  # mm
  0.0023 * (tave + 17.8) * (tmax - tmin)**0.5 * r_e / 2.45 / 4
}

#' Areal depeletion curve using a 3 parameter model
#'
#'    `adc=a*x^b+(1.0-a)*x^c`
#'
#' @param a a parameter (0<a<1)
#' @param b b parameter (b>=0)
#' @param c c parameter (c>=0)
#'
#' @return 11 element vector representing the ADC
#' @export
#'
#' @examples
#' adc <- adc3(.5, 0.1, 2)
adc3 <- function(a, b, c) {
  x <- seq(0, 1, by = 0.1)
  a * x^b + (1.0 - a) * x^c
}


#' Interpolate forcing adjustment factors
#'
#' This function interpolates 12 forcing adjustment factors (1 per month)
#' by placing them at the 15th of the month then interpolating between the
#' previous and next months values for every time step in between.
#'
#' @param factors 12 element vector of monthly adjustment factors
#' @param month a vector of month values for each time step
#' @param day a vector of day values for each time step
#' @param hour a vector of hour values for each time step
#'
#' @return A vector matching the length of month, containing interpolated adjustment factors
#' @export
#'
#' @examples
#' d1 <- as.POSIXct("2001-01-01 00:00:00", tz = "UTC")
#' d2 <- as.POSIXct("2001-12-31 18:00:00", tz = "UTC")
#' dates <- seq.POSIXt(d1, d2, by = "6 hours")
#'
#' month <- as.integer(format(dates, "%m"))
#' day <- as.integer(format(dates, "%d"))
#' hour <- as.integer(format(dates, "%H"))
#' factors <- c(.5, 2, 1, 1.5, 2, .5, 1, 2.5, 3, -1.5, 0, 1)
#'
#' ifa <- interp_fa(factors, month, day, hour)
#'
#' plot(dates, ifa, t = "l")
#' points(as.POSIXct(paste0("2001-", 1:12, "-15"), tz = "UTC"), factors, col = "red")
interp_fa <- function(factors, month, day, hour) {
  mdays <- c(
    Jan = 31L,
    Feb = 28L,
    Mar = 31L,
    Apr = 30L,
    May = 31L,
    Jun = 30L,
    Jul = 31L,
    Aug = 31L,
    Sep = 30L,
    Oct = 31L,
    Nov = 30L,
    Dec = 31L
  )
  # factors = c(.5,2,1,1.5,2,.5,1,2.5,3,-1.5,0,1)
  factors_prev <- c(factors[12], factors[1:11]) # c(1,.5,2,1,1.5,2,.5,1,2.5,3,-1.5,0)
  factors_next <- c(factors[2:12], factors[1]) # c(2,1,1.5,2,.5,1,2.5,3,-1.5,0,1,.5)
  factors_step <- dayi <- dayn <- numeric(length(month))

  dt_hours <- hour[2] - hour[1]
  interp_day <- 16 + dt_hours / 24

  for (i in 1:length(month)) {
    m <- month[i]
    decimal_day <- day[i] + hour[i] / 24
    if (decimal_day >= interp_day) {
      dayn[i] <- mdays[m]
      dayi[i] <- decimal_day - interp_day
      factors_step[i] <- factors[m] + dayi[i] / dayn[i] * (factors_next[m] - factors[m])
    } else if (decimal_day < interp_day & m == 1) {
      dayn[i] <- mdays[12]
      dayi[i] <- decimal_day - interp_day + mdays[12]
      factors_step[i] <- factors_prev[m] + dayi[i] / dayn[i] * (factors[m] - factors_prev[m])
    } else if (decimal_day < interp_day & m > 1) {
      dayn[i] <- mdays[m - 1]
      dayi[i] <- decimal_day - interp_day + mdays[m - 1]
      factors_step[i] <- factors_prev[m] + dayi[i] / dayn[i] * (factors[m] - factors_prev[m])
    }
  }
  factors_step
}


#' Adjust monthly climo based on 4 parameters
#'
#' description
#'
#' @param climo blah
#' @param pars blah
#' @param ll blah
#' @param ul blah
#' @param return_climo blah
#'
#' @return stuff
#' @export
#'
#' @examples
#' climo <- rep(2, 12)
#' pars <- c(.5, 0, 10, 0)
#' forcing_adjust_map_pet_ptps(climo, pars)
#' @importFrom stats dnorm median
forcing_adjust_map_pet_ptps <- function(
  climo,
  pars,
  ll = 0.9 * climo,
  ul = 1.1 * climo,
  return_climo = FALSE
) {
  scale <- pars[1]
  p_redist <- pars[2]
  sd <- pars[3]
  shift <- pars[4] # days

  # normal weights
  w <- dnorm(1:12, mean = 1, sd = sd)
  w <- rev(w / sum(w))

  # apply first scaling parameter
  climo_adj <- climo * scale
  # get the indexes of the sorted values to re-sort later
  climo_order <- order(order(climo_adj))
  # sort climo in ascending order to apply weights
  climo_adj <- sort(climo_adj)

  # percent of each month remaining before redistributing
  climo_remaining <- climo_adj * (1.0 - p_redist)
  # redistribute according to weights
  climo_redist <- climo_remaining + sum(climo_adj * p_redist) * w
  # re-sort to original order
  climo_adj <- climo_redist[climo_order]

  climo_interp <- numeric(14)
  if (shift != 0) {
    # apply shift
    climo_interp[1] <- climo_adj[12]
    climo_interp[2:13] <- climo_adj
    climo_interp[14] <- climo_adj[1]

    # interpolate between (x0,y0) and (x1,y1)
    # y = y0 + (x-x0)*(y1-y0)/(x1-x0)
    # interpolate between (month0,climo0) and (month1,climo1)
    # y = climo0 + ((month0+shift)-month0)*(climo1-climo0)/(month1-month0)
    # could get fancier here and account for the number of days in each month, but
    # 30 day months should be a reasonable approximation
    for (i in 1:12) {
      if (shift > 0) {
        climo_adj[i] <- climo_interp[i + 1] +
          shift * (climo_interp[i + 2] - climo_interp[i + 1]) / 30
      } else if (shift < 0) {
        climo_adj[i] <- climo_interp[i] +
          (30 - abs(shift)) * (climo_interp[i + 1] - climo_interp[i]) / 30
      }
    }
  }

  out <- numeric(12)
  # enforce limits
  for (i in 1:12) {
    if (climo_adj[i] > ul[i]) {
      climo_adj[i] <- ul[i]
    }
    if (climo_adj[i] < ll[i]) {
      climo_adj[i] <- ll[i]
    }
    if (climo[i] == 0) {
      out[i] <- 1
    } else {
      out[i] <- climo_adj[i] / climo[i]
    }
  }
  if (return_climo) {
    return(climo_adj)
  } else {
    return(out)
  }
  # out
}


#' Adjust monthly climo based on 4 parameters
#'
#' description
#'
#' @param climo blah
#' @param pars blah
#' @param ll blah
#' @param ul blah
#' @param return_climo blah
#'
#' @return stuff
#' @export
#'
#' @examples
#' climo <- rep(2, 12)
#' pars <- c(.5, 0, 10, 0)
#' forcing_adjust_mat(climo, pars)
#' @importFrom stats dnorm median
forcing_adjust_mat <- function(
  climo,
  pars,
  ll = climo * ifelse(climo > 0, 0.9, 1.1),
  ul = climo * ifelse(climo > 0, 1.1, 0.9),
  return_climo = FALSE
) {
  scale <- pars[1]
  p_redist <- pars[2]
  sd <- pars[3]
  shift <- pars[4] # days

  # normal weights
  w <- numeric(12)
  w[1:6] <- dnorm(1:6, mean = 1, sd = sd)
  w[7:12] <- rev(w[1:6])
  w <- w / sum(w) * 2

  # apply first scaling parameter
  climo_adj <- climo * scale
  med <- median(climo_adj)
  # get the indexes of the sorted values to re-sort later
  climo_order <- order(order(climo_adj))
  # sort climo in ascending order to apply weights
  climo_adj <- sort(climo_adj)
  # compute deviation from median
  climo_dev <- climo_adj - med

  # percent of each month remaining before redistributing
  climo_remaining <- climo_dev * (1.0 - p_redist)
  # get the total temperature to above the median and below median
  # write(*,*)climo_dev(1:6), p_redist
  climo_dist <- numeric(12)
  climo_dist[1:6] <- sum(climo_dev[1:6] * p_redist)
  climo_dist[7:12] <- sum(climo_dev[7:12] * p_redist)

  # redistribute according to weights
  climo_redist <- med + climo_remaining + climo_dist * w

  # re-sort to original order
  climo_adj <- climo_redist[climo_order]

  climo_interp <- numeric(14)
  if (shift != 0) {
    # apply shift
    climo_interp[1] <- climo_adj[12]
    climo_interp[2:13] <- climo_adj
    climo_interp[14] <- climo_adj[1]

    # interpolate between (x0,y0) and (x1,y1)
    # y = y0 + (x-x0)*(y1-y0)/(x1-x0)
    # interpolate between (month0,climo0) and (month1,climo1)
    # y = climo0 + ((month0+shift)-month0)*(climo1-climo0)/(month1-month0)
    # could get fancier here and account for the number of days in each month, but
    # 30 day months should be a reasonable approximation
    for (i in 1:12) {
      if (shift > 0) {
        climo_adj[i] <- climo_interp[i + 1] +
          shift * (climo_interp[i + 2] - climo_interp[i + 1]) / 30
      } else if (shift < 0) {
        climo_adj[i] <- climo_interp[i] +
          (30 - abs(shift)) * (climo_interp[i + 1] - climo_interp[i]) / 30
      }
    }
  }

  # enforce limits
  for (i in 1:12) {
    if (climo_adj[i] > ul[i]) {
      climo_adj[i] <- ul[i]
    }
    if (climo_adj[i] < ll[i]) climo_adj[i] <- ll[i]
  }

  out <- climo_adj - climo
  if (return_climo) {
    return(climo_adj)
  } else {
    return(out)
  }
}


# Internal: normalize an adjust/forcing_adj argument to the forcing types that
# should be adjusted. TRUE means all four, FALSE means none, and a character
# vector names the subset. Mirrors Python's NwsrfsRun._interrogate_fa_arg().
.fa_adjust_types <- function(adjust) {
  fa_types <- c("map", "mat", "ptps", "pet")

  if (is.logical(adjust)) {
    if (length(adjust) != 1 || is.na(adjust)) {
      stop("'forcing_adj' must be TRUE, FALSE, or a character vector of forcing types")
    }
    return(if (adjust) fa_types else character(0))
  }

  if (!is.character(adjust)) {
    stop("'forcing_adj' must be TRUE, FALSE, or a character vector of forcing types")
  }

  adjust <- unique(tolower(adjust))
  unknown <- setdiff(adjust, fa_types)
  if (length(unknown) > 0) {
    stop(
      "Forcing type(s) not understood: ",
      paste(unknown, collapse = ", "),
      ". Expecting: ",
      paste(fa_types, collapse = ", ")
    )
  }

  fa_types[fa_types %in% adjust]
}


#' Conduct NWRFC style forcing adjustments
#'
#' @param dt_hours timestep in hours (`1`, `2`, `3`, `4`, `6`, `8`, or `12`)
#' @param forcing named list of per-zone forcing `data.frame`s
#' @param pars parameter `data.frame`
#' @param climo optional `12 x 4` climatology matrix (`map`, `mat`, `pet`, `ptps`)
#' @param dry_run Do a run without any forcing adjustments, only compute pet and etd.
#'   Shorthand for `adjust = FALSE`; when TRUE it overrides `adjust`.
#' @param return_adj return monthly adjustment factors only
#' @param return_climo return the computed monthly climo
#' @param adjust Which forcings to adjust: TRUE for all of them, FALSE for none,
#'   or a character vector naming a subset of "map", "mat", "ptps" and "pet".
#'   Unadjusted forcings still pass through the routine, which computes pet and
#'   etd, but with neutral adjustment parameters.
#' @return Adjusted forcing list, monthly adjustment `data.frame`, or monthly
#'   climatology `data.frame`
#' @export
#'
#' @examples
#' data(nrkw1_forcing)
#' data(nrkw1_pars)
#' dt_hours <- 6
#' forcing_adj <- fa_nwrfc(dt_hours, nrkw1_forcing, nrkw1_pars)
#' @useDynLib nwsrfsr fa_ts_
fa_nwrfc = function(
  dt_hours,
  forcing,
  pars,
  climo = NULL,
  dry_run = FALSE,
  return_adj = FALSE,
  return_climo = FALSE,
  adjust = TRUE
) {
  if (return_adj && return_climo) {
    stop("Can only return adjustments or climo", call. = FALSE)
  }

  adjust_types = if (isTRUE(dry_run)) character(0) else .fa_adjust_types(adjust)
  pars = as.data.frame(pars)

  ts_info = .validate_ts_list(
    forcing,
    dt_hours = dt_hours,
    supported = .supported_fa_dt_hours,
    label = "forcing"
  )
  dt_hours = ts_info$dt_hours
  dt_seconds = dt_hours * 3600L

  n_zones = length(forcing)
  sim_length = nrow(forcing[[1L]])
  zone_names = names(forcing)

  req_cols = c("map_mm", "ptps", "mat_degc")
  for (z in seq_len(n_zones)) {
    miss_c = setdiff(req_cols, names(forcing[[z]]))
    if (length(miss_c) > 0L) {
      stop(
        sprintf(
          "fa_nwrfc(): forcing zone %s is missing column(s): %s.",
          if (!is.null(zone_names)) zone_names[z] else z,
          paste(miss_c, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  map_mat = do.call("cbind", lapply(forcing, \(df) as.double(df$map_mm)))
  ptps_mat = do.call("cbind", lapply(forcing, \(df) as.double(df$ptps)))
  mat_mat = do.call("cbind", lapply(forcing, \(df) as.double(df$mat_degc)))

  if (any(!is.finite(map_mat)) || any(!is.finite(ptps_mat)) || any(!is.finite(mat_mat))) {
    stop("fa_nwrfc(): forcing columns 'map_mm', 'ptps', and 'mat_degc' must contain finite values.", call. = FALSE)
  }
  if (any(map_mat < 0)) {
    stop("fa_nwrfc(): forcing 'map_mm' values must be >= 0.", call. = FALSE)
  }
  if (any(ptps_mat < 0 | ptps_mat > 1)) {
    stop("fa_nwrfc(): forcing 'ptps' values must be within [0, 1].", call. = FALSE)
  }

  if (is.null(climo)) {
    if (length(unique(forcing[[1L]]$month)) < 12L) {
      stop(
        "fa_nwrfc(): when 'climo' is NULL, forcing timestamps must include all 12 calendar months.",
        call. = FALSE
      )
    }
    climo_mat = matrix(-9999, nrow = 12L, ncol = 4L)
  } else {
    climo_mat = as.matrix(climo)
    storage.mode(climo_mat) = "double"
    if (!identical(dim(climo_mat), c(12L, 4L)) || any(!is.finite(climo_mat))) {
      stop("fa_nwrfc(): 'climo' must be a finite 12 x 4 numeric matrix.", call. = FALSE)
    }
    if (any(climo_mat[, c(1L, 3L, 4L)] < 0) || any(climo_mat[, 4L] > 1)) {
      stop(
        "fa_nwrfc(): 'climo' columns map, pet, and ptps must be >= 0 and ptps must be <= 1.",
        call. = FALSE
      )
    }
  }

  latitude = .align_zone_values(pars, "alat", n_zones, zone_names, "snow", "fa_nwrfc()")
  if (any(abs(latitude) > 90)) {
    stop("fa_nwrfc(): parameter 'alat' (latitude) must be within [-90, 90].", call. = FALSE)
  }
  area = .align_zone_values(pars, "zone_area", n_zones, zone_names, "uh", "fa_nwrfc()")
  if (any(area <= 0)) {
    stop("fa_nwrfc(): parameter 'zone_area' must be > 0.", call. = FALSE)
  }

  peadj_m = matrix(NA_real_, nrow = 12L, ncol = n_zones)
  for (m in 1:12) {
    peadj_m[m, ] = .align_zone_values(
      pars,
      sprintf("peadj_%02d", m),
      n_zones,
      zone_names,
      "sac",
      "fa_nwrfc()"
    )
  }
  if (any(peadj_m < 0)) {
    stop("fa_nwrfc(): 'peadj_01'..'peadj_12' values must be >= 0.", call. = FALSE)
  }

  get_fa_pars = function(f) {
    if (!(f %in% adjust_types)) {
      return(c(1, 0, 10, 0))
    }
    p_names = paste0(f, c("_scale", "_p_redist", "_std", "_shift"))
    vals = vapply(
      p_names,
      \(nm) {
        rows = pars[pars$name == nm, , drop = FALSE]
        if ("type" %in% names(rows) && any(rows$type == "fa")) {
          rows = rows[rows$type == "fa", , drop = FALSE]
        }
        if (nrow(rows) != 1L || !is.finite(rows$value[1L])) {
          stop(
            sprintf("fa_nwrfc(): parameter '%s' must supply exactly one finite value.", nm),
            call. = FALSE
          )
        }
        as.double(rows$value[1L])
      },
      numeric(1L)
    )
    if (vals[1L] <= 0) {
      stop(sprintf("fa_nwrfc(): '%s_scale' must be > 0.", f), call. = FALSE)
    }
    if (vals[2L] < 0 || vals[2L] > 1) {
      stop(sprintf("fa_nwrfc(): '%s_p_redist' must be in [0, 1].", f), call. = FALSE)
    }
    if (vals[3L] <= 0) {
      stop(sprintf("fa_nwrfc(): '%s_std' must be > 0.", f), call. = FALSE)
    }
    if (abs(vals[4L]) > 30) {
      stop(sprintf("fa_nwrfc(): '%s_shift' must be within [-30, 30].", f), call. = FALSE)
    }
    unname(vals)
  }

  get_fa_limits = function(f) {
    if (!(f %in% adjust_types)) {
      return(matrix(-999.0, nrow = 12L, ncol = 2L))
    }
    extract_bound = function(bound) {
      vapply(
        1:12,
        \(m) {
          nm = sprintf("%s_%s_%02d", f, bound, m)
          rows = pars[pars$name == nm, , drop = FALSE]
          if ("type" %in% names(rows) && any(rows$type == "fa_limit")) {
            rows = rows[rows$type == "fa_limit", , drop = FALSE]
          }
          if (!is.null(zone_names) && "zone" %in% names(rows) && zone_names[1L] %in% rows$zone) {
            rows = rows[rows$zone == zone_names[1L], , drop = FALSE]
            if (nrow(rows) != 1L || !is.finite(rows$value[1L])) {
              stop(
                sprintf(
                  "fa_nwrfc(): parameter '%s' must supply exactly one finite value for zone %s.",
                  nm,
                  zone_names[1L]
                ),
                call. = FALSE
              )
            }
            as.double(rows$value[1L])
          } else {
            if (nrow(rows) == 0L || !is.finite(rows$value[1L])) {
              stop(
                sprintf("fa_nwrfc(): parameter '%s' is missing or non-finite.", nm),
                call. = FALSE
              )
            }
            as.double(rows$value[1L])
          }
        },
        numeric(1L)
      )
    }
    lo = extract_bound("lower")
    hi = extract_bound("upper")
    if (any(lo > hi)) {
      stop(
        sprintf("fa_nwrfc(): '%s_lower_*' must be <= '%s_upper_*' for all 12 months.", f, f),
        call. = FALSE
      )
    }
    cbind(lo, hi, deparse.level = 0)
  }

  map_fa_pars = get_fa_pars("map")
  mat_fa_pars = get_fa_pars("mat")
  pet_fa_pars = get_fa_pars("pet")
  ptps_fa_pars = get_fa_pars("ptps")

  map_limits = get_fa_limits("map")
  mat_limits = get_fa_limits("mat")
  pet_limits = get_fa_limits("pet")
  ptps_limits = get_fa_limits("ptps")

  output_matrix = matrix(0, nrow = sim_length, ncol = n_zones)

  x = .Fortran(
    "fa_ts",
    n_hrus = as.integer(n_zones),
    dt = as.integer(dt_seconds),
    sim_length = as.integer(sim_length),
    year = as.integer(forcing[[1L]]$year),
    month = as.integer(forcing[[1L]]$month),
    day = as.integer(forcing[[1L]]$day),
    hour = as.integer(forcing[[1L]]$hour),
    latitude = latitude,
    area = area,
    peadj_m = peadj_m,
    map_fa_pars = map_fa_pars,
    mat_fa_pars = mat_fa_pars,
    pet_fa_pars = pet_fa_pars,
    ptps_fa_pars = ptps_fa_pars,
    map_fa_limits_in = map_limits,
    mat_fa_limits_in = mat_limits,
    pet_fa_limits_in = pet_limits,
    ptps_fa_limits_in = ptps_limits,
    climo = climo_mat,
    map = map_mat,
    ptps = ptps_mat,
    mat = mat_mat,
    map_adj = numeric(12L),
    mat_adj = numeric(12L),
    pet_adj = numeric(12L),
    ptps_adj = numeric(12L),
    map_fa = output_matrix,
    mat_fa = output_matrix,
    ptps_fa = output_matrix,
    pet_fa = output_matrix,
    etd = output_matrix
  )

  if (return_adj) {
    return(as.data.frame(do.call("cbind", x[c("map_adj", "mat_adj", "pet_adj", "ptps_adj")])))
  } else if (return_climo) {
    colnames(x$climo) = c("map", "mat", "pet", "ptps")
    return(as.data.frame(x$climo))
  } else {
    for (z in seq_len(n_zones)) {
      forcing[[z]]$map_mm = x$map_fa[, z]
      forcing[[z]]$mat_degc = x$mat_fa[, z]
      forcing[[z]]$ptps = x$ptps_fa[, z]
      forcing[[z]]$pet_mm = x$pet_fa[, z]
      forcing[[z]]$etd_mm = x$etd[, z]
    }
    return(forcing)
  }
}

#' Conduct NWRFC style forcing adjustments (return monthly adjustments or climo)
#'
#' @param dt_hours timestep in hours (`1`, `2`, `3`, `4`, `6`, `8`, or `12`)
#' @param forcing named list of per-zone forcing `data.frame`s
#' @param pars parameter `data.frame`
#' @param climo optional `12 x 4` climatology matrix
#' @param dry_run Do a run without any forcing adjustments, only compute pet and etd.
#'   Shorthand for `adjust = FALSE`; when TRUE it overrides `adjust`.
#' @param return_climo Return the computed climo, instead of adjustments
#' @param adjust Which forcings to adjust: TRUE for all of them, FALSE for none,
#'   or a character vector naming a subset of "map", "mat", "ptps" and "pet".
#' @return Monthly adjustment or climatology `data.frame`
#' @export
#'
#' @examples
#' data(nrkw1_forcing)
#' data(nrkw1_pars)
#' dt_hours <- 6
#' adj <- fa_adj_nwrfc(dt_hours, nrkw1_forcing, nrkw1_pars)
fa_adj_nwrfc = function(
  dt_hours,
  forcing,
  pars,
  climo = NULL,
  dry_run = FALSE,
  return_climo = FALSE,
  adjust = TRUE
) {
  if (return_climo) {
    fa_nwrfc(dt_hours, forcing, pars, climo, dry_run, return_climo = TRUE, adjust = adjust)
  } else {
    fa_nwrfc(dt_hours, forcing, pars, climo, dry_run, return_adj = TRUE, adjust = adjust)
  }
}

#' Replace ptps column with ptps derived using rain snow line code (lapse rate + MAT)
#'
#' @param forcing named list of per-zone forcing data frames (each with a
#'   \code{mat_degc} column). The list names are matched against the parameter
#'   \code{zone} column.
#' @param pars parameter data frame that supplies one value per forcing zone for
#'   each of \code{elev} (reference elevation, m), \code{talr} (temperature
#'   lapse rate, degC per 100 m) and \code{pxtemp} (rain/snow threshold
#'   temperature, degC). A \code{zone} column is used to align values with
#'   \code{forcing} when present.
#' @param ae_tbl data.frame with col1 containing quantile info and subsequent col with elev for each zone
#'
#' @return The \code{forcing} list with each zone's \code{ptps} column replaced
#'   by the value derived from the rsnwelev model.
#' @export
#'
#' @examples
#' # area_elev_curve is bundled for SFLN2 (2 zones); pair it with SFLN2 data.
#' data(sfln2_forcing)
#' data(sfln2_pars)
#' data(area_elev_curve)
#' # rsnwelev only needs the zones that match the area-elevation table,
#' # so drop the SFLN2-CU consumptive-use zone before calling.
#' forcing_zones <- sfln2_forcing[c("SFLN2-1", "SFLN2-2")]
#' # rsnwelev needs elev plus a temperature lapse rate (talr, degC/100m) and a
#' # rain/snow threshold (pxtemp, degC) per zone. The bundled calibration only
#' # carries elev, so add representative talr/pxtemp rows for the two zones.
#' zones <- c("SFLN2-1", "SFLN2-2")
#' rs_pars <- rbind(
#'   sfln2_pars[sfln2_pars$name == "elev" & sfln2_pars$zone %in% zones, ],
#'   data.frame(
#'     p_name = paste0(rep(c("talr_", "pxtemp_"), each = 2), zones),
#'     name = rep(c("talr", "pxtemp"), each = 2),
#'     type = "rsnwelev",
#'     zone = rep(zones, 2),
#'     value = c(0.5, 0.5, 1.0, 1.0)
#'   )
#' )
#' forcing_adj <- rsnwelev(forcing_zones, rs_pars, area_elev_curve)
#' @useDynLib nwsrfsr rsnwelev_
rsnwelev = function(forcing, pars, ae_tbl) {
  if (!is.list(forcing) || is.data.frame(forcing) || length(forcing) == 0L) {
    stop("rsnwelev(): 'forcing' must be a non-empty list of per-zone data.frames.", call. = FALSE)
  }
  if (all(c("year", "month", "day", "hour") %in% names(forcing[[1L]]))) {
    .validate_ts_list(forcing, dt_hours = NULL, supported = .supported_dt_hours, label = "forcing")
  } else {
    sim_len_chk = nrow(forcing[[1L]])
    if (is.null(sim_len_chk) || sim_len_chk < 1L) {
      stop("rsnwelev(): 'forcing' data.frames must contain at least 1 row.", call. = FALSE)
    }
    for (i in seq_along(forcing)) {
      if (!is.data.frame(forcing[[i]]) || nrow(forcing[[i]]) != sim_len_chk) {
        stop("rsnwelev(): all 'forcing' data.frames must have the same number of rows.", call. = FALSE)
      }
    }
  }

  pars = as.data.frame(pars)
  n_zones = length(forcing)
  sim_length = nrow(forcing[[1L]])
  zone_names = names(forcing)

  for (i in seq_len(n_zones)) {
    if (!"mat_degc" %in% names(forcing[[i]])) {
      stop("rsnwelev(): each forcing data.frame must contain 'mat_degc'.", call. = FALSE)
    }
  }
  mat_mat = do.call("cbind", lapply(forcing, \(df) as.double(df$mat_degc)))
  if (any(!is.finite(mat_mat))) {
    stop("rsnwelev(): 'mat_degc' must contain only finite numeric values.", call. = FALSE)
  }

  taelev_in = .align_zone_values(pars, "elev", n_zones, zone_names, "snow", "rsnwelev()")
  talr_in = .align_zone_values(pars, "talr", n_zones, zone_names, "rsnwelev", "rsnwelev()")
  pxtemp_in = .align_zone_values(pars, "pxtemp", n_zones, zone_names, "rsnwelev", "rsnwelev()")

  if (any(talr_in <= 0)) {
    stop("rsnwelev(): parameter 'talr' (lapse rate) must be strictly positive (> 0).", call. = FALSE)
  }
  if (any(taelev_in < 0)) {
    warning("rsnwelev(): negative reference elevation ('elev' < 0) encountered.", call. = FALSE)
  }

  ae_tbl = as.data.frame(ae_tbl)
  if (nrow(ae_tbl) < 2L || ncol(ae_tbl) < n_zones + 1L) {
    stop(
      sprintf(
        "rsnwelev(): 'ae_tbl' must have at least 2 rows and %d columns (quantile + %d zone elevations); got %d x %d.",
        n_zones + 1L,
        n_zones,
        nrow(ae_tbl),
        ncol(ae_tbl)
      ),
      call. = FALSE
    )
  }

  quantiles = as.double(ae_tbl[[1L]])
  if (any(!is.finite(quantiles)) || any(quantiles < 0 | quantiles > 1) || any(diff(quantiles) <= 0)) {
    stop(
      "rsnwelev(): first column of 'ae_tbl' must contain strictly ascending area fractions in [0, 1].",
      call. = FALSE
    )
  }

  elev_cols_names = names(ae_tbl)[-1L]
  if (!is.null(zone_names) && any(zone_names %in% elev_cols_names)) {
    miss_z = setdiff(zone_names, elev_cols_names)
    if (length(miss_z) > 0L) {
      stop(
        sprintf(
          "rsnwelev(): 'ae_tbl' is missing elevation column(s) for zone(s): %s.",
          paste(miss_z, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    elev_df = ae_tbl[, zone_names, drop = FALSE]
  } else if (!is.null(zone_names) && any(make.names(zone_names) %in% elev_cols_names)) {
    miss_z = zone_names[!make.names(zone_names) %in% elev_cols_names]
    if (length(miss_z) > 0L) {
      stop(
        sprintf(
          "rsnwelev(): 'ae_tbl' is missing elevation column(s) for zone(s): %s.",
          paste(miss_z, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    elev_df = ae_tbl[, make.names(zone_names), drop = FALSE]
  } else {
    elev_df = ae_tbl[, seq_len(n_zones) + 1L, drop = FALSE]
  }

  fortran_tbl = matrix(NA_real_, nrow = nrow(ae_tbl) * 2L, ncol = n_zones)
  for (i in seq_len(n_zones)) {
    vec_b = as.double(elev_df[[i]])
    if (any(!is.finite(vec_b)) || any(diff(vec_b) <= 0)) {
      stop(
        sprintf(
          "rsnwelev(): elevation column %d of 'ae_tbl' must contain strictly ascending finite values.",
          i + 1L
        ),
        call. = FALSE
      )
    }
    fortran_tbl[, i] = c(rbind(quantiles, vec_b))
  }

  output_matrix = matrix(0, nrow = sim_length, ncol = n_zones)

  ptps = .Fortran(
    "rsnwelev",
    n_hrus = as.integer(n_zones),
    sim_length = as.integer(sim_length),
    taelev_in = taelev_in,
    talr_in = talr_in,
    pxtemp_in = pxtemp_in,
    aetbl_len = as.integer(nrow(ae_tbl)),
    aetbl = as.double(fortran_tbl),
    mat_in = mat_mat,
    ptps_out = output_matrix
  )

  forcing_rsnwelev = forcing
  for (j in seq_len(n_zones)) {
    forcing_rsnwelev[[j]]$ptps = ptps$ptps_out[, j]
  }

  forcing_rsnwelev
}
