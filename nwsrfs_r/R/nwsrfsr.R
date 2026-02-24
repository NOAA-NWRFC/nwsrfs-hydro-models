#' nwsrfsr: NWS River Forecast System Hydrologic Models
#'
#' R wrappers for NWSRFS Fortran hydrologic models used operationally by NOAA's
#' National Weather Service River Forecast Centers. Includes SAC-SMA soil moisture
#' accounting, SNOW-17 snow accumulation/ablation, gamma unit hydrograph routing,
#' Lag-K channel routing, channel loss/gain, and consumptive use modules.
#'
#' @section Low-level model wrappers:
#' Individual model components callable via Fortran interface:
#' [sac_snow()], [uh()], [lagk()], [chanloss()], [consuse()], [fa_nwrfc()]
#'
#' @section High-level orchestration:
#' [nwsrfs_run()] reads NWRFC autocalibration directories and runs the full
#' model chain. [load_example()] provides bundled example data for NRKW1 and SFLN2.
#'
#' @section AdjustQ preprocessing:
#' [adjustq()] replicates the CHPS FEWS AdjustQ tool for creating upstream
#' flow timeseries from observed and simulated data.
#'
#' @name nwsrfsr
#' @useDynLib nwsrfsr
"_PACKAGE"

utils::globalVariables(c(
  "nrkw1_pars", "nrkw1_forcing", "nrkw1_upflow",
  "nrkw1_daily_flow", "nrkw1_inst_flow",
  "sfln2_pars", "sfln2_forcing",
  "sfln2_daily_flow", "sfln2_inst_flow"
))
