# %%
# --- NRKW1 example data ---

#' NRKW1 optimal parameters
#'
#' Optimal parameters from NWRFC autocalibration for Nooksack River at
#' North Cedarville, WA (USGS-12210700). 2-zone SAC-SMA/SNOW17/UH model
#' with 3-reach Lag-K routing (6 hour timestep).
#'
#' @format A data.frame with 5 columns:
#' \describe{
#'   \item{p_name}{Unique parameter identifier}
#'   \item{name}{Parameter name}
#'   \item{type}{Model type (sac, snow, uh, fa, lagk)}
#'   \item{zone}{Zone or upstream reach name}
#'   \item{value}{Parameter value}
#' }
#' @name nrkw1_pars
"nrkw1_pars"

#' NRKW1 forcing data
#'
#' 6-hour forcing data for Nooksack River at North Cedarville (NRKW1),
#' 2 zones, period of record.
#'
#' @format A named list of 2 data.frames (NRKW1-1, NRKW1-2), each with columns:
#' \describe{
#'   \item{year,month,day,hour}{Datetime columns}
#'   \item{map_mm}{Mean areal precipitation (mm)}
#'   \item{mat_degc}{Mean areal temperature (deg C)}
#'   \item{ptps}{Fraction of precipitation as snow}
#' }
#' @name nrkw1_forcing
"nrkw1_forcing"

#' NRKW1 upstream flow data
#'
#' 6-hour upstream flow timeseries for 3 upstream reaches (MFNW1, NFNW1, NSSW1)
#' routed via Lag-K to Nooksack River at North Cedarville.
#'
#' @format A named list of 3 data.frames, each with columns:
#' \describe{
#'   \item{year,month,day,hour}{Datetime columns}
#'   \item{flow_cfs}{Streamflow (cfs)}
#' }
#' @name nrkw1_upflow
"nrkw1_upflow"

#' NRKW1 observed daily flow
#'
#' Daily average observed streamflow at NRKW1.
#'
#' @format A data.frame with columns:
#' \describe{
#'   \item{year,month,day}{Date columns}
#'   \item{flow_cfs}{Daily average streamflow (cfs)}
#'   \item{Source}{Data source identifier}
#' }
#' @name nrkw1_daily_flow
"nrkw1_daily_flow"

#' NRKW1 observed instantaneous flow
#'
#' Instantaneous (6-hour) observed streamflow at NRKW1.
#'
#' @format A data.frame with columns:
#' \describe{
#'   \item{year,month,day,hour}{Datetime columns}
#'   \item{flow_cfs}{Instantaneous streamflow (cfs)}
#'   \item{Source}{Data source identifier}
#' }
#' @name nrkw1_inst_flow
"nrkw1_inst_flow"

# %%
# --- SFLN2 example data ---

#' SFLN2 optimal parameters
#'
#' Optimal parameters from NWRFC autocalibration for Salmon Falls Creek
#' NR San Jacinto NV (USGS-13105000). 2-zone SAC-SMA/SNOW17/UH model
#' with chanloss and consuse (6 hour timestep).
#'
#' @format A data.frame with 5 columns:
#' \describe{
#'   \item{p_name}{Unique parameter identifier}
#'   \item{name}{Parameter name}
#'   \item{type}{Model type (sac, snow, uh, fa, chanloss, consuse)}
#'   \item{zone}{Zone name}
#'   \item{value}{Parameter value}
#' }
#' @name sfln2_pars
"sfln2_pars"

#' SFLN2 forcing data
#'
#' 6-hour forcing data for Salmon Falls Creek (SFLN2), 2 zones plus
#' a consumptive use zone, period of record.
#'
#' @format A named list of 3 data.frames (SFLN2-1, SFLN2-2, SFLN2-CU),
#' each with columns:
#' \describe{
#'   \item{year,month,day,hour}{Datetime columns}
#'   \item{map_mm}{Mean areal precipitation (mm)}
#'   \item{mat_degc}{Mean areal temperature (deg C)}
#'   \item{ptps}{Fraction of precipitation as snow}
#' }
#' @name sfln2_forcing
"sfln2_forcing"

#' SFLN2 observed daily flow
#'
#' Daily average observed streamflow at SFLN2.
#'
#' @format A data.frame with columns:
#' \describe{
#'   \item{year,month,day}{Date columns}
#'   \item{flow_cfs}{Daily average streamflow (cfs)}
#'   \item{Source}{Data source identifier}
#' }
#' @name sfln2_daily_flow
"sfln2_daily_flow"

#' SFLN2 observed instantaneous flow
#'
#' Instantaneous (6-hour) observed streamflow at SFLN2.
#'
#' @format A data.frame with columns:
#' \describe{
#'   \item{year,month,day,hour}{Datetime columns}
#'   \item{flow_cfs}{Instantaneous streamflow (cfs)}
#'   \item{Source}{Data source identifier}
#' }
#' @name sfln2_inst_flow
"sfln2_inst_flow"

# %%
# --- Area-elevation curve (used by rsnwelev) ---

#' Area-elevation curve for the zones of SFLN2
#'
#' A dataset containing the percent area covered at a complete range of
#' elevations for the two zones of Salmon Falls Creek (SFLN2). Used as
#' input to [rsnwelev()].
#'
#' @format A data.frame with 21 rows and 3 columns:
#' \describe{
#'   \item{quantile}{Cumulative area fraction of the basin below the reference elevation}
#'   \item{SFLN2-1}{Reference elevations for zone 1 (ft)}
#'   \item{SFLN2-2}{Reference elevations for zone 2 (ft)}
#' }
#' @name area_elev_curve
"area_elev_curve"
