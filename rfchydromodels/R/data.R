#' Forcing data for 2 zone model of SFLN2
#'
#' A dataset containing 6 hour forcing data for the basin SFLN2 using 2 zones.
#'
#'
#' @format A list containing two data.frames (or data.table if you have that loaded).
#' each data frame has 43823 rows and 7 columns:
#' \describe{
#'   \item{year,month,day,hour}{datetime columns}
#'   \item{map_mm}{Mean areal precip in mm}
#'   \item{mat_degc}{Mean areal temperature in degrees C}
#'   \item{ptps}{Precent precip as snow}
#' }
"forcing"

#' Parameters for a model of SFLN2
#'
#' A dataset containing parameters for a 2 zone NWSRFS model
#' model of the basin SFLN2 (6 hour timestep).
#'
#' @format A data.frame (or data.table if you have that loaded) with
#' 517 rows and 4 columns:
#' \describe{
#'   \item{name}{The parameter name}
#'   \item{zone}{the zone name, matching the names of the forcing list elements}
#'   \item{value}{parameter value}
#'   \item{type}{The parameter type (model)}
#' }
"pars"

#' Area Elevation Curve for the zones of SFLN2
#'
#' A dataset containing the percent area covered at a complete range of elevations
#' for a two zone configuration at SFLN2.
#'
#' @format A data.table with
#' 21 rows and 3 columns:
#' \describe{
#'   \item{quantile}{percent area of the basin below the reference elevation}
#'   \item{SFLN2-1}{reference elevations for zone 1}
#'   \item{SFLN2-2}{reference elevations for zone 2}
#' }
"area_elev_curve"
