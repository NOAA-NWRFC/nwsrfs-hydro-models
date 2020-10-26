#' Forcing data for 2 zone model of CHTO3
#'
#' A dataset containing 6 hour forcing data for the basin CHTO3 using 2 zones.
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

#' Parameters for a model of CHTO3
#'
#' A dataset containing parameters for a 2 zone SAC-SMA/SNOW17/UNITHG
#' model of the basin CHTO3 (6 hour timestep).
#'
#' @format A data.frame (or data.table if you have that loaded) with
#' 43823 rows and 4 columns:
#' \describe{
#'   \item{name}{The parameter name}
#'   \item{zone}{the zone name, matching the names of the forcing list elements}
#'   \item{value}{parameter value}
#'   \item{type}{The parameter type (model), either 'sac', 'snow', or 'uh'}
#' }
"pars"
