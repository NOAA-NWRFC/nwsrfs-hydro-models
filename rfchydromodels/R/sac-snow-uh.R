#' Execute SAC-SMA, SNOW17 and UH with given parameters
#'
#' @param forcing data frame with with columns for forcing inputs
#' @param sac_pars sac parameters
#' @param snow_pars snow parameters
#' @param uh_pars uh parameters
#' @param fa_pars (optional) uh parameters
#'
#' @return Vector of routed flow in cfs
#' @export
#'
#' @examples
sac_snow_uh <- function(forcing, sac_pars, snow_pars, uh_pars, fa_pars = NULL){

}

#' Title
#'
#' @param elev surface elevation in meters
#'
#' @return Surface pressure in hPa
#' @export
#'
#' @examples
sfc_pressure <- function(elev){
  a=33.86
  b=29.9
  c=0.335
  d=0.00022
  e=2.4
  # sfc pres in hPa
  pa = a * (b - (c * (elev / 100)) + (d * ((elev / 100)^e)))
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
pet_hs <- function(lat,jday,tave,tmax,tmin){
  # Calculate extraterrestrial radiation
  # Inverse Relative Distance Earth to Sun
  d_r=1+0.033*cos((2*pi)/365*jday)
  #Solar Declination
  rho=0.409*sin((2*pi)/365*jday-1.39)
  #Sunset Hour
  omega_s=acos(-tan(lat*pi/180)*tan(rho))
  #Extraterrestrial Radiation (MJm^-2*day^-1)
  r_e=(24*60)/pi*0.0820*d_r*(omega_s*sin(lat*pi/180)*sin(rho)+
                   cos(lat*pi/180)*cos(rho)*sin(omega_s))
  # mm
  0.0023*(tave+17.8)*(tmax-tmin)**0.5*r_e/2.45/4
}

#' Areal depeletion curve using a 3 parameter model
#'    `a*x^b+(1.0-a)*x^c`
#'
#' @param a a parameter (0<a<1)
#' @param b b parameter (b>=0)
#' @param c c parameter (c>=0)
#'
#' @return 11 element vector representing the ADC
#' @export
#'
#' @examples
adc3 <- function(a,b,c){
  x =seq(0,1,by=0.1)
  a*x^b+(1.0-a)*x^c
}
