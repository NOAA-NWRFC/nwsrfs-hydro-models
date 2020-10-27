#' Execute SAC-SMA, SNOW17 and UH with given parameters
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with with columns for forcing inputs
#' @param pars sac parameters
#' @param forcing_adjust does the parameter set include forcing adjustments
#' @return Vector of routed flow in cfs
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' flow_cfs = sac_snow_uh(dt_hours,forcing, pars)
sac_snow_uh <- function(dt_hours, forcing, pars, forcing_adjust=FALSE){

  tci = sac_snow(dt_hours, forcing, pars, forcing_adjust = forcing_adjust)
  flow_cfs = uh(dt_hours, tci, pars)
  flow_cfs
}

#' Execute SAC-SMA, SNOW17, return total channel inflow per zone
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with with columns for forcing inputs
#' @param pars sac parameters
#' @param forcing_adjust does the parameter set include forcing adjustments
#' @return Matrix (1 column per zone) of unrouted channel inflow
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' tci = sac_snow(dt_hours, forcing, pars)
#' @useDynLib rfchydromodels sacsnow_
sac_snow <- function(dt_hours, forcing, pars, forcing_adjust=FALSE){

  sec_per_day = 86400
  dt_seconds = sec_per_day/(24/dt_hours)

  n_zones = length(forcing)
  sim_length = nrow(forcing[[1]])

  x = .Fortran('sacsnow',n_hrus=as.integer(n_zones),
                dt=as.integer(dt_seconds), sim_length=sim_length,
                year=as.integer(forcing[[1]]$year)[1:sim_length],
                month=as.integer(forcing[[1]]$month)[1:sim_length],
                day=as.integer(forcing[[1]]$day)[1:sim_length],
                hour=as.integer(forcing[[1]]$hour)[1:sim_length],
                # zone info
                latitude = pars[pars$name == 'alat',]$value,
                elev = pars[pars$name == 'elev',]$value,
                # sac parameters
                uztwm = pars[pars$name == 'uztwm',]$value,
                uzfwm = pars[pars$name == 'uzfwm',]$value,
                lztwm = pars[pars$name == 'lztwm',]$value,
                lzfpm = pars[pars$name == 'lzfpm',]$value,
                lzfsm = pars[pars$name == 'lzfsm',]$value,
                adimp = pars[pars$name == 'adimp',]$value,
                  uzk = pars[pars$name ==   'uzk',]$value,
                 lzpk = pars[pars$name ==  'lzpk',]$value,
                 lzsk = pars[pars$name ==  'lzsk',]$value,
                zperc = pars[pars$name == 'zperc',]$value,
                 rexp = pars[pars$name ==  'rexp',]$value,
                pctim = pars[pars$name == 'pctim',]$value,
                pfree = pars[pars$name == 'pfree',]$value,
                 riva = pars[pars$name ==  'riva',]$value,
                 side = pars[pars$name ==  'side',]$value,
                rserv = pars[pars$name == 'rserv',]$value,
                # pet and precp adjustments
                  peadj = rep(1,n_zones),
                  pxadj = rep(1,n_zones),
                peadj_m = matrix(1.0,nrow=12,ncol=n_zones),
                # snow parameters
                   scf = pars[pars$name ==    'scf',]$value,
                 mfmax = pars[pars$name ==  'mfmax',]$value,
                 mfmin = pars[pars$name ==  'mfmin',]$value,
                  uadj = pars[pars$name ==   'uadj',]$value,
                    si = pars[pars$name ==     'si',]$value,
                pxtemp = rep(0,n_zones),
                   nmf = pars[pars$name ==    'nmf',]$value,
                  tipm = pars[pars$name ==   'tipm',]$value,
                 mbase = pars[pars$name ==  'mbase',]$value,
                 plwhc = pars[pars$name ==  'plwhc',]$value,
                 daygm = pars[pars$name ==  'daygm',]$value,
                 adc_a = pars[pars$name ==  'adc_a',]$value,
                 adc_b = pars[pars$name ==  'adc_b',]$value,
                 adc_c = pars[pars$name ==  'adc_c',]$value,
                # forcing adjust parameters
                 map_adj = matrix(1,nrow=12,ncol=n_zones),
                 mat_adj = matrix(0,nrow=12,ncol=n_zones),
                 pet_adj = matrix(1,nrow=12,ncol=n_zones),
                ptps_adj = matrix(1,nrow=12,ncol=n_zones),
                # initial conditions
                  init_swe = pars[pars$name == 'init_swe',]$value  ,
                init_uztwc = pars[pars$name == 'init_uztwc',]$value,
                init_uzfwc = pars[pars$name == 'init_uzfwc',]$value,
                init_lztwc = pars[pars$name == 'init_lztwc',]$value,
                init_lzfsc = pars[pars$name == 'init_lzfsc',]$value,
                init_lzfpc = pars[pars$name == 'init_lzfpc',]$value,
                init_adimc = pars[pars$name == 'init_adimc',]$value,
                # forcings
                map = do.call('cbind',lapply(forcing,'[[','map_mm')),
                psfall = do.call('cbind',lapply(forcing,'[[','ptps')),
                mat = do.call('cbind',lapply(forcing,'[[','mat_degc')),
                # output
                tci = matrix(0,nrow=sim_length,ncol=n_zones))

  x$tci
}


#' Two parameter unit hydrograph routing for one or more basin zones
#'
#' @param dt_hours timestep in hours
#' @param tci channel inflow matrix, one column per zone
#' @param pars parameters
#' @return Vector of routed flow in cfs
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' tci = sac_snow(dt_hours,forcing, pars)
#' flow_cfs = uh(dt_hours, tci, pars)
#' @useDynLib rfchydromodels sacsnow_
uh <- function(dt_hours, tci, pars){

  sec_per_day = 86400
  dt_seconds = sec_per_day/(24/dt_hours)
  dt_days = dt_seconds / sec_per_day

  n_zones = ncol(tci)
  sim_length = nrow(tci)

  k = 1 # turns on 2 parameter UH
  m = 1000 # max unit hydro
  n = sim_length + m

  flow_cfs = numeric(sim_length)
  for(i in 1:n_zones){
    routed = .Fortran('duamel',
                 tci = as.single(tci[,i]),
                 as.single(pars[pars$name == 'unit_shape',]$value[i]),
                 as.single(pars[pars$name == 'unit_scale',]$value[i]),
                 as.single(dt_days),
                 as.integer(n),
                 as.integer(m),
                 1L,
                 0L,
                 qr = as.single(numeric(n)))

    # convert to cfs
    flow_cfs = flow_cfs +
      routed$qr[1:sim_length] * 1000 * 3.28084**3 / dt_seconds *
      pars[pars$name == 'hru_area',]$value[i]
  }

  flow_cfs
}



#' Title
#'
#' @param elev surface elevation in meters
#'
#' @return Surface pressure in hPa
#' @export
#'
#' @examples
#' sp = sfc_pressure(0)
sfc_pressure <- function(elev){
  a=33.86
  b=29.9
  c=0.335
  d=0.00022
  e=2.4
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
#' pet = pet_hs(42,200,20,25,15)
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
#' adc = adc3(.5,0.1,2)
adc3 <- function(a,b,c){
  x =seq(0,1,by=0.1)
  a*x^b+(1.0-a)*x^c
}
