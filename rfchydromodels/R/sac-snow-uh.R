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
sac_snow_uh <- function(dt_hours, forcing, pars, forcing_adjust=TRUE){

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
#' @importFrom stats reshape
sac_snow <- function(dt_hours, forcing, pars, forcing_adjust=TRUE){

  pars = as.data.frame(pars)

  sec_per_day = 86400
  dt_seconds = sec_per_day/(24/dt_hours)

  n_zones = length(forcing)
  sim_length = nrow(forcing[[1]])

  # limits are applied basin wide
  if(forcing_adjust){
    # using base R here to avoid package dependency
    map_lower = reshape(pars[grepl('map_lower',pars$name),c('name','zone','value')],
                      timevar='zone',idvar='name',direction='wide')[,-1]
    map_upper = reshape(pars[grepl('map_upper',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    mat_lower = reshape(pars[grepl('mat_lower',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    mat_upper = reshape(pars[grepl('mat_upper',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    pet_lower = reshape(pars[grepl('pet_lower',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    pet_upper = reshape(pars[grepl('pet_upper',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    ptps_lower = reshape(pars[grepl('ptps_lower',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    ptps_upper = reshape(pars[grepl('ptps_upper',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]

    # limits are applied basin wide
    if(n_zones == 1){
      map_limits = cbind(map_lower,map_upper)
      mat_limits = cbind(mat_lower,mat_upper)
      pet_limits = cbind(pet_lower,pet_upper)
      ptps_limits = cbind(ptps_lower,ptps_upper)
    }else{
      map_limits = cbind(map_lower[,1],map_upper[,1])
      mat_limits = cbind(mat_lower[,1],mat_upper[,1])
      pet_limits = cbind(pet_lower[,1],pet_upper[,1])
      ptps_limits = cbind(ptps_lower[,1],ptps_upper[,1])
    }

    # limits are applied basin wide
    map_fa_pars = c(pars[pars$name == 'map_scale',]$value[1],
                    pars[pars$name == 'map_p_redist',]$value[1],
                    pars[pars$name == 'map_std',]$value[1],
                    pars[pars$name == 'map_shift',]$value[1])
    mat_fa_pars = c(pars[pars$name == 'mat_scale',]$value[1],
                    pars[pars$name == 'mat_p_redist',]$value[1],
                    pars[pars$name == 'mat_std',]$value[1],
                    pars[pars$name == 'mat_shift',]$value[1])
    pet_fa_pars = c(pars[pars$name == 'pet_scale',]$value[1],
                    pars[pars$name == 'pet_p_redist',]$value[1],
                    pars[pars$name == 'pet_std',]$value[1],
                    pars[pars$name == 'pet_shift',]$value[1])
    ptps_fa_pars = c(pars[pars$name == 'ptps_scale',]$value[1],
                     pars[pars$name == 'ptps_p_redist',]$value[1],
                     pars[pars$name == 'ptps_std',]$value[1],
                     pars[pars$name == 'ptps_shift',]$value[1])

  }

  peadj_m = reshape(pars[grepl('peadj_',pars$name),c('name','zone','value')],
                    timevar='zone',idvar='name',direction='wide')[,-1]

  init = rbind(pars[pars$name == 'init_swe',]$value,
               pars[pars$name == 'init_uztwc',]$value,
               pars[pars$name == 'init_uzfwc',]$value,
               pars[pars$name == 'init_lztwc',]$value,
               pars[pars$name == 'init_lzfsc',]$value,
               pars[pars$name == 'init_lzfpc',]$value,
               pars[pars$name == 'init_adimc',]$value)


  x = .Fortran('sacsnow',
               n_hrus=as.integer(n_zones),
               dt=as.integer(dt_seconds),
               sim_length=sim_length,
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
               peadj_m = as.matrix(peadj_m),
               # snow parameters
                  scf = pars[pars$name ==    'scf',]$value,
                mfmax = pars[pars$name ==  'mfmax',]$value,
                mfmin = pars[pars$name ==  'mfmin',]$value,
                 uadj = pars[pars$name ==   'uadj',]$value,
                   si = pars[pars$name ==     'si',]$value,
                  nmf = pars[pars$name ==    'nmf',]$value,
                 tipm = pars[pars$name ==   'tipm',]$value,
                mbase = pars[pars$name ==  'mbase',]$value,
                plwhc = pars[pars$name ==  'plwhc',]$value,
                daygm = pars[pars$name ==  'daygm',]$value,
                adc_a = pars[pars$name ==  'adc_a',]$value,
                adc_b = pars[pars$name ==  'adc_b',]$value,
                adc_c = pars[pars$name ==  'adc_c',]$value,
               # forcing adjust parameters
               map_fa_pars = map_fa_pars,
               mat_fa_pars = mat_fa_pars,
               pet_fa_pars = pet_fa_pars,
               ptps_fa_pars = map_fa_pars,
               # forcing adjust limits
               map_fa_limits = map_limits,
               mat_fa_limits = mat_limits,
               pet_fa_limits = pet_limits,
               ptps_fa_limits = ptps_limits,
               # initial conditions
               init = init,
               # forcings
               map = do.call('cbind',lapply(forcing,'[[','map_mm')),
               ptps = do.call('cbind',lapply(forcing,'[[','ptps')),
               mat = do.call('cbind',lapply(forcing,'[[','mat_degc')),
               # output
               tci = matrix(0,nrow=sim_length,ncol=n_zones))

  x$tci
}

#' Execute SAC-SMA, SNOW17, return total channel inflow per zone, and model states
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with with columns for forcing inputs
#' @param pars sac parameters
#' @param forcing_adjust does the parameter set include forcing adjustments
#' @return data.frame (1 column per zone) of unrouted channel inflow (tci), sac states
#' uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc, and snow water equivalent (swe),
#' and adjusted forcing data.
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' states = sac_snow_states(dt_hours, forcing, pars)
#' @useDynLib rfchydromodels sacsnowstates_
#' @importFrom stats reshape
sac_snow_states <- function(dt_hours, forcing, pars, forcing_adjust=TRUE){

  pars = as.data.frame(pars)

  sec_per_day = 86400
  dt_seconds = sec_per_day/(24/dt_hours)

  n_zones = length(forcing)
  sim_length = nrow(forcing[[1]])


  # limits are applied basin wide
  if(forcing_adjust){
    # using base R here to avoid package dependency
    map_lower = reshape(pars[grepl('map_lower',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    map_upper = reshape(pars[grepl('map_upper',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    mat_lower = reshape(pars[grepl('mat_lower',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    mat_upper = reshape(pars[grepl('mat_upper',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    pet_lower = reshape(pars[grepl('pet_lower',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    pet_upper = reshape(pars[grepl('pet_upper',pars$name),c('name','zone','value')],
                        timevar='zone',idvar='name',direction='wide')[,-1]
    ptps_lower = reshape(pars[grepl('ptps_lower',pars$name),c('name','zone','value')],
                         timevar='zone',idvar='name',direction='wide')[,-1]
    ptps_upper = reshape(pars[grepl('ptps_upper',pars$name),c('name','zone','value')],
                         timevar='zone',idvar='name',direction='wide')[,-1]

    # limits are applied basin wide
    if(n_zones == 1){
      map_limits = cbind(map_lower,map_upper)
      mat_limits = cbind(mat_lower,mat_upper)
      pet_limits = cbind(pet_lower,pet_upper)
      ptps_limits = cbind(ptps_lower,ptps_upper)
    }else{
      map_limits = cbind(map_lower[,1],map_upper[,1])
      mat_limits = cbind(mat_lower[,1],mat_upper[,1])
      pet_limits = cbind(pet_lower[,1],pet_upper[,1])
      ptps_limits = cbind(ptps_lower[,1],ptps_upper[,1])
    }

    # limits are applied basin wide
    map_fa_pars = c(pars[pars$name == 'map_scale',]$value[1],
                    pars[pars$name == 'map_p_redist',]$value[1],
                    pars[pars$name == 'map_std',]$value[1],
                    pars[pars$name == 'map_shift',]$value[1])
    mat_fa_pars = c(pars[pars$name == 'mat_scale',]$value[1],
                    pars[pars$name == 'mat_p_redist',]$value[1],
                    pars[pars$name == 'mat_std',]$value[1],
                    pars[pars$name == 'mat_shift',]$value[1])
    pet_fa_pars = c(pars[pars$name == 'pet_scale',]$value[1],
                    pars[pars$name == 'pet_p_redist',]$value[1],
                    pars[pars$name == 'pet_std',]$value[1],
                    pars[pars$name == 'pet_shift',]$value[1])
    ptps_fa_pars = c(pars[pars$name == 'ptps_scale',]$value[1],
                     pars[pars$name == 'ptps_p_redist',]$value[1],
                     pars[pars$name == 'ptps_std',]$value[1],
                     pars[pars$name == 'ptps_shift',]$value[1])

  }

  peadj_m = reshape(pars[grepl('peadj_',pars$name),c('name','zone','value')],
                    timevar='zone',idvar='name',direction='wide')[,-1]

  output_matrix = matrix(0,nrow=sim_length,ncol=n_zones)

  init = rbind(pars[pars$name == 'init_swe',]$value,
               pars[pars$name == 'init_uztwc',]$value,
               pars[pars$name == 'init_uzfwc',]$value,
               pars[pars$name == 'init_lztwc',]$value,
               pars[pars$name == 'init_lzfsc',]$value,
               pars[pars$name == 'init_lzfpc',]$value,
               pars[pars$name == 'init_adimc',]$value)

  x = .Fortran('sacsnowstates',
               n_hrus=as.integer(n_zones),
               dt=as.integer(dt_seconds),
               sim_length=sim_length,
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
               peadj_m = as.matrix(peadj_m),
               # snow parameters
               scf = pars[pars$name ==    'scf',]$value,
               mfmax = pars[pars$name ==  'mfmax',]$value,
               mfmin = pars[pars$name ==  'mfmin',]$value,
               uadj = pars[pars$name ==   'uadj',]$value,
               si = pars[pars$name ==     'si',]$value,
               nmf = pars[pars$name ==    'nmf',]$value,
               tipm = pars[pars$name ==   'tipm',]$value,
               mbase = pars[pars$name ==  'mbase',]$value,
               plwhc = pars[pars$name ==  'plwhc',]$value,
               daygm = pars[pars$name ==  'daygm',]$value,
               adc_a = pars[pars$name ==  'adc_a',]$value,
               adc_b = pars[pars$name ==  'adc_b',]$value,
               adc_c = pars[pars$name ==  'adc_c',]$value,
               # forcing adjust parameters
               map_fa_pars = map_fa_pars,
               mat_fa_pars = mat_fa_pars,
               pet_fa_pars = pet_fa_pars,
               ptps_fa_pars = map_fa_pars,
               # forcing adjust limits
               map_fa_limits = map_limits,
               mat_fa_limits = map_limits,
               pet_fa_limits = map_limits,
               ptps_fa_limits = map_limits,
               # initial conditions
               init = init,
               # forcings
               map = do.call('cbind',lapply(forcing,'[[','map_mm')),
               ptps = do.call('cbind',lapply(forcing,'[[','ptps')),
               mat = do.call('cbind',lapply(forcing,'[[','mat_degc')),
               # output
               pet = output_matrix,
               tci = output_matrix,
               aet = output_matrix,
               uztwc = output_matrix,
               uzfwc = output_matrix,
               lztwc = output_matrix,
               lzfsc = output_matrix,
               lzfpc = output_matrix,
               adimc = output_matrix,
               swe = output_matrix)
  #print(head(x))

  format_states(x[c('year','month','day','hour','tci','aet',
                    'uztwc','uzfwc','lztwc','lzfsc','lzfpc','adimc','swe',
                    'map','mat','ptps','pet')])
}


#' Format state output from sac_snow_states
#'
#' @param x output list from sac_snow_states
#'
#' @return a data.frame with formatted output
#'
format_states <- function(x) {
  df = data.frame(year=x$year,month=x$month,day=x$day,hour=x$hour)
  n_zones = ncol(x$tci)
  for(i in 1:n_zones){
    for(name in names(x)[-(1:4)]){
      df[[paste0(name,'_',i)]] = x[[name]][,i]
    }
  }
  df
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
      pars[pars$name == 'zone_area',]$value[i]
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
#' d1 = as.POSIXct('2001-01-01 00:00:00',tz='UTC')
#' d2 = as.POSIXct('2001-12-31 18:00:00',tz='UTC')
#' dates = seq.POSIXt(d1,d2,by='6 hours')
#'
#' month = as.integer(format(dates,'%m'))
#' day = as.integer(format(dates,'%d'))
#' hour = as.integer(format(dates,'%H'))
#' factors = c(.5,2,1,1.5,2,.5,1,2.5,3,-1.5,0,1)
#'
#' ifa = interp_fa(factors, month, day, hour)
#'
#' plot(dates,ifa,t='l')
#' points(as.POSIXct(paste0('2001-',1:12,'-15'),tz='UTC'),factors,col='red')
interp_fa <- function(factors,month,day,hour){

  mdays = c(Jan = 31L, Feb = 28L, Mar = 31L, Apr = 30L, May = 31L, Jun = 30L,
            Jul = 31L, Aug = 31L, Sep = 30L, Oct = 31L, Nov = 30L, Dec = 31L)
  #factors = c(.5,2,1,1.5,2,.5,1,2.5,3,-1.5,0,1)
  factors_prev = c(factors[12],factors[1:11])#c(1,.5,2,1,1.5,2,.5,1,2.5,3,-1.5,0)
  factors_next = c(factors[2:12],factors[1])#c(2,1,1.5,2,.5,1,2.5,3,-1.5,0,1,.5)
  factors_step = dayi = dayn = numeric(length(month))

  for(i in 1:length(month)){
    m = month[i]
    if(day[i] >= 15){
      dayn[i] = mdays[m]
      dayi[i] = day[i] - 15 + hour[i]/24
      factors_step[i] = factors[m] + dayi[i]/dayn[i]*(factors_next[m]-factors[m])
    }else if(day[i] < 15 & m == 1){
      dayn[i] = mdays[12]
      dayi[i] = day[i] + mdays[12] - 15 + hour[i]/24
      factors_step[i] = factors_prev[m] + dayi[i]/dayn[i]*(factors[m]-factors_prev[m])
    }else if(day[i] < 15  & m > 1){
      dayn[i] = mdays[m-1]
      dayi[i] = day[i] + mdays[m - 1] - 15 + hour[i]/24
      factors_step[i] = factors_prev[m] + dayi[i]/dayn[i]*(factors[m]-factors_prev[m])
    }
  }
  factors_step
}
