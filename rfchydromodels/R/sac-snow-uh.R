#' Execute SAC-SMA, SNOW17 and UH with given parameters
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with columns for forcing inputs
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
sac_snow_uh <- function(dt_hours, forcing, pars, forcing_adjust=TRUE, climo=NULL){

  tci = sac_snow(dt_hours, forcing, pars, forcing_adjust = forcing_adjust, climo = climo)
  flow_cfs = uh(dt_hours, tci, pars)
  flow_cfs
}

#' Execute SAC-SMA, SNOW17, UH and LAG-K with given parameters
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with columns for forcing inputs
#' @param uptribs data frame with columns for upstream flow data
#' @param pars sac parameters
#' @param forcing_adjust does the parameter set include forcing adjustments
#' @return Vector of routed flow in cfs
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' data(upflows)
#' dt_hours = 6
#' flow_cfs = sac_snow_uh_lagk(dt_hours, forcing, uptribs, pars)
sac_snow_uh_lagk <- function(dt_hours, forcing, uptribs, pars, forcing_adjust=TRUE, climo=NULL){

  tci = sac_snow(dt_hours, forcing, pars, forcing_adjust = forcing_adjust, climo = climo)
  flow_cfs = uh(dt_hours, tci, pars)
  lagk_flow_cfs = lagk(dt_hours, uptribs, pars)
  flow_cfs + lagk_flow_cfs
}

#' Execute SAC-SMA, SNOW17, return total channel inflow per zone
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with with columns for forcing inputs
#' @param pars sac parameters
#' @param forcing_adjust does the parameter set include forcing adjustments
#' @param climo
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
sac_snow <- function(dt_hours, forcing, pars, forcing_adjust=TRUE, climo=NULL){

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

  }else{
    map_limits = mat_limits = pet_limits = ptps_limits = matrix(1,12,2)
    map_fa_pars = mat_fa_pars = pet_fa_pars = ptps_fa_pars = c(1,0,10,0)
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

  if(is.null(climo)) climo = matrix(-9999,12,4)


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
               area = pars[pars$name == 'zone_area',]$value,
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
               ptps_fa_pars = ptps_fa_pars,
               # forcing adjust limits
               map_fa_limits = map_limits,
               mat_fa_limits = mat_limits,
               pet_fa_limits = pet_limits,
               ptps_fa_limits = ptps_limits,
               # initial conditions
               init = init,
               # externally specified climatology
               climo = climo,
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
sac_snow_states <- function(dt_hours, forcing, pars, forcing_adjust=TRUE, climo=NULL){

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

  }else{
    map_limits = mat_limits = pet_limits = ptps_limits = matrix(1,12,2)
    map_fa_pars = mat_fa_pars = pet_fa_pars = ptps_fa_pars = c(1,0,10,0)
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

  if(is.null(climo)) climo = matrix(-9999,12,4)

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
               area = pars[pars$name == 'zone_area',]$value,
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
               ptps_fa_pars = ptps_fa_pars,
               # forcing adjust limits
               map_fa_limits = map_limits,
               mat_fa_limits = mat_limits,
               pet_fa_limits = pet_limits,
               ptps_fa_limits = ptps_limits,
               # initial conditions
               init = init,
               # externally specefied climatology
               climo = climo,
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

#' Lag-K Routing for any number of upstream points
#'
#' @param dt_hours timestep in hours
#' @param uptribs a matrix where each column contains flow data (in cfs) for an upstream point
#' @param pars parameters
#'
#' @return vector of routed flows
#' @export
#'
#' @examples NULL
lagk <- function(dt_hours, uptribs, pars, sum_routes = TRUE){

  sec_per_day = 86400
  dt_seconds = sec_per_day/(24/dt_hours)
  dt_days = dt_seconds / sec_per_day

  n_uptribs = length(uptribs)
  sim_length = nrow(uptribs[[1]])


  lagk_out = matrix(0,sim_length,n_uptribs)
  routed = .Fortran('lagk',
                    n_hrus = as.integer(n_uptribs),
                       ita = as.integer(dt_hours),
                       itb = as.integer(dt_hours),
                    #meteng = as.character('METR'),
                  lagtbl_a = pars[pars$name == 'lagtbl_a',]$value,
                  lagtbl_b = pars[pars$name == 'lagtbl_b',]$value,
                  lagtbl_c = pars[pars$name == 'lagtbl_c',]$value,
                  lagtbl_d = pars[pars$name == 'lagtbl_d',]$value,
                    ktbl_a = pars[pars$name == 'ktbl_a',]$value,
                    ktbl_b = pars[pars$name == 'ktbl_b',]$value,
                    ktbl_c = pars[pars$name == 'ktbl_c',]$value,
                    ktbl_d = pars[pars$name == 'ktbl_d',]$value,
               lagk_lagmax = pars[pars$name == 'lagk_lagmax',]$value,
                 lagk_kmax = pars[pars$name == 'lagk_kmax',]$value,
                 lagk_qmax = pars[pars$name == 'lagk_qmax',]$value,
                   init_co = pars[pars$name == 'init_co',]$value,
                   init_if = pars[pars$name == 'init_if',]$value,
                   init_of = pars[pars$name == 'init_of',]$value,
                 init_stor = pars[pars$name == 'init_stor',]$value,
                     qa_in = do.call('cbind',lapply(uptribs,'[[','flow_cfs')),
                sim_length = as.integer(sim_length),
                  lagk_out = lagk_out)

  if(sum_routes & n_uptribs>1){
    return(apply(routed$lagk_out,1,sum))
  }else if(n_uptribs > 1){
    return(routed$lagk_out)
  }else{
    return(as.vector(routed$lagk_out))
  }
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
#' @return
#' @export
#'
#' @examples
#' climo = rep(2,12)
#' pars = c(.5,0,10,0)
#' forcing_adjust_map_pet_ptps(climo,pars)
forcing_adjust_map_pet_ptps <- function(climo, pars, ll=0.9*climo, ul=1.1*climo, return_climo = FALSE){


  scale = pars[1]
  p_redist = pars[2]
  sd = pars[3]
  shift = pars[4] # days

  # normal weights
  w = dnorm(1:12, mean=1, sd=sd)
  w = rev(w/sum(w))

  # apply first scaling parameter
  climo_adj = climo * scale
  # get the indexes of the sorted values to re-sort later
  climo_order = order(order(climo_adj))
  # sort climo in ascending order to apply weights
  climo_adj = sort(climo_adj)

  # percent of each month remaining before redistributing
  climo_remaining = climo_adj * (1.0 - p_redist)
  # redistribute according to weights
  climo_redist = climo_remaining + sum(climo_adj * p_redist) * w
  # re-sort to original order
  climo_adj = climo_redist[climo_order]

  climo_interp = numeric(14)
  if( shift != 0 ){
    # apply shift
    climo_interp[1] = climo_adj[12]
    climo_interp[2:13] = climo_adj
    climo_interp[14] = climo_adj[1]

    # interpolate between (x0,y0) and (x1,y1)
    # y = y0 + (x-x0)*(y1-y0)/(x1-x0)
    # interpolate between (month0,climo0) and (month1,climo1)
    # y = climo0 + ((month0+shift)-month0)*(climo1-climo0)/(month1-month0)
    # could get fancier here and account for the number of days in each month, but
    # 30 day months should be a reasonable approximation
    for(i in 1:12){
      if(shift > 0 ){
        climo_adj[i] = climo_interp[i+1] + shift*(climo_interp[i+2]-climo_interp[i+1])/30
      }else if (shift < 0){
        climo_adj[i] = climo_interp[i] + (30-abs(shift))*(climo_interp[i+1]-climo_interp[i])/30
      }
    }
  }

  out = numeric(12)
  # enforce limits
  for( i in 1:12){
    if(climo_adj[i] > ul[i]) climo_adj[i] = ul[i]
    if(climo_adj[i] < ll[i]) climo_adj[i] = ll[i]
    if(climo[i] == 0){
      out[i] = 1
    }else{
      out[i] = climo_adj[i]/climo[i]
    }
  }
  if(return_climo) return(climo_adj) else return(out)
  #out
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
#' @return
#' @export
#'
#' @examples
#' climo = rep(2,12)
#' pars = c(.5,0,10,0)
#' forcing_adjust_mat(climo,pars)
forcing_adjust_mat <- function (climo, pars, ll=0.9*climo, ul=1.1*climo, return_climo = FALSE){

  scale = pars[1]
  p_redist = pars[2]
  sd = pars[3]
  shift = pars[4] # days

  # normal weights
  w = numeric(12)
  w[1:6] = dnorm(1:6, mean=1, sd=sd)
  w[7:12] = rev(w[1:6])
  w = w/sum(w)*2

  # apply first scaling parameter
  climo_adj = climo * scale
  med = median(climo_adj)
  # get the indexes of the sorted values to re-sort later
  climo_order = order(order(climo_adj))
  # sort climo in ascending order to apply weights
  climo_adj = sort(climo_adj)
  # compute deviation from median
  climo_dev = climo_adj - med

  # percent of each month remaining before redistributing
  climo_remaining = climo_dev * (1.0 - p_redist)
  # get the total temperature to above the median and below median
  # write(*,*)climo_dev(1:6), p_redist
  climo_dist = numeric(12)
  climo_dist[1:6] = sum(climo_dev[1:6] * p_redist)
  climo_dist[7:12] = sum(climo_dev[7:12] * p_redist)

  # redistribute according to weights
  climo_redist = med + climo_remaining + climo_dist * w

  # re-sort to original order
  climo_adj = climo_redist[climo_order]

  climo_interp = numeric(14)
  if(shift != 0 ){
    # apply shift
    climo_interp[1] = climo_adj[12]
    climo_interp[2:13] = climo_adj
    climo_interp[14] = climo_adj[1]

    # interpolate between (x0,y0) and (x1,y1)
    # y = y0 + (x-x0)*(y1-y0)/(x1-x0)
    # interpolate between (month0,climo0) and (month1,climo1)
    # y = climo0 + ((month0+shift)-month0)*(climo1-climo0)/(month1-month0)
    # could get fancier here and account for the number of days in each month, but
    # 30 day months should be a reasonable approximation
    for(i in 1:12){
      if(shift > 0 ){
        climo_adj[i] = climo_interp[i+1] + shift*(climo_interp[i+2]-climo_interp[i+1])/30
      }else if (shift < 0 ){
        climo_adj[i] = climo_interp[i] + (30-abs(shift))*(climo_interp[i+1]-climo_interp[i])/30
      }
    }
  }


  # enforce limits
  for(i in 1:12){
    if(climo_adj[i] > ul[i]) climo_adj[i] = ul[i]
    if(climo_adj[i] < ll[i]) climo_adj[i] = ll[i]
  }

  out = climo_adj-climo
  if(return_climo) return(climo_adj) else return(out)

}
