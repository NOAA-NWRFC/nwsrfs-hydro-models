#' Execute SAC-SMA, SNOW17 and UH with given parameters
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with columns for forcing inputs
#' @param pars sac parameters
#' @return Vector of routed flow in cfs
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' flow_cfs = sac_snow_uh(dt_hours,forcing, pars)
sac_snow_uh <- function(dt_hours, forcing, pars){

  tci = sac_snow(dt_hours, forcing, pars)
  flow_cfs = uh(dt_hours, tci, pars)
  flow_cfs = chanloss(flow_cfs, forcing, dt_hours, pars)
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
#' \dontrun{
#' data(forcing)
#' data(pars)
#' data(upflows)
#' dt_hours = 6
#' flow_cfs = sac_snow_uh_lagk(dt_hours, forcing, uptribs, pars)
#' }
sac_snow_uh_lagk <- function(dt_hours, forcing, uptribs, pars){

  tci = sac_snow(dt_hours, forcing, pars)
  flow_cfs = uh(dt_hours, tci, pars)
  lagk_flow_cfs = lagk(dt_hours, uptribs, pars)
  total_flow_cfs = chanloss(flow_cfs + lagk_flow_cfs, forcing, dt_hours, pars)
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
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' states = sac_snow_states(dt_hours, forcing, pars)
sac_snow_states <- function(dt_hours, forcing, pars){

  sac_snow(dt_hours, forcing, pars, return_states = TRUE)

}

#' Execute SAC-SMA, SNOW17, return total channel inflow per zone, and model states
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with with columns for forcing inputs
#' @param pars sac parameters
#' @param return_states logical value indicating if the states should be output as well as the tci
#' @return data.frame (1 column per zone) of unrouted channel inflow (tci), sac states
#' uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc, and snow water equivalent (swe),
#' and adjusted forcing data.
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' flow = sac_snow(dt_hours, forcing, pars)
#' @useDynLib rfchydromodels sacsnow_
sac_snow <- function(dt_hours, forcing, pars, return_states = FALSE){

  pars = as.data.frame(pars)

  sec_per_day = 86400
  dt_seconds = sec_per_day/(24/dt_hours)

  n_zones = length(forcing)
  sim_length = nrow(forcing[[1]])

  output_matrix = matrix(0,nrow=sim_length,ncol=n_zones)

  sac_pars = rbind(pars[pars$name == 'uztwm',]$value,
                   pars[pars$name == 'uzfwm',]$value,
                   pars[pars$name == 'lztwm',]$value,
                   pars[pars$name == 'lzfpm',]$value,
                   pars[pars$name == 'lzfsm',]$value,
                   pars[pars$name == 'adimp',]$value,
                   pars[pars$name ==   'uzk',]$value,
                   pars[pars$name ==  'lzpk',]$value,
                   pars[pars$name ==  'lzsk',]$value,
                   pars[pars$name == 'zperc',]$value,
                   pars[pars$name ==  'rexp',]$value,
                   pars[pars$name == 'pctim',]$value,
                   pars[pars$name == 'pfree',]$value,
                   pars[pars$name ==  'riva',]$value,
                   pars[pars$name ==  'side',]$value,
                   pars[pars$name == 'rserv',]$value,
                   pars[pars$name ==   'efc',]$value)

  snow_pars = rbind(pars[pars$name ==   'scf',]$value,
                    pars[pars$name == 'mfmax',]$value,
                    pars[pars$name == 'mfmin',]$value,
                    pars[pars$name ==  'uadj',]$value,
                    pars[pars$name ==    'si',]$value,
                    pars[pars$name ==   'nmf',]$value,
                    pars[pars$name ==  'tipm',]$value,
                    pars[pars$name == 'mbase',]$value,
                    pars[pars$name == 'plwhc',]$value,
                    pars[pars$name == 'daygm',]$value,
                    pars[pars$name == 'adc_a',]$value,
                    pars[pars$name == 'adc_b',]$value,
                    pars[pars$name == 'adc_c',]$value)

  # sacsnowstates(n_hrus, dt, sim_length, year, month, day, hour, &
  #                 latitude, elev, &
  #                 sac_pars, &
  #                 peadj, pxadj, &
  #                 snow_pars, &
  #                 init_swe, &
  #                 map, ptps, mat, etd, &
  #                 return_states,
  #                 tci, aet, uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc, &
  #                 swe, aesc, neghs, liqw, raim, psfall, prain)

  x = .Fortran('sacsnow',
               n_hrus = as.integer(n_zones),
               dt = as.integer(dt_seconds),
               sim_length = sim_length,
               year = as.integer(forcing[[1]]$year)[1:sim_length],
               month = as.integer(forcing[[1]]$month)[1:sim_length],
               day = as.integer(forcing[[1]]$day)[1:sim_length],
               hour = as.integer(forcing[[1]]$hour)[1:sim_length],
               # zone info
               latitude = pars[pars$name == 'alat',]$value,
               elev = pars[pars$name == 'elev',]$value,
               # sac parameters
               sac_pars = sac_pars,
               # pet and precp adjustments
               peadj = pars[pars$name == 'peadj',]$value,
               pxadj = pars[pars$name == 'pxadj',]$value,
               # snow parameters
               snow_pars = snow_pars,
               # initial conditions
               init_swe = pars[pars$name == 'init_swe',]$value,
               # forcings
               map = do.call('cbind',lapply(forcing,'[[','map_mm')),
               ptps = do.call('cbind',lapply(forcing,'[[','ptps')),
               mat = do.call('cbind',lapply(forcing,'[[','mat_degc')),
               etd = do.call('cbind',lapply(forcing,'[[','etd_mm')),
               # should the states be output
               return_states = return_states,
               # output
               tci = output_matrix,
               aet = output_matrix,
               uztwc = output_matrix,
               uzfwc = output_matrix,
               lztwc = output_matrix,
               lzfsc = output_matrix,
               lzfpc = output_matrix,
               adimc = output_matrix,
               swe = output_matrix,
               aesc = output_matrix,
               neghs = output_matrix,
               liqw = output_matrix,
               raim = output_matrix,
               psfall = output_matrix,
               prain = output_matrix)

  if(return_states){
    return_vars = c('year','month','day','hour',
                    'map','mat','ptps','etd','tci','aet',
                    'uztwc','uzfwc','lztwc','lzfsc','lzfpc','adimc',
                    'swe','aesc','neghs','liqw','raim','psfall', 'prain')

    # if pet exists in the input forcings, output it as is
    if(!is.null(forcing[[1]]$pet_mm)){
      x[['pet']] = do.call('cbind',lapply(forcing,'[[','pet_mm'))
      return_vars = c(return_vars,'pet')
    }

    return(format_states(x[return_vars]))
  }else{
    return(x$tci)
  }
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
#' @return stuff
#' @export
#'
#' @examples
#' dt = 6
#' shape = 2
#' scale = 1
#' y = uh2p(2,1,6)
#' x = seq(dt,dt*length(y),by=dt)
#' plot(x,y,t='l')
uh2p <- function(shape, scale, timestep, max_len = 1000){
  # this code needs timestep in days
  timestep = timestep/24
  uh = numeric(max_len)
  toc = log(gamma(shape)*scale)
  #print(toc)
  for(i in 1:max_len){
    top=i*timestep/scale
    tor=(shape-1)*log(top)-top-toc
    uh[i] = 0
    if(tor > -8.0){
      uh[i]=exp(tor)
    }else{
      if(i > 1){
        uh[i] = 0.0
        max_len = i
        break
      }
    }
  }
  s = sum(uh)
  s = ifelse(s==0,1.0e-5,s)
  # turn it into a unit hydrograph (sums to 1)
  uh=uh/s
  # dont return all the trailing zero values
  first0 = which(uh==0)[1]
  if(is.na(first0)){
    warning('UH may have been truncated, increase max_len.')
    return(uh)
  }
  return(uh[1:(first0-1)])
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
#' dt = 6
#' shape = 2
#' scale = 1
#' y = uh2p_cfs_in(2,1,6,1000)
#' x = seq(dt,dt*length(y),by=dt)
#' plot(x,y,t='l')
uh2p_cfs_in <- function(shape, scale, timestep, area){
  # convert to cfs/in
  # timestep in hours
  # area in sq mi
  uh2p(shape,scale,timestep)*5280^2*24/12/86400/timestep*area
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
#' uh2p_get_scale_r(2,50,1)
#' @importFrom stats optimize
uh2p_get_scale_r <- function(shape, toc, dt_hours){
  # find a reasonable upper limit for scale, some values are unstable
  scale_lim = scale_uplimit(shape, dt_hours)
  # optimization to find scale given shape and toc
  scale = optimize(uh2p_seek, shape, dt_hours, toc, interval=c(.01,scale_lim))$minimum
  # bump up very small or negative values to prevent 0 length UH
  max(as.numeric(scale),0.02)
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
#' uh2p_get_scale(2,50,1)
#' @useDynLib rfchydromodels uh2p_get_scale_root_
uh2p_get_scale <- function(shape, toc, dt_hours){

  scale = .Fortran('uh2p_get_scale_root',
                   shape = shape,
                   toc = toc,
                   dt_hours = dt_hours,
                   scale = 0)
  scale$scale
}


#' Find a reasonable scale upper limit for optimization
#'
#' @param shape blah
#' @param dt_hours blah
#'
#' @return numeric value indicating the upper limit for the scale
scale_uplimit <- function(shape, dt_hours){
  scale = 0.1
  len_1 = 0
  len_2 = length(uh2p(shape, scale, dt_hours))
  while ((len_1 <= len_2) & (scale < 5)){
    len_1 = len_2
    scale = scale+.1
    len_2 = length(uh2p(shape, scale, dt_hours))
  }
  return(scale-.1)
}

#' Objective function for finding a scale parameter given shape and toc
#'
#' @param scale blah
#' @param shape blah
#' @param dt_hours blah
#' @param toc blah
#'
#' @return objective function value
uh2p_seek <- function(scale, shape, dt_hours, toc){
  # add one to the length because the first ordinate is at time 0
  uh_len = round(toc/dt_hours,0)+1
  len_dif = abs(length(uh2p(shape, scale, dt_hours)) - uh_len)
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
#' @useDynLib rfchydromodels uh2p_len_obj_root_test_
uh2p_seek2 <- function(scale, shape, dt_hours, toc){
  obj = .Fortran('uh2p_len_obj_root_test',
                   scale = scale,
                   shape = shape,
                   toc = toc,
                   dt_hours = dt_hours,
                   obj=0)
  obj$obj

}


#' root finding function for finding a scale parameter given shape and toc
#'
#' @param scale blah
#' @param shape blah
#' @param dt_hours blah
#' @param toc blah
#'
#' @return function value for root finding
uh2p_root <- function(scale, shape, dt_hours, toc){
  # add one to the length because the first ordinate is at time 0
  uh_len = round(toc/dt_hours,0)+1
  len_dif = length(uh2p(shape, scale, dt_hours)) - uh_len
  return(len_dif)
}



#' Two parameter unit hydrograph routing for one or more basin zones
#'
#' @param dt_hours timestep in hours
#' @param tci channel inflow matrix, one column per zone
#' @param pars parameters
#' @param sum_zones should routed flows from multiple zones be added and returned as a vector, or
#'                  kept separate and returned as a matrix
#' @param start_of_timestep should the output flow data be shifted by one timestep to account for
#'                          forcing data that uses beginning of timestep labeling
#' @param backfill when start_of_timestep is TRUE, should the first value be duplicated
#' @return Vector of routed flow in cfs
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' tci = sac_snow(dt_hours,forcing, pars)
#' flow_cfs = uh(dt_hours, tci, pars)
#' @useDynLib rfchydromodels duamel_
uh <- function(dt_hours, tci, pars, sum_zones = TRUE, start_of_timestep = TRUE, backfill = TRUE){

  sec_per_day = 86400
  dt_seconds = sec_per_day/(24/dt_hours)
  dt_days = dt_seconds / sec_per_day

  n_zones = ncol(tci)
  sim_length = nrow(tci)

  k = 1 # turns on 2 parameter UH
  m = 1000 # max unit hydro
  n = sim_length + m

  flow_cfs = if(sum_zones) numeric(sim_length) else tci
  for(i in 1:n_zones){

    shape = pars[pars$name == 'unit_shape',]$value[i]
    toc_gis = pars[pars$name == 'unit_toc',]$value[i]
    toc_adj = pars[pars$name == 'unit_toc_adj',]$value[i]
    # required to define either shape, toc and toc_adj or shape and scale
    if(is.na(toc_gis) | is.na(toc_adj)){
      scale = pars[pars$name == 'unit_scale',]$value[i]
    }else{
      toc = toc_gis * toc_adj
      scale = uh2p_get_scale(shape, toc, 1)
    }

    routed = .Fortran('duamel',
                      tci = as.single(tci[,i]),
                      as.single(shape),
                      as.single(scale),
                      as.single(dt_days),
                      as.integer(n),
                      as.integer(m),
                      1L,
                      0L,
                      qr = as.single(numeric(n)))

    # convert to cfs
    zone_flow = routed$qr[1:sim_length] * 1000 * 3.28084**3 / dt_seconds *
      pars[pars$name == 'zone_area',]$value[i]
    if(sum_zones){
      flow_cfs = flow_cfs + zone_flow
    }else{
      flow_cfs[,i] = zone_flow
    }
  }

  # if the forcing data used was beginning of time step,
  # then the instantaneous output occurs at the end of the timestep
  # so we need to shift the output ahead by one timestep relative
  # to the focings
  if(start_of_timestep){
    if(sum_zones){
      c(if(backfill) flow_cfs[1] else NA,
        flow_cfs[1:(sim_length-1)])
    }else{
      rbind(if(backfill) flow_cfs[1,] else NA,
            flow_cfs[1:(sim_length-1),])
    }
  }else{
    flow_cfs
  }
}

#' Seasonal chanloss
#'
#' @param flow streamflow vector
#' @param forcing forcing data
#' @param dt_hours timestep in hours
#' @param pars parameters
#' @return Vector of flow modified by the chanloss pattern
#' @export
#'
#' @useDynLib rfchydromodels chanloss_
chanloss <- function(flow, forcing, dt_hours, pars){

  sim_length = nrow(forcing[[1]])

  # chanloss(n_clmods, dt, sim_length, year, month, day, hour, &
  #            factor, period, &
  #            sim, sim_adj)

  n_clmods = pars[pars$name == 'n_clmods',]$value[1]

  if(is.na(n_clmods) | n_clmods <= 0){
    return(flow)
  }else{
    cl_factors = numeric(n_clmods)
    cl_periods = matrix(NA, 2, n_clmods)
    for(i in 1:n_clmods){
      cl_periods[1,i] = pars[pars$name == sprintf('cl_period_start_%02d',i),]$value
      cl_periods[2,i] = pars[pars$name == sprintf('cl_period_end_%02d',i),]$value
      cl_factors[i] = pars[pars$name == sprintf('cl_factor_%02d',i),]$value
    }
    mode(cl_periods) <- 'integer'


    cl_flow = .Fortran('chanloss',
                      n_clmods = as.integer(n_clmods),
                      dt = as.integer(dt_hours),
                      sim_length = as.integer(sim_length),
                      year = as.integer(forcing[[1]]$year)[1:sim_length],
                      month = as.integer(forcing[[1]]$month)[1:sim_length],
                      day = as.integer(forcing[[1]]$day)[1:sim_length],
                      factor = cl_factors,
                      period = cl_periods,
                      sim = flow[1:sim_length],
                      sim_adj = numeric(sim_length))
    return(cl_flow$sim_adj)
  }
}

#' Daily consuse model
#'
#' @param input A data frame (or matrix with col names), must have
#' columns: flow, pet (units of mm), year, month, day
#' @param pars model parameters in the same format as the sac and snow models, with type=='consuse'
#' @param cfs if TRUE, then flow units of cfs are expected, if FALSE then cms are expected.
#'
#' @return data frame with consuse variables
#' @export
#'
#' @useDynLib rfchydromodels consuse_
consuse <- function(input, pars, cfs=TRUE){

  input = as.data.frame(input)
  zones = unique(pars$zone)
  cu_zones = grep('_CU',zones,value = T)
  cu_pars = pars[pars$type=='consuse',]
  sim_length = as.integer(nrow(input))

  cu_out = list()
  for(cu_zone in cu_zones){

    peadj_m = cu_pars[cu_pars$zone==cu_zone & substr(cu_pars$name,1,5)=='peadj',]$value

    # consuse(sim_length, year, month, day, &
    #           AREA_in,EFF_in,MFLOW_in, &
    #           IRFSTOR_in,ACCUM_in,DECAY_in, peadj_m, peadj, &
    #           PET_in,QNAT_in, &
    #           QADJ_out,QDIV_out,QRFIN_out,QRFOUT_out, &
    #           QOL_out,QCD_out,CE_out,RFSTOR_out)
    x = .Fortran('consuse',
                 # inputs
                 sim_length = sim_length,
                 year = as.integer(input$year),
                 month = as.integer(input$month),
                 day = as.integer(input$day),
                 AREA_in = cu_pars[cu_pars$zone==cu_zone & cu_pars$name=='area_km2',]$value,
                 EFF_in = cu_pars[cu_pars$zone==cu_zone & cu_pars$name=='irr_eff',]$value,
                 MFLOW_in = cu_pars[cu_pars$zone==cu_zone & cu_pars$name=='min_flow_cmsd',]$value * 0.028316847,
                 ACCUM_in = cu_pars[cu_pars$zone==cu_zone & cu_pars$name=='rf_accum_rate',]$value,
                 DECAY_in = cu_pars[cu_pars$zone==cu_zone & cu_pars$name=='rf_decay_rate',]$value,
                 peadj_m = as.numeric(peadj_m),
                 peadj = pars[pars$name=='peadj' & pars$zone == cu_zone,]$value,
                 PET_in = as.numeric(input$pet),
                 # consuse code expects cfs so convert if necessary
                 QNAT_in = as.numeric(input$flow * ifelse(cfs,1,0.028316847)),
                 # outputs
                 QADJ_out = numeric(sim_length),
                 QDIV_out = numeric(sim_length),
                 QRFIN_out = numeric(sim_length),
                 QRFOUT_out = numeric(sim_length),
                 QOL_out = numeric(sim_length),
                 QCD_out = numeric(sim_length),
                 CE_out = numeric(sim_length),
                 RFSTOR_out = numeric(sim_length)
                 )
    cu_out[[cu_zone]] = data.frame(year = x$year,month = x$month,day = x$day, qnat = x$QNAT_in,
      qadj = x$QADJ_out, qdiv = x$QDIV_out, qrfin = x$QRFIN_out, qrfout = x$QRFOUT_out,
      qol = x$QOL_out, qcd = x$QCD_out, ce = x$CE_out, rfstor = x$RFSTOR_out)
  }
  if(length(cu_zones)==1) return(cu_out[[1]]) else return(cu_out)
}


#' Lag-K Routing for any number of upstream points
#'
#' @param dt_hours timestep in hours
#' @param uptribs a matrix where each column contains flow data (in cfs) for an upstream point
#' @param pars parameters
#' @param sum_routes add all routed values together or leave separate
#' @param return_states return the lagk states
#'
#' @return vector of routed flows
#' @export
#'
#' @examples NULL
#' @useDynLib rfchydromodels lagk_
lagk <- function(dt_hours, uptribs, pars, sum_routes = TRUE, return_states = FALSE){

  sec_per_day = 86400
  dt_seconds = sec_per_day/(24/dt_hours)
  dt_days = dt_seconds / sec_per_day

  n_uptribs = length(uptribs)
  sim_length = nrow(uptribs[[1]])

  # lagk(n_hrus, ita, itb, &
  #      lagtbl_a_in, lagtbl_b_in, lagtbl_c_in, lagtbl_d_in,&
  #      ktbl_a_in, ktbl_b_in, ktbl_c_in, ktbl_d_in, &
  #      lagk_lagmax_in, lagk_kmax_in, lagk_qmax_in, &
  #      lagk_lagmin_in, lagk_kmin_in, lagk_qmin_in, &
  #      ico_in, iinfl_in, ioutfl_in, istor_in, &
  #      qa_in, sim_length, &
  #      return_states, &
  #      lagk_out, co_st_out, &
  #      inflow_st_out,storage_st_out)

  lagk_out = matrix(0,sim_length,n_uptribs)

  routed = .Fortran('lagk',
                    n_hrus = as.integer(n_uptribs),
                       ita = as.integer(dt_hours),
                       itb = as.integer(dt_hours),
                    #meteng = as.character('METR'),
               lagtbl_a_in = pars[pars$name == 'lagtbl_a',]$value,
               lagtbl_b_in = pars[pars$name == 'lagtbl_b',]$value,
               lagtbl_c_in = pars[pars$name == 'lagtbl_c',]$value,
               lagtbl_d_in = pars[pars$name == 'lagtbl_d',]$value,
                 ktbl_a_in = pars[pars$name == 'ktbl_a',]$value,
                 ktbl_b_in = pars[pars$name == 'ktbl_b',]$value,
                 ktbl_c_in = pars[pars$name == 'ktbl_c',]$value,
                 ktbl_d_in = pars[pars$name == 'ktbl_d',]$value,
            lagk_lagmax_in = pars[pars$name == 'lagk_lagmax',]$value,
              lagk_kmax_in = pars[pars$name == 'lagk_kmax',]$value,
              lagk_qmax_in = pars[pars$name == 'lagk_qmax',]$value,
            lagk_lagmin_in = pars[pars$name == 'lagk_lagmin',]$value,
              lagk_kmin_in = pars[pars$name == 'lagk_kmin',]$value,
              lagk_qmin_in = pars[pars$name == 'lagk_qmin',]$value,
                    ico_in = pars[pars$name == 'init_co',]$value,
                  iinfl_in = pars[pars$name == 'init_if',]$value,
                 ioutfl_in = pars[pars$name == 'init_of',]$value,
                  istor_in = pars[pars$name == 'init_stor',]$value,
                     qa_in = do.call("cbind", lapply(uptribs, function(x)as.numeric(x[['flow_cfs']]))),
                sim_length = as.integer(sim_length),
             return_states = as.logical(return_states),
                  lagk_out = lagk_out,
                 co_st_out = lagk_out,
             inflow_st_out = lagk_out,
            storage_st_out = lagk_out)

  if(isTRUE(return_states)){
    return_vars = c('lagk_out','co_st_out','inflow_st_out','storage_st_out')
    df = as.data.frame(do.call('cbind',routed[return_vars]))
    names(df) = paste0(gsub('_out','',return_vars),'_',1:n_uptribs)

    return(cbind(upflow[[1]][,c('year','month','day','hour')],df))
  }
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

  dt_hours = hour[2]-hour[1]
  interp_day = 16+dt_hours/24

  for(i in 1:length(month)){
    m = month[i]
    decimal_day = day[i]+hour[i]/24
    if(decimal_day >= interp_day){
      dayn[i] = mdays[m]
      dayi[i] = decimal_day - interp_day
      factors_step[i] = factors[m] + dayi[i]/dayn[i]*(factors_next[m]-factors[m])
    }else if(decimal_day < interp_day & m == 1){
      dayn[i] = mdays[12]
      dayi[i] = decimal_day - interp_day + mdays[12]
      factors_step[i] = factors_prev[m] + dayi[i]/dayn[i]*(factors[m]-factors_prev[m])
    }else if(decimal_day < interp_day & m > 1){
      dayn[i] = mdays[m-1]
      dayi[i] = decimal_day - interp_day + mdays[m - 1]
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
#' @return stuff
#' @export
#'
#' @examples
#' climo = rep(2,12)
#' pars = c(.5,0,10,0)
#' forcing_adjust_map_pet_ptps(climo,pars)
#' @importFrom stats dnorm median
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
#' @return stuff
#' @export
#'
#' @examples
#' climo = rep(2,12)
#' pars = c(.5,0,10,0)
#' forcing_adjust_mat(climo,pars)
#' @importFrom stats dnorm median
forcing_adjust_mat <- function (climo, pars, ll=climo*ifelse(climo>0,0.9,1.1),
                                ul=climo*ifelse(climo>0,1.1,0.9), return_climo = FALSE){

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


#' Conduct NWRFC style forcing adjustments
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with with columns for forcing inputs
#' @param pars sac parameters
#' @param climo climotology matrix
#' @param dry_run Do a run without any forcing adjustments, only compute pet and etd
#' @param return_adj return monthly adjustment factors only
#' @param return_climo return the computed monthly climo
#' @return Matrix (1 column per zone) of unrouted channel inflow
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' forcing_adj = fa_nwrfc(dt_hours, forcing, pars)
#' @useDynLib rfchydromodels fa_ts_
#' @importFrom stats reshape
fa_nwrfc <- function(dt_hours, forcing, pars, climo=NULL, dry_run=FALSE, return_adj=FALSE, return_climo=FALSE){

  if(return_adj & return_climo) stop('Can only return adjustments or climo')

  pars = as.data.frame(pars)

  sec_per_day = 86400
  dt_seconds = sec_per_day/(24/dt_hours)

  n_zones = length(forcing)
  sim_length = nrow(forcing[[1]])

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

  if(dry_run){
    map_fa_pars = mat_fa_pars = pet_fa_pars = ptps_fa_pars = c(1,0,10,0)
  }else{
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

  peadj_m = reshape(pars[grepl('peadj_',pars$name) & pars$type=='sac',
                         c('name','zone','value')],
                    timevar='zone',idvar='name',direction='wide')[,-1]

  if(is.null(climo)) climo = matrix(-9999,12,4)

  output_matrix = matrix(0,nrow=sim_length,ncol=n_zones)

  # fa_ts(n_hrus, dt, sim_length, year, month, day, hour, &
  #         latitude, area, &
  #         peadj_m, &
  #         map_fa_pars, mat_fa_pars, pet_fa_pars, ptps_fa_pars, &
  #         map_fa_limits_in, mat_fa_limits_in, pet_fa_limits_in, ptps_fa_limits_in, &
  #         climo, &
  #         map, ptps, mat, &
  #         map_fa, mat_fa, ptps_fa, pet_fa, etd)

  # browser()

  x = .Fortran('fa_ts',
               n_hrus = as.integer(n_zones),
               dt = as.integer(dt_seconds),
               sim_length = as.integer(sim_length),
               year = as.integer(forcing[[1]]$year),
               month = as.integer(forcing[[1]]$month),
               day = as.integer(forcing[[1]]$day),
               hour = as.integer(forcing[[1]]$hour),
               # zone info
               latitude = pars[pars$name == 'alat',]$value,
               area = pars[pars$name == 'zone_area',]$value,
               # monthly crop coefficients
               peadj_m = as.matrix(peadj_m),
               # forcing adjust parameters
               map_fa_pars = map_fa_pars,
               mat_fa_pars = mat_fa_pars,
               pet_fa_pars = pet_fa_pars,
               ptps_fa_pars = ptps_fa_pars,
               # forcing adjust limits
               map_fa_limits_in = map_limits,
               mat_fa_limits_in = mat_limits,
               pet_fa_limits_in = pet_limits,
               ptps_fa_limits_in = ptps_limits,
               # externally specified climatology
               climo = climo,
               # forcings
               map = do.call('cbind',lapply(forcing,'[[','map_mm')),
               ptps = do.call('cbind',lapply(forcing,'[[','ptps')),
               mat = do.call('cbind',lapply(forcing,'[[','mat_degc')),
               # output
               map_adj = numeric(12),
               mat_adj = numeric(12),
               pet_adj = numeric(12),
               ptps_adj = numeric(12),
               map_fa = output_matrix,
               mat_fa = output_matrix,
               ptps_fa = output_matrix,
               pet_fa = output_matrix,
               etd = output_matrix)

  if(return_adj){
    return(as.data.frame(do.call('cbind',x[c('map_adj','mat_adj','pet_adj','ptps_adj')])))
  }else if(return_climo){
    colnames(x$climo) = c('map','mat','pet','ptps')
    return(as.data.frame(x$climo))
  }else{
    for(z in 1:n_zones){
      forcing[[z]]$map_mm = x$map_fa[,z]
      forcing[[z]]$mat_degc = x$mat_fa[,z]
      forcing[[z]]$ptps = x$ptps_fa[,z]
      forcing[[z]]$pet_mm = x$pet_fa[,z]
      forcing[[z]]$etd_mm = x$etd[,z]
    }
    return(forcing)
  }
}

#' Conduct NWRFC style forcing adjustments
#'
#' @param dt_hours timestep in hours
#' @param forcing data frame with with columns for forcing inputs
#' @param pars sac parameters
#' @param climo climotology matrix
#' @param dry_run Do a run without any forcing adjustments, only compute pet and etd
#' @param return_climo Return the computed climo, instead of adjustments
#' @return Matrix (1 column per zone) of unrouted channel inflow
#' @export
#'
#' @examples
#' data(forcing)
#' data(pars)
#' dt_hours = 6
#' adj = fa_adj_nwrfc(dt_hours, forcing, pars)
#' @importFrom stats reshape
fa_adj_nwrfc <- function(dt_hours, forcing, pars, climo = NULL, dry_run = FALSE, return_climo = FALSE){

  if(return_climo){
    fa_nwrfc(dt_hours, forcing, pars, climo, dry_run, return_climo=TRUE)
  }else{
    fa_nwrfc(dt_hours, forcing, pars, climo, dry_run, return_adj=TRUE)
  }
}

