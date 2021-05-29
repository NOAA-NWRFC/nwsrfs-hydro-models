subroutine sacsnow(n_hrus, dt, sim_length, year, month, day, hour, &
    latitude, elev, area, &
    sac_pars, &
    peadj, pxadj, peadj_m, &
    snow_pars, & 
    map_fa_pars, mat_fa_pars, pet_fa_pars, ptps_fa_pars, & 
    map_fa_limits, mat_fa_limits, pet_fa_limits, ptps_fa_limits, & 
    init, climo, & 
    map, ptps, mat, &
    tci)

    ! !start_month, start_hour, start_day, start_year, end_month, end_day, end_hour, end_year, &
    ! ! zone info 
    ! latitude, elev, &
    ! ! sac parameters 
    ! uztwm, uzfwm, lztwm, lzfpm, lzfsm, adimp, uzk, lzpk, lzsk, zperc, &
    ! rexp, pctim, pfree, riva, side, rserv, &
    ! ! pet and precp adjustments
    ! peadj, pxadj, peadj_m, &
    ! ! snow parameters 
    ! scf, mfmax, mfmin, uadj, si, nmf, tipm, mbase, plwhc, daygm, &
    ! adc_a, adc_b, adc_c, & 
    ! ! forcing adjust parameters 
    ! map_adj, mat_adj, pet_adj, ptps_adj, & 
    ! ! initial conditions 
    ! init_swe, init_uztwc, init_uzfwc, init_lztwc, init_lzfsc, init_lzfpc, init_adimc, & 
    ! ! forcings 
    ! map, ptps, mat, &
    ! ! output
    ! tci)

  use utilities

  implicit none

  double precision, parameter:: pi=3.141592653589793238462643383279502884197d0
  double precision, parameter:: sec_day = 86400.     !seconds in a day
  double precision, parameter:: sec_hour = 3600.     !seconds in an hour
  integer, parameter:: sp = KIND(1.0)
  integer:: k

  ! initialization variables
  integer, intent(in):: n_hrus ! number of zones

  ! sac pars matrix 
  ! uztwm, uzfwm, lztwm, lzfpm, lzfsm, adimp, uzk, lzpk, lzsk, zperc, rexp, pctim, pfree, riva, side, rserv
  double precision, dimension(16,n_hrus), intent(in):: sac_pars 

  ! snow pars marix 
  ! scf, mfmax, mfmin, uadj, si, nmf, tipm, mbase, plwhc, daygm, adc_a, adc_b, adc_c
  double precision, dimension(13,n_hrus), intent(in):: snow_pars 


  integer, intent(in):: dt    ! model timestep in seconds
  !integer:: start_month, start_hour, start_day, start_year, &
  !          end_month, end_day, end_hour, end_year   

  ! initial states if for a cold start run
  ! used in all model HRUs
  ! model state variables not listed start at 0
  double precision, dimension(7, n_hrus), intent(in):: init
  double precision, dimension(n_hrus):: init_swe, init_uztwc, init_uzfwc, init_lztwc, init_lzfsc, &
          init_lzfpc, init_adimc

  ! 4 columns, map, mat, pet, ptps 
  double precision, dimension(12, 4), intent(in):: climo
  logical:: calc_climo

  ! SAC_model params & other key inputs in the sace param file
  !character(len = 20), dimension(n_hrus) :: hru_id   ! local hru id
  double precision, dimension(n_hrus):: uztwm, uzfwm, uzk, pctim, adimp, zperc, rexp, &
                                lztwm, lzfsm, lzfpm, lzsk, lzpk, pfree, &
                                riva, side, rserv, peadj, pxadj

  ! Snow17_model params 
  double precision, dimension(n_hrus), intent(in):: latitude   ! decimal degrees
  double precision, dimension(n_hrus), intent(in):: elev       ! m
  double precision, dimension(n_hrus), intent(in):: area       ! km2
  double precision, dimension(n_hrus):: scf, mfmax, mfmin, uadj, si, nmf, tipm, mbase, plwhc, daygm
  double precision, dimension(n_hrus):: pxtemp ! not used, set to zero
  double precision, dimension(11):: adc  ! different for each hru
  double precision, dimension(n_hrus):: adc_a, adc_b, adc_c ! areal depletion curve parameters ax^b+(1-a)x^c
  double precision, dimension(11) :: adc_x

  ! forcing adjustment parameter vectors: mult, p_redist, std, shift 
  double precision, dimension(4), intent(in):: map_fa_pars, mat_fa_pars, pet_fa_pars, ptps_fa_pars
  ! climo fa limits, static set by external data 
  double precision, dimension(12,2):: map_fa_limits, mat_fa_limits, pet_fa_limits, ptps_fa_limits

  ! static pet adjustment
  double precision, dimension(12, n_hrus), intent(in):: peadj_m

  ! monthly forcing adjustment parameters for map, mat, pet, ptps will be computed
  double precision, dimension(12):: map_adj, mat_adj, pet_adj, ptps_adj


  ! local variables
  integer:: nh,i           ! AWW index for looping through areas
  integer:: sim_length   ! length of simulation (days)

  ! single precision sac-sma and snow variables
  ! these are single precision so as to be supplied to 
  ! old NWS code models
  real(sp):: uztwc_sp, uzfwc_sp, lztwc_sp, lzfsc_sp, lzfpc_sp, adimc_sp
  ! double precision:: uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc


  ! snow-17 carry over variables
  double precision:: pa       ! snow-17 surface pressure
  real(sp):: tprev    ! carry over variable
  real(sp), dimension(19):: cs       ! carry over variable array

    ! sac-sma state variables
  ! double precision, dimension(sim_length ,n_hrus):: uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc, swe

  ! sac-sma output variables and routed flow
  !real(sp), dimension(sim_length):: qs_sp, qg_sp, aet_sp, tci_sp
  real(sp), dimension(sim_length, n_hrus):: qs, qg, aet, tci_sp
  double precision, dimension(sim_length ,n_hrus), intent(out):: tci

  ! snow-17 output variables  
  real(sp), dimension(sim_length, n_hrus):: raim, snowh, sneqv, snow 

  ! date variables
  integer, dimension(sim_length), intent(in):: year, month, day, hour

  ! atmospheric forcing variables
  double precision, dimension(sim_length, n_hrus), intent(in):: map, ptps, mat
  double precision, dimension(sim_length, n_hrus):: pet, mat_adjusted 
  double precision:: map_step, ptps_step, mat_step, pet_step

  ! area weighted forcings for climo calculations
  double precision, dimension(sim_length):: map_aw, ptps_aw, mat_aw, pet_aw

  double precision, dimension(12):: map_climo, mat_climo, pet_climo, ptps_climo


  integer, dimension(12) :: mdays, mdays_prev
  double precision :: dayn, dayi
  integer :: mo
  double precision, dimension(12) :: mat_adj_prev, mat_adj_next
  double precision, dimension(12) :: map_adj_prev, map_adj_next
  double precision, dimension(12) :: pet_adj_prev, pet_adj_next
  double precision, dimension(12) :: ptps_adj_prev, ptps_adj_next
  double precision, dimension(12,n_hrus) :: peadj_m_prev, peadj_m_next

  double precision:: mat_adj_step, map_adj_step, pet_adj_step, ptps_adj_step, peadj_step

  double precision:: dr, rho, omega_s, Ra
  integer, dimension(sim_length):: jday
  double precision:: pet_ts, tmax_daily, tmin_daily, tave_daily
  integer:: ts_per_day

  ! initilize outputs 
  tci = 0

  mdays =      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) 
  mdays_prev = (/ 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 /) 

  ! pull out the initial conditions to separate variables
    init_swe = init(1,:)
  init_uztwc = init(2,:)
  init_uzfwc = init(3,:)
  init_lztwc = init(4,:)
  init_lzfsc = init(5,:)
  init_lzfpc = init(6,:)
  init_adimc = init(7,:)

  ! pull out sac params to separate variables
  uztwm = sac_pars(1,:)
  uzfwm = sac_pars(2,:)
  lztwm = sac_pars(3,:)
  lzfpm = sac_pars(4,:)
  lzfsm = sac_pars(5,:)
  adimp = sac_pars(6,:)
    uzk = sac_pars(7,:)
   lzpk = sac_pars(8,:)
   lzsk = sac_pars(9,:)
  zperc = sac_pars(10,:)
   rexp = sac_pars(11,:)
  pctim = sac_pars(12,:)
  pfree = sac_pars(13,:)
   riva = sac_pars(14,:)
   side = sac_pars(15,:)
  rserv = sac_pars(16,:)

  ! pull out snow params to separate variables
    scf = snow_pars(1,:)
  mfmax = snow_pars(2,:)
  mfmin = snow_pars(3,:)
   uadj = snow_pars(4,:)
     si = snow_pars(5,:)
    nmf = snow_pars(6,:)
   tipm = snow_pars(7,:)
  mbase = snow_pars(8,:)
  plwhc = snow_pars(9,:)
  daygm = snow_pars(10,:)
  adc_a = snow_pars(11,:)
  adc_b = snow_pars(12,:)
  adc_c = snow_pars(13,:)


  ! do i=1,7
  !   write(*,*)init(i,:)
  ! end do
  ! do i=1,12
  !   write(*,*)map_fa_limits(i,:)
  ! end do
  ! write(*,*)map_fa_pars

  ts_per_day = 86400/dt
  ! write(*,*)'Timesteps per day:',ts_per_day

  ! this is not used, since ptps is input, but set it just so its not empty
  pxtemp = 0 

  ! julian day 
  jday = julian_day(year,month,day)

  ! check if real climo data is input 
  if(climo(1,1) <= -9999)then 
    calc_climo = .true.
  else 
    calc_climo = .false.
  end if 
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Compute the adjusted temperature for the entire POR 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  mat_adjusted = -99
  mat_aw = 0 

  ! area weighted forcings 
  do nh=1,n_hrus
    mat_aw = mat_aw + mat(:,nh) * area(nh)
  end do 

  mat_aw = mat_aw / sum(area)

  if(calc_climo)then
    ! compute 12 monthly climo values (calendar year)
    mat_climo = monthly_climo_mean(mat_aw, month)
  else
    do i = 1,12
      mat_climo(i) = climo(i,2)
    end do
  end if 
  
  ! expand forcing adjustment limits in case the limits are not set properly 
  do k=1,12
    if(mat_climo(k) < mat_fa_limits(k,1)) mat_fa_limits(k,1) = mat_climo(k) * 0.9
    if(mat_climo(k) > mat_fa_limits(k,2)) mat_fa_limits(k,2) = mat_climo(k) * 1.1
  end do 

  ! compute monthly adjustments using GW's method
  mat_adj = forcing_adjust_mat(mat_climo, mat_fa_pars, mat_fa_limits(:,1), mat_fa_limits(:,2))

  ! put the forcing adjustments in easy to use vectors
  mat_adj_prev(1) = mat_adj(12)
  mat_adj_prev(2:12) = mat_adj(1:11)
  mat_adj_next(12) = mat_adj(1)
  mat_adj_next(1:11) = mat_adj(2:12)

  ! compute adjusted temperature
  do nh = 1, n_hrus
    do i = 1, sim_length
      ! adjust days in february if the year is a leap year
      if(mod(year(i),100) .ne. 0 .and. mod(year(i),4) .eq. 0) then
        mdays(2) = 29 ! leap year
      else if(mod(year(i),400).eq.0) then
        mdays(2) = 29 ! leap year
      else
        mdays(2) = 28 ! not leap year
      endif

      ! interpolate between (x0,y0) and (x1,y1)
      ! y = y0 + (x-x0)*(y1-y0)/(x1-x0)
      ! interpolate between (day0,limit0)=(0,limit0) and (day1,limit1)=(dayn,limit1)
      ! y = limit0 + dayi/dayn*(limit1-limit0)
      mo = month(i)
      if(day(i) >= 15)then
        dayn = dble(mdays(mo))
        dayi = dble(day(i)) - 15. + dble(hour(i))/24.
        mat_adj_step = mat_adj(mo) + dayi/dayn*(mat_adj_next(mo)-mat_adj(mo))
      else if(day(i) < 15)then
        dayn = dble(mdays_prev(mo))
        dayi = dble(day(i)) + mdays_prev(mo) - 15. + dble(hour(i))/24.
        mat_adj_step = mat_adj_prev(mo) + dayi/dayn*(mat_adj(mo)-mat_adj_prev(mo))
      end if 
      mat_adjusted(i,nh) = mat(i,nh) + mat_adj_step
    end do 
  end do


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! compute HS PET for the entire run up front 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  pet_ts = 0d0
  pet = 0d0

  do nh = 1, n_hrus
    do i = 1, sim_length
      ! on the first timestep of the day (or for the whole day if the ts is daily), 
      ! compute the pet for the whole day, then divide it by the number of timesteps
      if((hour(i) .eq. 0) .or. (ts_per_day .eq. 1))then

        ! if(i .eq. 1)write(*,*)'mat',mat(i:(i+ts_per_day-1),nh)
        ! if(i .eq. 1)write(*,*) 'timestep', i, 'of', sim_length, year(i), month(i), day(i), hour(i), jday(i)
        tmax_daily = maxval(mat_adjusted(i:(i+ts_per_day-1),nh))
        tmin_daily = minval(mat_adjusted(i:(i+ts_per_day-1),nh))
        tave_daily = sum(mat_adjusted(i:(i+ts_per_day-1),nh))/dble(ts_per_day)

        !Calculate extraterrestrial radiation
        !Inverse Relative Distance Earth to Sun
        dr = 1. + 0.033 * dcos((2. * pi) / 365. * dble(jday(i)))
        !Solar Declination
        rho = 0.409 * dsin((2.*pi) / 365. * dble(jday(i)) - 1.39)
        !Sunset Hour
        omega_s = dacos(-dtan(latitude(nh)*pi/180.)*dtan(rho))
        !Extraterrestrial Radiation (MJm^-2*day^-1)
        Ra = (24. * 60.) / pi * 0.0820 * dr * (omega_s * dsin(latitude(nh) * pi / 180.) * dsin(rho) + &
                         dcos(latitude(nh) * pi / 180.) * dcos(rho) * dsin(omega_s))
        
        ! if(i .eq. 1)then
        !   write(*,*)'jday',jday(i)
        !   write(*,*)'dr',dr
        !   write(*,*)'rho',rho
        !   write(*,*)'omega_s',omega_s
        !   write(*,*)'mat',mat(i:(i+ts_per_day-1),nh)
        !   write(*,*)'mat',mat(1:4,nh)
        !   write(*,*)'tave_daily',tmax_daily
        !   write(*,*)'tmax_daily',tmin_daily
        !   write(*,*)'tmin_daily',tave_daily
        ! end if

        ! daily pet from Hargreaves-Semani equation, units are mm/day, so divide 
        ! by number of timesteps in a day   
        pet_ts = 0.0023 * (tave_daily + 17.8) * (tmax_daily - tmin_daily)**0.5 * Ra / 2.45
        ! if(i .eq. 1)write(*,*)tmax_daily, tmin_daily, tave_daily, pet_ts

        ! ignore negative values
        if(pet_ts < 0) pet_ts = 0

        ! apply global scaling peadj and distibute across timesteps 
        ! if(i .eq. 1)write(*,*)'pet_ts',pet_ts
        pet_ts = pet_ts * peadj(nh) / dble(ts_per_day)
        ! if(i .eq. 1)write(*,*)'pet_ts',pet_ts

      end if 
      pet(i,nh) = pet_ts 
    end do 
  end do

  ! write(*,*)'pet'
  ! do k=1,10
  !   write(*,*)pet(k,:)
  ! end do 


  map_aw = 0 
  ptps_aw = 0 
  pet_aw = 0

  ! area weighted forcings 
  do nh=1,n_hrus
    map_aw = map_aw + map(:,nh) * area(nh)
    pet_aw = pet_aw + pet(:,nh) * area(nh)
    ptps_aw = ptps_aw + ptps(:,nh) * area(nh)
  end do 

  map_aw = map_aw / sum(area)
  pet_aw = pet_aw / sum(area)
  ptps_aw = ptps_aw / sum(area)

  if(calc_climo)then
    ! compute 12 monthly climo values (calendar year)
    map_climo = monthly_climo_sum(map_aw, month)
    pet_climo = monthly_climo_sum(pet_aw, month)
    ptps_climo = monthly_climo_mean(ptps_aw, month)
  else 
    do i = 1,12
      map_climo(i) = climo(i,1)
      pet_climo(i) = climo(i,3)
      ptps_climo(i) = climo(i,4)
    end do
  end if 

  ! write(*,*)'climo: map, mat, pet, ptps'
  ! do k=1,12
  !   write(*,*)map_climo(k), mat_climo(k), pet_climo(k), ptps_climo(k)
  ! end do 
  ! write(*,*)'climo: lower, map, upper'
  ! do k=1,12
  !   write(*,*)map_fa_limits(k,1), map_climo(k), map_fa_limits(k,2)
  ! end do 
  ! return

  ! expand forcing adjustment limits in case the limits are not set properly 
  ! do k=1,12
  !   if(map_climo(k) < map_fa_limits(k,1)) map_fa_limits(k,1) = map_climo(k) * 0.9
  !   if(map_climo(k) > map_fa_limits(k,2)) map_fa_limits(k,2) = map_climo(k) * 1.1
  !   if(pet_climo(k) < pet_fa_limits(k,1)) pet_fa_limits(k,1) = pet_climo(k) * 0.9
  !   if(pet_climo(k) > pet_fa_limits(k,2)) pet_fa_limits(k,2) = pet_climo(k) * 1.1
  !   if(ptps_climo(k) < ptps_fa_limits(k,1)) ptps_fa_limits(k,1) = ptps_climo(k) * 0.75
  !   if(ptps_climo(k) > ptps_fa_limits(k,2)) ptps_fa_limits(k,2) = ptps_climo(k) * 1.25
  ! end do 

  ! compute monthly adjustments using GW's method
  map_adj = forcing_adjust_map_pet_ptps(map_climo, map_fa_pars, map_fa_limits(:,1), map_fa_limits(:,2))
  pet_adj = forcing_adjust_map_pet_ptps(pet_climo, pet_fa_pars, pet_fa_limits(:,1), pet_fa_limits(:,2))
  ptps_adj = forcing_adjust_map_pet_ptps(ptps_climo, ptps_fa_pars, ptps_fa_limits(:,1), ptps_fa_limits(:,2))

  ! write(*,*)'FA pars: ', mat_fa_pars
  ! do k=1,12
  !  write(*,*)map_adj(k),mat_adj(k),pet_adj(k),ptps_adj(k)
  ! end do 

  ! put the forcing adjustments in easy to use vectors
  map_adj_prev(1) = map_adj(12)
  map_adj_prev(2:12) = map_adj(1:11)
  map_adj_next(12) = map_adj(1)
  map_adj_next(1:11) = map_adj(2:12)

  pet_adj_prev(1) = pet_adj(12)
  pet_adj_prev(2:12) = pet_adj(1:11)
  pet_adj_next(12) = pet_adj(1)
  pet_adj_next(1:11) = pet_adj(2:12)

  ptps_adj_prev(1) = ptps_adj(12)
  ptps_adj_prev(2:12) = ptps_adj(1:11)
  ptps_adj_next(12) = ptps_adj(1)
  ptps_adj_next(1:11) = ptps_adj(2:12)

  !write(*,"(12f8.2)")map_adj(:,1)
  !write(*,"(12f8.2)")mat_adj(:,1)
  !write(*,"(12f8.2)")ptps_adj(:,1)
  !write(*,"(12f8.2)")pet_adj(:,1)
  !write(*,"(12f8.2)")peadj_m(:,1)

  ! ========================= HRU AREA LOOP ========================================================
  !   loop through the simulation areas, running the lump model code and averaging the output

  ! print*, '--------------------------------------------------------------'

  do nh=1,n_hrus
    ! print*, 'Running area',nh,'out of',n_hrus

    ! there are monthly crop coefficients per zone 
    peadj_m_prev(1,nh) = peadj_m(12,nh)
    peadj_m_prev(2:12,nh) = peadj_m(1:11,nh)
    peadj_m_next(12,nh) = peadj_m(1,nh)
    peadj_m_next(1:11,nh) = peadj_m(2:12,nh)

    ! set the areal depletion curve based on parameters ax^b+(1-a)x^c
    ! 0 < a < 1; b, c > 0 
    ! if b < 1 & c < 1 curve is concave up
    ! if b > 1 & c > 1 curve is concave up
    ! if b < 1 & c > 1 OR b > 1 & c < 1 curve is s-shaped
    adc_x = (/ 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0 /)
    adc = adc_a(nh)*adc_x**adc_b(nh)+(1.-adc_a(nh))*adc_x**adc_c(nh)
    ! "A value of As = 0.05 is used for a W/Ai = 0.0 ratio so that small amounts of snow
    ! don’t continue to exist well past the time when all the snow is gone in nature."
    ! - snow 17 manual 
    adc(1) = 0.05

    ! print run dates
    ! write(*,*)'  start:',year(1), month(1), day(1), hour(1)
    ! write(*,*)'    end:',year(sim_length), month(sim_length), day(sim_length), hour(sim_length)

    ! ================== RUN models for huc_area! ==========================================
  
    ! get sfc_pressure (pa is estimate by subroutine, needed by snow17 call)
    pa = sfc_pressure(elev(nh))
  
    ! set single precision sac state variables to initial values
    
    ! we are not warm starting from a state file
    uztwc_sp = real(init_uztwc(nh))
    uzfwc_sp = real(init_uzfwc(nh))
    lztwc_sp = real(init_lztwc(nh))
    lzfsc_sp = real(init_lzfsc(nh))
    lzfpc_sp = real(init_lzfpc(nh))
    adimc_sp = real(init_adimc(nh))
    ! AWW: just initialize first/main component of SWE (model 'WE')
    cs(1)    = real(init_swe(nh))
    ! set the rest to zero
    cs(2:19) = 0
    tprev    = real(mat(1,nh))

  
    ! =============== START SIMULATION TIME LOOP =====================================
    do i = 1,sim_length,1

      ! adjust days in february if the year is a leap year
      if(mod(year(i),100) .ne. 0 .and. mod(year(i),4) .eq. 0) then
        mdays(2) = 29 ! leap year
      else if(mod(year(i),400).eq.0) then
        mdays(2) = 29 ! leap year
      else
        mdays(2) = 28 ! not leap year
      endif


      ! interpolate between (x0,y0) and (x1,y1)
      ! y = y0 + (x-x0)*(y1-y0)/(x1-x0)
      ! interpolate between (day0,limit0)=(0,limit0) and (day1,limit1)=(dayn,limit1)
      ! y = limit0 + dayi/dayn*(limit1-limit0)
      mo = month(i)
      !write(*,*)mdays, mdays(mo), day(i), hour(i)
      if(day(i) >= 15)then
        dayn = dble(mdays(mo))
        dayi = dble(day(i)) - 15. + dble(hour(i))/24.
        map_adj_step = map_adj(mo) + dayi/dayn*(map_adj_next(mo)-map_adj(mo))
        pet_adj_step = pet_adj(mo) + dayi/dayn*(pet_adj_next(mo)-pet_adj(mo))
        ptps_adj_step = ptps_adj(mo) + dayi/dayn*(ptps_adj_next(mo)-ptps_adj(mo))
        peadj_step = peadj_m(mo,nh) + dayi/dayn*(peadj_m_next(mo,nh)-peadj_m(mo,nh))
      else if(day(i) < 15)then
        dayn = dble(mdays_prev(mo))
        dayi = dble(day(i)) + mdays_prev(mo) - 15. + dble(hour(i))/24.
        map_adj_step = map_adj_prev(mo) + dayi/dayn*(map_adj(mo)-map_adj_prev(mo))
        pet_adj_step = pet_adj_prev(mo) + dayi/dayn*(pet_adj(mo)-pet_adj_prev(mo))
        ptps_adj_step = ptps_adj_prev(mo) + dayi/dayn*(ptps_adj(mo)-ptps_adj_prev(mo))
        peadj_step = peadj_m_prev(mo,nh) + dayi/dayn*(peadj_m(mo,nh)-peadj_m_prev(mo,nh))
      end if 

      ! write(*,'(a,5i5,8f8.2)')'before adj',nh, year(i), month(i), day(i), hour(i), map(i,nh), mat(i,nh), ptps(i,nh), pet(i,nh), &
      !                                    mat_adj_step, map_adj_step, pet_adj_step, ptps_adj_step

      mat_step = mat_adjusted(i,nh)
      ! apply PXADJ (scaling the input values)
      map_step = map(i,nh) * pxadj(nh) * map_adj_step
      ! pet(i,nh) is the pet from HS, 
      ! peadj_step is the conversion to etdemand (crop factor)
      ! pet_adj_step is the forcing adjustment
      pet_step = pet(i,nh) * peadj_step * pet_adj_step
      ptps_step = min(ptps(i,nh) * ptps_adj_step, 1d0)
      ! write(*,'(a,5i5,8f8.2)')' after adj',nh, year(i), month(i), day(i), hour(i), map(i,nh), mat(i,nh), ptps(i,nh), pet(i,nh), &
      !                                    mat_adj_step, map_adj_step, pet_adj_step, ptps_adj_step

      ! if(i .eq. 1)then
      !   write(*,*)
      !   write(*,*)'Snow inputs:'
      !   write(*,'(a10,i12)')'dt_sec',int(dt,4)
      !   write(*,'(a10,i12)')'dt_hr',int(dt/sec_hour,4)
      !   write(*,'(a10,i12)')'day',int(day(i),4)
      !   write(*,'(a10,i12)')'month',int(month(i),4)
      !   write(*,'(a10,i12)')'year',int(year(i),4)
      !   write(*,'(a10,f30.17)')'map',real(map(i,nh))
      !   write(*,'(a10,f30.17)')'mat',real(mat(i,nh))
      !   write(*,'(a10,f30.17)')'ptps',real(ptps(i,nh))
      !   write(*,'(a10,f30.17)')'raim',raim(i,nh)
      !   write(*,'(a10,f30.17)')'alat',real(latitude(nh))
      !   write(*,'(a10,f30.17)')'scf',real(scf(nh))
      !   write(*,'(a10,f30.17)')'mfmax',real(mfmax(nh)) 
      !   write(*,'(a10,f30.17)')'mfmin',real(mfmin(nh))
      !   write(*,'(a10,f30.17)')'uadj',real(uadj(nh))
      !   write(*,'(a10,f30.17)')'si',real(si(nh))
      !   write(*,'(a10,f30.17)')'nmf',real(nmf(nh))
      !   write(*,'(a10,f30.17)')'tipm',real(tipm(nh))
      !   write(*,'(a10,f30.17)')'mbase',real(mbase(nh))
      !   write(*,'(a10,f30.17)')'pxtemp',real(pxtemp(nh))
      !   write(*,'(a10,f30.17)')'plwhc',real(plwhc(nh))
      !   write(*,'(a10,f30.17)')'daygm',real(daygm(nh))
      !   write(*,'(a10,f30.17)')'elev',real(elev(nh))
      !   write(*,'(a10,f30.17)')'pa',real(pa) 
      !   write(*,'(a10,11f5.2)')'adc',real(adc)
      !   write(*,'(a10,f30.17)')'cs(1)',cs(1)
      !   write(*,'(a10,f30.17)')'cs(2)',cs(2)
      !   write(*,'(a10,f30.17)')'cs(3)',cs(3)
      !   write(*,'(a10,f30.17)')'cs(4)',cs(4)
      !   write(*,'(a10,f30.17)')'cs(5)',cs(5)
      !   write(*,'(a10,f30.17)')'cs(6)',cs(6)
      !   write(*,'(a10,f30.17)')'cs(7)',cs(7)
      !   write(*,'(a10,f30.17)')'cs(8)',cs(8)
      !   write(*,'(a10,f30.17)')'cs(9)',cs(9)
      !   write(*,'(a10,f30.17)')'cs(10)',cs(10)
      !   write(*,'(a10,f30.17)')'cs(11)',cs(11)
      !   write(*,'(a10,f30.17)')'cs(12)',cs(12)
      !   write(*,'(a10,f30.17)')'cs(13)',cs(13)
      !   write(*,'(a10,f30.17)')'cs(14)',cs(14)
      !   write(*,'(a10,f30.17)')'cs(15)',cs(15)
      !   write(*,'(a10,f30.17)')'cs(16)',cs(16)
      !   write(*,'(a10,f30.17)')'cs(17)',cs(17)
      !   write(*,'(a10,f30.17)')'cs(18)',cs(18)
      !   write(*,'(a10,f30.17)')'cs(19)',cs(19)
      !   write(*,'(a10,f30.17)')'tprev',tprev
      ! end if 



      call exsnow19(int(dt,4),int(dt/sec_hour,4),int(day(i),4),int(month(i),4),int(year(i),4),&
          !SNOW17 INPUT AND OUTPUT VARIABLES
          real(map_step), real(ptps_step), real(mat_step), &
          raim(i,nh), sneqv(i,nh), snow(i,nh), snowh(i,nh),&
          !SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
          real(latitude(nh)), real(scf(nh)), real(mfmax(nh)), real(mfmin(nh)), &
          real(uadj(nh)), real(si(nh)), real(nmf(nh)), &
          real(tipm(nh)), real(mbase(nh)), real(pxtemp(nh)), real(plwhc(nh)), real(daygm(nh)),&
          real(elev(nh)), real(pa), real(adc), &
          !SNOW17 CARRYOVER VARIABLES
          cs, tprev) 

      ! swe(i,nh) = dble(cs(1))

      ! if(i .eq. 1)then
      !   write(*,*)
      !   write(*,*)'Snow outputs:'
      !   write(*,'(a10,f30.17)')'raim',raim(i,nh)
      !   write(*,'(a10,f30.17)')'sneqv',sneqv(i,nh)
      !   write(*,'(a10,f30.17)')'snow',snow(i,nh)
      !   write(*,'(a10,f30.17)')'snowh',snowh(i,nh)
      !   write(*,'(a10,f30.17)')'swe',cs(1)
      !   write(*,'(a10,f30.17)')'cs(2)',cs(2)
      !   write(*,'(a10,f30.17)')'cs(3)',cs(3)
      !   write(*,'(a10,f30.17)')'cs(4)',cs(4)
      !   write(*,'(a10,f30.17)')'cs(5)',cs(5)
      !   write(*,'(a10,f30.17)')'cs(6)',cs(6)
      !   write(*,'(a10,f30.17)')'cs(7)',cs(7)
      !   write(*,'(a10,f30.17)')'cs(8)',cs(8)
      !   write(*,'(a10,f30.17)')'cs(9)',cs(9)
      !   write(*,'(a10,f30.17)')'cs(10)',cs(10)
      !   write(*,'(a10,f30.17)')'cs(11)',cs(11)
      !   write(*,'(a10,f30.17)')'cs(12)',cs(12)
      !   write(*,'(a10,f30.17)')'cs(13)',cs(13)
      !   write(*,'(a10,f30.17)')'cs(14)',cs(14)
      !   write(*,'(a10,f30.17)')'cs(15)',cs(15)
      !   write(*,'(a10,f30.17)')'cs(16)',cs(16)
      !   write(*,'(a10,f30.17)')'cs(17)',cs(17)
      !   write(*,'(a10,f30.17)')'cs(18)',cs(18)
      !   write(*,'(a10,f30.17)')'cs(19)',cs(19)
      !   write(*,'(a10,f30.17)')'tprev',tprev
      ! end if 

      !cs_states(:,i,nh)  = cs
      !tprev_states(i,nh) = tprev

      !write(*,*)tprev

      ! tprev does not get updated in place like cs does
      tprev = real(mat_step)

      ! if(i .eq. 1)then
      !   write(*,*)
      !   write(*,*)'Sac inputs:'
      !   write(*,'(a10,f30.17)')'dt',real(dt)
      !   write(*,'(a10,f30.17)')'raim',raim(i,nh)
      !   write(*,'(a10,f30.17)')'mat',real(mat(i,nh))
      !   write(*,'(a10,f30.17)')'pet',real(pet(i,nh))
      !   write(*,'(a10,f30.17)')'uztwm',real(uztwm(nh))
      !   write(*,'(a10,f30.17)')'uzfwm',real(uzfwm(nh))
      !   write(*,'(a10,f30.17)')'uzk',real(uzk(nh))
      !   write(*,'(a10,f30.17)')'pctim',real(pctim(nh)) 
      !   write(*,'(a10,f30.17)')'adimp',real(adimp(nh))
      !   write(*,'(a10,f30.17)')'riva',real(riva(nh))
      !   write(*,'(a10,f30.17)')'zperc',real(zperc(nh))
      !   write(*,'(a10,f30.17)')'rexp',real(rexp(nh))
      !   write(*,'(a10,f30.17)')'lztwm',real(lztwm(nh))
      !   write(*,'(a10,f30.17)')'lzfsm',real(lzfsm(nh))
      !   write(*,'(a10,f30.17)')'lzfpm',real(lzfpm(nh))
      !   write(*,'(a10,f30.17)')'lzsk',real(lzsk(nh))
      !   write(*,'(a10,f30.17)')'lzpk',real(lzpk(nh))
      !   write(*,'(a10,f30.17)')'pfree',real(pfree(nh))
      !   write(*,'(a10,f30.17)')'side',real(side(nh))
      !   write(*,'(a10,f30.17)')'rserv',real(rserv(nh))
      ! end if 

      ! if(i .eq. 1)then
      !   write(*,*)
      !   write(*,*)'Sac States:'
      !   write(*,'(a10,f30.17)')'uztwc',uztwc_sp
      !   write(*,'(a10,f30.17)')'uzfwc',uzfwc_sp
      !   write(*,'(a10,f30.17)')'lztwc',lztwc_sp
      !   write(*,'(a10,f30.17)')'lzfsc',lzfsc_sp
      !   write(*,'(a10,f30.17)')'lzfpc',lzfpc_sp
      !   write(*,'(a10,f30.17)')'adimc',adimc_sp
      ! end if 
  
      call exsac(1, real(dt), raim(i,nh), real(mat_step), real(pet_step), &
          !SAC PARAMETERS
          !UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC, &
          !REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE, &
          !SIDE,RSERV, &
          real(uztwm(nh)), real(uzfwm(nh)), real(uzk(nh)), real(pctim(nh)), &
          real(adimp(nh)), real(riva(nh)), real(zperc(nh)), &
          real(rexp(nh)), real(lztwm(nh)), real(lzfsm(nh)), real(lzfpm(nh)), &
          real(lzsk(nh)), real(lzpk(nh)), real(pfree(nh)),&
          real(side(nh)), real(rserv(nh)), &
          !SAC State variables
          uztwc_sp, uzfwc_sp, lztwc_sp, lzfsc_sp, lzfpc_sp, adimc_sp, &
          !SAC OUTPUTS
          qs(i,nh), qg(i,nh), tci_sp(i,nh), aet(i,nh))

      ! if(i .eq. 1)then
      !   write(*,*)
      !   write(*,*)'Sac Outputs:'
      !   write(*,'(a10,f30.17)')'uztwc',uztwc_sp
      !   write(*,'(a10,f30.17)')'uzfwc',uzfwc_sp
      !   write(*,'(a10,f30.17)')'lztwc',lztwc_sp
      !   write(*,'(a10,f30.17)')'lzfsc',lzfsc_sp
      !   write(*,'(a10,f30.17)')'lzfpc',lzfpc_sp
      !   write(*,'(a10,f30.17)')'adimc',adimc_sp
      !   write(*,'(a10,f30.17)')'qs',qs(i,nh)
      !   write(*,'(a10,f30.17)')'qg',qg(i,nh)
      !   write(*,'(a10,f30.17)')'q',tci_sp(i,nh)
      !   write(*,'(a10,f30.17)')'aet',aet(i,nh)
      !   write(*,*)'***************************************************************'
      ! end if 
    
      ! place state variables in output arrays
      ! uztwc(i,nh) = dble(uztwc_sp)
      ! uzfwc(i,nh) = dble(uzfwc_sp)
      ! lztwc(i,nh) = dble(lztwc_sp)
      ! lzfsc(i,nh) = dble(lzfsc_sp)
      ! lzfpc(i,nh) = dble(lzfpc_sp)
      ! adimc(i,nh) = dble(adimc_sp)
      tci(i,nh) = dble(tci_sp(i,nh))
      ! aet(i,nh) = dble(aet_sp(i,nh))

      !write(*,'(5i5,4f8.2)')nh, year(i), month(i), day(i), hour(i), map(i,nh), mat(i,nh), ptps(i,nh), pet(i,nh), 
      !write(*,'(4i5,7f8.3)')year(i), month(i), day(i), hour(i), uztwc_sp, uzfwc_sp, lztwc_sp, &
      !                       lzfsc_sp, lzfpc_sp, adimc_sp, tci_sp(i,nh)
      

    end do  
    ! ============ end simulation time loop ====================

  end do   ! ========== END of simulation areas loop   ====================

  ! ====== print combined simulation output ============
  ! print*, 'Sim_length (days) =',sim_length/(86400/dt)
  ! print*, '--------------------------------------------------------------'

end subroutine