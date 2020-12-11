subroutine sacsnow(n_hrus, dt, sim_length, year, month, day, hour, &
    latitude, elev, &
    uztwm, uzfwm, lztwm, lzfpm, lzfsm, adimp, uzk, lzpk, lzsk, zperc, &
    rexp, pctim, pfree, riva, side, rserv, &
    peadj, pxadj, peadj_m, &
    scf, mfmax, mfmin, uadj, si, nmf, tipm, mbase, plwhc, daygm, &
    adc_a, adc_b, adc_c, & 
    map_adj, mat_adj, pet_adj, ptps_adj, & 
    init_swe, init_uztwc, init_uzfwc, init_lztwc, init_lzfsc, init_lzfpc, init_adimc, & 
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

  implicit none

  double precision, parameter:: pi=3.141592653589793238462643383279502884197
  double precision, parameter:: sec_day = 86400.     !seconds in a day
  double precision, parameter:: sec_hour = 3600.     !seconds in an hour
  integer, parameter:: sp = KIND(1.0)

    ! initialization variables
  ! these are in the namelist bloc &INIT_CONTROL
  integer, intent(in):: n_hrus ! number of HRU areas in parameter files

  integer, intent(in):: dt    ! model timestep in seconds
  !integer:: start_month, start_hour, start_day, start_year, &
  !          end_month, end_day, end_hour, end_year   

  ! initial states if for a cold start run
  ! used in all model HRUs
  ! model state variables not listed start at 0
  double precision, dimension(n_hrus), intent(in):: init_swe, init_uztwc, init_uzfwc, init_lztwc, init_lzfsc, &
          init_lzfpc, init_adimc

  ! SAC_model params & other key inputs in the sace param file
  !character(len = 20), dimension(n_hrus) :: hru_id   ! local hru id
  double precision, dimension(n_hrus), intent(in):: uztwm, uzfwm, uzk, pctim, adimp, zperc, rexp, &
                                lztwm, lzfsm, lzfpm, lzsk, lzpk, pfree, &
                                riva, side, rserv, peadj, pxadj

  ! Snow17_model params 
  double precision, dimension(n_hrus), intent(in):: latitude   ! decimal degrees
  double precision, dimension(n_hrus), intent(in):: elev       ! m
  double precision, dimension(n_hrus), intent(in):: scf, mfmax, mfmin, uadj, si, &
                                        nmf, tipm, mbase, plwhc, daygm
  double precision, dimension(n_hrus):: pxtemp ! not used, set to zero
  double precision, dimension(11):: adc  ! different for each hru
  double precision, dimension(n_hrus), intent(in):: adc_a, adc_b, adc_c ! areal depletion curve parameters ax^b+(1-a)x^c
  double precision, dimension(11) :: adc_x

  ! forcing adjustment parameters for map, mat, pet, ptps
  double precision, dimension(12, n_hrus), intent(in):: map_adj, mat_adj, pet_adj, ptps_adj, peadj_m

  ! local variables
  !character(len=10) :: state_date_str  ! AWW string to match date in input states
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
  double precision, dimension(sim_length ,n_hrus):: uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc, swe

  ! sac-sma output variables and routed flow
  !real(sp), dimension(sim_length):: qs_sp, qg_sp, eta_sp, tci_sp
  real(sp), dimension(sim_length, n_hrus):: qs, qg, eta, tci_sp
  double precision, dimension(sim_length ,n_hrus), intent(out):: tci

  ! snow-17 output variables  
  real(sp), dimension(sim_length, n_hrus):: raim, snowh, sneqv, snow 

  ! date variables
  integer, dimension(sim_length), intent(in):: year, month, day, hour

  ! atmospheric forcing variables
  double precision, dimension(sim_length, n_hrus):: pet, map, ptps, mat


  integer, dimension(12) :: mdays, mdays_prev
  double precision :: dayn, dayi
  integer :: mo
  double precision, dimension(12) :: mat_adj_prev, mat_adj_next
  double precision, dimension(12) :: map_adj_prev, map_adj_next
  double precision, dimension(12) :: pet_adj_prev, pet_adj_next
  double precision, dimension(12) :: ptps_adj_prev, ptps_adj_next
  double precision, dimension(12) :: peadj_m_prev, peadj_m_next

  double precision:: mat_adj_step, map_adj_step, pet_adj_step, ptps_adj_step, peadj_step

  double precision:: dr, rho, omega_s, Ra
  integer, dimension(sim_length):: jday
  double precision:: pet_ts, tmax_daily, tmin_daily, tave_daily
  integer:: ts_per_day

  mdays =      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) 
  mdays_prev = (/ 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 /) 

  ! =======  CODE starts below =====================================================================

  ts_per_day = 86400/dt
  ! write(*,*)'Timesteps per day:',ts_per_day

  ! this is not used, since ptps is input, but set it just so its not empty
  pxtemp = 0 

  ! ========================= HRU AREA LOOP ========================================================
  !   loop through the simulation areas, running the lump model code and averaging the output

  ! print*, '--------------------------------------------------------------'
 
  pet_ts = 0

  do nh=1,n_hrus
    ! print*, 'Running area',nh,'out of',n_hrus

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

    ! apply PXADJ (scaling the input values)
    map(:,nh) = map(:,nh) * pxadj(nh)

    ! print run dates
    ! write(*,*)'  start:',year(1), month(1), day(1), hour(1)
    ! write(*,*)'    end:',year(sim_length), month(sim_length), day(sim_length), hour(sim_length)

    ! put the forcing adjustments in easy to use vectors
    map_adj_prev(1) = map_adj(12,nh)
    map_adj_prev(2:12) = map_adj(1:11,nh)
    map_adj_next(12) = map_adj(1,nh)
    map_adj_next(1:11) = map_adj(2:12,nh)

    mat_adj_prev(1) = mat_adj(12,nh)
    mat_adj_prev(2:12) = mat_adj(1:11,nh)
    mat_adj_next(12) = mat_adj(1,nh)
    mat_adj_next(1:11) = mat_adj(2:12,nh)

    pet_adj_prev(1) = pet_adj(12,nh)
    pet_adj_prev(2:12) = pet_adj(1:11,nh)
    pet_adj_next(12) = pet_adj(1,nh)
    pet_adj_next(1:11) = pet_adj(2:12,nh)

    ptps_adj_prev(1) = ptps_adj(12,nh)
    ptps_adj_prev(2:12) = ptps_adj(1:11,nh)
    ptps_adj_next(12) = ptps_adj(1,nh)
    ptps_adj_next(1:11) = ptps_adj(2:12,nh)

    peadj_m_prev(1) = peadj_m(12,nh)
    peadj_m_prev(2:12) = peadj_m(1:11,nh)
    peadj_m_next(12) = peadj_m(1,nh)
    peadj_m_next(1:11) = peadj_m(2:12,nh)

    !write(*,"(12f8.2)")map_adj(:,1)
    !write(*,"(12f8.2)")mat_adj(:,1)
    !write(*,"(12f8.2)")ptps_adj(:,1)
    !write(*,"(12f8.2)")pet_adj(:,1)
    !write(*,"(12f8.2)")peadj_m(:,1)

    ! julian day 
    call julian_day(year,month,day,jday,sim_length)

    ! ================== RUN models for huc_area! ==========================================
  
    ! get sfc_pressure (pa is estimate by subroutine, needed by snow17 call)
    call sfc_pressure(elev(nh),pa)
  
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

      ! on the first timestep of the day (or for the whole day if the ts is daily), 
      ! compute the pet for the whole day, then divide it by the number of timesteps
      if((hour(i) .eq. 0) .or. (ts_per_day .eq. 1))then

        !write(*,*)'mat',mat(i:(i+ts_per_day-1))
        !write(*,*) 'timestep', i, 'of', sim_length, year(i), month(i), day(i), hour(i), jday(i)
        tmax_daily = maxval(mat(i:(i+ts_per_day-1),nh))
        tmin_daily = minval(mat(i:(i+ts_per_day-1),nh))
        tave_daily = sum(mat(i:(i+ts_per_day-1),nh))/dble(ts_per_day)

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
        ! write(*,*)jday(i)
        ! write(*,*)'dr',dr
        ! write(*,*)'rho',rho
        ! write(*,*)'omega_s',omega_s
        ! write(*,*)'mat',mat(i:(i+ts_per_day-1),nh)
        ! write(*,*)'mat',mat(1:4,nh)
        ! write(*,*)'tave_daily',tmax_daily
        ! write(*,*)'tmax_daily',tmin_daily
        ! write(*,*)'tmin_daily',tave_daily

        ! daily pet from Hargreaves-Semani equation, units are mm/day, so divide 
        ! by number of timesteps in a day   
        pet_ts = 0.0023 * (tave_daily + 17.8) * (tmax_daily - tmin_daily)**0.5 * Ra / 2.45
        !write(*,*)tmax_daily, tmin_daily, tave_daily, pet_ts

        ! ignore negative values
        if(pet_ts < 0) pet_ts = 0

        ! apply global scaling peadj and distibute across timesteps 
        ! write(*,*)'pet_ts',pet_ts
        pet_ts = pet_ts * peadj(nh) / dble(ts_per_day)
        ! write(*,*)'pet_ts',pet_ts
      end if 


      ! interpolate between (x0,y0) and (x1,y1)
      ! y = y0 + (x-x0)*(y1-y0)/(x1-x0)
      ! interpolate between (day0,limit0)=(0,limit0) and (day1,limit1)=(dayn,limit1)
      ! y = limit0 + dayi/dayn*(limit1-limit0)
      mo = month(i)
      !write(*,*)mdays, mdays(mo), day(i), hour(i)
      if(day(i) >= 15)then
        dayn = dble(mdays(mo))
        dayi = dble(day(i)) - 15. + dble(hour(i))/24.
        mat_adj_step = mat_adj(mo,nh) + dayi/dayn*(mat_adj_next(mo)-mat_adj(mo,nh))
        map_adj_step = map_adj(mo,nh) + dayi/dayn*(map_adj_next(mo)-map_adj(mo,nh))
        pet_adj_step = pet_adj(mo,nh) + dayi/dayn*(pet_adj_next(mo)-pet_adj(mo,nh))
        ptps_adj_step = ptps_adj(mo,nh) + dayi/dayn*(ptps_adj_next(mo)-ptps_adj(mo,nh))
        peadj_step = peadj_m(mo,nh) + dayi/dayn*(peadj_m_next(mo)-peadj_m(mo,nh))
      else if(day(i) < 15)then
        dayn = dble(mdays_prev(mo))
        dayi = dble(day(i)) + mdays_prev(mo) - 15. + dble(hour(i))/24.
        mat_adj_step = mat_adj_prev(mo) + dayi/dayn*(mat_adj(mo,nh)-mat_adj_prev(mo))
        map_adj_step = map_adj_prev(mo) + dayi/dayn*(map_adj(mo,nh)-map_adj_prev(mo))
        pet_adj_step = pet_adj_prev(mo) + dayi/dayn*(pet_adj(mo,nh)-pet_adj_prev(mo))
        ptps_adj_step = ptps_adj_prev(mo) + dayi/dayn*(ptps_adj(mo,nh)-ptps_adj_prev(mo))
        peadj_step = peadj_m_prev(mo) + dayi/dayn*(peadj_m(mo,nh)-peadj_m_prev(mo))
      end if 

      !write(*,'(a,5i5,8f8.2)')'before adj',nh, year(i), month(i), day(i), hour(i), map(i,nh), mat(i,nh), ptps(i,nh), pet(i,nh), &
      !                                    mat_adj_step, map_adj_step, pet_adj_step, ptps_adj_step

      mat(i,nh) = mat(i,nh) + mat_adj_step
      map(i,nh) = map(i,nh) * map_adj_step
      ! pet_ts is the pet from HS, peadj_step is the conversion to etdemand,
      ! pet_adj_step is the forcing adjustment
      pet(i,nh) = pet_ts * peadj_step * pet_adj_step
      ptps(i,nh) = min(ptps(i,nh) * ptps_adj_step,1.)
      !write(*,'(a,5i5,8f8.2)')' after adj',nh, year(i), month(i), day(i), hour(i), map(i,nh), mat(i,nh), ptps(i,nh), pet(i,nh), &
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
          real(map(i,nh)), real(ptps(i,nh)), real(mat(i,nh)), &
          raim(i,nh), sneqv(i,nh), snow(i,nh), snowh(i,nh),&
          !SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
          real(latitude(nh)), real(scf(nh)), real(mfmax(nh)), real(mfmin(nh)), &
          real(uadj(nh)), real(si(nh)), real(nmf(nh)), &
          real(tipm(nh)), real(mbase(nh)), real(pxtemp(nh)), real(plwhc(nh)), real(daygm(nh)),&
          real(elev(nh)), real(pa), real(adc), &
          !SNOW17 CARRYOVER VARIABLES
          cs, tprev) 

      swe(i,nh) = dble(cs(1))

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
      tprev = real(mat(i,nh))

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
  
      call exsac(real(dt), raim(i,nh), real(mat(i,nh)), real(pet(i,nh)), &
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
          qs(i,nh), qg(i,nh), tci_sp(i,nh), eta(i,nh))

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
      !   write(*,'(a10,f30.17)')'eta',eta(i,nh)
      !   write(*,*)'***************************************************************'
      ! end if 
    
      ! place state variables in output arrays
      uztwc(i,nh) = dble(uztwc_sp)
      uzfwc(i,nh) = dble(uzfwc_sp)
      lztwc(i,nh) = dble(lztwc_sp)
      lzfsc(i,nh) = dble(lzfsc_sp)
      lzfpc(i,nh) = dble(lzfpc_sp)
      adimc(i,nh) = dble(adimc_sp)
      tci(i,nh) = dble(tci_sp(i,nh))

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

subroutine sfc_pressure(elevation, sfc_pres)
  implicit none

  double precision, parameter :: sfc_pres_a=33.86      !constant for sfc pressure calc (snow-17)
  double precision, parameter :: sfc_pres_b=29.9     !constant for sfc pressure calc (snow-17)
  double precision, parameter :: sfc_pres_c=0.335      !constant for sfc pressure calc (snow-17)
  double precision, parameter :: sfc_pres_d=0.00022    !constant for sfc pressure calc (snow-17)
  double precision, parameter :: sfc_pres_e=2.4      !constant for sfc pressure calc (snow-17)

  double precision, intent(in)   :: elevation
  double precision, intent(out)  :: sfc_pres
  
  sfc_pres = sfc_pres_a * (sfc_pres_b - (sfc_pres_c * (elevation/100.)) &
             + (sfc_pres_d*((elevation/100.)**sfc_pres_e)))   !sfc pres in hPa

  return
end subroutine sfc_pressure

! The two Julian day routines are from G. Liston's SnowModel, but updated to F90
! A. Wood 2016 added two date math routines

subroutine julian_day(year,month,day,jday,sim_length)
! returns vector
  implicit none

  ! input variables
  integer:: sim_length
  integer, dimension(sim_length),intent(in) :: year
  integer, dimension(sim_length),intent(in) :: month
  integer, dimension(sim_length),intent(in) :: day

  ! output variables
  integer, dimension(sim_length),intent(out) :: jday

  ! Calculate the day of year (1,...,365,366) corresponding to the date
  !   iyear-imonth-iday. 
  jday = day &
          + min(1,max(0,month-1))*31 &
          + min(1,max(0,month-2))*(28+(1-min(1,mod(year,4)))) &
          + min(1,max(0,month-3))*31 &
          + min(1,max(0,month-4))*30 &
          + min(1,max(0,month-5))*31 &
          + min(1,max(0,month-6))*30 &
          + min(1,max(0,month-7))*31 &
          + min(1,max(0,month-8))*31 &
          + min(1,max(0,month-9))*30 &
          + min(1,max(0,month-10))*31 &
          + min(1,max(0,month-11))*30 &
          + min(1,max(0,month-12))*31

  return
end subroutine julian_day