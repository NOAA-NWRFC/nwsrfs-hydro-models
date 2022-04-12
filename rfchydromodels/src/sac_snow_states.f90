subroutine sacsnowstates(n_hrus, dt, sim_length, year, month, day, hour, &
    latitude, elev, &
    sac_pars, &
    peadj, pxadj, &
    snow_pars, & 
    init_swe, & 
    map, ptps, mat, etd, &
    tci, aet, uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc, &
    swe, aesc, neghs, liqw, raim, psfall, prain)


    ! ! zone info 
    ! latitude, elev, area, &
    ! ! sac-sma params in a matrix, see the variable declaration
    ! sac_pars, &
    ! ! zone specific etd (peadj) and map (pxadj) adjustments 
    ! peadj, pxadj, &
    ! ! snow17 params in a matrix, see the variable declaration
    ! snow_pars, & 
    ! ! initial state value for swe 
    ! init_swe, & 
    ! ! forcings 
    ! map, ptps, mat, etd, &
    ! ! outputs
    ! tci, aet, uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc, &
    ! swe, aesc, neghs, liqw, raim, psfall, prain)

  use utilities

  implicit none

  double precision, parameter:: pi=3.141592653589793238462643383279502884197
  double precision, parameter:: sec_day = 86400.     !seconds in a day
  double precision, parameter:: sec_hour = 3600.     !seconds in an hour
  integer, parameter:: sp = KIND(1.0)
  integer:: k

  integer, intent(in):: n_hrus ! number of HRU areas in parameter files

  ! sac pars matrix 
  ! uztwm, uzfwm, lztwm, lzfpm, lzfsm, adimp, uzk, lzpk, lzsk, zperc, rexp, pctim, pfree, riva, side, rserv, efc
  double precision, dimension(17,n_hrus), intent(in):: sac_pars 

  ! snow pars marix 
  ! scf, mfmax, mfmin, uadj, si, nmf, tipm, mbase, plwhc, daygm, adc_a, adc_b, adc_c
  double precision, dimension(13,n_hrus), intent(in):: snow_pars 

  ! this code is currently not set up to do any timestep less than 1 hour, 
  ! nor could it do fractional hour timesteps.
  integer, intent(in):: dt    ! model timestep in seconds
  integer:: dt_hours          ! model timestep in hours
  integer:: ts_per_day, ts_per_year

  ! initial states for a cold start run
  ! used in all model HRUs
  ! model state variables not listed start at 0
  double precision, dimension(6):: spin_up_start_states, spin_up_end_states
  integer:: spin_up_counter, spin_up_max_iter
  double precision:: pdiff
  double precision, dimension(n_hrus):: init_swe, init_uztwc, init_uzfwc, init_lztwc, init_lzfsc, &
          init_lzfpc, init_adimc

  ! SAC_model params & other key inputs in the sace param file
  !character(len = 20), dimension(n_hrus) :: hru_id   ! local hru id
  double precision, dimension(n_hrus):: uztwm, uzfwm, uzk, pctim, adimp, zperc, rexp, &
                                lztwm, lzfsm, lzfpm, lzsk, lzpk, pfree, &
                                riva, side, rserv, efc, peadj, pxadj

  ! Snow17_model params 
  double precision, dimension(n_hrus), intent(in):: latitude   ! decimal degrees
  double precision, dimension(n_hrus), intent(in):: elev       ! m
  double precision, dimension(n_hrus):: scf, mfmax, mfmin, uadj, si, nmf, tipm, mbase, plwhc, daygm
  double precision, dimension(n_hrus):: pxtemp ! not used, set to zero
  double precision, dimension(11):: adc  ! different for each hru
  double precision, dimension(n_hrus):: adc_a, adc_b, adc_c ! areal depletion curve parameters ax^b+(1-a)x^c
  double precision, dimension(11) :: adc_x

  ! local variables
  integer:: nh,i,j           ! AWW index for looping through areas
  integer:: sim_length   ! length of simulation (days)

  ! single precision sac-sma and snow variables
  ! these are single precision so as to be supplied to 
  ! NWS f77 models 
  real(sp):: uztwc_sp, uzfwc_sp, lztwc_sp, lzfsc_sp, lzfpc_sp, adimc_sp

  ! snow-17 carry over variables
  double precision:: pa       ! snow-17 surface pressure
  real(sp):: taprev_sp    ! carry over variable
  real(sp), dimension(19):: cs       ! carry over variable array

  !swe calculation varibles
  double precision::  TEX

  ! sac-sma state variables
  double precision, dimension(sim_length ,n_hrus), intent(out):: uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc
  ! snow state variables
  double precision, dimension(sim_length ,n_hrus), intent(out):: swe, aesc, neghs, liqw, raim, psfall,prain
  integer:: nexlag


  ! sac-sma output variables and channel inflow
  real(sp):: qs_sp, qg_sp, aet_sp, tci_sp
  double precision, dimension(sim_length ,n_hrus), intent(out):: tci, aet

  ! snow-17 output variables  
  real(sp):: raim_sp, snowh_sp, sneqv_sp, snow_sp, psfall_sp, prain_sp, aesc_sp

  ! date variables
  integer, dimension(sim_length), intent(in):: year, month, day, hour

  ! atmospheric forcing variables
  !f2py intent(in,out) map, etd
  double precision, dimension(sim_length, n_hrus), intent(inout):: map, etd
  double precision, dimension(sim_length, n_hrus), intent(in):: ptps, mat
  double precision:: map_step, etd_step

  ! initilize outputs 
  tci = 0
  aet = 0 
  uztwc = 0 
  uzfwc = 0 
  lztwc = 0 
  lzfsc = 0 
  lzfpc = 0 
  adimc = 0 
  swe = 0
  aesc = 0
  neghs = 0
  liqw = 0
  raim = 0
  psfall = 0
  prain = 0

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
    efc = sac_pars(17,:)

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

  ts_per_day = 86400/dt
  dt_hours = dt/3600
  ! write(*,*)'Timesteps per day:',ts_per_day

  ! this is not used, since ptps is input, but set it just so its not empty
  pxtemp = 0
  
  ! ========================= HRU AREA LOOP ========================================================
  !   loop through the zones, running the lumped model code for each

  do nh=1,n_hrus
    ! print*, 'Running area',nh,'out of',n_hrus

    ! print run dates
    ! write(*,*)'  start:',year(1), month(1), day(1), hour(1)
    ! write(*,*)'    end:',year(sim_length), month(sim_length), day(sim_length), hour(sim_length)

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
    do i=1,11
      if(adc(i) < 0.05) adc(i) = 0.05
    end do
  
    ! get sfc_pressure (pa is estimate by subroutine, needed by snow17 call)
    pa = sfc_pressure(elev(nh))
  
    ! =============== Spin up procedure =====================================

    ! starting values
    spin_up_start_states = 1d0 
    spin_up_end_states = 0d0
    pdiff = 1d0
    ts_per_year = ts_per_day * 365
    spin_up_counter = 0
    spin_up_max_iter = 50

    do while (pdiff > 0.01 .and. spin_up_counter < spin_up_max_iter)

      spin_up_counter = spin_up_counter + 1

      ! put the ending states from the previous iteration as the starting states 
      uztwc_sp = real(spin_up_end_states(1))
      uzfwc_sp = real(spin_up_end_states(2))
      lztwc_sp = real(spin_up_end_states(3))
      lzfsc_sp = real(spin_up_end_states(4))
      lzfpc_sp = real(spin_up_end_states(5))
      adimc_sp = real(spin_up_end_states(6))

      ! inital swe will usually be 0, except for glaciers 
      cs(1) = real(init_swe(nh))
      ! set the rest to zero
      cs(2:19) = 0.0
      taprev_sp = real(mat(1,nh))

      psfall_sp = real(0)
      prain_sp = real(0)
      aesc_sp = real(0)

      ! run for 1 year 
      do i = 1,ts_per_year

        ! apply pe and px adjustments (zone-wise) for the current timestep
        map_step = map(i,nh) * pxadj(nh)
        etd_step = etd(i,nh) * peadj(nh)

        call exsnow19(int(dt,4),int(dt/sec_hour,4),int(day(i),4),int(month(i),4),int(year(i),4),&
            !SNOW17 INPUT AND OUTPUT VARIABLES
            real(map_step), real(ptps(i,nh)), real(mat(i,nh)), &
            raim_sp, sneqv_sp, snow_sp, snowh_sp, psfall_sp, prain_sp, aesc_sp,&
            !SNOW17 PARAMETERS
            !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
            real(latitude(nh)), real(scf(nh)), real(mfmax(nh)), real(mfmin(nh)), &
            real(uadj(nh)), real(si(nh)), real(nmf(nh)), &
            real(tipm(nh)), real(mbase(nh)), real(pxtemp(nh)), real(plwhc(nh)), real(daygm(nh)),&
            real(elev(nh)), real(pa), real(adc), &
            !SNOW17 CARRYOVER VARIABLES
            cs, taprev_sp) 

        ! taprev does not get updated in place like cs does
        taprev_sp = real(mat(i,nh))

        ! modify ET demand using the effective forest cover 
        ! Anderson calb manual pdf page 232
        etd_step = efc(nh)*etd_step+(1d0-efc(nh))*(1d0-dble(aesc_sp))*etd_step
    
        call exsac(1, real(dt), raim_sp, real(mat(i,nh)), real(etd_step), &
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
            qs_sp, qg_sp, tci_sp, aet_sp)

      end do  ! spin up 1 year loop 

      spin_up_end_states(1) = dble(uztwc_sp)
      spin_up_end_states(2) = dble(uzfwc_sp)
      spin_up_end_states(3) = dble(lztwc_sp)
      spin_up_end_states(4) = dble(lzfsc_sp)
      spin_up_end_states(5) = dble(lzfpc_sp)
      spin_up_end_states(6) = dble(adimc_sp)

      pdiff = 0.0
      do k=1,6
        ! avoid divide by zero 
        if(spin_up_start_states(k) < 0.000001)then
          cycle
        end if
        pdiff = pdiff + abs(spin_up_start_states(k)-spin_up_end_states(k))/spin_up_start_states(k)
      end do
      ! on the first iteration all the states are at zero so 
      ! artificially set pdiff and keep going
      if(spin_up_counter .eq. 1) pdiff = 1.0

      spin_up_start_states = spin_up_end_states

      ! write(*,'(7f10.3)')pdiff, spin_up_start_states

    end do 
    ! write(*,*)

    ! Save the spun up states to use for init in the full run
    init_uztwc(nh) = spin_up_end_states(1)
    init_uzfwc(nh) = spin_up_end_states(2)
    init_lztwc(nh) = spin_up_end_states(3)
    init_lzfsc(nh) = spin_up_end_states(4)
    init_lzfpc(nh) = spin_up_end_states(5)
    init_adimc(nh) = spin_up_end_states(6)

    ! =============== End spin up procedure =====================================

    ! set single precision sac state variables to initial values
    uztwc_sp = real(init_uztwc(nh))
    uzfwc_sp = real(init_uzfwc(nh))
    lztwc_sp = real(init_lztwc(nh))
    lzfsc_sp = real(init_lzfsc(nh))
    lzfpc_sp = real(init_lzfpc(nh))
    adimc_sp = real(init_adimc(nh))

    ! initialize first/main component of SWE (model 'WE')
    ! inital swe will usually be 0, except for glaciers 
    cs(1) = real(init_swe(nh))
    ! set the rest to zero
    cs(2:19) = 0.0
    taprev_sp = real(mat(1,nh))
     
    psfall_sp = real(0)
    prain_sp = real(0)
    aesc_sp = real(0)
    
    ! =============== START SIMULATION TIME LOOP =====================================
    do i = 1,sim_length,1

      ! apply adjustments (zone-wise) for the current timestep
      map_step = map(i,nh) * pxadj(nh)
      etd_step = etd(i,nh) * peadj(nh) 

      call exsnow19(int(dt,4),int(dt/sec_hour,4),int(day(i),4),int(month(i),4),int(year(i),4),&
          !SNOW17 INPUT AND OUTPUT VARIABLES
          real(map_step), real(ptps(i,nh)), real(mat(i,nh)), &
          raim_sp, sneqv_sp, snow_sp, snowh_sp, psfall_sp, prain_sp, aesc_sp,&
          !SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
          real(latitude(nh)), real(scf(nh)), real(mfmax(nh)), real(mfmin(nh)), &
          real(uadj(nh)), real(si(nh)), real(nmf(nh)), &
          real(tipm(nh)), real(mbase(nh)), real(pxtemp(nh)), real(plwhc(nh)), real(daygm(nh)),&
          real(elev(nh)), real(pa), real(adc), &
          !SNOW17 CARRYOVER VARIABLES
          cs, taprev_sp) 


      ! taprev does not get updated in place like cs does
      taprev_sp = real(mat(i,nh))

      ! modify ET demand using the effective forest cover 
      ! Anderson calb manual pdf page 232
      etd_step = efc(nh)*etd_step+(1d0-efc(nh))*(1d0-dble(aesc_sp))*etd_step
  
      call exsac(1, real(dt), raim_sp, real(mat(i,nh)), real(etd_step), &
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
          qs_sp, qg_sp, tci_sp, aet_sp)
    
      ! place state variables in output arrays
      uztwc(i,nh) = dble(uztwc_sp)
      uzfwc(i,nh) = dble(uzfwc_sp)
      lztwc(i,nh) = dble(lztwc_sp)
      lzfsc(i,nh) = dble(lzfsc_sp)
      lzfpc(i,nh) = dble(lzfpc_sp)
      adimc(i,nh) = dble(adimc_sp)
      tci(i,nh) = dble(tci_sp)
      aet(i,nh) = dble(aet_sp)

      ! inout forcings to capture the pe/pxadj and efc
      map(i,nh) = map_step
      etd(i,nh) = etd_step

      raim(i,nh) = dble(raim_sp)
      psfall(i,nh) = dble(psfall_sp)
      prain(i,nh) = dble(prain_sp)
      neghs(i,nh) = dble(cs(2))
      liqw(i,nh) = dble(cs(3))
      aesc(i,nh) = dble(aesc_sp)

      nexlag = 5/int(dt/sec_hour) + 2

      TEX = 0.0
      DO j = 1,nexlag,1
        TEX = TEX+dble(cs(10+j))
      END DO

      swe(i,nh) = dble(cs(1))+dble(cs(3))+dble(cs(9))+TEX

      
      ! PQNET
      ! PRAIN
      ! PROBG
      ! PSNWRO
      ! SNSG
      ! TINDEX
      ! SWE

      ! SNOW=SXFALL
      ! RAIM=RM(1)
      ! SNEQV=TWE/1000.
      ! SNOWH=SNDPT/100.

    end do  ! ============ end simulation time loop ====================

  end do   ! ========== end of simulation areas loop   ====================

end subroutine