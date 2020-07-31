! This code runs the Snow17, Sacramento and UH routing model for multiple HRUs within a basin
!   and makes an areal average of the output
!
! Orig:  2014 -- A. Newman (NCAR) chopped a single-zone model out of larger calibration codebase  
!
! Major Modifications: 
!
!   AWW-20160107:  
!     adapted to run with input pcp, tmax, tmin, and external pet forcings
!     added state file restarts and outputs
!     cleaned up and added documention
! 
!   AWW-20160121:  - made to run subareas in loop so that multiple outputs could be combined
!                    -- but each still required its own namelist; and it used a control file
!   AWW-20160823: - MAJOR CHANGES to make the code work like a typical stand-alone model
!                 - internal PET approach removed, forcings now expect prcp, tmax, tmin, PET
!                     static parameters removed from header of forcing files; they're now tables
!                 - multi-HRU functionality added down the the input level
!                     param files combined to be listed once each, in a single namelist (as arg)
!                 - calc's sim_length using date math instead of forcing read 
!                 - HRUs are internally combined and written out into a basin-total file
!                 - streamflow read/write functionality removed
!                 - improved messaging about run details
!   AWW-20161111: - changed the way state files are read - now can read the row format output
!                   of the write_state functions, and will seek a date that is one day before
!                   the starting day of the run, since that contains end-of-day state values
!   AWW-20161207: - fixed bug in uh_statefile write/read
!   AWW-20170117: - added peadj and pxadj as sac parameters for model calibration
!   CWB-20200521: - Allow for the timestep to be specefied in the namelist 
!                 - Input psfall (% snow fall) at each timestep
!   CWB-20200521: - Input only average temp for the timestep, not min and max
! 
! ====================================================================================

program multi_driver

  ! specify modules needed
  use nrtype
  use def_namelists
  use constants, only:   sec_hour,sec_day
  use interfaces, only:  read_namelist, sfc_pressure, &
                         read_areal_forcing, &
			 julian_day,write_snow17_state,write_sac_state, &
			 read_sac_state,read_snow17_state,read_uh_state, &
			 write_uh_state, date_diff_ndays, day_before_date
  implicit none

  ! local variables
  character(len=2000)	:: arg  !command line arg for namelist file
  character(len=2000)	:: namelist_name  !command line arg for namelist file
  character(len=2000)	:: combined_output_filename  ! AWW constructed on the fly
  character(len=2000)	:: hru_output_filename  ! AWW constructed on the fly
  character(len=10)	:: state_date_str  ! AWW string to match date in input states

  integer      :: nh           ! AWW index for looping through areas
  real(dp)     :: total_area   ! (sqkm) AWW needed for combining outputs
  integer(I4B) :: i, ntau, k, m
  integer(I4B) :: sim_length   ! length of simulation (days)
  real(sp)     :: dtuh	       ! for unit hydrograph

  ! single precision sac-sma state variables
  real(sp)	:: uztwc_sp   ! these are single precision so as to be supplied to 
  real(sp)	:: uzfwc_sp   !   old NWS code models
  real(sp)	:: lztwc_sp
  real(sp)	:: lzfsc_sp
  real(sp)	:: lzfpc_sp
  real(sp)	:: adimc_sp
  real(sp)	:: pet_sp
  real(sp)	:: tair_sp
  real(sp)	:: precip_sp
  real(sp)  :: psfall_sp

  ! snow-17 carry over variables
  real(dp)                :: pa       ! snow-17 surface pressure
  real(sp)                :: tprev    ! carry over variable
  real(sp),dimension(19)  :: cs       ! carry over variable array
  real(sp),dimension(:),allocatable    :: tprev_states    ! for state file write
  real(sp),dimension(:,:),allocatable  :: cs_states       ! for state file write

  ! unit hydrograph
  real(sp),dimension(200)  :: unit_hydro ! in time steps ... this dim sets UH_LENGTH: 
                                        ! could cause truncation if UH is very long
                                        ! needs to match setting in duamel

  integer(I4B)	:: uh_length, routing_flag
  integer(I4B)  :: TOC_length ! DUAMEL.f new return variable -- A.Wood Feb 2016

  ! arrays for using state warm start with unit hydrograph
  real(sp),dimension(:),allocatable	:: prior_tci, expanded_tci ! AWW added

  ! ==== ALLOCATABLE VARIABLES ====

  ! sac-sma state variables
  real(dp),dimension(:),allocatable     :: uztwc_dp, uztwc_comb  ! AWW added combined vars
  real(dp),dimension(:),allocatable     :: uzfwc_dp, uzfwc_comb
  real(dp),dimension(:),allocatable     :: lztwc_dp, lztwc_comb
  real(dp),dimension(:),allocatable     :: lzfsc_dp, lzfsc_comb
  real(dp),dimension(:),allocatable     :: lzfpc_dp, lzfpc_comb
  real(dp),dimension(:),allocatable     :: adimc_dp, adimc_comb

  ! sac-sma output variables and routed flow
  real(sp), dimension(:),allocatable	:: qs, qg, eta, tci, route_tci

  ! snow-17 output variables  single precision
  real(sp), dimension(:),allocatable    :: snowh, sneqv, snow !output variables for snow-17

  ! date variables
  integer, dimension(:),allocatable :: year, month, day, hour
  integer(I4B)                      :: state_year, state_month, state_day

  ! precip/snowmelt inputs to Sac from Snow17
  real(sp), dimension(:),allocatable :: raim

  ! atmospheric forcing variables
  real(dp), dimension(:),allocatable :: precip, pet, raw_precip, raw_pet
  real(dp), dimension(:),allocatable :: psfall
  ! real(dp), dimension(:),allocatable :: vpd, dayl, swdown ! used in pet calc; not used currently

  ! derived forcing variables
  real(dp), dimension(:),allocatable :: tair

  ! various other combined variables (aggregating multiple basins zones)
  real(sp), dimension(:),allocatable	:: eta_comb, tci_comb, route_tci_comb ! AWW combined vars
  real(sp), dimension(:),allocatable	:: route_tci_cfs ! AWW convert mm/d to cfs given basin areas
  real(sp), dimension(:),allocatable    :: sneqv_comb ! AWW ditto, combined vars
  real(sp), dimension(:),allocatable :: raim_comb  ! AWW combined vars
  real(dp), dimension(:),allocatable :: precip_comb, precip_scf_comb ! AWW combined vars
  real(dp), dimension(:),allocatable :: pet_comb, tair_comb  ! AWW combined vars
  real(dp), dimension(:),allocatable :: psfall_comb

  double precision, dimension(11) :: adc_x

  ! =======  CODE starts below =====================================================================

  ! get control file filename as argument
  i = 0
  do
    call get_command_argument(i,arg)
    if(i .eq. 1) namelist_name=arg   ! first argument has i=1
    if(LEN_TRIM(arg) == 0) EXIT
    i = i + 1
  end do

  ! read namelist file to get info on the current simulation areas
  call read_namelist(namelist_name)

  ! read parameter files (contain params for all huc areas)
  call read_sac_params(sac_param_file, n_hrus)
  call read_snow17_params(snow17_param_file, n_hrus)
  call read_uh_params(uh_param_file, n_hrus)

  if(use_forcing_adjust .eq. 1)then
    call read_forcing_adjust(forcing_adjust_file, n_hrus)
  end if

  ! determine whether to route (ie, if unit_shape is > 0 for any hru)
  routing_flag = 0 
  do nh=1,n_hrus
    if(unit_shape(nh) > 0) then
      routing_flag = 1
    end if
  end do

  ! determine length of unit hydrograph (check?)
  uh_length = size(unit_hydro,1)   

  ! ========================= HRU AREA LOOP ========================================================
  !   loop through the simulation areas, running the lump model code and averaging the output

  print*, '--------------------------------------------------------------'

  do nh=1,n_hrus
    print*, 'Running area',nh,'out of',n_hrus,'for watershed ', main_id

    ! set the areal depletion curve based on parameters ax^b+(1-a)x^c
    ! 0 < a < 1; b, c > 0 
    ! if b < 1 & c < 1 curve is concave up
    ! if b > 1 & c > 1 curve is concave up
    ! if b < 1 & c > 1 OR b > 1 & c < 1 curve is s-shaped
    adc_x = (/ 0.0d0, 0.1d0, 0.2d0, 0.3d0, 0.4d0, 0.5d0, 0.6d0, 0.7d0, 0.8d0, 0.9d0, 1.0d0 /)
    adc = adc_a(nh)*adc_x**adc_b(nh)+(1d0-adc_a(nh))*adc_x**adc_c(nh)
    ! "A value of As = 0.05 is used for a W/Ai = 0.0 ratio so that small amounts of snow
    ! don’t continue to exist well past the time when all the snow is gone in nature."
    ! - snow 17 manual 
    adc(1) = 0.05d0

    ! --- allocate variables (before simulating first area) ---
    if(nh .eq. 1) then
      ! get sim length to use in allocating variables (AWW new routine)
      call date_diff_ndays(start_year,start_month,start_day,end_year,end_month,end_day,sim_length)
     
      ! for non-daily timestep
      ! for simplicity don't consider partial days
      ! dt is defined in the namelist
      sim_length = sim_length * (86400d0/dt)

      !forcing variables
      allocate(year(sim_length))
      allocate(month(sim_length))
      allocate(day(sim_length))
      allocate(hour(sim_length))

      allocate(raw_precip(sim_length))  ! input values
      allocate(precip(sim_length))      ! values after applying pxadj
      allocate(raw_pet(sim_length))     ! input values
      allocate(pet(sim_length))         ! values after applying peadj
      allocate(tair(sim_length))
      allocate(psfall(sim_length)) 
  
      !sac-sma state variables
      allocate(uztwc_dp(sim_length))
      allocate(uzfwc_dp(sim_length))
      allocate(lztwc_dp(sim_length))
      allocate(lzfsc_dp(sim_length))
      allocate(lzfpc_dp(sim_length))
      allocate(adimc_dp(sim_length))

      !sac-sma output variables
      allocate(qg(sim_length))
      allocate(qs(sim_length))
      allocate(eta(sim_length))
      allocate(tci(sim_length))
  
      ! snow-17 output variables
      allocate(snowh(sim_length))
      allocate(sneqv(sim_length))
      allocate(snow(sim_length))
      allocate(raim(sim_length))
      allocate(cs_states(19,sim_length)) ! new for state write
      allocate(tprev_states(sim_length)) ! ditto

      ! UH routing variables
      allocate(expanded_tci(sim_length+uh_length-1))   ! routing state, includes spinup-period
      allocate(route_tci(sim_length+uh_length-1)) ! routing output, also includes spinup routing
      allocate(prior_tci(uh_length))              ! related (for state file read)

    end if  ! end of IF case for allocating only when running the first simulation area

    ! read forcing data
    call read_areal_forcing(year,month,day,hour,tair,raw_precip,raw_pet,psfall,hru_id(nh)) 

    ! apply PEADJ and PXADJ (scaling the input values)
    pet    = raw_pet * peadj(nh)
    precip = raw_precip * pxadj(nh)

    ! print run dates
    print*, '  start:',year(1),month(1),day(1),hour(1)
    print*, '    end:',year(sim_length),month(sim_length),day(sim_length), hour(sim_length)

    ! ================== RUN models for huc_area! ==========================================
  
    ! get sfc_pressure (pa is estimate by subroutine, needed by snow17 call)
    call sfc_pressure(elev(nh),pa)
  
    ! set single precision sac state variables to initial values
    if(warm_start_run .eq. 0) then
      ! we are not warm starting from a state file
      uztwc_sp = init_uztwc
      uzfwc_sp = init_uzfwc
      lztwc_sp = init_lztwc
      lzfsc_sp = init_lzfsc
      lzfpc_sp = init_lzfpc
      adimc_sp = init_adimc
      cs(1)    = init_swe   ! AWW: just initialize first/main component of SWE (model 'WE')
      cs(2:19) = 0          !      set the rest to zero
      tprev    = 0          ! AWW ADDED
      ! AWW: initialize prior part of routing tci state (a uh_length-1 long period) with zeros
      ! this gets used in writing states for runs shorter than uh_length
      expanded_tci(1:uh_length-1) = 0.0

    else if(warm_start_run .gt. 0) then
      ! we *ARE* warm starting from a state file
      ! read in external state files and overwrites namelist state variables 

      ! starting state files must match format of state file outputs (date then vars)
      !   state read routines look for state from one day before the start date of run
      call day_before_date(start_year,start_month,start_day,state_year,state_month,state_day)
      ! create string that will be matched in state file
      write(state_date_str,'(I0.4,I0.2,I0.2,I0.2)') state_year,state_month,state_day,hour(1)
  
      call read_snow17_state(state_date_str, cs, tprev, hru_id(nh))
      call read_sac_state(state_date_str, uztwc_sp, uzfwc_sp, lztwc_sp, lzfsc_sp, lzfpc_sp,&
                            adimc_sp, hru_id(nh))
      call read_uh_state(state_date_str, prior_tci, hru_id(nh))
      ! pre-pend TCI from prior simulation to expanded tci that will get routed
      expanded_tci(1:uh_length-1) = prior_tci(2:uh_length)  ! 1st value not used

    endif
  
    ! =============== START SIMULATION TIME LOOP =====================================
    do i = 1,sim_length,1

      ! adjust forcings based on monthly adjustment from file 
      if(use_forcing_adjust .eq. 1)then
        select case (month(i))
          case (1)
            tair(i) = tair(i) + mat_adj_jan(nh)
            precip(i) = precip(i) * map_adj_jan(nh)
            pet(i) = pet(i) * pet_adj_jan(nh)
            psfall(i) = min(psfall(i) * ptps_adj_jan(nh),1d0)
          case (2)
            tair(i) = tair(i) + mat_adj_feb(nh)
            precip(i) = precip(i) * map_adj_feb(nh)
            pet(i) = pet(i) * pet_adj_feb(nh)
            psfall(i) = min(psfall(i) * ptps_adj_feb(nh),1d0)
          case (3)
            tair(i) = tair(i) + mat_adj_mar(nh)
            precip(i) = precip(i) * map_adj_mar(nh)
            pet(i) = pet(i) * pet_adj_mar(nh)
            psfall(i) = min(psfall(i) * ptps_adj_mar(nh),1d0)
          case (4)
            tair(i) = tair(i) + mat_adj_apr(nh)
            precip(i) = precip(i) * map_adj_apr(nh)
            pet(i) = pet(i) * pet_adj_apr(nh)
            psfall(i) = min(psfall(i) * ptps_adj_apr(nh),1d0)
          case (5)
            tair(i) = tair(i) + mat_adj_may(nh)
            precip(i) = precip(i) * map_adj_may(nh)
            pet(i) = pet(i) * pet_adj_may(nh)
            psfall(i) = min(psfall(i) * ptps_adj_may(nh),1d0)
          case (6)
            tair(i) = tair(i) + mat_adj_jun(nh)
            precip(i) = precip(i) * map_adj_jun(nh)
            pet(i) = pet(i) * pet_adj_jun(nh)
            psfall(i) = min(psfall(i) * ptps_adj_jun(nh),1d0)
          case (7)
            tair(i) = tair(i) + mat_adj_jul(nh)
            precip(i) = precip(i) * map_adj_jul(nh)
            pet(i) = pet(i) * pet_adj_jul(nh)
            psfall(i) = min(psfall(i) * ptps_adj_jul(nh),1d0)
          case (8)
            tair(i) = tair(i) + mat_adj_aug(nh)
            precip(i) = precip(i) * map_adj_aug(nh)
            pet(i) = pet(i) * pet_adj_aug(nh)
            psfall(i) = min(psfall(i) * ptps_adj_aug(nh),1d0)
          case (9)
            tair(i) = tair(i) + mat_adj_sep(nh)
            precip(i) = precip(i) * map_adj_sep(nh)
            pet(i) = pet(i) * pet_adj_sep(nh)
            psfall(i) = min(psfall(i) * ptps_adj_sep(nh),1d0)
          case (10)
            tair(i) = tair(i) + mat_adj_oct(nh)
            precip(i) = precip(i) * map_adj_oct(nh)
            pet(i) = pet(i) * pet_adj_oct(nh)
            psfall(i) = min(psfall(i) * ptps_adj_oct(nh),1d0)
          case (11)
            tair(i) = tair(i) + mat_adj_nov(nh)
            precip(i) = precip(i) * map_adj_nov(nh)
            pet(i) = pet(i) * pet_adj_nov(nh)
            psfall(i) = min(psfall(i) * ptps_adj_nov(nh),1d0)
          case (12)
            tair(i) = tair(i) + mat_adj_dec(nh)
            precip(i) = precip(i) * map_adj_dec(nh)
            pet(i) = pet(i) * pet_adj_dec(nh)
            psfall(i) = min(psfall(i) * ptps_adj_dec(nh),1d0)
          case default
            error stop "Urecognised month for forcing adjustment"
        end select
      end if
  
      !set single precision inputs
      tair_sp   = real(tair(i),kind(sp))
      precip_sp = real(precip(i),kind(sp))
      pet_sp    = real(pet(i),kind(sp))
      psfall_sp = real(psfall(i),kind(sp))
   
      call exsnow19(int(dt),int(dt/sec_hour),day(i),month(i),year(i),&
  	!SNOW17 INPUT AND OUTPUT VARIABLES
  	  precip_sp,psfall_sp,tair_sp,raim(i),sneqv(i),snow(i),snowh(i),&
  	!SNOW17 PARAMETERS
          !ALAT,SCF,MFMAX,MFMIN,UADJ,SI,NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,ELEV,PA,ADC
  	  real(latitude(nh),kind(sp)),scf(nh),mfmax(nh),mfmin(nh),uadj(nh),si(nh),nmf(nh),&
               tipm(nh),mbase(nh),pxtemp(nh),plwhc(nh),daygm(nh),&
               real(elev(nh),kind(sp)),real(pa,kind(sp)),adc,&
  	!SNOW17 CARRYOVER VARIABLES
  			  cs,tprev) 
  
      call exsac(1,real(dt),raim(i),tair_sp,pet_sp,&
  	!SAC PARAMETERS
          !UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC, &
          !REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE, &
          !SIDE,RSERV, &
  		      uztwm(nh),uzfwm(nh),uzk(nh),pctim(nh),adimp(nh),riva(nh),zperc(nh), &
  		      rexp(nh),lztwm(nh),lzfsm(nh),lzfpm(nh),lzsk(nh),lzpk(nh),pfree(nh),&
  		      side(nh),rserv(nh), &
    	!SAC State variables
  			uztwc_sp,uzfwc_sp,lztwc_sp,lzfsc_sp,lzfpc_sp,adimc_sp,&
  	!SAC OUTPUTS
  			qs(i),qg(i),tci(i),eta(i))
    
      ! place state variables in output arrays
      uztwc_dp(i) = real(uztwc_sp,kind(dp))
      uzfwc_dp(i) = real(uzfwc_sp,kind(dp))
      lztwc_dp(i) = real(lztwc_sp,kind(dp))
      lzfsc_dp(i) = real(lzfsc_sp,kind(dp))
      lzfpc_dp(i) = real(lzfpc_sp,kind(dp))
      adimc_dp(i) = real(adimc_sp,kind(dp))

      ! store other single timestep run outputs for possible state write
      if(write_states > 0) then
        cs_states(:,i)  = cs
        tprev_states(i) = tprev
        ! print*,'tprev: ',i,day(i),month(i),year(i),tprev,cs(1)  AWW debugging
      end if  

    end do  
    ! ============ end simulation time loop ====================
  
    ! ============ now route full runoff timeseries using UH =========================

    ! add simulation tci to expanded tci timeseries that includes warm up is supplied
    expanded_tci(uh_length:sim_length+uh_length-1) = tci(1:sim_length)

    ! calculate some routing model parameters
    dtuh = real(dt/sec_day)
    if (unit_shape(nh) <= 0.0 .and. unit_scale(nh) <= 0.0) then
      k = 0
      m = 1
    else
      k = 1      ! ?
      m = 1000   ! max UH length (could be set elsewhere)
    end if
    ntau = 0  ! ?
  
    ! ==== if routing params are non-trivial, then call unit hydrograph routine
    if(routing_flag == 1) then
      ! call DUAMEL(tci,1,unit_hydro,unit_shape,unit_scale,dtuh,sim_length+uh_length,&
      !               m,route_tci,k,ntau)  ORIG

      call DUAMEL(expanded_tci, 1, unit_hydro, unit_shape(nh), unit_scale(nh), dtuh,&
                    sim_length+uh_length-1, m, route_tci, k, ntau, TOC_length) ! AWW          
                    ! should this be sim_length+uh_length*2-1?   AW

      ! now reset route_tci (output variable) to correct time period, ie: 
      !   trim off prior routing tci so that time index matches other output variables
      route_tci(1:sim_length) = route_tci(uh_length:sim_length+uh_length-1)  


      !print*, 'TOC_length', TOC_length, 'days'
      if(TOC_length > uh_length) then
        print*, ' '
        print*, 'WARNING:  TOC for simulation is greater than the routing UH_LENGTH' 
        print*, 'TOC_length =',TOC_length,' and uh_length =',uh_length
        print*, '    This is controlled by the dimension of unit_hydro'
        print*, '    It could cause flow initialization errors in a warm start run'
        print*, '    Check UH params to assess length of UH'
        print*, 'CONTINUING nonetheless...'
        print*, ' '
      end if 

    end if  ! end if BLOCK for whether to route at all

    ! ==== WRITE output for current area simulation ====

    if (output_hrus == 1 .and. calibration_mode == 0) then

      hru_output_filename = trim(output_root) // trim(hru_id(nh))

      open(unit=45,FILE=trim(hru_output_filename),FORM='formatted',status='replace')  ! output file open
  
      ! print header first
      write(45,'(A)') 'year mo dy hr tair pcp pcp*scf swe raim pet psfall &
      &eta uztwc uzfwc lztwc lzfsc lzfpc adimc sim_runoff sim_flow_mmd'
  
      31 FORMAT(I4.4, 3(1x,I2.2),8(F8.2),6(F8.2),3(F9.2),2(F10.3),F9.1)  ! AWW: goes with update
      32 FORMAT(I4.4, 3(1x,I2.2),8(F8.2),6(F8.2),2(F9.2),2(F10.3),F9.1)  ! AWW: one fewer field (routed tci)

      do i = 1,sim_length
        if (routing_flag == 1) then
          write(45,31) year(i),month(i),day(i),hour(i),tair(i),precip(i),precip(i)*scf(nh),&
                       sneqv(i)*1000.,raim(i),pet(i),psfall(i),eta(i),uztwc_dp(i),uzfwc_dp(i),&
                       lztwc_dp(i),lzfsc_dp(i),lzfpc_dp(i),adimc_dp(i),tci(i),route_tci(i)
        else
          ! AWW originally this was the same as above, but I think the routed flow needed removing
          !   need to fix this - make the order consistent
          write(45,32) year(i),month(i),day(i),hour(i),tair(i),precip(i),precip(i)*scf(nh),&
                       sneqv(i)*1000.,raim(i),pet(i),eta(i),uztwc_dp(i),uzfwc_dp(i),&
                       lztwc_dp(i),lzfsc_dp(i),lzfpc_dp(i),adimc_dp(i),tci(i)
        end if
      end do
      close(unit=45)
    end if  ! IF case for writing HRU-specific output to file (not including states)

    ! === write out STATE FILES for snow17, sac and uh if needed ===
    if(write_states > 0 .and. calibration_mode == 0) then
      call write_snow17_state(year,month,day,hour,cs_states,tprev_states,sim_length,hru_id(nh))
      call write_sac_state(year, month, day, hour, uztwc_dp, uzfwc_dp, lztwc_dp, &
                           lzfsc_dp, lzfpc_dp, adimc_dp, sim_length, hru_id(nh))
      ! call write_uh_state(year,month,day,hour,tci,sim_length,uh_length,hru_id(nh))
      call write_uh_state(year,month,day,hour,expanded_tci,sim_length,uh_length,hru_id(nh))
    end if

    ! ==== AWW store/add single simulation area timeseries ====
    if(nh == 1) then

      ! first area:  allocate combination output variables before first use (if any)
      allocate(tair_comb(sim_length))
      allocate(precip_comb(sim_length))
      allocate(precip_scf_comb(sim_length))
      allocate(sneqv_comb(sim_length))
      allocate(raim_comb(sim_length))
      allocate(pet_comb(sim_length))
      allocate(psfall_comb(sim_length))
      allocate(eta_comb(sim_length))
      allocate(uztwc_comb(sim_length))
      allocate(uzfwc_comb(sim_length))
      allocate(lztwc_comb(sim_length))
      allocate(lzfsc_comb(sim_length))
      allocate(lzfpc_comb(sim_length))
      allocate(adimc_comb(sim_length))
      allocate(tci_comb(sim_length))
      if(routing_flag == 1) then
        allocate(route_tci_comb(sim_length))
        allocate(route_tci_cfs(sim_length))
      end if

      ! store current area variable multiplied by area (in sqkm)
      tair_comb       = tair * hru_area(nh)   
      precip_comb     = precip * hru_area(nh)
      precip_scf_comb = precip * scf(nh) * hru_area(nh)
      sneqv_comb      = sneqv * hru_area(nh) 
      raim_comb       = raim * hru_area(nh)
      pet_comb        = pet * hru_area(nh)
      psfall_comb     = psfall * hru_area(nh)
      eta_comb        = eta * hru_area(nh)
      uztwc_comb      = uztwc_dp * hru_area(nh) 
      uzfwc_comb      = uzfwc_dp * hru_area(nh) 
      lztwc_comb      = lztwc_dp * hru_area(nh) 
      lzfsc_comb      = lzfsc_dp * hru_area(nh) 
      lzfpc_comb      = lzfpc_dp * hru_area(nh) 
      adimc_comb      = adimc_dp * hru_area(nh) 
      tci_comb        = tci * hru_area(nh)
      total_area      = hru_area(nh)  ! initialize on first area
      if(routing_flag == 1) then
        route_tci_comb  = route_tci(1:sim_length) * hru_area(nh)
      end if

    else       ! not first area, so add to summary variables already started

      !    AWW 'hru_area' from 'use def_namelists' statement (in sqkm)
      tair_comb       = tair_comb + tair * hru_area(nh)
      precip_comb     = precip_comb + precip * hru_area(nh)
      precip_scf_comb = precip_scf_comb + precip * scf(nh) * hru_area(nh)
      sneqv_comb      = sneqv_comb + sneqv * hru_area(nh) 
      raim_comb       = raim_comb + raim * hru_area(nh)
      pet_comb        = pet_comb + pet * hru_area(nh)
      psfall_comb     = psfall_comb + psfall * hru_area(nh)
      eta_comb        = eta_comb + eta * hru_area(nh)
      uztwc_comb      = uztwc_comb + uztwc_dp * hru_area(nh) 
      uzfwc_comb      = uzfwc_comb + uzfwc_dp * hru_area(nh) 
      lztwc_comb      = lztwc_comb + lztwc_dp * hru_area(nh) 
      lzfsc_comb      = lzfsc_comb + lzfsc_dp * hru_area(nh) 
      lzfpc_comb      = lzfpc_comb + lzfpc_dp * hru_area(nh) 
      adimc_comb      = adimc_comb + adimc_dp * hru_area(nh) 
      tci_comb        = tci_comb + tci * hru_area(nh)
      total_area      = total_area + hru_area(nh)
      if(routing_flag == 1) then
        route_tci_comb  = route_tci_comb + route_tci(1:sim_length) * hru_area(nh)
      end if 
    end if

  end do   ! ========== END of simulation areas loop   ====================

  ! ====== print combined simulation output ============
  print*, 'Sim_length (days) =',sim_length/(86400/dt)
  print*, '--------------------------------------------------------------'

  combined_output_filename = trim(output_root) // trim(main_id)
  print*, 'Combining outputs and writing ', trim(combined_output_filename)

  ! take weighted average of sum of HRU areas
  tair_comb       = tair_comb / total_area
  precip_comb     = precip_comb / total_area
  precip_scf_comb = precip_scf_comb / total_area
  sneqv_comb      = sneqv_comb / total_area
  raim_comb       = raim_comb / total_area
  pet_comb        = pet_comb / total_area
  psfall_comb     = psfall_comb / total_area
  eta_comb        = eta_comb / total_area
  uztwc_comb      = uztwc_comb / total_area
  uzfwc_comb      = uzfwc_comb / total_area
  lztwc_comb      = lztwc_comb / total_area
  lzfsc_comb      = lzfsc_comb / total_area
  lzfpc_comb      = lzfpc_comb / total_area
  adimc_comb      = adimc_comb / total_area
  tci_comb        = tci_comb / total_area
  if(routing_flag == 1) then
    route_tci_cfs   = route_tci_comb * 1000 * 3.28084**3 / dt  ! mm/timestep*total_area to cfs
    route_tci_comb  = route_tci_comb / total_area  ! mm/d
  end if

  ! -- write out combined file that is similar to each area file, but add flow variable in CFS units

  ! combined output file open
  open(unit=125,FILE=trim(combined_output_filename),FORM='formatted',status='replace')
  
  ! print header & data (diffence if there's routing or not)
  33 FORMAT(I4.4, 3(1x,I2.2),3(F12.2),F12.2,F12.2,3(F12.2),6(F12.2),2(F12.3),F12.2) ! AWW: 1 more field (cfs) than orig
  34 FORMAT(I4.4, 3(1x,I2.2),3(F12.2),F12.2,F12.2,3(F12.2),4(F12.2),2(F12.3),F12.1)  ! AWW: two fewer fields (not routed)
  35 FORMAT(I4.4, 3(1x,I2.2), F20.2)

  if(routing_flag == 1) then

    if(calibration_mode == 0)then
      write(125,'(A)') 'year mo dy hr tair pcp pcp*scf swe raim pet psfall &
      &eta uztwc uzfwc lztwc lzfsc lzfpc adimc sim_runoff sim_flow_mmd sim_flow_cfs'
      do i = 1,sim_length
        write(125,33) year(i),month(i),day(i),hour(i),tair_comb(i),precip_comb(i),precip_scf_comb(i),&
                         sneqv_comb(i)*1000.,raim_comb(i),pet_comb(i),psfall_comb(i),eta_comb(i),uztwc_comb(i),uzfwc_comb(i),&
                         lztwc_comb(i),lzfsc_comb(i),lzfpc_comb(i),adimc_comb(i),&
                         tci_comb(i),route_tci_comb(i),route_tci_cfs(i)
      end do
    else
      write(125,'(A)') 'year mo dy hr sim_flow_cfs'
        do i = 1,sim_length
          write(125,35) year(i),month(i),day(i),hour(i),route_tci_cfs(i)
      end do
    endif 

  else  
    ! doesn't have routing (header and data have fewer fields)
    if(calibration_mode == 0)then
      write(125,'(A)') 'year mo dy hr tair pcp pcp*scf swe raim pet psfall &
      &eta uztwc uzfwc lztwc lzfsc lzfpc adimc sim_runoff'
      do i = 1,sim_length
        write(125,34) year(i),month(i),day(i),hour(i),tair_comb(i),precip_comb(i),precip_scf_comb(i),&
                         sneqv_comb(i)*1000.,raim_comb(i),pet_comb(i),psfall_comb(i),eta_comb(i),uztwc_comb(i),uzfwc_comb(i),&
                         lztwc_comb(i),lzfsc_comb(i),lzfpc_comb(i),adimc_comb(i),tci_comb(i)
      end do
    else 
      write(125,'(A)') 'year mo dy hr sim_runoff'
      do i = 1,sim_length
        write(125,35) year(i),month(i),day(i),hour(i),tci_comb(i)
      end do
    end if 
  endif  

  close(unit=125)

  print*, 'DONE!'
  print*, '--------------------------------------------------------------'

end program
