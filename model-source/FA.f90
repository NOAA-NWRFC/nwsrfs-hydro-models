subroutine  fa_ts(n_hrus, dt, sim_length, year, month, day, hour, &
    latitude, area, &
    peadj_m, &
    map_fa_pars, mat_fa_pars, pet_fa_pars, ptps_fa_pars, & 
    map_fa_limits_in, mat_fa_limits_in, pet_fa_limits_in, ptps_fa_limits_in, & 
    climo, & 
    map, ptps, mat, &
    map_adj, mat_adj, pet_adj, ptps_adj, &
    map_fa, mat_fa, ptps_fa, pet_fa, etd)
  
  !! Calculates a forcing adjusted timeseries for MAP, MAT, PTPS, and PET
  
  use utilities
  implicit none

  ! this code is currently not set up to do any timestep less than 1 hour, 
  ! nor could it do fractional hour timesteps.
  
  ! ! PARAMETERS ! !
  double precision, parameter:: pi=3.141592653589793238462643383279502884197d0
  double precision, parameter:: sec_hour = 3600.     !seconds in an hour

  ! ! INPUTS ARGUMENTS ! !
  integer, intent(in):: n_hrus ! number of zones
  integer, intent(in):: dt    ! model timestep in seconds
  integer, intent(in):: sim_length   ! length of simulation for dt
  integer, dimension(sim_length), intent(in):: year, month, day, hour !TS of date component
  double precision, dimension(sim_length, n_hrus), intent(in):: map, ptps, mat  ! atmospheric forcing variables
  double precision, dimension(n_hrus), intent(in):: latitude   ! PET param, decimal degrees
  double precision, dimension(n_hrus), intent(in):: area       ! km2
  ! forcing adjustment parameter vectors: mult, p_redist, std, shift 
  double precision, dimension(4), intent(in):: map_fa_pars, mat_fa_pars, pet_fa_pars, ptps_fa_pars
  ! climo fa limits, static set by external data 
  double precision, dimension(12,2), intent(in):: map_fa_limits_in, mat_fa_limits_in, pet_fa_limits_in, ptps_fa_limits_in
  ! static pet adjustment
  double precision, dimension(12, n_hrus), intent(in):: peadj_m

  ! ! LOCAL VARIBLES ! !
  logical:: calc_climo
  integer:: nh,i,k           ! AWW index for looping through areas
  integer:: dt_hours, ts_per_day, ts_in_day    ! model timestep in hours and number of timesteps per day
  ! area weighted forcings for climo calculations
  double precision, dimension(sim_length):: map_aw, ptps_aw, mat_aw, pet_aw
  ! climo fa limits, static set by external data 
  double precision, dimension(12,2):: map_fa_limits, mat_fa_limits, pet_fa_limits, ptps_fa_limits
  double precision:: dr, rho, omega_s, Ra ! pet calculation varible
  integer, dimension(sim_length):: jday ! julian day for pet calculations
  double precision:: pet_ts, tmax_daily, tmin_daily, tave_daily ! pet for a time step and required temperature varible
  double precision, dimension(sim_length, n_hrus):: pet_hs ! pet unadjusted by pet_adj
  double precision:: map_step, ptps_step, mat_step, pet_step !adj forcing for current ts in loop
  double precision:: mat_adj_step, map_adj_step, pet_adj_step, ptps_adj_step, peadj_step !adj for current ts in loop
  double precision, dimension(12):: map_climo, mat_climo, pet_climo, ptps_climo !monthly climo
  integer, dimension(12) :: mdays, mdays_prev !lookup tables with number of days in month
  integer:: mo  !current month for ts 
  double precision :: dayn, dayi,interp_day, decimal_day !used to interpolate fa per ts
  ! arrays to assist in interpolating fa to apply
  double precision, dimension(12):: mat_adj_prev, mat_adj_next
  double precision, dimension(12):: map_adj_prev, map_adj_next
  double precision, dimension(12):: pet_adj_prev, pet_adj_next
  double precision, dimension(12):: ptps_adj_prev, ptps_adj_next
  double precision, dimension(12,n_hrus):: peadj_m_prev, peadj_m_next

  ! ! INOUT ARGUMENTS ! !  
  double precision, dimension(12, 4), intent(inout):: climo ! 4 columns, map, mat, pet, ptps

  ! ! OUTPUT ARGUMENTS ! !
  ! monthly forcing adjustment parameters for map, mat, pet, ptps will be computed
  double precision, dimension(12), intent(out):: map_adj, mat_adj, pet_adj, ptps_adj
  ! forcing adjusted timeseries
  double precision, dimension(sim_length, n_hrus), intent(out):: map_fa, mat_fa, ptps_fa, pet_fa, etd
  ! plane 1: map_fa, plane 2: mat_fa, plane 3: ptps_fa, plane 4:  pet_fa
  !double precision, dimension(4,sim_length, n_hrus):: fa_ts

  mdays =      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) 
  mdays_prev = (/ 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 /) 

  map_fa = 0 
  mat_fa = 0 
  ptps_fa = 0 
  pet_fa = 0
  etd = 0

  ts_per_day = 86400/dt
  dt_hours = dt/3600
  ! julian day 
  jday = julian_day(year,month,day)

  ! set local forcing limits
  map_fa_limits = map_fa_limits_in
  mat_fa_limits = mat_fa_limits_in
  pet_fa_limits = pet_fa_limits_in
  ptps_fa_limits = ptps_fa_limits_in

  ! check if real climo data is input 
  if(climo(1,1) <= -9999d0)then 
    calc_climo = .true.
  else 
    calc_climo = .false.
  end if 
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Compute the monthly MAT FA
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  mat_fa = -99
  mat_aw = 0 

  ! area weighted forcings 
  do nh=1,n_hrus
    mat_aw = mat_aw + mat(:,nh) * area(nh)
  end do 

  mat_aw = mat_aw / sum(area)

  if(calc_climo)then
    ! compute 12 monthly climo values (calendar year)
    mat_climo = monthly_climo_mean(mat_aw, month)
    climo(:,2) = mat_climo
  else
    do i = 1,12
      mat_climo(i) = climo(i,2)
    end do
  end if 
  
  ! expand forcing adjustment limits in case the limits are not set properly 
  do k=1,12
    if(mat_fa_limits(k,1) == -999. .and. mat_climo(k) >= 0.) mat_fa_limits(k,1) = mat_climo(k) * 0.9
    if(mat_fa_limits(k,1) == -999. .and. mat_climo(k) <  0.) mat_fa_limits(k,1) = mat_climo(k) * 1.1
    if(mat_fa_limits(k,2) == -999. .and. mat_climo(k) >= 0.) mat_fa_limits(k,2) = mat_climo(k) * 1.1
    if(mat_fa_limits(k,2) == -999. .and. mat_climo(k) <  0.) mat_fa_limits(k,2) = mat_climo(k) * 0.9
  end do 

  ! write(*,*)'FA pars: ', mat_fa_pars
  ! write(*,*)'mat: lower, climo, upper'
  ! do k=1,12
  !   write(*,*)mat_fa_limits(k,1), mat_climo(k), mat_fa_limits(k,2)
  ! end do 

  ! compute monthly adjustments using GW's method
  mat_adj = forcing_adjust_mat(mat_climo, mat_fa_pars, mat_fa_limits(:,1), mat_fa_limits(:,2))

  ! write(*,*)'mat_adj'
  ! do i = 1,12
  !   write(*,*)mat_adj(i)
  ! end do

  ! put the forcing adjustments in easy to use vectors
  mat_adj_prev(1) = mat_adj(12)
  mat_adj_prev(2:12) = mat_adj(1:11)
  mat_adj_next(12) = mat_adj(1)
  mat_adj_next(1:11) = mat_adj(2:12)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Compute the forcing adjusted MAT for entire POR
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

      ! decimal day to start interpolation
      interp_day = 16. + dble(dt_hours)/24
      ! current decimal day
      decimal_day = dble(day(i)) + dble(dt_hours)/24.

      mo = month(i)
      if(decimal_day >= interp_day)then
        dayn = dble(mdays(mo))
        dayi = decimal_day - interp_day 
        mat_adj_step = mat_adj(mo) + dayi/dayn*(mat_adj_next(mo)-mat_adj(mo))
      else 
        dayn = dble(mdays_prev(mo))
        dayi = decimal_day - interp_day + mdays_prev(mo) 
        mat_adj_step = mat_adj_prev(mo) + dayi/dayn*(mat_adj(mo)-mat_adj_prev(mo))
      end if 
      mat_fa(i,nh) = mat(i,nh) + mat_adj_step
    end do  ! ============ end simulation time loop ====================
  end do   ! ========== END of simulation areas loop   ====================


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Compute unadjusted PET for the entire POR 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  pet_hs = 0d0
  pet_ts = 0d0
  
  do nh = 1, n_hrus
    do i = 1, sim_length
      ! on the first timestep of the day (or for the whole day if the ts is daily), 
      ! compute the pet for the whole day, then divide it by the number of timesteps
      if((hour(i) .eq. 0) .or. (ts_per_day .eq. 1))then

        ts_in_day = min(ts_per_day,sim_length-i+1)
        ! if(i .eq. 1)write(*,*)'mat',mat(i:(i+ts_per_day-1),nh)
        ! if(i .eq. 1)write(*,*) 'timestep', i, 'of', sim_length, year(i), month(i), day(i), hour(i), jday(i)
        tmax_daily = maxval(mat_fa(i:(i+ts_in_day-1),nh))
        tmin_daily = minval(mat_fa(i:(i+ts_in_day-1),nh))
        tave_daily = sum(mat_fa(i:(i+ts_in_day-1),nh))/dble(ts_in_day)

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

        ! daily pet from Hargreaves-Semani equation, units are mm/day, so divide 
        ! by number of timesteps in a day   
        pet_ts = 0.0023 * (tave_daily + 17.8) * (tmax_daily - tmin_daily)**0.5 * Ra / 2.45
        ! if(i .eq. 1)write(*,*)tmax_daily, tmin_daily, tave_daily, pet_ts

        ! ignore negative values
        if(pet_ts < 0) pet_ts = 0

        ! if(isnan(pet_ts))then
        !   write(*,*)i, nh, jday(i), ts_per_day, ts_in_day
        !   write(*,*)'lat',latitude(nh)
        !   write(*,*)'dr',dr
        !   write(*,*)'rho',rho
        !   write(*,*)'omega_s',omega_s
        !   write(*,*)'Ra',Ra
        !   write(*,*)'mat',mat_fa(i:(i+ts_per_day-1),nh)
        !   write(*,*)'mat',mat_fa(1:4,nh)
        !   write(*,*)'tave_daily',tave_daily
        !   write(*,*)'tmax_daily',tmax_daily
        !   write(*,*)'tmin_daily',tmin_daily
        ! end if

        ! apply global scaling peadj and distibute across timesteps 
        pet_ts = pet_ts / dble(ts_per_day)

      end if 
      pet_hs(i,nh) = pet_ts 
    end do  ! ============ end simulation time loop ====================
  end do   ! ========== END of simulation areas loop   ====================
  
  ! write(*,*)'pet'
  ! do k=1,10
  !   write(*,*)pet(k,:)
  ! end do 

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Compute the monthly FA for MAP, PTPS, and PET
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  map_aw = 0 
  ptps_aw = 0 
  pet_aw = 0

  ! area weighted forcings 
  do nh=1,n_hrus
    map_aw = map_aw + map(:,nh) * area(nh)
    pet_aw = pet_aw + pet_hs(:,nh) * area(nh)
    ptps_aw = ptps_aw + ptps(:,nh) * area(nh)
  end do 

  map_aw = map_aw / sum(area)
  pet_aw = pet_aw / sum(area)
  ptps_aw = ptps_aw / sum(area)

  ! do i = 1,sim_length
  !   if(isnan(pet_aw(i)))write(*,*)'pet is nan at step:',i
  ! end do

  if(calc_climo)then
    ! compute 12 monthly climo values (calendar year)
    map_climo = monthly_climo_sum(map_aw, month)
    pet_climo = monthly_climo_sum(pet_aw, month)
    ptps_climo = monthly_climo_mean(ptps_aw, month)
    climo(:,1) = map_climo
    climo(:,3) = pet_climo
    climo(:,4) = ptps_climo
  else 
    do i = 1,12
      map_climo(i) = climo(i,1)
      pet_climo(i) = climo(i,3)
      ptps_climo(i) = climo(i,4)
    end do
  end if 

  ! write(*,*)'climo: map, mat, pet, ptps'
  ! do k=1,12
  !   write(*,"(4f8.2)")map_climo(k), mat_climo(k), pet_climo(k), ptps_climo(k)
  ! end do 
  ! write(*,*)'climo: lower, map, upper'
  ! do k=1,12
  !   write(*,*)map_fa_limits(k,1), map_climo(k), map_fa_limits(k,2)
  ! end do 
  ! return

  ! expand forcing adjustment limits in case the limits are not set properly 
  do k=1,12
    if(map_fa_limits(k,1) == -999.) map_fa_limits(k,1) = map_climo(k) * 0.9
    if(map_fa_limits(k,2) == -999.) map_fa_limits(k,2) = map_climo(k) * 1.1
    if(pet_fa_limits(k,1) == -999.) pet_fa_limits(k,1) = pet_climo(k) * 0.9
    if(pet_fa_limits(k,2) == -999.) pet_fa_limits(k,2) = pet_climo(k) * 1.1
    if(ptps_fa_limits(k,1) == -999.) ptps_fa_limits(k,1) = ptps_climo(k) * 0.75
    if(ptps_fa_limits(k,2) == -999.) ptps_fa_limits(k,2) = ptps_climo(k) * 1.25
  end do 

  ! compute monthly adjustments using GW's method
  map_adj = forcing_adjust_map_pet_ptps(map_climo, map_fa_pars, map_fa_limits(:,1), map_fa_limits(:,2))
  pet_adj = forcing_adjust_map_pet_ptps(pet_climo, pet_fa_pars, pet_fa_limits(:,1), pet_fa_limits(:,2))
  ptps_adj = forcing_adjust_map_pet_ptps(ptps_climo, ptps_fa_pars, ptps_fa_limits(:,1), ptps_fa_limits(:,2))
  ! write(*,*)'FA pars: ', map_fa_pars
  ! write(*,*)'adjments: map, mat, pet, ptps'
  ! do k=1,12
  !  write(*,"(4f8.2)")map_adj(k),mat_adj(k),pet_adj(k),ptps_adj(k)
  ! end do 

  ! write(*,*)'PET FA pars: ', pet_fa_pars
  ! do k=1,12
  !  write(*,*)pet_climo(k),pet_fa_limits(k,1), pet_fa_limits(k,2)
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

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! Compute forcing adjusted MAP, PTPS, and PET for entire POR
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  do nh=1,n_hrus
    ! print*, 'Running area',nh,'out of',n_hrus

    ! there are monthly crop coefficients per zone 
    peadj_m_prev(1,nh) = peadj_m(12,nh)
    peadj_m_prev(2:12,nh) = peadj_m(1:11,nh)
    peadj_m_next(12,nh) = peadj_m(1,nh)
    peadj_m_next(1:11,nh) = peadj_m(2:12,nh)

    ! print run dates
    ! write(*,*)'  start:',year(1), month(1), day(1), hour(1)
    ! write(*,*)'    end:',year(sim_length), month(sim_length), day(sim_length), hour(sim_length)

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

      ! decimal day to start interpolation
      interp_day = 16. + dble(dt_hours)/24
      ! current decimal day
      decimal_day = dble(day(i)) + dble(dt_hours)/24.
      mo = month(i)
      !write(*,*)mdays, mdays(mo), day(i), hour(i)
      if(decimal_day >= interp_day)then
        dayn = dble(mdays(mo))
        dayi = decimal_day - interp_day 
        map_adj_step = map_adj(mo) + dayi/dayn*(map_adj_next(mo)-map_adj(mo))
        pet_adj_step = pet_adj(mo) + dayi/dayn*(pet_adj_next(mo)-pet_adj(mo))
        ptps_adj_step = ptps_adj(mo) + dayi/dayn*(ptps_adj_next(mo)-ptps_adj(mo))
        peadj_step = peadj_m(mo,nh) + dayi/dayn*(peadj_m_next(mo,nh)-peadj_m(mo,nh))
      else 
        dayn = dble(mdays_prev(mo))
        dayi = decimal_day - interp_day + mdays_prev(mo) 
        map_adj_step = map_adj_prev(mo) + dayi/dayn*(map_adj(mo)-map_adj_prev(mo))
        pet_adj_step = pet_adj_prev(mo) + dayi/dayn*(pet_adj(mo)-pet_adj_prev(mo))
        ptps_adj_step = ptps_adj_prev(mo) + dayi/dayn*(ptps_adj(mo)-ptps_adj_prev(mo))
        peadj_step = peadj_m_prev(mo,nh) + dayi/dayn*(peadj_m(mo,nh)-peadj_m_prev(mo,nh))
      end if 
      ! if(isnan(pet_adj_step) .and. i .eq. 1) then 
      !   write(*,'(2i3,5f10.3)')nh, mo, pet_adj_prev(mo), dayi, dayn, pet_adj(mo), pet_adj_prev(mo)
      !   write(*,*)'adjustments: map, mat, pet, ptps'
      !   do k=1,12
      !    write(*,"(4f8.2)")map_adj(k),mat_adj(k),pet_adj(k),ptps_adj(k)
      !   end do 

      !   write(*,*)'PET FA pars: ', pet_fa_pars
      !   do k=1,12
      !    write(*,*)pet_climo(k),pet_fa_limits(k,1), pet_fa_limits(k,2)
      !   end do 
      ! end if 


      ! if(i < 6)then
      !   write(*,'(a,6i5,8f8.2)')'before adj',nh, i, year(i), month(i), day(i), hour(i), map(i,nh), & 
      !                                    mat(i,nh), ptps(i,nh), pet(i,nh), &
      !                                    map_adj_step, mat_adj_step, ptps_adj_step, pet_adj_step
      ! end if

      mat_step = mat_fa(i,nh)
      ! apply PXADJ (scaling the input values)
      map_step = map(i,nh) * map_adj_step
      if(ptps(i,nh) < 1d0)then
        ptps_step = min(ptps(i,nh) * ptps_adj_step, 1d0)
      else
        ptps_step = ptps(i,nh)
      end if 
      ! pet_hs(i,nh) is the pet from HS, 
      ! peadj_step is the conversion to etdemand (crop factor)
      ! pet_adj_step is the forcing adjustment
      pet_step = pet_hs(i,nh) * pet_adj_step
      ! if(i < 2)then
      !   write(*,'(a,6i5,8f8.2)')' after adj',nh, i, year(i), month(i), day(i), hour(i), & 
      !                                    map_step, mat_step, ptps_step, pet_step, &
      !                                    map_adj_step, mat_adj_step, ptps_adj_step, pet_adj_step
      !   write(*,*)'pet: ',pet_step, pet_hs(i,nh), peadj_step,  pet_adj_step
      ! end if
      
      ! assign map_fa, pet_fa, mat_fa
      map_fa(i,nh) = map_step
      ptps_fa(i,nh) = ptps_step
      pet_fa(i,nh) = pet_step
      etd(i,nh) = pet_step * peadj_step

      
    end do  ! ============ end simulation time loop ====================
  end do   ! ========== END of simulation areas loop   ====================

end subroutine