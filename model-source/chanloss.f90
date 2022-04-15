subroutine chanloss(n_clmods, dt, sim_length, year, month, day, &
    factor, period, &
    sim, sim_adj)

! !     Subroutine Description
! !     -----------------------------------
! !     The Chanloss subroutine uses a modular approach for adjustment.  Each chanloss module has two primary parameters: 
! !     "factor" and "period".  The "factor" parameter is the adjustment to be applied to the simulation and the "period"
! !     parameter is an 2d array which specifies the beginning and ending span of month(s) to apply the factor  The 
! !     adjustment "factors" and "periods" for all of the chanloss modules are combined to form a single monthly adjustment
! !     table.  This adjustment table is applied to the flow sim input argument.  The output is the adjusted simulation.

! !     NOTE
! !     -----------------------------------
! !     - The first value in the "period" argument array is the beginning of the period and the second is the end of the 
! !       period.  If the first value is greater than the second (EX:  [11,2]), the period will cross a year (EX CONT: 
! !       factor will be applied to Nov, Dec, Jan, Feb)
! !     -If more than one Chanloss modules periods share months, the average factor between the modules are used for those
! !      months.
! !     -Adjustment factors are applied to the 16th+delta_ts of every month, and an adjustment factor for a particular day is
! !      a linear interpolation between the nearest neighboring months. The adjustment factor is the same for all timesteps
! !      in a day.

! !     Arguments
! !     -----------------------------------
! !     INPUTS
! !     n_clmods:  Number of Chanloss modules (integer)
! !     dt:  Time step of the simulation (integer)
! !     sim_length:  The number of time steps of the simulation (integer)
! !     year:  The year associated with each time step (integer array)
! !     month:  The month associated with each time step (integer array)
! !     day:  The day associatede with each time step (integer array)
! !     hour:  The hour associated with each time step (integer array)
! !     factor:  The adjustment factor for each module (double array)
! !     period:  The beginning and ending month that the factor is applied for each module (integer array)
! !     sim:  Simulated streamflow (double array)
! !     OUTPUTS
! !     sim_ad j:  Simulated streamflow adjusted by the factors/periods of all the Chanloss modules (double array)
! !        1         2         3         4         5         6         7

  ! ! Inputs
  integer, intent(in):: n_clmods, dt, sim_length
  integer, dimension(sim_length), intent(in):: year, month, day
  double precision, dimension(n_clmods), intent(in):: factor
  integer, dimension(2,n_clmods), intent(in):: period
  double precision, dimension(sim_length), intent(in):: sim

  ! ! Local varible
  integer:: dt_hours, n, i, j, mo, first_period, second_period, num_fa
  double precision::  sum_fa, interp_day, decimal_day,cl_adj_step, dayn, dayi
  integer, dimension(12):: mdays, mdays_prev
  double precision, dimension(12):: cl_adj_m,cl_adj_m_prev,cl_adj_m_next
  integer, dimension(12,n_clmods):: cl_adj_lookup

  ! ! Output 
  double precision, dimension(sim_length), intent(out):: sim_adj

  ! ! Lookup Tables
  mdays =      (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /) 
  mdays_prev = (/ 31, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 /) 
  
  dt_hours = dt/3600

  ! do i=1,2
  !   write(*,'(100i3)') period(i,:)
  ! end do

  !! 1) Create monthly adjustment table
  
  !! 1a) Create adjustment table lookup
  
  !! Initially set all value in lookup table to 0
  cl_adj_lookup=0

  !! Loop through each chanloss module and switch the appropriate row in the lookup table from 0 to 1 to designate
  !! it as a month to apply that module's factor
  do n = 1, n_clmods
    
    !! Grab the beginning and ending month for the period 
    !! If either of those parameter values are outside a value of 1-12 
    !! Set it to the closest value
    first_period=int(period(1,n))
    second_period=int(period(2,n))
    !! Checks
    if(first_period < 1) first_period = 1
    if(first_period > 12) first_period = 12
    if(second_period < 1) second_period = 1
    if(second_period > 12) second_period = 12
    
    !! If first_period>second_period then have the adjustment span across a year
    if (first_period<=second_period) then
      cl_adj_lookup(first_period:second_period,n)=1
    else
     cl_adj_lookup(first_period:12,n)=1
     cl_adj_lookup(1:second_period,n)=1
    end if
  end do

  ! do i=1,n_clmods
  !   write(*,'(12i2)') cl_adj_lookup(:,i)
  ! end do
  
  !! 1b) Create adjustment tableI
  cl_adj_m=1
  
  !! This loop ensures that for modules which share months, the average factor is used amongst the modules
  !! Loop through each month
  do j = 1, 12
    num_fa=0
    sum_fa=0
    !!Loop through each module
    do n = 1, n_clmods
      !! Check if adjustment factor is applied to the current month
      if (cl_adj_lookup(j,n)==1) then
        num_fa=num_fa+1
        sum_fa=sum_fa+factor(n)
      end if
    end do
    !! If the month has module(s) being applied, take the average of all of the factors
    if (num_fa>=1) then
      cl_adj_m(j)=sum_fa/dble(num_fa)
    end if
  end do    

  ! write(*,'(12f6.3)') cl_adj_m 

  !! 2) Use monthly adjustment table to interpolate the adjustment factor for each time step

  !! Adjustment arrays for interpolation
  cl_adj_m_prev(1) = cl_adj_m(12)
  cl_adj_m_prev(2:12) = cl_adj_m(1:11)
  cl_adj_m_next(12) = cl_adj_m(1)
  cl_adj_m_next(1:11) = cl_adj_m(2:12)

  !! Loop through each time step in the simulation
  do i = 1,sim_length,1

    !! 1) Determine the adjustement factor for the time step
    !! Adjust days in February if the year is a leap year
    if(mod(year(i),100) .ne. 0 .and. mod(year(i),4) .eq. 0) then
      mdays(2) = 29 ! leap year
    else if(mod(year(i),400).eq.0) then
      mdays(2) = 29 ! leap year
    else
      mdays(2) = 28 ! not leap year
    endif

    !! Decimal day to start interpolation.  The monthly chaloss adjustments are applied to the 16th + time step (days)
    interp_day = 16. + dble(dt_hours)/24
    ! Current decimal day
    decimal_day = dble(day(i)) + dble(dt_hours)/24.
    mo = month(i)

    !! Interpolate the adjustment factor using neighboring months
    !! the equation for interpolation depends on if the time step is before or after the 16th
    if(decimal_day >= interp_day)then
      dayn = dble(mdays(mo))
      dayi = decimal_day - interp_day 
      cl_adj_step = cl_adj_m(mo) + dayi/dayn*(cl_adj_m_next(mo)-cl_adj_m(mo))
    else 
      dayn = dble(mdays_prev(mo))
      dayi = decimal_day - interp_day + mdays_prev(mo) 
      cl_adj_step = cl_adj_m_prev(mo) + dayi/dayn*(cl_adj_m(mo)-cl_adj_m_prev(mo))
    end if 
    
    !! 2b) Apply the adjustment factor
    sim_adj(i)=sim(i)*cl_adj_step
    
  end do
end subroutine