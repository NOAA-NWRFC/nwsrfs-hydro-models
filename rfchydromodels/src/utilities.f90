module utilities 

  use stats 
  use sorting 

  implicit none

  contains

  function forcing_adjust_map_pet_ptps(climo, pars, ll, ul)
      
      implicit none

      double precision, dimension(12), intent(in):: climo, ul, ll
      double precision, dimension(4), intent(in):: pars
      double precision, dimension(12):: forcing_adjust_map_pet_ptps 
      double precision:: scale, p_redist, std, shift
      double precision, dimension(12):: climo_adj, climo_remaining, climo_redist, w
      integer:: i
      integer, dimension(12):: climo_order 
      double precision :: m(12) = (/(i, i=1,12, 1)/)
      double precision, dimension(14):: climo_interp

      scale = pars(1)
      p_redist = pars(2)
      std = pars(3)
      shift = pars(4) ! days 

      ! normal weights 
      w = norm_pdf(m, 1d0, std)
      w = reverse(w/sum(w))

      ! apply first scaling parameter
      climo_adj = climo * scale
      ! get the indexes of the sorted values to re-sort later 
      climo_order = argsort(argsort(climo_adj))
      ! sort climo in ascending order to apply weights
      call sort(climo_adj)
      !do i=1,12
      !  write(*,*)climo_adj(i)
      !end do
      !write(*,*)
      ! percent of each month remaining before redistributing 
      climo_remaining = climo_adj * (1.0 - p_redist)
      ! redistribute according to weights 
      climo_redist = climo_remaining + sum(climo_adj * p_redist) * w
      ! re-sort to original order 
      climo_adj = climo_redist(climo_order)


      ! write(*,*)'Before shift'
      ! do i=1,12
      !   write(*,*)climo_adj(i)
      ! end do

      if(shift .ne. 0 ) then
        ! apply shift 
        climo_interp(1) = climo_adj(12)
        climo_interp(2:13) = climo_adj
        climo_interp(14) = climo_adj(1)

        ! interpolate between (x0,y0) and (x1,y1)
        ! y = y0 + (x-x0)*(y1-y0)/(x1-x0)
        ! interpolate between (month0,climo0) and (month1,climo1)
        ! y = climo0 + ((month0+shift)-month0)*(climo1-climo0)/(month1-month0)
        ! could get fancier here and account for the number of days in each month, but 
        ! 30 day months should be a reasonable approximation 
        do i=1,12
          if(shift > 0 ) then
            climo_adj(i) = climo_interp(i+1) + shift*(climo_interp(i+2)-climo_interp(i+1))/30
          else if (shift < 0 ) then
            climo_adj(i) = climo_interp(i) + (30-abs(shift))*(climo_interp(i+1)-climo_interp(i))/30
          end if 
        end do 
      end if 

      ! write(*,*)'After shift'
      ! do i=1,12
      !   write(*,*)climo_adj(i)
      ! end do
      

      ! write(*,*)'Limits'
      ! do i=1,12
      !   write(*,*)ll(i),ul(i)
      ! end do

      ! enforce limits 
      do i=1,12
        if(climo_adj(i) > ul(i)) climo_adj(i) = ul(i)
        if(climo_adj(i) < ll(i)) climo_adj(i) = ll(i)
        if(abs(climo(i)) < 0.1d-5)then
          forcing_adjust_map_pet_ptps(i) = 1
        else
          forcing_adjust_map_pet_ptps(i) = climo_adj(i)/climo(i)
        end if
      end do 

      ! write(*,*)'After limits'
      ! do i=1,12
      !   write(*,*)climo_adj(i)
      ! end do

      ! write(*,*)'Climo'
      ! do i=1,12
      !   write(*,*)climo(i)
      ! end do

      return 
  end function

  function forcing_adjust_mat(climo, pars, ll, ul)

      implicit none

      double precision, dimension(12), intent(in):: climo, ul, ll
      double precision, dimension(4), intent(in):: pars
      double precision, dimension(12):: forcing_adjust_mat 
      double precision:: scale, p_redist, std, shift
      double precision, dimension(12):: climo_adj, climo_dev, climo_dist, climo_remaining, &
                                        climo_redist, w
      integer:: i
      integer, dimension(12):: climo_order 
      double precision :: m(6) = (/(i, i=1,6, 1)/)
      double precision, dimension(14):: climo_interp
      double precision:: med

      scale = pars(1)
      p_redist = pars(2)
      std = pars(3)
      shift = pars(4) ! days 

      ! normal weights
      w(1:6) = norm_pdf(m, 1d0, std)
      w(7:12) = reverse(w(1:6))
      w = w/sum(w)*2
      ! write(*,*)'w'
      ! do i=1,12
      !   write(*,*)w(i)
      ! end do

      ! apply first scaling parameter
      climo_adj = climo * scale
      med = median(climo_adj)
      ! write(*,*)'Median: ', med
      ! get the indexes of the sorted values to re-sort later 
      climo_order = argsort(argsort(climo_adj))
      ! sort climo in ascending order to apply weights
      call sort(climo_adj)
      ! compute deviation from median 
      climo_dev = climo_adj - med

      ! write(*,*)'climo_dev'
      ! do i=1,12
      !   write(*,*)climo_dev(i)
      ! end do

      ! percent of each month remaining before redistributing 
      climo_remaining = climo_dev * (1.0 - p_redist)
      ! get the total temperature to above the median and below median
      ! write(*,*)climo_dev(1:6), p_redist
      climo_dist(1:6) = sum(climo_dev(1:6) * p_redist)
      climo_dist(7:12) = sum(climo_dev(7:12) * p_redist)

      ! write(*,*)'climo_dist'
      ! do i=1,12
      !   write(*,*)climo_dist(i)
      ! end do

      ! redistribute according to weights 
      climo_redist = med + climo_remaining + climo_dist * w

      ! write(*,*)'climo_redist'
      ! do i=1,12
      !   write(*,*)climo_redist(i)
      ! end do

      ! re-sort to original order 
      climo_adj = climo_redist(climo_order)

      if(shift .ne. 0 ) then
        ! apply shift 
        climo_interp(1) = climo_adj(12)
        climo_interp(2:13) = climo_adj
        climo_interp(14) = climo_adj(1)

        ! interpolate between (x0,y0) and (x1,y1)
        ! y = y0 + (x-x0)*(y1-y0)/(x1-x0)
        ! interpolate between (month0,climo0) and (month1,climo1)
        ! y = climo0 + ((month0+shift)-month0)*(climo1-climo0)/(month1-month0)
        ! could get fancier here and account for the number of days in each month, but 
        ! 30 day months should be a reasonable approximation 
        do i=1,12
          if(shift > 0 ) then
            climo_adj(i) = climo_interp(i+1) + shift*(climo_interp(i+2)-climo_interp(i+1))/30
          else if (shift < 0 ) then
            ! for negative shift the shift applies from the right endpoint of the 
            ! interpolation, so need to convert it to a shift from the left endpoint
            climo_adj(i) = climo_interp(i) + (30-abs(shift))*(climo_interp(i+1)-climo_interp(i))/30
          end if 
        end do 
      end if 

      ! write(*,*)'After shift'
      ! do i=1,12
      !   write(*,*)climo_adj(i)
      ! end do

      ! enforce limits 
      do i=1,12
        if(climo_adj(i) > ul(i)) climo_adj(i) = ul(i)
        if(climo_adj(i) < ll(i)) climo_adj(i) = ll(i)
      end do 

      ! write(*,*)'After limits'
      ! do i=1,12
      !   write(*,*)climo_adj(i)
      ! end do

      ! write(*,*)'Climo'
      ! do i=1,12
      !   write(*,*)climo(i)
      ! end do

      forcing_adjust_mat = climo_adj-climo

      return 
  end function

  function monthly_climo_mean(forcing, month)
    implicit none
    double precision, dimension(:), intent(in):: forcing
    integer, dimension(:), intent(in):: month
    double precision, dimension(12):: monthly_climo_mean, climo_counts
    integer:: nts, i, j

    nts = size(month)

    monthly_climo_mean = 0 
    climo_counts = 0 

    do i=1,nts
      monthly_climo_mean(month(i)) = monthly_climo_mean(month(i)) + forcing(i)
      climo_counts(month(i)) = climo_counts(month(i)) + 1d0
    end do

    monthly_climo_mean = monthly_climo_mean/climo_counts

    return

  end function

  function monthly_climo_sum(forcing, month)
    implicit none
    double precision, dimension(:), intent(in):: forcing
    integer, dimension(:), intent(in):: month
    double precision, dimension(12):: monthly_climo_sum, month_year, month_count, month_sum
    integer:: nts, i, j, current_month, prev_month
    double precision:: msum 

    nts = size(month)

    month_count = 0 
    month_year = 0
    month_sum = 0 
    msum = 0 

    if(month(1) .eq. 1) then
      prev_month = 12
    else 
      prev_month = month(1) - 1
    end if

    do i=1,nts
        if(i > 1) prev_month = month(i-1)
        current_month = month(i)

        if (prev_month .eq. current_month) then 
          msum = msum + forcing(i)
        else
          ! new month
          if(i>1)then
            month_count(prev_month) = month_count(prev_month) + 1
            month_sum(prev_month) = month_sum(prev_month) + msum
          end if
          msum = forcing(i)
        end if 

    end do

    ! last month
    month_count(current_month) = month_count(current_month) + 1
    month_sum(current_month) = month_sum(current_month) + msum

    monthly_climo_sum = month_sum/month_count

    return

  end function

  function sfc_pressure(elevation)
    implicit none

    double precision, parameter :: sfc_pres_a=33.86      !constant for sfc pressure calc (snow-17)
    double precision, parameter :: sfc_pres_b=29.9     !constant for sfc pressure calc (snow-17)
    double precision, parameter :: sfc_pres_c=0.335      !constant for sfc pressure calc (snow-17)
    double precision, parameter :: sfc_pres_d=0.00022    !constant for sfc pressure calc (snow-17)
    double precision, parameter :: sfc_pres_e=2.4      !constant for sfc pressure calc (snow-17)

    double precision, intent(in)   :: elevation
    double precision:: sfc_pressure
    
    sfc_pressure = sfc_pres_a * (sfc_pres_b - (sfc_pres_c * (elevation/100.)) &
               + (sfc_pres_d*((elevation/100.)**sfc_pres_e)))   !sfc pres in hPa

    return
  end function

  function julian_day(year,month,day)
    ! returns vector
    implicit none

    ! input variables
    integer, dimension(:),intent(in) :: year, month, day
    ! output variables
    integer, dimension(size(year)) :: julian_day

    ! Calculate the day of year (1,...,365,366) corresponding to the date
    !   iyear-imonth-iday. 
    julian_day = day &
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
  end function

end module 

