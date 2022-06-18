subroutine rsnwelev(n_hrus,sim_length, &
    taelev_in, talr_in, pxtemp_in, &
    aetbl_len, aetbl, &
    mat_in, &
    ptps_out)

! !     Subroutine Description
! !     -----------------------------------
! !     The rsnwelev subroutine is a wrapper to run 
! !     the NWRFS the subroutine in ex42.f.
! !
! !     The EX42 subroutine uses a temperature timeseries, lapse rate,
! !     threshold temperature, and reference zone elevation to calculate a 
! !     rain snowline timeseries.
! !
! !     Arguments
! !     -----------------------------------
! !     INPUTS
! !     n_hrus:  Number of zones(integer)
! !     sim_length:  The number of time steps of the simulation (integer)
! !     taelev_in:  Elevation associated with the air temperature time
! !                 series,M (double)
! !     talr_in:  Lapse rate during precipitation periods,DEGC/100M (double)
! !     pxtemp_in:  Threshold temperature, DEGC (double)
! !     aetbl_len:  Length of the area elevation table (integer)
! !     aetbl:  percent area and elev (m) pairs, in
! !                 that order, in a single column array.  Must be in ascending order
! !                 of elevation (example: [.1, 1092, .15, 1135, .2, 1158]).
! !     mat_in:  temperature timeseries, DEGC (double)
! !
! !     OUTPUTS
! !     rsel_out:  rainsnow line timeseries, M (double)

  implicit none

  ! ! inputs
  integer, intent(in):: sim_length, n_hrus,aetbl_len
  double precision, dimension(n_hrus), intent(in):: taelev_in, talr_in, pxtemp_in
  double precision, dimension(sim_length, n_hrus), intent(in):: mat_in
  double precision, dimension(2*aetbl_len, n_hrus), intent(in):: aetbl

  ! ! local varible
  integer, parameter:: izin = 0
  integer::  nh, i, n
  real, dimension(sim_length, n_hrus):: mat, rsel
  real, dimension(sim_length):: zelv
  real, dimension(n_hrus):: taelev, talr, pxtemp
  real::  co, ptps_ts,rsel_ts, ae_interp

  ! ! output 
  double precision, dimension(sim_length ,n_hrus), intent(out):: ptps_out

  ! ! Convert double precision to single precision.
  mat=real(mat_in)
  taelev=real(taelev_in)
  talr=real(talr_in)
  pxtemp=real(pxtemp_in)

  do nh=1,n_hrus

    call EX42(taelev(nh),talr(nh),pxtemp(nh),int(izin,4),int(sim_length,4),co,mat(:,nh),rsel(:,nh),zelv)

    ! !convert resel to a ptps value
    ! !loop though each timestep
    do i = 1,sim_length,1
      rsel_ts=rsel(i,nh)
      ptps_ts=-1

      ! ! if the rsel is below the area elev curve, set ptps to 1
      if(rsel_ts<aetbl(2,nh)) then
        ptps_ts=1d0
      else if(rsel_ts==aetbl(2,nh)) then
        ptps_ts=1d0-aetbl(2,nh)
      endif

      n=4
      ! !Walk through the area elevation curve finding when a elevation value in greater than rsel, then interpolate PTPS
      ! !using the table
      do while (ptps_ts==-1 .and. n<=aetbl_len*2)
        if(rsel_ts<=aetbl(n,nh)) then
          ae_interp=((aetbl(n-1,nh)-aetbl(n-3,nh))/(aetbl(n,nh)-aetbl(n-2,nh)))*(rsel_ts-aetbl(n-2,nh))+aetbl(n-3,nh)
          ptps_ts=1d0-ae_interp
        endif
        n=n+2
      end do

      !If rsel is greater than all elevation values in the table, set ptps to 0
      if(ptps_ts==-1) then
        ptps_ts=0d0
      endif

      ptps_out(i,nh)=ptps_ts
    
    end do
  end do
end subroutine