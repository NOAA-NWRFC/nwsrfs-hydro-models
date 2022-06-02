subroutine rsnwelev(n_hrus,sim_length, &
    taelev_in, talr_in, pxtemp_in, &
    mat_in, &
    rsel_out)

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
! !     mat_in:  temperature timeseries, DEGC (double)
! !
! !     OUTPUTS
! !     rsel_out:  rainsnow line timeseries, M (double)

  implicit none

  ! ! inputs
  integer, intent(in):: sim_length, n_hrus
  double precision, dimension(n_hrus), intent(in):: taelev_in, talr_in, pxtemp_in
  double precision, dimension(sim_length, n_hrus), intent(in):: mat_in

  ! ! local varible
  integer, parameter:: izin = 0
  integer::  nh
  real, dimension(sim_length, n_hrus):: mat, rsel
  real, dimension(sim_length):: zelv
  real, dimension(n_hrus):: taelev, talr, pxtemp
  real::  co

  ! ! output 
  double precision, dimension(sim_length ,n_hrus), intent(out):: rsel_out

  ! ! Convert double precision to single precision.
  mat=real(mat_in)
  taelev=real(taelev_in)
  talr=real(talr_in)
  pxtemp=real(pxtemp_in)
  
  do nh=1,n_hrus
  
    call EX42(taelev(nh),talr(nh),pxtemp(nh),int(izin,4),int(sim_length,4),co,mat(:,nh),rsel(:,nh),zelv)
  
  end do

  rsel_out=dble(rsel)

end subroutine