module uh_optim

! Unit-hydrograph scale optimization utilities.
!
! All routines in this file are distributed under the Apache License (>= 2),
! matching the rest of the package.
!
! `zero_uh2p` below finds the gamma-UH scale by bracketed root finding using
! Brent's method. The solver itself, `zero_brent` in
! `model_source/zero_brent.f90`, is Richard Brent's algorithm in the
! MIT-licensed Fortran 90 version by John Burkardt (see inst/COPYRIGHTS,
! Section 2); it is vendored verbatim and not modified here.

implicit none

! Module-scope state used to pass extra parameters (shape, toc, dt_hours) to
! the one-argument objective function `objective_callback`. Not thread-safe,
! but nwsrfsr only calls zero_uh2p serially from R's .Fortran interface, one
! simulation at a time.
double precision, private :: g_shape
double precision, private :: g_toc
double precision, private :: g_dt_hours

private :: objective_callback

contains

function uh2p_len_obj_root(scale, shape, toc, dt_hours)

  double precision, intent(in):: shape, scale, dt_hours, toc
  double precision:: uh2p_len_obj_root
  integer:: uh_len
  integer, parameter:: m = 1000

  ! add one to the length becuase the first ordinate is at time 0
  uh_len = idnint(toc/dt_hours)+1
  uh2p_len_obj_root = dble(uh2p_len(shape, scale, dt_hours, m) - uh_len)
end function

function scale_uplimit(shape, dt_hours, max_len)

  double precision, intent(in):: shape, dt_hours
  double precision:: scale_uplimit, scale
  integer:: max_len

  ! local variables
  integer:: len_1, len_2

  scale = 0.1d0
  len_1 = 0
  len_2 = uh2p_len(shape, scale, dt_hours, max_len)

  do while ((len_1 .le. len_2) .and. (scale .lt. 5d0))
    len_1 = len_2
    scale = scale+0.1d0
    len_2 = uh2p_len(shape, scale, dt_hours, max_len)
  end do

  scale_uplimit = scale-0.1d0
end function

function uh2p_len(shape, scale, timestep, max_len)
  !' Returns the length (in timesteps) of a 2 parameter (shape,scale) gamma unit hydrograph.
  !' The ordinates are based on the given timestep (in hours). To match the F77 code, a max
  !' length is used.
  !'
  !' @param shape gamma shape parameter
  !' @param scale gamma scale parameter
  !' @param timestep timestep in hours
  !' @param max_len max length of the uh

  double precision, intent(in):: shape, scale, timestep
  integer, intent(in):: max_len
  integer:: uh2p_len

  ! local variables
  double precision, dimension(max_len)::uh
  integer:: i

  uh = uh2p(shape, scale, timestep, max_len)

  uh2p_len = max_len
  do i = 2, max_len
    if(uh(i) .eq. 0d0)then
      uh2p_len = i-1
      exit
    end if
  end do
end function


function uh2p(shape, scale, timestep, max_len)
  !' Returns ordinates for a 2 parameter (shape,scale) gamma unit hydrograph. The ordinates are
  !' based on the given timestep (in hours). To match the F77 code, a max length is used.

  !' @param shape gamma shape parameter
  !' @param scale gamma scale parameter
  !' @param timestep timestep in hours
  !' @param max_len max length of the uh

  double precision, intent(in):: shape, scale, timestep
  integer, intent(in):: max_len
  double precision:: uh2p(max_len)
  double precision:: timestep_days

  ! local variables
  double precision:: toc, top, tor, s
  integer:: i

  ! this code needs timestep in days
  timestep_days = timestep/24d0
  uh2p = 0d0
  ! gamma() here is the Fortran 2008 intrinsic provided by the compiler
  toc = log(gamma(shape) * scale)

  do i = 1, max_len
    top = i * timestep_days / scale
    tor = (shape - 1) * log(top) - top - toc
    uh2p(i) = 0
    if(tor > -8.0) then
      uh2p(i) = exp(tor)
    else
      if(i > 1) then
        uh2p(i) = 0.0
        exit
      end if
    end if
  end do
  s = sum(uh2p)
  if(s .eq. 0d0) s = 1d-5
  uh2p = uh2p/s ! turn it into a unit hydrograph (sums to 1)

end function


function objective_callback(x) result(f)
  ! One-argument objective passed to the Brent solver. Extra parameters come
  ! from module-private state (g_shape, g_toc, g_dt_hours) populated by
  ! zero_uh2p immediately before the solve. This is a module procedure (not an
  ! internal one), so passing it as an actual argument does not require a
  ! compiler trampoline.
  double precision, intent(in):: x
  double precision:: f
  f = uh2p_len_obj_root(x, g_shape, g_toc, g_dt_hours)
end function


function zero_uh2p(a_in, b_in, t, shape, toc, dt_hours)
  ! Find a root of uh2p_len_obj_root on [a_in, b_in] using Brent's method.
  !
  ! Inputs:
  !   a_in, b_in : bracketing interval, must satisfy f(a_in)*f(b_in) <= 0
  !   t          : positive error tolerance passed to zero_brent
  !   shape, toc, dt_hours : parameters forwarded to the objective via
  !                          module-scope state variables
  !
  ! Returns the estimated root. If the interval is not a valid bracket
  ! (endpoints have the same sign), returns the endpoint with the smaller
  ! absolute residual, matching the previous defensive fallback behaviour.

  double precision, intent(in):: a_in, b_in, t
  double precision, intent(in):: shape, toc, dt_hours
  double precision:: zero_uh2p

  ! zero_brent is Brent's method (Burkardt's MIT-licensed Fortran 90 version),
  ! vendored verbatim in zero_brent.f90. It takes the objective as an external
  ! function. objective_callback is a module procedure, so passing it here does
  ! not require a compiler trampoline (which would mark the shared object's
  ! stack executable).
  double precision, external:: zero_brent

  double precision:: fa, fb
  integer:: calls

  ! Stash extra parameters for the one-argument objective_callback
  g_shape = shape
  g_toc = toc
  g_dt_hours = dt_hours

  ! zero_brent requires a sign-changing bracket. Guard against a degenerate
  ! interval the same way the previous implementation did.
  fa = uh2p_len_obj_root(a_in, shape, toc, dt_hours)
  fb = uh2p_len_obj_root(b_in, shape, toc, dt_hours)
  if ((fa > 0d0 .and. fb > 0d0) .or. (fa < 0d0 .and. fb < 0d0)) then
    if (abs(fa) <= abs(fb)) then
      zero_uh2p = a_in
    else
      zero_uh2p = b_in
    end if
    return
  end if

  zero_uh2p = zero_brent(a_in, b_in, t, objective_callback, calls)

end function

end module
