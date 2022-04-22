module uh_optim

implicit none

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

function uh2p_len_obj_optim(scale, shape, toc, dt_hours)

  double precision, intent(in):: shape, scale, dt_hours, toc
  double precision:: uh2p_len_obj_optim
  integer:: uh_len
  integer, parameter:: m = 1000

  ! add one to the length becuase the first ordinate is at time 0
  uh_len = idnint(toc/dt_hours)+1
  uh2p_len_obj_optim = dabs(dble(uh2p_len(shape, scale, dt_hours, m) - uh_len))
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
  ! write(*,*)dt_hours

  do while ((len_1 .le. len_2) .and. (scale .lt. 5d0))
    len_1 = len_2
    scale = scale+0.1d0
    len_2 = uh2p_len(shape, scale, dt_hours, max_len)
    ! write(*,*)shape, dt_hours, len_1, len_2, scale
  end do
  ! write(*,*)scale-0.1d0

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
  ! if(uh(1)==0)write(*,*)"First UH ordinate is zero, that's bad"
  do i = 2, max_len
    if(uh(i) .eq. 0d0)then
      uh2p_len = i-1
      exit
    end if
  end do
  ! write(*,'(100f10.3)') uh(1:uh2p_len)
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
  toc = log(gamma(shape) * scale)

  do i = 1, max_len
    top = i * timestep_days / scale
    tor = (shape - 1) * log(top) - top - toc
    uh2p(i) = 0
    if(tor > -8.0) then
      uh2p(i) = exp(tor)
      ! write(*,*)i, uh2p(i), toc, top, tor
    else
      if(i > 1) then
        uh2p(i) = 0.0
        ! max_len = i
        exit
      end if
    end if
  end do
  s = sum(uh2p)
  if(s .eq. 0d0) s = 1d-5
  uh2p = uh2p/s ! turn it into a unit hydrograph (sums to 1)
  ! write(*,*)s

  ! counter = 0
  ! do i = 1,max_len
  !   if(uh(i) .eq. 0)then
  !     write(*,*)'UH may have been tuncated, increase max_len.'
  !   end if
  ! end do

  !uh2p(:) = uh(:)

end function

function glomin_uh2p( a, b, c, m, machep, e, t, x, shape, toc, dt_hours)

!*****************************************************************************80
!
!! GLOMIN seeks a global minimum of a function F(X) in an interval [A,B].
!
!  Discussion:
!
!    This function assumes that F(X) is twice continuously differentiable
!    over [A,B] and that F''(X) <= M for all X in [A,B].
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    12 April 2008
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization Without Derivatives,
!    Dover, 2002,
!    ISBN: 0-486-41998-3,
!    LC: QA402.5.B74.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the interval.
!    It must be the case that A < B.
!
!    Input, real ( kind = 8 ) C, an initial guess for the global
!    minimizer.  If no good guess is known, C = A or B is acceptable.
!
!    Input, real ( kind = 8 ) M, the bound on the second derivative.
!
!    Input, real ( kind = 8 ) MACHEP, an estimate for the relative machine
!    precision.
!
!    Input, real ( kind = 8 ) E, a positive tolerance, a bound for the
!    absolute error in the evaluation of F(X) for any X in [A,B].
!
!    Input, real ( kind = 8 ) T, a positive error tolerance.
!
!    Input, external real ( kind = 8 ) F, the name of a user-supplied
!    function, of the form "FUNCTION F ( X )", which evaluates the
!    function whose global minimum is being sought.
!
!    Output, real ( kind = 8 ) X, the estimated value of the abscissa
!    for which F attains its global minimum value in [A,B].
!
!    Output, real ( kind = 8 ) GLOMIN, the value F(X).
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) a0
  real ( kind = 8 ) a2
  real ( kind = 8 ) a3
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d0
  real ( kind = 8 ) d1
  real ( kind = 8 ) d2
  real ( kind = 8 ) e
  real ( kind = 8 ) glomin_uh2p
  real ( kind = 8 ) h
  integer ( kind = 4 ) k
  real ( kind = 8 ) m
  real ( kind = 8 ) m2
  real ( kind = 8 ) machep
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) qs
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) sc
  real ( kind = 8 ) t
  real ( kind = 8 ), intent(out):: x
  real ( kind = 8 ) y
  real ( kind = 8 ) y0
  real ( kind = 8 ) y1
  real ( kind = 8 ) y2
  real ( kind = 8 ) y3
  real ( kind = 8 ) yb
  real ( kind = 8 ) z0
  real ( kind = 8 ) z1
  real ( kind = 8 ) z2
  double precision:: shape, toc, dt_hours

  a0 = b
  x = a0
  a2 = a
  y0 = uh2p_len_obj_optim ( b, shape, toc, dt_hours)
  yb = y0
  y2 = uh2p_len_obj_optim ( a, shape, toc, dt_hours)
  y = y2

  if ( y0 < y ) then
    y = y0
  else
    x = a
  end if

  if ( m <= 0.0D+00 .or. b <= a ) then
    glomin_uh2p = y
    return
  end if

  m2 = 0.5D+00 * ( 1.0D+00 + 16.0D+00 * machep ) * m

  if ( c <= a .or. b <= c ) then
    sc = 0.5D+00 * ( a + b )
  else
    sc = c
  end if

  y1 = uh2p_len_obj_optim ( sc , shape, toc, dt_hours)
  k = 3
  d0 = a2 - sc
  h = 9.0D+00 / 11.0D+00

  if ( y1 < y ) then
    x = sc
    y = y1
  end if

  do

    d1 = a2 - a0
    d2 = sc - a0
    z2 = b - a2
    z0 = y2 - y1
    z1 = y2 - y0
    r = d1 * d1 * z0 - d0 * d0 * z1
    p = r
    qs = 2.0D+00 * ( d0 * z1 - d1 * z0 )
    q = qs

    if ( k < 1000000 .or. y2 <= y ) then

      do

        if ( q * ( r * ( yb - y2 ) + z2 * q * ( ( y2 - y ) + t ) ) < &
          z2 * m2 * r * ( z2 * q - r ) ) then
          a3 = a2 + r / q
          y3 = uh2p_len_obj_optim ( a3, shape, toc, dt_hours)

          if ( y3 < y ) then
            x = a3
            y = y3
          end if
        end if

        k = mod ( 1611 * k, 1048576 )
        q = 1.0D+00
        r = ( b - a ) * 0.00001D+00 * real ( k, kind = 8 )

        if ( z2 <= r ) then
          exit
        end if

      end do

    else

      k = mod ( 1611 * k, 1048576 )
      q = 1.0D+00
      r = ( b - a ) * 0.00001D+00 * real ( k, kind = 8 )

      do while ( r < z2 )

        if ( q * ( r * ( yb - y2 ) + z2 * q * ( ( y2 - y ) + t ) ) < &
          z2 * m2 * r * ( z2 * q - r ) ) then
          a3 = a2 + r / q
          y3 = uh2p_len_obj_optim ( a3, shape, toc, dt_hours)

          if ( y3 < y ) then
            x = a3
            y = y3
          end if
        end if

        k = mod ( 1611 * k, 1048576 )
        q = 1.0D+00
        r = ( b - a ) * 0.00001D+00 * real ( k, kind = 8 )

      end do

    end if

    r = m2 * d0 * d1 * d2
    s = sqrt ( ( ( y2 - y ) + t ) / m2 )
    h = 0.5D+00 * ( 1.0D+00 + h )
    p = h * ( p + 2.0D+00 * r * s )
    q = q + 0.5D+00 * qs
    r = - 0.5D+00 * ( d0 + ( z0 + 2.01D+00 * e ) / ( d0 * m2 ) )

    if ( r < s .or. d0 < 0.0D+00 ) then
      r = a2 + s
    else
      r = a2 + r
    end if

    if ( 0.0D+00 < p * q ) then
      a3 = a2 + p / q
    else
      a3 = r
    end if

    do

      a3 = max ( a3, r )

      if ( b <= a3 ) then
        a3 = b
        y3 = yb
      else
        y3 = uh2p_len_obj_optim ( a3, shape, toc, dt_hours)
      end if

      if ( y3 < y ) then
        x = a3
        y = y3
      end if

      d0 = a3 - a2

      if ( a3 <= r ) then
        exit
      end if

      p = 2.0D+00 * ( y2 - y3 ) / ( m * d0 )

      if ( ( 1.0D+00 + 9.0D+00 * machep ) * d0 <= abs ( p ) ) then
        exit
      end if

      if ( 0.5D+00 * m2 * ( d0 * d0 + p * p ) <= &
        ( y2 - y ) + ( y3 - y ) + 2.0D+00 * t ) then
        exit
      end if

      a3 = 0.5D+00 * ( a2 + a3 )
      h = 0.9D+00 * h

    end do

    if ( b <= a3 ) then
      exit
    end if

    a0 = sc
    sc = a2
    a2 = a3
    y0 = y1
    y1 = y2
    y2 = y3

  end do

  glomin_uh2p = y

  return
end

function zero_uh2p ( a, b, machep, t, shape, toc, dt_hours)

!*****************************************************************************80
!
!! ZERO seeks the root of a function F(X) in an interval [A,B].
!
!  Discussion:
!
!    The interval [A,B] must be a change of sign interval for F.
!    That is, F(A) and F(B) must be of opposite signs.  Then
!    assuming that F is continuous implies the existence of at least
!    one value C between A and B for which F(C) = 0.
!
!    The location of the zero is determined to within an accuracy
!    of 6 * MACHEPS * abs ( C ) + 2 * T.
!
!    Thanks to Thomas Secretin for pointing out a transcription error in the
!    setting of the value of P, 11 February 2013.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    11 February 2013
!
!  Author:
!
!    Original FORTRAN77 version by Richard Brent.
!    FORTRAN90 version by John Burkardt.
!
!  Reference:
!
!    Richard Brent,
!    Algorithms for Minimization Without Derivatives,
!    Dover, 2002,
!    ISBN: 0-486-41998-3,
!    LC: QA402.5.B74.
!
!  Parameters:
!
!    Input, real ( kind = 8 ) A, B, the endpoints of the change of 
!    sign interval.
!
!    Input, real ( kind = 8 ) MACHEP, an estimate for the relative machine
!    precision.
!
!    Input, real ( kind = 8 ) T, a positive error tolerance.
!
!    Input, external real ( kind = 8 ) F, the name of a user-supplied
!    function, of the form "FUNCTION F ( X )", which evaluates the
!    function whose zero is being sought.
!
!    Output, real ( kind = 8 ) ZERO, the estimated value of a zero of
!    the function F.
!
  implicit none

  real ( kind = 8 ) a
  real ( kind = 8 ) b
  real ( kind = 8 ) c
  real ( kind = 8 ) d
  real ( kind = 8 ) e
  ! real ( kind = 8 ) f
  real ( kind = 8 ) fa
  real ( kind = 8 ) fb
  real ( kind = 8 ) fc
  real ( kind = 8 ) m
  real ( kind = 8 ) machep
  real ( kind = 8 ) p
  real ( kind = 8 ) q
  real ( kind = 8 ) r
  real ( kind = 8 ) s
  real ( kind = 8 ) sa
  real ( kind = 8 ) sb
  real ( kind = 8 ) t
  real ( kind = 8 ) tol
  real ( kind = 8 ) zero_uh2p
  double precision:: shape, toc, dt_hours
!
!  Make local copies of A and B.
!
  sa = a
  sb = b
  fa = uh2p_len_obj_root ( sa, shape, toc, dt_hours)
  fb = uh2p_len_obj_root ( sb, shape, toc, dt_hours)

  c = sa
  fc = fa
  e = sb - sa
  d = e

  do

    if ( abs ( fc ) < abs ( fb ) ) then

      sa = sb
      sb = c
      c = sa
      fa = fb
      fb = fc
      fc = fa

    end if

    tol = 2.0D+00 * machep * abs ( sb ) + t
    m = 0.5D+00 * ( c - sb )

    if ( abs ( m ) <= tol .or. fb == 0.0D+00 ) then
      exit
    end if

    if ( abs ( e ) < tol .or. abs ( fa ) <= abs ( fb ) ) then

      e = m
      d = e

    else

      s = fb / fa

      if ( sa == c ) then

        p = 2.0D+00 * m * s
        q = 1.0D+00 - s

      else

        q = fa / fc
        r = fb / fc
        p = s * ( 2.0D+00 * m * q * ( q - r ) - ( sb - sa ) * ( r - 1.0D+00 ) )
        q = ( q - 1.0D+00 ) * ( r - 1.0D+00 ) * ( s - 1.0D+00 )

      end if

      if ( 0.0D+00 < p ) then
        q = - q
      else
        p = - p
      end if

      s = e
      e = d

      if ( 2.0D+00 * p < 3.0D+00 * m * q - abs ( tol * q ) .and. &
        p < abs ( 0.5D+00 * s * q ) ) then
        d = p / q
      else
        e = m
        d = e
      end if

    end if

    sa = sb
    fa = fb

    if ( tol < abs ( d ) ) then
      sb = sb + d
    else if ( 0.0D+00 < m ) then
      sb = sb + tol
    else
      sb = sb - tol
    end if

    fb =  uh2p_len_obj_root ( sb, shape, toc, dt_hours)

    if ( ( 0.0D+00 < fb .and. 0.0D+00 < fc ) .or. &
         ( fb <= 0.0D+00 .and. fc <= 0.0D+00 ) ) then
      c = sa
      fc = fa
      e = sb - sa
      d = e
    end if

  end do

  zero_uh2p = sb

  return
end

function dgamma_burkardt ( x )

!*****************************************************************************80
!
!! GAMMA evaluates the gamma function.
!
!  Discussion:
!
!    This function calculates the GAMMA function for a real argument X.
!    Computation is based on an algorithm outlined in reference 1.
!    The program uses rational functions that approximate the GAMMA
!    function to at least 20 significant decimal digits.  Coefficients
!    for the approximation over the interval (1,2) are unpublished.
!    Those for the approximation for X >= 12 are from reference 2.
!    The accuracy achieved depends on the arithmetic system, the
!    compiler, the intrinsic functions, and proper selection of the
!    machine-dependent constants.
!
!  Modified:
!
!    10 January 2016
!
!  Author:
!
!    Original FORTRAN77 version by William Cody, Laura Stoltz.
!    FORTRAN90 version by John Burkardt.
!
! Explanation of machine-dependent constants.  Let
!
! beta   - radix for the floating-point representation
! maxexp - the smallest positive power of beta that overflows
!
! Then the following machine-dependent constants must be declared
!   in DATA statements.  IEEE values are provided as a default.
!
! XBIG   - the largest argument for which GAMMA(X) is representable
!      in the machine, i.e., the solution to the equation
!      GAMMA(XBIG) = beta**maxexp
! XINF   - the largest machine representable floating-point number;
!      approximately beta**maxexp
! EPS    - the smallest positive floating-point number such that
!      1.0+EPS > 1.0
! XMININ - the smallest positive floating-point number such that
!      1/XMININ is machine representable
!
! Error returns
!
!  The program returns the value XINF for singularities or
!     when overflow would occur.  The computation is believed
!     to be free of underflow and overflow.
!
!  Reference:
!
!    An Overview of Software Development for Special
!    functions", W. J. Cody, Lecture Notes in Mathematics,
!    506, Numerical Analysis Dundee, 1975, G. A. Watson
!    (ed.), Springer Verlag, Berlin, 1976.
!
!    Computer Approximations, Hart, Et. Al., Wiley and
!    sons, New York, 1968.
!
  implicit none

  real ( kind = 8 ) dgamma_burkardt
  integer ( kind = 4 ) i
  integer ( kind = 4 ) n
  logical ( kind = 4 ) parity
  real ( kind = 8 ) &
      c,conv,eps,fact,half,one,p,pi,q,res,sqrtpi,sum,twelve, &
      two,x,xbig,xden,xinf,xminin,xnum,y,y1,ysq,z,zero
  dimension c(7),p(8),q(8)
!
!  Mathematical constants
!
  data one,half,twelve,two,zero/1.0d0,0.5d0,12.0d0,2.0d0,0.0d0/, &
       sqrtpi/0.9189385332046727417803297d0/, &
       pi/3.1415926535897932384626434d0/
!
!  Machine dependent parameters
!
  data xbig,xminin,eps/171.624d0,2.23d-308,2.22d-16/, &
       xinf/1.79d308/
!
!  Numerator and denominator coefficients for rational minimax
!     approximation over (1,2).
!
  data p/-1.71618513886549492533811d+0,2.47656508055759199108314d+1, &
     -3.79804256470945635097577d+2,6.29331155312818442661052d+2, &
     8.66966202790413211295064d+2,-3.14512729688483675254357d+4, &
     -3.61444134186911729807069d+4,6.64561438202405440627855d+4/
  data q/-3.08402300119738975254353d+1,3.15350626979604161529144d+2, &
    -1.01515636749021914166146d+3,-3.10777167157231109440444d+3, &
      2.25381184209801510330112d+4,4.75584627752788110767815d+3, &
    -1.34659959864969306392456d+5,-1.15132259675553483497211d+5/
!
!  Coefficients for minimax approximation over (12, INF).
!
  data c/-1.910444077728d-03,8.4171387781295d-04, &
       -5.952379913043012d-04,7.93650793500350248d-04, &
       -2.777777777777681622553d-03,8.333333333333333331554247d-02, &
    5.7083835261d-03/
!
!  Statement functions for conversion between integer and float
!
  conv(i) = dble(i)
  parity = .false.
  fact = one
  n = 0
  y = x

  if (y <= zero) then
!
!  Argument is negative
!
    y = -x
    y1 = aint(y)
    res = y - y1

    if (res /= zero) then
      if (y1 /= aint(y1*half)*two) parity = .true.
      fact = -pi / sin(pi*res)
      y = y + one
    else
      res = xinf
      dgamma_burkardt = res
      return
    end if

  end if
!
!  Argument is positive
!
  if (y < eps) then
!
!  Argument < EPS
!
    if (y >= xminin) then
      res = one / y
    else
      res = xinf
      dgamma_burkardt = res
      return
    end if

  else if (y < twelve) then

    y1 = y
    if (y < one) then
!
!  0.0 < argument < 1.0
!
      z = y
      y = y + one

    else
!
!  1.0 < argument < 12.0, reduce argument if necessary
!
      n = int(y) - 1
      y = y - conv(n)
      z = y - one

    end if
!
!  Evaluate approximation for 1.0 < argument < 2.0
!
    xnum = zero
    xden = one
    do i = 1, 8
      xnum = (xnum + p(i)) * z
      xden = xden * z + q(i)
    end do
    res = xnum / xden + one

    if (y1 < y) then
!
!  Adjust result for case  0.0 < argument < 1.0
!
      res = res / y1

    else if (y1 > y) then
!
!  Adjust result for case  2.0 < argument < 12.0
!
      do i = 1, n
        res = res * y
        y = y + one
      end do

    end if

  else
!
!  Evaluate for argument >= 12.0,
!
    if (y <= xbig) then
      ysq = y * y
      sum = c(7)
      do i = 1, 6
        sum = sum / ysq + c(i)
      end do
      sum = sum/y - y + sqrtpi
      sum = sum + (y-half)*log(y)
      res = exp(sum)

    else

      res = xinf
      dgamma_burkardt = res
      return

    end if

  end if
!
!  Final adjustments and return
!
  if (parity) res = -res
  if (fact /= one) res = fact / res

  dgamma_burkardt = res

  return
end

end module 