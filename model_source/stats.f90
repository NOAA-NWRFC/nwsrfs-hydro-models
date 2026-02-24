module stats 
  
  use sorting 

  implicit none 

  contains 

  function median(x)
    implicit none
    double precision, dimension(:), intent(in) :: x
    double precision, dimension(size(x)):: tmp
    double precision:: median
    integer:: n

    n = size(x)
    tmp = x

    call sort(tmp)
    if (mod(n,2) == 0) then           ! compute the median
       median = (tmp(n/2) + tmp(n/2+1)) / 2.0
    else
       median = tmp(n/2+1)
    end if
  end function

  function norm_pdf(x, mean, std)

      implicit none

      double precision, dimension(:), intent(in):: x
      double precision, dimension(size(x))::norm_pdf
      double precision, intent(in):: mean, std
      double precision, parameter:: pi=3.141592653589793238462643383279d0 

      norm_pdf = 1.0/(std*sqrt(2.0*pi)) * exp(-0.5*((x-mean)/std)**2.0)

  end function

end module 
