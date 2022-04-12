c  **  Transform 'm' characters of array 'x' into integer value
c  **  V. Koren  11/01/92
      function intconv(x,m)
      character*(m) x
c      character*200 x      
      real*8 rv
      
      do 2 i=1,m
      n=m+1-i
      if(x(n:n).eq.' ') goto 2
      goto 3
    2 continue
    3 iv=0
      rvs=0.
      do 1 i=1,n
       if(x(i:i) .eq. ' ') goto 1
        iv=(ICHAR(x(i:i))-48)
        rv=iv*10**(n-i)
        rvs=rvs+rv
    1 continue
      intconv=rvs

      return
      end
