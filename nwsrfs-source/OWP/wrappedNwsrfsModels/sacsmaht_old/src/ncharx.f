      function ncharx(name,n)
      character*(*) name
      
      do i=1,n
        k=n+1-i
        if(name(k:k) .ne. ' ') goto 2
      enddo
    2 ncharx=k
    
      return
      end      
