      subroutine readTS(idt,ts,jtx,knt,fileName)
c
      character*80 fileName
      integer knt,idt
      real ts(*)
	  integer jtx(*)
c 
      open(11,file=fileName,status='old')
	  knt=0
	  read (11,'(i2)',end=100)idt
      do
	    knt=knt+1
        read (11,'(f8.3,1x,i10)',end=100)ts(knt),jtx(knt)
      end do
  100 continue
c
      close(11)

	  knt=knt-1 !adjust the last line
	  
c  
      return
      end
