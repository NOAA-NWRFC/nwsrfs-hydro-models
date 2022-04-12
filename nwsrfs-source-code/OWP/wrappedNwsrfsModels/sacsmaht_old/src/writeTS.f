      subroutine writeTS(idt,ts,jtx,knt,fileName)
c
      character*1024 fileName
      integer knt,idt
      real ts(*)
      integer jtx(*)
c 
      open(11,file=fileName,status='REPLACE')
	  write(11,'(i2)')idt
      do 100 i=1,knt
        write (11,'(f8.3,1x,i10)')ts(i),jtx(i)
100   continue
c
      close(11)
c  
      return
      end
