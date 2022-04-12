      subroutine pin1(parampath, po)
     
      include 'flogm'
      
c Initialize ionum common block
      common/ionum/IN, IPR, IPU
      data IN/55/
      character*200 parampath
      dimension po(27)
c
c Get the SAC-HT parameters
c
      open(IN,file=trim(parampath),status='old',iostat=ier)
   
      call pin1b(po)
    
CP file will be closed in frdg1.f 
c     close(IN)
   
      return
      end
