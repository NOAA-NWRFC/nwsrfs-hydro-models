      subroutine writeStates(ta,sh,we,fileName)

      common/fpmfg1/ itta,fgpm(15),iver,ifrze
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)
cv07     +               ppx,ppsc,pta,pwe,psh,tsoil(8)

      real uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc
c      real*6 fgco
      real ta,we,sh
      integer numSoilLayer

      character*1024 fileName
c 
    
      open(11,file=fileName(1:len(filename)),status='REPLACE')

      write(11,'(f)')uztwc
      write(11,'(f)')uzfwc
      write(11,'(f)')lztwc
      write(11,'(f)')lzfsc
      write(11,'(f)')lzfpc
      write(11,'(f)')adimc
      write(11,'(f)')fgco(1)
      write(11,'(f)')fgco(2)
      write(11,'(f)')fgco(3)
      write(11,'(f)')fgco(4)
      write(11,'(f)')fgco(5)
      write(11,'(f)')fgco(6)
      write(11,'(f)')ta
      write(11,'(f)')we
      write(11,'(f)')sh
c
      numSoilLayer=fgpm(7)
      write(11,'(8(f,1x))')fgpm(8:7+numSoilLayer)
      write(11,'(8(f,1x))')TSOIL(1:numSoilLayer)
      write(11,'(8(f,1x))') smc(1:numSoilLayer)
      write(11,'(8(f,1x))') sh2o(1:numSoilLayer)
c
      close(11)
c  
      return
      end
