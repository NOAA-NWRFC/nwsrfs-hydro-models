      subroutine printstates

      include 'flogm'

      common/pass10/par(20),adj(2),state(6)
      common/finfg0/ nsinp,zs(8),ts(8),fgco0(6),pta0,pwe0,psh0
      common/fpmfg1/ itta,fgpm(15),iver,ifrze,ixrc,nsoillayer
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)

      WRITE(MESSAGESTRING,*) 'Initial state(6) ...'
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

      WRITE(MESSAGESTRING,'(6(f,1x))') state(1:6)
      call logfromfortran(DEBUG_LEVEL, MESSAGESTRING) 

      if ( ixrc .gt. 0 ) then
         WRITE(MESSAGESTRING,*) 'Extra carryover - rsm(7), ppx, ppsc '
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
         WRITE(MESSAGESTRING,'(7(f,1x))') rsm(1:7)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
         WRITE(MESSAGESTRING,'(2(f,1x))')ppx, ppsc
      end if

      if ( ifrze .gt. 0 ) then
        WRITE(MESSAGESTRING,*) 'frozen ground carryover'
        call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
        WRITE(MESSAGESTRING,'(6(f,1x))') fgco(1:6)
        call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
        WRITE(MESSAGESTRING,'(3(f,1x))') pta, pwe, psh
        call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
c
        WRITE(MESSAGESTRING, 10) nsinp, nsoillayer
 10     FORMAT(19HNumber Soil Layer: i4,2x,i4)
        call logfromfortran(DEBUG_LEVEL, MESSAGESTRING) 

        if ( iver.gt.1 .and. (nsinp .gt. 0 .or.
     1       nsoillayer .gt. 0) ) then

          if ( nsoillayer .lt. nsinp ) nsoillayer = nsinp

          WRITE(MESSAGESTRING,'(8(f,1x))') fgpm(8:7+nsoillayer)
          call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
          WRITE(MESSAGESTRING,'(8(f,1x))') tsoil(1:nsoillayer)
          call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
          WRITE(MESSAGESTRING,'(8(f,1x))') smc(1:nsoillayer)
          call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
          WRITE(MESSAGESTRING,'(8(f,1x))') sh2o(1:nsoillayer)
          call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
        end if

      end if
c  
      return
      end
