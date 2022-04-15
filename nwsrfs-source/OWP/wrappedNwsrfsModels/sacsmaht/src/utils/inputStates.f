c**********************************************************************
c This subroutine gets the input state from statesI.txt file
c
c Cham Pham 2/19/09
c**********************************************************************
      subroutine inputStates( CO )

      include 'flogm'
      
      common/pass10/par(20),adj(2),state(6) 
      common/finfg0/ nsinp,zs(8),ts(8),fgco0(6),pta0,pwe0,psh0
      common/fpmfg1/ itta,fgpm(15),iver,ifrze,ixrc,nsoillayer
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)
      dimension co(*)

      if ( FEWSDEBUG .GE. 4 ) then
         WRITE(MESSAGESTRING, *) ' INPUT CO ARRAY - EX1 '
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      end if
c
c Get inital states - state(6)
c
      do i = 1, 6
         state(i) = co(i)
      end do
      
      !write(*,'(6(f,1x))') state(1:6)
      
      nco = 6
      !print *,'nco1: ',nco
c
c Get additional carryover if ixrc is greater than 0
c
      if ( ixrc .gt. 0 ) then
        do i = 1, 7
           rsm(i) = co(nco+i)
        end do
        !write(*,'(7(f,1x))') rsm(1:7)
        nco = nco + 7
       
        ppx = co(nco+1)
        ppsc = co(nco+2)
        !print *,'ppx: ',ppx,' ppsc: ',ppsc
        nco = nco + 2
        !print *,'nco2: ',nco
      end if
c
c Get number of soil layer
c
      nsinp = co(nco+1)
      nco = nco + 1
c
c Get frozen ground carryover
c
      do i = 1, 6
        fgco(i) = co(nco+i)
      end do

      !write(*,'(6(f,1x))') fgco(1:6)
      nco = nco + 6
c
c Get inital air temperature, snow water equivalent and snow depth
c
      pta = co(nco+1)
      pwe = co(nco+2)
      psh = co(nco+3)
      !print *,'pta: ',pta,' pwe: ',pwe,' psh: ',psh
      nco = nco + 3
      !print *,'nco3: ',nco
c
c Get depth to bottom, soil moisture, soil temperature and unfrozen
c water if number of soil layer is greater than 0
c
      if ( nsinp .gt. 0 ) then
         do i = 1, nsinp
           fgpm(8:7+i) = co(nco+i)
           tsoil(i) = co (nco+nsinp+i)
           smc(i) = co(nco+(nsinp*2)+i)
           sh2o(i) = co(nco+(nsip*3)+i)
         end do
         !write(*,'(<nsinp>(f,1x))') fgpm(8:7+nsinp)
         !write(*,'(<nsinp>(f,1x))') tsoil(1:nsinp)
         !write(*,'(<nsinp>(f,1x))') smc(1:nsinp)
         !write(*,'(<nsinp>(f,1x))') sh2o(1:nsinp)
       end if
       
      
      if ( FEWSDEBUG .GE. 4 ) then
         nco = nco + (nsinp * 4)  !for testing purposes
         write(MESSAGESTRING, 100) nco
 100     format(22H InputStates: num co: i4)
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)

         write(FORMATSTR, *) '%12.6f'
         call logonedimensionarrayfromfortran(DEBUG_LEVEL,NULLLINE,1,4,
     1        FORMATSTR,1,nco,1,co)
      end if
      return
      end
