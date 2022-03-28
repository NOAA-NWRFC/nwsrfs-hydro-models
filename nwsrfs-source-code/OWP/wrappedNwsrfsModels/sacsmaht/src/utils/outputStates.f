c**********************************************************************
c This subroutine write out the state to CO array then pass to
c C function for writing data into StatesO.txt file 
c
c Cham Pham 2/19/09
c**********************************************************************
      subroutine outputStates(ta,sh,we,CO)

      include 'flogm'

      common/pass10/par(20),adj(2),state(6)
      common/fpmfg1/ itta,fgpm(15),iver,ifrze,ixrc,nsoillayer
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)

      real uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc
      real ta,we,sh
      dimension co(*)
      
      if ( FEWSDEBUG .GE. 4 ) THEN
         write( MESSAGESTRING, *) '  OUTPUT CO ARRAY - EX'
	 call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
      end if
c
c  Store inital output states - state(6)
c
      co(1) = uztwc
      co(2) = uzfwc
      co(3) = lztwc
      co(4) = lzfsc
      co(5) = lzfpc
      co(6) = adimc
c
c Store addtional carryover - rsm(7), ppx, ppsc
c
      nco = 6
      if ( ixrc .gt. 0 ) then
        do i = 1, 7
	   co(nco+i) = rsm(i)
        end do
        co(nco+7+1) = ppx
        co(nco+7+2) = ppsc
        nco = nco+9
      end if
c
c Store number of soil layer
c num soil layer is greater than 0 for warm state
c
        co(nco+1) = nsoillayer
        nco = nco + 1
c
c Store frozen ground carryover
c fgco(6) and updated values of air temperature, snow water equivalent 
c and snow depth
c
        do i = 1, 6
           co(nco+i) = fgco(i)
        end do

        co(nco+6+1) = ta
        co(nco+6+2) = we
        co(nco+6+3) = sh

        nco = nco + 9
c
c Store fgpm, tsoil smc and sho2 for each soil layer
c max num soil layer is 7
c
        if ( nsoillayer .gt. 0 ) then
          do i = 1, nsoillayer
             co(nco+i) = fgpm(7+i)
             co(nco+nsoillayer + i) = tsoil(i)
             co(nco+(2*nsoillayer)+i) = smc(i)
             co(nco+(3*nsoillayer)+i) = sh2o(i)
          end do
        end if
c
      if ( FEWSDEBUG .GE. 4 ) then
         nco = nco + (nsoillayer *4)  !for testing purposes
         write(MESSAGESTRING, 100) nco
 100     format(22HOutputStates: num co: i4) 
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
   
         write(FORMATSTR, *) '%12.6f'
         call logonedimensionarrayfromfortran(DEBUG_LEVEL,NULLLINE,1,4,
     1        FORMATSTR,1,nco,1,co)
      end if

      return
      end
