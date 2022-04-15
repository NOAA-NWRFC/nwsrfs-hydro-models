c**********************************************************************
c N7_Nov_04.f - runs frozen ground estimation                         *
c               version Oct 10, 2003 by D.-J. Seo, V. Kuzmin and V.   *
c               Koren                                                 *
c**********************************************************************
      subroutine EX1(PO, CO, TCIVALS, FGIXVALS, FRZDVALS, UZTWCO,
     >               UZFWCO, LZTWCO, LZFSCO, LZFPCO, ADIMCO, SSM, SST)
      
      include 'flogm'      
      
      parameter (kx=85000)
CP    parameter (IPRINT = 0, IOUT = 6)
   
c
cv  Frozen ground added
c
      common/fpmfg1/ itta,fgpm(15),iver,ifrze,ixrc,nsoillayer
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)
      common/pass4/knt,dt,nv,saved,parea,knq,idtq,istrt,iend,mstrt,mend
      common/pass10/par(20),adj(2),state(6)    
      common/pass1/pxv0(kx),edmnd0(kx),qobs0(kx),jtime(kx),
     +             tair0(kx),sdpt0(kx),swe0(kx),sasc0(kx)  
      COMMON/TSLINT/DSINT(10),TSINT(10),NDSL,SWINT(10),SWHINT(10) 
      dimension state1(6), co(kx), po(27)
      real  ssm(*), sst(*)
      real lztwc,lzfsc,lzfpc
      real*4 tcivals(kx), fgixvals(kx), frzdvals(kx)
      real*4 UZTWCO(KX), UZFWCO(KX), LZTWCO(KX), LZFSCO(KX), LZFPCO(KX),
     +       ADIMCO(KX)
c
c Copy states information from statesI.txt file
c CP 2/25/09
      call inputStates(CO)
c
c Initialize few frozen ground constant to pass through common statement
c and set default new frozen ground parameters
c CP 2/26/09 
CP    call frdfg1(idt,supm,slwm,state,par(18),par(19),par(20))
CP    if ( ifrze .ne. 0 ) then
        supm=par(1)+par(2)
        slwm=par(9)+par(10)+par(11)
        call frdfg1(supm,slwm)
CP    end if
c
c Recalculate states into ratio of Max_parameters
cCP 2/28/09
      state(1)=state(1)/par(1)
      state(2)=state(2)/par(2)
      state(3)=state(3)/par(9)
      state(4)=state(4)/par(10)
      state(5)=state(5)/par(11)
      if (par(5) .eq. 0.) then
        state(6)=state(6)/(par(1)+par(9))
      else
        state(6)=state(6)/par(5)
      endif
c
c Convert states into storages
c
      state1(1)=state(1)*par(1)
      state1(2)=state(2)*par(2)
      state1(3)=state(3)*par(9)
      state1(4)=state(4)*par(10)
      state1(5)=state(5)*par(11)
      if (par(5) .eq. 0.) then
        state1(6)=state(6)*(par(1)+par(9))
      else
        state1(6)=state(6)*par(5)
      endif  
c  
c Check state ranges
c
      call check_range(par,adj,state1)
c   
c Fill initial states from 'fland1' definition
c
      uztwc=state1(1)
      uzfwc=state1(2)
      lztwc=state1(3)
      lzfsc=state1(4)
      lzfpc=state1(5)
      adimc=state1(6)
      if (adimc .lt. uztwc) adimc=uztwc

      saved=par(15)*(par(11)+par(10))
      parea=1.0-par(4)-par(5)
c
c Initialize frozen ground states and constants
c 03/07 if(iver .eq. 3) then
c 03/07 supm=par(1)+par(2)
c 03/07 slwm=par(9)+par(10)+par(11)
c 03/07 call frdfg0(supm,slwm,state1)
c 03/07 endif 
c
c  Copy frozen ground parameters from PAR to FGPM (in case of calibratio
c  of frozen ground parameters: FGPM(4), or FGPM(5), or FGPM(6))
c
      fgpm(4)=par(18)
      fgpm(5)=par(19)
      fgpm(6)=par(20)
c
c Get user-defined number of soil depths from param file
c CP added 2/25/09
      po(24) = NDSL + 0.01
c      
c Get number of soil layer from model calculated
      nsoillayer = fgpm(7) 
      po(22) = nsoillayer+0.01
    
c
c Get number of soil layer from user defined  
c
      nslayer = nsoillayer 
      if ( NDSL .gt. 0 ) nslayer = NDSL
c
c Initialize
c
      idt=int(dt*24)
      
CP 02/09 unused???  func=-1.   
c----------------------------------------------------------------------
c Perform operations for each time step
c----------------------------------------------------------------------
      do 10 k=1,knt
       edmnd=edmnd0(k)*adj(2)
       pxv=pxv0(k)*adj(1)
       ta=tair0(k)
       sh=sdpt0(k)
       we=swe0(k)
       aesc=sasc0(k)

       call time1(jtime(k),iyr,mon,iday,ihr)

c=====================================================================
c      Run sac-sma for one time step 
c=====================================================================
       CALL FLAND1(PAR,DT,PXV,EDMND,TA,WE,AESC,SH,TCI,FGIX,FRZD,
     +             IDAY,IHR,MON,IYR,k,RUZICE,RLZICE,RUZPERC)
CP 02/09+             IPRINT,IOUT,IDAY,IHR,MON,IYR,k
CP      +             ,RUZICE,RLZICE,RUZPERC)
       
       tcivals(k) = TCI      !CP - runoff ts
       fgixvals(k) = FGIX    !CP - frozen ground index
c
c CP 3/2/09 Because java version is writing negative values, so we want
c the values to be negative for frozen depth (SFGD)
c
       frzdvals(k) = FRZD    !CP - frozen depth

       if ( FRZD .GT. 0. ) then
         frzdvals(k) = -FRZD !CP - frozen depth
       end if
c
c CP added 02/09
c Write out soil moiture and soil temperature at each layer as outputs
c
       iperiod = 0
       DO i=1, nslayer 

        IF ( iver .gt. 1 ) THEN
c
c If user defined depths (DNSL > 0), then use interpolate soil variable
c as output ts
c
           if ( NDSL .gt. 0 ) then
             ssm(k*i+iperiod) = swint(i) 
             sst(k*i+iperiod) = tsint(i)
           else
             ssm(k*i+iperiod) = smc(i)
             sst(k*i+iperiod) = tsoil(i)
           end if
c
c Use soil moisture only
c
        ELSE
           if ( NDSL .gt. 0 ) then
             ssm(k*i+iperiod) = swint(i)
           else 
             ssm(k*i+iperiod) = smc(i)
           endif
        END IF
        IF ( FEWSDEBUG .GE. 5 ) THEN
	   WRITE(MESSAGESTRING,*) 'ssm(',i,',',k*i+iperiod,') = ',
     >                            smc(i),' sst = ',tsoil(i)
           call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
        END IF

        iperiod = iperiod + knt - k

       END DO
c
c Write out uztwc, uzfwc, lztwc, lzfsc, lzfpc and adimc as outputs
c
       UZTWCO(K) = uztwc
       UZFWCO(K) = uzfwc
       LZTWCO(K) = lztwc
       LZFSCO(K) = lzfsc
       LZFPCO(K) = lzfpc
       ADIMCO(K) = adimc
       
   10 continue 
c
c Copy the output states to CO array then pass to C function 
c CP 02/09
c
       call outputStates(ta,sh,we,CO)  
   
      return
      end
