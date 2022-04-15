c**********************************************************************
c N7_Nov_04.f - runs frozen ground estimation         *
c               version Oct 10, 2003 by D.-J. Seo, V. Kuzmin and V.   *
c               Koren                                                 *
c**********************************************************************

      common/pass4/knt,dt,nv,saved,parea,knq,idtq,istrt,iend    
	  
      common/fpmfg1/ itta,fgpm(15),iver,ifrze


      character*200 infile,mapx_name
      character*200 tair_name, sdpt_name,swe_name,sasc_name
      character*80 inputDir
      character*80 basinName
      character*12 startDate
      character*12 endDate
      integer istrt, iend
      

      call getarg(1,basinName)      
      call getarg(2,startDate)
      read(startDate,'I')istrt
      call getarg(3,endDate)
      read(endDate,'I')iend
      call getarg(4,inputDir)
       
	  
       iver=3
c construct file names for SAC-UH parameters, MAPX, MAT time series, and
c hourly streamflow data
       infile=TRIM(inputDir)//'/statesNParams.txt'
       tair_name=TRIM(inputDir)//'/mat.txt'
       sdpt_name=TRIM(inputDir)//'/snsg.txt'     
       swe_name=TRIM(inputDir)//'/swe.txt'
       sasc_name=TRIM(inputDir)//'/sasc.txt'
       mapx_name=TRIM(inputDir)//'/raim.txt'
       

c
c
c get the SAC-UH parameters, MAPX time series and hourly streamflow
c data
c
      call get_params_and_data(infile,mapx_name,tair_name,
     +      sdpt_name,swe_name,sasc_name)
c
       call func(inputDir)
     
      stop
      end
c**********************************************************************
      subroutine get_params_and_data(infile,mapx_name,
     +       tair_name,sdpt_name,swe_name,sasc_name)
c**********************************************************************
      parameter (kx=85000,lx=200)
      common/pass1/pxv0(kx),edmnd0(kx),qobs0(kx),jtime(kx),
     +             tair0(kx),sdpt0(kx),swe0(kx),sasc0(kx)      
      common/pass4/knt,dt,nv,saved,parea,knq,idtq,istrt,iend,mstrt,mend
      common/pass7/ett(24),idt
      common/pass10/po(lx),par(20),adj(2),state(6)
      integer jtx(kx)
      dimension epdist(24)
      character meteng*4
      data dl,dles/4hl   ,4hdles/
      data prst/4hprst/
      data blank,sums,rocl,smzc/4h    ,4hsums,4hrocl,4hsmzc/
      data frze/4hfrze/
      character*200 infile,mapx_name,outfile*80
      character*200 tair_name,sdpt_name,swe_name,sasc_name
      character*200 text*40
      character string*80,site1*5,site2*5,cdum1*8,cdum2*8
c
c read in the input deck (the $basin_oper.opt file)
c
c   do not read if run test period after calibration
       call readOpt(infile,state,ett,idt,par,adj,saved,parea)
      
       call readTS(idt,pxv0,jtime,knt,mapx_name)
       call readTS(idt,tair0,jtx,knx,tair_name)
       call readTS(idt,sdpt0,jtx,knx,sdpt_name)
       call readTS(idt,swe0,jtx,knx,swe_name)
       call readTS(idt,sasc0,jtx,knx,sasc_name)

c
c initialize
cv      istart=0
      iSavedDate = -999
      do 10 k=1,knt
       jyr=jtime(k)/1000000
       jx=jtime(k)-jyr*1000000
       jmon=jx/10000
       jx=jx-jmon*10000
       jday=jx/100
       ihour=jx-jday*100
       
cv       if(istart.eq.0.or.ihour.eq.24) then
       if(((iSavedDate .NE. jday) .AND. (ihour.NE.0)).OR.
     + (iSavedDate .EQ. -999)) then
c----------------------------------------------------------------------
c  define daily et-distribution for internal time
c  uniform daily et distribution
        mv=24/idt
        v=1.0/mv
        do 107 i=1,mv
         epdist(i)=v
107     continue

c   initial et-demand or pe-adjustment
c   get month and day number for internal time
c   determine number of days in month for incrementing.
        moet=jmon
        idet=jday
        iyet=jyr
        if (idet.lt.16) then
         j=moet-1
         if (j.eq.0) j=12
         ei=ett(1+11+j)
         v=16-idet
         et=ett(1+moet-1)-v*ei
        else
         ei=ett(1+11+moet)
         v=idet-16
         et=ett(1+moet-1)+v*ei
        endif
c
        dt=idt/24.0
        kint=ihour/idt
        epuadj=et
c       ep=epuadj*peadj
        ep=epuadj
        spet=spet+ep !!!!!!!!!!!!!!!!!!
        etd=ep
        iSavedDate = jday
c----------------------------------------------------------------------
       endif

c     compute evapotranspiration loss for the time interval.
c        edmnd is the et-demand for the time interval
      edmnd=etd*epdist(1)
      edmnd0(k)=edmnd
   10 continue
      return
      end

c**********************************************************************
      real*8 function func(inputDir)
c**********************************************************************
      parameter (nndt = 3,kx=85000,lx=200)
      parameter (IPRINT = 0, IOUT = 6)
	  character*80 inputDir

cv  frozen ground added
      common/fpmfg1/ itta,fgpm(15),iver,ifrze
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)
     
      common/pass4/knt,dt,nv,saved,parea,knq,idtq,istrt,iend,mstrt,mend
      common/pass10/po(lx),par(20),adj(2),state(6)    
      common/pass1/pxv0(kx),edmnd0(kx),qobs0(kx),jtime(kx),
     +             tair0(kx),sdpt0(kx),swe0(kx),sasc0(kx)  
      dimension state1(6)
      real lztwc,lzfsc,lzfpc
      real tciVals(kx)
      character*20 fileName
      character*1024 stateFile, tci_name

c Convert states into storages
      state1(1)=state(1)*par(1)
      state1(2)=state(2)*par(2)
      state1(3)=state(3)*par(9)
      state1(4)=state(4)*par(10)
      state1(5)=state(5)*par(11)
      if(par(5) .eq. 0.) then
       state1(6)=state(6)*(par(1)+par(9))
      else
       state1(6)=state(6)*par(5)
      endif  
c Check state ranges
      call check_range(par,adj,state1)
c Fill initial states from 'fland1' definition
      uztwc=state1(1)
      uzfwc=state1(2)
      lztwc=state1(3)
      lzfsc=state1(4)
      lzfpc=state1(5)
      adimc=state1(6)
      if(adimc .lt. uztwc) adimc=uztwc

      saved=par(15)*(par(11)+par(10))
      parea=1.0-par(4)-par(5)

c initialize frozen ground states and constants
c 03/07 if(iver .eq. 3) then
c 03/07 supm=par(1)+par(2)
c 03/07 slwm=par(9)+par(10)+par(11)
c 03/07 call frdfg0(supm,slwm,state1)
c 03/07 endif 

c  Copy frozen ground parameters from PAR to FGPM (in case of calibratio
c  of frozen ground parameters: FGPM(4), or FGPM(5), or FGPM(6))
      fgpm(4)=par(18)
      fgpm(5)=par(19)
      fgpm(6)=par(20)
c
c initialize
c
      idt=int(dt*24)

      func=-1.

c
c perform operations for each time step
c
       
      do 10 k=1,knt
       edmnd=edmnd0(k)*adj(2)
       pxv=pxv0(k)*adj(1)
       ta=tair0(k)
       sh=sdpt0(k)
       we=swe0(k)
       aesc=sasc0(k)

       call time1(jtime(k),iyr,mon,iday,ihr)
c
c run sac-sma for one time step ---------------------------------------
c
       CALL FLAND1(PAR,DT,PXV,EDMND,TA,WE,AESC,SH,TCI,
     +     IPRINT,IOUT,IDAY,IHR,MON,IYR,k
     +     ,RUZICE,RLZICE,RUZPERC)
       tciVals(k)=TCI
   10 continue 

C writ out time series
      tci_name=TRIM(inputdir)//'/tci.txt'
      call writeTS(idt,tciVals,jtime,knt,tci_name)

C     write out state
      call getarg(5,stateFile)      
      call writeStates(ta,sh,we,stateFile)
      return
      end
c**********************************************************************
      subroutine check_range(par,adj,state)
c**********************************************************************
c
c subroutine performs range check on SAC parameters, adjustment factors
c to precip and pe, empirical UH and SAC states
c version Apr 06, 2003 by D.-J. Seo at NWS/HL
c
      parameter (lx=200)
      dimension par(20),adj(2),state(6)
c
c     parameter checks -- to eliminate impossible values.
c
      ichge=0
      if (par(4).gt.1.0) then
      par(4)=1.0
      ichge=ichge+1
      endif
      if (par(5).gt.(1.0-par(4))) then
      par(5)=1.0-par(4)
      ichge=ichge+1
      endif
      if (par(6).gt.1.0) then
      par(6)=1.0
      ichge=ichge+1                    
      endif
      if (par(17).gt.1.0) then
      par(17)=1.0
      ichge=ichge+1
      endif
c
c     insure that capacities are not equal to 0.0
c
      if (par(1).lt.0.1) par(1)=0.1
      if (par(2).lt.0.1) par(2)=0.1
      if (par(9).lt.0.1) par(9)=0.1
      if (par(10).lt.0.1) par(10)=0.1
      if (par(11).lt.0.1) par(11)=0.1
c
c     parameter checks -- to eliminate impossible values.
c
      if (par(3).gt.1.0) then
      par(3)=1.0
      ichge=ichge+1
      endif
      if (par(12).gt.1.0) then   
      par(12)=1.0
      ichge=ichge+1
      endif
      if (par(13).gt.1.0) then
      par(13)=1.0
      ichge=ichge+1
      endif
      if (par(14).gt.1.0) then
      par(14)=1.0
      ichge=ichge+1
      endif
      if (par(15).gt.1.0) then
      par(15)=1.0
      ichge=ichge+1
      endif
      if (ichge.ne.0) then
      endif                                

      if (state(1).lt.0.) state(1)=0.01
      if (state(2).lt.0.) state(2)=0.01
      if (state(3).lt.0.) state(3)=0.01
      if (state(4).lt.0.) state(4)=0.01
      if (state(5).lt.0.) state(5)=0.01
      if (state(6).lt.0.) state(6)=0.01

      if (state(1).gt.par(1)) state(1)=par(1)
      if (state(2).gt.par(2)) state(2)=par(2)
      if (state(3).gt.par(9)) state(3)=par(9)
      if (state(4).gt.par(10)) state(4)=par(10) 
      if (state(5).gt.par(11)) state(5)=par(11) 
      if (state(6).gt.(par(1)+par(9))) state(6)=par(1)+par(9)
      if (state(6).lt.state(1)) state(6)=state(1)

      return               
      end
c *************************************************************************
      subroutine time1(itime,iyr,mon,iday,ihr)
c**********************************************************************
      iyr=itime/1000000
      mon=itime-iyr*1000000
      mon=mon/10000
      iday=itime-iyr*1000000-mon*10000
      iday=iday/100
      ihr=itime-iyr*1000000-mon*10000-iday*100
      return
      end

