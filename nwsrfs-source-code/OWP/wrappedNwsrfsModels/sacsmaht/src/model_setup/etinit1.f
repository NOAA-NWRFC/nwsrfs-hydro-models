      subroutine etinit1(po, raim, sasc, mat, swe, snsg, mape,
     >                   datetime, periodts)
   
      parameter (kx = 85000)
      real raim(*),mat(*),snsg(*),sasc(*),swe(*),mape(*)
      integer datetime(kx), periodts   
      common/fpmfg1/ itta,fgpm(15),iver,ifrze,ixrc,nsoillayer
      common/pass1/pxv0(kx),edmnd0(kx),qobs0(kx),jtime(kx),
     +             tair0(kx),sdpt0(kx),swe0(kx),sasc0(kx)
      common/pass4/knt,dt,nv,saved,parea,knq,idtq,istrt,iend
      common/pass7/ett(24),idt
      dimension epdist(24), po(27)
      character petype*4
      data dl,dles/4hl   ,4hdles/
      data prst/4hprst/
      data blank,sums,rocl,smzc/4h    ,4hsums,4hrocl,4hsmzc/
      data frze/4hfrze/

c
c Initialize
c
cv    istart=0
      knt = periodts              !CP added 02/19/09

      iSavedDate = -999
      koff = 1                    !CP added 03/24/09

      do 10 k=1,knt
       jtime(k)=datetime(k)       !CP copy datetime pass from C function 
       jyr=datetime(k)/1000000
       jx=datetime(k)-jyr*1000000
       jmon=jx/10000
       jx=jx-jmon*10000
       jday=jx/100
       ihour=jx-jday*100
       
cv     if(istart.eq.0.or.ihour.eq.24) then
       if(((iSavedDate .NE. jday) .AND. (ihour.NE.0)).OR.
     +    (iSavedDate .EQ. -999)) then
c----------------------------------------------------------------------
c  Define daily et-distribution for internal time
c  uniform daily et distribution
        mv=24/idt
        v=1.0/mv
        do 107 i=1,mv
         epdist(i)=v
107     continue

c   Initial et-demand or pe-adjustment
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
c
c No daily PE input - Get ET demand from seasonal curve.
c
        epuadj=et
c
c If daily PE time series (MAPE) avaiable then values are PE-adjustment.
c CP 03/24/09
c
        if ( petype .eq. 'MAPE' ) then
          epuadj = mape(koff)

          if (epuadj .lt. 0.00001 ) epuadj = 0.0
          epuadj = epuadj * et
          koff = koff + 1  
        end if

c       ep=epuadj*peadj
        ep=epuadj
        spet=spet+ep !!!!!!!!!!!!!!!!!! UNUSED?????
        etd=ep

        iSavedDate = jday
c----------------------------------------------------------------------
       endif
       
c     Compute evapotranspiration loss for the time interval.
c     edmnd is the et-demand for the time interval
cp    edmnd=etd*epdist(1)
      edmnd=etd*epdist(kint)
      edmnd0(k)=edmnd
c
c Copy input time series data to common pass1
c CP added 02/19/09
       pxv0(k)=raim(k)
c
c Initilze input time serires if run for soil moisture only
c
       tair0(k)=10.
       sdpt0(k)=0.
       sdpt0(k)=0.
       sasc0(k)=0.
c
c If iver > 0, use optional input time series
c
       if ( iver .gt. 1 ) then
         tair0(k)=mat(k)
         sdpt0(k)=snsg(k)
         sasc0(k)=sasc(k)
         swe0(k)=swe(k)
       end if
c      
   10 continue

      return 
      end
