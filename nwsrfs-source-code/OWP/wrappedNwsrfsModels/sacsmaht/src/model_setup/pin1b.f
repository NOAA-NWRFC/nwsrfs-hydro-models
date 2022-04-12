c**********************************************************************
c This is the input subroutine for the sacramento soil-moisture 
c accounting operation.  This subroutine inputs all cards for the
c operation and fills the parms, and fsmpm1 common blocks, and passes
c the initial values of the state variables to the calling routine.
c
c Subroutine initially written by. . .
c Eric Anderson - hrl     march 1979     version 1
c
c Modifications made by Bryce Finnerty, june, 1995, for
c the purpose of running in stand alone mode, and interacting
c with D.J. Seo's distributed modeling precipitation processing
c driver.
c**********************************************************************
c CP 2/19/09 subroutine pinb1(state,ett,idt,par,adj,saved,parea,po)
        
      SUBROUTINE PIN1B(PO)
      
      include 'flogm'

      COMMON/IONUM/IN,IPR,IPU

      common/pass10/par(20),adj(2),state(6)
      common/pass4/knt,dt,nv,saved,parea,knq,idtq,istrt,iend 
      common/pass7/ett(24),idt
      common/finfg0/ nsinp,zs(8),ts(8),fgco0(6),pta0,pwe0,psh0
      common/fpmfg1/ itta,fgpm(15),iver,ifrze,ixrc,nsoillayer
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)

      dimension aname(5),pxid(2),roid(2),scid(2),rocid(2),smid(2),
     *          taid(2),weid(2),fiid(2),shid(2), 
     *          peid(2),ndays(12),sname(2),ci(6),po(27)
      dimension state_frz(6)
      character*4 frzes 
      
      real lztwc,lzfsc,lzfpc
c      
c     data statements
c
      data dl,dles/4hl   ,4hdles/
      data prst/4hprst/
      data blank,sums,rocl,smzc/4h    ,4hsums,4hrocl,4hsmzc/
      data frze/4hfrze/
      data sname/4hpin1,4h    /
      data ndays/31,28,31,30,31,30,31,31,30,31,30,31/

      frzes=' '
      iver = 3

c  VK  states are changed into relative values
      
c     read input cards and make checks
c
c     input card no. 1.
c
      read (IN,901) aname,idt,pxid,pxtype,roid,rotype
  901 format (5a4,8x,i2,2x,2a4,1x,a4,7x,2a4,1x,a4)
c
c     check computational time interval
c
      if (((24/idt)*idt).eq.24) then
c
c     input card no. 2.
c
      read (IN,906) scid,sctype,itsc,rocid,smid,store,prot,itroc,
     1                  itsm,frzes
  906 format (2x,2a4,1x,a4,3x,i2,2x,2a4,2x,2a4,1x,a4,1x,a4,3x,i2,3x,i2,
     11x,a4)
      if(frzes .eq. ' ') then
         iver=1
         ifrze=0
      else
         iver=2
         ifrze=1
      endif
C       ifrze=0
C       if(iver .ne. 0) then
C        if(frzes .eq. ' ') then
C         write(*,*) ' ERROR: Frozen ground input card option'
C         stop
C        else
C         ifrze=1
C         if(frzes(1:4) .eq. 'FRZE') then
C          iver=1
C         else 
C          iver=3
C         endif 
C        endif 
C       endif 
      else
        WRITE(MESSAGESTRING, *)'Unacceptable time interval...stop'
        call logfromfortran(FATAL_LEVEL, MESSAGESTRING)
      endif
c
c     input card no. 3
c
      read (IN,908) adj(1),adj(2),par(1),par(2),par(3),par(4),
     1              par(5),par(6),ioptet,par(17)

  908 format (20x,2f6.3,2f6.1,4f6.3,i5,f6.3)
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
c     input card no. 4
c
      read (IN,909) par(7),par(8),par(9),par(10),par(11),par(12),
     *par(13),par(14),par(15),par(16)
  909 format (20x,2f6.2,3f6.1,5f6.3)
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
c
c     input card no. 5
c
      read (IN,911) peid,petype,(ett(i),i=1,12)
  911 format (2x,2a4,1x,a4,5x,12f7.3)
c
c  bryce re did this et section to fill positions 13-24 with
c  the daily et increment.
c
      iplet=1
      do 138 i=2,13
      nd=ndays(i-1)
      m=iplet+i-1
      k=m-1
      if (i.eq.13) m=iplet
      l=k+12
      ett(l)=(ett(m)-ett(k))/nd
  138 continue
c
c     input card no. 6
c
  140 read (IN,913) state(1),state(2),state(3),state(4),state(5),
     *state(6),irxc
c03/07victor change 913 format (20x,6f6.1,4x,i1)

  913 format (20x,6f10.3,4x,i1)
c
c     computed parameters
c
      saved=par(15)*(par(11)+par(10))
      parea=1.0-par(4)-par(5)
c
c     check state variables for illegitimate values.
c
      ci(1)=state(1)
      ci(2)=state(2)
      ci(3)=state(3)
      ci(4)=state(4)
      ci(5)=state(5)
      ci(6)=state(6)
      l=0
      if (state(1).gt.par(1)) then
      state(1)=par(1)
      l=1
      endif
      if (state(2).gt.par(2)) then
      state(2)=par(2)
      l=1
      endif
      if (state(3).gt.par(9)) then
      state(3)=par(9)
      l=1
      endif
      if (state(4).gt.par(10)) then
      state(4)=par(10)
      l=1
      endif
      if (state(5).gt.par(11)) then
      state(5)=par(11)
      l=1
      endif
      if (state(6).gt.(par(1)+par(9))) then
      state(6)=par(1)+par(9)
      l=1
      endif
      if (state(6).lt.state(1)) then
      state(6)=state(1)
      l=1
      endif

      PPSC=0.0
c
C     READ CARD 6A
c  Reads additional carryover states: runoffsumm, pe, snow cover
c
      if(irxc .eq. 1) then
       READ (IN,915) rsm,ppx,ppsc
      endif
  915 FORMAT(7F5.0,5X,2F10.0)
c
c  read frozen ground information
c
CP02/09 if(ifrze .ne. 0) then
CP02/09   supm=par(1)+par(2)
CP02/09   slwm=par(9)+par(10)+par(11)
CP02/09   call frdfg1(idt,supm,slwm,state,par(18),par(19),par(20))
c
c VK 06/07 Skip frozen ground, but simulate soil moisture
c
CP02/09if(iver .gt. 1) then
c....................................................
c        READ FROZEN GROUND CARD NUMBER 1.
c....................................................
         READ (IN,900) TAID,TATYPE,ITTA,WEID,WETYPE,ITWE,FIID,ITFI
 900     FORMAT (2X,2A4,1X,A4,3X,I2,17X,2A4,1X,A4,3X,I2,2X,2A4,3X,I2)
         IF ((ITTA/IDT)*IDT .NE. ITTA) THEN
           WRITE (MESSAGESTRING,902) TAID,TATYPE,ITTA,IDT
 902       FORMAT (1H0,10X,49H**ERROR** THE TIME INTERVAL OF TIME SERIES
     1 (I.D.=,2A4,2X,5HTYPE=,A4,2X,I2,1X,6HHOURS),/16X,
     139HIS NOT A MUTIPLE OF THE TIME INTERVAL (,I2,1X,
     129HHOURS) OF THE RAIN+MELT DATA.)
           call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
CP 03/09   STOP
         END IF
 
         READ(IN,900) SHID,SHTYPE,ITSH
         IF ((ITSH/IDT)*IDT .NE. ITSH) THEN
           WRITE (MESSAGESTRING,902) SHID,SHTYPE,ITSH,IDT
           call logfromfortran(FATAL_LEVEL,MESSAGESTRING)
CP 03/09   STOP
         END IF 
CP02/09end if 
CP    else
CP     do ix=1,4
c
c Skip four records (assumes that input deck is defined for FRZV)
c
CP      read(IN,'(A)') 
CP     enddo 
CP    endif
c     
c Recalculate states into ratio of Max_parameters
c CP 2/25/09
CP      state(1)=state(1)/par(1)
CP      state(2)=state(2)/par(2)
CP      state(3)=state(3)/par(9)
CP      state(4)=state(4)/par(10)
CP      state(5)=state(5)/par(11)
CP      if(par(5) .eq. 0.) then
CP       state(6)=state(6)/(par(1)+par(9))
CP      else
CP       state(6)=state(6)/par(5)
CP      endif  
c 
c Store information in po array
c CP added 02/09    
      po(1)=idt+0.01      !model time step, input and output
      po(2)=ifrze+0.01    !frozen groun flag
      call rmleadblk8(pxid, pxid)
      po(3)=pxid(1)       !precip rain+melt ts id
      po(4)=pxid(2)    
      call rmleadblk4(pxtype,pxtype)
      po(5)=pxtype        !precip rain+melt ts type
      call rmleadblk8(roid, roid)
      po(6)=roid(1)       !runoff ts id
      po(7)=roid(2)
      call rmleadblk4(rotype,rotype)
      po(8)=rotype        !runoff ts type
      call rmleadblk8(scid, scid)
      po(9)=scid(1)       !aeral snow cover ts id
      po(10)=scid(2)        
      call rmleadblk4(sctype,sctype)
      po(11)=sctype       !aeral snow cover ts type
      po(12)=irxc+0.01    !additional carryover flag
      call rmleadblk8(taid, taid)
      po(13)=taid(1)      !air temperature ts id
      po(14)=taid(2)
      call rmleadblk4(tatype, tatype)
      po(15)=tatype       !air temperature ts type
      call rmleadblk8(weid, weid)
      po(16)=weid(1)      !snow water equivalent ts id
      po(17)=weid(2)
      call rmleadblk4(wetype, wetype)
      po(18)=wetype       !snow water equivalent ts type
      call rmleadblk8(shid, shid)
      po(19)=shid(1)      !snow depth ts id
      po(20)=shid(2)
      call rmleadblk4(shtype, shtype)
      po(21)=shtype       !snow depth ts type
      po(22)=NSINP+0.01   !nsinp - number of soil layer      
      po(23)=iver+0.01
c     po(24)=ndsl         !user-defined soil layer
      call rmleadblk8(peid, peid)
      po(25)=peid(1)      !potential evaporation ts id
      po(26)=peid(2)
      call rmleadblk4(petype, petype) 
      po(27)=petype       !potential evaporation ts type
c--------------------------------------------------------------- 
c
c
c     definition of selected variables.
c
c
c    idt         computational time interval in hours
c    pcx         precipitation multipying factor - applied during the
c                        computational period only.  initially = 1.0
c
c     order for parameter values is...
c                      1. adj(1)     2. adj(2)     3. par(1)     4. par(2)
c                      5. par(3)     6. par(4)     7. par(5)     8. par(6)
c                      9. par(7)    10. par(8)    11. par(9)    12. par(10)
c                     13. par(11)   14. par(12)   15. par(13)   16. par(14)
c                     17. par(15)   18. par(16)   19. diurnal et variation
c                     20. par(17)
c
c     seasonal et curve information is...
c        1-12      seasonal et-demand or pe-adjustment curve (jan-dec)
c        13-24     daily increments in seasonal et-demand or
c                        pe-adjustment curve (first value is for mid
c                        jan. to mid feb.)
c
c        water balance components -- for calibration, order is...
c           1. precip.     2. runoff         3. recharge    4. actual-et
c           5. residual    6. imperivous     7. direct      8. surface
c           9. interflow  10. supp. gw      11. primary    12. et-demand
c          13. et-uz      14. et-lz         15. et-par(5)  16. et-par(6)
      return
      end
