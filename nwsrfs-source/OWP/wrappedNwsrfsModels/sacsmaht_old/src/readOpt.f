c**********************************************************************
c readOpt reads file $basin_oper.opt
c
      subroutine readOpt(infile,state,ett,idt,par,adj,saved,parea)

	   parameter (lx=200)
       dimension par(20),adj(2),state(6), ett(24)
       real parea, saved
       character*200 infile
       integer idt

       open(55,file=infile,status='old',iostat=ier)
       call pinb1(state,ett,idt,par,adj,saved,parea)
       close(55)
      end
c**********************************************************************
      subroutine pinb1(state,ett,idt,par,adj,saved,parea)
c**********************************************************************
cthis is the input subroutine for the sacramento soil-
c moisture accounting operation.  this subroutine
c inputs all cards for the operation and fills the
c parms, and fsmpm1 common blocks, and passes the initial
c values of the state variables to the calling routine.
c subroutine initially written by. . .
c eric anderson - hrl     march 1979     version 1
c modifications made by bryce finnerty, june, 1995, for
c the purpose of running in stand alone mode, and interacting
c with d.j. seo's distributed modeling precipitation processing
c driver.
      dimension aname(5),pxid(2),roid(2),scid(2),rocid(2),smid(2),
     *peid(2),ndays(12),sname(2),ci(6),ett(24)
      dimension par(20),adj(2),state(6)
      dimension state_frz(6)
      character*4 frzes
      real lztwc,lzfsc,lzfpc
      common/fpmfg1/ itta,fgpm(15),iver,ifrze
      common/fsmco1/ uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,fgco(6),rsm(7),
     +               ppx,ppsc,pta,pwe,psh,tsoil(8),smc(8),sh2o(8)
c     data statements
      data dl,dles/4hl   ,4hdles/
      data prst/4hprst/
      data blank,sums,rocl,smzc/4h    ,4hsums,4hrocl,4hsmzc/
      data frze/4hfrze/
      data sname/4hpin1,4h    /
      data ndays/31,28,31,30,31,30,31,31,30,31,30,31/

      frzes=' '

c  VK  states are changed into relative values
      
c     read input cards and make checks
c
c     input card no. 1.
c
      read (55,901) aname,idt,pxid,pxtype,roid,rotype
  901 format (5a4,8x,i2,2x,2a4,1x,a4,7x,2a4,1x,a4)
c
c     check computational time interval
c
      if (((24/idt)*idt).eq.24) then
c
c     input card no. 2.
c
      read (55,906) scid,sctype,itsc,rocid,smid,store,prot,itroc,itsm,
     1frzes
  906 format (2x,2a4,1x,a4,3x,i2,2x,2a4,2x,2a4,1x,a4,1x,a4,3x,i2,3x,i2,
     11x,a4)
       ifrze=0
       if(iver .ne. 0) then
        if(frzes .eq. ' ') then
         write(*,*) ' ERROR: Frozen ground input card option'
         stop
        else
         ifrze=1
         if(frzes(1:4) .eq. 'FRZE') then
          iver=1
         else 
          iver=3
         endif 
        endif 
       endif 
      else
      write(6,*) 'unacceptable time interval...stop'
      stop
      endif
c
c     input card no. 3
c
      read (55,908) adj(1),adj(2),par(1),par(2),par(3),par(4),par(5),
     1par(6),ioptet,par(17)

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
      read (55,909) par(7),par(8),par(9),par(10),par(11),par(12),
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
      read (55,911) peid,petype,(ett(i),i=1,12)
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
  140 read (55,913) state(1),state(2),state(3),state(4),state(5),
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
C     READ CARD 6A
c  Reads additional carryover staets: runoffsumm, pe, snow cover
      if(irxc .eq. 1) then
       READ (55,915) rsm,ppx,ppsc
      endif
  915 FORMAT(7F5.0,5X,2F10.0)
c
c  read frozen ground information
      if(ifrze .ne. 0) then
       supm=par(1)+par(2)
       slwm=par(9)+par(10)+par(11)
       call frdfg1(idt,supm,slwm,state,par(18),par(19),par(20))
      else
       do ix=1,4
c Skip four records (assumes that input deck is defined for FRZV)
        read(55,'(A)') 
       enddo 
      endif
      
c Recalculate states into ratio of Max_parameters
      state(1)=state(1)/par(1)
      state(2)=state(2)/par(2)
      state(3)=state(3)/par(9)
      state(4)=state(4)/par(10)
      state(5)=state(5)/par(11)
      if(par(5) .eq. 0.) then
       state(6)=state(6)/(par(1)+par(9))
      else
       state(6)=state(6)/par(5)
      endif  
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
