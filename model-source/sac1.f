      SUBROUTINE SAC1(DT,PXV,EP,TCI,ROIMP,SDRO,SSUR,SIF,BFS,BFP,TET,
C     SAC FROZEN GROUND VARIABLES
     &                IFRZE,TA,LWE,WE,ISC,AESC,
C     SAC PARAMETERS
     &                UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,
     &                REXP,LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE,
     &                SIDE,RSERV,
C     SAC State variables  ',
     &                UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC)


C      IMPLICIT NONE

C.......................................
C     THIS SUBROUTINE EXECUTES THE 'SAC-SMA ' OPERATION FOR ONE TIME
C         PERIOD.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON - HRL     APRIL 1979     VERSION 1
C.......................................

C     RCS Id string, for version control
CCB      CHARACTER(len=60) RCSID
      !DATA RCSID/"$Id: sac1.f,v 1.1 2006/09/01 21:59:44 vicadmin Exp $"/

      REAL DT
      REAL PXV
      REAL EP
      REAL TCI
      REAL ROIMP,SDRO,SSUR,SIF,BFS,BFP,TET

      INTEGER IFRZE, ISC
      REAL TA,LWE,WE,AESC  

      !REAL LZTWM,LZFSM,LZFPM,LZSK,LZPK,LZTWC,LZFSC,LZFPC

      REAL, INTENT(IN)  ::  UZTWM, UZFWM, UZK, PCTIM, ADIMP, RIVA, ZPERC
      REAL, INTENT(IN)  ::  REXP, LZTWM, LZFSM, LZFPM, LZSK, LZPK, PFREE
      REAL, INTENT(IN)  ::  SIDE, RSERV

      REAL, INTENT(INOUT)  ::  UZTWC, UZFWC, LZTWC, LZFSC, LZFPC, ADIMC


C     COMMON BLOCKS
      COMMON/FSMCO1/FGCO(6),RSUM(7),PPE,PSC,PTA,PWE
      COMMON/FSUMS1/SROT,SIMPVT,SRODT,SROST,SINTFT,SGWFP,SGWFS,SRECHT,
     1              SETT,SE1,SE3,SE4,SE5


C      write(*,*) 'pars - ',UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,REXP,
C     1           LZTWM,LZFSM,LZFPM,LZSK,LZPK,PFREE,SIDE,RSERV
C      write(*,*) 'start sac1 - states ', UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,
C     &           ADIMC
C      write(*,*) '           - runoff ', ROIMP,SDRO,SSUR,SIF,BFS,BFP
C      write(*,*) '           - ET ', E1,E2,E3,E4,E5,TET

C.......................................
C     COMPUTE EVAPOTRANSPIRATION LOSS FOR THE TIME INTERVAL.
C        EDMND IS THE ET-DEMAND FOR THE TIME INTERVAL
      EDMND=EP

C
C     COMPUTE ET FROM UPPER ZONE.
      E1=EDMND*(UZTWC/UZTWM)
      RED=EDMND-E1
C     RED IS RESIDUAL EVAP DEMAND
      UZTWC=UZTWC-E1
      E2=0.0
      IF(UZTWC.GE.0.) GO TO 220
C     E1 CAN NOT EXCEED UZTWC
      E1=E1+UZTWC
      UZTWC=0.0
      RED=EDMND-E1
      IF(UZFWC.GE.RED) GO TO 221
C     E2 IS EVAP FROM UZFWC.
      E2=UZFWC
      UZFWC=0.0
      RED=RED-E2
      GO TO 225
  221 E2=RED
      UZFWC=UZFWC-E2
      RED=0.0
  220 IF((UZTWC/UZTWM).GE.(UZFWC/UZFWM)) GO TO 225
C     UPPER ZONE FREE WATER RATIO EXCEEDS UPPER ZONE
C     TENSION WATER RATIO, THUS TRANSFER FREE WATER TO TENSION
      UZRAT=(UZTWC+UZFWC)/(UZTWM+UZFWM)
      UZTWC=UZTWM*UZRAT
      UZFWC=UZFWM*UZRAT
  225 IF (UZTWC.LT.0.00001) UZTWC=0.0
      IF (UZFWC.LT.0.00001) UZFWC=0.0
C
C     COMPUTE ET FROM THE LOWER ZONE.
C     COMPUTE ET FROM LZTWC (E3)
      E3=RED*(LZTWC/(UZTWM+LZTWM))
      LZTWC=LZTWC-E3
      IF(LZTWC.GE.0.0) GO TO 226
C     E3 CAN NOT EXCEED LZTWC
      E3=E3+LZTWC
      LZTWC=0.0
  226 RATLZT=LZTWC/LZTWM
CC+
CC+   INFERRED PARAMETER (ADDED BY Q DUAN ON 3/6/95)
      SAVED = RSERV * (LZFPM + LZFSM)
      RATLZ=(LZTWC+LZFPC+LZFSC-SAVED)/(LZTWM+LZFPM+LZFSM-SAVED)
      IF(RATLZT.GE.RATLZ) GO TO 230
C     RESUPPLY LOWER ZONE TENSION WATER FROM LOWER
C     ZONE FREE WATER IF MORE WATER AVAILABLE THERE.
      DEL=(RATLZ-RATLZT)*LZTWM
C     TRANSFER FROM LZFSC TO LZTWC.
      LZTWC=LZTWC+DEL
      LZFSC=LZFSC-DEL
      IF(LZFSC.GE.0.0) GO TO 230
C     IF TRANSFER EXCEEDS LZFSC THEN REMAINDER COMES FROM LZFPC
      LZFPC=LZFPC+LZFSC
      LZFSC=0.0
  230 IF (LZTWC.LT.0.00001) LZTWC=0.0
C
C     COMPUTE ET FROM ADIMP AREA.-E5
      E5=E1+(RED+E2)*((ADIMC-E1-UZTWC)/(UZTWM+LZTWM))
C      ADJUST ADIMC,ADDITIONAL IMPERVIOUS AREA STORAGE, FOR EVAPORATION.
      ADIMC=ADIMC-E5
      IF(ADIMC.GE.0.0) GO TO 231
C     E5 CAN NOT EXCEED ADIMC.
      E5=E5+ADIMC
      ADIMC=0.0
  231 E5=E5*ADIMP
C     E5 IS ET FROM THE AREA ADIMP.
C.......................................
C     COMPUTE PERCOLATION AND RUNOFF AMOUNTS.
      TWX=PXV+UZTWC-UZTWM
C     TWX IS THE TIME INTERVAL AVAILABLE MOISTURE IN EXCESS
C     OF UZTW REQUIREMENTS.
      IF(TWX.GE.0.0) GO TO 232
C     ALL MOISTURE HELD IN UZTW--NO EXCESS.
      UZTWC=UZTWC+PXV
      TWX=0.0
      GO TO 233
C      MOISTURE AVAILABLE IN EXCESS OF UZTW STORAGE.
  232 UZTWC=UZTWM
  233 ADIMC=ADIMC+PXV-TWX
C
C     COMPUTE IMPERVIOUS AREA RUNOFF.
      ROIMP=PXV*PCTIM
C      ROIMP IS RUNOFF FROM THE MINIMUM IMPERVIOUS AREA.
      SIMPVT=SIMPVT+ROIMP
C
C     INITIALIZE TIME INTERVAL SUMS.
      SBF=0.0
      SSUR=0.0
      SIF=0.0
      SPERC=0.0
      SDRO=0.0
      SPBF=0.0

C
C     DETERMINE COMPUTATIONAL TIME INCREMENTS FOR THE BASIC TIME
C     INTERVAL
      NINC=int(1.0+0.2*(UZFWC+TWX))
C     NINC=NUMBER OF TIME INCREMENTS THAT THE TIME INTERVAL
C     IS DIVIDED INTO FOR FURTHER
C     SOIL-MOISTURE ACCOUNTING.  NO ONE INCREMENT
C     WILL EXCEED 5.0 MILLIMETERS OF UZFWC+PAV
      DINC=(1.0/NINC)*DT
C     DINC=LENGTH OF EACH INCREMENT IN DAYS.
      PINC=TWX/NINC

C     PINC=AMOUNT OF AVAILABLE MOISTURE FOR EACH INCREMENT.
C      COMPUTE FREE WATER DEPLETION FRACTIONS FOR
C     THE TIME INCREMENT BEING USED-BASIC DEPLETIONS
C      ARE FOR ONE DAY
      DUZ=1.0-((1.0-UZK)**DINC)
      DLZP=1.0-((1.0-LZPK)**DINC)
      DLZS=1.0-((1.0-LZSK)**DINC)
CC+
CC+   INFERRED PARAMETER (ADDED BY Q DUAN ON 3/6/95)
      PAREA = 1.0 - ADIMP - PCTIM
C.......................................
C     START INCREMENTAL DO LOOP FOR THE TIME INTERVAL.
C.......................................
      DO 240 I=1,NINC

      ADSUR=0.0
C     COMPUTE DIRECT RUNOFF (FROM ADIMP AREA).
      RATIO=(ADIMC-UZTWC)/LZTWM
C      WRITE(*,*) 'ADIMC ', ADIMC, UZTWC, LZTWM
      IF (RATIO.LT.0.0) RATIO=0.0
      ADDRO=PINC*(RATIO**2)
C      WRITE(*,*) 'ADDRO = ', ADDRO, 'PINK = ', PINK, RATIO
C     ADDRO IS THE AMOUNT OF DIRECT RUNOFF FROM THE AREA ADIMP.
C
C     COMPUTE BASEFLOW AND KEEP TRACK OF TIME INTERVAL SUM.
      BF=LZFPC*DLZP
      LZFPC=LZFPC-BF
      IF (LZFPC.GT.0.0001) GO TO 234
      BF=BF+LZFPC
      LZFPC=0.0
  234 SBF=SBF+BF
      SPBF=SPBF+BF
      BF=LZFSC*DLZS
      LZFSC=LZFSC-BF
      IF(LZFSC.GT.0.0001) GO TO 235
      BF=BF+LZFSC
      LZFSC=0.0
  235 SBF=SBF+BF

C
C      COMPUTE PERCOLATION-IF NO WATER AVAILABLE THEN SKIP
      IF((PINC+UZFWC).GT.0.01) GO TO 251
      UZFWC=UZFWC+PINC
      GO TO 249
  251 PERCM=LZFPM*DLZP+LZFSM*DLZS
      PERC=PERCM*(UZFWC/UZFWM)
      DEFR=1.0-((LZTWC+LZFPC+LZFSC)/(LZTWM+LZFPM+LZFSM))
C     DEFR IS THE LOWER ZONE MOISTURE DEFICIENCY RATIO
      FR=1.0
C     FR IS THE CHANGE IN PERCOLATION WITHDRAWAL DUE TO FROZEN GROUND.
      FI=1.0
C     FI IS THE CHANGE IN INTERFLOW WITHDRAWAL DUE TO FROZEN GROUND.
      IF (IFRZE.EQ.0) GO TO 239
      UZDEFR=1.0-((UZTWC+UZFWC)/(UZTWM+UZFWM))
      CALL FGFR1(DEFR,FR,FI)
  239 PERC=PERC*(1.0+ZPERC*(DEFR**REXP))*FR
C     NOTE...PERCOLATION OCCURS FROM UZFWC BEFORE PAV IS ADDED.
      IF(PERC.LT.UZFWC) GO TO 241
C      PERCOLATION RATE EXCEEDS UZFWC.
      PERC=UZFWC
C     PERCOLATION RATE IS LESS THAN UZFWC.
  241 UZFWC=UZFWC-PERC
C     CHECK TO SEE IF PERCOLATION EXCEEDS LOWER ZONE DEFICIENCY.
      CHECK=LZTWC+LZFPC+LZFSC+PERC-LZTWM-LZFPM-LZFSM
      IF(CHECK.LE.0.0) GO TO 242
      PERC=PERC-CHECK
      UZFWC=UZFWC+CHECK
  242 SPERC=SPERC+PERC
C     SPERC IS THE TIME INTERVAL SUMMATION OF PERC
C
C     COMPUTE INTERFLOW AND KEEP TRACK OF TIME INTERVAL SUM.
C     NOTE...PINC HAS NOT YET BEEN ADDED
      DEL=UZFWC*DUZ*FI
      SIF=SIF+DEL
      UZFWC=UZFWC-DEL
C     DISTRIBE PERCOLATED WATER INTO THE LOWER ZONES
C     TENSION WATER MUST BE FILLED FIRST EXCEPT FOR THE PFREE AREA.
C     PERCT IS PERCOLATION TO TENSION WATER AND PERCF IS PERCOLATION
C         GOING TO FREE WATER.
      PERCT=PERC*(1.0-PFREE)
      IF ((PERCT+LZTWC).GT.LZTWM) GO TO 243
      LZTWC=LZTWC+PERCT
      PERCF=0.0
      GO TO 244
  243 PERCF=PERCT+LZTWC-LZTWM
      LZTWC=LZTWM
C
C      DISTRIBUTE PERCOLATION IN EXCESS OF TENSION
C      REQUIREMENTS AMONG THE FREE WATER STORAGES.
  244 PERCF=PERCF+PERC*PFREE
      IF(PERCF.EQ.0.0) GO TO 245
      HPL=LZFPM/(LZFPM+LZFSM)
C     HPL IS THE RELATIVE SIZE OF THE PRIMARY STORAGE
C     AS COMPARED WITH TOTAL LOWER ZONE FREE WATER STORAGE.
      RATLP=LZFPC/LZFPM
      RATLS=LZFSC/LZFSM
C     RATLP AND RATLS ARE CONTENT TO CAPACITY RATIOS, OR
C     IN OTHER WORDS, THE RELATIVE FULLNESS OF EACH STORAGE
      FRACP=(HPL*2.0*(1.0-RATLP))/((1.0-RATLP)+(1.0-RATLS))
C     FRACP IS THE FRACTION GOING TO PRIMARY.
      IF (FRACP.GT.1.0) FRACP=1.0
      PERCP=PERCF*FRACP
      PERCS=PERCF-PERCP
C     PERCP AND PERCS ARE THE AMOUNT OF THE EXCESS
C     PERCOLATION GOING TO PRIMARY AND SUPPLEMENTAL
C      STORGES,RESPECTIVELY.
      LZFSC=LZFSC+PERCS
      IF(LZFSC.LE.LZFSM) GO TO 246
      PERCS=PERCS-LZFSC+LZFSM
      LZFSC=LZFSM
  246 LZFPC=LZFPC+(PERCF-PERCS)
C     CHECK TO MAKE SURE LZFPC DOES NOT EXCEED LZFPM.
      IF (LZFPC.LE.LZFPM) GO TO 245
      EXCESS=LZFPC-LZFPM
      LZTWC=LZTWC+EXCESS
      LZFPC=LZFPM
C
C     DISTRIBUTE PINC BETWEEN UZFWC AND SURFACE RUNOFF.
  245 IF(PINC.EQ.0.0) GO TO 249
C     CHECK IF PINC EXCEEDS UZFWM
      IF((PINC+UZFWC).GT.UZFWM) GO TO 248
C     NO SURFACE RUNOFF
      UZFWC=UZFWC+PINC
      GO TO 249
C
C     COMPUTE SURFACE RUNOFF (SUR) AND KEEP TRACK OF TIME INTERVAL SUM.
  248 SUR=PINC+UZFWC-UZFWM
      UZFWC=UZFWM
      SSUR=SSUR+SUR*PAREA
      ADSUR=SUR*(1.0-ADDRO/PINC)
C     ADSUR IS THE AMOUNT OF SURFACE RUNOFF WHICH COMES
C     FROM THAT PORTION OF ADIMP WHICH IS NOT
C     CURRENTLY GENERATING DIRECT RUNOFF.  ADDRO/PINC
C     IS THE FRACTION OF ADIMP CURRENTLY GENERATING
C     DIRECT RUNOFF.
      SSUR=SSUR+ADSUR*ADIMP
C
C     ADIMP AREA WATER BALANCE -- SDRO IS THE 6 HR SUM OF
C          DIRECT RUNOFF.
  249 ADIMC=ADIMC+PINC-ADDRO-ADSUR
      IF (ADIMC.LE.(UZTWM+LZTWM)) GO TO 247
      ADDRO=ADDRO+ADIMC-(UZTWM+LZTWM)
      ADIMC=UZTWM+LZTWM
  247 SDRO=SDRO+ADDRO*ADIMP
C      WRITE(*,*) SDRO, ADDRO, ADIMP
      IF (ADIMC.LT.0.00001) ADIMC=0.0
  240 CONTINUE
C.......................................
C     END OF INCREMENTAL DO LOOP.
C.......................................
C     COMPUTE SUMS AND ADJUST RUNOFF AMOUNTS BY THE AREA OVER
C     WHICH THEY ARE GENERATED.
      EUSED=E1+E2+E3
C     EUSED IS THE ET FROM PAREA WHICH IS 1.0-ADIMP-PCTIM
      SIF=SIF*PAREA
C
C     SEPARATE CHANNEL COMPONENT OF BASEFLOW
C     FROM THE NON-CHANNEL COMPONENT
      TBF=SBF*PAREA
C     TBF IS TOTAL BASEFLOW
      BFCC=TBF*(1.0/(1.0+SIDE))
C     BFCC IS BASEFLOW, CHANNEL COMPONENT
      BFP=SPBF*PAREA/(1.0+SIDE)
      BFS=BFCC-BFP
      IF(BFS.LT.0.0)BFS=0.0
      BFNCC=TBF-BFCC
C     BFNCC IS BASEFLOW,NON-CHANNEL COMPONENT
C
C     ADD TO MONTHLY SUMS.
      SINTFT=SINTFT+SIF
      SGWFP=SGWFP+BFP
      SGWFS=SGWFS+BFS
      SRECHT=SRECHT+BFNCC
      SROST=SROST+SSUR
      SRODT=SRODT+SDRO
C
C     COMPUTE TOTAL CHANNEL INFLOW FOR THE TIME INTERVAL.
      TCI=ROIMP+SDRO+SSUR+SIF+BFCC
C
C     COMPUTE E4-ET FROM RIPARIAN VEGETATION.
      E4=(EDMND-EUSED)*RIVA
C
C     SUBTRACT E4 FROM CHANNEL INFLOW
      TCI=TCI-E4
      IF(TCI.GE.0.0) GO TO 250
      E4=E4+TCI
      TCI=0.0
  250 SROT=SROT+TCI
C
C     COMPUTE TOTAL EVAPOTRANSPIRATION-TET
      EUSED=EUSED*PAREA
      TET=EUSED+E5+E4
      SETT=SETT+TET
      SE1=SE1+E1*PAREA
      SE3=SE3+E3*PAREA
      SE4=SE4+E4
      SE5=SE5+E5
C     CHECK THAT ADIMC.GE.UZTWC
      IF (ADIMC.LT.UZTWC) ADIMC=UZTWC

C      write(*,*) 'end sac1 - states ', UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,
C     &           ADIMC
C      write(*,*) '           - runoff ', ROIMP,SDRO,SSUR,SIF,BFS,BFP
C      write(*,*) '           - ET ', E1,E2,E3,E4,E5,TET

C
C     COMPUTE NEW FROST INDEX AND MOISTURE TRANSFER.
      IF (IFRZE.GT.0) CALL FROST1(PXV,SSUR,SDRO,TA,LWE,WE,ISC,AESC,DT)
C
C     ADD TO SUMS OF RUNOFF COMPONENTS.
      RSUM(1)=RSUM(1)+TCI
      RSUM(2)=RSUM(2)+ROIMP
      RSUM(3)=RSUM(3)+SDRO
      RSUM(4)=RSUM(4)+SSUR
      RSUM(5)=RSUM(5)+SIF
      RSUM(6)=RSUM(6)+BFS
      RSUM(7)=RSUM(7)+BFP
C.......................................

C      WRITE(*,*) 'sac1.for ', DT,PXV,EP,TCI,ROIMP,SDRO,SSUR,SIF,BFS,BFP,TET,
C     &                IFRZE,TA,LWE,WE,ISC,AESC

      RETURN
      END
C
C====================================================================
C
      SUBROUTINE FGFR1(LZDEFR,FR,FI)
C.......................................
C     THIS SUBROUTINE COMPUTES THE CHANGE IN THE PERCOLATION AND
C        INTERFLOW WITHDRAWAL RATES DUE TO FROZEN GROUND.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON - HRL   JUNE 1980
C.......................................
      REAL LZDEFR,LZTWC,LZFSC,LZFPC
C
C     COMMON BLOCKS
      COMMON/SACSTAT1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC
      COMMON/FSMCO1/FGCO(6),RSUM(7),PPE,PSC,PTA,PWE
      COMMON/FPMFG1/FGPM(10)
C.......................................
C     INITIAL VALUES
      FINDX=FGCO(1)
      FRTEMP=FGPM(5)
      SATR=FGPM(6)
      FREXP=FGPM(7)
C.......................................
C     DETERMINE IF FROZEN GROUND EFFECT EXISTS.
      IF (FINDX.LT.FRTEMP) GO TO 100
      RETURN
C.......................................
C     COMPUTE SATURATED REDUCTION.
  100 EXP=FRTEMP-FINDX
      FSAT=(1.0-SATR)**EXP
C     CHANGE AT DRY CONDITIONS
      FDRY=1.0
C     COMPUTE ACTUAL CHANGE
      IF (LZDEFR.GT.0.0) GO TO 101
      FR=FSAT
      FI=FR
      RETURN
  101 FR=FSAT+(FDRY-FSAT)*(LZDEFR**FREXP)
      FI=FR
C.......................................
      RETURN
      END
C
C====================================================================
C
      SUBROUTINE FROST1(PX,SUR,DIR,TA,LWE,WE,ISC,AESC,DT)
C.......................................
C     THIS SUBROUTINE COMPUTES THE CHANGE IN THE FROZEN GROUND
C        INDEX AND MOISTURE MOVEMENT DUE TO TEMPERATURE GRADIENTS.
C.......................................
C     WRITTEN BY ERIC ANDERSON - HRL   JUNE 1980
C.......................................
      REAL LZTWM,LZFSM,LZFPM,LZSK,LZPK,LZTWC,LZFSC,LZFPC
      REAL LWE
C
C     COMMON BLOCKS
      COMMON/SACPARM1/UZTWM,UZFWM,UZK,PCTIM,ADIMP,RIVA,ZPERC,REXP,LZTWM,
     1                LZFSM,LZFPM,LZSK,LZPK,PFREE,SIDE,RSERV
      COMMON/SACSTAT1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC
      COMMON/FSMCO1/FGCO(6),RSUM(7),PPE,PSC,PTA,PWE
      COMMON/FPMFG1/FGPM(10)
C.......................................
C     INITIAL VALUES
      FINDX=FGCO(1)
      FINDX1=FINDX
      CSOIL=4.0*DT*FGPM(1)
      CSNOW=FGPM(2)
      GHC=FGPM(3)*DT
      RTHAW=FGPM(4)
C.......................................
C     COMPUTE MOISTURE MOVEMENT
C        EQUATIONS NOT READY YET.
C.......................................
C     COMPUTE CHANGE IN FROZEN GROUND INDEX.
C     CHANGE DUE TO WATER FREZING IN THE SOIL.
      IF (FINDX.GE.0.0) GO TO 120
      WATER=PX-SUR-DIR
      IF (WATER.LE.0.0) GO TO 120
      FINDX=FINDX+RTHAW*WATER
      IF (FINDX.GT.0.0) FINDX=0.0
C.......................................
C     CHANGE DUE TO TEMPERATURE.
  120 IF ((FINDX.GE.0.0).AND.(TA.GE.0.0)) GO TO 190
C
C     COMPUTE TRANSFER COEFFIENT.
      IF (LWE.EQ.0) GO TO 124
      IF (WE.EQ.0.0) GO TO 124
      IF (ISC.GT.0) GO TO 121
      COVER=1.0
      GO TO 122
  121 COVER=AESC
      IF (COVER.EQ.0.0) GO TO 124
  122 TWE=WE/COVER
      C=CSOIL*(1.0-COVER)+CSOIL*((1.0-CSNOW)**TWE)*COVER
      GO TO 125
  124 C=CSOIL
C
C     COMPUTE CHANGE IN FROST INDEX.
  125 IF (TA.GE.0.0) GO TO 126
      CFI=-C*SQRT(TA*TA+FINDX*FINDX)-C*FINDX+GHC
      FINDX=FINDX+CFI
      GO TO 190
  126 FINDX=FINDX+C*TA+GHC
C.......................................
C     CHECK FROST INDEX
  190 IF (FINDX.LT.0.0) GO TO 195
      FINDX=0.0
      GO TO 199
C.......................................
  195 CONTINUE
C.......................................
C     SAVE NEW FROST INDEX
  199 FGCO(1)=FINDX
C.......................................

      RETURN
      END
