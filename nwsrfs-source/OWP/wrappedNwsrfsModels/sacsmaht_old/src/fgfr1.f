C MEMBER FGFR1
C  (from old member FCFLAND1)
C
      SUBROUTINE FGFR1(LZDEFR,FR,UZDEFR,FI)
C.......................................
C     THIS SUBROUTINE COMPUTES THE CHANGE IN THE PERCOLATION AND
C        INTERFLOW WITHDRAWAL RATES DUE TO FROZEN GROUND.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON - HRL   JUNE 1980
C.......................................
      REAL LZDEFR,LZTWC,LZFSC,LZFPC
C
C     COMMON BLOCKS
      COMMON/FSMCO1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO(6),RSUM(7),
     1PPE,PSC,PTA,PWE

CVK  FROZEN GROUND PARAMETER LENGTH IS INCREASED
CVK      COMMON/FPMFG1/FGPM(10)
      COMMON/FPMFG1/itta,FGPM(15),ivers,ifrze      
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /usr/apps/nwsrfs/ofs/src/fcst_sac/RCS/fgfr1.f,v $
     . $',                                                             '
     .$Id$
     . $' /
C    ===================================================================
C
C.......................................
C     INITIAL VALUES
      FINDX=FGCO(1)

CVK  ADDED VERSION TO USE A NEW FROZEN GROUND INDEX
      dfrs=lzdefr
      IF(IVERS .EQ. 1) THEN       
       FRTEMP=FGPM(5)
       SATR=FGPM(6)
       FREXP=FGPM(7)
      ELSE
cc       lzdefr=0.
       FRTEMP=FGPM(5)
       SATR=FGPM(6)
       FREXP=FGPM(4)
      ENDIF
        
C.......................................
C     DETERMINE IF FROZEN GROUND EFFECT EXISTS.
      IF (FINDX.LT.FRTEMP) GO TO 100
       lzdefr=dfrs
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
       lzdefr=dfrs
      RETURN
  101 FR=FSAT+(FDRY-FSAT)*(LZDEFR**FREXP)
      FI=FR
C.......................................
      RETURN
      END
