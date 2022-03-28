C MODULE TRAN51
C----------------------------------------------------------------
C
      SUBROUTINE TRAN51 (ENDSTR,NENDSTR)
C
C  ROUTINE TO TRANSFER ALL INPUT FOR SSARRESV OPERATION TO UNIT 89.
C
C  ARGUMENT LIST:
C    ENDSTR - CHARACTER STRING DENOTING END OF TRANSFER
C   NENDSTR - NUMBER OF WORDS IN ENDSTR
C
      COMMON /UIOX/ LP,ICD,LPD,LPE,ICDPUN,LSYS
      COMMON/FLD51/NFLD,ISTRT,LEN,ITYPE,NREP,INTEGR,REAL,
     *   NCHAR,CHAR(25),LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT,
     *   NCARD,USEDUP
      LOGICAL USEDUP
      COMMON/READ51/MUNI51,NCD51,LINFL,LUPB,LSAR,LTRP,LLWB,
     & NINFL,NUPB,NSAR,NTRP,NLWB,LTITLE,LUNITS
C
      CHARACTER*4 ENDSTR(NENDSTR)
      CHARACTER*80 STRNG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_ssarresv/RCS/tran51.f,v $
     . $',                                                             '
     .$Id: tran51.f,v 1.4 2001/06/13 09:59:23 mgm Exp $
     . $' /
C    ===================================================================
C
C
      MUNI51=89
C
       
      REWIND MUNI51
C
      NCD51=0
C
10    READ (ICD,20,END=30) STRNG
20    FORMAT (A)
      WRITE (MUNI51,20) STRNG
      NCD51=NCD51+1
      ISAME=IUSAME(STRNG,ENDSTR,NENDSTR)
      IF (ISAME.NE.1) GO TO 10
      GO TO 40
C
C  NO CARD WITH 'ENDSTR' FOUND ON IT
30    CALL STER51 (21,1)
C
40    RETURN
C
      END
