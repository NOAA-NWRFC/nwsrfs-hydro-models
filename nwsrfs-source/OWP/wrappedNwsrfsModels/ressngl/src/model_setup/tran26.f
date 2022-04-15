C MODULE TRAN26
C-----------------------------------------------------------------------
C
C  ROUTINE TO TRANSFER ALL INPUT FOR RES-SNGL OPERATION TO UNIT 89.
C
      SUBROUTINE TRAN26 (ENDSTR,NENDSTR)
C
C  ARGUMENT LIST:
C    ENDSTR - CHARACTER STRING DENOTING END OF TRANSFER
C   NENDSTR - NUMBER OF WORDS IN ENDSTR
C
      CHARACTER*4 ENDSTR(NENDSTR)
      CHARACTER*32 FILNAM
      CHARACTER*80 STRNG
C
      INCLUDE 'upvrsx'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fld26'
      INCLUDE 'common/read26'
      COMMON /UIOX/ LP,ICD,LPD,LPE,ICDPUN,LSYS
C
      SAVE I89
      DATA I89/-1/
      DATA LSYS/3/
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/tran26.f,v $
     . $',                                                             '
     .$Id: tran26.f,v 1.3 2001/06/13 09:51:26 mgm Exp $
     . $' /
C    ===================================================================
C
C
      MUNI26=89
      ICD = IN
C LC comment out the code below; instead do like tran51.f (SSARRESV)
C this requires opening a unit=89 file upfront which is done 
C in the same routine that opens the DECK file
C      PGMNAM='MCP3'
C      IERR = 0 
C
C      IF (PGMNAM(1:4).EQ.'MCP3'.OR.PGMNAM(1:4).EQ.'OPT3') THEN
C         IF (I89.EQ.-1) THEN
C             FILNAM=''
C            CALL UPOPEN (MUNI26,FILNAM,0,'F',IERR)
C             I89=0
C           ENDIF
C         ENDIF
C
      REWIND MUNI26
C
      NCD26=0
C
10    READ (ICD,20,END=30) STRNG
      WRITE (MUNI26,20) STRNG
20    FORMAT (A)
      NCD26=NCD26+1
      ISAME=IUSAME(STRNG,ENDSTR,NENDSTR)
      IF (ISAME.NE.1) GO TO 10
      GO TO 40
C
C  NO CARD WITH 'ENDSTR' FOUND ON IT
30    CALL STER26 (21,1)
C
40    RETURN
C
      END
