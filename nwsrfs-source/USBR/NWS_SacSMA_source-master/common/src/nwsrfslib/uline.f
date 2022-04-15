C MEMBER ULINE
C-----------------------------------------------------------------------
C
      SUBROUTINE ULINE (NUNIT,LINES)
C
C  ROUTINE TO COUNT LINES PRINTED TO THE SPECIFIED UNIT.
C  IF THE SPECIFIED LINES WILL NOT FIT ON THE PAGE, A NEW PAGE
C  IS STARTED.
C
      COMMON /UCMDBX/ ICMPRU,ICMTRC,ICMDBG
      COMMON /UPAGEX/ PUSRID,IPSPAG(99),IPSLIN(99),
     *   NPSPAG(99),NPSMLN(99),NPSNLN(99),NPSNLT(99),IPSNWP(99)
      CHARACTER*8 PUSRID
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/uline.f,v $
     . $',                                                             '
     .$Id: uline.f,v 1.1 1995/09/17 19:05:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IUNIT=IABS(NUNIT)
C
C  CHECK IF PAGE HEADER HAS BEEN PRINTED
      IF (ICMDBG.GT.2)
     *   WRITE (ICMPRU,20) IUNIT,NPSPAG(IUNIT)
C
C LC commented out call to upage; we are not
C using it
C  CHECK IF PAGE HEADER HAS BEEN PRINTED
C     IF (NPSPAG(IUNIT).EQ.0) CALL UPAGE (NUNIT)
C
C  CHECK IF LINES TO BE COUNTED
      IF (IPSLIN(IUNIT).EQ.0) GO TO 10
C
C  CHECK IF SPECIFIED LINES WILL FIT ON PAGE
      IF (ICMDBG.GT.2)
     *   WRITE (ICMPRU,30) IUNIT,NPSNLN(IUNIT),LINES,NPSMLN(IUNIT)
C LC commented out call to UPAGE; we are not using
C it
C      IF (NPSMLN(IUNIT).GT.0.AND.
C     *    (NPSNLN(IUNIT)+LINES.GT.NPSMLN(IUNIT))) CALL UPAGE (NUNIT)
C
C  UPDATE LINE COUNTERS
      NPSNLN(IUNIT)=NPSNLN(IUNIT)+LINES
      NPSNLT(IUNIT)=NPSNLT(IUNIT)+LINES
C
C  RESET TOP OF PAGE INDICATOR
      IPSNWP(NUNIT)=0
C
10    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' IN ULINE - IUNIT=',I2,3X,'NPSPAG(IUNIT)=',I2)
30    FORMAT (' IN ULINE - IUNIT=',I2,3X,'NPSNLN(IUNIT)=',I2,3X,
     *   'LINES=',I2,3X,'NPSMLN(IUNIT)=',I2)
C
      END
