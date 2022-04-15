      SUBROUTINE  CO1026()
 
      INCLUDE 'common/ionum'
      INCLUDE 'flogm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_stubs/RCS/co1026.f,v $
     . $',                                                             '
     .$Id: co1026.f,v 1.1 1995/09/17 18:53:34 dws Exp $
     . $' /
C    ===================================================================
C

!CP     WRITE(IPR,100)
        WRITE(MESSAGESTRING, 100)
        call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
  100   FORMAT(' >>>>> SUBROUTINE  CO1026  ENTERED - ',
     $         'CURRENTLY STUBBED OFF')
 
      RETURN
      END
