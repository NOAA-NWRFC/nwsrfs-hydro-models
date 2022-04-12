C MODULE ERROR
C-----------------------------------------------------------------------
C
      SUBROUTINE ERROR
C
C JUST EXIT (THE ACTUAL REASON FOR THE EXIT WOULD HAVE BEEN LOGGED IN THE CALLER)
	include 'flogm'

	CALL EXIT(1)
      RETURN
C
      END
