
      SUBROUTINE initCommonBlocksPin(FILENAME)

      CHARACTER(LEN=1024) FILENAME
      COMMON/IONUM/IN,IPR,IPU
      COMMON /FATLGK/ IATL,C1,C2
      

      IN = 1
      IPR = 0
      IPU = 0
      
      FILENAME = ''
C      open(IN,file=TRIM(FILENAME),status='old')
      

      RETURN
      END
