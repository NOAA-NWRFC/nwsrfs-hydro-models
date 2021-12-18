      SUBROUTINE SNEW (TSNEW,SFALL,DPTNEW,DENNEW )
C    
C  CALCULATES DEPTH AND DENSITY OF THE NEW SNOWFALL
C      TSNEW   - AIR TEMPERATURE, C (INPUT)
C      SFALL   - NET NEW SNOWFALL, MM (INPUT)
C      DPTNEW  - SNOW DEPTH, CM (OUTPUT)
C      DENNEW  - SNOW DENSITY (OUTPUT)
C
C ----------------------------------------------------------------------
C ***  CALCULATE NEW SNOWFALL DENSITY DEPENDING ON TEMPERATURE
C ***  EQUATION FROM ANDERSON - NOAA TECH REPORT NWS 19
C ***  BASED ON DATA FROM ALTA, UTAH
C-----------------------------------------------------------------------
C
C  CALCULATE NEW SNOW DENSITY BASED ON TEMPERATURE
      IF(TSNEW.LE.-15.) THEN
        DENNEW=0.05 
      ELSE                                                      
        DENNEW=0.05+0.0017*(TSNEW+15.)**1.5
      ENDIF
C
C  CALCULATE DEPTH OF NEW SNOWFALL      
      DPTNEW=(0.1*SFALL)/DENNEW
      RETURN
      END      
