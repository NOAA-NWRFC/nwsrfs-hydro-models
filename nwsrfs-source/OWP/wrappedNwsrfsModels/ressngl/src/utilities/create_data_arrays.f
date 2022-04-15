      SUBROUTINE create_data_arrays( PO, DATAIDS,DATATYPES,
     1                               DATATIMESTEPS, DATAINOROUT,
     2                               TOTIO )

      INCLUDE 'flogm'

      PARAMETER (maxInputTs=15, maxOutputTs=21)      
      PARAMETER (IOsize=maxInputTs+maxOutputTs) 
   
      INTEGER NUMTS, BEGTSIDX, INOROUT, CURRTIMESTEP
      INTEGER DATATIMESTEPS(IOsize), DATAINOROUT(IOsize)
      INTEGER TOTIO

      CHARACTER*8 CURRID,BLANKS
      CHARACTER*4 CURRDATATYPE
      CHARACTER*4 DATATYPES(IOsize)
      CHARACTER*8 DATAIDS(IOsize)

      DATA BLANKS/8H        /
     
      REAL PO(500000)
C
C Determine lowest elevation
C
      LOCPEL = PO(10) + 1
C       
C Beginning time series index at word 11
C
      BEGTSIDX = PO(11)  
C
C Number of Time Series and initialize total time step 
C
      NUMTS = PO(BEGTSIDX)            
      IF ( FEWSDEBUG.GE.4 ) THEN
         WRITE(MESSAGESTRING, 9999) BEGTSIDX, NUMTS
         call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 9999    FORMAT('Begin TS index P(11) = ',I5,
     $          /'Num of TS PO(PO(11)) = ',I5) 
      END IF

C
C Get input/output infomation
C
      DO I=1,NUMTS
         WRITE(CURRID,'(2A4)') PO(BEGTSIDX+1),PO(BEGTSIDX+2)
         WRITE(CURRDATATYPE,'(A4)') PO(BEGTSIDX+3)
         CURRTIMESTEP = PO(BEGTSIDX+4)            
         INOROUT=PO(BEGTSIDX+5)

         IF ( (CURRID.NE.BLANKS) .AND.  
     >        ((INOROUT.EQ.0) .OR. (INOROUT.EQ.1)) ) THEN 
            DATAIDS(I)= CURRID
            DATATYPES(I) = CURRDATATYPE
            DATATIMESTEPS(I)= CURRTIMESTEP
            DATAINOROUT(I) = INOROUT 
            TOTIO = TOTIO + 1
            IF ( FEWSDEBUG.GE.4 ) THEN
               WRITE(MESSAGESTRING, 1111) DATAIDS(I),DATATYPES(I),
     $                                     CURRTIMESTEP   
               call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 1111          FORMAT('CurrID: ',A8,' CurrDataType: ',A4,
     $                 '    CurrTimeStep: ',I5)  
               WRITE(MESSAGESTRING, 2222) DATAINOROUT(I), TOTIO
               call logfromfortran(DEBUG_LEVEL, MESSAGESTRING) 
 2222          FORMAT('INorOUT: ',I2,' TotalIO: ',I5)
            END IF
            BEGTSIDX = BEGTSIDX + 5            
         ELSE
            DATAINOROUT(I) = -1 
            BEGTSIDX = BEGTSIDX + 2
         ENDIF

      ENDDO
      
      RETURN
      END    
