cc Routine create_data_index()
cc
cc  Input data: PO           - P Array data
cc              NUMTIMESTEPS - Timeseries time interval 
cc              
cc  Input/Ouput data: DATAINDEX   - contains next array index
cc                    DATAINOROUT - 1 = input
cc                                  0 = ouput
cc            
      SUBROUTINE create_data_index(PO,NUMTIMESTEPS,                           
     1                             DATAINDEX,DATAINOROUT)
        
     
      PARAMETER (MAXINPUTTS=10,MAXOUTPUTTS=20)
      PARAMETER (IOsize=MAXINPUTTS+MAXOUTPUTTS)
      INTEGER NUMTS
      INTEGER PINDEX
      INTEGER INOROUT
      INTEGER MODELTIMESTEP
      INTEGER NEXTDARRAYINDEX
      INTEGER DATAINDEX(IOsize)
      INTEGER DATAINOROUT(IOsize)
      INTEGER NUMTIMESTEPS
      INTEGER CURRENTTIMESTEP
      
      REAL PO(500000)
      
      CHARACTER*8 CURRENTID
      
      PINDEX=PO(12)
      NUMTS=PO(PINDEX)	
      NEXTDARRAYINDEX=1
      MODELTIMESTEP=PO(7)
      
      
      DO I=1,NUMTS
          WRITE(CURRENTID,'(2A4)') PO(PINDEX+1),PO(PINDEX+2)          
          CURRENTTIMESTEP = PO(PINDEX+4)            
          INOROUT=PO(PINDEX+5)
      
	  IF((INOROUT.EQ.0) .AND. CURRENTID.NE.' ')THEN
	     DATAINDEX(I) = NEXTDARRAYINDEX
	     DATAINOROUT(I) = INOROUT              
	     NEXTDARRAYINDEX=NUMTIMESTEPS*                      
     1          (MODELTIMESTEP/CURRENTTIMESTEP)+DATAINDEX(I)
                
	  ELSEIF(INOROUT.EQ.1)THEN
	     DATAINDEX(I) = NEXTDARRAYINDEX
	     DATAINOROUT(I) = INOROUT
	     NEXTDARRAYINDEX = NUMTIMESTEPS*
     1                    (CURRENTTIMESTEP/MODELTIMESTEP)+DATAINDEX(I)
	  ELSE 
	      DATAINOROUT(I) = -1
	      DATAINDEX(I) = 0
	  ENDIF	    	 
	  PINDEX=PINDEX+5            
      ENDDO
      RETURN
      END   
