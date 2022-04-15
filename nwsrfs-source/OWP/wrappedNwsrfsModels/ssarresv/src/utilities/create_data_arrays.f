C Routine create_data_arrays()
C
C DIVIDE TS INTO INPUT AND OUTPUT TS
C UNUSED TS ARE STORED WITH BLANK ' ' IN DATATYPE FIELD
C also define pointers to data input/output based on NDD (number of days in d array)
C SSARRESV has 2 modes
C 1st mode uses 15 time series
C 2nd mode uses 28 time series
C the number of input time series can vary
C 0 for index represents case where ts not used
C the mode and which ts needed can be determined after pin executed
C Argument Variables Type   Description
C PO                 input  - P array data
C DATAIDS            I/O    - Timeseries ID
C DATATYPES          I/O    - Timeseires Types
C DATATIMESTEPS      I/O    - Timeseries time interval
C

      SUBROUTINE create_data_arrays(PO,DATAIDS,DATATYPES,                           
     1                             DATATIMESTEPS)
                                 
      PARAMETER (MAXINPUTTS=10, MAXOUTPUTTS=20)
      PARAMETER (IOsize=MAXINPUTTS+MAXOUTPUTTS)
            
      INTEGER NUMTS
      INTEGER PINDEX
      INTEGER INOROUT
CCCCC      INTEGER MODELTIMESTEP
      INTEGER CURRENTTIMESTEP
CCCCC      INTEGER NUMTIMESTEPS
CCCC      INTEGER NEXTDARRAYINDEX
       
      REAL PO(500000)
      CHARACTER*8 CURRENTID
      CHARACTER*4 CURRENTDATATYPE
      CHARACTER*4 SCURRENTTIMESTEP
      
      CHARACTER*4 DATATYPES(IOsize)
      CHARACTER*8 DATAIDS(IOsize)
      INTEGER DATAINDEX(IOsize)
      INTEGER DATATIMESTEPS(IOsize)
      
      PINDEX=PO(12)
      NUMTS=PO(PINDEX)	
CCCCCC      NEXTDARRAYINDEX=1
CCCCCC      MODELTIMESTEP=PO(7)
	
       
      DO I=1,NUMTS
          WRITE(CURRENTID,'(2A4)') PO(PINDEX+1),PO(PINDEX+2)
          WRITE(CURRENTDATATYPE,'(A4)') PO(PINDEX+3)
          CURRENTTIMESTEP = PO(PINDEX+4)            
          INOROUT=PO(PINDEX+5)
      
	  IF((INOROUT.EQ.0) .AND. CURRENTID.NE.' ')THEN
C IT IS AN INPUT DATA TYPE
	       
CCCCCC              DATAINDEX(I) = NEXTDARRAYINDEX
	      DATAIDS(I)= CURRENTID
	      DATATYPES(I)= CURRENTDATATYPE
	      DATATIMESTEPS(I)= CURRENTTIMESTEP
CCCCCC	      DATAINOROUT(I) = INOROUT              
CCCCCC	      NEXTDARRAYINDEX=NUMTIMESTEPS*                      
CCCCCC     1          (MODELTIMESTEP/CURRENTTIMESTEP)+DATAINDEX(I)
                
	  ELSEIF(INOROUT.EQ.1)THEN
C IT IS AN OUTPUT DATA TYPE
	       
CCCCCC	      DATAINDEX(I) = NEXTDARRAYINDEX
	      DATAIDS(I) = CURRENTID
	      DATATYPES(I) = CURRENTDATATYPE
	      DATATIMESTEPS(I) = CURRENTTIMESTEP
CCCCCC	      DATAINOROUT(I) = INOROUT
CCCCCC	      NEXTDARRAYINDEX = NUMTIMESTEPS*
CCCCCC     1                    (MODELTIMESTEP/CURRENTTIMESTEP)+DATAINDEX(I)
	  ELSE 
CCCCCC	      DATAINOROUT(I) = -1
	      DATAINDEX(I) = 0
	  ENDIF	    	 
	  PINDEX=PINDEX+5            
	ENDDO
	RETURN
	END    
