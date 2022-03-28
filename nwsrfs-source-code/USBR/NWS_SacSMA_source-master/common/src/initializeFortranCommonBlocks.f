CC  
CC  Initialize start,end and last observation dates common blocks
CC
CC  
       SUBROUTINE initializefortrancommonblocks(startjulday, 
     1  					startjulhour, 
     1                                          lastjulday, 
     1						lastjulhour,
     1                                          lastobsjulday, 
     1						lastobsjulhour,
     1                                          fewsdebuglevel,
     1                                          localhouroffset,
     1                                          localstartofday)
	INCLUDE 'flogm'
	INCLUDE 'fctime'
	INCLUDE 'fcary'

	INTEGER startjulday, startjulhour,lastjulday, lastjulhour
	INTEGER fewsdebuglevel, localhouroffset, localstartofday
	
	DEBUG_LEVEL=4
	INFO_LEVEL=3
	WARNING_LEVEL=2
	FATAL_LEVEL=0

        FEWSDEBUG=fewsdebuglevel
	ITRACE=FEWSDEBUG
	NDEBUG=1
	IDBALL=1
	MAX_NUMBER_OF_LINES=20

	DO I=1,20
	   MESSAGESTRING(I) = ' '
	END DO
 
        MESSAGELINE = ' '
        
        NULLLINE = ' '
    
        FORMATSTR = ' '  

C TIME ZONE NUMBER
        NLSTZ = localhouroffset
        LOCAL = localstartofday + NLSTZ 

        IDA = startjulday
        IHR = startjulhour
        LDA = lastjulday
        LHR = lastjulhour

C TO DO set LDACPD and LHRCPD from FEWS ARGUMENTS      
        LDACPD = lastobsjulday
        LHRCPD = lastobsjulhour
        IDADAT = startjulday

C
C Reset FEWDEBUG flag to 4 if model runs more than 1-year
C and user defines FEWSDEBUG flag is greater than 4.
C
        IYEAR = (lastjulday - startjulday) / 365
        IF ( FEWSDEBUG.GT.4 .AND. IYEAR.GT.1 )  THEN
          FEWSDEBUG = 4
          WRITE(MESSAGESTRING,*) 
     >    'Model runs more than 1-year =>>>>>> Reset FEWSDEBUG to 4'
          call logfromfortran(INFO_LEVEL, MESSAGESTRING)
        END IF	
C
        IF ( FEWSDEBUG.GE.1 ) THEN
           WRITE(MESSAGESTRING, 100) NLSTZ, LOCAL
           call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 100       FORMAT('TimeZoneNumber(NLSTZ) = ',I3/,
     1     'Local standard time at start of Hydrologic day'
     2     ' (LOCAL) = ',I3/)

           WRITE(MESSAGESTRING, 111) IDA, IHR
           call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 111       FORMAT('Start julian day = ',I8, 
     1            ' Start julian hour = ',I3/)
 
           WRITE(MESSAGESTRING, 222) LDA, LHR
           call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 222       FORMAT('End julian day = ',I8, 
     1            ' End julian hour= ',I3/)
 
           WRITE(MESSAGESTRING, 333) LDACPD, LHRCPD
           call logfromfortran(DEBUG_LEVEL, MESSAGESTRING)
 333       FORMAT('End obs julian day = ',I8,
     1            ' End obs julian hour= ',I3/)
        END IF
      
C THE BELOW IS USED TO INITIALIZE FCARY common block     
 
        IFILLC = 1
        NCSTOR = 1
        ICDAY(1) = lastjulday
        ICHOUR(1) = lastjulhour
      
        RETURN
        END
