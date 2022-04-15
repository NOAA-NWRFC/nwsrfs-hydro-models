C MODULE HCKDAT
C***********************************************************************
C  HCKDAT PARSES AN HCL DATE STRING AND CHECKS FOR VALID DATE AND TIME.
C
C  ORIGINAL 8/18/1981 - JIM ERLANDSON - DATA SCIENCES INC.
C
C  CHANGED 11/27/1985 - GFS - HRL - NOW VALID TO ENTER SINGLE DIGIT.
C      DAY IN *+ OR *- DATES AS LONG AS NO HOURS ENTERED.
C
C  CHANGED 3/1993 - JTOSTROWSKI - HRL - ALLOW NEW DATE CODES OF # AND %
C    - # DATE IS USED TO SELECT A DATE AND TIME THAT FALLS ON THE NEXT
C      OR PREVIOUS EVEN MULTIPLE SPECIFIED BY A GIVEN HOUR OR BY THE
C      DEFAULT INTERVAL (SET BY THE TECHNIQUE INTERVAL).
C      FORMAT FOR INPUT IS #+N OR #-N (WITH # EQUIVALENT TO #-INTERVAL
C      TECHNIQUE).  N MUST BE EVENLY DIVISIBLE INTO 24, AND BE BETWEEN
C      1 AND 24 INCLUSIVELY.  IF N IS OMITTED, THE INTERVAL TECHNIQUE
C      VALUE IS USED, THEREBY ALLOWING THE CONSTRUCTS, #+ AND #-.
C    - % DATE IS USED TO SELECT THE NEXT (OR PREVIOUSLY) OCCURRING TIME
C      OF DAY.  THE FORMAT IS %+HHTZC OR %-HHTZC.  TZC DEFAULTS TO THE
C      DEFAULT INPUT TIME ZONE CODE IF NOT PROVIDED.  IF THE + OR - IS
C      PROVIDED, THEN THE HH (1 OR 2 DIGITS) MUST ALSO BE PROVIDED.  IF
C      JUST THE % IS PROVIDED, THE PREVIOUS 12Z IS SELECTED.
C
C  CHANGED 12/1996 - ADDED LASTPP24 OPTION.
C
C  CHANGED 6/1997 - CHANGED ERROR MESSAGES TO WARNING MESSAGES,
C      CONTROLLED THE DISPLAY OF MOD'S WARNING MESSAGES.
C
C  CHANGED 1/1998 and 4/1998 - Y2K CHANGES.
C
C  CHANGED 7/1998 - ADDED CODE TO CHECK IF INPUT IS ONLY LETTERS.
C
C  CHANGED 8/1998 - DWS - MASSIVE STRUCTURAL CHANGES, BROKE INTO FOUR
C      ROUTINES: HCKDAT,HCLDAT,HCLERR,HCLDBG.  ADDED NEW DATE ROUTINES.
C***********************************************************************
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ISX         I    I     1     STARTING COLUMN OF IBUF (UNPACKED)
C       IEX         I    I     1     ENDING COLUMN OF IBUF (UNPACKED)
C       ITBUF       I    O     7     ARRAY TO HOLD DATE AND TIME VALUES:
C                                     ITBUF(1)=JULIAN HOUR
C                                     ITBUF(2)=MONTH
C                                     ITBUF(3)=DAY
C                                     ITBUF(4)=YEAR
C                                     ITBUF(5)=HOUR
C                                     ITBUF(6)=TIME ZONE CODE
C                                     ITBUF(7)=ZERO
C       IERR        I    O     1     STATUS CODE:
C                                     0=NORMAL RETURN
C                                     1=NOT A VALID DATE
C***********************************************************************
C  cmt: Common and its variables used by this routine:
C  cmt:     UFREEI ..... IBUF(80)      input string, unpacked int array
C  cmt:     WHERE ...... IOPNUM        operation number for trace
C  cmt:                  OPNAME(2)     operation name as real (8 chars)
C***********************************************************************
      SUBROUTINE HCKDAT (ISX,IEX,ITBUF,IERR)

C STUB OUT

      RETURN
      END
