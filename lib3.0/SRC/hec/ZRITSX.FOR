      SUBROUTINE ZRITSX (IFLTAB, CPATH, JULS, ISTIME, JULE, IETIME,
     * ITIMES, VALUES, KVALS, NVALS, IBDATE, IQUAL, LQUAL, LQREAD,
     * CUNITS, CTYPE, IUHEAD, KUHEAD, NUHEAD, INFLAG, ISTAT)
C
C     Main subroutine for retrieving irregular time series data
C
C     Written by Bill Charley
C
C
C     09JUL87 - Alaric Clinton  add the next and
C    previous (see ISTAT) capability
C
C     variables
C       LPREV - get the previous data value
C       LCASE - special case of the previous value being the last
C                value of the previous block
C       LNEXT - get the next data value
C       IFORWD - (1) increment time (-1) decrement time
C
C     special cases
C       1) previous value not in the current record
C       2) next value not in the current record
C       3) time window contains an initial empty intervals
C       4) time window contains internal empty intervals
C       5) entire time window is empty
C
C     notes
C       1) should the time window be empty, NVALS is returned as zero;
C          otherwise one or two extra data values may be inclued
C          depending on ISTAT
C
C     INTEGER*6 IFLTAB(*), IQUAL(*), ITIMES(*), IUHEAD(*), JTIME, I6    H
C     INTEGER IFLTAB(*), IQUAL(*), ITIMES(*), IUHEAD(*)                 u
      INTEGER*4 IFLTAB(*), IQUAL(*), ITIMES(*), IUHEAD(*), JTIME, I6    ML
      INTEGER*4 JULS, JULE, IBDATE, JUL, IYMDJL, JLS, JLE, JDATE        ML
      INTEGER*4 IUNIT, JQUAL, IDTYPE                                    ML
      REAL VALUES(*)
      LOGICAL LQUAL, LQREAD
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      PARAMETER (KIHEAD=20)
C     INTEGER*6 IIHEAD(KIHEAD)                                          H
      INTEGER*4 IIHEAD(KIHEAD)                                          ML
C     INTEGER IIHEAD(KIHEAD)                                            u
C
      CHARACTER CPATH*(*), CUNITS*(*), CTYPE*(*)
      CHARACTER CPATH1*80, CDATE*9, CDATE1*9, CDATE2*9, CSCRAT*20
      CHARACTER CTEMP*4
C
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSTS                                                           H
      INCLUDE 'zdssts.h'                                                MLu
C
      LOGICAL LF, LFOUND, LGETQ, LPREV, LNEXT, LCASE
      INTEGER IFORWD
C
C
C
C
      IF (MLEVEL.GE.9) THEN                                             D
      CDATE1 = ' '                                                      D
      CDATE2 = ' '                                                      D
      IF (ISTIME.GE.0) THEN                                             D
      CALL JULDAT (JULS, 114, CDATE1, NDATE1)                           D
      CALL JULDAT (JULE, 114, CDATE2, NDATE2)                           D
      ENDIF                                                             D
      WRITE (MUNIT, 10) JULS, ISTIME, CDATE1, JULE, IETIME, CDATE2      D
 10   FORMAT (T10,'----- ENTERING ZRITSX-----',                         D
     * /,T5,'Starting date and time:',3X,2I8,2X,A,                      D
     * /,T5,'Ending date and time:  ',3X,2I8,2X,A)                      D
      WRITE (MUNIT, 20) CPATH                                           D
 20   FORMAT (T5,'Pathname: ',A)                                        D
      WRITE (MUNIT,30) KVALS, INFLAG                                    D
 30   FORMAT (T5,'Data dimension limit:',I6,'  INFLAG:',I3)             D
      ENDIF                                                             D
C
C     IF (IFLTAB(1).EQ.4) THEN                                          H
C     CALL CHRLNB (CPATH, NPATH)                                        H
C     N = NIBUFF                                                        H
C     CALL ZGIRTS (IFLTAB, CPATH, NPATH, JULS, ISTIME, JULE, IETIME,    H
C    * BUFF, KLBUFF, INTBUF, N, KVALS, ITIMES, VALUES,                  H
C    * NVALS, BDATE, CUNITS, CTYPE, ISTAT)                              H
C     IBDATE = BDATE                                                    H
C     CALL ZR2MIN (ITIMES, ITIMES, 0, NVALS)                            H
C     LQREAD = .FALSE.                                                  H
C     NUHEAD = 0                                                        H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZRITSX', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
      CALL ZINQIR (IFLTAB, 'UNIT', CTEMP,IUNIT)
      LFOUND = .FALSE.
      LQREAD = .FALSE.
C
C
C     Check for reading just one record (no time window)
C
      IF (ISTIME.EQ.-2) THEN
C
      ISTAT = 0
      KB = KLBUFF
      CALL ZREADX (IFLTAB, CPATH, IIHEAD, KIHEAD, NIHEAD,
     * IDUM, 0, N, IUHEAD, KUHEAD, NUHEAD, BUFF, KB, NB, 2, LF)
C
      LFOUND = LF
C
      IF (LF) THEN
C
C     Check to be sure that the data is irregular-interval time series
      CALL ZINQIR (IFLTAB, 'TYPE', CSCRAT, IDTYPE)
      IF (IDTYPE.NE.110) GO TO 930
C
      LGETQ = .FALSE.
      CALL ZINQIR (IFLTAB, 'QUAL', CSCRAT, JQUAL)
      IF (JQUAL.EQ.1) THEN
      IMULT = 3
      IF (LQUAL) LGETQ = .TRUE.
      ELSE
      IMULT = 2
      CPATH1 = CPATH
      ENDIF
C
C     Check to be sure we retrieved a full block
      J = IIHEAD(2) * 2
C     Not enough room to move all the data into the array
      IF (KLBUFF.LT.J) GO TO 900
C
C     Move data from buff into values and dates
      NVALS = IIHEAD(2)
      DO 70 I = 1, NVALS
      J = ((I-1)*IMULT) + 1
      ITIMES(I) = ILBUFF(J)
      VALUES(I) = BUFF(J+1)
      IF (LGETQ) IQUAL(I) = ILBUFF(J+2)
 70   CONTINUE
C
      IBDATE = IIHEAD(3)
C
      ENDIF
C
      ELSE
C
C
C     A time window was specified - read blocks
C
C     set next and previous variables
      IFORWD = 1
      LCASE = .FALSE.
      LPREV = .FALSE.
      LNEXT = .FALSE.
      IF ((INFLAG .EQ. 1) .OR. (INFLAG .EQ. 3)) LPREV = .TRUE.
      IF ((INFLAG .EQ. 2) .OR. (INFLAG .EQ. 3)) LNEXT = .TRUE.
C
      IBDATE = JULS
      NVALS = 0
      ISTAT = 0
C
C
      CALL ZUPATH (CPATH, IBPART, IEPART, ILPART, ISTAT)
      IF (ISTAT.EQ.0) CALL ZIRBEG (IFLTAB, JULS,
     * CPATH(IBPART(5):IEPART(5)), IYR, IMON, IDAY, IBLOCK, MINBLK,
     * INCBLK)
C
      IF ((IBLOCK.LE.0).OR.(ISTAT.NE.0)) GO TO 910
C
C     Get the date of the first block
      JUL = IYMDJL (IYR,IMON,IDAY)
      CPATH1 = CPATH(1:IBPART(4)-1) // '01JAN1900/' //
     * CPATH(IBPART(5):IEPART(6)+1)
      CALL CHRLNB (CPATH1, NPATH)
      GO TO 200
C
C
 120  CONTINUE
C
C     Get dates of subsequent blocks
C
      IF (IBLOCK.EQ.1) THEN
C     Increment by day
      JUL = IYMDJL (IYR, IMON, IDAY) + IFORWD
      K = JLIYMD( JUL, IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.2) THEN
C     Increment by month
      IMON = IMON + IFORWD
      IF ((IMON .LT. 1) .OR. (IMON .GT. 12)) THEN
      IYR = IYR + IFORWD
      IMON = IMON - 12*IFORWD
      ENDIF
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.3) THEN
C     Increment by year
      IYR = IYR + IFORWD
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.4) THEN
C     Increment by decade
      IYR = IYR + 10*IFORWD
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.5) THEN
C     Increment by decade
      IYR = IYR + 100*IFORWD
      JUL = IYMDJL (IYR, IMON, IDAY)
      ENDIF
C
C     set IFORWD to increment
C
 200  CONTINUE
      IFORWD = 1
C
      IF (MLEVEL.GE.9) WRITE (MUNIT, 201) JULE, JUL
 201  FORMAT (T5,'At 200, JULE, JUL: ',2I8)                             D
      IF ((MLEVEL.GE.9).AND.(INFLAG.NE.0)) WRITE (MUNIT, 202)           D
     * LPREV, LNEXT, LCASE                                              D
 202  FORMAT (T5,'LPREV:',L2,'  LNEXT:',L2,'  LCASE:',L2)               D
C
C     have we reached the end of the time window - may get the next
C     value if provided the time window isn't empty
      IF ((JULE.LT.JUL).AND.(.NOT.LNEXT)) GO TO 800
C
C     FORM THE PATHNAME
      CALL JULDAT (JUL, 104, CDATE, NDATE1)
      CPATH1(IBPART(4):IBPART(4)+8) = CDATE
C
      KB = KLBUFF !!!!!!!!!!!
      CALL ZREADX (IFLTAB, CPATH1, IIHEAD, KIHEAD, NIHEAD,
     * IDUM, 0, N, IUHEAD, KUHEAD, NUHEAD, BUFF, KB, NB, 2, LF)
C
C     record not found
      IF (.NOT.LF) THEN
C
C     is the first record found is empty - go to the next one
      IF (LPREV) THEN
      LCASE = .TRUE.
      LPREV = .FALSE.
      IFORWD = -1
      GOTO 120
      ENDIF
C
C     previous value not found - first data point is missing
      IF (LCASE) THEN
      LCASE = .FALSE.
C     go to the next (initial) record
      GOTO 120
C
C     next value not found - test for overflow
      ELSE IF (LNEXT) THEN
C
C     if the time series has initial empty intervals do not end
      IF (JUL .LE. JULE) THEN
C     if the series is missing an interval in the middle
      GOTO 120
      ELSE
      GOTO 800
      ENDIF
C
C     missing record is within the time window
      ELSE
      GO TO 120
      ENDIF
      ENDIF
C
C
C     A valid record was read
      LFOUND = .TRUE.
      LGETQ = .FALSE.
      CALL ZINQIR (IFLTAB, 'QUAL', CSCRAT, JQUAL)
      IF (JQUAL.EQ.1) THEN
      IMULT = 3
      IF (LQUAL) LGETQ = .TRUE.
      ELSE
      IMULT = 2
      ENDIF
C
C     Check to be sure we retrieved a full block
      J = IIHEAD(2) * 2
C     Not enough room to move all the data into the array
      IF (KLBUFF.LT.J) GO TO 900
C
C     MOVE BUFF INTO DATES AND VALUES ARRAY
C
      JDATE = IIHEAD(3)
      JTIME = ILBUFF(1)
C
C     previous value not in the block
      IF (LPREV) THEN
      JTIME = ISTIME
      IF (MLEVEL.GE.9) THEN                                             D
      N= ICTIME (JDATE, ILBUFF(1), IBDATE, JTIME)                       D
      WRITE (MUNIT, 305) JDATE, ILBUFF(1), IBDATE, JTIME, N             D
 305  FORMAT (T5,'LPREV TRUE, JDATE, ILBUFF(1), IBDATE, JTIME, ICTIME:',D
     * /,T15,5I8)                                                       D
      ENDIF                                                             D
      IF (ICTIME (JDATE, ILBUFF(1), IBDATE, JTIME).GE.0) THEN
      LCASE = .TRUE.
      LPREV = .FALSE.
      IFORWD = -1
      GOTO 120
      ENDIF
      ENDIF
C
C     get the last value from the record - special case
      IF (LCASE) THEN
      LCASE = .FALSE.
      IBDATE = IIHEAD(3)
      N = ((IIHEAD(2) - 1) * IMULT) + 1
      ITIMES(1) = ILBUFF(N)
      VALUES(1) = BUFF(N+1)
      IF (LGETQ) IQUAL(1) = ILBUFF(N+2)
      NVALS = 1
      GOTO 120
      ENDIF
C
C     LOOK FOR FIRST VALUE IN OUR TIME WINDOW
      DO 310 I=1,IIHEAD(2)
      IARY = I
      J = ((I-1) * IMULT) + 1
      JTIME = ISTIME
      IF (ICTIME (JDATE, ILBUFF(J), JULS, JTIME).GE.0) GO TO 350
 310  CONTINUE
C
C     Found no requested data in this record
      GO TO 120
C
C     get the previous value, and turn off LPREV
 350  CONTINUE
      IF (LPREV) THEN
      LPREV = .FALSE.
      IARY = IARY - 1
      ENDIF
C
C     NOW MOVE DATA FROM BUFF INTO VALUES AND DATES
      DO 370 I = IARY, IIHEAD(2)
C     if LNEXT is set read one more value
      J = ((I-1) * IMULT) + 1
      JTIME = IETIME
      IF ((ICTIME (JDATE, ILBUFF(J), JULE, JTIME).GT.0).AND.
     * (.NOT.LNEXT)) GO TO 800
      NVALS = NVALS + 1
      IF (NVALS.GT.KVALS) GO TO 920
      I6 = JDATE - IBDATE
      ITIMES(NVALS) = (I6 * 1440) + ILBUFF(J)
      VALUES(NVALS) = BUFF(J+1)
      IF (LGETQ) IQUAL(NVALS) = ILBUFF(J+2)
C     only executed if LNEXT was true above
      JTIME = IETIME
      IF (ICTIME (JDATE, ILBUFF(J), JULE, JTIME).GT.0) GO TO 800
 370  CONTINUE
C
C     Go back and read next record
C
      GO TO 120
C
      ENDIF
C
C
C     Completed
 800  CONTINUE
      IF ((NVALS.EQ.0).AND.(ISTAT.EQ.0)) ISTAT = 4
      IF (LQUAL) THEN
      LQREAD = .FALSE.
      IF (LGETQ) LQREAD = .TRUE.
      ENDIF
C
      IF (.NOT.LFOUND) THEN
      IF (MLEVEL.GE.2) WRITE (MUNIT,810) IUNIT, CPATH1
 810  FORMAT(' -----DSS*** ZRITS:  Caution - Missing Block of Data;',
     * ' Unit:',I4,/,' Pathname: ',A)
      NVALS = 0
      IBDATE = 0
      ISTAT = 4
C
      ELSE
C
C     MOVE HEADER INFORMATION INTO ARGUMENTS
      CALL HOLCHR (IIHEAD(6),  1, 8, CSCRAT, 1)
      CUNITS = CSCRAT
      CALL HOLCHR (IIHEAD(8), 1, 8, CSCRAT,  1)
      CTYPE = CSCRAT
C
      ENDIF
C
C
 820  CONTINUE
      IF (MLEVEL.GE.9) THEN                                             D
      WRITE (MUNIT, 840) NVALS, ISTAT                                   D
 840  FORMAT (T5,'----- Exiting ZRITS;  Number of Data:',I7,            D
     * ',  Status:',I4)                                                 D
C
      IF ((NVALS .LT. KVALS) .AND.                                      D
     +(NVALS .GT. 0)) THEN                                              D
C
      CALL DATCLL (IBDATE, ITIMES(1), JLS, IST)                         D
      CALL JULDAT (JLS, 104, CDATE1, NDATE1)                            D
      CALL DATCLL (IBDATE, ITIMES(NVALS), JLE, IET)                     D
      CALL JULDAT (JLE, 104, CDATE2, NDATE2)                            D
C
      WRITE (MUNIT, 860) JLS, IST, CDATE1, JLE, IET, CDATE2, IBDATE     D
 860  FORMAT (T10,'Date of first value: ',2I8,2X,A,                     D
     * /,T10,'Date of Last value:  ',2I8,2X,A,                          D
     * /,T10,'Base date: ',I8,/)                                        D
C
      ENDIF                                                             D
      ENDIF                                                             D
C
      RETURN
C
C
C     ERROR STATEMENTS
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 901) KLBUFF, J, CPATH1(1:NPATH)
 901  FORMAT(/' ***** Error:  ZRITS:  Buffer size not large enough',/,
     * ' to read the record specified',/,' Buffer size:',I6,
     * '  Record size:',I6,/,' Pathname: ',A,/)
      ISTAT = 21
      GO TO 820
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) CPATH1(1:NPATH)
 911  FORMAT(' -----DSS*** ZRITS;  ERROR:  Unable to Recognize'
     * ' Pathname as Irregular Time-Series',/,
     * ' Pathname: ',A)
      ISTAT = 24
      GO TO 820
C
 920  CONTINUE
      IF (MLEVEL.GE.5) WRITE (MUNIT,921) NVALS, KVALS, CPATH1(1:NPATH)
 921  FORMAT(' ***** WARNING - ZRITS;  Number of Data Found',
     * ' Exceeds Dimension Limit - Reading Terminated *****',/,
     * ' Number Found: ',I6,'   Dimension Limit: ',I6,/,
     * ' Pathname: ',A)
      ISTAT = 1
      GO TO 820
C
 930  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,931) CPATH, IDTYPE
 931  FORMAT (/,' *****DSS*** ZRITS:  FATAL ERROR  - The Data is not',
     * ' Irregular-Interval Time Series',/,' Pathname: ',A,/,
     * ' Data Type:',I5,/)
      ISTAT = 20
      GO TO 820
C
      END
