      SUBROUTINE ZRRTSX ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,
     * JQUAL, LQUAL, LQREAD, CUNITS, CTYPE, IUHEAD, KUHEAD, NUHEAD,
     * IOFSET, JCOMP, ISTAT)
C
C     Z-Retrieve Regular interval Time-Series data
C     This routine provides an altertantive means from ZGTDTS of
C     retrieving time-series data.  Data can either be retrieved
C     according to a time window set, or all the data from the
C     pathname specified.
C
C     Input:
C        IFLTAB:  Working DSS array used in ZOPEN call.  Must be
C                 be dimensioned as INTEGER with 1200 words
C        CPATH:   Pathname of the data to be retrieved.  If a time
C                 window is specified (CDATE and CTIME), the 'D'
C                 part is ignored;  Otherwise all parts must be
C                 correct.  CPATH sould be declared as CHARACTER*80
C        CDATE:   Beginning date of the time window.  This may be
C                 a standard military style date (e.g. 01MAR74).
C                 If the data is to be retrieved without a time window
C                 (i.e. all data specified by pathname), set CDATE to
C                 all blanks, or make length short (e.g. ' ').
C        CTIME:   Beginning time of the time window.  This must be
C                 a standard 24 hour clock time (e.g. 1630).  If no
C                 time window is set, this parameter is ignored.
C                 CTIME should be declared as CHARACTER*4.
C        NVALS:   The number of data values to retrieve.  This parameter
C                 defines the end of the time window.  If the entire
C                 record is to be retrived (CDATE equal ' '), then NVALS
C                 is returned with the number of data read.  In this
C                 case, VALUES must be dimensioned large enough to
C                 hold all the data in the record, and NVALS may be
C                 the dimension of the array VALUES.
C     Output:
C        NVALS:   The number of data retrieved.  Note that this is
C                 also and input argument.
C        VALUES:  The data retrieved.
C        CUNITS:  Character string returning the units of the data.
C                 CUNITS must be declared CHARACTER*8
C        CTYPE:   Character string returning the type of the data
C                 (e.g., PER-AVER).  CTYPE must be declared CHARACTER*8
C        IOFSET:  The time offset of the data in minutes.  If there
C                 is no offset, IOFSET is returned as zero.
C        ISTAT:   Integer status parameter, indicating the
C                 successfullness of the retrieval.
C                 ISTAT = 0  All ok.
C                 ISTAT = 1  Some missing data (still ok)
C                 ISTAT = 2  Missing data blocks, but some data found
C                 ISTAT = 3  Combination of 1 and 2 (some data found)
C                 ISTAT = 4  No data found, although a pathname was read
C                 ISTAT = 5  No pathname(s) found
C                 ISTAT > 9  Illegal call to ZRRTS
C
C     Written by Bill Charley
C
C
C     DIMENSION STATEMENTS
C
C     Argument Dimensions
      CHARACTER CPATH*(*), CTYPE*(*), CUNITS*(*), CDATE*(*), CTIME*(*)
C     INTEGER*6 IFLTAB(*), IUHEAD(*), JQUAL(*)                          H
C     INTEGER IFLTAB(*), IUHEAD(*), JQUAL(*)                            u
      INTEGER*4 IFLTAB(*), IUHEAD(*), JQUAL(*)                          ML
      INTEGER*4 JULS, JULE, INTL, IOFF, JULSD, JULAST, IYMDJL, JUL      ML
      INTEGER*4 IOFSET                                                  ML
      REAL VALUES(*)
C
C     Local Dimensions
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      CHARACTER CTSPAT*80
      CHARACTER CDATE1*12, CDATE2*12, CTIME1*4, CTIME2*4, CSCRAT*8
      LOGICAL LFOUND, LTIMEW, LPATH, LQUAL, LQREAD, LQBLOK
C
C
CADD C.ZDSSKZ                                                           H
      INCLUDE 'zdsskz.h'                                                MLu
C
CADD C.ZDSSIZ                                                           H
      INCLUDE 'zdssiz.h'                                                MLu
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
CADD C.ZDSSTS                                                           H
      INCLUDE 'zdssts.h'                                                MLu
C
C
C
      NSTART = 1
      ISTAT = 0
      JCOMP = 0
      NUHEAD = 0
      LPATH = .FALSE.
      LQREAD = .FALSE.
C
      IFUNIT = IFLTAB(KUNIT)
C
C     IF (IFLTAB(1).EQ.4) THEN                                          H
C     CALL ZRRTS ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,          H
C    * CUNITS, CTYPE, IOFSET, ISTAT)                                    H
C     NUHEAD = 0                                                        H
C     JCOMP = 0                                                         H
C     LQREAD = .FALSE.                                                  H
C     RETURN                                                            H
C     ENDIF                                                             H
C
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) THEN                                             D
      WRITE (MUNIT,20) IFUNIT                                           D
 20   FORMAT (T10,'----- Entering ZRRTSX for unit',I3,' -----')         D
      ENDIF                                                             D
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL ZERROR (IFLTAB, 5, 'ZRRTSX', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
C
C     Unform the pathname
      CALL CHRLNB (CPATH, NPATH)
      IF ((NPATH.GT.80).OR.(NPATH.LE.1)) GO TO 900
      CALL ZUPATH (CPATH, IBPART, IEPART, ILPART, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (ILPART(5).LT.4) GO TO 960
C
      IF (MLEVEL.GE.7) WRITE (MUNIT, 40) CPATH                          D
 40   FORMAT (' Pathname: ',A)                                          D
C
C     Determine the time, interval in minutes
      I = 1
      CALL ZGINTL (INTL, CPATH(IBPART(5):IEPART(5)), NUMDAT, I)
      IF (I.LT.0) GO TO 910
C
C     See if a time window has been set
      LTIMEW = .FALSE.
      I = LEN(CDATE)
      IF (I.GT.4) THEN
      J = NINDX (CDATE,' ')
      IF (J.NE.0) LTIMEW = .TRUE.
      ENDIF
C
C     Check that a positive number of data was requested
      IF (NVALS.LE.0) GO TO 920
C
      IF (.NOT.LTIMEW) THEN
      IF (MLEVEL.GE.7) WRITE (MUNIT,60) INTL, NVALS                     D
 60   FORMAT (T10,'No time Window set.  Interval:',I7,'  Maximum',      D
     * ' number of data values:',I7,/,T10,'Computed Maximum Times:')    D
      IF (NVALS.LT.NUMDAT) THEN
      IF (MLEVEL.GE.5) WRITE (MUNIT,70) CPATH(1:NPATH), NVALS, NUMDAT
 70   FORMAT (' -----DSS--- ZRRTS:  Warning - More data available',
     * ' than requested',/,' Pathname: ',A,/,' Number requested:',I5,
     * '  Number Available:',I5)
      ENDIF
C
C     If no time window set, compute the starting date and time
      IF (ILPART(4).LT.5) GO TO 900
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)), JULS, IERR)
      IF (IERR.NE.0) GO TO 900
      ISTIME = 0
      I = INCTIM (INTL, 0, 1, JULS, ISTIME, JULS, ISTIME)
      NVALS = MIN0 (NVALS,NUMDAT)
C
      ELSE
C
C     Time window set - compute julian dates and times
      CALL DATJUL ( CDATE, JULS, IERR)
      IF (IERR.NE.0) GO TO 930
      ISTIME = IHM2M (CTIME)
      IF (ISTIME.EQ.0) THEN
      JULS = JULS - 1
      ISTIME = 1440
      ENDIF
C     Check for an illegal starting time
      IF ((ISTIME.LT.0).OR.(ISTIME.GT.1440)) GO TO 940
C     If the time is not on the standard boundaries, adjust it
      CALL ZOFSET (JULS,ISTIME,INTL,1,IOFF)
C
      ENDIF
C
C     Compute the ending time from the number of values desired
      I = INCTIM (INTL, 0, NVALS-1, JULS, ISTIME, JULE, IETIME)
C
      IF (MLEVEL.GE.7) THEN                                             D
      IF (LTIMEW) WRITE (MUNIT,80)INTL,NVALS                            D
 80   FORMAT (T10,'Time Window set.  Interval:',I6,'  Number of'        D
     * ' data values:',I7)                                              D
      CALL JULDAT (JULS, 1, CDATE1, NDATE1)                             D
      CALL JULDAT (JULE, 1, CDATE2, NDATE2)                             D
      I = M2IHM (ISTIME, CTIME1)                                        D
      I = M2IHM (IETIME, CTIME2)                                        D
      WRITE (MUNIT,90) CDATE1(1:NDATE1), CTIME1, JULS, ISTIME,          D
     * CDATE2(1:NDATE2), CTIME2, JULE, IETIME, IOFF                     D
 90   FORMAT (T10,'Starting date and time:  ',A,2X,A,'  (',I7,I5,')',/, D
     *        T10,'Ending   date and time:  ',A,2X,A,'  (',I7,I5,')',/, D
     *        T10,'Input time offset:',I7)                              D
      ENDIF                                                             D
C
C
C
C     Obtain the date of the first block
      CALL ZBEGDT (JULS, INTL, IYR, IMON, IDAY, IBLOCK)
      JULSD = IYMDJL (IYR, IMON, IDAY)
C
C     Get the date of the last block
      CALL ZBEGDT (JULE, INTL, JYR, JMON, JDAY, IBLOCK)
      JULAST = IYMDJL (JYR, JMON, JDAY)
C
C
C     Loop, reading data blocks, changing the D (date)
C     part of the pathname each time
C
      CDATE1 = ' '
 100  CONTINUE
C     Get the new D (Date) part
      CALL YMDDAT (IYR, IMON, IDAY, 104, CDATE1, NDATE1, IERR)
      IF (IERR.NE.0) GO TO 950
      JULSD = IYMDJL (IYR, IMON, IDAY)
C
      N = IEPART(3) + 1
      IF (ILPART(3).EQ.0) N = IEPART(3)
      CTSPAT = CPATH(1:N) // CDATE1(1:NDATE1) //
     * CPATH(IBPART(5)-1:NPATH)
      CALL CHRLNB (CTSPAT, NTSPAT)
C
C
C     Get the time of the next data block
      CALL ZINCBK (IBLOCK, JUL, IYR, IMON, IDAY)
C
      IF (INTL.EQ.10080) THEN
C     Weekly data.  Set so that the data is always on
C     Saturday at 2400 hours.
      NDAY = IDAYWK (JUL)
      JUL = JUL - NDAY + 1
      NDAY = IDAYWK (JULSD)
      JULSD = JULSD - NDAY + 1
      ENDIF
C
C
C     Determine if this record exists.
C     If it does, read its information block.
C
      CALL ZRDINF (IFLTAB, CTSPAT(1:NTSPAT), NHEAD, NADATA, JSTAT)
      IF (JSTAT.EQ.0) THEN
      IF (IFLTAB(KDTYPE).NE.100) GO TO 970
      LFOUND = .TRUE.
      ELSE
      LFOUND = .FALSE.
      ENDIF
C
      IF (MLEVEL.GE.8) THEN                                             D
      WRITE (MUNIT,220) LFOUND, CTSPAT                                  D
 220  FORMAT (T10,'After ZRDINF, Record found:',L2,/,T10,'Pathname: ',A)D
      IF (LFOUND) THEN                                                  D
      WRITE (MUNIT,230) NADATA, NHEAD, INFO(NPPWRD+KICOMP),             D
     * INFO(NPPWRD+KIQUAL)                                              D
 230  FORMAT (T10,'Number of actual data:',I5,'  Header length:',I4,/,  D
     * T10,'Compression:',I4,'  Quality:',I4)                           D
      ENDIF                                                             D
      ENDIF                                                             D
C
      IF (LFOUND) THEN
C
C     Write the message that the data was read
      IF (MLEVEL.GE.4) THEN
      IF (L80COL) THEN
      WRITE ( MUNIT,231) CTSPAT(1:NTSPAT)
 231  FORMAT(' --ZREAD:  ',A)
      ELSE
      WRITE ( MUNIT,232) IFLTAB(KUNIT), INFO(NPPWRD+KIVER),
     * CTSPAT(1:NTSPAT)
 232  FORMAT(' -----DSS--- ZREAD Unit',I4,'; Vers.',I5,':',2X,A)
      ENDIF
      ENDIF
C
C
C     Determine the number of logical data values (which is different
C     when data compression or quality flags are used)
      NLDATA = INFO(NPPWRD+KILNDA)
C
C     Determine if the data quality flag is set
      IF (INFO(NPPWRD+KIQUAL).EQ.0) THEN
      LQBLOK = .FALSE.
      ELSE
      LQBLOK = .TRUE.
      ENDIF
C
C     Read the internal header (containing offset, units, etc.)
      N = INFO(NPPWRD+KINIHE)
      IF (N.GT.0)
     * CALL ZGTREC (IFLTAB, INTBUF, N, INFO(NPPWRD+KIAIHE), .TRUE.)
C
C     If there is a user header, retrieve that
      NUHEAD = INFO(NPPWRD+KINUHE)
      NUHEAD = MIN0 (NUHEAD, KUHEAD)
      IF (NUHEAD.GT.0)
     *CALL ZGTREC (IFLTAB, IUHEAD, NUHEAD, INFO(NPPWRD+KIAUHE), .TRUE.)
C
      ELSE
C
C     Record not found - need to compute the number of data that would
C     have been read
      NLDATA = NOPERS (INTL, 0, JULSD, 0, JUL, 0)
C
      ENDIF
C
C     If there is no time window, set the number of data
      IF (.NOT.LTIMEW) THEN
      NLDATA = MIN0 (NLDATA,NVALS)
      NVALS = NLDATA
      ENDIF
C
C     Now retrieve the data
C
      CALL ZRRTSB (IFLTAB, JULS, ISTIME, INTL, JULSD, NSTART,
     * NLDATA, NADATA, NVALS, VALUES, JQUAL, LQBLOK, LQUAL, JCOMP,
     * LFOUND, ISTAT)
      IF (ISTAT.GT.9) GO TO 980
C
C     Clear the Buffer save flags set above by calling ZBDUMP
      CALL ZBDUMP (IFLTAB, 1)
C
C
C     If this record was not found, write an error message
C
      IF (.NOT.LFOUND) THEN
C
      IF ((ISTAT.EQ.0).AND.(MLEVEL.GE.2)) WRITE (MUNIT,240)IFUNIT,CTSPAT
 240  FORMAT (' -----DSS*** ZRRTS:  CAUTION - Data block not ',
     * 'found in file.  Unit:',I4,/,' Pathname: ',A)
C
      IF (.NOT.LTIMEW) THEN
      ISTAT = 5
      GO TO 800
      ELSE
      ISTAT = 2
      ENDIF
C
      ELSE
C     Get the data time offset, data type, and data units
      IOFSET = INTBUF(1)
      CALL HOLCHR (INTBUF(2),  1, 8, CSCRAT, 1)
      CUNITS = CSCRAT
      CALL HOLCHR (INTBUF(4), 1, 8, CSCRAT,  1)
      CTYPE = CSCRAT
      LPATH = .TRUE.
      ENDIF
C
      IF ((LQBLOK).AND.(LQUAL)) LQREAD = .TRUE.
C
C     If not time window set, set the status flag and return.
      IF (LTIMEW) THEN
C     Are we done reading the data?
      IF (NSTART.GT.NVALS) GO TO 300
      IF (JUL.GT.JULAST) GO TO 300
C     Need to read more data, loop back to 100
      GO TO 100
      ENDIF
C
C
C     Reading complete.  Locate any missing data and indicate in ISTAT
 300  CONTINUE
      IF (ISTAT.GE.4) GO TO 800
      LFOUND = .FALSE.
      DO 320 I=1,NVALS
      IF (VALUES(I).EQ.-901.0) THEN
      IF (ISTAT.LT.1) THEN
      ISTAT = 1
      ELSE IF (ISTAT.GT.1) THEN
      ISTAT = 3
      ENDIF
      ELSE IF (VALUES(I).EQ.-902.0) THEN
      ELSE
      LFOUND = .TRUE.
      ENDIF
 320  CONTINUE
C     All missing data, but pathname(s) read?
      IF (.NOT.LFOUND) ISTAT = 4
C     No Pathnames found?
      IF (.NOT.LPATH)  ISTAT = 5
C
C
 800  CONTINUE
      IF (ISTAT.GE.5) THEN
      CUNITS = ' '
      CTYPE = ' '
      ENDIF
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NVALS, ISTAT, IOFSET,          D
     * CUNITS, CTYPE                                                    D
 820  FORMAT(T8,'----- Exiting ZRRTS, Number of data values:',I7,       D
     * ',  Status:',I3/,T10,'Offset:',I8,',  Units: ',A,',  Type:',A)   D
C
      RETURN
C
C
C     --- ERROR STATEMENTS ---
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,901) NPATH, CPATH
 901  FORMAT (/,' *****DSS*** ZRRTS:  FATAL ERROR  - ILLEGAL PATHNAME',
     * ' OR PATHAME LENGTH',/,' Length: ',I5,/,' Pathname: ',A,/)
      ISTAT = 24
      GO TO 990
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) CPATH(IBPART(5):IEPART(5)),
     * INTL, CPATH
 911  FORMAT (/,' *****DSS*** ZRRTS:  FATAL ERROR  - NON-STANDARD TIME',
     * ' INTERVAL',/,' Interval: ',A,2X,I8,/,' Pathname: ',A,/)
      ISTAT = 12
      GO TO 990
C
 920  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,921) NVALS, CPATH
 921  FORMAT (/,' *****DSS*** ZRRTS:  FATAL ERROR  - NUMBER OF VALUES',
     * ' REQUESTED LESS THAN 1',/,' NVALS: ',I8,/,' Pathname: ',A,/)
      ISTAT = 11
      GO TO 990
C
 930  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,931) CDATE, JULS, CPATH
 931  FORMAT (/,' *****DSS*** ZRRTS:  FATAL ERROR  - ILLEGAL STARTING',
     * ' DATE SPECIFIED',/,' Date: ',A,3X,I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 940  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,941) CTIME, ISTIME, CPATH
 941  FORMAT (/,' *****DSS*** ZRRTS:  FATAL ERROR  - ILLEGAL STARTING',
     * ' TIME SPECIFIED',/,' Time: ',A,3X,I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 950  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,951) CDATE1, IYR, IMON, IDAY, CPATH
 951  FORMAT(/,' *****DSS*** ZRRTS:  FATAL ERROR  - UNABLE TO GENERATE',
     * ' BLOCK DATE',/,' Date: ',A,3X,3I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 960  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,961) CPATH
 961  FORMAT (/,' *****DSS*** ZRRTS:  FATAL ERROR  - NO TIME',
     * ' INTERVAL',/,' Pathname: ',A,/)
      ISTAT = 12
      GO TO 990
C
 970  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,971) CPATH, IFLTAB(KDTYPE)
 971  FORMAT (/,' *****DSS*** ZRRTS:  FATAL ERROR  - The Data is not',
     * ' Regular-Interval Time Series',/,' Pathname: ',A,/,
     * ' Data Type:',I5,/)
      ISTAT = 20
      GO TO 990
C
 980  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,981) ISTAT, CPATH
 981  FORMAT (/,' *****DSS*** ZRRTS:  FATAL ERROR  - UNABLE TO ',
     * ' RETRIEVE DATA',/,' Status: ',I8,/,' Pathname: ',A,/)
      GO TO 990
C
C
 990  CONTINUE
      NVALS = 0
      IF (MLEVEL.GE.7) WRITE (MUNIT,991) ISTAT                          D
 991  FORMAT(T10,'----- Exiting ZRRTS, Error return; Status:',I4)       D
C
      RETURN
      END
