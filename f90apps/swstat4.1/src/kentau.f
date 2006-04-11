C      
C 
C 
      SUBROUTINE   PKNTAU 
     I                    (MESSFL,WDMSFL,
     M                     DSNCNT,DSNBMX,DSNBUF)
C
C     + + + PURPOSE + + +
C     trend option from swstat.  Manages computation of Kendall's
C     tau.  Can read annual peaks from table data set or an annual
C     time series data set.  By default, time series data sets with
C     time step shorter than 1 year are aggregated by summing all
C     values for the year.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMSFL,DSNCNT,DSNBMX,DSNBUF(DSNBMX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of PTREND message file
C     WDMSFL - users WDM file of data
C     DSNCNT - number of data sets in the buffer
C     DSNBMX - size of data set buffer
C     DSNBUF - array of data set numbers to be processed
C
C     + + + COMMONS + + +
      INCLUDE 'ckntau.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      L1, L2, L3, L4, L5, L6,  L12, N, NC, SGRP, SCLU,
     $             FOUT, QFLG, RETCOD, DSNTYP,          OPT, ICOL, J,
     $             FLAGS(4), DATES(6,2), COUNT(6,MAXCNT), INDX(3),
     $                   RTCMND, TRANSF, PREV, ON, OFF, COMP, AGAIN,
     $             QFSAVE, SGRP2, KEEP
      REAL         MINMAX(2), STATS(3,MAXCNT), MNMX(2)
      CHARACTER*1  STAID(16), STANAM(48), CTYPE(4), QC(12), QCSAVE(12)
      CHARACTER*8  PTHNAM
      CHARACTER*9  FLNAME
C
C     + + + LOCAL DEFINITIONS + + +
C     STATS  - comuted statistics for Kendall's Tau
C              (1,n) - Kendall's Tau
C              (2,n) - P-level
C              (3,n) - median slope of trend
C     FLAGS  - array of option flags
C              (1) - limit analysis by data set 1-no, 2-yes
C              (2) - wdm output: 1-yes, 2-no
C              (3) - file output:
C                    1 - none         3 - narrow
C                    2 - list         4 - wide
C              (4) - transformation code for time-series data sets
C                    1 - average      3 - maximum
C                    2 - total        4 - minimum
C
C     + + + FUNCTIONS + + +
      INTEGER    STRLNX
C
C     + + + EXTERNALS + + +
      EXTERNAL   KNTCMP, KNTDAT, KNTMOD, GTINFO, REOPEN
      EXTERNAL   Q1INIT, QSETI,  QSETR,  QSETCO, QSETIB, QSTCTF
      EXTERNAL   Q1EDIT, QGETI,  QGETR,  QGETCO
      EXTERNAL   Q2INIT, Q2SETR, Q2STIB, Q2EDIT
      EXTERNAL   QFCLOS, QRESP,  ZSTCMA, PRNTXT, STRLNX
      EXTERNAL   WDBSAR, CHRCHR, QUPCAS
      EXTERNAL   PRWMSE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  L1, L2, L3, L4, L5, L6, L12, PREV, ON, OFF, INDX
     $     / 1,  2,  3,  4,  5,  6,  12,    4,  1,   0, 283,284,285 /
      DATA  FLNAME,      FLAGS,    DATES
     $     /'trend.out', 1,2,1,1,  0,1,1,0,0,0, 12,31,24,0,0,0 /
C                              nc = no of non-blank qc
      DATA   MINMAX,    QFLG,  NC,  QC
     $     / 0.0,1.0E6,   30,   5,  '3','4','6','7','8',7*' ' /
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (///, ' Data set number   = ', I5,
     $          /, '  Station number   = ', 16A1,
     $          /, '  Station name     = ', 48A1,
     $          /, '  Data type        = ', 4A1,
     $          /, '  Starting year    = ', I5,
     $          /, '  Ending year      = ', I5,
     $          /, '  Minimum value    = ', G16.7
     $          /, '  Maximum value    = ', G16.7
     $          /, '  Qualifiers       = ', I5, ' | ', 12A1,
     $          /, '  Values used      = ', I5,
     $          /, '  Values skipped   = ', I5,
     $          /, '  Return code      = ', I5,
     $          /, '  error count      = ', I5,
     $          /, '  Kendall''s tau    = ', F9.3,
     $          /, '  P-level          = ', F9.3,
     $          /, '  Median slope     = ', F9.3 )
 2009 FORMAT (///, ' Data set number   = ', I5,
     $          /, '  Values used      = ', I5,
     $          /, '  Values skipped   = ', I5,
     $          /, '  Return code      = ', I5,
     $          /, '  error count      = ', I5 )
 2030 FORMAT (///, '  DATA-                      MEDIAN',
     $             ' PERIOD OF NUMBER OF NON-ZERO',
     $          /, '  SET   KENDALL''S          SLOPE OF',
     $             '  RECORD     VALUES   RETURNS',
     $          /, ' NUMBER    TAU     P-LEVEL   TREND ',
     $             ' FROM  TO  USED  NOT CODE CNT',
     $          /, ' ------ --------- -------- --------',
     $             ' ---- ---- ---- ---- ---- ---' )
 2031 FORMAT ( 1X, I6, F10.3,2F9.3, 2I5, 2I5, I5,I4,
     $         2(1X,G14.7), 1X, I2,'/',12A1, 1X, 4A1, 1X, 16A1 )
 2039 FORMAT ( 1X, I6, 38X,  2I5, I5,I4 )
 2040 FORMAT (///, '  DATA-                      MEDIAN',
     $             ' PERIOD OF NUMBER OF NON-ZERO',
     $          /, '  SET   KENDALL''S          SLOPE OF',
     $             '  RECORD     VALUES   RETURNS',
     $             '       RANGE OF DATA                          ',
     $             ' DATA',
     $          /, ' NUMBER    TAU     P-LEVEL   TREND ',
     $             ' FROM  TO  USED  NOT CODE CNT',
     $             '    MINIMUM        MAXIMUM     QUALIFIERS     ',
     $             ' TYPE STATION NUMBER ',
     $          /, ' ------ --------- -------- --------',
     $             ' ---- ---- ---- ---- ---- ---',
     $             ' -------------- -------------- ---------------',
     $             ' ---- ----------------' )
C 
C     + + + END SPECIFICATIONS + + +
C
      SCLU = 131
      QFLG = 30
      FOUT = 0
C
      OPT = 1
      COMP = 0
 10   CONTINUE
C       Option:  1-Select, 2-Define, 3-output, 4-compute, 5-View, 6-Return
        SGRP = 1
        CALL QRESP (MESSFL, SCLU, SGRP, OPT)
        IF (OPT .EQ. 1) THEN
C         select data sets
          PTHNAM = 'ST      '
          CALL PRWMSE (MESSFL, WDMSFL, DSNBMX, PTHNAM, 
     M                 DSNBUF, DSNCNT)
C         default Trend option to Define, computations to no
          OPT = 2
          COMP = 0
        ELSE IF (OPT .EQ. 2) THEN
C         Define input, computation, and output options, allow Prev
          CALL ZSTCMA ( PREV, ON )
 210      CONTINUE
            AGAIN = 0
            SGRP = 3
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
C           Flags: 1-limit,2-wdm,3-file,4-transformation code
            CALL QSETCO ( L4, FLAGS )
C           data range: 1-minimum, 2-maximum
            CALL QSETR  ( L2, MINMAX )
C           time series quality code and table qualification code
            CALL QSETI  ( L1, QFLG )
C           table qualification codes: 123456789ABCDE (up to 12)
            CALL QSTCTF ( L5, L12, QC )
C           edit screen
            CALL Q1EDIT ( RTCMND )
            IF (RTCMND .EQ. 1) THEN
C             user wants to continue, get values
              CALL QGETCO ( L4, FLAGS )
              CALL QGETR  ( L2, MINMAX )
              CALL QGETI  ( L1, QFLG )
              CALL QGTCTF ( L5, L12, QC )
              CALL QUPCAS ( L12, QC )
              NC = STRLNX ( L12, QC )
            ELSE IF (RTCMND .EQ. -1) THEN
C             oops, try again
              AGAIN = 1
            ELSE
C             assume Prev, leave unchanged
            END IF
          IF (AGAIN .EQ. 1) GO TO 210
C         turn off Prev and default Trend option to Compute
          CALL ZSTCMA ( PREV, OFF )
          OPT = 3
        ELSE IF (OPT .EQ. 3) THEN
C         open new outut file
          SGRP = 5
          SGRP2 = 6
          CALL REOPEN ( MESSFL, SCLU, SGRP, SGRP2, FOUT )
          OPT = 4
        ELSE IF (OPT .EQ. 4  .AND.  DSNCNT .LE. 0) THEN
C         can't compute trend, no data sets selected
          SGRP = 7
          CALL PRNTXT (MESSFL, SCLU, SGRP)
          OPT = 1
        ELSE IF (OPT .EQ. 4) THEN
C         compute trend statistics
          IF (FLAGS(3) .GT. 1  .AND.  FOUT .EQ. 0) THEN
C           printed output but file not open, use default trend.out
            CALL GETFUN ( L1, FOUT )
            OPEN (  UNIT = FOUT,
     $              FILE = FLNAME,
     $            STATUS = 'UNKNOWN' )
          END IF
          IF (FLAGS(3) .EQ. 3) THEN
C           print file needs narrow header
            WRITE (FOUT,2030)
          ELSE IF (FLAGS(3) .EQ. 4) THEN
C           print file needs wide header
            WRITE (FOUT,2040)
          END IF
C         save default min/max, transformation, quality flags
          MNMX(1) = MINMAX(1)
          MNMX(2) = MINMAX(2)
          TRANSF = FLAGS(4) - 1
          QFSAVE = QFLG
          CALL CHRCHR ( L12, QC, QCSAVE )
          N = 0
 310      CONTINUE
            N = N + 1
            CALL KNTDAT ( WDMSFL, MESSFL, DSNBUF(N),
     O                    DSNTYP, DATES, RETCOD )
            IF (RETCOD .EQ. 0) THEN
C             data set looks ok
              IF (FLAGS(1) .EQ. 2) THEN
C               user wants to modify processing options, get defaults
                MINMAX(1) = MNMX(1)
                MINMAX(2) = MNMX(2)
                TRANSF = FLAGS(4) - 1
                QFLG = QFSAVE
                CALL CHRCHR ( L12, QCSAVE, QC )
                CALL KNTMOD ( MESSFL, SCLU, DSNBUF(N), DSNTYP,
     M                        QFLG, TRANSF, DATES, MINMAX, QC )
                CALL QUPCAS ( L12, QC )
                NC = STRLNX ( L12, QC )
              END IF
              CALL KNTCMP ( WDMSFL, DSNBUF(N), DSNTYP,
     I                      MINMAX, DATES, QFLG, TRANSF, QC, NC,
     O                      COUNT(1,N), STATS(1,N) )
              IF (FLAGS(2) .EQ. 1) THEN
C               put results on WDM dataset attributes
                DO 350 J = 1, 3
                  CALL WDBSAR ( WDMSFL, DSNBUF(N), MESSFL,
     I                          INDX(J), L1, STATS(J,N),
     O                          RETCOD )
                  IF (RETCOD .NE. 0) THEN
C                   problem saving attribute
                    COUNT(5,N) = RETCOD
                    COUNT(6,N) = COUNT(6,N) + 1
                  END IF
 350            CONTINUE
              END IF
              IF (FLAGS(3) .GT. 1) THEN
C               output to print file
                IF (FLAGS(3) .EQ. 3) THEN
C                 write narrow table, no descriptive attributes
                  WRITE (FOUT,2031) DSNBUF(N), (STATS(J,N), J = 1, 3),
     $                              (COUNT(J,N), J = 1, 6)
                ELSE
C                 get descriptive info & write results to file
                  CALL GTINFO ( WDMSFL, DSNBUF(N),
     O                          STAID, STANAM, CTYPE )
                  IF (FLAGS(3) .EQ. 2) THEN
C                   write list
                    WRITE(FOUT,2000) DSNBUF(N), STAID, STANAM, CTYPE,
     $                               (COUNT(J,N), J = 1, 2),
     $                               (MINMAX(J),  J = 1, 2),
     $                               QFLG, (QC(J), J = 1, 12),
     $                               (COUNT(J,N), J = 3, 6),
     $                               (STATS(J,N), J = 1, 3)
                  ELSE
C                   write wide table
                    WRITE(FOUT,2031) DSNBUF(N), (STATS(J,N), J = 1, 3),
     $                               (COUNT(J,N), J = 1, 6),
     $                               (MINMAX(J),  J = 1, 2),
     $                               QFLG, (QC(J), J = 1, 12),
     $                               CTYPE, STAID
                  END IF
                END IF
              END IF
            ELSE
C             problem with data set
              COUNT(3,N) = 0
              COUNT(4,N) = 0
              COUNT(5,N) = RETCOD
              COUNT(6,N) = 1
              IF (FLAGS(3) .EQ. 2) THEN
C               print to list format
                WRITE (FOUT,2009) DSNBUF(N), (COUNT(J,N), J = 3, 6)
              ELSE IF (FLAGS(3) .GT. 2) THEN
C               print to table format
                WRITE (FOUT,2039)  DSNBUF(N), (COUNT(J,N), J = 3, 6)
              END IF
            END IF
          IF (N .LT. DSNCNT) GO TO 310
          OPT = 5
          COMP = 1
        ELSE IF (OPT .EQ. 5  .AND.  COMP .EQ. 0) THEN
C         can't View statistics until they are computed
          SGRP = 25
          CALL PRNTXT ( MESSFL, SCLU, SGRP )
C         default to Compute
          OPT = 4
        ELSE IF (OPT .EQ. 5) THEN
C         View computed statistics
          IF (DSNCNT .GT. 1) THEN
C           table statistics on screen
            SGRP = 26
            CALL Q2INIT ( MESSFL, SCLU, SGRP )
            ICOL = 1
            CALL Q2STIB ( L1, ICOL, DSNCNT, DSNBUF )
            ICOL = 2
            CALL Q2STIB ( L6, ICOL, DSNCNT, COUNT )
            CALL Q2SETR ( L3, DSNCNT, STATS )
            CALL Q2EDIT ( DSNCNT, RTCMND )
          ELSE
C           list single station on screen
            SGRP = 27
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            CALL QSETR  ( L3, STATS )
            CALL QSETI  ( L1, DSNBUF(1) )
            CALL QSETIB ( L6, L2, COUNT(1,1) )
            CALL Q1EDIT ( RTCMND )
          END IF
C         default to Select
          OPT = 1
        END IF 
C
      IF (OPT .NE. 6) GO TO 10
      IF (FOUT .GT. 0) THEN
C       close output file
        KEEP = 0
        CALL QFCLOS ( FOUT, KEEP )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   KNTDAT
     I                   ( WDMSFL, MESSFL, DSN,                    
     O                     DSNTYP, DATES, RETCOD )
C
C     + + + PURPOSE + + +
C     Determine type of data set (Time series, Table) and
C     period of available data.  For Table data sets, also
C     retrieves data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSN, MESSFL, DSNTYP, DATES(6,2), RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the wdm file
C     DSN    - data set number
C     DSNTYP - type of data set
C              1 - time series
C              2 - data
C              others - invalid type
C     DATES  - available period of record
C              (n,1) - begin yr, mo, dy, hr, mn, sc
C              (n,2) - end yr, mo, dy, hr, mn, sc
C     RETCOD - return code
C              0 - everything successful
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ckntau.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      TABID, J407FG, INIT, SDATE(6), EDATE(6),
     $             TBCNT, LREC, TGRPT, TGRP, TQNU,
     $             REC1, GPFLG, MPKS
      CHARACTER*1  MFID(2)
      CHARACTER*16 TNAME
C
C     + + + FUNCTIONS + + +
      INTEGER   WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDCKDT, WTFNDT, WDTBFX, GETTB
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  GPFLG, TABID, TNAME,             J407FG, INIT
     $     /    1,     1, 'AN.PEAKS        ',     0,   1 /
C
C     + + + END SPECIFICATIONS + + +
C
C     check for data set type
      DSNTYP = WDCKDT ( WDMSFL, DSN )
      IF (DSNTYP .EQ. 1) THEN
C       time series data set, get start and end date
        CALL WTFNDT ( WDMSFL, DSN, GPFLG,
     O                 REC1, SDATE, EDATE, RETCOD )
        IF (RETCOD .EQ. 0) THEN
C         force to calendar year
          IF (SDATE(2) .GT. 1  .OR.  SDATE(3) .GT. 1)
     $       SDATE(1) = SDATE(1) + 1
          IF (EDATE(2) .LT. 12  .OR.  EDATE(3) .LT. 31)
     $        EDATE(1) = EDATE(1) - 1
          IF (SDATE(1) .LT. EDATE(1)) THEN
C           dates look ok
            DATES(1,1) = SDATE(1)
            DATES(1,2) = EDATE(1)
          ELSE
C           must be at least 2 data values
            RETCOD = 8
          END IF
        END IF
      ELSE IF (DSNTYP .EQ. 2) THEN
C       table data set, get table information
        CALL WDTBFX ( WDMSFL, DSN, TABID, TNAME,
     O                TBCNT, LREC, TGRPT, MFID, TGRP, TQNU, NROW,
     O                RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         get full available record
          MPKS = MAXPKS
          CALL GETTB ( MESSFL, TNAME, WDMSFL, DSN,
     I                 MPKS, J407FG, INIT,
     M                 NROW,
     O                 PK, WYR, QCOD, RETCOD ) 
          IF (NROW .LE. 1  .OR.  RETCOD .NE. 0) THEN
C           must be at least 2 data values
            RETCOD = 8
          ELSE
C           save dates
            DATES(1,1) = WYR(1)
            DATES(1,2) = WYR(NROW)
          END IF
        END IF
      ELSE
C       data set is neither time series nor table
        RETCOD = -63
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   KNTMOD
     I                   ( MESSFL, SCLU, DSN, DSNTYP,
     M                     QFLG, TRANSF, DATES, MINMAX, QC )
C
C     + + + PURPOSE + + +
C     Let user modify processing options for period of analysis,
C     range of data, qualification codes, and (for time series
C     data sets) transformation code.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, SCLU, DSN, DSNTYP,
     $            QFLG, TRANSF, DATES(6,2)
      REAL        MINMAX(2)
      CHARACTER*1 QC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     SCLU   - message cluster group
C     DSN    - data set number
C     DSNTYP - data set type
C              1 - time series data set
C              2 - table data set
C     QFLG   - quality code for time series data set
C     TRANSF - transformation code for time series data set
C     DATES  - period of record to be analyzed
C     MINMAX - range of data to be considered
C     QC     - qualification codes for table data set
C
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SGRP, AGAIN, INUM, IVAL(4), IVMN(2), IVMX(2),
     $          LEN1, LEN2, LEN12, CVAL(1), RTCMND
C
C     + + + EXTERNALS + + +
      EXTERNAL  Q1INIT, QSETI, QSETR, QSETCO, QSTCTF, QSCSTI
      EXTERNAL  Q1EDIT, QGETI, QGETR, QGETCO, QGTCTF
      EXTERNAL  PRNTXT
C
C     + + + DATA INITIALIZATION + + +
      DATA  LEN1, LEN2, LEN12
     $     /   1,    2,    12 /
C
C     + + + END SPECIFICATIONS + + +
C
C     allow user to modify defaults by data set
 100  CONTINUE
        AGAIN = 0
        IF (DSNTYP .EQ. 1) THEN
C         time series data set
          SGRP = 15
          INUM = 4
          IVAL(3) = QFLG
          CVAL(1) = TRANSF + 1
        ELSE
C         table data set
          SGRP = 16
          INUM = 3
        END IF
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
C       default, min, & max for date fields
        IVAL(1) = DATES(1,1)
        IVAL(2) = DATES(1,2)
        IVAL(INUM) = DSN
        IVMN(1) = IVAL(1)
        IVMN(2) = IVAL(1)
        IVMX(1) = IVAL(2)
        IVMX(2) = IVAL(2)
        CALL QSCSTI ( LEN2, LEN1, IVMN, IVMX, IVAL )
C       initialize data fields
        CALL QSETI  ( INUM, IVAL )
        CALL QSETR  ( LEN2, MINMAX )
        IF (DSNTYP .EQ. 1) THEN
C         set tranformation code for time series data set
          CALL QSETCO ( LEN1, CVAL )
        ELSE 
C         set qualification codes for table data set
          CALL QSTCTF ( LEN1,  LEN12, QC )
        END IF
C       edit screen
        CALL Q1EDIT ( RTCMND )
        IF (RTCMND .EQ. 1) THEN
C         user wants to continue, get values
          CALL QGETI ( INUM, IVAL )
          CALL QGETR ( LEN2, MINMAX )
          IF (IVAL(1) .LT. IVAL(2)) THEN
C           at least 2 data values, ok to continue
            DATES(1,1) = IVAL(1)
            DATES(1,2) = IVAL(2)
          ELSE
C           not enough data values
            AGAIN = 1
            SGRP = 14
            CALL PRNTXT ( MESSFL, SCLU, SGRP )
          END IF
          IF (DSNTYP .EQ. 1) THEN
C           transformation and qualification codes
            CALL QGETCO ( LEN1, CVAL )
            TRANSF = CVAL(1) - 1
            QFLG = IVAL(3)
          ELSE
C           qualification codes
            CALL QGTCTF ( LEN1, LEN12, QC )
          END IF
        ELSE IF (RTCMND .EQ. -1) THEN
C         oops, try again
          AGAIN = 1
        ELSE
C         assume Prev, leave unchanged
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
C         
      RETURN
      END
C
C
C
      SUBROUTINE   KNTCMP
     I                   ( WDMSFL, DSN, DSNTYP,
     I                     MINMAX, DATES, QFLG, TRANSF, QC, NC,
     O                     COUNT, STATS )
C
C     + + + PURPOSE + + +
C     Sets up data for calls to KENT, which computes the
C     statistics.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL, DSN, DSNTYP, DATES(6,2), QFLG, TRANSF,
     $            NC, COUNT(6)
      REAL        MINMAX(2), STATS(3)
      CHARACTER*1 QC(12)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of wdm file
C     DSN    - data set number
C     DSNTYP - data set type
C              1 - time series data set
C              2 - table data set
C     MINMAX - range of data to be considered in analysis
C              (1) - minimum value
C              (2) - maximum value
C     DATES  - period of record to be analyzed
C              (n,1) - begin year,month,day,hour,minute,second
C              (n,2) - ending year,month,day,hour,minute,second
C     QFLG   - quality code for time series data set
C     TRANSF - transformation code for time series data set
C     QC     - qualification codes for table data set
C     NC     - number of qualification codes in QC
C     COUNT  - metrics for analyisis
C              (1) - begin year
C              (2) - end year
C              (3) - number of years used
C              (4) - number of years not used
C              (5) - return code, if count(6) > 0, last non-zero
C                     0 - no problems
C                    -1 - too many years
C                    -3 - not enough good years
C                    -8 - invalid date
C                   -14 - date specified not within valid range for dsn
C                   -20 - problem with one or more of following:
C                         GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT
C                   -21 - date from WDM doesn't match expected date
C                   -82 - data set exists, but is wrong DSTYP
C     STATS  - computed statistics for Kendall's Tau
C              (1) - Kendall's Tau
C              (2) - P-level
C              (3) - median slope of trend
C              (6) - count of non-zero returns
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ckntau.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, IPOS, L1, L12, TS, TU, FLG, KEEP, SKIP, M, NVAL,
     $          RETCOD
      REAL      BAD, QM(MAXPKS)
C
C     + + + FUNCTIONS + + +
      INTEGER   STRFND
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   TIMDIF, STRFND, WDTGET, ZIPR
      EXTERNAL   KENT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   L1, L12, TS, TU, BAD
     $      / 1,  12,  1,  6, -999999. /
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize stats and count
      STATS(1) = BAD
      STATS(2) = BAD
      STATS(3) = BAD
      COUNT(1) = 0
      COUNT(2) = 0
      COUNT(3) = 0
      COUNT(4) = 0
      COUNT(5) = 0
      COUNT(6) = 0
      RETCOD = 0
C
C     how many years?
      CALL TIMDIF ( DATES(1,1), DATES(1,2), TU, TS, NVAL)
      COUNT(1) = DATES(1,1)
      COUNT(2) = DATES(1,2)
      IF (NVAL .LE. MAXPKS) THEN
C       number of years is ok
        IF (DSNTYP .EQ. 1) THEN
C         time series data set
          CALL WDTGET (WDMSFL, DSN, TS, DATES, NVAL,
     I                         TRANSF, QFLG, TU, 
     O                         QM, RETCOD)
        ELSE
C         table data set, fill QM array
          CALL ZIPR ( NVAL, BAD, QM )
          DO 100 I = 1, NROW
C           check each row
            IPOS = WYR(I) + 1 - DATES(1,1)
            IF (IPOS .GE. 1 .AND. IPOS .LE. NVAL) THEN
C             year within requested range
              KEEP = 1
              IF (ABS(QM(IPOS)-BAD) .GT. .1) THEN
C               there is already a value for this year, assume peaks
                IF (PK(I) .LE. QM(IPOS)) THEN
C                 new peak is smaller, Keep old
                  KEEP = 0
                END IF
              END IF
              IF (KEEP. EQ. 1) THEN
C               check quality flags
                FLG = 0
                DO 35 M = 1,NC
C                 check for peaks with unwanted qualification codes
                  IF (QC(M) .NE. ' ') THEN
                    FLG = FLG + STRFND ( L12, QCOD(1,I), L1, QC(M) )
                  END IF
 35             CONTINUE
                IF (FLG .EQ. 0) THEN
C                 add to the array
                  QM(IPOS) = PK(I)
                END IF
              END IF
            END IF
 100      CONTINUE
        END IF
      ELSE
C       too many years
        RETCOD = -1
      END IF
C
      IF (RETCOD .EQ. 0) THEN
C       check for invalid or undesired values
        SKIP = 0
        DO 55 I = 1,NVAL
          IF (QM(I) .LT. MINMAX(1)) QM(I) = -999999.
          IF (QM(I) .GT. MINMAX(2)) QM(I) = -999999.
          IF (QM(I) .LE. BAD)  SKIP = SKIP + 1
 55     CONTINUE
        COUNT(3) = NVAL - SKIP
        COUNT(4) = SKIP
        IF (COUNT(3) .GT. 2) THEN
C         ok do the analysis
          CALL KENT ( QM, NVAL, STATS(1), STATS(2), STATS(3) )
        ELSE
C         not enough good data values
          RETCOD = -3
          COUNT(5) = RETCOD
          COUNT(6) = COUNT(6) + 1
        END IF
      ELSE
C       problem getting data
        COUNT(5) = RETCOD
        COUNT(6) = COUNT(6) + 1
      END IF
C
      RETURN
      END
