C
C
C
      SUBROUTINE   PRBAST
     I                    (MESSFL,WDMFL,
     M                     DSNCNT,DSNBMX,DSNBUF)
C
C     + + + PURPOSE + + +
C     This routine computes the basic statistics minimum, maximum,
C     mean, and standard deviation for data in time-series data sets.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMFL,DSNCNT,DSNBMX,DSNBUF(DSNBMX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     WDMFL  - Fortran unit number of WDM file
C     DSNCNT - number of data sets in the buffer
C     DSNBMX - size of data set buffer
C     DSNBUF - array of data set numbers to be processed
C
C     + + + PARAMETERS + + +
C     note this must be greater than or equal to DSNBMX
C     ie, quick fix
      INTEGER BMXDSN 
      PARAMETER (BMXDSN = 300)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU, SGRP, FOUT,
     $             I, N, LEN, LEN1, LEN3, LEN4, LEN15, AGAIN,
     $             QFLG, TRAN, NVAL, CNTS(4,BMXDSN), FLAGS(3),
     $             RETCOD, RETC, RET, RTCMND, RESP, ZEROI,
     $             PREV,         ON, OFF, ICOL, COMP, INDX(4),
     $             DATES(15), DATEST(15), CONT, FIRST, CERR,
     $             IWRT, SCNI
      REAL         TSFILL, STAT(4,BMXDSN), ZEROR
      CHARACTER*8  PTHNAM
C
C     + + + LOCAL DEFINITIONS + + +
C     FLAGS  - array of option flags
C              (1) - date/time: 1-Defalut,2-Same,3-Specify
C              (2) - wdm output: 1-yes,2-no
C              (3) - file output: 1-yes,2-no
C     DATES  - array containing begin and end dates and transformation
C              (1-6)  - begin year, month, day, hour, minute, second
C              (7-12) - end year, month, day, hour, minute, second
C              (13)   - time step
C              (14)   - time units code
C              (15)   - transformation 1-rate,2-sum,3-max-4-min
C     CNTS   - contains good and bad counts and return codes
C              (1) - number of values used to compute statistics
C              (2) - number of values not used to compute statistics
C              (3) - return code
C              (4) - number of wdm errors.
C
C     + + + EXTERNALS + + +
      EXTERNAL     COPYI, ZIPI, ZIPR, TIMDIF
      EXTERNAL     WDBSAR
      EXTERNAL     PRWMSE
      EXTERNAL     PRNTXI, PRNTXT, PMXTXI
      EXTERNAL     QRESP,  QFOPEN, ZSTCMA, ZGTRET
      EXTERNAL     Q1INIT, QSETI,  QSETCO, QSETR,  QSETIB
      EXTERNAL     Q1EDIT, QGETI,  QGETCO
      EXTERNAL     Q2INIT, Q2EDIT, Q2SETR, Q2STIB
      EXTERNAL     WDGTTM, STBASD, STBASS, PMXTFT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  SCLU,  LEN1, LEN3, LEN4, LEN15, ZEROR, ZEROI
     $     / 151,     1,    3,    4,    15,   0.0,     0 /
      DATA  FLAGS,  ON, OFF, PREV, IWRT, INDX
     $    / 1,2,2,   1,   0,    4,    1, 12,13,14,15 /
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT ( 1X, I5, 4F12.3, 1X, 2I8, I5, I3 )
C
C     + + + END SPECIFICATIONS + + +
C
      QFLG= 30
      COMP = 0
C     Basic options, default to start with Select
      RESP = 1
 10   CONTINUE
C       option: 1-Select,2-Define,3-Compute,4-View,5-Return
        SGRP = 1
        CALL QRESP (MESSFL,SCLU,SGRP,RESP)
        IF (RESP.EQ.1) THEN
C         Select data sets for analysis
          PTHNAM = 'SB      '
          CALL PRWMSE (MESSFL,WDMFL,DSNBMX, PTHNAM,
     M                 DSNBUF,DSNCNT)
C         default Basic option to Define, computations to no
          RESP = 2
          COMP = 0
        ELSE IF (RESP.EQ.2) THEN
C         Define input and ouput options, allow previous
          CALL ZSTCMA ( PREV, ON )
  200     CONTINUE
            AGAIN = 0
            SGRP = 5
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
C           flags: 1-date/time,2-wdm,3-file, and quality code
            CALL QSETCO ( LEN3, FLAGS )
            CALL QSETI  ( LEN1, QFLG )
            CALL Q1EDIT (RTCMND )
            IF (RTCMND .EQ. 1) THEN
C             user wants to continue, get values
              CALL QGETCO ( LEN3, FLAGS )
              CALL QGETI  ( LEN1, QFLG )
              IF (FLAGS(3) .EQ. 1) THEN
C               open an output file
                SGRP = 6
                CALL QFOPEN ( MESSFL, SCLU, SGRP, FOUT, RETC )
C               get user exit command
                CALL ZGTRET ( RTCMND )
                IF (RTCMND .EQ. 2) THEN
C                 Prev, back to Define screen
                  AGAIN = 1
                ELSE IF (RETC .NE. 0) THEN
C                 problem opening output file, don't use
                  FLAGS(3) = 2
                END IF
              END IF
            ELSE IF (RTCMND .EQ. -1) THEN
C             oops, try again
              AGAIN = 1
            ELSE
C             assume Prev, leave unchanged
            END IF
          IF (AGAIN .EQ. 1) GO TO 200
C         turn off Prev and default Basic option to Compute
          CALL ZSTCMA ( PREV, OFF )
          RESP = 3
        ELSE IF (RESP .EQ. 3  .AND.  DSNCNT .LE. 0) THEN
C         can't Compute statistics, no data sets selected.
          SGRP = 2
          CALL PRNTXT ( MESSFL, SCLU, SGRP )
C         default Basic option to Select
          RESP = 1
        ELSE IF (RESP .EQ. 3) THEN
C         Compute statistics
           IF (FLAGS(3) .EQ. 1) THEN
C            write heading to output file
             SGRP = 15
             CALL PMXTFT ( MESSFL, FOUT, SCLU, SGRP )
          END IF
          CONT = 1
          CERR = 0
          SCNI = 1
          DATEST(15) = 1
          N = 0
 300      CONTINUE
            N = N + 1
            CALL ZIPR ( LEN4, ZEROR, STAT(1,N) )
            CALL ZIPI ( LEN4, ZEROI, CNTS(1,N) )
            CALL WDGTTM ( WDMFL, DSNBUF(N),
     O                    DATEST(1), DATEST(7), DATEST(13), DATEST(14),
     O                    TSFILL, RETCOD )
            IF (RETCOD .NE. 0) THEN
C             skipping dsn, problem with data
              SGRP = 10
              CALL PRNTXI ( MESSFL, SCLU, SGRP, DSNBUF(N) )
              CNTS(3,N) = RETCOD
              CNTS(4,N) = 1
              CERR = CERR + 1
              SCNI = 1
            ELSE
C             data set is probably ok, get dates
              IF (FLAGS(1) .EQ. 1) THEN
C               use Default dates
                CALL COPYI ( LEN15, DATEST, DATES )
              ELSE IF (FLAGS(1) .EQ. 3) THEN
C               Specify date for each data set
                CALL STBASD ( MESSFL, DATEST, RETCOD )
                IF (RETCOD .NE. 7) THEN
C                 user accepted or modified date
                  CALL COPYI ( LEN15, DATEST, DATES )
                ELSE
C                 interrupt, stop computing statistics
                  CONT = 0
                  CERR = CERR + 1
                END IF
                SCNI = 1
              ELSE IF (FLAGS(1) .EQ. 2  .AND.  FIRST .EQ. 1) THEN
C               define Common date with first good data set
                CALL STBASD ( MESSFL, DATEST, RETCOD )
                CALL COPYI ( LEN15, DATEST, DATES )
                FIRST = 0
                SCNI = 1
              END IF
              IF (CONT .EQ. 1) THEN
C               computing basic statistics for dsn &.
                SGRP = 11
                CALL PMXTXI ( MESSFL, SCLU, SGRP, LEN1,
     I                        SCNI, IWRT, LEN1, DSNBUF(N) )
                SCNI = -1
C               compute number of time steps
                CALL TIMDIF ( DATES(1),DATES(7),DATES(14), DATES(13),
     O                        NVAL )
                TRAN = DATES(15) - 1
                CALL STBASS ( WDMFL, DSNBUF(N),
     I                        DATES, DATES(13), DATES(14),
     I                        NVAL, TRAN, QFLG, TSFILL,
     O                        STAT(1,N), CNTS(1,N), RETC )
                IF (FLAGS(2) .EQ. 1) THEN
C                 save statistics attributes to wdm file
                  DO 350 I = 1, 4
                    CALL WDBSAR ( WDMFL, DSNBUF(N), MESSFL,
     I                            INDX(I), LEN1, STAT(I,N),
     O                            RET )
                    IF (RET .NE. 0) THEN
C                     problem adding data
                      CNTS(3,N) = RET
                      CNTS(4,N) = CNTS(4,N) + 1
                    END IF
  350             CONTINUE
                END IF
                CERR = CERR + CNTS(4,N)
              END IF
           END IF
           IF (CONT .EQ. 1  .AND.  FLAGS(3) .EQ. 1) THEN
C             save statistics to a file
              IF (CNTS(4,N) .EQ. 0) THEN
C               no wdm read or write errors
                WRITE (FOUT,2010) DSNBUF(N), (STAT(I,N),I=1,4),
     $                            (CNTS(I,N),I=1,2)
              ELSE
C               problems adding attributes, include remarks
                WRITE (FOUT,2010) DSNBUF(N), (STAT(I,N),I=1,4),
     $                            (CNTS(I,N),I=1,4)
              END IF
              IF (CERR .GT. 0) THEN
C               at least one error occured, write return codes to file
                SGRP = 16
                CALL PMXTFT ( MESSFL, FOUT, SCLU, SGRP )
              END IF
            END IF
          IF (N .LT. DSNCNT  .AND.  CONT .EQ. 1) GO TO 300
C         finished computing statistics for selected data sets
          SGRP = 12
          CALL PRNTXT ( MESSFL, SCLU, SGRP )
C         default basic option to View, set computation to yes
          RESP = 4
          COMP = 1
        ELSE IF (RESP .EQ. 4  .AND.  COMP .EQ. 0) THEN
C         can't View statistics until they are Computed
          SGRP = 3
          CALL PRNTXT ( MESSFL, SCLU, SGRP ) 
C         default to Compute
          RESP = 3
        ELSE IF (RESP .EQ. 4) THEN
C         View computed statistics
          IF (DSNCNT .GT. 1) THEN
C           table statistics on screen
            SGRP = 17
            CALL Q2INIT ( MESSFL, SCLU, SGRP )
            ICOL = 1
            CALL Q2STIB ( LEN1, ICOL, DSNCNT, DSNBUF )
            ICOL = 2
            CALL Q2STIB ( LEN4, ICOL, DSNCNT, CNTS )
            CALL Q2SETR ( LEN4, DSNCNT, STAT )
            CALL Q2EDIT ( DSNCNT, RTCMND )
          ELSE
C           list single station on screen
            IF (CERR .EQ. 0) THEN
C             no errors, don't need return codes
              SGRP = 18
              LEN = 2
            ELSE
C             errors, include return codes
              SGRP = 19
              LEN = 4
            END IF
            CALL Q1INIT ( MESSFL, SCLU, SGRP )
            CALL QSETR ( LEN4, STAT(1,N) )
            CALL QSETI ( LEN,  CNTS(1,N) )
            CALL QSETIB ( LEN1, LEN+1, DSNBUF(1) )
            CALL Q1EDIT ( RTCMND )
          END IF
C         default to Select
          RESP = 1
        END IF
      IF (RESP.NE.5) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   STBASS
     I                   ( WDMFL, DSN, DATES, TS, TU, TVAL,
     I                     TRAN, QFLG, TSFILL,
     O                     STAT, CNTS, RETC )
C
C     + + + PURPOSE + + +
C     Compute basic statistics for a time series in a wdm file.
C     Statistics include minimum, maximum, mean, and standard
C     deviation.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL, DSN, DATES(6), TS, TU, TVAL, TRAN, QFLG,
     $          CNTS(4), RETC
      REAL      TSFILL, STAT(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of WDM file
C     DSN    - number of the data set to be analyzed
C     DATES  - starting date of period to be analyzed
C     TS     - time step, in TU units, to use for analysis
C     TU     - time units to use for analysis
C     TVAL   - number of time steps to be analyzed
C     TRAN   - transformation 0-rate,1-total,2-min,3-max
C     QFLG   - qualification code to use for data retrievals
C     TSFILL - missing value indicator
C     STAT   - array containing comuted statistics
C              (1) minimum value in time period
C              (2) maximum value in time period
C              (3) mean value over the time period
C              (4) standard deviatin over the time period
C     CNTS   - counts used in computation of statistics
C              (1) number of values used
C              (2) number of values not used (bad or missing)
C              (3) return code (last error code if error count > 1)
C              (4) error count
C     + + + PARAMETERS + + +
      INTEGER   BFMAX
      PARAMETER ( BFMAX = 3660 )
C
C     + + + LOCAL VARIABLES + + +
      INTEGER           GCNT, BCNT, I, DATE(12), AGAIN, NVAL,
     $                  LENS, LEN6
      REAL              VALUES(BFMAX), RMAX, RMIN, ZERO
      DOUBLE PRECISION  DVAL, DSUM, DSQR, DCNT, DMNV, DSTD, DVAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   LENS, LEN6, ZERO
     $      /   4,    6,  0.0 /
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize statisitics and counters
      DSUM = 0.0
      DSQR = 0.0
      RMAX = -1.0E20
      RMIN =  1.0E20
      GCNT = 0
      BCNT = 0
C     initialize date and values to get
      CALL COPYI ( LEN6, DATES, DATE )
      IF (TVAL .LE. BFMAX) THEN
C       all data will fit in value buffer
        NVAL = TVAL
      ELSE
C       buffer too small for all of the data values
        NVAL = BFMAX
      END IF
C
      AGAIN = 1
  100 CONTINUE
C       get time series data
        CALL WDTGET ( WDMFL, DSN, TS, DATE, NVAL, TRAN, QFLG, TU,
     O                VALUES, RETC )
        IF (RETC .EQ. 0) THEN
C         compute statistics
          DO 125 I = 1, NVAL
            IF (ABS (VALUES(I)-TSFILL) .GT. 1.0E-9) THEN
C             data value not missing, use double precision for statistics
              GCNT = GCNT + 1
              DVAL = DBLE ( VALUES(I) )
              DSUM = DSUM + DVAL
              DSQR = DSQR + DVAL * DVAL
              IF (VALUES(I) .GT. RMAX) THEN
C               new maximum value, update
                RMAX = VALUES(I)
              END IF
              IF (VALUES(I) .LT. RMIN) THEN
C               new minimum value, update
                RMIN = VALUES(I)
              END IF
            ELSE
C             missing data value
              BCNT = BCNT + 1
            END IF
  125     CONTINUE
C
          IF (GCNT+BCNT .LT. TVAL) THEN
C           still more data
            CALL TIMADD ( DATE, TU, TS, NVAL, DATE(7) )
            CALL COPYI  ( LEN6, DATE(7), DATE )
            NVAL = TVAL - ( GCNT + BCNT )
            IF (NVAL .GT. BFMAX) THEN
C             too many values still left for buffer
              NVAL = BFMAX
            END IF
          ELSE
C           all data has been read
            AGAIN = 0
          END IF
        ELSE
C         problem retrieving data from wdm file, give it up
          AGAIN = 0
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
C
      IF (RETC .EQ. 0) THEN
C       all data read, compute mean, variance, standard deviation
        CNTS(1) = GCNT
        CNTS(2) = BCNT
        IF (GCNT .GT. 0) THEN
C         at least one good data value
          DCNT = DBLE ( GCNT )
          DMNV = DSUM / DCNT
          DVAR = (DSQR - DCNT*DMNV*DMNV)/(DCNT - DBLE(1.0))
          DSTD = DSQRT ( DVAR )
C         return data values as real (not double precision)
          STAT(1) = RMIN
          STAT(2) = RMAX
          STAT(3) = REAL ( DMNV )
          STAT(4) = REAL ( DSTD )
          CNTS(3) = 0
        ELSE
C         no good data values
          CALL ZIPR ( LENS, ZERO, STAT )
          CNTS(3) = -99
          CNTS(4) = 1
        END IF
      ELSE
C       problem retrieving all of data, statistics not computed
        CALL ZIPR ( LENS, ZERO, STAT )
        CNTS(1) = 0
        CNTS(2) = 0
        CNTS(3) = RETC
        CNTS(4) = 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   STBASD
     I                   ( MESSFL,
     M                     DATMT,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Get a start date, end date, time step, and transformation from
C     the user.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, DATMT(15), RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     DATMTE - begin and end dates, time step, and transformation
C              (1-6)  - begin year, month, day, hour, minute, second
C              (7-12) - end year, month, day, hour, minute, second
C              (13)   - time step
C              (14)   - time units code
C              (15)   - transformation 1-rate,2-sum,3-max-4-min
C     RETC   - return code
C              0 - ok
C              7 - interrupt, give it up
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   AGAIN, SGRP, SCLU, RET, RET2, LEN2, LEN13, LEN15,
     $          DATMTX(15), PRFLG, INTRPT, ON, OFF, RTCMND
      CHARACTER*20 WNDWNM
C
C     + + + EXTERNALS + + +
      EXTERNAL  Q1INIT, QSETI, QSETCO, Q1EDIT, QGETI, QGETCO 
      EXTERNAL  ZSTCMA, PRNTXT
      EXTERNAL  CKDATE, CKTSTU, COPYI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   SCLU, LEN2, LEN13, LEN15, PRFLG, INTRPT, ON, OFF
     $     /  151,    2,    13,     15,    1,     16,  1,   0 /
      DATA   WNDWNM
     $     / 'Dates (SBGD) problem' /
C     + + + END SPECIFICATIONS + + +
C
C     turn on interrupt
      CALL ZSTCMA ( INTRPT, ON )
C     get begin and end dates, time step and transformation
 100  CONTINUE
        AGAIN = 0
        SGRP = 20
        CALL Q1INIT ( MESSFL, SCLU, SGRP )
        CALL QSETI  ( LEN13, DATMT )
        CALL QSETCO ( LEN2, DATMT(14) )
        CALL Q1EDIT ( RTCMND )
        IF (RTCMND .EQ. 1) THEN
C         user wants to continue, get values
          CALL QGETI  ( LEN13, DATMTX )
          CALL QGETCO ( LEN2, DATMTX(14) )
C         check dates
          CALL CKDATE ( DATMTX(1), DATMTX(7), RET )
          IF (RET .EQ. 1  .OR.  RET .EQ. 0) THEN
C           start date follows end date or is the same as end
            SGRP = 21
            CALL PRNTXT ( MESSFL, SCLU, SGRP )
            AGAIN = 1
          ELSE
C           start and end date ok, check time step
            CALL CKTSTU ( DATMTX(13), DATMTX(14), PRFLG, WNDWNM,
     O                    RET )
            IF (RET .NE. 0) THEN
C             time step and time units not compatible
              AGAIN = 1
            END IF
          END IF
        ELSE IF (RTCMND .EQ. -1) THEN
C         oops, try again
          AGAIN = 1
        ELSE IF (RTCMND .EQ. 7) THEN
C         interrupt, give it up
          AGAIN = -1
        ELSE
C         assume Prev, leave dates unchanged
          AGAIN = -2
        END IF
        IF (AGAIN .EQ. 0) THEN
C         looks ok so far, make sure time period in valid range
          CALL CKDATE ( DATMT(1), DATMTX(1), RET )
          CALL CKDATE ( DATMTX(7), DATMT(7), RET2 )
          IF (RET .EQ. 1  .OR.  RET2 .EQ. 1) THEN
C           date selected outside valid range
            SGRP = 22
            CALL PRNTXT ( MESSFL, SCLU, SGRP )
            AGAIN = 1
          END IF
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
C
      IF (AGAIN .EQ. 0) THEN
C       return selected dates and time step
        CALL COPYI ( LEN15, DATMTX, DATMT )
        RETC = 0
      ELSE IF (AGAIN .EQ. -1) THEN
C       interrupt, give it up
        RETC = 7
      ELSE IF (AGAIN .EQ. -2) THEN
C       assume previous, don't change
        RETC = 0
      END IF
C
C     turn off interrupt
      CALL ZSTCMA ( INTRPT, OFF )
C
      RETURN
      END
