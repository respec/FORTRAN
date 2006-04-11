C
C
C
      SUBROUTINE   GNMEXE
     I                   ( WDMSFL, DSN, OPT, NIN,
     I                     TSTEP, TUNIT, STRT, STPP, TBLSIZ, C1, C2, K1,
     O                     ERRFLG, SUM, KOUNT )
C
C     + + + PURPOSE + + +
C     Perform the specified math compuations in Timeseries Generate.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSN(3), OPT, NIN, TSTEP(3), ERRFLG,
     $          TUNIT(3), STRT(6), STPP(6), TBLSIZ, K1, KOUNT
      REAL      C1(TBLSIZ), C2(TBLSIZ), SUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set numbers
C              (1) - output data set
C              (2) - input data set, T1
C              (3) - optional second input data set, T2
C     OPT    - algebra option
C               1 - DONE    2 - +C      3 - *C
C               4 - ADD     5 - SUB     6 - MULT
C               7 - DIV     8 - MEAN    9 - WGHT
C              10 - **C    11 - C**    12 - POW
C              13 - LGE    14 - LG10   15 - LGE
C              16 - ABS    17 - Z-     18 - Z+
C              19 - MIN    20 - MAX    21 - SUM
C              22 - SIGF   23 - LINE   24 - TABLE
C     NIN    - count of input data sets
C     TSTEP  - time step of the data sets
C     TUNIT  - time units of the data sets
C     STRT   - requested start date and time
C     STPP   - requested stop date and time
C     TBLSIZ - array size of real coefficients
C     C1     - real coefficient, required for
C              OPT = 2, 3, 9, 11, 17, 18 and 23,
C              optional for OPT = 10
C     C2     - real coefficient, required for
C              OPT = 17, 18, and 23
C     K1     - integer coefficient, required for
C              OPT = 21, optional for OPT = 10
C     ERRFLG - error flag to return
C     KOUNT  - count of math errors
C     SUM    - sum of values from algebra
C
C     + + + PARAMETERS + + +
      INTEGER     NSIZ
      PARAMETER  (NSIZ = 60000)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, I1, I6, SAIND, SALEN, N, RETCOD,
     $            NGET, NGOT, NVAL, GRP,
     $            DATE(6), DATET(6), QFGI, QFGO, DTRAN, DTOVWR
      REAL        BUFIN1(NSIZ), BUFIN2(NSIZ), BUFOUT(NSIZ)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDBSGI, TIMDIF, WTEGRP, COPYI, TIMADD, TSBWDS, TSBTIM
      EXTERNAL    ALGEBR, WDTPUT, TSBGET
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I6 = 6
      QFGI  = 31
      QFGO  = 0
      DTRAN = 0
      DTOVWR= 0
C
C     get minimum group
      GRP = 6
      DO 80 I = 1,3
        IF (DSN(I) .GT. 0) THEN
          SALEN = 1
          SAIND = 34
          CALL WDBSGI (WDMSFL, DSN(I), SAIND, SALEN, N, RETCOD)
          IF (RETCOD .NE. 0) N = 6
          IF (N .LT. GRP) GRP = N
        END IF
 80   CONTINUE
      CALL TIMDIF (STRT, STPP, TUNIT, TSTEP, NVAL)
      CALL COPYI ( I6, STRT, DATE )
C
      NGOT = 0
      NGET = NVAL
      IF (NGET .GT. NSIZ) NGET = NSIZ
C     check for group boundary
      CALL WTEGRP (STRT, GRP, DATET)
      CALL TIMDIF (STRT, DATET, TUNIT, TSTEP, N)
      IF (N .LE. NGET) NGET = N
      SUM = 0.0
      KOUNT = 0
      ERRFLG= 0
C
 200  CONTINUE
C       get first input time series
        CALL TSBWDS (WDMSFL,DSN(2))
        CALL TSBTIM (TUNIT,TSTEP,DTRAN,QFGI)
        CALL TSBGET (DATE,NGET,
     O               BUFIN1,RETCOD)
        IF (RETCOD .NE. 0) THEN
C         error in retrieval of time series
          ERRFLG = RETCOD
        END IF
        IF (NIN.EQ.2 .AND. ERRFLG.EQ.0) THEN
C         get second input time series
          CALL TSBWDS (WDMSFL,DSN(3))
          CALL TSBGET (DATE,NGET,
     O                 BUFIN2,RETCOD)
          IF (RETCOD .NE. 0) THEN
C           error in retrieval of time series
            ERRFLG = RETCOD
          END IF
        END IF
C
        IF (ERRFLG.EQ.0) THEN
C         compute the new time series
          CALL ALGEBR( I1, OPT, NGET, TBLSIZ, C1, C2, K1,
     I                 BUFIN1, BUFIN2,
     M                 SUM, KOUNT,
     O                 BUFOUT )
C
C         dump output buffer
          CALL WDTPUT( WDMSFL, DSN(1), TSTEP, DATE, NGET, DTOVWR,
     #                 QFGO, TUNIT, BUFOUT, RETCOD )
          IF (RETCOD .NE. 0) THEN
C           error in put operation
            ERRFLG = RETCOD
          ELSE
C           update dates and counters for any further retrievals
            NGOT = NGOT + NGET
            IF (NGOT .LT. NVAL) THEN
C             increment start date
              CALL TIMADD( DATE, TUNIT, TSTEP, NGET, DATET )
              CALL COPYI ( I6, DATET, DATE )
              IF(NGOT+NGET .GT. NVAL) NGET = NVAL - NGOT
C             check for group boundary
              CALL WTEGRP (DATE, GRP, DATET)
              CALL TIMDIF (DATE, DATET, TUNIT, TSTEP, N)
              IF (N .LE. NGET) NGET = N
            END IF
          END IF
        END IF
      IF (NGOT.LT.NVAL .AND. ERRFLG.EQ.0) GO TO 200
C
      RETURN
      END
C
C
C
      SUBROUTINE   GNTTRN
     I                    ( MESSFL, WDMSFL, DSN, NAORO,
     I                      DATES, DTRAN, TSTEP, TUNIT,
     O                      CRTFLG, CMPFLG, RETCOD )
C
C     + + + PURPOSE + + +
C     Transform a time series.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMSFL, DSN(2), NAORO, DATES(6,3), DTRAN,
     $          TSTEP, TUNIT, CRTFLG, CMPFLG, RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file with attributes
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data set numbers for the time series
C              (1) - input time-series data-set number
C              (2) - output time-series data-set number
C     NAORO  - output data set status flag
C              1 - new data set
C              2 - existing data set, append data
C              3 - existing data set, overwrite data
C     DATES  - dates for transformation, defaults to input dsn dates
C              (_,1) start date for time series to be transformed
C              (_,2) end date for time series to be transformed
C              (_,3) start date for transformed time series
C     DTRAN  - type of transformation
C              0 - rate (average or same)
C              1 - total (sum or divide)
C              2 - maximum
C              3 - minimum
C     TSTEP  - time step of transformed time series
C     TUNIT  - time units of transformed time series
C     CRTFLG - return flag for data set create status
C     CMPFLG - return flag for completion status
C     RETCOD - return code for error messages
C
C     + + + PARAMETERS + + +
      INTEGER      NSIZ
      PARAMETER  ( NSIZ = 60000 )
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, I1, I6, N, ILEN, IND, GRP, GRPI, GRPO,
     $             DATET(6), DATE(6), DATEN(6), DTOVWR,
     $             NGOT, NGET, NVAL, QFGI, QFGO, WDMFL1, WDMFL2,
     $             SAVAL(1), RETC, DSN1, DSN2
      REAL         BUFF(NSIZ)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBSGI, WTEGRP, TIMDIF, WDTPUT, TSBWDS, WID2UD
      EXTERNAL     TIMADD, COPYI, WDDSCL, WDBSAI, TSBTIM, TSBGET
C
C     + + + END SPECIFICATIONS + + +
C
      I1    = 1
      I6    = 6
      QFGI  = 31
      QFGO  = 0
      RETCOD= 0
      CMPFLG= 1
      CRTFLG= 0
C
      IF (NAORO.EQ.1) THEN
C       output data set is new, copy original's attributes to it
        I= 0
C        CALL WID2UD (I,DSN(1),
C     O               WDMFL1,DSN1)
        WDMFL1 = WDMSFL
        DSN1   = DSN(1)
C        CALL WID2UD (I,DSN(2),
C     O               WDMFL2,DSN2)
        WDMFL2 = WDMSFL
        DSN2   = DSN(2)
        CALL WDDSCL( WDMFL1, DSN1, WDMFL2, DSN2, I, RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         label copied, update time units
          IND= 17
          ILEN= 1
          SAVAL(1)= TUNIT
          CALL WDBSAI( WDMSFL, DSN(2), MESSFL, IND, ILEN, SAVAL,
     O                 RETCOD )
          IF (RETCOD .EQ. 0) THEN
C           time units updated, update time step
            IND= 33
            SAVAL(1)= TSTEP
            CALL WDBSAI( WDMSFL, DSN(2), MESSFL, IND, ILEN, SAVAL,
     O                   RETCOD )
          END IF
        END IF
        IF (RETCOD .EQ. 0) THEN
C         label copied and updated successfully
          CRTFLG = 1
        ELSE
C         some problem in label copy and update
          CRTFLG = 2
        END IF
      END IF
      IF (RETCOD .EQ. 0) THEN
C       get the data group size for the input and output data sets
        ILEN= 1
        IND = 34
        CALL WDBSGI( WDMSFL, DSN(1), IND, ILEN, GRPI, RETC )
        IF (RETC .NE. 0) GRPI = 6
        CALL WDBSGI( WDMSFL, DSN(2), IND, ILEN, GRPO, RETC )
        IF (RETC .NE. 0) GRPO = 6
        IF (GRPO .LE. TUNIT) THEN
C         problem with TGROUP for output data set
          IF (NAORO .EQ. 1) THEN
C           this is a new data set, fix it
            GRPO = TUNIT + 1
            IND = 34
            ILEN=1
            SAVAL(1) = GRPO
            CALL WDBSAI( WDMSFL, DSN(2), MESSFL, IND, ILEN, SAVAL,
     O                   RETCOD )
          ELSE
C           can't fix problem in existing data set
            CRTFLG = 3
            RETCOD = -1
          END IF
        END IF
      END IF
      IF (RETCOD .EQ. 0) THEN
C       determine the minimum group size to use
        GRP = 6
        IF (GRPI .LT. GRP) GRP = GRPI
        IF (GRPO .LT. GRP) GRP = GRPO
C       determine total number of values to transform
        !write (*,*) dates,tunit,tstep
        CALL TIMDIF( DATES(1,1), DATES(1,2), TUNIT, TSTEP, NVAL )
        CALL COPYI( I6, DATES(1,1), DATE )
C       make copy of starting date for transformed data
        CALL COPYI( I6, DATES(1,3), DATEN )
        NGOT = 0
        !write (*,*) '1 nget',NVAL
        NGET = NVAL
        IF (NGET .GT. NSIZ) NGET = NSIZ
        !write (*,*) '2 nget',NSIZ
C       check for group boundary
        CALL WTEGRP( DATES(1,1), GRP, DATET )
        CALL TIMDIF( DATES(1,1), DATET, TUNIT, TSTEP, N )
        IF (N .LT. NGET) NGET = N
        !write (*,*) '3 nget',N
        IF (NAORO.LE.2) THEN
C         either new data set of appending data to end of data set
          DTOVWR= 0
        ELSE
C         overwrite any existing data
          DTOVWR= 1
        END IF
 200    CONTINUE
C         read from input dataset
          CALL TSBWDS (WDMSFL,DSN(1))
          CALL TSBTIM (TUNIT,TSTEP,DTRAN,QFGI)
          !write (*,*) 'tsbget,date,nget,nsiz',date,nget,nsiz
          CALL TSBGET (DATE,NGET,
     O                 BUFF,RETCOD)
          !write (*,*) 'return from tsbget'
          IF (RETCOD .NE. 0) THEN
C           error in retrieval operation
            CMPFLG = 2
          ELSE
C           write to output dataset
            CALL WDTPUT( WDMSFL, DSN(2), TSTEP, DATEN, NGET,
     I                   DTOVWR, QFGO, TUNIT, BUFF,
     O                   RETCOD )
            IF (RETCOD .NE. 0) CMPFLG = 2
          END IF
          IF (CMPFLG .NE. 2) THEN
C           update counters
            NGOT = NGOT + NGET
            IF (NGOT .LT. NVAL) THEN
C             increment dates
              CALL TIMADD( DATE, TUNIT, TSTEP, NGET, DATET )
              CALL COPYI ( I6, DATET, DATE )
              CALL TIMADD( DATEN, TUNIT, TSTEP, NGET, DATET )
              CALL COPYI ( I6, DATET, DATEN )
              IF (NGOT+NGET .GT. NVAL) NGET = NVAL - NGOT
C             check for group boundary
              CALL WTEGRP( DATES(1,1), GRP, DATET )
              CALL TIMDIF( DATES(1,1), DATET, TUNIT, TSTEP, N )
              IF (N .LT. NGET) NGET = N
            END IF
          END IF
        IF (NGOT .LT. NVAL  .AND.  CMPFLG .NE. 2) GO TO 200
      ELSE
C       none of the data was transformed
        CMPFLG = 3
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GMANEX
     I                   (MESSFL,WDMSFL,DSN,TSTEP,TUNIT,STRT,STPP,
     I                    INTERP,C1,
     O                    ERRCOD,CRECOD)
C
C     + + + PURPOSE + + +
C     Perform the specified manual manipulations in Generate.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,TSTEP,TUNIT,STRT(6),STPP(6),INTERP,ERRCOD,
     $          CRECOD,MESSFL
      REAL      C1
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - unit number of message file with attributes
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number of data set to manipulate
C     TSTEP  - time step of the data set
C     TUNIT  - time units of the data set
C     STRT   - requested start date and time
C     STPP   - requested stop date and time
C     INTERP - interpolate(2) or use value(1) flag
C     C1     - real coefficient to use as fill value or interpolate to
C     ERRCOD - error code from manipulation operations
C     CRECOD - create new data set return code
C
C     + + + PARAMETERS + + +
      INTEGER     NSIZ
      PARAMETER  (NSIZ = 60000)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, I6, RETCOD, NGET, NVAL, IND, ILEN, SAVAL(1), I0,
     $            DATE(6), QFGO, DTRAN, DTOVWR, DREC, LDSN, LWDMFL
      REAL        BUFFER(NSIZ), C2(1), RDIF, RINC
C
C     + + + EXTERNALS + + +
      EXTERNAL    TIMDIF,TSBWDS,TSBTIM,WDTPUT,TSBGET,TIMBAK,ZIPR,COPYI
      EXTERNAL    WDDSCK,WDBCRL,WDBSAI,WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I6 = 6
      QFGO  = 0
      DTOVWR= 0
      ERRCOD= 0
      CRECOD= 0
      DTRAN = 0
C
C     find out how many values to fill
      CALL TIMDIF (STRT,STPP,TUNIT,TSTEP,NVAL)
C
      IF (INTERP.EQ.1) THEN
C       want to fill in data using value given
        CALL ZIPR (NVAL,C1,BUFFER)
      ELSE
C       want to fill in data by interpolation,
C       need to get previous value
        CALL TSBWDS (WDMSFL,DSN)
        CALL TSBTIM (TUNIT,TSTEP,DTRAN,QFGO)
        CALL COPYI (I6,STRT,DATE)
        CALL TIMBAK (TUNIT,
     M               DATE)
        NGET = 1
        CALL TSBGET (DATE,NGET,
     O               C2,RETCOD)
        IF (RETCOD.NE.0) THEN
C         problem, cant get a previous value
          ERRCOD = 1
        ELSE
C         now calculate interpolated values
          RDIF = C1 - C2(1)
          RINC = RDIF / NVAL
          DO 10 I = 1,NVAL
C           fill buffer with interpolated values
            BUFFER(I) = C2(1) + (I*RINC)
 10       CONTINUE
        END IF
      END IF
C
C      CALL WID2UD (I0,DSN,
C    O             LWDMFL,LDSN)
      LWDMFL = WDMSFL
      LDSN   = DSN
C     check if dsn exists
      CALL WDDSCK (LWDMFL,LDSN,
     O             DREC,RETCOD)
      IF (RETCOD .NE. 0) THEN
C       data set does not exist
        IF (INTERP.EQ.2) THEN
C         data set must exist in this case
          ERRCOD = 3
        ELSE
C         creat data set with attributes
          I = 1
          CALL WDBCRL (LWDMFL,LDSN,I,RETCOD)
          IF (RETCOD .EQ. 0) THEN
C           label copied, add time units
            IND= 17
            ILEN= 1
            SAVAL(1)= TUNIT
            CALL WDBSAI (WDMSFL,DSN,MESSFL,IND,ILEN,SAVAL,
     O                   RETCOD)
            IF (RETCOD .EQ. 0) THEN
C             time units added, add time step
              IND= 33
              SAVAL(1)= TSTEP
              CALL WDBSAI (WDMSFL,DSN,MESSFL,IND,ILEN,SAVAL,
     O                     RETCOD)
              IF (RETCOD .EQ. 0) THEN
C               time step added, add base year
                IND= 27
                SAVAL(1)= STRT(1)
                CALL WDBSAI (WDMSFL,DSN,MESSFL,IND,ILEN,SAVAL,
     O                       RETCOD)
                IF (RETCOD .EQ. 0) THEN
C                 base year added, add tsform
                  IND= 84
                  SAVAL(1)= 1
                  CALL WDBSAI (WDMSFL,DSN,MESSFL,IND,ILEN,SAVAL,
     O                         RETCOD)
                END IF
              END IF
            END IF
          END IF
          IF (RETCOD .EQ. 0) THEN
C           label copied and updated successfully
            CRECOD = 1
          ELSE
C           some problem in label copy and update
            CRECOD = 2
          END IF
        END IF
      END IF
C
      IF (ERRCOD.EQ.0) THEN
C       put buffer to file
        CALL WDTPUT (WDMSFL,DSN,TSTEP,STRT,NVAL,DTOVWR,
     #               QFGO,TUNIT,BUFFER,RETCOD)
        IF (RETCOD .NE. 0) THEN
C         error in put operation
          ERRCOD = 2
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CKBDRY
     I                   ( DATE, TSSTEP, TCODE,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     This routine is used to check that a date and time step will
C     correctly cross time boundries.  Second time steps must cross
C     an even hour time boundry.  Minute, hour, and day times steps
C     must cross a day boundry at midnight.  Month time steps must
C     cross at midnight between the last day of a month and the first
C     day of a month.  Month and year time steps must cross at
C     midnight between the last day of a year and the first day
C     of a year.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(6), TSSTEP, TCODE, RETC
C
C     + + + ARUGMENT DEFINITIONS + + +
C     DATE   - date string (yr,mo,dy,hr,mn,sc)
C     TSSTEP - time step in TCODE units
C     TCODE  - time units code
C              1 - second      4 - day
C              2 - minute      5 - month
C              3 - hour        6 - year
C     RETC   - return code
C              0 - will cross boundry correctly
C              1 - time step will not cross boundry
C              2 - date and time step will not cross boundry
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TS, TC, ITMP, TSB(6)
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   DAYMON
C
C     + + + DATA INITIALIZATIONS + + +
C                     sc    mn  hr dy  mo yr
      DATA   TSB  / 3600, 1440, 24, 1, 12, 1 /
C
C     + + + END SPECIFICATIONS + + +
C
      TS = TSSTEP
      TC = TCODE
      IF ((TC .EQ. 2  .AND.  TS .EQ. 1440)  .OR.
     #    (TC .EQ. 3  .AND.  TS .EQ. 24)  ) THEN
C       1 day time step
        TS = 1
        TC = 4
      END IF
C
      IF (MOD(TSB(TC),TS) .EQ. 0) THEN
C       time step should cross valid boundry, check with date
        RETC = 0
        GO TO ( 10, 20, 30, 40, 50, 60 ), TC
C               sc  mm  hr  dy  mo  yr
 10       CONTINUE
C           seconds time step, through hour
            ITMP = DATE(6) + DATE(5) * 60
            IF (MOD(ITMP,TS) .NE. 0) RETC = 2
            GO TO 90
 20       CONTINUE
C           minute time step, through day
            ITMP = DATE(5) + DATE(4) * 60
            IF (MOD(ITMP,TS) .NE. 0  .OR.  DATE(6) .NE. 0) RETC = 2
            GO TO 90
 30       CONTINUE
C           hour time step, through day
            ITMP = DATE(4)
            IF (DATE(5) .NE. 0  .OR.  DATE(6) .NE. 0  .OR.
     #          MOD(ITMP,TS) .NE. 0) RETC = 2
            GO TO 90
 40       CONTINUE
C           day time step, through day boundry
            IF (DATE(6) .NE. 0  .OR.  DATE(5) .NE. 0  .OR.
     #         (DATE(4) .NE. 0  .AND.  DATE(4) .NE. 24) ) RETC = 2
            GO TO 90
 50       CONTINUE
C           month time step, through month
            IF (DATE(6) .NE. 0  .OR.  DATE(5) .NE. 0) THEN
              RETC = 2
            ELSE IF (DATE(4) .EQ. 24) THEN
              IF (DAYMON(DATE(1),DATE(2)) .NE. DATE(3)) RETC = 2
            ELSE IF (DATE(4) .EQ. 0  .AND.  DATE(3) .NE. 1) THEN
              RETC = 2
            END IF
            GO TO 90
 60       CONTINUE
C           year time step, through year
            IF (DATE(6) .NE. 0  .OR.  DATE(5) .NE. 0) THEN
              RETC = 2
            ELSE IF (DATE(4) .EQ. 24) THEN
              IF (DATE(3) .NE. 31  .AND.  DATE(2) .NE. 12) RETC = 2
            ELSE IF (DATE(4) .EQ. 0) THEN
              IF (DATE(3) .NE. 1  .AND.  DATE(2) .NE. 1) RETC = 2
            END IF
 90     CONTINUE
      ELSE
C       time step will not cross valid boundry
        RETC = 1
      END IF
C
      RETURN
      END
