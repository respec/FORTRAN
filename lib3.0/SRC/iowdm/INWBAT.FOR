C
C
C
      SUBROUTINE   INWIOX
     I                    ( INTFLG )
C
C     + + + PURPOSE + + +
C     Specifies interaction for wdm input.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INTFLG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INTFLG - Interactive (1) or non-interactive (0)
C
C     + + + INCLUDES + + +
      INCLUDE 'ciowdm.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     set interactive flag in common
      INTFG = INTFLG
C
      RETURN
      END
C
C
C
      SUBROUTINE   INSVTS
     I                   ( WDMFL, DSN,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Saves the time-series array VAL to the WDM file.
C     Error messages are written to Fortran unit 99.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL, DSN, RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the wdm file
C     DSN    - Data-set number for data
C     RETC   - Return code
C              0 - successful
C
C     + + + INCLUDES + + +
      INCLUDE 'ciowdm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDTPUT, WDTPTQ
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X, 'Warning:  problem writing to wdm file    (insvts)',
     $      /,1X, '            dsn =', I5,
     $      /,1X, '          start =', I5, 5I3,
     $      /,1X, '           nval =', I5,
     $      /,1X, '           retc =', I5 )
C
C     + + + END SPECIFICATIONS + + +
C
C     write data to wdm file
      IF (QFLAG .GE. 0) THEN
C       all data has same quality code
        CALL WDTPUT ( WDMFL, DSN, TSSTEP, DATES(1,1), IPT,
     I                DTOVWR, QFLAG, TUNITS, VAL,
     O                RETC )
      ELSE
C       quality code may vary
        CALL WDTPTQ ( WDMFL, DSN, TSSTEP, DATES(1,1), IPT,
     I                DTOVWR, VALQ, TUNITS, VAL,
     O                RETC )
      END IF
      IF (RETC .NE. 0) THEN
C       error adding data to wdm file
        WRITE (99,2000) DSN, (DATES(I,1),I=1,6), IPT, RETC
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INFREE
     I                    ( WDMFL, DSTYPE, DSN, DSNINC,
     O                      DSNF, RETC )
C
C     + + + PURPOSE + + +
C     Identify an unused data set in a wdm file.  Will start checking
C     at the data set number supplied in DSN.  The value of DSN will
C     be incremented by 1 untill an unused data set is found.  If no
C     free data set is found between DSN and 32000, the first free
C     data set between 1 and DSN will be returned.  If no free data
C     set can be found (data sets 1-3200 are already used!), a value
C     of zero will be returned for DSN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL, DSTYPE, DSN, DSNINC, DSNF, RETC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the wdm data file
C     DSTYPE - data set type
C              1 - time series      6 - rastor
C              2 - table            7 - space-time
C              3 - schematic        8 - attribute
C              4 - project          9 - message
C              5 - vector
C     DSN    - initial data set to try
C     DSNINC - increment to use for successive data set to try
C     DSNF   - free data set found, a value of zero indicates
C              no free data set was found
C     RETC   - return code
C              0 - found a free data set
C              -1 -unable to find a free data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LOOP, MORE, RET, GPFLG, LREC, GRCNT, INC
C
C     + + + LOCAL DEFINITIONS + + +
C     LOOP   - infinite loop flag
C              0 - initial dsn supplied is between 1 and 32000, and
C                  routine is checking between dsn and 32000 for a
C                  free dsn
C              1 - either user supplied an invalid initial dsn or
C                  no free dsn was found between dsn and 32000, so
C                  program is now checking between 1 and dsn
C              2 - all data sets between 1 and 32000 have been checked,
C                  and no free dsn was found.
C     MORE   - flag to continue looking for free data set
C              0 - stop looking
C              1 - continue looking
C     GPFLG  - read(1)/write(2) flag
C     LREC   - record number of first record in data set (contains label)
C     GRCNT  - number of groups in data set
C     RET    - return code from call to wdscha
C                0 - data set exists and is correct DSTYPE
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYPE
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDSCHA
C
C     + + + END SPECIFICATIONS + + +
C
      LOOP = 0
      MORE = 1
      GPFLG = 2
C
C     start with supplied dsn and increment
      DSNF = DSN
      INC = DSNINC
      IF (INC .LT. 1  .OR.  INC .GT. 3000) THEN
C       outside valid range, use 1
        INC = 1
      END IF
      IF (DSNF .LE. 0  .OR.  DSNF .GT. 32000) THEN
C       outside valid range, start at 1
        DSNF = 1
        LOOP = 1
      END IF
 10   CONTINUE
C       check to see if the data set exists
        CALL WDSCHA ( WDMFL, DSNF, DSTYPE, GPFLG,
     O                LREC, GRCNT, RET )
        IF (RET .EQ. -81) THEN
C         data set does not exist, done
          MORE = 0
          RETC = 0
        ELSE IF (RET .EQ. 0  .OR.  RET .EQ. -85) THEN
C         data set exists or data set is read only
          DSNF = DSNF + INC
        ELSE IF (RET .EQ. -84) THEN
C         data set is out of valid range, start at 1
          DSNF = 1
          LOOP = LOOP + 1
          IF (LOOP .GT. 1) THEN
C           infinity check, have been past dsn 1 before, give it up
            MORE = 0
            RETC = -1
            DSNF = 0
            INC = 1
          END IF
        ELSE
C         unknown error, try next data set
          DSNF = DSNF + INC
        END IF
      IF (MORE .EQ. 1) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTPTQ
     I                   ( WDMFL, DSN, DELT, DATE, NVAL, DTOVWR,
     I                     QUAL, TUNITS, VALU,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     This routine writes a timeseries and its quality flags to a
C     WDM file.  Error messages are written to Fortran unit 99.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL, DSN, DELT, DATE(6), NVAL, DTOVWR, TUNITS, RETC
      INTEGER   QUAL(NVAL)
      REAL      VALU(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of WDM file
C     DSN    - WDM dataset number
C     DELT   - timestep in TUNITS units
C     DATE   - start date/time of array VALU
C     NVAL   - number of values to be written to WDM file
C     DTOVWR - data overwrite flag
C     QUAL   - array containing quality flags
C     TUNITS - time units code
C              1 - seconds       5 - months
C              2 - minutes       6 - years
C              3 - hours         7 - centuries
C              4 - days
C     VALU   - timeseries array to be written to WDM file
C     RETC   - return code from WDTPUT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LEN6, DATEW(6), DATEN(6), KNT, QFLAG, IPT, KPT, CONT
C
C     + + + EXTERNALS + + +
      EXTERNAL  COPYI, WDTPUT, TIMADD
C
C     + + + DATA INITIALIZATION + + +
      DATA  LEN6 / 6 /
C
C     + + + OUTPUT FORMATS + + +
 2001 FORMAT (1X,'in wdtptq, after call to wdtput',
     $      /,1X,'         ', I5, ' = wdmfl',
     $      /,1X,'         ', I5, ' = dsn',
     $      /,1X,'         ', I5,I2, ' = time step',
     $      /,1X,'         ', 2I4, ' = dtovwr, qflag' )
 2002 FORMAT (1X,'         ', 3(I5,5I3), ' = dates',
     $      /,1X,'         ', 2I5, ' = ipt, ret' )
C
C     + + + END SPECIFICATIONS + + +
C
      RETC = 0
C     initialize date, flags, counters, and pointers
      CALL COPYI ( LEN6, DATE, DATEW )
      QFLAG = QUAL(1)
      KNT = 1
      KPT = 1
      IPT = 1
      CONT = 1
 100  CONTINUE
C       does time series need to be saved
        IF (IPT .EQ. NVAL) THEN
C         last time step, set write flag
          CONT = 3
        ELSE IF (QUAL(IPT+1) .NE. QFLAG) THEN
C         quality is changing set flag to save data with current flag
          CONT = 2
        END IF
        IF (CONT .GT. 1) THEN
C         save data for current quality flag
          CALL WDTPUT ( WDMFL, DSN, DELT, DATEW, KNT, DTOVWR,
     I                  QFLAG, TUNITS, VALU(KPT),
     O                  RETC )
          IF (RETC .NE. 0) THEN
C           error adding data to wdm file
            WRITE (99,2001) WDMFL, DSN, DELT, TUNITS, DTOVWR, QFLAG
            WRITE (99,2002) DATEW, KNT, RETC
          END IF
          IF (CONT .EQ. 2) THEN
C           there is still more data, update date, flags, and pointers
            CALL TIMADD ( DATEW, TUNITS, DELT, KNT, DATEN )
            CALL COPYI ( LEN6, DATEN, DATEW )
            KPT = IPT + 1
            QFLAG = QUAL(KPT)
            KNT = 0
            CONT = 1
          END IF
        END IF
C       update counter and array pointer
        KNT = KNT + 1
        IPT = IPT + 1
C       next time step if more data
      IF (CONT .EQ. 1) GO TO 100
C
      RETURN
      END
