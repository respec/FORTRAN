C
C
C
      SUBROUTINE   INWDVA
     I                   ( FE, INFL, INITFG)
C
C     + + + PURPOSE + + +
C     Advance through standard WATSTORE card image daily values
C     cards from one data set to the next.
C
C     + + + KEYWORDS + + +
C     WDM file, message file, interactive
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FE, INFL, INITFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FE     - Fortran unit number for debug output (0 if none)
C     INFL   - Fortran unit number of daily values file
C     INITFG - initialize for new watstore file flag
C
C     + + + COMMONS + + +
      INCLUDE 'ciowdm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       NEW
C     integer       i
      CHARACTER*1   ID
      CHARACTER*15  STA
      CHARACTER*80  RECORD
C
C     + + + LOCAL DEFINITIONS + + +
C     NEW    - flag indicating record processing status
C              0 - continuing processing the same time series
C              1 - starting to process a new time series
C              2 - processing last input record
C              3 - read error
C     ID     - identifier for type of input record
C              Z - station header, agency record
C              H - station header, location record
C              N - station header, station name record
C              Y - station header, base discharge record
C              2 - daily values parameter and statistics record
C              3 - daily values data record
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWREC, INITAX
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize counters, pointers, and dates
      KTERR = 0
      IF (INITFG.EQ.1) THEN
C       initialize starting record
        KTREC = 0
      END IF
      NEW = 0
C
C     initialize identifiers
      STA    = '               '
      ID     = ' '
C
 100  CONTINUE
C       process next record
        CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
        IF (STA .NE. RECORD(2:16)) THEN
C         new station
          IF (STA.EQ.'               ') THEN
C           this is the first time through
            CALL INITAX
            NEW = 1
            STA = RECORD(2:16)
          ELSE
C           need to get out of this routine
            NEW = 2
            KTREC = KTREC - 1
            BACKSPACE (INFL)
          END IF
        END IF
      IF (NEW .NE. 2) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWDVB
     I                   ( MESSFL, WDMFL, FE, INFL,
     I                     DSNINC, INITFG,
     M                     DSN )
C
C     + + + PURPOSE + + +
C     This routine reads standard WATSTORE card image daily values
C     cards that include the station header information and outputs
C     the information to a new data set in a WDM file.
C     Only one data set is processed at a time.
C
C     + + + KEYWORDS + + +
C     WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, FE, INFL,
     $          DSNINC, DSN, INITFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of WDM file
C     FE     - Fortran unit number for debug output (0 if none)
C     INFL   - Fortran unit number of daily values file
C     DSNINC - increment for new data sets
C     DSN    - data-set number
C     INITFG - initialize for new watstore file flag
C
C     + + + COMMONS + + +
      INCLUDE 'ciowdm.inc'
      INCLUDE 'catrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       NEW, LEN, RETC, DSTYPE, IZIP
      INTEGER       RET, DSNF, PSA
      CHARACTER*1   ID
      CHARACTER*10  PARMST
      CHARACTER*15  STA
      CHARACTER*80  RECORD
C
C     + + + LOCAL DEFINITIONS + + +
C     NEW    - flag indicating record processing status
C              0 - continuing processing the same time series
C              1 - starting to process a new time series
C              2 - processing last input record
C              3 - read error
C     ID     - identifier for type of input record
C              Z - station header, agency record
C              H - station header, location record
C              N - station header, station name record
C              Y - station header, base discharge record
C              2 - daily values parameter and statistics record
C              3 - daily values data record
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWHDR, INWAT2, INWAT3, INSVTS, CVWTAT
      EXTERNAL   INWSTA, INWREC, INITAX, INWATZ
      EXTERNAL   INFREE, WDLBAX, INATRB
      EXTERNAL   ZIPI, ZIPR
C
C     + + + DATA INITIALIZATION + + +
      DATA  DSTYPE / 1 /
C
C     + + + INPUT FORMATS + + +
 1002 FORMAT ( I4 )
C
C     + + + OUTPUT FORMATS + + +
 2001 FORMAT (/,' Warning:  unexpected or invalid record at', I6,
     $                      ', kount is', I6, ' (inwdvx)',
     $        /,'          ', A40, /,'          ', A40 )
C
C     + + + END SPECIFICATIONS + + +
C
C     set wdput arguments
      TSSTEP = 1
      TUNITS = 4
      DTOVWR = 0
      QFLAG  = 0
C
C     initialize counters, pointers, and dates
      KTERR = 0
      IF (INITFG.EQ.1) THEN
C       initialize starting record
        KTREC = 0
      END IF
      IPT = 0
      NEW = 0
      DSN = DSN - DSNINC
      IZIP = 0
      LEN = 18
      CALL ZIPI ( LEN, IZIP, DATES )
C
C     initialize identifiers
      PARMST = '          '
      STA    = '               '
      ID     = ' '
C
 100  CONTINUE
C       process next record
        IF (ID .EQ. ' ') THEN
C         record is empty or invalid, get a new record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
        END IF
        IF (ID .EQ. 'Z') THEN
C         agency record
          CALL INWATZ ( INFL, FE,
     M                  KTREC, ID, RECORD, ACVAL(85), NEW )
        END IF
        IF (STA .NE. RECORD(2:16)) THEN
C         new station
          IF (STA.EQ.'               ') THEN
C           either this is the first time through or we are
C           processing multiple data sets
            CALL INITAX
            PARMST = '          '
            NEW = 1
            STA = RECORD(2:16)
            CALL INWSTA ( STA, ACVAL(1), AIVAL(13) )
          ELSE
C           need to get out of this routine
            NEW = 2
            KTREC = KTREC - 1
            BACKSPACE (INFL)
          END IF
        END IF
C       process record according to type
        IF (ID .EQ. 'Z'  .OR.  (STA .EQ. RECORD(2:16)  .AND.
     $     (ID .EQ. 'H' .OR. ID .EQ. 'Y' .OR. ID .EQ. 'N'))) THEN
C         same station record, same station
          CALL INWHDR ( INFL, FE, STA,
     M                  KTREC, ID, RECORD, NEW )
        ELSE IF (ID .EQ. '2'  .AND.  RECORD(2:16) .EQ. STA) THEN
C         parameter/statistics record, same station
          CALL INWAT2 ( INFL, FE,
     M                  KTREC, ID, RECORD, STA, PARMST, NEW,
     O                  AIVAL(14) )
          CALL CVWTAT ( MESSFL, AIVAL(14), ACVAL(17) )
        ELSE IF (ID .EQ. '3'  .AND.  RECORD(2:16) .EQ. STA) THEN
C         daily values record, same station
          IF (NEW .EQ. 1) THEN
C           this is a new time series, get base year from record
            READ (RECORD(17:20),1002) AIVAL(6)
C           get the data set number
            DSN  = DSN + DSNINC
            RETC = 0
C           get a free dsn
            CALL INFREE ( WDMFL, DSTYPE, DSN, DSNINC,
     O                    DSNF, RET )
            IF (RET .EQ. 0) THEN
C             add data set
              DSN = DSNF
              CALL WDLBAX ( WDMFL, DSN, DSTYPE, AIVAL(11), AIVAL(12),
     I                      AIVAL(8), AIVAL(9), AIVAL(10),
     O                      PSA )
              RETC = 1
C             data set found, copied, or created, add/update attributes
              CALL INATRB ( MESSFL, FE, WDMFL, DSN, RET )
              IF (RET .NE. 0) THEN
C               problem, set return code
                RETC = -1
              END IF
            ELSE
C             couldn't find a free dsn
              RETC = -1
            END IF
C           fill time-series array with missing value
            IPT = MXVAL
            CALL ZIPR ( IPT, ARVAL(1), VAL )
            IPT = 0
            NEW = 0
          END IF
 300      CONTINUE
C           process the daily values records
            CALL INWAT3 ( INFL, FE,
     M                    ID, RECORD, STA, NEW )
            IF (IPT .GT. 0) THEN
C             save the daily values to the wdm
              CALL INSVTS ( WDMFL, DSN, RETC )
              CALL ZIPR ( IPT, ARVAL(1), VAL )
              IPT = 0
              DATES(1,1) = 0
            END IF
C           continue reading daily values if same station
          IF (ID .EQ. '3'  .AND.  RECORD(2:16) .EQ. STA) GO TO 300
        ELSE IF (NEW .NE. 2) THEN
C         invalid or bad record (not end of file)
          KTERR = KTERR + 1
          WRITE (FE,2001) KTREC,  KTERR, RECORD(1:40), RECORD(41:80)
          ID = ' '
        END IF
C       continue processing if end of file not reached
      IF (NEW .NE. 2) GO TO 100
C
      RETURN
      END
