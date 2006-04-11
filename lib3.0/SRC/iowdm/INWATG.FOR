C
C
C
      SUBROUTINE   INWATZ
     I                   ( INFL, FE,
     M                     KTREC, ID, RECORD, AGENCY, NEW )
C
C     + + + PURPOSE + + +
C     Process the standard Watstore card-image "Z" record for
C     agency name.
C
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      INFL, FE, KTREC, NEW
      CHARACTER*1  ID, AGENCY(8)
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input file
C     FE     - Fortran unit number of file for debug output
C     KTREC  - record count
C     ID     - record identifier for input record
C              Z - agency record
C     RECORD - record from input file
C     AGENCY - agency
C     NEW    - record processing flag
C              1 - new record, new time series
C              2 - end of file
C              3 - read error
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL  INWREC
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 80A1 )
C
C     + + + END SPECIFICATIONS + + +
C
 100  CONTINUE
C       process next record
        IF (ID .EQ. 'Z') THEN
C         agency: new agency record
          READ (RECORD(33:40),1000) (AGENCY(I), I = 1, 5)
          AGENCY(6) = ' '
          AGENCY(7) = ' '
          AGENCY(8) = ' '
        END IF
C       get a new record
        CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
        IF (NEW .EQ. 0  .AND. ID .NE. 'Z') THEN
C         new record is not an agency record
          NEW = 1
        END IF
C       if valid station header record, process it
      IF (NEW .EQ. 0) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWHDR
     I                   ( INFL, FE, STA,
     M                     KTREC, ID, RECORD, NEW )
C
C     + + + PURPOSE + + +
C     Process station header records in the standard WATSTORE card-image
C     format.  Subroutine will keep reading input file until a record
C     that is not a station header record is encountered, or until a
C     new station is encountered.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, KTREC, NEW
      CHARACTER*1   ID
      CHARACTER*15 STA
      CHARACTER*80  RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input file
C     FE     - Fortran unit number of file for debug output
C     STA    - station identifier
C     KTREC  - record count
C     ID     - record identifier for input record
C              Z - agency record
C              H - location record
C              N - station name record
C              Y - base discharge record
C     RECORD - record from input file
C     NEW    - processing status indicator
C              0 - new record being returned
C              1 - new station
C              2 - end of file
C              3 - read error
C
C     + + + INCLUDES + + +
      INCLUDE 'catrib.inc'
C
C     + + + SAVES + + +
      CHARACTER*80 HEADER(4)
      SAVE         HEADER
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, CONT, NEWR
      REAL      HOLDR
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWATH, INWREC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA HEADER / '   ', '    ', '    ',  '    ' /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 80A1 )
 1008 FORMAT ( F7.0 )
C
C     + + + END SPECIFICATSIONS + + +
C
      CONT = 1
 100  CONTINUE
C       process next record
        IF (RECORD(2:16) .NE. STA  .AND.  ID .NE. 'Z') THEN
C         new station (no sta on agency record)
          CONT = 2
          NEW = 1
        ELSE
C         expected station, get attribute information from header
          IF (ID .EQ. 'Z'  .AND.  HEADER(1) .NE. RECORD) THEN
C           agency: new agency record
            HEADER(1) = RECORD
            NEW = 1
            READ (RECORD(33:37),1000) (ACVAL(I), I = 85, 89)
            ACVAL(90) = ' '
            ACVAL(91) = ' '
            ACVAL(92) = ' '
          ELSE IF (ID .EQ. 'H'  .AND.  HEADER(2) .NE. RECORD) THEN
C           new location record, initialize attributes
            HEADER(2) = RECORD
            NEW = 1
            CALL INWATH ( RECORD )
          ELSE IF (ID .EQ. 'N'  .AND.  HEADER(3) .NE. RECORD) THEN
C           stanam: new station name
            HEADER(3) = RECORD
            NEW = 1
            READ (RECORD(17:64),1000) (ACVAL(I), I = 21, 68)
            IF (RECORD(65:72) .NE. '        ') THEN
C             gucode: geologic unit code
              READ (RECORD(65:72),1000) (ACVAL(I), I = 1, 77, 84)
            END IF
            IF (RECORD(73:73) .NE. ' ') THEN
C             aqtype: aquifer type
              ACVAL(73) = RECORD(73:73)
              ACVAL(74) = ' '
              ACVAL(75) = ' '
              ACVAL(76) = ' '
            END IF
          ELSE IF (ID .EQ. 'Y'  .AND.  HEADER(4) .NE. RECORD) THEN
C           baseq: base discharge
            HEADER(4) = RECORD
            NEW = 1
            READ (RECORD(17:23),1008,ERR=310) HOLDR
              ARVAL(8) = HOLDR
 310        CONTINUE
          END IF
C
C         get a new record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEWR )
          IF (NEWR .EQ. 0  .AND.
     $       (ID .NE. 'Z'  .AND.  ID .NE. 'H'  .AND.
     $        ID .NE. 'N'  .AND.  ID .NE. 'Y')) THEN
C           new record not station header, assume same station
            CONT = 2
          ELSE IF (NEWR .NE. 0) THEN
            NEW = NEWR
            CONT = 2
          END IF
        END IF
C       if valid station header record, process it
      IF (CONT .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWATH
     I                   ( RECORD )
C
C     + + + PURPOSE + + +
C     Reads information from a Station header location record.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*80 RECORD
C
C     + + + INCLUDES + + +
      INCLUDE 'catrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, HOLDI
      REAL      HOLDR, LTLG(3)
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 80A1 )
 1002 FORMAT ( 3F2.0 )
 1003 FORMAT ( F3.0,2F2.0 )
 1004 FORMAT ( I2 )
 1006 FORMAT ( I3 )
 1007 FORMAT ( I8 )
 1008 FORMAT ( F7.0 )
 1010 FORMAT ( F8.0 )
 1011 FORMAT ( F9.0 )
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RECORD(17:22) .NE. '      ') THEN
C       latdeg: latitude
        READ (RECORD(17:22),1002,ERR=201) LTLG
          ARVAL(2) = LTLG(1) + LTLG(2)/60. + LTLG(3)/3600.
 201    CONTINUE
      END IF
      IF (RECORD(23:29) .NE. '       ') THEN
C       lngdeg: longitude
        READ (RECORD(23:29),1003,ERR=202) LTLG
          ARVAL(3) = LTLG(1) + LTLG(2)/60. + LTLG(3)/3600.
 202    CONTINUE
      END IF
      IF (RECORD(32:33) .NE. '  ') THEN
C       stfips: state code
        READ (RECORD(32:33),1004,ERR=203) HOLDI
          AIVAL(16) = HOLDI
 203    CONTINUE
      END IF
      IF (RECORD(34:35) .NE. '  ') THEN
C       dscode: district code
        READ (RECORD(34:35),1004,ERR=204) HOLDI
          AIVAL(17) = HOLDI
 204    CONTINUE
      END IF
      IF (RECORD(36:38) .NE. '   ') THEN
C       cocode: county code
        READ (RECORD(36:38),1006,ERR=205) HOLDI
          AIVAL(18) = HOLDI
 205    CONTINUE
      END IF
      IF (RECORD(39:40) .NE. '  ') THEN
C       siteco: site code
        READ (RECORD(39:40),1000) (ACVAL(I), I = 69, 70)
        ACVAL(71) = ' '
        ACVAL(72) = ' '
      END IF
      IF (RECORD(41:48) .NE. '        ') THEN
C       hucode: hydrologic unit code
        READ (RECORD(41:48),1007,ERR=206) HOLDI
          AIVAL(19) = HOLDI
 206    CONTINUE
      END IF
      IF (RECORD(49:55) .NE. '       ') THEN
C       darea: total drainage area
        READ (RECORD(49:55),1008,ERR=207) HOLDR
          ARVAL(4) = HOLDR
 207    CONTINUE
      END IF
      IF (RECORD(56:62) .NE. '       ') THEN
C       contda: contributing drainage area
        READ (RECORD(56:62),1008,ERR=208) HOLDR
          ARVAL(5) = HOLDR
 208    CONTINUE
      END IF
      IF (RECORD(63:70) .NE. '        ') THEN
C       datum: datum elevation
        READ (RECORD(63:70),1010,ERR=209) HOLDR
          ARVAL(6) = HOLDR
 209    CONTINUE
      END IF
      IF (RECORD(71:79) .NE. '         ') THEN
C       welldp: total well depth
        READ (RECORD(71:79),1011,ERR=210) HOLDR
          ARVAL(7) = HOLDR
 210    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWAT2
     I                   ( INFL, FE,
     M                     KTREC, ID, RECORD, STA, PARMST, NEW,
     O                     AIVAL )
C
C     + + + PURPOSE + + +
C     Process the parameter and statistics record.  Subroutine will
C     keep reading input file until a record that is not a "2" card
C     is found.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, KTREC, NEW, AIVAL(2)
      CHARACTER*1  ID
      CHARACTER*10 PARMST
      CHARACTER*15 STA
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of input file
C     FE     - Fortran unit number for debug output file
C     KTREC  - records processed
C     ID     - type identifier for record
C              2 - parameter and statistics record
C     RECORD - record from input file
C     STA    - station identifier
C              enters as last station processed
C              leaves as current station being processed
C     PARMST - parameter and statistics identifier
C              enters and parm and stat for last station processed
C              leaves as parm and stat for current station being processed
C     NEW    - indicator for processing status
C              0 - new record, same time series
C              1 - new record,  new time series
C              2 - reached end of input file
C              3 - read error
C     AIVAL  - parameter and statistics attributes
C              (1) - PARMCD, parameter code
C              (2) - STATCD, statistics code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   PARM, STAT, CONT, NEWT
C
C     + + + EXTERNALS + + +
      EXTERNAL INWREC
C
C     + + + INPUT FORMATS + + +
 1001 FORMAT ( 2I5 )
C
C     + + + END SPECIFICATIONS + + +
C
C     look at the record
      CONT = 1
 10   CONTINUE
C       check for new time series
        IF (RECORD(2:16) .NE. STA) THEN
C         station has changed
          NEW = 1
          CONT = 2
        ELSE IF (RECORD(29:38) .NE. PARMST) THEN
C         parmcd,statcd: parameter or statistics code has changed
          NEW = 1
          PARMST = RECORD(29:38)
          READ (PARMST,1001,ERR=108) PARM, STAT
C           successful read
            AIVAL(1) = PARM
            AIVAL(2) = STAT
            GO TO 110
 108      CONTINUE
C           read error
            AIVAL(1) = -999
            AIVAL(2) = -999
 110      CONTINUE
C         get a new record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEWT )
          IF (NEWT .EQ. 0  .AND.  ID .NE. '2') THEN
C           new record is not parameter/statistics record
            CONT = 2
          ELSE IF (NEWT .NE. 0) THEN
            NEW  = NEWT
            CONT = 2
          END IF
        ELSE
C         no change in station, parameter or statistics, get record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEWT )
          IF (NEWT .EQ. 0  .AND.  ID .NE. '2') THEN
C           new record is not parameter/statistics record
            CONT = 2
          ELSE IF (NEWT .NE. 0) THEN
            NEW  = NEWT
            CONT = 2
          END IF
        END IF
C       if 2 record, process it
      IF (CONT .EQ. 1) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWAT3
     I                   ( INFL, FE,
     M                     ID, RECORD, STA, NEW )
C
C     + + + PURPOSE + + +
C     reads and process watstore daily values "3" records
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, NEW
      CHARACTER*1 ID
      CHARACTER*15 STA
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input file
C     FE     - Fortran unit number for debug output
C     ID     - record type identifier
C              3 - valid watstore daily values record to be processed
C              /= 3 - not a daily values record, return to calling program
C     RECORD - input record being processed
C     STA    - station number being processed
C     NEW    - indicator flag for status for time series being processed
C
C     + + + COMMONS + + +
      INCLUDE 'ciowdm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CD, J, JN, JPT, KOUNT, CONT, LEN7, RETC
C
C     + + + LOCAL DEFINITIONS + + +
C     CONT   - flag to continue reading input file
C              1 - continue reading
C              2 - stop reading, time-series has changed or buffer
C                  is full, Return to calling program to empty it.
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMON, TIMDIF, INWREC , RDVALU
C
C     + + + INPUT FORMATS + + +
 1014 FORMAT ( 16X, I4, I2, I2 )
C
C     + + + END SPECIFICATIONS + + +
C
      LEN7 = 7
      CONT = 1
 100  CONTINUE
C       check for new time series
        IF (RECORD(2:16) .NE. STA) THEN
C         station has changed
          STA = RECORD(2:16)
          NEW = 1
          CONT = 2
        ELSE
C         same station, get year, month, and card number, calculate day
          READ (RECORD,1014) DATES(1,2), DATES(2,2), CD
          DATES(3,2) = ( CD - 1 ) * 8  +  1
          IF (CD .LT. 4) THEN
C           first three records contain 8 values
            JN = 8
          ELSE
C           last record for the month, number depends on the month
            JN = DAYMON( DATES(1,2), DATES(2,2) ) - 24
          END IF
          IF (IPT .EQ. 0) THEN
C           start new array of data
            DATES(1,1) = DATES(1,2)
            DATES(2,1) = DATES(2,2)
            DATES(3,1) = DATES(3,2)
            IPTN = 1
          ELSE
C           add to existing array of data, determine position
            CALL TIMDIF ( DATES(1,1), DATES(1,2), TUNITS, TSSTEP, IPTN )
            IPTN = IPTN + 1
          END IF
          IF (IPTN+JN .GT. MXVAL) THEN
C           new data will not fit, empty array
            CONT = 2
          ELSE
C           add data to array
            JPT = 25
            KOUNT = 0
            DO 320 J = 1, JN
C             read data from card
              IF (RECORD(JPT:JPT+6) .NE. '       ') THEN
C               found data in field
                IPT = IPTN
                CALL RDVALU ( LEN7, RECORD(JPT:JPT+6),
     M                        VAL(IPT),
     O                        RETC )
                IF (RETC .NE. 0) THEN
C                 error reading card
                  KOUNT = KOUNT + 1
                END IF
              ELSE
C               blank field, use missing value filler
                IPT = IPTN
              END IF
              JPT = JPT + 7
              IPTN = IPTN + 1
 320        CONTINUE
            IF (KOUNT .NE. 0) THEN
C             error on input record
              KTERR = KTERR + 1
            END IF
C
C           get next record
            CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
            IF (NEW .EQ. 0  .AND.  ID .NE. '3') THEN
C             not a unit values record
              CONT = 2
            ELSE IF (NEW .NE. 0) THEN
              CONT = 2
            END IF
          END IF
        END IF
C       if still daily value record & same station, continue processing
      IF (CONT .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWATB
     I                   ( INFL, FE,
     M                     ID, RECORD, STA, NEW )
C
C     + + + PURPOSE + + +
C     reads and process watstore unit values "B" records
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, NEW
      CHARACTER*1 ID
      CHARACTER*15 STA
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input file
C     FE     - Fortran unit number for debug output
C     ID     - record type identifier
C              B - valid watstore unit values record to be processed
C              /= B - not a unit values record, return to calling program
C     RECORD - input record being processed
C     STA    - station number being processed
C     NEW    - indicator flag for status for time series being processed
C
C     + + + COMMONS + + +
      INCLUDE 'ciowdm.inc'
C
C     + + + SAVES + + +
      INTEGER   ORPD
      SAVE      ORPD
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, JN, JPT, KOUNT, CONT, RPD, DELT, I, LEN7, RETC
C
C     + + + LOCAL DEFINITIONS + + +
C     CONT   - flag to continue reading input file
C              1 - continue reading
C              2 - stop reading, time-series has changed or buffer
C                  is full, Return to calling program to empty it.
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIMDIF, INHRMN, INWREC, RDVALU
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   ORPD
     :     /    0 /
C
C     + + + INPUT FORMATS + + +
 1016 FORMAT ( 16X, I4, 4I2, 2X, I5 )
C
C     + + + END SPECIFICATIONS + + +
C
      LEN7 = 7
      CONT = 1
 100  CONTINUE
C       check for new time series
        IF (RECORD(2:16) .NE. STA) THEN
C         station has changed
          STA = RECORD(2:16)
          NEW = 1
          CONT = 2
        ELSE
C         same station, get date/time and readings per day
          READ (RECORD,1016) (DATES(I,2), I = 1, 5), RPD
          IF (IPT .EQ. 0) THEN
C           start new array of data
            DELT = 1440 / RPD
            TSSTEP = DELT
C           adjust time from end of time step to begining
            CALL INHRMN ( DELT, DATES(4,2), DATES(5,2) )
            DATES(1,1) = DATES(1,2)
            DATES(2,1) = DATES(2,2)
            DATES(3,1) = DATES(3,2)
            DATES(4,1) = DATES(4,2)
            DATES(5,1) = DATES(5,2)
            IPTN = 1
            ORPD = RPD
            IF (RPD .GE. 6) THEN
C             full records
              JN = 6
            ELSE
C             record will not be full
              JN = RPD
            END IF
          ELSE IF (ORPD .NE. RPD) THEN
C           same time series, but time step has changed, empty array
            CONT = 2
          ELSE
C           adjust time from end of time step to beginning
            CALL INHRMN ( DELT, DATES(4,2), DATES(5,2) )
C           add to existing array of data, determine position
            CALL TIMDIF ( DATES(1,1), DATES(1,2), TUNITS, TSSTEP, IPTN )
            IPTN = IPTN + 1
          END IF
          IF (IPTN+JN .GT. MXVAL) THEN
C           new data will not fit, empty array
            CONT = 2
          END IF
C
          IF (CONT .EQ. 1) THEN
C           add data to array
            JPT = 39
            KOUNT = 0
            DO 320 J = 1, JN
C             read data from card
              IF (RECORD(JPT:JPT+6) .NE. '       ') THEN
C               found data in field
                IPT = IPTN
                CALL RDVALU ( LEN7, RECORD(JPT:JPT+6),
     M                        VAL(IPT),
     O                        RETC )
                IF (RETC .NE. 0) THEN
C                 error reading card
                  KOUNT = KOUNT + 1
                END IF
              ELSE
C               blank field, use missing value filler
                IPT = IPTN
              END IF
              JPT = JPT + 7
              IPTN = IPTN + 1
 320        CONTINUE
            IF (KOUNT .NE. 0) THEN
C             error on input record
              KTERR = KTERR + 1
            END IF
C
C           get next record
            CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
            IF (NEW .EQ. 0  .AND.  ID .NE. 'B') THEN
C             not a unit values record
              CONT = 2
            ELSE IF (NEW .NE. 0) THEN
              CONT = 2
            END IF
          END IF
        END IF
C       if still unit value record & same station, continue processing
      IF (CONT .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWAT1
     I                   ( INFL, FE,
     M                     KTREC, ID, RECORD, STA,
     O                     NEW, AIVAL, ACVAL )
C
C     + + + PURPOSE + + +
C     Process the station record for basin characteristics.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, KTREC, NEW, AIVAL(15)
      CHARACTER*1   ID, ACVAL(76)
      CHARACTER*15  STA
      CHARACTER*80  RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input file
C     FE     - Fortran unit number for messages and warnings
C     KTREC  - records processed
C     ID     - record type identifier
C              0 or 1 - station identifier
C     RECORD - input record being processed
C     STA    - staiton number being processed
C     NEW    - indicator flag for status of time series being processed
C     AIVAL  - ????
C     ACVAL  - ????
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I, HOLDI, CONT
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWSTA, INWREC
C
C     + + + INUT FORMATS + + +
 1001 FORMAT ( 48A1 )
 1004 FORMAT ( I2 )
C
C     + + + END SPECIFICATIONS + + +
C
      CONT = 1
 100  CONTINUE
C       check for new station
        IF (RECORD(2:16) .NE. STA) THEN
C         station has changed
          CONT = 2
        ELSE
C         expected station
C         staid:  station id
          CALL INWSTA ( RECORD(2:16), ACVAL(1), AIVAL(13) )
C         stanam:  new station name
          READ (RECORD(21:68),1001) (ACVAL(I), I = 21, 68)
          IF (RECORD(76:80) .NE. '     ') THEN
C           agency:  agency
            READ (RECORD(76:80),1001) (ACVAL(I), I = 69, 73)
            ACVAL(74) = ' '
            ACVAL(75) = ' '
            ACVAL(76) = ' '
          END IF
          IF (RECORD(17:18) .NE. '  ') THEN
C           stfips:  state code
            READ (RECORD(17:18),1004,ERR=204) HOLDI
              AIVAL(14) = HOLDI
 204        CONTINUE
          END IF
          IF (RECORD(19:20) .NE. '  ') THEN
C           dscode:  district code
            READ (RECORD(19:20),1004,ERR=205) HOLDI
              AIVAL(15) = HOLDI
 205        CONTINUE
          END IF
C         get a new record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
          IF (NEW .EQ. 0  .AND.  (ID .NE. '0' .AND. ID .NE. '1')) THEN
C           new record not station identifier
            CONT = 2
          ELSE IF (NEW .NE. 0) THEN
            CONT = 2
          END IF
        END IF
C       continue processing if station identifier record
      IF (CONT .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWATC
     I                   ( INFL, FE,
     M                     KTREC, ID, RECORD,
     M                     STA, NEW, ABVAL, INDEX, KNDX )
C
C     + + + PURPOSE + + +
C     Process the basin characteristics records
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, KTREC, NEW, INDEX(200), KNDX
      REAL      ABVAL(200)
      CHARACTER*1  ID
      CHARACTER*15 STA
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CONT, IPT, IPT2, IPTN, I, NUM
      REAL      VAL
C
C     + + + EXTERNALS + + +
      EXTERNAL  INWREC
C
C     + + + INPUT FORMATS + + +
 1006 FORMAT ( I3, F7.0 )
C
C     + + + END SPECIFICATIONS + + +
C
      CONT = 1
 100  CONTINUE
C       check for new station
        IF (RECORD(2:16) .NE. STA) THEN
C         station has changed
          CONT = 2
        ELSE
C         same station, get characteristics
          IPT = 17
          IPT2 = 20
          IPTN = 26
          DO 250 I = 1, 6
C           look at each field
            IF (RECORD(IPT2:IPTN) .NE. '       '  .AND.
     $          RECORD(IPT2:IPTN) .NE. ' 999999') THEN
C             basin characteristic is not missing
              READ (RECORD(IPT:IPTN),1006,ERR=227) NUM, VAL
C               successful read, save characteristic
                IF (NUM .GT. 0  .AND.  NUM .LE. 200) THEN
C                 valid characteristic index
                  KNDX = KNDX + 1
                  ABVAL(KNDX) = VAL
                  INDEX(KNDX) = NUM
                END IF
                GO TO 229
 227          CONTINUE
C               read error
 229          CONTINUE
            END IF
            IPT = IPT + 10
            IPT2 = IPT2 + 10
            IPTN = IPTN + 10
 250      CONTINUE
C
C         get next record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
          IF (NEW .NE. 0  .AND.  ID .NE. '2') THEN
C           new record not a basin characteristics record
            CONT = 2
          ELSE IF (NEW .NE. 0) THEN
            CONT = 2
          END IF
        END IF
C       if still basin characteristics record, continue processing
      IF (CONT .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWTP1
     I                   ( INFL, FE, CALND,
     M                     ID, RECORD, STA, NEW )
C
C     + + + PURPOSE + + +
C     Process watstore card image annual peak flow records to
C     be written to a wdm time-series data set.  Will read both
C     the "3" records of annual peak flow and the "4" records for
C     partial record data.  The highest peak occuring in the year
C     will be saved to the data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, CALND(2), NEW
      CHARACTER*1  ID
      CHARACTER*15 STA
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input peak flow file
C     FE     - Fortran unit number of file for debug output
C     CALND  - calender months for year or season
C              (1) - beginning month (1-Jan,...12-Dec)
C              (2) - ending month (1-Jan,...12-Dec)
C     ID     - record identifier for input record
C     RECORD - record from input file
C     STA    - station currently being processed
C     NEW    - indicator for processing
C              1 - new station
C              2 - end of file encountered
C
C     + + + INCLUDES + + +
      INCLUDE 'ciowdm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DATEC(3), LENQ, YEAR
      REAL      PEAK
      CHARACTER*1 PEAKQ(12)
C
C     + + + FUNCTIONS + + +
      INTEGER   PKQLTY
C
C     + + + EXTERNALS + + +
      EXTERNAL  PKQLTY, IOCLND, INWREC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   LENQ
     $      /  12 /
C
C     + + + INPUT FORMATS + + +
 1002 FORMAT ( I4,I2,I2, F7.0, 12A1 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' Warning: unexpected or invalid record at', I6,
     $        /,'          ', A40, /,'          ', A40 )
 2001 FORMAT (/,' Warning: unexpected or invalid record at', I6,
     $        /, '         expected a date after', I5, ' found',
     $        /,'          ', A40, /,'          ', A40 )
 2002 FORMAT (/,' Warning:  unexpected or invalid record at', I6,
     $        /,'           record contains an invalid date',
     $        /,'          ', A40, /,'          ', A40 )
 2003 FORMAT (/,' Warning:  unexpected or invalid record at', I6,
     $        /,'           too many years after start date', I5,
     $        /,'          ', A40, /,'          ', A40 )
C
C     + + + END SPECIFICATIONS + + +
C
      NEW = 0
 100  CONTINUE
C       check for same time series
        IF (RECORD(2:16) .NE. STA) THEN
C         station has changed
          NEW = 1
        ELSE IF (RECORD(25:31) .EQ. '       ') THEN
C         peakf field is blank, skip this record
        ELSE
C         expected station - get year, peak, and qualification codes
          READ (RECORD(17:43),1002,ERR=270) DATEC, PEAK, PEAKQ
C           successful read - get year based on begin and end months
            CALL IOCLND ( CALND, DATEC(1), DATEC(2),
     O                    YEAR )
            IF (YEAR .GT. 0) THEN
C             valid date
              IF (DATES(1,1) .EQ. 0) THEN
C               first peak for this time series, save year
                DATES(1,1) = YEAR
              END IF
C             determine position in val array
              IPT = YEAR - DATES(1,1) + 1
              IF (IPT .GT. 0  .AND.  IPT .LE. MXVAL) THEN
C               date is within valid range
                IF (VAL(IPT) .LT. .001) THEN
C                 first peak for this year, save it
                  VAL(IPT) = PEAK
                  VALQ(IPT) = PKQLTY ( LENQ, PEAKQ )
                ELSE
C                 there is already a peak for this year
                  IF (PEAK .GT. VAL(IPT)) THEN
C                   this peak is higher, save it
                    VAL(IPT) = PEAK
                    VALQ(IPT) = PKQLTY ( LENQ, PEAKQ )
                  END IF
                END IF
              ELSE IF (IPT .LE. 0) THEN
C               this date is before start date, warning
                WRITE (FE,2001) KTREC, DATES(1,1),
     $                          RECORD(1:40), RECORD(41:80)
              ELSE
C               big problem, too many years, probably input file error
                WRITE (FE,2003) KTREC,  DATES(1,1),
     $                          RECORD(1:40), RECORD(41:80)
              END IF
            ELSE
C             date is invalid, skip
              WRITE (FE,2002) KTREC, RECORD(1:40), RECORD(41:80)
            END IF
            GO TO 290
 270      CONTINUE
C           error reading record, print warning message
            WRITE (FE,2000) RECORD(1:40), RECORD(41:80)
 290      CONTINUE
        END IF
        IF (NEW .EQ. 0) THEN
C         get next record
          CALL INWREC ( INFL,
     M                  KTREC,
     O                  ID, RECORD, NEW )
          IF (NEW .EQ. 0  .AND.
     $       (ID .NE. '3' .AND. ID .NE. '4')) THEN
C           new record not a peak flow record
            NEW = 1
          END IF
        END IF
C       if still peak flow record, continue processing
      IF (NEW .EQ. 0) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWTP2
     I                   ( INFL, FE,
     M                     KTREC, ID, RECORD, STA, NEW )
C
C     + + + PURPOSE + + +
C     Process watstore card image annual peak flow records to
C     be written to a wdm table data set.  Will read both the "3" type
C     annual peak records and the "4" type partial record peaks.  All
C     peaks will be stored in the data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, KTREC, NEW
      CHARACTER*1  ID
      CHARACTER*15 STA
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input peak flow file
C     FE     - Fortran unit number of file for debug output
C     KTREC  - number of records processed
C     ID     - record identifier for input record
C     RECORD - record from input file
C     STA    - station currently being processed
C     NEW    - indicator for processing
C              1 - new station
C              2 - end of file encountered
C              3 - read error
C
C     + + + INCLUDES + + +
      INCLUDE 'ciowtb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + EXTERNALS + + +
      EXTERNAL  INWREC
C
C     + + + INPUT FORMATS + + +
 1002 FORMAT ( I4,I2,I2, F7.0, 12A1, F8.0, 4A1, I4,I2,I2, F8.0, 4A1 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' Warning: unexpected or invalid record at', I6,
     $        /,'          ', A40, /,'          ', A40 )
 2001 FORMAT (/,' Warning:  unexpected or invalid record at', I6,
     $        /,'           too many peaks, maximum expected is', I4,
     $        /,'          ', A40, /,'          ', A40 )
C
C     + + + END SPECIFICATIONS + + +
C
 100  CONTINUE
C       check for same time series
        IF (RECORD(2:16) .NE. STA) THEN
C         station has changed
          NEW = 1
        ELSE IF (NPT .GE. MXPK) THEN
C         too many peaks for this station
          NEW = 2
          WRITE (FE,2001) KTREC, MXPK, RECORD(1:40), RECORD(41:80)
        ELSE
C         expected station - get year, peak, and qualification codes
          NPT = NPT + 1
          READ (RECORD(17:80),1002,ERR=270)
     $         PKYR(NPT), PKMO(NPT), PKDY(NPT), PK(NPT),
     $         (PKQ(I,NPT), I = 1, 12), PKG(NPT),
     $         (PKGQ(I,NPT), I = 1, 4), PKHI(NPT),
     $         GGMO(NPT), GGDY(NPT), GG(NPT),
     $         (GGQ(I,NPT), I = 1, 4)
C           successful read, make sure no false 0.0 data saved
            IF (RECORD(25:31) .EQ. '       ') THEN
C             peak field is blank
              PK(NPT) = -99999.
            END IF
            IF (RECORD(44:51) .EQ. '        ') THEN
C             gage height field is blank
              PKG(NPT) = -99999.
            END IF
            IF (RECORD(64:71) .EQ. '        ') THEN
C             second gage height field is blank
              GG(NPT) = -99999.
            END IF
            GO TO 290
 270      CONTINUE
C           read error
            NPT = NPT - 1
            WRITE (FE,2000) KTREC, RECORD(1:40), RECORD(41:80)
 290      CONTINUE
C
C         get next record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
          IF (NEW .EQ. 0  .AND.
     $       (ID .NE. '3'  .AND.  ID .NE. '4')) THEN
C           new record not a peak flow record
            NEW = 1
          END IF
        END IF
C       if still peak flow record, continue processing
      IF (NEW .EQ. 0) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWHL2
     I                   ( INFL, FE,
     M                     KTREC, ID, RECORD, SESN, NDYS, LOC,
     O                     YEAR, FLAG, NEW )
C
C     + + + PURPOSE + + +
C     Process watstore card image n-day high and/or low flow station
C     identifier records.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, YEAR, KTREC, FLAG, NEW
      CHARACTER*1  ID
      CHARACTER*4  SESN
      CHARACTER*5  NDYS
      CHARACTER*37 LOC
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input peak flow file
C     FE     - Fortran unit number of file for debug output
C     KTREC  - count of records processed
C     ID     - record identifier for input record
C     RECORD - record from input file
C     SESN   - begin and end month of season or year
C              1:2 - 2 digit start month
C              3:4 - 2 digit end month
C     NDYS   - n-day identification
C              1:2 - high or low indication
C                    " 2" - low flow
C                    " 3" - high flow
C                    others - unknown
C              3:5 - number of days for flow
C     LOC    - location identification
C               1:15 - station id
C              16:21 - cross section
C              22:27 - depth
C              28:32 - parameter code
C              33-37 - statistics code
C     YEAR   - starting year for this set of values
C     FLAG   - processing flag
C              0 - same station, n-day, and season
C              1 - same station, new n-day or season
C              2 - new station/location
C     NEW    - indicator for input file
C              1 - file still available
C              2 - end of file encountered
C
C     + + + INCLUDES + + +
      INCLUDE 'catrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CONT
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWREC, NDYTST, INWSTA
C
C     + + + INPUT FORMATS + + +
 1001 FORMAT ( I4 )
 1002 FORMAT ( 2I2 )
 1003 FORMAT ( F6.0 )
 1004 FORMAT ( 2I5 )
C
C     + + + END SPECIFICATIONS + + +
C
      NEW = 1
      CONT = 1
 100  CONTINUE
C       get the starting year
        READ (RECORD(44:47),1001,ERR=138) YEAR
C         status: 0-same n-day, 1-new n-day, 2-new location
          IF (RECORD(2:38)  .EQ. LOC   .AND.
     $        RECORD(39:43) .EQ. NDYS  .AND.
     $        RECORD(52:55) .EQ. SESN) THEN
C           same n-day, season, and location, break in record
            FLAG = 0
          ELSE
C           new time series
            IF (RECORD(39:43) .NE. NDYS) THEN
C             new n-day
              FLAG = 1
              NDYS = RECORD(39:43)
C             tstype: time series type
              CALL NDYTST ( NDYS, ACVAL(17) )
            END IF
            IF (RECORD(52:55) .NE. SESN ) THEN
C             change in season
              FLAG = 1
              SESN = RECORD(52:55)
C             seasbg,seasnd: season begin and end months
              READ (SESN,1002,ERR=118) AIVAL(16), AIVAL(17)
C               successful read
                GO TO 119
 118          CONTINUE
C               read error
                AIVAL(16) = 0
                AIVAL(17) = 0
 119          CONTINUE
            END IF
            IF (RECORD(2:38) .NE. LOC) THEN
C             new location/description
              FLAG = 2
              LOC = RECORD(2:38)
C             staid,istaid: station number
              CALL INWSTA ( RECORD(2:16), ACVAL(1), AIVAL(13) )
              IF (RECORD(17:22) .NE. '999999'  .AND.
     $            RECORD(17:22) .NE. '      ') THEN
C               xseclc: cross-section location
                READ (RECORD(17:22),1003,ERR=148) ARVAL(2)
                  GO TO 149
 148            CONTINUE
                  ARVAL(2) = 0
 149            CONTINUE
              END IF
              IF (RECORD(23:28) .NE. '999999'  .AND.
     $            RECORD(23:28) .NE. '      ') THEN
C               depth: sampling depth
                READ (RECORD(23:28),1004,ERR=158) ARVAL(3)
                  go to 159
 158            CONTINUE
                  ARVAL(3) = 0.0
 159            CONTINUE
              END IF
              IF (RECORD(29:38) .NE. '9999999999'  .AND.
     $            RECORD(29:38) .NE. '          ') THEN
C               parmcd,statcd: parameter and statistics codes
                READ (RECORD(29:38),1004,ERR=168) AIVAL(14), AIVAL(15)
                  GO TO 169
 168            CONTINUE
                  AIVAL(14) = 0
                  AIVAL(15) = 0
 169            CONTINUE
              END IF
            END IF
          END IF
          GO TO 139
 138    CONTINUE
C         error reading starting year
          CONT = 2
          ID = ' '
 139    CONTINUE
        IF (CONT .EQ. 1) THEN
C         get the next record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
          IF (ID .NE. '2'  .OR.  NEW .GT. 1) THEN
C           new kind or record, or read error
            CONT = 2
          END IF
        END IF
C       if next record is a "2" record, go again
      IF (CONT .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWHL3
     I                   ( INFL, FE,
     M                     ID, RECORD, NEW )
C
C     + + + PURPOSE + + +
C     Process watstore card image n-day high and/or low flow records
C     to be written to a wdm time-series data set.  These records are
C     expected to have a "3" in column 1.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   INFL, FE, NEW
      CHARACTER*1  ID
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input peak flow file
C     FE     - Fortran unit number of file for debug output
C     ID     - record identifier for input record
C     RECORD - record from input file
C     NEW    - indicator for processing
C              1 - new station
C              2 - end of file encountered
C
C     + + + INCLUDES + + +
      INCLUDE 'ciowdm.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CONT, IPTY, JPT, J, KOUNT
      REAL      HOLD
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWREC
C
C     + + + INPUT FORMATS + + +
 1010 FORMAT ( F7.0 )
C
C     + + + END SPECIFICATIONS + + +
C
      CONT = 1
      IPTY = 0
 100  CONTINUE
C       read n-day values from record
        JPT = 25
        KOUNT = 0
        DO 220 J = 1, 8
C         look at each field on record
          IF (RECORD(JPT:JPT+6) .EQ. '9999999') THEN
C           no value, force missing to be included
            IPT = IPTY + J
          ELSE IF (RECORD(JPT:JPT+6) .NE. '       ') THEN
C           found data in field, try to read it
            IPT = IPTY + J
            READ (RECORD(JPT:JPT+6),1010,ERR=208) HOLD
C             successful read, save it
              VAL(IPT) = HOLD
              GO TO 210
 208        CONTINUE
C             error on read
              KOUNT = KOUNT + 1
 210        CONTINUE
          END IF
          JPT = JPT + 7
 220    CONTINUE
        IF (KOUNT .NE. 0) THEN
C         one or more errors on input record
          KTERR = KTERR + 1
        END IF
C       get next record
        CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
        IF (ID .EQ. '3') THEN
C         more n-day values, advance pointer 8 years for this record
          IPTY = IPTY + 8
        ELSE
C         not and n-day high/low record
          CONT = 2
        END IF
C       if still "3" record, continue processing
      IF (CONT .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWREC
     I                   ( INFL,
     M                     KTREC,
     O                     ID, RECORD, NEW )
C
C     + + + PURPOSE + + +
C     Reads an 80-character record from the input file INFL.
C     Records containing an asteric (*) in the first position
C     are treated as comments and skipped.  Records with a blank 
C     in the first position are skipped.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      INFL, KTREC, NEW
      CHARACTER*1  ID
      CHARACTER*80 RECORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INFL   - Fortran unit number of the input file
C     KTREC  - number of records processed
C     ID     - record identifier, first character of the input record
C     RECORD - input record
C     NEW    - processing status
C              0 - new record returned
C              2 - end of file reached
C              3 - read error, processing discontinued
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   AGAIN
C
C     + + + INTRINSICS + + +
      INTRINSIC LEN
C
C     + + + INPUT FORMATS + + +
 1001 FORMAT ( A80 )
C
C     + + + END SPECIFICATIONS + + +
C
      NEW = 0
 100  CONTINUE
C       read next input record
        AGAIN = 0
        READ (INFL, 1001,ERR=108,END=109) RECORD
C         record successfully read
          KTREC = KTREC + 1
          IF (RECORD(1:1) .EQ. '*') THEN
C           this is a comment, get another record
            AGAIN = 1
          ELSE IF (RECORD(1:1) .EQ. ' ') THEN
C           unknown type, get another record
            AGAIN = 1
          ELSE IF (LEN(RECORD) .EQ. 0) THEN
C           blank record, get another record
            AGAIN = 1
          ELSE
C           return this record
            ID = RECORD(1:1)
          END IF
          GO TO 110
 108    CONTINUE
C         error reading record, give it up
          NEW = 3
          ID = ' '
          RECORD(2:16) = 'read error      '
          GO TO 110
 109    CONTINUE
C         end of file
          NEW = 2
          ID = ' '
          RECORD(2:16) = 'end of file     '
 110    CONTINUE
C       get another record if last one was comment
      IF (AGAIN .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   RDVALU
     I                   ( LEN, STRING,
     M                     VALUE,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Return a real value from a character string.  The data value may
C     be right or left justified in the string or anywhere in the string.
C     If no decimal is present, it is assumed to follow the last digit.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN, RETC
      REAL        VALUE
      CHARACTER*1 STRING(LEN)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     LEN    - length of string containing number
C     STRING - character strin containing real number
C     VALUE  - real number found in character string,
C              if RETC = -1, this value is not changed.
C     RETC   - return code
C               0 - value successfully read from string
C              -1 - value not read from string
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LENS, ERRFLG
      REAL      HOLD
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, CHDECE
C
C     + + + END SPECIFICATIONS + + +
C
C     get actual length of character string, excluding trailing blanks
      LENS = LENSTR ( LEN, STRING)
C
C     convert character string to real
      CALL CHDECE ( LENS, STRING,
     O              HOLD, ERRFLG )
C
      IF (ERRFLG .EQ. 0) THEN
C       value successfully read
        VALUE = HOLD
        RETC = 0
      ELSE
C       problem with value
        RETC = -1
      END IF
C
      RETURN
      END
