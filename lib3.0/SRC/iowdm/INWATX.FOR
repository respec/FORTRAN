C
C
C
      SUBROUTINE   INWDVX
     I                   ( MESSFL, WDMFL, FE, INFL, OTFL,
     I                     DSSTAT, DSCFRM, DSNINC, LPTH, PTHNAM,
     M                     DSN )
C
C     + + + PURPOSE + + +
C     This routine reads standard WATSTORE card image daily values
C     cards that include the station header information and outputs
C     the information to a WDM file for each data set.
C
C     + + + KEYWORDS + + +
C     WDM file, message file, interactive
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, FE, INFL, OTFL,
     $          DSSTAT, DSCFRM, LPTH, DSNINC, DSN
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of WDM file
C     FE     - Fortran unit number for debug output (0 if none)
C     INFL   - Fortran unit number of daily values file
C     OTFL   - Fortran unit number for summary output
C              0 -no summary output file
C     DSSTAT - data set status
C              1 - new
C              2 - find
C              3 - existing
C     DSCFRM - confirm data set numbers, space, and attributes
C              1 - none
C              2 - first
C              3 - all
C     DSNINC - increment for new data sets
C     DSN    - data-set number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       ONLY1, INITFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWDV1
C
C     + + + END SPECIFICATIONS + + +
C
      ONLY1 = 0
      INITFG= 1
C     process as many data sets as are in watstore file
      CALL INWDV1 ( MESSFL, WDMFL, FE, INFL, OTFL,
     I              DSSTAT, DSCFRM, DSNINC, LPTH, PTHNAM,
     I              ONLY1, INITFG,
     M              DSN )
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWDV1
     I                   ( MESSFL, WDMFL, FE, INFL, OTFL,
     I                     DSSTAT, DSCFRM, DSNINC, LPTH, PTHNAM,
     I                     ONLY1, INITFG,
     M                     DSN )
C
C     + + + PURPOSE + + +
C     This routine reads standard WATSTORE card image daily values
C     cards that include the station header information and outputs
C     the information to a WDM file.  Contains flag to specify if
C     only one data set should be processed at a time.
C
C     + + + KEYWORDS + + +
C     WDM file, message file, interactive
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, FE, INFL, OTFL,
     $          DSSTAT, DSCFRM, LPTH, DSNINC, DSN, ONLY1, INITFG
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of WDM file
C     FE     - Fortran unit number for debug output (0 if none)
C     INFL   - Fortran unit number of daily values file
C     OTFL   - Fortran unit number for summary output
C              0 -no summary output file
C     DSSTAT - data set status
C              1 - new
C              2 - find
C              3 - existing
C     DSCFRM - confirm data set numbers, space, and attributes
C              1 - none
C              2 - first
C              3 - all
C     DSNINC - increment for new data sets
C     DSN    - data-set number
C     ONLY1  - 1 - process only 1 data set at a time
C              0 - process all available data sets
C     INITFG - initialize for new watstore file flag
C
C     + + + COMMONS + + +
      INCLUDE 'ciowdm.inc'
      INCLUDE 'catrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       NEW, LEN, RETC, DSTYPE, CFRM, IZIP, FLAGST
C     integer       i
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
      EXTERNAL   INWHDR, INWAT2, INWAT3, INDSOP, INSVTS, CVWTAT
      EXTERNAL   INWSTA, INRDST, INWREC, INITAX, INWATZ
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
C     + + + debug formats + + +
C3000 format ( '  back from   ktrec   new   sta              other',
C    $       /,' -----------  -----   ---   ---------------  --------')
C3001 format ( ' <-- inwrec:', i7, i5, 21x, 'rec = ', a80 )      
C3002 format ( ' <-- inwsta:', 15x, a15, 3x,
C    $                         'acval = ', 16a1,
C    $                         'aival =', I10 )
C3003 format ( ' <-- inwhdr:', i7, i5, 3x, a15, 3x, 'rec =', a80 )
C3004 format ( ' <-- inwat2:', i7, i5, 3x, a15, 3x, 'rec =', a80 )
C3005 format ( ' --> 3 loop:', i7, i5, 3x, a15, 3x, 'rec =', a80 )
C3006 format ( ' <-- inrdst:', i7, i5, 3x, a15, 3x, 'rec =', a80 )
C3007 format ( ' <-- inwat3:', i7, i5, 3x, a15, 3x, 'rec =', a80,
C    $       /,'            ', 32x, 'ipt =', i5 )
C3008 format ( ' <-- indsop:', i7, i5, 3x, a15, 3x, 'rec =', a80 )
C3009 format ( ' <-- insvts:', i7, i5, 3x, a15, 3x, 'retc =', i5 )
C3010 format ( ' end of 100:', i7, i5, 3x, a15, 3x, 'rec =', a80 )
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
      FLAGST = 1
      CFRM = DSCFRM
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
C     write (fe,3000)
 100  CONTINUE
C       process next record
        IF (ID .EQ. ' ') THEN
C         record is empty or invalid, get a new record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
C         write (fe,3001) ktrec, new, record
        END IF
        IF (ID .EQ. 'Z') THEN
C         agency record
          CALL INWATZ ( INFL, FE,
     M                  KTREC, ID, RECORD, ACVAL(85), NEW )
        END IF
        IF (STA .NE. RECORD(2:16)) THEN
C         new station
          IF (ONLY1.EQ.0 .OR. STA.EQ.'               ') THEN
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
C        write (fe,3002) sta, (acval(i),i=1,16), aival(13)
        END IF
C       process record according to type
        IF (ID .EQ. 'Z'  .OR.  (STA .EQ. RECORD(2:16)  .AND.
     $     (ID .EQ. 'H' .OR. ID .EQ. 'Y' .OR. ID .EQ. 'N'))) THEN
C         same station record, same station
          CALL INWHDR ( INFL, FE, STA,
     M                  KTREC, ID, RECORD, NEW )
C         write (fe,3003) ktrec, new, sta, record
        ELSE IF (ID .EQ. '2'  .AND.  RECORD(2:16) .EQ. STA) THEN
C         parameter/statistics record, same station
          CALL INWAT2 ( INFL, FE,
     M                  KTREC, ID, RECORD, STA, PARMST, NEW,
     O                  AIVAL(14) )
C         write (fe,3004) ktrec, new, sta, record
          CALL CVWTAT ( MESSFL, AIVAL(14), ACVAL(17) )
        ELSE IF (ID .EQ. '3'  .AND.  RECORD(2:16) .EQ. STA) THEN
C         daily values record, same station
C         write (fe,3005) ktrec, new, sta, record
          IF (NEW .EQ. 1) THEN
C           this is a new time series, get base year from record
            READ (RECORD(17:20),1002) AIVAL(6)
C           get the data set number
            DSN = DSN + DSNINC
            CALL INDSOP ( MESSFL, WDMFL, FE, OTFL, LPTH, PTHNAM,
     I                    DSTYPE, DSSTAT, CFRM, DSNINC,
     M                    DSN,
     O                    RETC )
C           write (fe,3008) ktrec, new, sta, record
            IF (CFRM .EQ. 2) THEN
C             confirm only the first time series
              CFRM = 1
            END IF
C           fill time-series array with missing value
            IPT = MXVAL
            CALL ZIPR ( IPT, ARVAL(1), VAL )
            IPT = 0
            NEW = 0
C           set status flag to new
            FLAGST = 1
          END IF
 300      CONTINUE
C           process the daily values records
            IF (INTFG.EQ.1) THEN
C             do interactive routine
              CALL INRDST ( MESSFL, LPTH, PTHNAM, FLAGST, STA, DSN )
            END IF
C           write (fe,3006)  ktrec, new, sta, record
            FLAGST = 2
            CALL INWAT3 ( INFL, FE,
     M                    ID, RECORD, STA, NEW )
C           write (fe,3007)  ktrec, new, sta, record, ipt
            IF (IPT .GT. 0) THEN
C             save the daily values to the wdm
              CALL INSVTS ( WDMFL, DSN, RETC )
C             write (fe,3009) ktrec, new, sta, retc
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
C       write (fe,3010) ktrec, new, sta, record
C       continue processing if end of file not reached
      IF (NEW .NE. 2) GO TO 100
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWUVX
     I                   ( MESSFL, WDMFL, FE, INFL, OTFL,
     I                     DSSTAT, DSCFRM, DSNINC, LPTH, PTHNAM,
     M                     DSN )
C
C     + + + PURPOSE + + +
C     This routine reads standard WATSTORE card image unit values
C     cards that include the station header information and outputs
C     the information to a WDM file.
C
C     + + + KEYWORDS + + +
C     WDM file, message file, interactive
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, FE, INFL, OTFL,
     $          DSSTAT, DSCFRM, LPTH, DSNINC, DSN
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of WDM file
C     FE     - Fortran unit number for debug output (0 if none)
C     INFL   - Fortran unit number of unit values file
C     OTFL   - Fortran unit number for summary output
C              0 -no summary output file
C     DSSTAT - data set status
C              1 - new
C              2 - find
C              3 - existing
C     DSCFRM - confirm data set numbers, space, and attributes
C              1 - none
C              2 - first
C              3 - all
C     DSNINC - increment for new data sets
C     DSN    - data-set number
C
C     + + + COMMONS + + +
      INCLUDE 'ciowdm.inc'
      INCLUDE 'catrib.inc'

C     + + + LOCAL VARIABLES + + +
      INTEGER       NEW, LEN, RETC, DSTYPE, CFRM, IZIP, RPD, FLAGST
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
C     ID     - identifier for type of input record
C              Z - station header, agency record
C              H - station header, location record
C              N - station header, station name record
C              Y - station header, base discharge record
C              2 - unit values parameter and statistics record
C              3 - unit values data record
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWHDR, INWAT2, INWATB, INDSOP, INSVTS, CVWTAT
      EXTERNAL   INWSTA, INRDST, INWREC, INITAX, INWATZ
      EXTERNAL   ZIPI, ZIPR
C
C     + + + DATA INITIALIZATION + + +
      DATA  DSTYPE / 1 /
C
C     + + + INPUT FORMATS + + +
 1002 FORMAT ( I4 )
 1003 FORMAT ( I5 )
C
C     + + + OUTPUT FORMATS + + +
 2001 FORMAT (/,' Warning:  unexpected or invalid record at', I6,
     $                      ', kount is', I6, ' (inwuvx)',
     $        /,'          ', A40, /,'          ', A40 )
C
C     + + + END SPECIFICATIONS + + +
C
C     set wdput arguments
      TSSTEP = 15
      TUNITS = 2
      DTOVWR = 0
      QFLAG  = 0
C
C     initialize counters, pointers, and dates
      KTERR = 0
      KTREC = 0
      IPT = 0
      NEW = 0
      FLAGST = 1
      CFRM = DSCFRM
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
          CALL INITAX
          PARMST = '          '
          NEW = 1
          STA = RECORD(2:16)
          CALL INWSTA ( STA, ACVAL(1), AIVAL(13) )
        END IF
C       process record according to type
        IF (ID .EQ. 'Z'  .OR.  (STA .EQ. RECORD(2:16)  .AND.
     $     (ID .EQ. 'H' .OR. ID .EQ. 'Y' .OR. ID .EQ. 'N'))) THEN
C         station header record, same station
          CALL INWHDR ( INFL, FE, STA,
     M                  KTREC, ID, RECORD, NEW )
        ELSE IF (ID .EQ. '2'  .AND.  RECORD(2:16) .EQ. STA) THEN
C         parameter/statistics record
          CALL INWAT2 ( INFL, FE,
     M                  KTREC, ID, RECORD, STA, PARMST, NEW,
     O                  AIVAL(14) )
          CALL CVWTAT ( MESSFL, AIVAL(14), ACVAL(17) )
        ELSE IF (ID .EQ. 'B'  .AND.  RECORD(2:16) .EQ. STA) THEN
C         unit values record, same station
          IF (NEW .EQ. 1) THEN
C           this is a new time series, get base year from record
            READ (RECORD(17:20),1002) AIVAL(6)
C           get time step from record
            READ (RECORD(31:35),1003) RPD
            IF (RPD .GT. 0  .AND.  RPD .LE. 1440) THEN
C             valid readings per day
              AIVAL(7) = 1440 / RPD
            ELSE
C             readings per day not valid
              AIVAL(7) = 0
            END IF
C           get the data set number
            DSN = DSN + DSNINC
            CALL INDSOP ( MESSFL, WDMFL, FE, OTFL, LPTH, PTHNAM,
     I                    DSTYPE, DSSTAT, CFRM, DSNINC,
     M                    DSN,
     O                    RETC )
            IF (CFRM .EQ. 2) THEN
C             confirm only the first time series
              CFRM = 1
            END IF
C           fill time-series array with missing value
            IPT = MXVAL
            CALL ZIPR ( IPT, ARVAL(1), VAL )
            IPT = 0
            NEW = 0
            FLAGST = 1
          END IF
 300      CONTINUE
C           process the unit values records
            IF (INTFG.EQ.1) THEN
C             do interactive routine
              CALL INRDST ( MESSFL, LPTH, PTHNAM, FLAGST, STA, DSN )
            END IF
            FLAGST = 2
            CALL INWATB ( INFL, FE,
     M                    ID, RECORD, STA, NEW )
            IF (IPT .GT. 0) THEN
C             save unit values to the wdm file
              CALL INSVTS ( WDMFL, DSN, RETC )
              CALL ZIPR ( IPT, ARVAL(1), VAL )
              IPT = 0
              DATES(1,1) = 0
            END IF
C           continue reading daily values if same station
          IF (ID .EQ. 'B'  .AND.  RECORD(2:16) .EQ. STA) GO TO 300
        ELSE IF (NEW .NE. 2) THEN
C         invalid or bad record (not end of file)
          KTERR = KTERR + 1
          WRITE (FE,2001) KTREC, KTERR, RECORD(1:40), RECORD(41:80)
          ID = ' '
        END IF
C       continue processing if end of file not reached
      IF (NEW .NE. 2) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWBCX
     I                   ( MESSFL, WDMFL, FE, INFL, OTFL,
     I                     DSSTAT, DSCFRM, DSTYPE, DSNINC, LPTH, PTHNAM,
     I                     ABNDX, ABNAME,
     M                     DSN )
C
C     + + + PURPOSE + + +
C     This routine reads standard WATSTORE basin characteristics
C     card image records and outputs the information to a WDM file.
C
C     + + + KEYWORDS + + +
C     WDM file, message file, interactive
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, FE, INFL, OTFL,
     $          DSSTAT, DSCFRM, DSTYPE, DSNINC, LPTH, DSN, ABNDX(200)
      CHARACTER*1 ABNAME(6,200)
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of WDM file
C     FE     - Fortran unit number for debug output (0 if none)
C     INFL   - Fortran unit number of unit values file
C     OTFL   - Fortran unit number for summary output
C              0 -no summary output file
C     DSSTAT - data set status
C              1 - new
C              2 - find
C              3 - existing
C     DSCFRM - confirm data set numbers, space, and attributes
C              1 - none
C              2 - first
C              3 - all
C     DSTYPE - type of data set
C              1 - time series
C              2 = table
C     DSNINC - increment for new data sets
C     DSN    - data-set number
C
C     + + + COMMONS + + +
      INCLUDE 'catrib.inc'
      INCLUDE 'ciowdm.inc'

C     + + + LOCAL VARIABLES + + +
      INTEGER       NEW, OLD, LEN, RETC, CFRM, FLAGST,
     $              INDEX(200), KNDX
      REAL          ABVAL(200)
      CHARACTER*1   ID
      CHARACTER*15  STA
      CHARACTER*80  RECORD
C
C     + + + LOCAL DEFINITIONS + + +
C     NEW    - flag indicating record processing status
C              0 - continuing processing the same time series
C              1 - starting to process a new time series
C              2 - processing last input record
C     OLD    - flag indicating data from a previous stations
C              0 - time-series array is empty
C              1 - time-series array contains data
C              2 - time-series array is full
C     ID     - identifier for type of input record
C              Z - station header, agency record
C              H - station header, location record
C              N - station header, station name record
C              Y - station header, base discharge record
C              2 - unit values parameter and statistics record
C              3 - unit values data record
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWAT1, INWATC, INDSOP, INBCA, INRDST, INWREC
      EXTERNAL   ZIPI
C
C     + + + OUTPUT FORMATS + + +
 2001 FORMAT (/,' Warning:  unexpected or invalid record at', I6,
     $                      ', kount is', I6, ' (inwbcx)',
     $        /,'          ', A40, /,'          ', A40 )
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize counters, pointers, and dates
      KTERR = 0
      KTREC = 0
      KNDX = 0
      LEN  = 200
      CALL ZIPI ( LEN, KNDX, INDEX )
      OLD = 0
      NEW = 0
      FLAGST = 1
      CFRM = DSCFRM
      DSN = DSN - DSNINC
C
C     initialize identifiers
      STA    = '               '
      ID     = ' '
C
 100  CONTINUE
C       process next record
        IF (ID .EQ. ' ') THEN
C         record is empty or invalid, get a new record
          CALL INWREC ( INFL, KTREC, ID, RECORD, NEW )
        END IF
C
        IF (RECORD(2:16) .EQ. STA) THEN
C         expected station,  process record according to type
          IF (ID .EQ. '0'  .OR.  ID .EQ. '1') THEN
C           station identifier
            CALL INWAT1 ( INFL, FE,
     M                    KTREC, ID, RECORD, STA, NEW,
     O                    AIVAL, ACVAL )
          ELSE IF (ID .EQ. '2') THEN
C           basin characteristics record
            CALL INWATC ( INFL, FE,
     M                    KTREC,  ID, RECORD,
     M                    STA, NEW, ABVAL, INDEX, KNDX )
          ELSE
C           invalid or bad record (not end of file)
            KTERR = KTERR + 1
            ID = ' '
            WRITE (FE,2001) KTREC, KTERR, RECORD(1:40), RECORD(41:80)
          END IF
        END IF
C
        IF (RECORD(2:16) .NE. STA  .AND.  OLD .EQ. 1) THEN
C         get dsn to save data to
C** Note:  there may be a problem with an invalid record
          DSN = DSN + DSNINC
          CALL INDSOP ( MESSFL, WDMFL, FE, OTFL, LPTH, PTHNAM,
     I                  DSTYPE, DSSTAT, CFRM, DSNINC,
     M                  DSN,
     O                  RETC )
          IF (INTFG.EQ.1) THEN
C           do interactive routine to print status message
            CALL INRDST ( MESSFL, LPTH, PTHNAM, FLAGST, STA, DSN )
          END IF
C         add the basin characteristics attributes
          CALL INBCA ( MESSFL, WDMFL, FE, DSN,
     I                 ABVAL, KNDX, INDEX, ABNDX, ABNAME,
     O                 RETC )
C         update sta
          STA = RECORD(2:16)
C         zero out attribute indexes and counter
          KNDX = 0
          LEN  = 200
          CALL ZIPI ( LEN, KNDX, INDEX )
          IF (CFRM .EQ. 2) THEN
C           confirm only the first time series
            CFRM = 1
          END IF
        ELSE
C         first time thru, initialize STA
          STA = RECORD(2:16)
          OLD = 1
        END IF
C       continue processing if end of file not reached
      IF (NEW .NE. 2) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWPKX
     I                   ( MESSFL, WDMFL, FE, INFL, OTFL, LPTH,
     I                     PTHNAM, DSSTAT, DSCFRM, DSTYPE, CALND,
     I                     DSNINC,
     M                     DSN )
C
C     + + + PURPOSE + + +
C     This routine reads standard WATSTORE card image peak values
C     cards that include the station header information and outputs
C     the information to a WDM file.
C
C     + + + KEYWORDS + + +
C     WDM file, message file, interactive
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, WDMFL, FE, INFL, OTFL, LPTH,
     $            DSSTAT, DSCFRM, DSTYPE, CALND(2), DSNINC, DSN
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of WDM file
C     FE     - Fortran unit number for debug output (0 if none)
C     INFL   - Fortran unit number of daily values file
C     OTFL   - Fortran unit number for summary output
C              0 -no summary output file
C     LPTH   -
C     PTHNAM -
C     DSSTAT - status of data set
C              1 - new
C              2 - existing, to be supplied
C              3 - find
C     DSCFRM - confirm data set numbers, space, and attributes
C              1 - none
C              2 - first
C              3 - all
C     DSTYPE - type of data set
C              1 - time
C              2 - table
C     CALND  - calender months for year or season
C              (1) - beginning month (1-Jan,...12-Dec)
C              (2) - ending month (1-Jan,...12-Dec)
C     DSNINC - increment for new data sets
C     DSN    - data-set number
C
C     + + + COMMONS + + +
      INCLUDE 'ciowdm.inc'
      INCLUDE 'ciowtb.inc'
      INCLUDE 'catrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       NEW, LEN, RETC, CFRM, IZIP, YEAR, MXCOL, MXROW
      INTEGER       FLAGST, INDX
      REAL          TRBUFF(MXCL,MXPK)
      CHARACTER*1   ID, SVTYPE(4)
      CHARACTER*6   NAME
      CHARACTER*15  STA
      CHARACTER*80  RECORD
C
C     + + + LOCAL DEFINITIONS + + +
C     NEW    - flag indicating record processing status
C              0 - continuing processing the same time series
C              1 - starting to process a new time series
C              2 - processing last input record
C     ID     - identifier for type of input record
C              Z - station header, agency record
C              H - station header, location record
C              N - station header, station name record
C              Y - station header, base discharge record
C              2 - daily values parameter and statistics record
C              3 - daily values data record
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWHDR, INSVTS, INWTP1, INWTP2, INSVTB, INDSOP
      EXTERNAL   INWATZ, INTBBF, INWSTA, INRDST, INITAX
      EXTERNAL   ZIPI, ZIPR, WDBSAI, CHRCHR, INWREC
C
C     + + + INPUT FORMATS + + +
 1002 FORMAT ( I4 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' Warning:  unexpected or invalid record at', I6,
     $                      ', kount is', I6, ' (inwpkx)',
     $        /,'          ', A40, /,'          ', A40 )
 2001 FORMAT (   2X, '              ', A6, 2X, I10 )
 2011 FORMAT (   2X, '    --->      ', A6, 2X, 'return code = ', I3,
     $               ',  value =', I10 )
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize counters, pointers, and flags
      KTERR = 0
      KTREC = 0
      IPT = 0
      NEW = 0
      FLAGST = 1
      CFRM = DSCFRM
      DSN = DSN - DSNINC
      IZIP = 0
C     default tstype to PEAK
      ACVAL(17) = 'P'
      ACVAL(18) = 'E'
      ACVAL(19) = 'A'
      ACVAL(20) = 'K'
      LEN = 4
      CALL CHRCHR ( LEN, ACVAL(17), SVTYPE )
      IF (DSTYPE .EQ. 1) THEN
C       time-series data set, set related values
        LEN = 18
        CALL ZIPI ( LEN, IZIP, DATES )
        DATES(1,1) = 0
        DATES(2,1) = 1
        DATES(3,1) = 1
        DATES(4,1) = 0
        DATES(5,1) = 0
        DATES(6,1) = 0
C       set wdput arguments
        TSSTEP = 1
        TUNITS = 6
        DTOVWR = 0
        QFLAG  = -1
      ELSE
C       table data set, set related values
        MXCOL = MXCL
        MXROW = MXPK
      END IF
C
C     initialize identifiers
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
          CALL INITAX
          LEN = 4
          CALL CHRCHR ( LEN, SVTYPE, ACVAL(17) )
          NEW = 1
          STA = RECORD(2:16)
          CALL INWSTA ( STA, ACVAL(1), AIVAL(13) )
        END IF
        IF (STA .EQ. RECORD(2:16)  .AND.
     $     (ID .EQ. 'H' .OR. ID .EQ. 'Y' .OR. ID .EQ. 'N')) THEN
C         station header record, same station
          CALL INWHDR ( INFL, FE, STA,
     M                  KTREC, ID, RECORD, NEW )
        ELSE IF (ID .EQ. '3'  .OR.  ID .EQ. '4') THEN
C         peak flow record, same station, get base year from record
          READ (RECORD(17:20),1002) YEAR
          AIVAL(6) = ( (YEAR - 1) / 100 )  *  100
C         get the data set number
          DSN = DSN + DSNINC
          CALL INDSOP ( MESSFL, WDMFL, FE, OTFL, LPTH, PTHNAM,
     I                  DSTYPE, DSSTAT, CFRM, DSNINC,
     M                  DSN, RETC )
C         save tstype
          LEN = 4
          CALL CHRCHR ( LEN, ACVAL(17), SVTYPE )
          IF (CFRM .EQ. 2) THEN
C           confirm only the first time series
            CFRM = 1
          END IF
          IF (INTFG.EQ.1) THEN
C           do interactive routine to print status message
            CALL INRDST ( MESSFL, LPTH, PTHNAM, FLAGST, STA, DSN )
          END IF
          IF (DSTYPE .EQ. 1) THEN
C           time series, save calendar as attributes
            LEN = 1
            INDX = 256
            NAME = 'SEASBG'
            CALL WDBSAI (WDMFL, DSN, MESSFL, INDX, LEN, CALND(1), RETC)
            IF (RETC .EQ. 0) THEN
              WRITE (FE,2001) NAME, CALND(1)
            ELSE
              WRITE (FE,2011) NAME, RETC, CALND(1)
            END IF
            INDX = 257
            NAME = 'SEASND'
            CALL WDBSAI (WDMFL, DSN, MESSFL, INDX, LEN, CALND(2), RETC)
            IF (RETC .EQ. 0) THEN
              WRITE (FE,2001) NAME, CALND(2)
            ELSE
              WRITE (FE,2011) NAME, RETC, CALND(2)
            END IF
C           fill array with missing value
            IPT = MXVAL
            CALL ZIPR ( IPT, ARVAL(1), VAL )
            CALL ZIPI ( IPT, IZIP, VALQ )
            IPT = 0
            NEW = 0
            CALL INWTP1 ( INFL, FE, CALND,
     M                    ID, RECORD, STA, NEW )
            IF (IPT .GT. 0) THEN
C             save the peak flows
              CALL INSVTS ( WDMFL, DSN, RETC )
              IPT = 0
              DATES(1,1) = 0
            END IF
          ELSE IF (DSTYPE .EQ. 2) THEN
C           table data set
            NPT = 0
            NEW = 0
            CALL INWTP2 (INFL, FE,
     M                   KTREC, ID, RECORD, STA, NEW )
            IF (NPT .GT. 0) THEN
C             move data into table buffer
              CALL INTBBF ( MXCOL, MXROW,
     O                      TRBUFF, RETC )
C             write table to dsn
              CALL INSVTB ( MESSFL, WDMFL, DSN,
     I                      MXCOL, MXROW, TRBUFF,
     O                      RETC )
            END IF
          END IF
        ELSE IF (NEW .NE. 2) THEN
C         invalid or bad record
          KTERR = KTERR + 1
          ID = ' '
          WRITE (FE,2000) KTREC, KTERR, RECORD(1:40), RECORD(41:80)
        END IF
C       continue processing if end of file not reached
      IF (NEW .NE. 2) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INTBBF
     I                   ( MXCOL, MXROW,
     O                     TRBUFF, RETC )
C
C     + + + PURPOSE + + +
C     Puts the peak flow values arrays into the table buffer.
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MXCOL, MXROW, RETC
      REAL      TRBUFF(MXCOL,MXROW)
C
C     + + + COMMONS + + +
      INCLUDE 'ciowtb.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NINDEX, INDEX, LENTH
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTBCDI, WTBCDR, WTBCDC
C
C     + + + END SPECIFICATIONS + + +
C
      RETC = 0
      IF (NPT .GT. MXROW) THEN
C       too many rows
        RETC = -1
      ELSE
C       put the data in the buffer
        NINDEX = 14
C       put year in buffer
        INDEX = 1
        CALL WTBCDI ( NPT, NINDEX, INDEX, PKYR, TRBUFF )
C
C       put month in buffer
        INDEX = 2
        CALL WTBCDI ( NPT, NINDEX, INDEX, PKMO, TRBUFF )
C
C       put day in buffer
        INDEX = 3
        CALL WTBCDI ( NPT, NINDEX, INDEX, PKDY, TRBUFF )
C
C       put peak in buffer
        INDEX = 4
        CALL WTBCDR ( NPT, NINDEX, INDEX, PK, TRBUFF )
C
C       put peak qualification codes in buffer
        INDEX = 5
        LENTH = 12
        CALL WTBCDC ( NPT, NINDEX, INDEX, LENTH, PKQ, TRBUFF )
C
C       put associated gage height in buffer
        INDEX = 8
        CALL WTBCDR ( NPT, NINDEX, INDEX, PKG, TRBUFF )
C
C       put associated gage height qualification codes in buffer
        INDEX = 9
        LENTH = 4
        CALL WTBCDC ( NPT, NINDEX, INDEX, LENTH, PKGQ, TRBUFF )
C
C       put highest peak since year in buffer
        INDEX = 10
        CALL WTBCDI ( NPT, NINDEX, INDEX, PKHI, TRBUFF )
C
C       put month of highest gage height IN buffer
        INDEX = 11
        CALL WTBCDI ( NPT, NINDEX, INDEX, GGMO, TRBUFF )
C
C       put day of highest gage height in buffer
        INDEX = 12
        CALL WTBCDI ( NPT, NINDEX, INDEX, GGDY, TRBUFF )
C
C       put highest gage height in buffer
        INDEX = 13
        CALL WTBCDR ( NPT, NINDEX, INDEX, GG, TRBUFF )
C
C       put highest gage height qualifications codes in buffer
        INDEX = 14
        LENTH = 4
        CALL WTBCDC ( NPT, NINDEX, INDEX, LENTH, GGQ, TRBUFF )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWHLX
     I                   ( MESSFL, WDMFL, FE, INFL, OTFL,
     I                     DSSTAT, DSCFRM, DSNINC, LPTH, PTHNAM,
     M                     DSN )
C
C     + + + PURPOSE + + +
C     This routine reads standard WATSTORE card image n-day high
C     and/or low flow values records and outputs the information
C     to a WDM file.
C
C     + + + KEYWORDS + + +
C     WDM file, message file, interactive
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, FE, INFL, OTFL,
     $          DSSTAT, DSCFRM, LPTH, DSNINC(2), DSN
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of WDM file
C     FE     - Fortran unit number for debug output (0 if none)
C     INFL   - Fortran unit number of daily values file
C     OTFL   - Fortran unit number for summary output
C              0 -no summary output file
C     DSSTAT - data set status
C              1 - new
C              2 - find
C              3 - enter
C     DSCFRM - confirm data set numbers, space, and attributes
C              1 - none
C              2 - first dsn
C              3 - each station
C              3 - all data sets
C     DSNINC - increment for new data sets
C              (1) - increment between n-day for same station
C              (2) - increment between different stations
C     DSN    - data-set number
C
C     + + + COMMONS + + +
      INCLUDE 'ciowdm.inc'
      INCLUDE 'diowdm.inc'
      INCLUDE 'catrib.inc'
      INCLUDE 'datrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       NEW, LEN, RETC, DSTYPE, CFRM, IZIP, FLAG,
     $              MXD, FLAGST, YEAR, DSNT, DSCPY, DSNEW
      CHARACTER*1   ID
      CHARACTER*4   SESN
      CHARACTER*5   NDYS
      CHARACTER*37  LOC
      CHARACTER*80  RECORD
C
C     + + + LOCAL DEFINITIONS + + +
C     FLAGST - 1 - new station
C              2 - continue with same station
C
C     + + + EXTERNALS + + +
      EXTERNAL   INWREC, INWHL2, INWHL3, INDSOP, INSVTS, INRDST
      EXTERNAL   ZIPI, ZIPR
C
C     + + + DATA INITIALIZATION + + +
      DATA  DSTYPE / 1 /
C
C     + + + OUTPUT FORMATS + + +
 2001 FORMAT (/,' Warning:  unexpected or invalid record at', I6,
     $                      ', kount is', I6, ' (inhlvx)',
     $        /,'          ', A40, /,'          ', A40 )
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize counters, pointers, and flags
      KTERR = 0
      KTREC = 0
      IPT   = 0
      NEW   = 0
      MXD   = 200
      CFRM  = DSCFRM
      DSCPY = 4
      FLAGST = 1
      DSNEW = 2
      DSNT  = DSN
      TSSTEP = 1
      TUNITS = 6
      DTOVWR = 0
      QFLAG  = 0
      LEN    = 18
      IZIP   = 0
      CALL ZIPI ( LEN, IZIP, DATES )
      DATES(2,1) = 1
      DATES(3,1) = 1
C
C     initialize identifiers
      ID  = ' '
      SESN = '    '
      NDYS = '     '
      LOC  = '                                     '
C
 100  CONTINUE
C       process next record
        IF (ID .EQ. ' ') THEN
C         record is empty or invalid, get a new record
          CALL INWREC (INFL, KTREC, ID, RECORD, NEW )
        END IF
C
        IF (ID .EQ. '2') THEN
C         station identifier record
          CALL INWHL2 ( INFL, FE,
     M                  KTREC, ID, RECORD, SESN, NDYS, LOC,
     O                  YEAR, FLAG, NEW )
          IF (FLAG .EQ. 1) THEN
C           processing same station but new n-day, copy attributes
            CALL INDSOP ( MESSFL, WDMFL, FE, OTFL, LPTH, PTHNAM,
     I                    DSTYPE, DSCPY, CFRM, DSNINC(1),
     M                    DSN,
     O                    RETC )
          ELSE IF (FLAG .EQ. 2) THEN
C           new station or location, find attributes
            IF (DSCFRM .GE. 3) THEN
C             confirm each station or each data set
              CFRM = DSCFRM
              FLAGST = 1
            END IF
            CALL INDSOP ( MESSFL, WDMFL, FE, OTFL, LPTH, PTHNAM,
     I                    DSTYPE, DSNEW, CFRM, DSNINC(2),
     M                    DSNT,
     O                    RETC )
            IF (DSCFRM .LE. 3) THEN
C             confirm no, first, or each station
              CFRM = 1
            END IF
            DSN = DSNT
            DSNT = DSNT + DSNINC(2)
          END IF
        END IF
        IF (ID .EQ. '3') THEN
C         n-day record, print station
          IF (INTFG.EQ.1) THEN
C           do interactive routine to print station
            CALL INRDST ( MESSFL, LPTH, PTHNAM, FLAGST, LOC(1:15), DSN )
          END IF
          FLAGST = 2
          CALL ZIPR ( MXD, ARVAL(1), VAL )
          IPT = 0
          DATES(1,1) = YEAR
          CALL INWHL3 ( INFL, FE,
     M                  ID, RECORD, NEW )
C         save n-day to data set
          CALL INSVTS ( WDMFL, DSN, RETC )
        END IF
        IF (ID .NE. '2'  .AND.  ID .NE. '3'  .AND.  ID .NE. ' ') THEN
C         invalid record
          KTERR = KTERR + 1
          ID = ' '
          WRITE (FE,2001) KTREC, KTERR, RECORD(1:40), RECORD(41:80)
        END IF
C       continue processing input if not done
      IF (NEW .LT. 2) GO TO 100
C
      RETURN
      END
