C
C
C
      SUBROUTINE   INDSOP
     I                    ( MESSFL, WDMFL, FE, OT, LPTH, PTHNAM,
     I                      DSTYPE, DSSTAT, DSCFRM, DSNINC,
     M                      DSN,
     O                      RETC )
C
C     + + + PURPOSE + + +
C     For new data set being processed, either add a new
C     data set and attributes to the wdm file or identify an
C     existing data set to be used.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, WDMFL, FE, OT, LPTH, DSTYPE, DSSTAT,
     $          DSCFRM, DSNINC, DSN, RETC
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     WDMFL  - Fortran unit number of wdm file
C     FE     - Fortran unit number for debugging output
C     OT     - Fortran unit number for optional summary output
C     LPTH   - number of window paths
C     PTHNAM - array of window paths
C     DSTYPE - data-set type
C              1 - time series
C              2 - table
C     DSSTAT - data-set status
C              1 - new data set
C              2 - find a matching data set
C              3 - enter existing data set
C     DSCFRM - confirm data-set numbers, space, and attributes
C              1 - never
C              2 - first
C              3 - all
C     DSNINC - increment for successive new data sets
C     DSN    - data set for output
C     RETC   - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RET, SCLU, SGRP, OPT, DSNF, DSNFLG, RTC
C
C     + + + LOCAL DEFINITIONS + + +
C     OPT    - processing option flag
C              1 - ask about data set number and DSSTAT
C              2 - ask user for time attributes and space
C              3 - ask user for descriptive attributes
C              4 - continue processing time series
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWNSOP, QRESP
      EXTERNAL   INFREE, INDSLC, INDSSP, INDSGT
C
C     + + + DATA INITIALIZATION + + +
      DATA  SCLU
     $     / 104 /
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize flags
      RETC = 0
      DSNFLG = 0
C     get a free dsn
      CALL INFREE ( WDMFL, DSTYPE, DSN, DSNINC,
     O              DSNF, RET )
C
C     determine what and how to add to dsn
      IF (DSCFRM .GT. 1  .AND.  RET .EQ. 0) THEN
C       confirm dsn, space, and attributes with user
C       initial opt for location and description
        OPT = 2
 100    CONTINUE
          IF (DSSTAT .EQ. 3) THEN
C           user will specify dsn, include opt 4
            IF (DSTYPE .EQ. 1) THEN
C             opt:  1-continue, 2-loc&descr, 3-space&time, 4-dsn
              SGRP = 1
            ELSE
C             opt:  1-continue, 2-loc&descr, 3-space, 4-dsn
              SGRP = 2
            END IF
          ELSE
C           dsn is new or will be found
            IF (DSTYPE .EQ. 1) THEN
C             opt:  1-continue, 2-loc&descr, 3-space&time
              SGRP = 3
            ELSE
C             opt:  1-continue, 2-loc&descr, 3-space
              SGRP = 4
            END IF
          END IF
          CALL ZWNSOP ( LPTH, PTHNAM )
          CALL QRESP ( MESSFL, SCLU, SGRP, OPT )
          IF (OPT .EQ. 2) THEN
C           location and description attributes
            CALL INDSLC ( MESSFL, LPTH, PTHNAM, RTC )
C           now default to space&time
            OPT = 3
          ELSE IF (OPT .EQ. 3) THEN
C           space allocation and/or time attr
            CALL INDSSP ( MESSFL, LPTH, PTHNAM, DSTYPE, RTC )
C           now default to data set
            OPT = 4
          ELSE IF (OPT .EQ. 4) THEN
C           data-set number
            CALL INDSGT ( MESSFL, WDMFL, FE, LPTH, PTHNAM,
     I                    DSSTAT, DSCFRM, DSTYPE, DSNINC,
     M                    DSN,
     O                    RETC )
            IF (RETC .EQ. 1) THEN
C             found or created new data set
              DSNFLG = 1
            ELSE
C             some problem
              DSNFLG = 0
            END IF
          END IF
        IF (OPT .NE. 1) GO TO 100
C       end of loop to confirm dsn, space and attributes
      END IF
C
      IF (DSNFLG .EQ. 0) THEN
C       user did not select a dsn, do it
        CALL INDSGT ( MESSFL, WDMFL, FE, LPTH, PTHNAM,
     I                DSSTAT, DSCFRM, DSTYPE, DSNINC,
     M                DSN,
     O                RETC )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INDSLC
     I                 ( MESSFL, LPTH, PTHNAM,
     O                   RTC )
C
C     + + + PURPOSE + + +
C     Asks user to confirm/modify location and description attributes.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, LPTH, RTC
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     LPTH   - number of window paths
C     PTHNAM - array of window paths
C     RTC    - ?????
C
C     + + + INCLUDES + + +
      INCLUDE 'catrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU, SGRP(4), NUMI(4), NUMR(4), NUMC(4), VER, RTCMND,
     $          AGAIN
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZWNSOP
      EXTERNAL  Q1INIT, QSETR, QSETI, QSETCT
      EXTERNAL  Q1EDIT, QGETR, QGETI, QGETCT
C
C     + + + DATA INITIALIZATION + + +
      DATA  SCLU / 104 /,
     $      SGRP / 10, 11, 12, 13 /,
     $      NUMI /  7,  3,  5,  1 /,
     $      NUMR /  7,  0,  4,  0 /,
     $      NUMC /  7,  4,  3,  3 /
C
C     + + + END SPECIFICATIONS + + +
C
C     determine version for location and description info
      VER = AVER
      IF (VER .LT. 1  .OR.  VER .GT. 4) THEN
C       unknown version, use minimum question
        VER = 4
      END IF
C
 100  CONTINUE
        AGAIN = 0
C       set path name and initialize question
        CALL ZWNSOP ( LPTH, PTHNAM )
        CALL Q1INIT ( MESSFL, SCLU, SGRP(VER) )
C       istaid & additional integer attributes
        CALL QSETI ( NUMI(VER), AIVAL(13) )
        IF (NUMR(VER) .GT. 0) THEN
C         real attributes
          CALL QSETR ( NUMR(VER), ARVAL(2) )
        END IF
C       staid,tstype,stanam, & additional character attributes
        CALL QSETCT ( NUMC(VER), ACLEN(1), ACLONG, ACVAL(1) )
C       edit the screen
        CALL Q1EDIT ( RTCMND )
C
        IF (RTCMND .EQ. 1) THEN
C         user wants to continue, set values
          CALL QGETI ( NUMI(VER), AIVAL(13) )
          IF (NUMR(VER) .GT. 0) THEN
            CALL QGETR ( NUMR(VER), ARVAL(2) )
          END IF
          CALL QGETCT ( NUMC(VER), ACLEN(1), ACLONG, ACVAL(1) )
          RTC = 1
        ELSE IF (RTCMND .EQ. -1) THEN
C         oops, try again
          AGAIN = 1
        ELSE
C         assume prev, leave attributes unchanged
          RTC = 2
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   INDSSP
     I                   ( MESSFL, LPTH, PTHNAM, DSTYPE,
     O                     RTC )
C
C     + + + PURPOSE + + +
C     Ask the user to verify/modify space allocation and time attributes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, LPTH, DSTYPE, RTC
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     LPTH   - number of window paths
C     PTHNAM - array of window paths
C     RTC    - ?????
C
C     + + + INCLUDES + + +
      INCLUDE 'catrib.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU, SGRP, NUM, RTCMND, AGAIN, RETC, LEN3
      CHARACTER*8  PATH(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL  CKSPAC
      EXTERNAL  ZWNSOP
      EXTERNAL  Q1INIT, QSETR, QSETI, QSETCO
      EXTERNAL  Q1EDIT, QGETR, QGETI, QGETCO
C
C     + + + DATA INITIALZATIONS + + +
      DATA  SCLU, LEN3, PATH
     $     / 104,    3, 'I','&','PS' /
C
C     + + + END SPECIFICATIONS + + +
C
      PATH(2) = PTHNAM(1)
      IF (DSTYPE .EQ. 1) THEN
C       space allocation and time attributes
 100    CONTINUE
          AGAIN = 0
          SGRP = 20
          CALL ZWNSOP ( LPTH, PTHNAM )
          CALL Q1INIT ( MESSFL, SCLU, SGRP )
C         initialize fields
C         tcode, tgroup (adjusted), tsform, compfg, vbtime
          AIVAL(2) = AIVAL(2) - 2
          NUM = 5
          CALL QSETCO ( NUM, AIVAL(1) )
C         tsbyr,tsstep, max # attr, max space for attr, max groups
          NUM = 5
          CALL QSETI ( NUM, AIVAL(6) )
C         tsfill
          NUM = 1
          CALL QSETR ( NUM, ARVAL(1) )
C         edit the screen
          CALL Q1EDIT ( RTCMND )
          IF (RTCMND .EQ. 1) THEN
C           user wants to continue, get values
C           tcode, tgroup (adjusted), tsform, compfg, vbtime
            NUM = 5
            CALL QGETCO ( NUM, AIVAL(1) )
            AIVAL(2) = AIVAL(2) + 2
C           tsbyr, tsstep, max # attr, max space for attr, max groups
            NUM = 5
            CALL QGETI ( NUM, AIVAL(6) )
C           tsfill
            NUM = 1
            CALL QGETR ( NUM, ARVAL(1) )
            CALL CKSPAC ( MESSFL, LEN3, PATH, AIVAL(8), AIVAL(9),
     I                    AIVAL(10), AIVAL(11), AIVAL(12),
     O                    RETC )
            IF (RETC .EQ. 0) THEN
C             data set space allocation is ok
              RTC = 1
            ELSE
C             problem with data set space allocation, try again
              AGAIN = 1
            END IF
          ELSE IF (RTCMND .EQ. -1) THEN
C           oops, try again
            AGAIN = 1
          ELSE
C           leave space allocation and time attr unchanged, adjust tgroup
            AIVAL(2) = AIVAL(2) + 2
            RTC = 2
          END IF
        IF (AGAIN .EQ. 1) GO TO 100
      ELSE
C       space allocation
 200    CONTINUE
          AGAIN = 0
          SGRP = 21
          CALL ZWNSOP ( LPTH, PTHNAM )
          CALL Q1INIT ( MESSFL, SCLU, SGRP )
C         initialize fields: max # attr, max space for attr, max groups
          NUM = 3
          CALL QSETI ( NUM, AIVAL(8) )
C         edit the screen
          CALL Q1EDIT ( RTCMND )
          IF (RTCMND .EQ. 1) THEN
C           get values from screen
            NUM = 3
            CALL QGETI ( NUM, AIVAL(8) )
            CALL CKSPAC ( MESSFL, LEN3, PATH, AIVAL(8), AIVAL(9),
     I                    AIVAL(10), AIVAL(11), AIVAL(12),
     O                    RETC )
            IF (RETC .EQ. 0) THEN
C             data set space allocation is ok
              RTC = 1
            ELSE
C             problem with data set space allocation, try again
              AGAIN = 1
            END IF
          ELSE IF (RTCMND .EQ. -1) THEN
C           oops, try again
            AGAIN = 1
          ELSE
C           leave space unchanged
            RTC = 2
          END IF
        IF (AGAIN .EQ. 1) GO TO 200
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INDSGT
     I                   ( MESSFL, WDMFL, FE, LPTH, PTHNAM,
     I                     DSSTAT, DSCFRM, DSTYPE, DSNINC,
     M                     DSN,
     O                     RETC )
C
C     + + + PURPOSE + + +
C     Gets the number of the data set to which the new data should
C     be placed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, WDMFL, FE, LPTH, DSSTAT, DSCFRM, DSTYPE,
     $            DSNINC, DSN, RETC
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMFL  - Fortran unit number of WDM file
C     FE     - Fortran unit number for debug output (0 if none)
C     LPTH   - number of window paths
C     PTHNAM - array of window paths
C     DSSTAT - status of data set
C              1 - new
C              2 - look for a match
C              3 - existing, to be supplied
C              4 - copy attributes from dsn to new dsn+dsninc
C     DSCFRM - confirm data set numbers, space, and attributes
C              1 - none
C              2 - first
C              3 - all
C     DSTYPE - type of data set
C              1 - time
C              2 - table
C     DSNINC - increment for new data sets
C     DSN    - data-set number
C     RETC   - return code
C               1 - successful
C              -1 - problem adding attributes
C
C     + + + COMMONS + + +
      INCLUDE 'catrib.inc'
C
C     + + + PARAMETERS + + +
      INTEGER   DSNMX
      PARAMETER (DSNMX=1)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   STAT, DSNF, RET, TSSTEP, TCODE,
     $          DSNFND(DSNMX), DSNKNT, DSNMAX, PSA, WDMNW
C
C     + + + EXTERNALS + + +
      EXTERNAL  INFREE, WDLBAX, INATRB, INDSFN, WDDSCL, PRWMSE, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      STAT = DSSTAT
      DSNMAX = DSNMX
      WDMNW  = WDMFL
      IF (STAT .EQ. 2) THEN
C       try to find a matching data set
        IF (DSTYPE .EQ. 1) THEN
C         time series attributes
          TSSTEP = AIVAL(7)
          TCODE  = AIVAL(1)
        ELSE
C         not time series
          TSSTEP = 0
          TCODE  = 0
        END IF
C       look for the data set
        CALL INDSFN ( WDMFL, DSTYPE, ACVAL(17), ACVAL(1), AIVAL(13),
     I                TSSTEP, TCODE, DSNMAX,
     O                DSNFND, DSNKNT )
        IF (DSNKNT .EQ. 1) THEN
C         found a single matching data set, use it
          DSN = DSNFND(1)
          RETC = 1
        ELSE
C         found no or multiple matching data sets, create a new one
          STAT = 1
        END IF
      END IF
C
      IF (STAT .EQ. 3) THEN
C       use standard select to get data set number
        DSNKNT = 0
        CALL ZIPI (DSNMAX,DSNKNT,DSNFND)
        CALL PRWMSE ( MESSFL, WDMFL, DSNMAX, PTHNAM,
     M                DSNFND, DSNKNT )
        IF (DSNKNT .EQ. 1) THEN
C         single data set selected
          DSN = DSNFND(1)
          RETC = 1
        ELSE
C         no data set selected, default to create
          STAT = 1
        END IF
      END IF
C
      IF (STAT .EQ. 4) THEN
C       copy attributes from dsn to new dsn+dsninc, is dsn+dsninc free?
        CALL INFREE ( WDMFL, DSTYPE, DSN, DSNINC,
     O                DSNF, RET )
        IF (RET .EQ. 0) THEN
C         found data set to be free, copy in dsn attributes
          CALL WDDSCL ( WDMFL, DSN, WDMNW, DSNF, DSTYPE, RET )
          IF (RET .EQ. 0) THEN
C           attributes successfully copied into new dsn
            DSN = DSNF
          ELSE
C           problem copying attributes, create a new data set
            STAT = 1
          END IF
          RETC = 1
        ELSE
C         problem finding free dsn, try creating a new dsn
          STAT = 1
        END IF
      END IF
C
      IF (STAT .EQ. 1) THEN
C       new data set, get free dsn
        CALL INFREE ( WDMFL, DSTYPE, DSN, DSNINC,
     O                DSNF, RET )
        IF (RET .EQ. 0) THEN
C         add data set
          DSN = DSNF
          CALL WDLBAX ( WDMFL, DSN, DSTYPE, AIVAL(11), AIVAL(12),
     I                  AIVAL(8), AIVAL(9), AIVAL(10),
     O                  PSA )
          RETC = 1
        ELSE
C         couldn't find a free dsn
          RETC = -1
        END IF
      END IF
C
      IF (RETC .EQ. 1  .AND.  STAT .NE. 3) THEN
C       data set found, copied, or created, add/update attributes
        CALL INATRB ( MESSFL, FE, WDMFL, DSN, RET )
        IF (RET .NE. 0) THEN
C         problem, set return code
          RETC = -1
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INITBC
     I                   ( MESSFL,
     O                     ABNDX, ABNAME )
C
C     + + + PURPOSE + + +
C     Inintialize arrays for attribute identification for
C     basin characteristics.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, ABNDX(200)
      CHARACTER*1 ABNAME(6,200)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     ABNDX  - array containing attribute numbers for the
C              basin characteristics
C     ABNAME - array containing attribute name for the basin
C              characteristics
C
C     + + + PARAMETERS + + +
      INCLUDE 'pbcatr.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   J, CONT, INIT, OLEN, SCLU, SGRP, IPT, LEN6
      CHARACTER*1 TXT(26)
      CHARACTER*3 TEMP
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYC
      EXTERNAL   WMSGTT
C
C     + + + DATA INITIALIZATIONS + + +
C          missing value indicators for attributes
      DATA  SCLU, LEN6
     $     / 106,    6 /
C
C     + + + INPUT FORMATS + + +
 1001 FORMAT ( I3 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ( 6A1 )
C
C     + + + END SPECIFICATIONS + + +
C
C     get basin characteristics attribute names and index numbers
      SGRP = 1
      INIT = 1
      IPT = 0
 200  CONTINUE
C       get the next line from the message file
        OLEN = 26
        CALL WMSGTT ( MESSFL, SCLU, SGRP, INIT,
     M                OLEN,
     O                TXT, CONT )
        INIT = 0
        IPT = IPT + 1
        CALL COPYC ( LEN6, TXT(19), ABNAME(1,IPT) )
        WRITE (TEMP,2000) (TXT(J), J = 15, 17)
        READ  (TEMP,1001) ABNDX(IPT)
      IF (IPT .LT. 200) GO TO 200
C
      RETURN
      END
C
C
C
      SUBROUTINE   INOPTN
     I                   ( MESSFL, LPTH, PTHNAM, OPTV,
     M                     OPTS, DSNS, CLND,
     O                     RTC )
C
C     + + + PURPOSE + + +
C     Returns processing options for inputting data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL, LPTH, OPTV, OPTS(3), DSNS(3), CLND(2), RTC
      CHARACTER*8 PTHNAM(LPTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     LPTH   - number of window paths
C     PTHNAM - array of window paths
C     OPTV   - indicator flag for version of options screen
C              1: dscfrm, dsstat, dsn, dsninc
C              2: dscfrm, dsstat, dstype, dsn, dsninc
C              3: dscfrm, dsstat, dstype, dsn, dsninc
C              4: dscfrm, dsstat, dsn, dsni, dsnj
C              5: dscfrm, dsstat, dsn, dsni, dsnj
C     OPTS   - array containing option flags
C              OPTV=1: dscfrm:1-no,2-first,3-all
C                      dsstat:1-new,2-find,3-enter
C                      not used
C                =2,3: dscfrm:1-no,2-first,3-all
C                      dsstat:1-new,2-find,3-enter
C                      dstype:1-time,2-table
C                  =4: dscfrm:1-no,2-first dsn,3-each sta,4-all nday
C                      dsstat:1-new,2-find,3-enter
C                      not used
C                  =5: dscfrm: 1-no,2-first column,3-all columns
C                      dsstat:1-new,2-find,3-enter
C                      not used
C     DSNS   - data-set number and increment information
C              OPTV=1: dsn: starting dsn
C                      dsninc: increment between new data sets
C                      not used
C                =2,3: same as 1
C                  =4: dsn: starting dsn
C                      dsni: increment between n-day
C                      dsnj: increment between stations
C                  =5: dsn: starting dsn
C                      dsni: increment between columns
C                      dsnj: increment between files
C     CLND   - begin and end months of the year or season
C              used only with OPT=2.
C              1 - January, ..., 12 - December
C              calendar year = 1, 12
C              water year = 10, 9
C              climatic year = 4, 3
C              summer = 6, 9
C     RTC    - return code from screen edit
C
C
C     + + + PARAMETERS + + +
      INTEGER   MAX
      PARAMETER ( MAX = 5 )
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SCLU, SGRP(MAX), OPCNT(MAX), OPLEN(MAX), DSLEN(MAX),
     $          CALEN(MAX), MXSEL(3,MAX), MNSEL(3,MAX), RTCMND, OPT,
     $          AGAIN
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZWNSOP
      EXTERNAL  Q1INIT, QSETOP, QSETI, QSETCO
      EXTERNAL  Q1EDIT, QGETOP, QGETI, QGETCO
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  SCLU / 103 /,
     $      SGRP /  10,     11,     12,     13,     14   /
     $     OPCNT /   2,      3,      3,      2,      2   /
     $     OPLEN /   2,      3,      3,      2,      2   /
     $     DSLEN /   2,      2,      2,      3,      3   /
     $     CALEN /   0,      2,      0,      0,      0   /
     $     MXSEL / 1,1,0,  1,1,1,  1,1,1,  1,1,0,  1,1,0 /
     $     MNSEL / 1,1,0,  1,1,1,  1,1,1,  1,1,0,  1,1,0 /
C
C     + + + END SPECIFICATIONS + + +
C
C     set version of option screen to be used
      OPT = OPTV
      IF (OPT .LT. 1  .OR.  OPT .GT. MAX) THEN
C       version is out of range, use default of simplest version
        OPT = 1
      END IF
C
 100  CONTINUE
        AGAIN = 0
C       set path name and initialize question
        CALL ZWNSOP ( LPTH, PTHNAM )
        CALL Q1INIT ( MESSFL, SCLU, SGRP(OPT) )
C       set options: dscfrm, dsstat[, dstype]
        CALL QSETOP ( OPCNT(OPT), OPLEN(OPT),
     I                MXSEL(1,OPT), MNSEL(1,OPT), OPTS )
C       set dsns: start, inc1[, inc2]
        CALL QSETI ( DSLEN(OPT), DSNS )
        IF (OPT .EQ. 2) THEN
C         set begin and end months for calendar or season
          CALL QSETCO ( CALEN(OPT), CLND )
        END IF
C
C       edit the screen
        CALL Q1EDIT ( RTCMND )
C
        IF (RTCMND .EQ. 1) THEN
C         user wants to accept and move on to next screen
C         get options
          CALL QGETOP ( OPLEN(OPT), OPTS )
C         get dsn and increment[s]
          CALL QGETI ( DSLEN(OPT), DSNS )
          IF (OPT .EQ. 2) THEN
C           get calendar or season
            CALL QGETCO ( CALEN(OPT), CLND )
          END IF
          RTC = 1
        ELSE IF (RTCMND .EQ. -1) THEN
C         oops, try again
          AGAIN = 1
        ELSE IF (RTCMND .EQ. 2) THEN
C         return to previous screen, no changes
          RTC = 2
        ELSE
C         unexpected command, no changes
          RTC = 2
        END IF
      IF (AGAIN .EQ. 1) GO TO 100
C
      RETURN
      END
