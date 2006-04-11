C
C
C
      SUBROUTINE   CVWTAT
     I                   ( MESSFL, PARMCD,
     O                     TSTYPE )
C
C     + + + PURPOSE + + +
C     Supply a tstype based on the parameter code
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, PARMCD
      CHARACTER*1 TSTYPE(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of the message file
C     PARMCD - Watstore parameter code
C     TSTYPE - 4-character descriptor based on PARMCD
C
C     + + + SAVES + + +
      INTEGER      FIRST
      CHARACTER*1  TYPE(4,100)
      SAVE         FIRST, TYPE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      SCLU, SGRP, LEN, I, INIT, INDX, CONT
      CHARACTER*1  BLNK, TXT(72)
      CHARACTER*11 TEMP
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPC, WMSGTT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  SCLU, BLNK, FIRST
     $     / 106,  ' ',     1 /
C
C     + + + INPUT FORMATS + + +
 1001 FORMAT ( I5 )
 1002 FORMAT ( 4A1 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ( 11A1 )
C
C     + + + END SPECIFICATIONS + + +
C
      IF (FIRST .EQ. 1) THEN
C       first time, blank out tstype array
        LEN = 400
        CALL ZIPC ( LEN, BLNK, TYPE )
C       get known tstypes
        SGRP = 2
        INIT = 1
 100    CONTINUE
C         get next line from the message file
          LEN = 11
          CALL WMSGTT ( MESSFL, SCLU, SGRP, INIT,
     M                  LEN,
     O                  TXT, CONT )
          WRITE (TEMP,2000) (TXT(I), I = 1, 11)
          INIT = 0
          READ (TEMP(7:11),1001) INDX
          IF (0 .LT. INDX  .AND.  INDX .LT. 101) THEN
C           within expected parameter range
            READ (TEMP(1:4),1002) (TYPE(I,INDX), I = 1, 4)
          END IF
        IF (CONT .EQ. 1) GO TO 100
        FIRST = 2
      END IF
C
C     determine tstype that corresponds to parameter code
      IF (0 .LT. PARMCD  .AND.  PARMCD .LE. 100) THEN
C       may know this parameter
        TSTYPE(1) = TYPE(1,PARMCD)
        TSTYPE(2) = TYPE(2,PARMCD)
        TSTYPE(3) = TYPE(3,PARMCD)
        TSTYPE(4) = TYPE(4,PARMCD)
      ELSE
C       do not know this parameter
        LEN = 4
        CALL ZIPC ( LEN, BLNK, TSTYPE )
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   INHRMN
     I                   ( DELT,
     M                     HR, MN )
C
C     + + + PURPOSE + + +
C     This routine converts the hour and minute from the end
C     of a timestep to hour and minute at the beginning of the
C     timestep.  Generally used with WATSTORE unit values card
C     image input.  If both HR and MN are zero, they will not be
C     changed.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DELT, HR, MN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT   - time step in minutes
C     HR     - input as hour at end of timestep
C              output as hour at start of timestep
C     MN     - input as minute at end of timestep
C              output as minute at start of timestep
C
C     + + + LOCAL VARIABLES
      INTEGER   TIME
C
C     + + + END SPECIFICATIONS + + +
C
      IF (HR .NE. 0  .OR.  MN .NE. 0) THEN
        TIME = HR * 60 + MN - DELT
        HR = TIME / 60
        MN = TIME - HR * 60
      END IF
C
      RETURN
      END
      SUBROUTINE   IOCLND
     I                    ( CALND, CLNDYR, MNTH,
     O                      YEAR )
C
C     + + + PURPOSE + + +
C     Given a calendar year and month returns the water, climate,
C     calendar, or other year, as appropriate for the beginning and
C     ending months of the year or season.  The year returned is the
C     year at the end of the season.  For example, if CALND(1) = 10
C     (October) and CALND(2) = 9 (September) for a water year, for
C     CLNDYR = 1989 and MNTH = 11 (November), the subroutine returns
C     the value 1990 for YEAR.
C     For calendar year: CALND(1) = 1  and CALND(2) = 12
C     For water year:    CALND(1) = 10 and CALND(2) = 9
C     For climatic year: CALND(1) = 4  and CALND(2) = 3
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CALND(2), CLNDYR, MNTH, YEAR
C
C     + + + ARUGMENT DEFINITIONS + + +
C     CALND  - calender months for year or season
C              (1) - beginning month (1-Jan,...12-Dec)
C              (2) - ending month (1-Jan,...12-Dec)
C     CLNDYR - calendar year
C     MNTH   - calendar month, if zero, YEAR = CLNDYR
C     YEAR   - climatic or seasonal year containing CLNDYR, MNTH
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize year to invalid
      YEAR = 0
      IF (1 .LE. CALND(1)  .AND.  CALND(1) .LE. 12  .AND.
     $    1 .LE. CALND(2)  .AND.  CALND(2) .LE. 12  .AND.
     $    0 .LT. CLNDYR    .AND.    CLNDYR .LE. 2100) THEN
C       valid months for calendar and valid year for date
        IF (1 .LE. MNTH  .AND.  MNTH .LE. 12) THEN
C         valid month
          IF (CALND(1) .LE. CALND(2)) THEN
C           calendar year or partial year within calendar year
            IF (CALND(1) .LE. MNTH  .AND.  MNTH .LE. CALND(2)) THEN
C             month is within year or part of year
              YEAR = CLNDYR
            END IF
          ELSE IF (MNTH .LT. CALND(1)  .OR.  MNTH .GT. CALND(2)) THEN
C           year or season that crosses the Dec/Jan boundry
            IF (MNTH .GT. CALND(2)) THEN
C             adjust calendar year for season or year
              YEAR = CLNDYR + 1
            ELSE
C             no adjustment needed for year
              YEAR = CLNDYR
            END IF
          END IF
        ELSE IF (MNTH .EQ. 0) THEN
C         no month, go with calendar year
          YEAR = CLNDYR
        END IF
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   PKQLTY
     I                            ( LENQ, QFLAGS )
C
C     + + + PURPOSE + + +
C     Given Watstore peak flow qualification codes, returns a
C     single wdm time series quality code.
C
C     + + + DUMMY ARUGMENTS + + +
      INTEGER   LENQ
      CHARACTER*1 QFLAGS(LENQ)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LENQ   - number of qualificaton codes that may be present
C     QFLAGS - array of watstore peak flow qualification codes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NSTR, BREAK, MISS, OFF, LEN, LEN1, QUAL, QMAX, QREL,
     $          I
      CHARACTER*1 QF(14)
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR, CHKSTR
C
C     + + + EXTERNAL + + +
      EXTERNAL  LENSTR, CHKSTR
C
C     + + + DATA INITIALZATIONS + + +
      DATA  LEN1, NSTR, BREAK, MISS, OFF
     $     /   1,   14,     9,   15,  10 /
      DATA  QF
     $     / '1', '2', '7', 'A', 'B', 'D', 'E', '9', 'C',
     $       '3', '5', '6', '4', '8' /
C
C     + + + END SPECIFICATIONS + + +
C
C     determine actual number of qualification codes
      LEN = LENSTR ( LENQ, QFLAGS )
      IF (LEN .EQ. 0) THEN
C       no qualification codes, assume good
        QUAL = 0
      ELSE
C       qualification code(s) present, check each one
        QMAX = 0
        DO 100 I = 1, LEN
C         get relative severity for qualification code
          QREL = CHKSTR ( LEN1, NSTR, QFLAGS(I), QF )
          IF (QREL .GT. QMAX) THEN
C           this qualification code more serious than previous
            QMAX = QREL
          END IF
 100    CONTINUE
C       determine quality code
        IF (QMAX .EQ. 0) THEN
C         found an unknown qualification code
          QUAL = MISS
        ELSE IF (QMAX .GT. BREAK) THEN
C         increase quality code for unacceptable qualification code
          QUAL = QMAX + OFF
        ELSE
C         acceptable quality code
          QUAL = QMAX
        END IF
      END IF
C
      PKQLTY = QUAL
C
      RETURN
      END
C
C
C
      SUBROUTINE   INWSTA
     I                   ( STA,
     O                     STAC, STAI )
C
C     + + + PURPOSE + + +
C     From a character*15 variable containing a station number,
C     determine the character array station id and, if possible,
C     the integer station id.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       STAI
      CHARACTER*1   STAC(16)
      CHARACTER*15  STA
C
C     + + + ARGUMENT DEFINITION + + +
C     STA    - station id as a character variable
C     STAC   - station id as a character array, 16th positon is
C              set to blank
C     STAI   - integer version of station id
C              0 - station id contains non-numeric characters or
C                  is larger than can be represented as an integer
C              non-zero - station id can be represented as an integer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LEN, LENC, I, BAD
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR, CRINTE
C
C     + + + EXTERNALS + + +
      EXTERNAL  LENSTR, CRINTE, LFTSTR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BAD, LEN / -999, 16 /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 15A1 )
C
C     + + + END SPECIFICATIONS + + +
C
C     fill character station id
      READ (STA,1000) ( STAC(I), I = 1, 15 )
      STAC(16) = ' '
C
C     left justify station
      CALL LFTSTR ( LEN, STAC )
C
C     set integer station id
      LENC = LENSTR ( LEN, STAC )
      IF (LENC .GT. 0) THEN
        STAI = CRINTE ( BAD, LENC, STAC )
      ELSE
        STAI = BAD
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   NDYTST
     I                   ( NDYS,
     O                     TSTYPE )
C
C     + + + PURPOSE + + +
C     Convert a 5-character n-day designation to a 4-character
C     TSTYPE.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*1  TSTYPE(4)
      CHARACTER*5  NDYS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDYS   - n-day identification
C              1:2 - high or low indication
C                    " 2" - low flow
C                    " 3" - high flow
C                    others - unknown
C              3:5 - number of days for flow
C     TSTYPE - type of data
C              1:1 - "L" if NDYS(1:2) is " 2"
C                  - "H" if NDYS(1:2) is " 3"
C                    "U" for all other values
C              2:4 - number of days for flow
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NDYS(1:2) .EQ. ' 2') THEN
C       low flow n-day
        TSTYPE(1) = 'L'
      ELSE IF (NDYS(1:2) .EQ. ' 3') THEN
C       high flow n-day
        TSTYPE(1) = 'H'
      ELSE
C       unknown code
        TSTYPE(1) = 'U'
      END IF
      TSTYPE(2) = NDYS(3:3)
      TSTYPE(3) = NDYS(4:4)
      TSTYPE(4) = NDYS(5:5)
C
      RETURN
      END
