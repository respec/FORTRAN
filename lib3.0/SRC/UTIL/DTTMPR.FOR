C
C
C
      SUBROUTINE   SYDATM
     O                   ( YR, MO, DY, HR, MN, SC )
C
C     + + + PURPOSE + + +
C     Returns the current date and time.  Calls the system dependent
C     subroutines SYDATE for the date and SYTIME for the time.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DY, HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS
C     YR     - year
C     MO     - month
C     DA     - day
C     HR     - hour
C     MN     - minute
C     SC     - second
C
C     + + + EXTERNALS + + +
      EXTERNAL   SYDATE, SYTIME
C
C     + + + END SPECIFICATIONS + + +
C
C     get date
      CALL SYDATE ( YR, MO, DY )
C
C     get time
      CALL SYTIME ( HR, MN, SC )
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYDATE
     O                   ( YR, MO, DA )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system date.
C     This version of SYDATE calls the Prime system routine TIMDAT.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year
C     MO     - month
C     DA     - day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2    STRING(3), NUM
      CHARACTER*6  LIDO
C
C     + + + LOCAL DEFINITIONS + + +
C     STRING - array containing date and time
C              (1) - current month
C              (2) - current day
C              (3) - current year
C              (4) - minutes since midnight
C              (5) - seconds passed after minute
C              (6) - ticks passed after second
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIMDAT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NUM / 3 /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 3A2 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ( 3I2 )
C
C     + + + END SPECIFICATIONS + + +
C
      CALL TIMDAT ( STRING, NUM )
      WRITE (LIDO,1000) STRING(1), STRING(2), STRING(3)
      READ (LIDO,2000) MO, DA, YR
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYTIME
     O                   ( HR, MN, SC )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system time.
C     This version of SYTIME calls the Prime system routine TIMDAT.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HR     - Number of hours since midnight
C     MN     - Number of minutes since hour
C     SC     - Number of seconds since minute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2 STRING(6), NUM
C
C     + + + LOCAL DEFINITIONS + + +
C     STRING - array containing date and time
C              (1) - current month
C              (2) - current day
C              (3) - current year
C              (4) - minutes since midnight
C              (5) - seconds passed after minute
C              (6) - ticks passed after second
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIMDAT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  NUM / 6 /
C
C     + + + END SPECIFICATIONS + + +
C
      CALL TIMDAT ( STRING, NUM )
      MN = STRING(4)
      HR = MN / 60
      MN = MN  -  HR * 60
      SC = STRING(5)
C
      RETURN
      END
