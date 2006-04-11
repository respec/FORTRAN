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
C     This version of SYDATE calls the standard SUN OS system
C     subroutine IDATE.  Note that the SUN VMS version of
C     IDATE is call idate ( m, d, y )
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
      INTEGER   STRING(3)
C
C     + + + EXTERNALS + + +
      EXTERNAL  IDATE
C
C     + + + END SPECIFICATIONS + + +
C
      CALL IDATE ( STRING )
      YR = MOD( STRING(3), 100 )
      MO = STRING(2)
      DA = STRING(1)
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
C     This version of SYTIME calls the standard SUN OS system
C     subroutine  ITIME.
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
      INTEGER   STRING(3)
C
C     + + + LOCAL DEFINITIONS + + +
C     STRING - current time
C              (1) hour since midnight
C              (2) minute since hour
C              (3) second since minute
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITIME
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ITIME ( STRING )
C     broken out to avoid I*2 overflow possibility
      HR = STRING(1)
      MN = STRING(2)
      SC = STRING(3)
C
      RETURN
      END
