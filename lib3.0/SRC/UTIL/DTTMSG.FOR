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
C     This version of SYDATE calls the Silicon Graphics system
C     routine FDATE.
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
      CHARACTER*24 DATBUF
C
C     + + + FUNCTIONS + + +
      CHARACTER*24 FDATE
C
C     + + + EXTERNALS + + +
      EXTERNAL  FDATE
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 5X, I2, 1X, I2, 12X, I2 )
C
C     + + + END SPECIFICATIONS + + +
C
      DATBUF = FDATE ( )
      READ (DATBUF,1000) MO, YR, DA
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
C     This version of SYTIME calls the Silicon Graphics system
C     routine FDATE.
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
      CHARACTER*24 DATBUF
C
C     + + + FUNCTIONS + + +
      CHARACTER*24 FDATE
C
C     + + + EXTERNALS + + +
      EXTERNAL  FDATE
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 11X, I2, 1X, I2, 1X, I2 )
C
C     + + + END SPECIFICATIONS + + +
C
      DATBUF = FDATE ( )
      READ (DATBUF,1000) HR, MN, SC
C
      RETURN
      END
