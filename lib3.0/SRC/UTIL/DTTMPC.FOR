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
C     This version of SYDATE calls the Lahey Fortran system 
C     routine DATE.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year
C     MO     - month
C     DA     - day
C
C      + + + LOCAL VARIABLES + + +
       CHARACTER*8 CDATE
C 
C     + + + EXTERNALS + + +
      EXTERNAL   DATE
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 2 ( I2, 1X ), I2 )
C
C     + + + END SPECIFICATIONS + + +
C
      CALL DATE ( CDATE )
      READ (CDATE,1000) MO, DA, YR
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYTIME
     O                  ( HR, MN, SC )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system time.
C     This version of SYTIME calls the Lahey Fortran system
C     routine TIME.
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
      CHARACTER*11 CTIME
C
C     + + + EXTERNALS + + +
      EXTERNAL  TIME
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 3 ( I2, 1X ) )
C
C     + + + END SPECIFICATIONS + + +
C
      CALL TIME ( CTIME )
      READ (CTIME,1000) HR, MN, SC
C
      RETURN
      END
