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
C     This version of SYDATE calls the DG system routine IDATE.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year
C     MO     - month
C     DA     - day
C
C     + + + EXTERNALS + + +
      EXTERNAL IDATE
C
C     + + + END SPECIFICATIONS + + +
C
      CALL IDATE ( MO, DA, YR )
C     change for y2k, return only 2 digits of year
      YR = MOD( YR, 100 )
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
C     This version of SYTIME calls the DG system routine SECNDS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HR     - Number of hours since midnight
C     MN     - Number of minutes since hour
C     SC     - Number of seconds since minute
C
C     + + + FUNCTIONS + + +
      REAL      SECNDS
C
C     + + + INTRINSICS + + +
C     INTRINSIC INT
C
C     + + + EXTERNALS + + +
      EXTERNAL  SECNDS
C
C     + + + END SPECIFICATIONS + + +
C
C     SECNDS returns the difference, in seconds, between the current
C     system time and the user supplied time.  Supplying a value of
C     zero (midnight) causes SECNDS to return the current system time.
C
      SC = INT ( SECNDS(0.0) + 0.005 )
      HR = SC / 3600
      MN = (SC  -  HR * 3600)  /  60
      SC = SC  -  HR * 3600  -  MN * 60
C
      RETURN
      END
