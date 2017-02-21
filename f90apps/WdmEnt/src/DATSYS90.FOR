C
C
C
      SUBROUTINE   WDSYSD
     O                   (IDATE)
C
C     + + + PURPOSE + + +
C     Fetch system date and time for DSN creation/modification attributes.
C     *** FORTRAN 90 ONLY ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IDATE(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDATE  - integer array containing character representation of date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   VALUES(8)
      CHARACTER*10 DATE,TIME,ZONE
C
C     + + + INTRINSICS + + +
      INTRINSIC DATE_AND_TIME
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (2A4)
 1010 FORMAT (A4,A2,2X)
C
C     + + + END SPECIFICATIONS + + +
C
      CALL DATE_AND_TIME (DATE,TIME,ZONE,VALUES)
      READ (DATE(1:8),1000) IDATE(1),IDATE(2)
      READ (TIME(1:6),1010) IDATE(3),IDATE(4)
C
      RETURN
      END
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
C     DY     - day
C     HR     - hour
C     MN     - minute
C     SC     - second
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   VALUES(8)
      CHARACTER*10 DATE,TIME,ZONE
C
C     + + + INTRINSICS + + +
      INTRINSIC DATE_AND_TIME
C
C     + + + END SPECIFICATIONS + + +
C
      CALL DATE_AND_TIME (DATE,TIME,ZONE,VALUES)
      YR = VALUES(1)
      YR = MOD ( YR, 100 )
      MO = VALUES(2)
      DY = VALUES(3)
      HR = VALUES(5)
      MN = VALUES(6)
      SC = VALUES(7)
C
      RETURN
      END
