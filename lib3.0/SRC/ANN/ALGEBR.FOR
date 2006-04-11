C
C
C
      SUBROUTINE   ALGEBR
     I                    (MESSFL, OPT, NVAL, TBLSIZ, C1, C2, K1,
     I                     BUFIN1, BUFIN2,
     M                     SUM, KOUNT,
     O                     BUFOUT)
C
C     + + + PURPOSE + + +
C     This routine performs math transformations on the time series
C     in the CMATH common block.
C
C     + + + KEYWORDS + + +
C     message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, OPT, NVAL, TBLSIZ, K1, KOUNT
      REAL      C1(TBLSIZ), C2(TBLSIZ), BUFIN1(NVAL), BUFIN2(NVAL),
     #          SUM, BUFOUT(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     OPT    - algebra option
C               1 - DONE    2 - +C      3 - *C
C               4 - ADD     5 - SUB     6 - MULT
C               7 - DIV     8 - MEAN    9 - WGHT
C              10 - **C    11 - C**    12 - POW
C              13 - LGE    14 - LG10   15 - LGE
C              16 - ABS    17 - Z-     18 - Z+
C              19 - MIN    20 - MAX    21 - SUM
C              22 - SIGF   23 - LINE   24 - TABLE
C     NVAL   - array size of input and output buffers
C     TBLSIZ - array size of real coefficients
C     K1     - integer coefficient, required for
C              OPT = 21, optional for OPT = 10
C     C1     - real coefficient, required for
C              OPT = 2, 3, 9, 11, 17, 18 and 23,
C              optional for OPT = 10
C     C2     - real coefficient, required for
C              OPT = 17, 18, and 23
C     BUFIN1 - first input array
C     BUFIN2 - second input array, optional
C     SUM    - cumulative sum of array BUFIN1,
C              for OPT = 21
C     KOUNT  - count of invalid algebra
C              calculations
C     BUFOUT - output array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   N, SCLU, SGRP, L, LOC
      REAL      RANGEE, BAD, ZERO, B, BLOG, SIGN
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS, ALOG, ALOG10, AMAX1, AMIN1, EXP, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL   ERMATH
C
C     + + + DATA INITIALIZATIONS + ++
      DATA RANGEE / 80.5 /
      DATA BAD    / 10.0E20 /
      DATA ZERO   / 0.1E-20 /
C
C     + + + END SPECIFICATIONS + + +
C
C
      SCLU = 129
      SGRP = 1
C
      GO TO ( 990, 20, 30, 40, 40, 60, 70, 80, 90, 100, 110, 120,
     #        130, 140, 150, 160, 170, 180, 190, 200, 210, 220,
     #        230, 240) OPT
C
 20   CONTINUE
C       add constant
        DO 22 N = 1, NVAL
          BUFOUT(N) = BUFIN1(N) + C1(1)
 22     CONTINUE
        GO TO 990
C
 30   CONTINUE
C       multiply by a constant
        DO 32 N = 1, NVAL
          BUFOUT(N) = C1(1) * BUFIN1(N)
 32     CONTINUE
        GO TO 990
C
 40   CONTINUE
C       add two time series
        IF(OPT .EQ. 4) THEN
          C1(1) = 1.0
        ELSE
          C1(1) = -1.0
        ENDIF
        DO 42 N = 1, NVAL
          BUFOUT(N) = BUFIN1(N) + C1(1) * BUFIN2(N)
 42     CONTINUE
        GO TO 990
C
 60   CONTINUE
C       multiply two time series
        DO 62 N = 1, NVAL
          BUFOUT(N) = BUFIN1(N) * BUFIN2(N)
 62     CONTINUE
        GO TO 990
C
 70   CONTINUE
C       divide a time series by a time series
        DO 72 N = 1, NVAL
          IF (ABS(BUFIN2(N)) .GE. ZERO) THEN
            BUFOUT(N) = BUFIN1(N) / BUFIN2(N)
          ELSE
            KOUNT = KOUNT + 1
            IF (KOUNT .LT. 10) CALL ERMATH (MESSFL, SCLU, SGRP, OPT)
            BUFOUT(N) = BAD
          ENDIF
 72     CONTINUE
        GO TO 990
C
 80   CONTINUE
C       finds the mean time series of two time series
        DO 82 N= 1, NVAL
          BUFOUT(N) = ( BUFIN1(N) + BUFIN2(N) ) / 2.
 82     CONTINUE
        GO TO 990
C
 90   CONTINUE
C       finds the weighted average of two time series
        DO 92 N = 1, NVAL
          BUFOUT(N) = C1(1) * BUFIN1(N) + C2(1) * BUFIN2(N)
 92     CONTINUE
        GO TO 990
C
 100  CONTINUE
C       raises a time series to a constant power
        IF(K1 .NE. 0) THEN
          DO 102 N = 1, NVAL
            IF (ABS(BUFIN1(N)) .LT. ZERO  .AND.  K1 .LE. 0) THEN
              KOUNT = KOUNT + 1
              IF (KOUNT .LT. 10) CALL ERMATH( MESSFL, SCLU, SGRP, OPT )
              BUFOUT(N) = BAD
            ELSE
              BUFOUT(N) = BUFIN1(N) ** K1
            ENDIF
 102      CONTINUE
        ELSE
          DO 104 N = 1,NVAL
            IF (BUFIN1(N) .LT. -ZERO) THEN
              KOUNT = KOUNT + 1
              IF (KOUNT .LT. 10) CALL ERMATH( MESSFL, SCLU, SGRP, OPT )
              BUFOUT(N) = BAD
            ELSE IF (BUFIN1(N) .LT. ZERO) THEN
              BUFOUT(N) = 0.0
            ELSE
              BUFOUT(N) = BUFIN1(N) ** C1(1)
            ENDIF
 104      CONTINUE
        ENDIF
        GO TO 990
C
 110  CONTINUE
C       raises a constant to the power of the time series
        DO 112 N = 1, NVAL
          BUFOUT(N) = C1(1) ** BUFIN1(N)
 112    CONTINUE
        GO TO 990
C
 120  CONTINUE
C       raises a time series to the power of a time series
        DO 122 N = 1, NVAL
          IF (ABS(BUFIN1(N)) .LT. ZERO  .AND.  BUFIN2(N) .LE. ZERO) THEN
            KOUNT = KOUNT + 1
            IF (KOUNT .LT. 10) CALL ERMATH( MESSFL, SCLU, SGRP, OPT )
            BUFOUT(N) = 0.0
          ELSE
            BUFOUT(N) = BUFIN1(N) ** BUFIN2(N)
          ENDIF
 122    CONTINUE
        GO TO 990
C
 130  CONTINUE
C       raises e to the power of the time series
        DO 132 N = 1, NVAL
          IF (BUFIN1(N) .LE. RANGEE) THEN
            BUFOUT(N) = EXP( BUFIN1(N) )
          ELSE
            KOUNT = KOUNT + 1
            IF (KOUNT .LT. 10) CALL ERMATH( MESSFL, SCLU, SGRP, OPT )
            BUFOUT(N) = BAD
          ENDIF
 132    CONTINUE
        GO TO 990
C
 140  CONTINUE
C       log base 10 of a time series
        DO 142 N = 1, NVAL
          IF (BUFIN1(N) .GT. 0.0) THEN
            BUFOUT(N) = ALOG10( BUFIN1(N) )
          ELSE
            KOUNT = KOUNT + 1
            IF (KOUNT .LT. 10) CALL ERMATH( MESSFL, SCLU, SGRP, OPT )
            BUFOUT(N) = BAD
          ENDIF
 142    CONTINUE
        GO TO 990
C
 150  CONTINUE
C       log base e of a time series
        DO 152 N = 1, NVAL
          IF (BUFIN1(N) .GT. 0.0) THEN
            BUFOUT(N) = ALOG( BUFIN1(N) )
          ELSE
            KOUNT = KOUNT + 1
            IF (KOUNT .LT. 10) CALL ERMATH( MESSFL, SCLU, SGRP, OPT )
            BUFOUT(N) = BAD
          ENDIF
 152    CONTINUE
        GO TO 990
C
 160  CONTINUE
C       absolute value
        DO 162 N = 1, NVAL
          BUFOUT(N) = ABS( BUFIN1(N) )
 162    CONTINUE
        GO TO 990
C
 170  CONTINUE
C       replaces values in a timeseries below C1(1) with C2(1)
        DO 172 N = 1, NVAL
          IF(BUFIN1(N) .LT. C1(1)) THEN
            BUFOUT(N) = C2(1)
          ELSE
            BUFOUT(N) = BUFIN1(N)
          ENDIF
 172    CONTINUE
        GO TO 990
C
 180  CONTINUE
C       replaces values in a timeseries above C1(1) with C2(1)
        DO 182 N = 1, NVAL
          IF(BUFIN1(N) .GT. C1(1)) THEN
            BUFOUT(N) = C2(1)
          ELSE
            BUFOUT(N) = BUFIN1(N)
          ENDIF
 182    CONTINUE
        GO TO 990
C
 190  CONTINUE
C       finds the minimum time series of two time series
        DO 192 N = 1, NVAL
          BUFOUT(N) = AMIN1( BUFIN1(N), BUFIN2(N) )
 192    CONTINUE
        GO TO 990
C
 200  CONTINUE
C       finds the maximum time series of two time series
        DO 202 N = 1, NVAL
          BUFOUT(N) = AMAX1( BUFIN1(N), BUFIN2(N) )
 202    CONTINUE
        GO TO 990
C
 210  CONTINUE
C       cumulates a time series
        DO 212 N = 1, NVAL
           SUM = SUM + BUFIN1(N)
           BUFOUT(N) = SUM
 212    CONTINUE
        GO TO 990
 220  CONTINUE
C       rounds off to K1 significant figures
        DO 222 N = 1, NVAL
          IF (ABS(BUFIN1(N)) .GT. ZERO) THEN
            IF (BUFIN1(N) .LT. 0.0) THEN
              B = ABS(BUFIN1(N))
              SIGN = -1.
            ELSE
              B = BUFIN1(N)
              SIGN = 1.
            ENDIF
            BLOG = ALOG10(B) + .001
            IF (BLOG .LT. 0.0) BLOG = BLOG - 1.
            L = INT(BLOG) - K1 + 1
            B = ( B / 10.0 ** L )  +  0.5
            BUFOUT(N) = INT(B) * 10.0 ** L * SIGN
          ELSE
            BUFOUT(N) = BUFIN1(N)
          ENDIF
 222    CONTINUE
        GO TO 990
C
 230  CONTINUE
C       straight line
        DO 232 N = 1, NVAL
          BUFOUT(N) = C1(1) * BUFIN1(N) + C2(1)
 232    CONTINUE
        GO TO 990
C
 240  CONTINUE
C       use table
        LOC = 1
        DO 248 N = 1,NVAL
          IF (BUFIN1(N) .LT. C1(1)) THEN
            BUFOUT(N) = -1.0E20
          ELSE IF (BUFIN1(N) .GT. C1(TBLSIZ)) THEN
            BUFOUT(N) = 1.0E20
          ELSE
            IF (C1(LOC) .LE. BUFIN1(N)) THEN
C             go up table
 243          CONTINUE
                LOC = LOC + 1
              IF (C1(LOC) .LE. BUFIN1(N)) GO TO 243
              LOC  = LOC - 1
            ELSE
C             go down table
 245          CONTINUE
                LOC = LOC - 1
              IF (C1(LOC) .GT. BUFIN1(N)) GO TO 245
            END IF
            BUFOUT(N) = C2(LOC) + (C2(LOC+1)-C2(LOC))*
     #                  (BUFIN1(N) - C1(LOC))/
     #                  (C1(LOC+1)-C1(LOC))
          END IF
 248    CONTINUE
        GO TO 990
C
 990  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ERMATH
     I                    (MESSFL, SCLU, SGRP0, ERR)
C
C     + + + PURPOSE + + +
C     This routine writes out error messages for the PGENER group.
C
C     + + + KEYWORDS + + +
C     message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, SCLU, SGRP0, ERR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     SCLU  - message file group number
C     SGRP0  - question number
C     ERR    - error number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SGRP
C
C     + + + EXTERNALS + + +
C     EXTERNAL   PRNTXT
C
C     + + + END SPECIFICATIONS + + +
C
C
      GO TO ( 10, 10, 10, 10, 10, 10, 20, 10, 10, 30, 10, 40, 50, 60,
     #        70, 10 ) ERR
C
 10     CONTINUE
          SGRP = SGRP0
          GO TO 100
C
 20     CONTINUE
          SGRP = SGRP0 + 1
          GO TO 100
C
 30     CONTINUE
          SGRP = SGRP0 + 2
          GO TO 100
C
 40     CONTINUE
          SGRP = SGRP0 + 3
          GO TO 100
C
 50     CONTINUE
          SGRP = SGRP0 + 4
          GO TO 100
C
 60     CONTINUE
          SGRP = SGRP0 + 5
          GO TO 100
C
 70     CONTINUE
          SGRP = SGRP0 + 6
          GO TO 100
C
 100  CONTINUE
C
C     CALL PRNTXT ( MESSFL, SCLU, SGRP )
c
      RETURN
      END
