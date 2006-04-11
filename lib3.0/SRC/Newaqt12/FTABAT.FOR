C
C
C
      SUBROUTINE   COMPFB
     I                   (FIN1,FOUT,
     I                    MAXRCH,NUMRCH,TABNO,LENGTH,CUMDA)
C
C     + + + PURPOSE + + +
C     do computations to produce ftables, no interaction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      FIN1,FOUT,
     1             MAXRCH,NUMRCH,TABNO(MAXRCH)
      REAL         LENGTH(MAXRCH), CUMDA(MAXRCH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FIN1   - input file unit number
C     FOUT   - output file unit number, or record number if batch version
C     MAXRCH - max number of reaches
C     NUMRCH - actual number of reaches
C     TABNO  - array of ftable numbers
C     LENGTH - array of reach lengths
C     CUMDA  - array of upstream areas for each reach
C
C     + + + PARAMETERS + + +
      INTEGER MAXRAT, MAXROW
      PARAMETER (MAXRAT=100, MAXROW=25)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  NUMROW(MAXRAT), NUMRAT, I0
      REAL     AREA(MAXRAT), RAT(MAXRAT,MAXROW,4)
C
C     + + + EXTERNALS + + +
      EXTERNAL   GETRAT, WRFTAB
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C
C     read the file with the ratings
      CALL GETRAT (FIN1,MAXRAT,MAXROW,RAT,NUMRAT,AREA,NUMROW)
C     close file with ratings
      CLOSE (FIN1)
      IF (NUMRAT .GE. 0) THEN
C
        IF (NUMRCH .GT. 0) THEN
C         start processing
          CALL WRFTAB (MAXRCH,MAXRAT,MAXROW,CUMDA,AREA,NUMRAT,
     I                 I0,FOUT,TABNO,NUMROW,NUMRCH,LENGTH,RAT)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE GETRAT
     I                  (FIN1,MAXRAT,MAXROW,
     O                   RAT,NUMRAT,AREA,NUMROW)
C
C     + + + PURPOSE + + +
C     This routine reads rating tables for use in creating
C     FTABLEs for HSPF input.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MAXRAT, MAXROW, NUMRAT, FIN1, NUMROW(MAXRAT)
      REAL    RAT(MAXRAT,MAXROW,4), AREA(MAXRAT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FIN1   - Fortran unit number of input file
C     MAXRAT - maximum number of rating curves that can be used
C     MAXROW - maximum number of rows in a rating curve
C     RAT    - array of values in rating curve
C     NUMRAT - number of rating curves in the file
C     AREA   - drainage areas at the point in the stream for
C              the rating curve
C     NUMROW - number of rows in the rating curve
C
C     + + + LOCAL VARIABLES + +
      INTEGER I, J, K
      CHARACTER*80 LINE
C
C     + + + END SPECIFICATIONS + + +
C
C     zero out buffers
      DO 5 I = 1,MAXRAT
        NUMROW(I) = 0
        AREA(I)   = 0.0
        DO 4 J = 1,MAXROW
          DO 3 K = 1,4
            RAT(I,J,K) = 0.0
 3        CONTINUE
 4      CONTINUE
 5    CONTINUE
C
      I = 0
 10   CONTINUE
C
        READ(FIN1,FMT='(A)',ERR=91,END=99) LINE
C       write(*,*) line
C
        IF (LINE(1:1) .EQ. 'C' .OR. LINE(1:1) .EQ. 'c'
     $      .OR. LINE(1:1). EQ. '#') THEN
C         comment line, ignore
        ELSE IF (LINE(1:1) .EQ. 'R' .OR. LINE(1:1) .EQ. 'r') THEN
C         new rating, get drainage area
          I = I + 1
          READ(LINE,FMT='(1X,F9.0)') AREA(I)
          J = 1
        ELSE
C         read rating records
          IF (J .LE. MAXROW) THEN
C           use new record if array of MAXROW not reached
            READ(LINE,*,ERR=91) (RAT(I,J,K),K=1,4)
            IF (J .EQ. 1 .AND. RAT(I,J,4) .GT. 1E-9) THEN
              WRITE(99,*) 'WARNING: Discharge for first row should',
     $                   ' be zero for HSPF FTABLEs.'
            END IF
            NUMROW(I) = J
            J = J + 1
          END IF
        END IF
      IF (I .LT. MAXRAT) GO TO 10
C
      GO TO 99
 91   CONTINUE
C       WRITE(*,*) 'ERROR reading rating number',I
        IF (NUMROW(I) .LE. 1) I = I - 1
 99   CONTINUE
      NUMRAT = I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WRFTAB
     I                   (MAXRCH,MAXRAT,MAXROW,CUMDA,AREA,NUMRAT,
     I                    INTACT,FOUT,TABNO,NUMROW,NUMRCH,LENGTH,RAT)
C
C     + + + PURPOSE + + +
C     do computations to produce ftables
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MAXRCH,MAXRAT,MAXROW,INTACT,FOUT,
     1             NUMRCH,TABNO(MAXRCH),NUMRAT,NUMROW(MAXRAT)
      REAL         LENGTH(MAXRCH),CUMDA(MAXRCH),AREA(MAXRAT),
     1             RAT(MAXRAT,MAXROW,4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MAXRCH - max number of reaches
C     MAXRAT - max number of rating curves
C     MAXROW - max number of rows of curve
C     CUMDA  - array of upstream areas for each reach
C     AREA   - contributing area to each reach
C     NUMRAT - number of rating curves
C     INTACT - interactive version flag
C     FOUT   - output file unit number, or record number if batch version
C     TABNO  - array of ftable numbers
C     NUMROW - number of rows of curves
C     NUMRCH - actual number of reaches
C     LENGTH - array of reach lengths
C     RAT    - rating curves
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I, N, IRAT, NCOL, I0, KEY
      REAL          S, A, V, Q, ACHK, ADIFF, CONV
      CHARACTER*64  STR1, STR2, STR3
      CHARACTER*80  TXT
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   PUTUCI, GETUCI
C
C     + + + DATA INITIALIZATION + + +
      DATA STR1/' rows cols                                 ***'/
      DATA STR2/'     depth      area    volume   outflow   ***'/
      DATA STR3/'      (ft)   (acres) (acre-ft)   (ft3/s)   ***'/
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
C
      IF (INTACT.NE.1) THEN
        KEY = FOUT
      END IF
C
C     start processing
      N = 0
 20   CONTINUE
        N = N + 1
        I    = 1
        IRAT = 1
        ADIFF = ABS(CUMDA(N) - AREA(1))
        IF (NUMRAT .GT. 1) THEN
C         find appropriate rating (closest in drainage area)
C         to use for Nth reach
 30       CONTINUE
            I = I + 1
            ACHK =  ABS(CUMDA(N) - AREA(I))
            IF (ACHK .LT. ADIFF) THEN
              ADIFF = ACHK
              IRAT = I
            END IF
          IF (I .LT. NUMRAT) GO TO 30
        END IF
C
C       start processing to compute ftable
        NCOL = 4
        I    = 0
        IF (INTACT.EQ.1) THEN
          WRITE(FOUT,FMT='(A,I7)') '  FTABLE',TABNO(N)
          WRITE(FOUT,FMT='(A)') STR1
          WRITE(FOUT,FMT='(2I5)') NUMROW(IRAT),NCOL
          WRITE(FOUT,FMT='(A)') STR2
          WRITE(FOUT,FMT='(A)') STR3
        ELSE
          WRITE(TXT,FMT='(A,I7)') '  FTABLE',TABNO(N)
          CALL PUTUCI (TXT,I0,KEY)
          CALL GETUCI (I0,
     M                 KEY,
     O                 TXT)
          WRITE(TXT,FMT='(A)') STR1
          CALL PUTUCI (TXT,I0,KEY)
          CALL GETUCI (I0,
     M                 KEY,
     O                 TXT)
          WRITE(TXT,FMT='(2I5)') NUMROW(IRAT),NCOL
          CALL PUTUCI (TXT,I0,KEY)
          CALL GETUCI (I0,
     M                 KEY,
     O                 TXT)
          WRITE(TXT,FMT='(A)') STR2
          CALL PUTUCI (TXT,I0,KEY)
          CALL GETUCI (I0,
     M                 KEY,
     O                 TXT)
          WRITE(TXT,FMT='(A)') STR3
          CALL PUTUCI (TXT,I0,KEY)
          CALL GETUCI (I0,
     M                 KEY,
     O                 TXT)
        END IF
C       conversion ft-mi to acres and sqft-miles to area-feet
        CONV = 5280.0/43560.0
 40     CONTINUE
          I = I + 1
          S = RAT(IRAT,I,1)
          A = RAT(IRAT,I,2)*LENGTH(N)*CONV
          V = RAT(IRAT,I,3)*LENGTH(N)*CONV
          Q = RAT(IRAT,I,4)
          IF (INTACT.EQ.1) THEN
            WRITE(FOUT,FMT='(F10.2,F10.3,F10.3,F10.2)') S,A,V,Q
          ELSE
            WRITE(TXT,FMT='(F10.2,F10.3,F10.3,F10.2)') S,A,V,Q
            CALL PUTUCI (TXT,I0,KEY)
            CALL GETUCI (I0,
     M                   KEY,
     O                   TXT)
          END IF
        IF (I .LT. NUMROW(IRAT)) GO TO 40
        IF (INTACT.EQ.1) THEN
          WRITE(FOUT,FMT='(A,I3)') '  END FTABLE',TABNO(N)
        ELSE
          WRITE(TXT,FMT='(A,I3)') '  END FTABLE',TABNO(N)
          CALL PUTUCI (TXT,I0,KEY)
          CALL GETUCI (I0,
     M                 KEY,
     O                 TXT)
        END IF
      IF (N .LT. NUMRCH) GO TO 20
C
      RETURN
      END
