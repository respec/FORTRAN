C
C
C
      SUBROUTINE   UCIWRT
     I                   (OUTFL,WRTFG)
C
C     + + + PURPOSE + + +
C     Write out a UCI file to disk.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OUTFL,WRTFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUTFL  - Fortran unit number for output UCI file
C     WRTFG  - write messages to screen flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J
      CHARACTER*78  TBUFF
C
C     + + + EXTERNALS + + +
      EXTERNAL     QFCLOS,ZWRSCR,UCISAV
C
C     + + + END SPECIFICATIONS + + +
C
      IF (WRTFG.EQ.1) THEN
C       outputting UCI file
        TBUFF= ' '
        J= 2
        DO 5 I= 2,6
          CALL ZWRSCR (TBUFF,I,J)
 5      CONTINUE
        TBUFF= 'Writing UCI file to disk'
        I= 6
        J= 26
        CALL ZWRSCR (TBUFF(1:30),I,J)
      END IF
C
      CALL UCISAV (OUTFL)
C
C     close UCI file
      IF (WRTFG.EQ.1) THEN
        I= 0
        CALL QFCLOS (OUTFL,I)
      ELSE
        CLOSE(UNIT=OUTFL)
      END IF
C
      RETURN
      END
