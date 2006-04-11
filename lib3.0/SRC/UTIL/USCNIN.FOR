C
C
C
      SUBROUTINE   SCCLAL
C
C     + + + PURPOSE + + +
C     homes the cursor and clears the screen - Interactor version
C
C     + + + EXTERNALS + + +
      EXTERNAL IWinClear
C
C     + + + END SPECIFICATIONS + + +
C
      !CALL IClearScreen
      CALL IWinClear
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCLR
C
C     + + + PURPOSE + + +
C     clears the screen
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I3
      CHARACTER*1 CLS(3)
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    SCPRST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I3/3/
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE(99,*) 'SCCLR:can we skip?'
C     ESC
      CLS(1)= CHAR(27)
C     [
      CLS(2)= CHAR(91)
C     J
      CLS(3)= CHAR(74)
C
      CALL SCPRST (I3,CLS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCUMV
     I                    (CROW,CCOL)
C
C     + + + PURPOSE + + +
C     moves cursor to the absolute row and column specified - Interactor
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CROW,CCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CROW   - row to move cursor to
C     CCOL   - column to move cursor to
C
C     + + + EXTERNALS + + +
      EXTERNAL    ICursorXY
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ICursorXY(CCOL,CROW)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCURM
     I                    (CROW,CCOL)
C
C     + + + PURPOSE + + +
C     moves the cursor to the relative row and column specified - Interactor
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  CROW,CCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CROW   - number of rows to move
C     CCOL   - number of columns to move
C
C     + + + EXTERNALS + + +
      EXTERNAL   ICursorRight, ICursorLeft
C
C     + + + END SPECIFICATIONS + + +
C
      !WRITE(99,*) 'SCCURM:',CROW,CCOL
      IF (CCOL > 0) THEN
        CALL ICursorRight(CCOL)
      ELSE IF (CCOL < 0) THEN
        CALL ICursorLeft(-CCOL)
      END IF
C
      IF (CROW > 0) THEN
        CALL ICursorRight(CROW*80)    ! assumes window 80 chars wide
      ELSE IF (CROW < 0) THEN
        CALL ICursorLeft(-CCOL*80)    ! assumes window 80 chars wide
      END IF
C
      RETURN
      END
