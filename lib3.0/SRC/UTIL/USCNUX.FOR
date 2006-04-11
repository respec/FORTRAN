C
C
C
      SUBROUTINE   SCCLAL
C
C     + + + PURPOSE + + +
C     homes the cursor and clears the screen
C     *** UNIX VERSION, USES CURSES ***
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,L
C
C     + + + FUNCTIONS + + +
      INTEGER   CRSMOV,CRSCLR
C
C     + + + EXTERNALS + + +
      EXTERNAL  CRSMOV,CRSCLR
C
      I= 0
      J= 0
      L= CRSMOV(I,J)
      L= CRSCLR()
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
C     *** UNIX VERSION, USES CURSES ***
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I
C
C     + + + FUNCTIONS + + +
      INTEGER  CRSCLR
C
C     + + + EXTERNALS + + +
      EXTERNAL CRSCLR
C
      I= CRSCLR()
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
C     moves cursor to the absolute row and column specified
C     *** UNIX VERSION, USES CURSES ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CROW,CCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CROW   - row to move cursor to
C     CCOL   - column to move cursor to
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  I,J,L
C
C     + + + FUNCTIONS + + +
      INTEGER  CRSMOV
C
C     + + + EXTERNALS + + +
      EXTERNAL CRSMOV
C
C     + + + END SPECIFICATIONS + + +
C
      I= CROW- 1
      J= CCOL- 1
      L= CRSMOV(I,J)
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
C     moves the cursor to the relative row and column specified
C     *** UNIX VERSION, USES CURSES ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  CROW,CCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CROW   - number of rows to move
C     CCOL   - number of columns to move
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,L
C
C     + + + FUNCTIONS  + + +
      INTEGER     CRSGET,CRSMOV
C
C     + + + EXTERNALS + + +
      EXTERNAL    CRSGET,CRSMOV
C
C     + + + END SPECIFICATIONS + + +
C
      L= CRSGET(I,J)
      I= I+ CROW
      J= J+ CCOL
      L= CRSMOV(I,J)
C
      RETURN
      END
