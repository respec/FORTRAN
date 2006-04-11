C
C
C
      SUBROUTINE   ZWRVDR
     I                    (STRING,LINE,COLUMN)
C
C     + + + PURPOSE + + +
C     To write a string to a given screen position in the video
C     reverse mode.
C
C     *** System specific for PC, calls to COLSET
C     *** only valid on PC
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LINE,COLUMN
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - character string to write
C     LINE   - starting line number to write
C     COLUMN - starting column number to write
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C     COMMON AREAS: zcntrl.inc contains control parameters
C
C     + + + EXTERNALS + + +
      EXTERNAL   COLSET, ZWRSCR
C
C     + + + END SPECIFICATIONS + + +
C
C     set to inverse colors
      CALL COLSET (ZRFCOL,ZRBCOL)
C
      CALL ZWRSCR (STRING,LINE,COLUMN)
C
C     set back to normal colors
      CALL COLSET (ZNFCOL,ZNBCOL)
C
      RETURN
      END
