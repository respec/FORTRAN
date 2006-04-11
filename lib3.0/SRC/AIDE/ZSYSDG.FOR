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
C     ***  System specific for PRIME/VAX, calls to COLSET
C     ***  replaced with calls to ZFMTWR,
C     ***  thus ZCNTRL.INC not needed in this version
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
C     + + + INTRINSICS + + +
      INTRINSIC  CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZFMTWR, ZWRSCR
C
C     + + + END SPECIFICATIONS + + +
C
C     set to inverse colors
C     CALL COLSET (ZRFCOL,ZRBCOL)
      CALL ZFMTWR (CHAR(27)//'[7m')
C
      CALL ZWRSCR (STRING,LINE,COLUMN)
C
C     set back to normal colors
C     CALL COLSET (ZNFCOL,ZNBCOL)
      CALL ZFMTWR (CHAR(27)//'[0m')
C
      RETURN
      END
C
C
C
      SUBROUTINE   GT1ST
     I                  (PAT,ATTR,
     O                   FTIME,FDATE,FSIZE,ERR)
C
C     + + + PURPOSE + + +
C     dummy routine to get first file name from directory
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*2    ATTR,FTIME,FDATE,ERR
      INTEGER      FSIZE
      CHARACTER*64 PAT
C
C     + + + END SPECIFICATIONS + + +
C
      ERR= 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   GTNXT
     I                  (PAT,ATTR,
     O                   FTIME,FDATE,FSIZE,ERR)
C
C     + + + PURPOSE + + +
C     dummy routine to get file names from directory
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*2    ATTR,FTIME,FDATE,ERR
      INTEGER      FSIZE
      CHARACTER*64 PAT
C
C     + + + END SPECIFICATIONS + + +
C
      ERR= 1
C
      RETURN
      END
