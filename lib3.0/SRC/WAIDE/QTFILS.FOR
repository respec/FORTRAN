C
C
C
      SUBROUTINE   OPTOUT
     I                    (MESSFL, WNDNAM,
     M                     TORF, LINES, WIDTH,
     O                     FOUT, RETC)
C
C     + + + PURPOSE + + +
C     This routine asks if output is to go to the terminal or a file.
C     For the terminal option the user specifies the width and number
C     of lines for the terminal.  for file the user specifies the width
C     and the number of lines per page for printout.  For a flat file
C     the user should specify a value greater than the expected output.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, TORF, LINES, WIDTH, FOUT, RETC
      CHARACTER WNDNAM*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file
C     WNDNAM - data window screen name
C     TORF   - output option
C              1 - terminal
C              2 - file, flat or printed
C     LINES  - number of lines for page or terminal screen
C     WIDTH  - number of characters wide
C     FOUT   - Fortran unit number for output
C     RETC   - return code
C              0 - successful
C              1 - could not open output file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ERR, RNUM, INUM, CNUM, IVAL(2), OPT(1,3),
     #          SGRP, FSCLU, IND
      REAL      RVAL(1)
      CHARACTER*1  TBUFF(80)
C
C     + + + EXTERNALS + + +
      EXTERNAL   QRESPM, ANPRGT, QFOPEN, ZWNSET
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  RNUM, INUM, CNUM, IND
     #    /    1,   2,     1,   4 /
C
C     + + + END SPECIFICATIONS + + +
C
      FSCLU= 2
      RETC = 0
      ERR  = 0
C
C     set programmer specified defaults
      IVAL(1) = -999
      IVAL(2) = -999
      OPT(1,1)= 1
      IF (LINES.GT.5 .AND. LINES.LT.200000) THEN
C       valid number of lines
        IVAL(1) = LINES
      END IF
      IF (WIDTH.GT.40 .AND. WIDTH.LT.250) THEN
C       valid number of characters
        IVAL(2) = WIDTH
      END IF
      IF (TORF.NE.1) THEN
C       set output target to file (instead of default of screen)
        OPT(1,1) = TORF
      END IF
C
C     get user specifications
      CALL ZWNSET (WNDNAM)
      SGRP = 57
      CALL QRESPM (MESSFL, FSCLU, SGRP, INUM, RNUM, CNUM,
     M             IVAL, RVAL, OPT, TBUFF)
      IF (OPT(1,1) .EQ. 1) THEN
C       terminal output, get unit number
        CALL ANPRGT (IND, FOUT)
      ELSE
C       file output, open file and get unit number
        CALL ZWNSET (WNDNAM)
        SGRP = 12
        CALL QFOPEN (MESSFL, FSCLU, SGRP, FOUT, ERR)
      END IF
C
      TORF = OPT(1,1)
      LINES= IVAL(1)
      WIDTH= IVAL(2)
      IF (ERR.NE.0) THEN
C       problem opening file
        RETC = 1
      END IF
C
      RETURN
      END
