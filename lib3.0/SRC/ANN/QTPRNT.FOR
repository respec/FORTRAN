C
C
C
      SUBROUTINE   ERROR
     I                   (MESSFL,SCLU,SGRP,FIOU)
C
C     + + + PURPOSE + + +
C     This routine writes an error message to the output file FIOU,
C     then stops the program.  Only used for fatal system errors.
C     Error messages are from the message file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,SCLU,SGRP,FIOU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file containing error message
C     SCLU   - group containing error message
C     SGRP   - question number containing error message
C     FIOU   - output file unit number
C
C     + + + EXTERNALS + + +
      EXTERNAL   PRNTXT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (76('*'))
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE (FIOU,2000)
      CALL PRNTXT (MESSFL,SCLU,SGRP)
C     maybe print 'run terminated'?
      WRITE (FIOU,2000)
C     close files
C
      STOP
      END
C
C
C
      SUBROUTINE   PRTERR
     I                    (MESSFL,WNDNAM,RETCOD)
C
C     + + + PURPOSE + + +
C     This routine prints an error from the message file to the terminal.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,RETCOD
      CHARACTER WNDNAM*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file containing text to print
C     WNDNAM - window name for message
C     RETCOD - error return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    SCLU,SGRP
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZWNSET, PRNTXT
C
C     + + + END SPECIFICATIONS + + +
C
C     set window name
      CALL ZWNSET (WNDNAM)
C     display message based on return code
      SCLU = 50
      SGRP = ABS(RETCOD)
      CALL PRNTXT (MESSFL,SCLU,SGRP)
C
      RETURN
      END
