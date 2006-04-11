      SUBROUTINE PRSTAT (LFLAG, CHCOM, CHFUNC)
C
C        SUBROUTINE 'PRSTAT' SIMPLY IDENTIFIES AND STORES THE
C        STATUS OF PREAD COMMANDS AND CONTROL CHARACTERS.  THE
C        STATUS OF THE COMMANDS ARE STORED IN A BOOLEAN
C        AND THE CONTROL CHARACTERS FOR COMMENTS AND FUNCTIONS
C        IN CHARACTER VARIABLES.
C
C
CADD C.PCHAR                                                            H
      INCLUDE 'pchar.h'                                                 MLlg
CADD C.PLFLAG                                                           H
      INCLUDE 'plflag.h'                                                MLlg
C
      CHARACTER*1 CHCOM,CHFUNC
C
      LOGICAL LFLAG(6)
C   ***
C   ***
C   ***
        LFLAG(1) = LMACRO
        LFLAG(2) = LMENU
        LFLAG(3) = LECHO
        LFLAG(4) = LFUN
        LFLAG(5) = LLEARN
        LFLAG(6) = LLOG
        CHCOM = CSPL(1:1)
        CHFUNC = CSPL(4:4)
        RETURN
      END
