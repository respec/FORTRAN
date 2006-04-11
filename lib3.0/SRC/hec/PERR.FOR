      SUBROUTINE PERR
C
C       SUBROUTINE 'PERR' IS THE CONTINGENCY SUBROUTINE FOR PREAD.
C       IF AN ERROR IS ENCOUNTERED WHILE IN THE PREAD PROCESS,
C       PERR IS CALLED AND IN TURN CLOSES ALL OPENED FILES AND
C       RESETS THE SYSTEM PROMPT.
C
CADD C.PFILES                                                           H
      INCLUDE 'pfiles.h'                                                MLlg
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
CADD C.PERROR                                                           H
      INCLUDE 'perror.h'                                                MLlg
CADD C.PTAB                                                             H
      INCLUDE 'ptab.h'                                                  MLlg
C
      COMMON /TABEXI/ LTABEX
      LOGICAL LTABEX
C
C
C
      IF (ISCT.NE.-1) CLOSE (UNIT=ISCT)
      IF (IFUN.NE.-1) CLOSE (UNIT=IFUN)
      IF (IMAC.NE.-1) CLOSE (UNIT=IMAC)
      IF (IMEN.NE.-1) CLOSE (UNIT=IMEN)
      IF (ILOG.NE.-1) CLOSE (UNIT=ILOG)
      IF (ISCN.NE.-1) CLOSE (UNIT=ISCN)
C
      END
