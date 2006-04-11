      SUBROUTINE PREAD2 (CLIN1, CLIN2)
C
C
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
C
      CHARACTER CLIN1*(*),CLIN2*(*)
C
C
C
      IENTRY = 2
      ISCRT = IKB
      CALL PMAIN(ISCRT,CLIN1,CLIN2,IENTRY)
      RETURN
      END
