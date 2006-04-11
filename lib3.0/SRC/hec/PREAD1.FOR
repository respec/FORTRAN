C
      SUBROUTINE PREAD1 (CLIN1)
C
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
C
      CHARACTER CLIN1*(*),CLIN2*128
C
C
C
      IENTRY = 1
      ISCRT = IKB
      CALL PMAIN(ISCRT,CLIN1,CLIN2,IENTRY)
      RETURN
      END
