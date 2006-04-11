      SUBROUTINE PREADC (ISCRT, CLIN1, IENTRY, *)
C
C
CADD C.PNUMS                                                            H
      INCLUDE 'pnums.h'                                                 MLlg
C
      CHARACTER CLIN1*(*), CLIN2*128
C
C
      IENTRY = 3
      IOLDKB = IKB
      IKB =ISCRT
      CALL PMAIN(ISCRT,CLIN1,CLIN2,IENTRY)
      IKB = IOLDKB
      IF (IENTRY.EQ.-1) RETURN 1
      RETURN
C
      END
