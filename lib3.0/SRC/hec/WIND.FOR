      SUBROUTINE WIND (ILFN)                                            PMLlg
C
      CHARACTER CA*1                                                    PMLlg
C
      DO 20 I=1,30000                                                   PMLlg
      READ (ILFN,10,ERR=100,END=100) CA                                 PMLlg
 10   FORMAT (A)                                                        PMLlg
 20   CONTINUE                                                          PMLlg
C
 100  CONTINUE                                                          PMLlg
      BACKSPACE ILFN                                                    MLlg
      RETURN                                                            PMLlg
      END                                                               PMLlg
