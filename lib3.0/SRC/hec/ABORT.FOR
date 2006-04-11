      SUBROUTINE ABORT                                                  PCMALlg
C
C     TEMPOARY ABORT ROUTINE FOR NON-HARRIS SITES
C
      WRITE (6,10)                                                      PCMALlg
 10   FORMAT (//,' ***** PROGRAM CALLED ABORT *****',//)                PCMALlg
C
C     GET THE SQUARE ROOT OF A NEGATIVE NUMBER TO CAUSE A FATAL ERROR
      X = SIN (0.2)                                                     PCMALlg
      X = -X                                                            PCMALlg
      Y = SQRT (X)                                                      PCMALlg
      WRITE (6,*) Y                                                     PCMALlg
      STOP                                                              PCMALlg
      END                                                               PCMALlg
