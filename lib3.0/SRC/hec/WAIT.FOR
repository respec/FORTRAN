      SUBROUTINE WAIT ( I1, I2, I3)                                     PCMALlg
C
C     TEMPORARY SUBROUTINE FOR NON-HARRIS SITES
C
      REAL TIM                                                          MLlg
      I3 = 1                                                            MLlg
      IF (I2.EQ.0) THEN                                                 MLlg
         TIM = I1/120.0                                                 MLlg
      ELSE IF (I2.EQ.1) THEN                                            MLlg
         TIM = .001*I1                                                  MLlg
      ELSE IF (I2.EQ.2) THEN                                            MLlg
         TIM = I1*1.0                                                   MLlg
      ELSE IF (I2.EQ.3) THEN                                            MLlg
         TIM = I1*60.0                                                  MLlg
      ELSE                                                              MLlg
         I3 = 2                                                         MLlg
C     RETURN                                                            lg
      ENDIF                                                             MLlg
      CALL WAITS (TIM)                                                  MLlg
      RETURN                                                            PCMALlg
      END                                                               PCMALlg
