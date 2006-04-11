      SUBROUTINE WAITS (SECS)
C     WAIT THE SPECIFIED # OF SECONDS
C     I=SECS*120.0                                                      H
C     CALL WAIT (I,0,K)                                                 H
C
      INTEGER*4 IHUND, JHUND                                            ML
C
C     CALL CBTIME(IHUND)                                                M
      CALL TIMER (IHUND)                                                L
 5    CONTINUE                                                          ML
C     CALL CBTIME(JHUND)                                                M
      CALL TIMER (JHUND)                                                L
      T = (JHUND - IHUND) * .01                                         ML
      IF ( T .LT. SECS ) GO TO 5                                        ML
      RETURN
      END
