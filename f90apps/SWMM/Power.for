      SUBROUTINE POWER(D0,DFULL,TOP,WPP,PEXP,DINC)
C	CALLED BY GETCUR NEAR LINE 412 (TRANSPORT AND EXTRAN)
C=======================================================================
C     Romberg integration to find the wetted perimeter
C             of a power function cross section.
C=======================================================================
      REAL INTEG,DELTA,START,T(9,9)
C=======================================================================
      START     =  D0 - DINC
      END       =  D0
      DO 100 I  = 1,9
      N         = 2**I
      DELTA     = (END-START)/FLOAT(N)
      CALL GETSUM(N,START,DELTA,INTEG,TOP,DFULL,PEXP)
      T(I,1) = INTEG
      IF(I.GT.1) THEN
                 DO 50 J = 2,I
                 J1      = J-1
50               T(I,J)  = (4.0**J1*T(I,J-1)-T(I-1,J-1))/
     +                     (4.0**J1-1.0)
                 IF(ABS(T(I,I)-T(I-1,I-1)).LE.1.0E-04) GO TO 101
                 ENDIF
100   CONTINUE
      I   = 9
101   WPP = 2.0*T(I,I)
      RETURN
      END
