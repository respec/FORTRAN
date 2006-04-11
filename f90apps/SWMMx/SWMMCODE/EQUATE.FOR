      SUBROUTINE EQUATE(I,IP,X,Y)
C=======================================================================
C     SUBROUTINE TO CALCULATE THE POLLUTANT REMOVAL EQUATION 7-1.
C=======================================================================
      INCLUDE 'S1.INC'
      DIMENSION F(6),P(6),X(11)
C=======================================================================
  100 Y         = 0.0
      DO 150 II = 1,6
      F(II)     = 0.0
  150 P(II)     = 0.0
C
  200 IF(A(I,IP,1)*X(1).GE.-20.0) F(1) = EXP(A(I,IP,1)*X(1))
      IF(A(I,IP,3)*X(3).GE.-20.0) F(2) = EXP(A(I,IP,3)*X(3))
      IF(A(I,IP,5)*X(5).GE.-20.0) F(3) = EXP(A(I,IP,5)*X(5))
      IF(A(I,IP,7)*X(7)+A(I,IP,8)*X(8).GE.-20.0) F(4) =
     +                            EXP(A(I,IP,7)*X(7)+A(I,IP,8)*X(8))
      IF(X(2).GT.0.0) P(1)  = X(2)**A(I,IP,2)
      IF(X(4).GT.0.0) P(2)  = X(4)**A(I,IP,4)
      IF(X(6).GT.0.0) P(3)  = X(6)**A(I,IP,6)
      IF(X(9).GT.0.0) P(4)  = X(9)**A(I,IP,9)
      IF(X(10).GT.0.0) P(5) = X(10)**A(I,IP,10)
      IF(X(11).GT.0.0) P(6) = X(11)**A(I,IP,11)
C
  300 DO 350 II = 1,6
      IF(F(II).LE.1.0E-5) F(II) = 0.0
      IF(P(II).LE.1.0E-5) P(II) = 0.0
  350 CONTINUE
C
  400 Y1 = A(I,IP,12)*F(1)*P(1)+A(I,IP,13)*F(2)*P(2)+
     1     A(I,IP,14)*F(3)*P(3)+A(I,IP,15)*F(4)*P(4)*P(5)*P(6)
  450 IF(Y1.GT.0.0) Y = Y1**A(I,IP,16)
C
  500 IF(Y.GT.RMX(I,IP)) Y = RMX(I,IP)
  550 IF(Y.LE.0.0)       Y = 0.0
C
      RETURN
      END
