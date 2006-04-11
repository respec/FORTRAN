      SUBROUTINE TINTRP(X,Y,L,XE,YE)
C=======================================================================
C     LINEAR INTERPOLATION ROUTINE FOR USE BY STORAGE UNIT CALCULATIONS.
C     UPDATED BY S.J.NIX, SEPTEMBER 1981.
C=======================================================================
      DIMENSION X(L),Y(L)
C=======================================================================
C     SEARCH FOR THE SMALLEST X GREATER THAN XE.
C=======================================================================
      DO 150 J = 2,L
      K        = J
      IF(XE-X(J)) 300,200,150
  150 CONTINUE
C=======================================================================
C     XE COINCIDES WITH A VALUE OF X, SO INTERPOLATION IS UNNECESSARY.
C=======================================================================
  200 YE = Y(K)
      RETURN
C=======================================================================
C     NOW X(K) IS THE SMALLEST VALUE OF X GREATER THAN XE, COMPUTE
C     YE BY LINEAR INTERPOLATION.
C=======================================================================
  300 YE = Y(K-1)+(Y(K)-Y(K-1))*(XE-X(K-1))/(X(K)-X(K-1))
      RETURN
      END
