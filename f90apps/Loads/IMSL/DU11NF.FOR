C-----------------------------------------------------------------------
C  IMSL Name:  U11NF/DU11NF (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Compute X = Y**K * Z.
C
C  Usage:      CALL U11NF (N, Y, K, Z, X)
C
C  Arguments:
C     N      - Length of the vectors X, Y and Z.  (Input)
C     Y      - Vector of length N.  (Input)
C     K      - Integer specifying the exponent.  (Input)
C     Z      - Vector of length N.  (Input)
C     X      - Vector of length N.  (Output)
C
C  Chapter:    MATH/LIBRARY Optimization
C
C  Copyright:  1985 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DU11NF (N, Y, K, Z, X)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, K
      DOUBLE PRECISION Y(*), Z(*), X(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
C
      IF (K .LT. 0) THEN
         IF (K .EQ. -1) THEN
            DO 10  I=1, N
               X(I) = Z(I)/Y(I)
   10       CONTINUE
         ELSE
            DO 20  I=1, N
               X(I) = Z(I)/(Y(I)**(-K))
   20       CONTINUE
         END IF
      ELSE
         IF (K .EQ. 1) THEN
            DO 30  I=1, N
               X(I) = Z(I)*Y(I)
   30       CONTINUE
         ELSE
            DO 40  I=1, N
   40       X(I) = Y(I)**K*Z(I)
         END IF
      END IF
C
      RETURN
      END
