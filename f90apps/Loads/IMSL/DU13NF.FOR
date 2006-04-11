C-----------------------------------------------------------------------
C  IMSL Name:  U13NF/DU13NF (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Solve L*s = g for s.
C
C  Usage:      CALL U13NF (N, H, LDH, GC, SNWTN)
C
C  Arguments:
C     N      - Length of the vectors GC, SNWTN.  (Input)
C     H      - N by N matrix containing the Cholesky factor of the
C              Hessian in the lower triangle and diagonal.  (Input)
C     LDH    - Leading dimension of H exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     GC     - Vector of length N containing the current gradient.
C              (Input)
C     SNWTN  - Vector of length N containing the solution.  (Output)
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
      SUBROUTINE DU13NF (N, H, LDH, GC, SNWTN)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDH
      DOUBLE PRECISION H(LDH,*), GC(*), SNWTN(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
      DOUBLE PRECISION SUM
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DDOT
      DOUBLE PRECISION DDOT
C
      SNWTN(1) = GC(1)/H(1,1)
      DO 10  I=2, N
         SUM = DDOT(I-1,H(I,1),LDH,SNWTN,1)
         SNWTN(I) = (GC(I)-SUM)/H(I,I)
   10 CONTINUE
C
      RETURN
      END
