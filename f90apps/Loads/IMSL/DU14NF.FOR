C-----------------------------------------------------------------------
C  IMSL Name:  U14NF/DU14NF (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Solve TRANS(L)*s = y for s.
C
C  Usage:      CALL U14NF (N, H, LDH, Y, SNWTN)
C
C  Arguments:
C     N      - Length of the vector SNWTN.  (Input)
C     H      - N by N matrix containing the Cholesky factor of the
C              Hessian in the lower triangle and diagonal.  (Input)
C     LDH    - Leading dimension of H exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     Y      - Vector of length N containing the right-hand-side.
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
      SUBROUTINE DU14NF (N, H, LDH, Y, SNWTN)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDH
      DOUBLE PRECISION H(LDH,*), Y(*), SNWTN(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
      DOUBLE PRECISION SUM
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DDOT
      DOUBLE PRECISION DDOT
C
      SNWTN(N) = Y(N)/H(N,N)
      DO 10  I=N - 1, 1, -1
         SUM = DDOT(N-I,H(I+1,I),1,SNWTN(I+1),1)
         SNWTN(I) = (Y(I)-SUM)/H(I,I)
   10 CONTINUE
C
      RETURN
      END
