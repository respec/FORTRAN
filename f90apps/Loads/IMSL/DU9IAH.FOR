C-----------------------------------------------------------------------
C  IMSL Name:  U9IAH/DU9IAH (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Calculated the perturbed Cholesky decomposition.
C
C  Usage:      CALL U9IAH (N, A, LDA, IRC)
C
C  Arguments:
C     N      - Dimension of the problem.  (Input)
C     A      - Real N by N matrix.  (Input/Output)
C              On input, A is the symmetric positive definite matrix
C                 with only the lower triangle and diagonal stored.
C              On output, A contains L of the Cholesky factorization of
C                 the perturbed matrix in the lower triangular part and
C                 diagonal.
C     LDA    - Leading dimension of A exactly as specified in the
C              dimension statement of the calling program.  (Input)
C     IRC    - Scalar indicating if A is positive definite.  (Output)
C              If IRC = 0, then A is positive definite.
C              If IRC = J, then the leading principal J by J submatrix
C              of A is not positive definite.
C
C  Remark:
C     This is based on Algorithm A5.5.2, page 318, Dennis-Schnabel book,
C     and NL2SOL routine LSQRT.
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
      SUBROUTINE DU9IAH (N, A, LDA, IRC)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA, IRC
      DOUBLE PRECISION A(LDA,*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, J
      DOUBLE PRECISION TEMP
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DSQRT
      INTRINSIC  DSQRT
      DOUBLE PRECISION DSQRT
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   DSCAL
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DDOT, DNRM2
      DOUBLE PRECISION DDOT, DNRM2
C
      IRC = 0
      DO 20  J=1, N
C                                  FORM COLUMN J OF THE CHOL. FACTOR
         A(J,J) = A(J,J) - DNRM2(J-1,A(J,1),LDA)**2
         DO 10  I=J + 1, N
            A(I,J) = A(I,J) - DDOT(J-1,A(I,1),LDA,A(J,1),LDA)
   10    CONTINUE
         IF (A(J,J) .GT. 0.0D0) THEN
            A(J,J) = DSQRT(A(J,J))
         ELSE
            IRC = J
            GO TO 30
         END IF
C
         TEMP = 1.0D0/A(J,J)
         IF (J .LT. N) CALL DSCAL (N-J, TEMP, A(J+1,J), 1)
   20 CONTINUE
C
   30 RETURN
      END
