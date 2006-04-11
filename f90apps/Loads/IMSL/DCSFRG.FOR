C-----------------------------------------------------------------------
C  IMSL Name:  CSFRG/DCSFRG (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    August 23, 1985
C
C  Purpose:    Extend a real symmetric matrix defined in its upper
C              triangle to its lower triangle.
C
C  Usage:      CALL CSFRG (N, A, LDA)
C
C  Arguments:
C     N      - Order of the matrix A.  (Input)
C     A      - N x N symmetric matrix to be filled out.  (Input/Output)
C     LDA    - Leading dimension of A exactly as specified in the
C              dimension statement of the calling program.  (Input)
C
C  GAMS:       D1b9
C
C  Chapter:    MATH/LIBRARY Basic Matrix/Vector Operations
C
C  Copyright:  1986 by IMSL, Inc.  All Rights Reserved.
C
C  Warranty:   IMSL warrants only that IMSL testing has been applied
C              to this code.  No other warranty, expressed or implied,
C              is applicable.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DCSFRG (N, A, LDA)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA
      DOUBLE PRECISION A(LDA,*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, DCOPY
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   N1RCD
      INTEGER    N1RCD
C
      CALL E1PSH ('DCSFRG ')
C
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'N = %(I1).  The order of A, N, '//
     &               'must be greater than 0.')
      END IF
C
      IF (N .GT. LDA) THEN
         CALL E1STI (1, N)
         CALL E1STI (2, LDA)
         CALL E1MES (5, 2, 'N = %(I1) and LDA = %(I2).  The order of '//
     &               'A, N, must be less than or equal to the '//
     &               'leading dimension of A, LDA.')
      END IF
      IF (N1RCD(0) .NE. 0) GO TO 9000
C                                  Copy upper triangular values to lower
C                                  triangular values
      DO 10  I=1, N - 1
         CALL DCOPY (N-I, A(I,I+1), LDA, A(I+1,I), 1)
   10 CONTINUE
C                                  Exit section
 9000 CALL E1POP ('DCSFRG ')
      RETURN
      END
