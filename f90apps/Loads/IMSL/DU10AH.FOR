C-----------------------------------------------------------------------
C  IMSL Name:  U10AH/DU10AH (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Compute a good over-estimate of the smallest singular
C              value of a lower triangular matrix.
C
C  Usage:      U10AH (N, L, LDL, X, Y)
C
C  Arguments:
C     N      - Dimension of the problem.  (Input)
C     L      - Real N by N lower triangular matrix.  (Input)
C     LDL    - Row dimension of L exactly as specified in the dimension
C              statement of the calling program.  (Input)
C     X      - Real vector of length N containing a normalized
C              approximat
C              left singular vector if U10AH return a positive value
C                (Output)
C     Y      - Real vector of length N containing an unnormalized
C              approximate right singular vector if U10AH return a
C              positive value.  (Output)
C
C  Remark:
C     This is based on NL2SOL routine LSVMIN.
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
      DOUBLE PRECISION FUNCTION DU10AH (N, L, LDL, X, Y)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDL
      DOUBLE PRECISION L(LDL,*), X(*), Y(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, J, JJJ, JM1, NPLUS1
      DOUBLE PRECISION B, PSJ, SMINUS, SPLUS, T, XMINUS, XPLUS
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    IX
      DOUBLE PRECISION HALF, ONE, R9973, ZERO
      SAVE       HALF, IX, ONE, R9973, ZERO
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DBLE,MOD
      INTRINSIC  DABS, DBLE, MOD
      INTEGER    MOD
      DOUBLE PRECISION DABS, DBLE
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   DAXPY, DSCAL
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DDOT, DNRM2
      DOUBLE PRECISION DDOT, DNRM2
C
      DATA IX/2/
      DATA HALF/0.5D0/, ONE/1.0D0/, R9973/9973.0D0/, ZERO/0.0D0/
C
C  ***  BODY  ***
C
C  ***  FIRST CHECK WHETHER TO RETURN DU10AH = 0 AND INITIALIZE X
C
      DO 10  I=1, N
         X(I) = 0.0D0
         IF (L(I,I) .EQ. 0.0D0) GO TO 60
   10 CONTINUE
      IF (MOD(IX,9973) .EQ. 0) IX = 2
      NPLUS1 = N + 1
C
C  ***  SOLVE (L**T)*X = B, WHERE THE COMPONENTS OF B HAVE RANDOMLY
C  ***  CHOSEN MAGNITUDES IN (.5,1) WITH SIGNS CHOSEN TO MAKE X LARGE.
C
C     DO J = P TO 1 BY -1...
      DO 40  JJJ=1, N
         J = NPLUS1 - JJJ
C       ***  DETERMINE X(J) IN THIS ITERATION. NOTE FOR I = 1,2,...,J
C       ***  THAT X(I) HOLDS THE CURRENT PARTIAL SUM FOR ROW I.
         IX = MOD(3432*IX,9973)
         B = 0.5D0*(1.0D0+DBLE(IX)/R9973)
         XPLUS = (B-X(J))
         XMINUS = (-B-X(J))
         SPLUS = DABS(XPLUS)
         SMINUS = DABS(XMINUS)
         JM1 = J - 1
         XPLUS = XPLUS/L(J,J)
         XMINUS = XMINUS/L(J,J)
         IF (JM1 .EQ. 0) GO TO 30
         DO 20  I=1, JM1
            SPLUS = SPLUS + DABS(X(I)+L(J,I)*XPLUS)
            SMINUS = SMINUS + DABS(X(I)+L(J,I)*XMINUS)
   20    CONTINUE
   30    IF (SMINUS .GT. SPLUS) XPLUS = XMINUS
         X(J) = XPLUS
C       ***  UPDATE PARTIAL SUMS  ***
         CALL DAXPY (JM1, XPLUS, L(J,1), LDL, X, 1)
   40 CONTINUE
C
C  ***  NORMALIZE X  ***
C
      T = 1.0D0/DNRM2(N,X,1)
      CALL DSCAL (N, T, X, 1)
C
C  ***  SOLVE L*Y = X AND RETURN SVMIN = 1/TWONORM(Y)  ***
C
      DO 50  J=1, N
         PSJ = DDOT(J-1,Y,1,L,LDL)
         Y(J) = (X(J)-PSJ)/L(J,J)
   50 CONTINUE
C
      DU10AH = 1.0D0/DNRM2(N,Y,1)
      GO TO 70
C
   60 DU10AH = 0.0D0
   70 RETURN
      END
