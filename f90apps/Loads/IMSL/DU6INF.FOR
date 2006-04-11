C-----------------------------------------------------------------------
C  IMSL Name:  U6INF/DU6INF  (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Stopping conditions for unconstrained minimization.
C
C  Usage:      CALL U6INF (N, XP, SC, FP, GP, XSCALE, FSCALE, ICODE,
C                          ITER, NFCN, NGRAD, NHESS, USRHES, MXTAKE)
C
C  Arguments:
C     N      - Dimension of the problem.  (Input)
C     XP     - Vector of length N containing the new iterate.
C              (Input)
C     SC     - Vector of length N containing step taken.  (Input)
C     FP     - Scalar containing the function value at XP.  (Input)
C     GP     - Vector of length N containing the gradient at XP.
C              (Input)
C     XSCALE - Vector of length N containing the diagonal scaling
C              matrix for the variables.  (Input)
C     FSCALE - Scalar containing the function scaling.  (Input)
C     ICODE  - Return code from the global strategy algorithm.  (Input)
C     ITER   - Number of iterations.  (Input)
C     NFCN   - Number of function evaluations.  (Input)
C     NGRAD  - Number of gradient evaluations.  (Input)
C     NHESS  - Number of Hessian evaluations.   (Input)
C     USRHES - Logical variable.  (Input)
C              USRHES = .TRUE. if Newton's method is used.
C              USRHES = .FALSE. otherwise.
C     MXTAKE - Logical variable indicating a step of maximum length was
C              taken.  (Input)
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
      SUBROUTINE DU6INF (N, XP, SC, FP, GP, XSCALE, FSCALE, ICODE,
     &                   ITER, NFCN, NGRAD, NHESS, USRHES, MXTAKE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, ICODE, ITER, NFCN, NGRAD, NHESS
      DOUBLE PRECISION FP, FSCALE, XP(*), SC(*), GP(*), XSCALE(*)
      LOGICAL    USRHES, MXTAKE
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
      DOUBLE PRECISION SCGRAD, SCSTEP, VALMAX
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      INTEGER    NMAXS
      SAVE       NMAXS
C                                  SPECIFICATIONS FOR COMMON /DU16NF/
      COMMON     /DU16NF/ GRADTL, STEPTL, RFTOL, AFTOL, FALSTL,
     &           MXITER, MAXFCN, MAXGRD, MAXHES
      INTEGER    MXITER, MAXFCN, MAXGRD, MAXHES
      DOUBLE PRECISION GRADTL, STEPTL, RFTOL, AFTOL, FALSTL
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DMAX1
      INTRINSIC  DABS, DMAX1
      DOUBLE PRECISION DABS, DMAX1
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STD, DU18NF
C
      CALL E1PSH ('DU6INF ')
C
C                                  TEST OF NORM OF SCALED GRADIENT
      VALMAX = 0.0D0
      DO 10  I=1, N
         SCGRAD = DABS(GP(I))*DMAX1(DABS(XP(I)),1.0D0/XSCALE(I))/
     &            DMAX1(DABS(FP),FSCALE)
         VALMAX = DMAX1(SCGRAD,VALMAX)
   10 CONTINUE
      IF (VALMAX .LE. GRADTL) THEN
         ICODE = -999
         GO TO 9000
      END IF
C                                  IF FIRST ITER., INITIALIZE COUNTER
C                                  FOR MX. STEP TAKEN AND RETURN
      IF (ITER .EQ. 0) THEN
         NMAXS = 0
         GO TO 9000
      END IF
C                                  CHECK LAST GLOBAL STEP
      IF (ICODE .EQ. 1) THEN
         CALL E1STD (1, STEPTL)
         CALL E1MES (3, 8, 'The last global step failed to '//
     &               'locate a lower point than the current X '//
     &               'value.  The current X may be an approximate '//
     &               'local minimizer and no more accuracy is '//
     &               'possible or the step tolerance may be too '//
     &               'large where STEPTL = %(D1) is given.')
         GO TO 9000
      END IF
C                                  TEST NORM OF SCALED STEP
      VALMAX = 0.0D0
      DO 20  I=1, N
         SCSTEP = DABS(SC(I))/DMAX1(DABS(XP(I)),1.0D0/XSCALE(I))
         VALMAX = DMAX1(SCSTEP,VALMAX)
   20 CONTINUE
      IF (VALMAX .LE. STEPTL) THEN
         ICODE = -999
         GO TO 9000
      END IF
C                                  CHECK RELATIVE FUNCTION CONVERGENCE
C                                  TOLERANCE
      IF (ICODE .EQ. 2) THEN
         CALL E1STD (1, RFTOL)
         CALL E1MES (3, 1, 'RELATIVE FUNCTION CONVERGENCE - '//
     &               'Both the actual and predicted relative '//
     &               'reductions in the function are less '//
     &               'than or equal to the relative function '//
     &               'convergence tolerance RFTOL = %(D1).')
         GO TO 9000
      END IF
C                                  CHECK FALSE CONVERGENCE TOLERANCE
      IF (ICODE .EQ. 3) THEN
         CALL E1MES (4, 2, 'FALSE CONVERGENCE - The iterates '//
     &               'appear to be converging to a noncritical '//
     &               'point.  Possibly incorrect gradient '//
     &               'information is used, or the function is '//
     &               'discontinuous, or the other stopping '//
     &               'tolerances are too tight.')
         GO TO 9000
      END IF
C                                  CHECK ITERATION, FUNCTION, GRADIENT
C                                  & HESSIAN EVALUATIONS LIMIT
      IF (ITER .GE. MXITER) THEN
C                                  Print error message
         CALL DU18NF (3)
CC         CALL E1MES (4, 3, ' Maximum number of iterations '//
CC     &               'exceeded.')
C
      ELSE IF (NFCN .GE. MAXFCN) THEN
C                                  Print error message
         CALL DU18NF (4)
CC         CALL E1MES (4, 4, ' Maximum number of function evaluations'//
CC     &               ' exceeded.')
C
      ELSE IF (NGRAD .GE. MAXGRD) THEN
C                                  Print error message
         CALL DU18NF (5)
CC         CALL E1MES (4, 5, ' Maximum number of gradient evaluations'//
CC     &               ' exceeded.')
C
      ELSE IF (USRHES .AND. (NHESS.GE.MAXHES)) THEN
C                                  Print error message
         CALL DU18NF (7)
CC         CALL E1MES (4, 7, 'Maximum number of Hessian evaluations '//
CC     &               'exceeded.')
C
      ELSE IF (MXTAKE) THEN
         NMAXS = NMAXS + 1
         IF (NMAXS .EQ. 5) THEN
C                                  Print error message
            CALL DU18NF (6)
CC            CALL E1MES (4, 6, ' Five consecutive steps of '//
CC     &                  'length STEPMX have been taken; either the '//
CC     &                  'function is unbounded below, or has a '//
CC     &                  'finite asymptote in some direction or the '//
CC     &                  'maximum allowable step size STEPMX is too '//
CC     &                  'small.')
         END IF
      END IF
C
 9000 CALL E1POP ('DU6INF ')
      RETURN
      END
