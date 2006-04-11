C-----------------------------------------------------------------------
C  IMSL Name:  U3IAH/DU3IAH (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Driver for unconstrained minimization solver using finite
C              difference or analytic Hessian.
C
C  Usage:      CALL U3IAH (FCN, GRAD, HESS, N, XC, XSCALE, FSCALE,
C                          IPARAM, RPARAM, FVALUE, XP, SC, SNWTN, GC,
C                          GP, H, LDH, HDIAG, WK1, WK2, WK3)
C
C  Arguments:
C     FCN    - Name of the subroutine to evaluate the function.
C                 SUBROUTINE FCN (N, X, F)
C                 REAL X(*), F
C     GRAD   - Name of the subroutine to evaluate the gradient.
C                 SUBROUTINE GRAD (N, X, G)
C                 REAL X(*), G(*)
C     HESS   - Name of the subroutine to evaluate the Hessian if this
C              option is picked.
C                 SUBROUTINE HESS (N, X, H, LDH)
C                 REAL X(*), H(LDH,*)
C     N      - Dimension of the problem.  (Input)
C     XC     - Vector of length N containing initial guess on input
C              and current point on output.  (Input/Output)
C     XSCALE - Vector of length N containing the diagonal scaling
C              matrix for the variables.  (Input)
C     FSCALE - Scalar containing the function scaling.  (Input)
C     IPARAM - Parameters vector of length 7.  (Input/Output)
C              See UMIAH for details.
C     RPARAM - Parameters vector of length 7.  (Input)
C              See UMIAH for details.
C     FVALUE - Scalar containing the value of the function at the
C              solution.  (Output)
C     XP     - Vector of length N containing the updated point.
C                 (Output)
C     SC     - Vector of length N containing the last step taken.
C                 (Output)
C     SNWTN  - Vector of length N containing the last Newton step.
C                 (Output)
C     GC     - Vector of length N containing the gradient at the
C              current point.  (Output)
C     GP     - Vector of length N containing the gradient at the
C              updated point.  (Output)
C     H      - N by N matrix containing an estimate of the Hessian
C              at the solution.  (Output)
C     LDH    - Leading dimension of H as specified in the dimension
C              statement of the calling program.  (Input)
C     HDIAG  - Real work vector of length N containing the diagonal of
C              the estimate Hessian at the solution.  (Output)
C     WK1    - Real work vector of length N.  (Output)
C     WK2    - Real work vector of length N.  (Output)
C     WK3    - Real work vector of length N.  (Output)
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
      SUBROUTINE DU3IAH (FCN, GRAD, HESS, N, XC, XSCALE, FSCALE,
     &                   IPARAM, RPARAM, FVALUE, XP, SC, SNWTN, GC,
     &                   GP, H, LDH, HDIAG, WK1, WK2, WK3)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDH, IPARAM(*)
      DOUBLE PRECISION FSCALE, FVALUE, XC(*), XSCALE(*), RPARAM(*),
     &           XP(*), SC(*), SNWTN(*), GC(*), GP(*), H(LDH,*),
     &           HDIAG(*), WK1(*), WK2(*), WK3(*)
      EXTERNAL   FCN, GRAD, HESS
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, ICODE, ITER, J, KA, NFCN, NGRAD, NHESS
      DOUBLE PRECISION DELTA, FC, FP, PREDUC, STEPMX
      LOGICAL    LTRY1, MXTAKE, NEWTON
C                                  SPECIFICATIONS FOR COMMON /DU16NF/
      COMMON     /DU16NF/ GRADTL, STEPTL, RFTOL, AFTOL, FALSTL,
     &           MXITER, MAXFCN, MAXGRD, MAXHES
      INTEGER    MXITER, MAXFCN, MAXGRD, MAXHES
      DOUBLE PRECISION GRADTL, STEPTL, RFTOL, AFTOL, FALSTL
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1POP, E1PSH, E1USR, DCOPY, DU4INF, DU5INF, DU6INF,
     &           DU7IAH, DU8IAH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   N1RCD, N1RTY
      INTEGER    N1RCD, N1RTY
C
      CALL E1PSH ('DU3IAH ')
C                                  SET DEFAULT PARAMETERS
      IF (IPARAM(1) .EQ. 0) CALL DU4INF (IPARAM, RPARAM)
C                                  CHECK THE VALIDITY OF THE USER
C                                  SPECIFIED PARAMETERS
      CALL DU5INF (N, XC, XSCALE, FSCALE, .TRUE., IPARAM, RPARAM)
      IF (N1RTY(1) .EQ. 5) GO TO 9000
C
      MXITER = IPARAM(3)
      MAXFCN = IPARAM(4)
      MAXGRD = IPARAM(5)
      MAXHES = IPARAM(7)
C
      GRADTL = RPARAM(1)
      STEPTL = RPARAM(2)
      RFTOL = RPARAM(3)
      AFTOL = RPARAM(4)
      FALSTL = RPARAM(5)
      STEPMX = RPARAM(6)
      DELTA = RPARAM(7)
C                                  INITIALIZE ITERATION, FUNCTION,
C                                  GRADIENT & HESSIAN EVAL. COUNTER
      ITER = 0
      NFCN = 0
      NGRAD = 0
      NHESS = 0
      LTRY1 = .TRUE.
C                                  EVALUATE THE FUNCTION AND GRADIENT
C                                  AT THE INITIAL POINT; ALSO SET
C                                  FVALUE = FUNCTION VALUE AT THE
C                                  INITIAL GUESS FOR THE CASE THAT
C                                  THE INITIAL GUESS IS THE SOLUTION.
      CALL E1USR ('ON')
      CALL FCN (N, XC, FC)
      CALL GRAD (N, XC, GC)
      CALL E1USR ('OFF')
      NFCN = NFCN + 1
      NGRAD = NGRAD + 1
      FVALUE = FC
      ICODE = 0
C                                  CHECK STOPPING CRITERIA AT THE
C                                  INITIAL POINT
      CALL DU6INF (N, XC, SC, FC, GC, XSCALE, FSCALE, ICODE, ITER,
     &             NFCN, NGRAD, NHESS, .TRUE., MXTAKE)
      IF (N1RCD(1).NE.0 .OR. ICODE.EQ.-999) GO TO 50
C                                  GET THE (APPROXIMATE) HESSIAN AT THE
C                                  INITAL POINT
      CALL E1USR ('ON')
      CALL HESS (N, XC, H, LDH)
      CALL E1USR ('OFF')
      NHESS = NHESS + 1
C                                  MAIN ITERATION LOOP
   10 CONTINUE
      ITER = ITER + 1
C                                  COMPUTE THE LOCALLY CONSTRAINED
C                                  OPTIMAL STEP
      DO 30  J=1, N
         DO 20  I=J, N
            H(I,J) = H(I,J)/(XSCALE(I)*XSCALE(J))
   20    CONTINUE
   30 CONTINUE
      ICODE = 6
      KA = -1
   40 CALL DU8IAH (N, GC, H, LDH, XSCALE, HDIAG, STEPMX, DELTA, KA,
     &             NEWTON, LTRY1, SC, PREDUC, WK1, WK2, SNWTN)
C                                  CHECK NEW POINT AND UPDATE THE TRUST
C                                  REGION
      CALL DU7IAH (FCN, N, XC, FC, GC, H, LDH, SC, XSCALE, NEWTON,
     &             STEPMX, 1, HDIAG, DELTA, ICODE, WK3, XP, FP,
     &             MXTAKE, NFCN, PREDUC)
      IF (ICODE .GE. 4) GO TO 40
C                                  EVALUATE THE GRADIENT AT NEW POINT
      CALL E1USR ('ON')
      CALL GRAD (N, XP, GP)
      CALL E1USR ('OFF')
      NGRAD = NGRAD + 1
C                                  CHECK STOPPING CRITERIA AT NEW POINT
      CALL DU6INF (N, XP, SC, FP, GP, XSCALE, FSCALE, ICODE, ITER,
     &             NFCN, NGRAD, NHESS, .TRUE., MXTAKE)
      IF (N1RCD(1).EQ.0 .AND. ICODE.NE.-999) THEN
C                                  CALCULATE THE NEXT HESSIAN
C                                  APPROXIMATION; UPDATE XC, FC AND
C                                  GC AND START NEXT ITERATION
         CALL E1USR ('ON')
         CALL HESS (N, XP, H, LDH)
         CALL E1USR ('OFF')
         NHESS = NHESS + 1
         CALL DCOPY (N, XP, 1, XC, 1)
         CALL DCOPY (N, GP, 1, GC, 1)
         FC = FP
         GO TO 10
      END IF
C                                  OTHERWISE THE STOPPING CRITERIA IS
C                                  SATISFIED; RETURN
      CALL DCOPY (N, XP, 1, XC, 1)
      CALL DCOPY (N, GP, 1, GC, 1)
      FVALUE = FP
   50 IPARAM(3) = ITER
      IPARAM(4) = NFCN
      IPARAM(5) = NGRAD
      IPARAM(7) = NHESS
C
 9000 CALL E1POP ('DU3IAH ')
      RETURN
      END
