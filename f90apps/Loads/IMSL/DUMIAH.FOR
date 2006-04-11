C-----------------------------------------------------------------------
C  IMSL Name:  UMIAH/DUMIAH (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Minimize a function of N variables using a modified
C              Newton method and a user-supplied Hessian.
C
C  Usage:      CALL UMIAH (FCN, GRAD, HESS, N, XGUESS, XSCALE, FSCALE,
C                          IPARAM, RPARAM, X, FVALUE)
C
C  Arguments:
C     FCN    - User-supplied SUBROUTINE to evaluate the function to be
C              minimized.  The usage is
C              CALL FCN (N, X, F), where
C              N      - Length of X.  (Input)
C              X      - Vector of length N at which point the function
C                       is evaluated.  (Input)
C                       X should not be changed by FCN.
C              F      - The computed function value at the point X.
C                       (Output)
C              FCN must be declared EXTERNAL in the calling program.
C     GRAD   - User-supplied SUBROUTINE to compute the gradient at the
C              point X.  The usage is
C              CALL GRAD (N, X, G), where
C              N      - Length of X and G.  (Input)
C              X      - Vector of length N at which point the gradient
C                       is evaluated.  (Input)
C                       X should not be changed by GRAD.
C              G      - The gradient evaluated at the point X.
C                       (Output)
C              GRAD must be declared EXTERNAL in the calling program.
C     HESS   - User-supplied SUBROUTINE to compute the Hessian at the
C              point X.  The usage is
C              CALL HESS (N, X, H, LDH), where
C              N      - Length of X.  (Input)
C              X      - Vector of length N at which point the Hessian
C                       is evaluated.  (Input)
C                       X should not be changed by HESS.
C              H      - The Hessian evaluated at the point X.
C                       (Output)
C              LDH    - Leading dimension of H exactly as specified in
C                       in the dimension statement of the calling
C                       program.  (Input)
C              HESS must be declared EXTERNAL in the calling program.
C     N      - Dimension of the problem.  (Input)
C     XGUESS - Vector of length N containing initial guess.  (Output)
C     XSCALE - Vector of length N containing the diagonal scaling matrix
C              for the variables.  (Input)
C              In the absence of other information, set all entries to
C              1.0.
C     FSCALE - Scalar containing the function scaling.  (Input)
C              In the absence of other information, set FSCALE to 1.0.
C     IPARAM - Parameter vector of length 7.  (Input/Output)
C              See Remarks.
C     RPARAM - Parameter vector of length 7.  (Input/Output)
C              See Remarks.
C     X      - Vector of length N containing the computed solution.
C              (Output)
C     FVALUE - Scalar containing the value of the function at the
C              computed solution.  (Output)
C
C  Remarks:
C  1. Automatic workspace usage is
C              UMIAH     N*(N+9)  units, or
C              DUMIAH    2*N*(N+9) units.
C     Workspace may be explicitly provided, if desired, by use of
C     U2IAH/DU2IAH.  The reference is
C              CALL U2IAH (FCN, GRAD, HESS, N, XGUESS, XSCALE, FSCALE,
C                          IPARAM, RPARAM, X, FVALUE, WK)
C     The additional argument is
C     WK     - Real work vector of length N*(N+9).  WK contains the
C              the following information on output:
C              The second N locations contain the last step taken.
C              The third N locations contain the last Newton step.
C              The fourth N locations contain an estimate of the
C                  gradient at the solution.
C              The final N*N locations contain the Hessian at the
C                  approximate solution.
C
C  2. Informational errors
C     Type Code
C       3   1  Both the actual and predicted relative reductions in the
C              function are less than or equal to the relative function
C              convergence tolerance.
C       4   2  The iterates appear to be converging to a noncritical
C              point.
C       4   3  Maximum number of iterations exceeded.
C       4   4  Maximum number of function evaluations exceeded.
C       4   5  Maximum number of gradient evaluations exceeded.
C       4   6  Five consecutive steps have been taken with the maximum
C              step length.
C       4   7  Maximum number of gradient evaluations exceeded.
C
C  3. The first stopping criterion for UMIAH occurs when the norm of the
C     gradient is less than the given gradient tolerance (RPARAM(1)).
C     The second stopping criterion for UMIAH occurs when the scaled
C     distance between the last two steps is less than the step
C     tolerance (RPARAM(2)).
C
C  4. If the default parameters are desired for UMIAH, then set
C     IPARAM(1) to zero and call the optimization program omitting the
C     call to U4INF.  Otherwise, if the nondefault parameters are
C     desired for IPARAM or RPARAM, then U4INF is called and the
C     corresponding parameters are set to the desired value before
C     calling the optimization program.  The use of U4INF would be as
C     follows:
C
C              CALL U4INF (IPARAM, RPARAM).
C              Set the desired parameters.
C
C     The following is a list of the parameters and the default values:
C     IPARAM - Integer vector of length 7.
C              IPARAM(1) = Initialization flag. (0)
C              IPARAM(2) = Number of good digits in the function.
C                          (Machine dependent)
C              IPARAM(3) = Maximum number of iterations. (100)
C              IPARAM(4) = Maximum number of function evaluations. (400)
C              IPARAM(5) = Maximum number of gradient evaluations. (400)
C              IPARAM(6) = Hessian initialization parameter. (0)
C                          Not required when using UMIAH.
C              IPARAM(7) = Maximum number of Hessian evaluations. (100)
C     RPARAM - Real vector of length 7.
C              RPARAM(1) = Scaled gradient tolerance. (eps**(2/3))
C              RPARAM(2) = Scaled step tolerance. (eps**(2/3))
C              RPARAM(3) = Relative function tolerance.
C                          (MAX(1.0E-10,eps**(2/3)))
C              RPARAM(4) = Absolute function tolerance.
C                          (MAX(1.0E-10,eps**(2/3)))
C              RPARAM(5) = False convergence tolerance. (100*eps)
C              RPARAM(6) = Maximum allowable step size.
C                          (1000*MAX(TOL1,TOL2)) where,
C                          TOL1 = SQRT(sum of (XSCALE(I)*XGUESS(I))**2)
C                                 for I = 1,...,N
C                          TOL2 = 2-norm of XSCALE.
C              RPARAM(7) = Size of initial trust region radius.
C                          (Based on the initial scaled Cauchy step)
C     eps is machine epsilon.
C     If double precision is desired, then DU4INF is called and RPARAM
C     is declared double precision.
C
C  Keywords:   Locally constrained optimal step; Trust region
C
C  GAMS:       G1a1c
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
      SUBROUTINE DUMIAH (FCN, GRAD, HESS, N, XGUESS, XSCALE, FSCALE,
     &                   IPARAM, RPARAM, X, FVALUE)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, IPARAM(*)
      DOUBLE PRECISION FSCALE, FVALUE, XGUESS(*), XSCALE(*),
     &           RPARAM(*), X(*)
      EXTERNAL   FCN, GRAD, HESS
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    INDR
C                                  SPECIFICATIONS FOR SPECIAL CASES
C                                  SPECIFICATIONS FOR COMMON /WORKSP/
      REAL       RWKSP(35000)
      DOUBLE PRECISION RDWKSP(17500)
      DOUBLE PRECISION DWKSP(17500)
      COMPLEX    CWKSP(17500)
      COMPLEX    *16 CZWKSP(8750)
      COMPLEX    *16 ZWKSP(8750)
      INTEGER    IWKSP(35000)
      LOGICAL    LWKSP(35000)
      EQUIVALENCE (DWKSP(1), RWKSP(1))
      EQUIVALENCE (CWKSP(1), RWKSP(1)), (ZWKSP(1), RWKSP(1))
      EQUIVALENCE (IWKSP(1), RWKSP(1)), (LWKSP(1), RWKSP(1))
      EQUIVALENCE (RDWKSP(1), RWKSP(1)), (CZWKSP(1), RWKSP(1))
      COMMON     /WORKSP/ DWKSP
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, DU2IAH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   I1KGT, N1RCD
      INTEGER    I1KGT, N1RCD
C
      CALL E1PSH ('DUMIAH ')
C
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'The number of variables must be '//
     &               'positive while N = %(I1) is given.')
      ELSE
C                                  ALLOCATE WORKSPACE
         INDR = I1KGT(N*(N+9),4)
         IF (N1RCD(0) .NE. 0) THEN
            CALL E1MES (5, 2, ' ')
            CALL E1STI (1, N)
            CALL E1MES (5, 2, 'The workspace allocation is based on '//
     &                  'N while N = %(I1) is given.')
         ELSE
            CALL DU2IAH (FCN, GRAD, HESS, N, XGUESS, XSCALE, FSCALE,
     &                   IPARAM, RPARAM, X, FVALUE, RDWKSP(INDR))
         END IF
      END IF
C
      CALL E1POP ('DUMIAH ')
      RETURN
      END
