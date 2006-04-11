C-----------------------------------------------------------------------
C  IMSL Name:  U2IAH/DU2IAH (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Minimize a function of N variables using a modified
C              Newton method and a user-supplied Hessian.
C
C  Usage:      CALL U2IAH (FCN, GRAD, HESS, N, XGUESS, XSCALE, FSCALE,
C                          IPARAM, RPARAM, X, FVALUE, WK)
C
C  Arguments:  See UMIAH/DUMIAH.
C
C  Remarks:    See UMIAH/DUMIAH.
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
      SUBROUTINE DU2IAH (FCN, GRAD, HESS, N, XGUESS, XSCALE, FSCALE,
     &                   IPARAM, RPARAM, X, FVALUE, WK)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, IPARAM(*)
      DOUBLE PRECISION FSCALE, FVALUE, XGUESS(*), XSCALE(*),
     &           RPARAM(*), X(*), WK(*)
      EXTERNAL   FCN, GRAD, HESS
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, DCOPY, DU3IAH
C
      CALL E1PSH ('DU2IAH ')
C
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
         CALL E1MES (5, 1, 'The number of variables must be '//
     &               'positive while N = %(I1) is given.')
      ELSE
C                                  CALL UNCONSTRAINED MINIMIZATION
C                                  SOLVER USING ANALYTIC HESSIAN
         CALL DCOPY (N, XGUESS, 1, X, 1)
         CALL DU3IAH (FCN, GRAD, HESS, N, X, XSCALE, FSCALE, IPARAM,
     &                RPARAM, FVALUE, WK(1), WK(N+1), WK(2*N+1),
     &                WK(3*N+1), WK(4*N+1), WK(9*N+1), N, WK(8*N+1),
     &                WK(5*N+1), WK(6*N+1), WK(7*N+1))
      END IF
C
      CALL E1POP ('DU2IAH ')
      RETURN
      END
