C-----------------------------------------------------------------------
C  IMSL Name:  U4INF/DU4INF  (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Provide default parameters for the minimization problem.
C
C  Usage:      CALL U4INF (IPARAM, RPARAM)
C
C  Arguments:
C     IPARAM - Integer parameters vector of length 6.  (Output)
C              IPARAM(1) = initialization flag
C              IPARAM(2) = number of good digits in the function
C              IPARAM(3) = maximum number of iterations.
C              IPARAM(4) = maximum number of function evaluations
C              IPARAM(5) = maximum number of gradient evaluations
C              IPARAM(6) = initialization of Hessian parameter
C              IPARAM(7) = maximum number of Hessian evaluations
C     RPARAM - Integer parameters vector of length 7.  (Output)
C              RPARAM(1) = scaled gradient tolerance
C              RPARAM(2) = scaled step tolerance
C              RPARAM(3) = relative function tolerance
C              RPARAM(4) = absolute function tolerance
C              RPARAM(5) = false convergence tolerance
C              RPARAM(6) = maximum allowable step size
C              RPARAM(7) = size of initial trust region radius
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
      SUBROUTINE DU4INF (IPARAM, RPARAM)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    IPARAM(*)
      DOUBLE PRECISION RPARAM(*)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      DOUBLE PRECISION ARFTOL, EPS, OV3, TV3
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DMAX1
      INTRINSIC  DMAX1
      DOUBLE PRECISION DMAX1
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, IMACH
      INTEGER    IMACH
      DOUBLE PRECISION DMACH
C
      EPS = DMACH(4)
      OV3 = 1.0D0/3.0D0
      TV3 = 2.0D0/3.0D0
C                                  Set up integer parameters
      IPARAM(1) = 1
      IPARAM(2) = IMACH(10)
      RPARAM(1) = EPS**OV3
      ARFTOL = 1.0D-20
      IPARAM(3) = 100
      IPARAM(4) = 400
      IPARAM(5) = 400
      IPARAM(6) = 0
      IPARAM(7) = 100
C                                  Set up real parameters
      RPARAM(2) = EPS**TV3
      RPARAM(3) = DMAX1(ARFTOL,EPS**TV3)
      RPARAM(4) = DMAX1(ARFTOL,EPS*EPS)
      RPARAM(5) = 1.0D2*EPS
      RPARAM(6) = -9999.0D0
      RPARAM(7) = -9999.0D0
C
      RETURN
      END
