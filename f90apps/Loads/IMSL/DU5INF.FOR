C-----------------------------------------------------------------------
C  IMSL Name:  U5INF/DU5INF  (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Check validity of input to unconstrained minimization.
C
C  Usage:      CALL U5INF (N, XC, XSCALE, FSCALE, USRHES, IPARAM,
C                          RPARAM)
C
C  Arguments:
C     N      - Size of the problem.  (Input)
C     XC     - Vector of length N containing the initial point.  (Input)
C     XSCALE - Vector of length N containing the diagonal scaling matrix
C              for the variables.  (Input/Output)
C     FSCALE - Estimate of the scale of the objective function.
C              (Input/Output)
C     USRHES - Logical variable.  (Input)
C              USRHES = .TRUE. if analytic Hessian or finite difference
C                       Hessian is used.
C              USRHES = .FALSE. otherwise.
C     IPARAM - Integer parameters vector of length 6.  (Input/Output)
C              See UMINF or UMIDH for details.
C     RPARAM - Real parameters vector of length 7.  (Input/Output)
C              See UMINF or UMIDH for details.
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
      SUBROUTINE DU5INF (N, XC, XSCALE, FSCALE, USRHES, IPARAM, RPARAM)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, IPARAM(*)
      DOUBLE PRECISION FSCALE, XC(*), XSCALE(*), RPARAM(*)
      LOGICAL    USRHES
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
      DOUBLE PRECISION MACHEP, TEMP, TEMP1, TEMP2
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DMAX1,DSQRT
      INTRINSIC  DMAX1, DSQRT
      DOUBLE PRECISION DMAX1, DSQRT
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1MES, E1POP, E1PSH, E1STI, E1STD, DSET, DU19NF
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, IMACH, DNRM2
      INTEGER    IMACH
      DOUBLE PRECISION DMACH, DNRM2
C
      CALL E1PSH ('DU5INF ')
      MACHEP = DMACH(4)
C                                  FDIGIT  good digits in F
C                                  MXITER  Min number of iterations
C                                  MAXFCN  Max fcn. evaluations
C                                  MAXGRD  Max grad. evaluations
C                                  IHESS   Hessian initial. parameter
C                                  MAXHES  Max Hess. evaluations
C                                  GRADTL  Scaled gradient tolerance
C                                  STEPTL  Scaled step tolerance
C                                  RFTOL   Relative function tolerance
C                                  AFTOL   Absolute function tolerance
C                                  FALSTL  False convergence tolerance
C                                  STEPMX  Maximum allowable step size
C                                  DELTA   Size of initial trust region
      IF (N .LE. 0) THEN
         CALL E1STI (1, N)
C                                  Print error message
         CALL DU19NF (0)
CC         CALL E1MES (5, 1, 'The size of the problem must be '//
CC     &               'positive while N = %(I1) is given.')
         GO TO 9000
      ELSE IF (N .EQ. 1) THEN
C                                  Print error message
         CALL DU19NF (1)
CC         CALL E1MES (6, 1, 'This routine may be inefficient '//
CC     &               'for a problem of size N = 1.')
      END IF
C                                  CHECK VARIABLE SCALING MATRIX
      DO 10  I=1, N
         IF (XSCALE(I) .LE. 0.0D0) GO TO 20
   10 CONTINUE
      GO TO 30
C                                  Print error message
   20 CALL DU19NF (2)
CC   20 CALL E1MES (6, 2, 'The diagonal scaling matrix for '//
CC     &            'the variables must be positive while some '//
CC     &            'of the entries are less than or equal to zero.'//
CC     &            '  The algorithm will use the identity scaling '//
CC     &            'matrix for XSCALE.')
      CALL DSET (N, 1.0D0, XSCALE, 1)
C                                  CHECK FUNCTION SCALING
   30 IF (FSCALE .LE. 0.0D0) THEN
         CALL E1STD (1, FSCALE)
         CALL E1MES (6, 3, 'The estimate of the scale of the '//
     &               'objective function must be positive while '//
     &               'FSCALE = %(D1) is given.  The algorithm will '//
     &               'use FSCALE = 1.0.')
         FSCALE = 1.0D0
      END IF
C                                  CHECK ACCURACY OF THE PROBLEM
      IF (IPARAM(2) .LE. 0) THEN
         CALL E1STI (1, IPARAM(2))
C                                  Print error message
         CALL DU19NF (4)
CC         CALL E1MES (6, 4, 'The estimate of the number of '//
CC     &               'good digits in the function must be positive '//
CC     &               'while FDIGIT = %(I1) is given.  The algorithm'//
CC     &               ' will assume that the function is accurate to'//
CC     &               ' the precision of the arithmetic.')
         IPARAM(2) = IMACH(10)
      END IF
C                                  CHECK MAXIMUM NUMBER OF ITERATIONS
      IF (IPARAM(3) .LE. 0) THEN
         CALL E1STI (1, IPARAM(3))
C                                  Print error message
         CALL DU19NF (5)
CC         CALL E1MES (6, 5, 'The maximum number of iterations '//
CC     &               'must be positive while MXITER = %(I1) is '//
CC     &               'given.  The algorithm will use MXITER = 100.')
         IPARAM(3) = 100
      END IF
C                                  CHECK MAXIMUM FUNCTION EVALUATIONS
      IF (IPARAM(4) .LE. 0) THEN
         CALL E1STI (1, IPARAM(4))
C                                  Print error message
         CALL DU19NF (6)
CC         CALL E1MES (6, 6, 'The maximum number of function '//
CC     &               'evaluations must be positive while MAXFCN = '//
CC     &               '%(I1) is given.  The algorithm will use '//
CC     &               'MAXFCN = 400.')
         IPARAM(4) = 400
      END IF
C                                  CHECK MAXIMUM GRADIENT EVALUATIONS
      IF (IPARAM(5) .LE. 0) THEN
         CALL E1STI (1, IPARAM(5))
C                                  Print error message
         CALL DU19NF (7)
CC         CALL E1MES (6, 7, 'The maximum number of gradient '//
CC     &               'evaluations must be positive while MAXGRD = '//
CC     &               '%(I1) is given.  The algorithm will use '//
CC     &               'MAXGRD = 400.')
         IPARAM(5) = 400
      END IF
C                                  CHECK MAXIMUM HESSIAN EVALUATIONS IF
C                                  A NEWTON METHOD IS USED
      IF (USRHES) THEN
         IF (IPARAM(7) .LE. 0) THEN
            CALL E1STI (1, IPARAM(7))
C                                  Print error message
            CALL DU19NF (8)
CC            CALL E1MES (6, 8, 'The maximum number of Hessian '//
CC     &                  'evaluations must be positive while MAXHES '//
CC     &                  '= %(I1) is given.  The algorithm will use '//
CC     &                  'MAXHES = 100.')
            IPARAM(7) = 100
         END IF
      END IF
C
      TEMP1 = MACHEP**(2.0D0/3.0D0)
      TEMP2 = MACHEP**(2.0D0/3.0D0)
C                                  CHECK THE GRADIENT TOLERANCE
      IF (RPARAM(1) .LT. 0.0D0) THEN
         CALL E1STD (1, RPARAM(1))
         CALL E1STD (2, TEMP1)
         CALL E1MES (6, 9, 'The gradient tolerance must be '//
     &               'nonnegative while GRADTL = %(D1) is given.  '//
     &               'The algorithm will use GRADTL = %(D2).')
         RPARAM(1) = TEMP1
      END IF
C                                  CHECK THE STEP TOLERANCE
      IF (RPARAM(2) .LT. 0.0D0) THEN
         CALL E1STD (1, RPARAM(2))
         CALL E1STD (2, TEMP2)
         CALL E1MES (6, 10, 'The step tolerance must be nonnegative '//
     &               'while STEPTL = %(D1) is given.  The algorithm '//
     &               'will use STEPTL = %(D2).')
         RPARAM(2) = TEMP2
      END IF
C                                  CHECK RELATIVE FUNCTION TOLERANCE
      IF (RPARAM(3) .LT. 0.0D0) THEN
         TEMP = DMAX1(1.0D-10,TEMP2)
         CALL E1STD (1, RPARAM(3))
         CALL E1STD (2, TEMP)
         CALL E1MES (6, 11, 'The relative function tolerance '//
     &               'must be nonnegative while RFTOL = %(D1) is '//
     &               'given.  The algorithm will use RFTOL = %(D2).')
         RPARAM(3) = TEMP
      END IF
C                                  CHECK ABSOLUTE FUNCTION TOLERANCE
      IF (RPARAM(4) .LT. 0.0D0) THEN
         TEMP = DMAX1(1.0D-20,MACHEP*MACHEP)
         CALL E1STD (1, RPARAM(4))
         CALL E1STD (2, TEMP)
         CALL E1MES (6, 12, 'The absolute function tolerance '//
     &               'must be nonnegative while AFTOL = %(D1) is '//
     &               'given.  The algorithm will use AFTOL = %(D2).')
         RPARAM(4) = TEMP
      END IF
C                                  CHECK FALSE CONVERGENCE TOLERANCE
      IF (RPARAM(5) .LT. 0.0D0) THEN
         TEMP = 1.0D2*MACHEP
         CALL E1STD (1, RPARAM(5))
         CALL E1STD (2, TEMP)
         CALL E1MES (6, 13, 'The false convergence tolerance '//
     &               'must be nonnegative while FALSTL = %(D1) is '//
     &               'given.  The algorithm will use FALSTL = %(D2).')
         RPARAM(5) = TEMP
      END IF
C                                  CHECK MAXIMUM ALLOWED STEP SIZE
      IF (RPARAM(6) .LE. 0.0D0) THEN
         TEMP1 = 0.0D0
         DO 40  I=1, N
            TEMP1 = TEMP1 + (XSCALE(I)*XC(I))**2
   40    CONTINUE
         TEMP1 = DSQRT(TEMP1)
         TEMP2 = DNRM2(N,XSCALE,1)
         TEMP = 1.0D3*DMAX1(TEMP1,TEMP2)
         IF (IPARAM(1).NE.0 .AND. RPARAM(6).NE.-9999.0D0) THEN
            CALL E1STD (1, RPARAM(6))
            CALL E1STD (2, TEMP)
            CALL E1MES (6, 14, 'The maximum allowable scaled '//
     &                  'step length must be positive while STEPMX = '//
     &                  '%(D1) is given.  The algorithm will use '//
     &                  'STEPMX = %(D2).')
         END IF
         RPARAM(6) = TEMP
      END IF
C                                  CHECK INITIAL TRUST REGION RADIUS
      IF (RPARAM(7) .LE. 0.0D0) THEN
         IF (IPARAM(1).NE.0 .AND. RPARAM(7).NE.-9999.0D0) THEN
            CALL E1STD (1, RPARAM(7))
            CALL E1MES (6, 15, 'The initial trust region '//
     &                  'radius must be positive while DELTA = %(D1) '//
     &                  'is given.  The algorithm will use the '//
     &                  'length of the initial scaled Cauchy step '//
     &                  'for DELTA.')
         END IF
         RPARAM(7) = -1.0D0
      END IF
C
 9000 CALL E1POP ('DU5INF ')
      RETURN
      END
