C-----------------------------------------------------------------------
C  IMSL Name:  U7IAH/DU7IAH  (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Updating the model trust region for the minimization
C              problem.
C
C  Usage:      CALL U7IAH (FCN, N, XC, FC, GC, A, LDA, SC, XSCALE,
C                          NEWTON, STEPMX, ISTEP, HDIAG, DELTA, ICODE,
C                          XPPREV, XP, FP, MXTAKE, NFCN)
C
C  Arguments:
C     FCN    - A user supplied subroutine to evaluate the function at a
C              point X.  FCN must be declared EXTERNAL in the calling
C              program and must have the form,
C                 SUBROUTINE FCN (N, X, F)
C                 INTEGER    N
C                 REAL       X(*), F
C                    .
C                    .
C                 RETURN
C                 END
C              X should not be changed by FCN.
C     N      - Dimension of the problem.  (Input)
C     XC     - Vector of length N containing the current iterate.
C              (Input)
C     FC     - Scalar containing the function value at XC.  (Input)
C     GC     - Vector of length N containing the gradient at XC.
C              (Input)
C     A      - N by N matrix containing the Cholesky factorization
C              of the Hessian in the lower triangular part and diagonal.
C              If ISTEP = 1, the Hessian is also contained in the upper
C              triangular part and HDIAG.  (Input)
C     LDA    - Row dimension of A exactly as specified in the dimension
C              statement of the calling program.  (Input)
C     SC     - Vector of length N containing current step.  (Input)
C     XSCALE - Vector of length N containing the diagonal scaling
C              matrix for X.  (Input)
C     NEWTON - Logical variable equal to .TRUE. when the full Newton
C              step is taken.  (Input)
C     STEPMX - Maximum allowable step size.  (Input)
C     ISTEP  - Integer flag indicating the type of step used.  (Input)
C              ISTEP = 1 for locally constrained optimal step
C              ISTEP = 2 for double dogleg step.
C     HDIAG  - Vector of length N containing the diagonal of the
C              model Hessian (only referenced when ISTEP = 1).  (Input)
C     DELTA  - Trust region radius with value retainied between calls.
C              (Input/Output)
C     ICODE  - Return code.  (Output)
C              ICODE = 0 means XP accepted as next iterate, DELTA is
C                        trust region for next iteration.
C              ICODE = 1 means the algorithm was unable to find a
C                        satisfactory XP sufficiently distinct from XC.
C              ICODE = 2 means both the scaled actual and predicted
C                        function reductions are smaller than RFTOL.
C              ICODE = 3 means that false convergence is detected.
C              ICODE = 4 means FP is too large, current iteration is
C                        continued with a new, reduced trust region.
C              ICODE = 5 means FP is sufficiently small, but the chance
C                        of taking a longer successful step seems good
C                        that the current iteration is to be continued
C                        with a new, doubled trust region.
C     XPPREV - Vector of length N containing previous XP.
C              (Input/Output)
C     XP     - Vector of length N containing the new iterate.
C              (Output)
C     FP     - Function value at XP.  (Output)
C     MXTAKE - Logical variable.  (Output)
C              MXTAKE = .TRUE. if a step of maximum length was taken.
C              MXTAKE = .FALSE. otherwise.
C     NFCN   - Number of function evaluations used.  (Input/Output)
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
      SUBROUTINE DU7IAH (FCN, N, XC, FC, GC, A, LDA, SC, XSCALE,
     &                   NEWTON, STEPMX, ISTEP, HDIAG, DELTA, ICODE,
     &                   XPPREV, XP, FP, MXTAKE, NFCN, PREDUC)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDA, ISTEP, ICODE, NFCN
      DOUBLE PRECISION FC, STEPMX, DELTA, FP, PREDUC, XC(*), GC(*),
     &           A(LDA,*), SC(*), XSCALE(*), HDIAG(*), XPPREV(*), XP(*)
      LOGICAL    NEWTON, MXTAKE
      EXTERNAL   FCN
C                                  SPECIFICATIONS FOR PARAMETERS
      DOUBLE PRECISION ALPHA
      PARAMETER  (ALPHA=1.0D-4)
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I
      DOUBLE PRECISION ACTRED, PRERED, RELLEN, SLOPE, STPLEN, TEMP
      LOGICAL    LTEMP
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      DOUBLE PRECISION FPPREV
      SAVE       FPPREV
C                                  SPECIFICATIONS FOR COMMON /DU16NF/
      COMMON     /DU16NF/ GRADTL, STEPTL, RFTOL, AFTOL, FALSTL,
     &           MXITER, MAXFCN, MAXGRD, MAXHES
      INTEGER    MXITER, MAXFCN, MAXGRD, MAXHES
      DOUBLE PRECISION GRADTL, STEPTL, RFTOL, AFTOL, FALSTL
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DMAX1,DMIN1
      INTRINSIC  DABS, DMAX1, DMIN1
      DOUBLE PRECISION DABS, DMAX1, DMIN1
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   E1USR, DCOPY, DU11NF
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DDOT, DNRM2
      DOUBLE PRECISION DDOT, DNRM2
C
      MXTAKE = .FALSE.
      CALL DU11NF (N, XSCALE, 1, SC, XP)
      STPLEN = DNRM2(N,XP,1)
C                                  COMPUTE NEW TRIAL POINT AND NEW
C                                    FUNCTION VALUE
      DO 10  I=1, N
         XP(I) = XC(I) + SC(I)
   10 CONTINUE
      CALL E1USR ('ON')
      CALL FCN (N, XP, FP)
      CALL E1USR ('OFF')
      NFCN = NFCN + 1
      ACTRED = FP - FC
      SLOPE = DDOT(N,GC,1,SC,1)
C
      IF (ICODE .NE. 5) FPPREV = 0.0D0
      IF (ICODE.EQ.5 .AND. ((FP.GE.FPPREV).OR.(ACTRED.GT.ALPHA*SLOPE))
     &    ) THEN
C                                  INTERNAL DOUBLING NO GOOD; RESET XP
C                                    TO XPPREV AND TERMINATE
         ICODE = 0
         CALL DCOPY (N, XPPREV, 1, XP, 1)
         FP = FPPREV
         DELTA = 0.5D0*DELTA
      ELSE IF (ACTRED .GE. ALPHA*SLOPE) THEN
C                                  FP IS TOO LARGE; THE STEP IS
C                                    UNACCEPTABLE
         RELLEN = 0.0D0
         DO 20  I=1, N
            TEMP = DABS(SC(I))/DMAX1(DABS(XP(I)),1.0D0/XSCALE(I))
            RELLEN = DMAX1(RELLEN,TEMP)
   20    CONTINUE
         IF (RELLEN .LT. STEPTL) THEN
C                                  XP - XC IS TOO SMALL, TERMINATE THE
C                                    GLOBAL STEP
            ICODE = 1
            CALL DCOPY (N, XC, 1, XP, 1)
         ELSE
C                                  QUADRATIC INTERPOLATION STEP; REDUCE
C                                    DELTA AND CONTINUE
            ICODE = 4
            DELTA = DMIN1(DELTA,STPLEN)
            TEMP = (-SLOPE*STPLEN)/(2.0D0*(ACTRED-SLOPE))
            IF (TEMP .LT. 0.1D0*DELTA) THEN
               DELTA = 0.1D0*DELTA
            ELSE IF (TEMP .GT. 0.5D0*DELTA) THEN
               DELTA = 0.5D0*DELTA
            ELSE
               DELTA = TEMP
            END IF
         END IF
      ELSE
C                                  FP IS SUFFICIENTLY SMALL; THE STEP
C                                    IS ACCEPTABLE COMPUTE PREDICTED
C                                    REDUCTION PRERED = G(t)*S +
C                                    (1/2)*S(t)*H*S
         PRERED = -PREDUC
CX         IF (ISTEP .EQ. 1) THEN
CX            DO 30  I=1, N
CX               TEMP   = SDOT(I-1,A(1,I),1,SC,1)
CX               TEMP   = TEMP + HDIAG(I)*SC(I)
CX               TEMP   = TEMP + SDOT(N-I,A(I,I+1),LDA,SC(I+1),1)
CX               TEMP   = 0.5E0*SC(I)*TEMP
CX               PRERED = PRERED + TEMP
CX   30       CONTINUE
CX         ELSE
CX            DO 40  I=1, N
CX               TEMP   = SDOT(N-I+1,A(I,I),1,SC(I),1)
CX               PRERED = PRERED + (0.5E0*TEMP*TEMP)
CX   40       CONTINUE
CX         END IF
C
         LTEMP = DABS(PRERED-ACTRED).LE.0.1D0*DABS(ACTRED)
         IF (ICODE.NE.4 .AND. (LTEMP.OR.(ACTRED.LE.SLOPE)) .AND.
     &       .NOT.NEWTON .AND. (DELTA.LE.0.99D0*STEPMX)) THEN
C                                  IF ACTRED AND PRERED AGREE TO WITHIN
C                                    RELATIVE ERROR 0.1 OR NEGATIVE
C                                    CURVATURE IS INDICATED, AND A
C                                    LONGER STEP IS POSSIBLE AND DELTA
C                                    HAS NOT BEEN DECREASED THIS
C                                    ITERATION, THEN DOUBLE TRUST
C                                    REGION AND CONTINUE GLOBAL STEP
            ICODE = 5
            CALL DCOPY (N, XP, 1, XPPREV, 1)
            FPPREV = FP
            DELTA = DMIN1(2.0D0*DELTA,STEPMX)
         ELSE
C                                  ACCEPT XP AND CHOOSE NEW TRUST
C                                    REGION FOR NEXT ITERATION
            ICODE = 0
            IF (STPLEN .GT. 0.99D0*STEPMX) MXTAKE = .TRUE.
            IF (ACTRED .GE. 0.1D0*PRERED) THEN
               DELTA = 0.5D0*DELTA
            ELSE IF (ACTRED .LE. 0.75D0*PRERED) THEN
               DELTA = DMIN1(2.0D0*DELTA,STEPMX)
            ELSE
C                                  DELTA = DELTA !* KEEP SAME DELTA
            END IF
         END IF
C                                  CHECK RELATIVE FUNCTION CONVERGENCE
C                                    AND FALSE CONVERGENCE
         IF (ACTRED .LE. 2.0D0*PRERED) THEN
            IF (DABS(ACTRED).LE.RFTOL*DABS(FC) .AND.
     &          DABS(PRERED).LE.RFTOL*DABS(FC)) THEN
               ICODE = 2
            END IF
         ELSE
            RELLEN = 0.0D0
            DO 30  I=1, N
               TEMP = DABS(SC(I))/DMAX1(DABS(XP(I)),1.0D0/XSCALE(I))
               RELLEN = DMAX1(RELLEN,TEMP)
   30       CONTINUE
            IF (RELLEN .LT. FALSTL) ICODE = 3
         END IF
      END IF
C
      RETURN
      END
