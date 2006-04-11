C-----------------------------------------------------------------------
C  IMSL Name:  U8IAH/DU8IAH (Single/Double precision version)
C
C  Computer:   prime/DOUBLE
C
C  Revised:    September 16, 1985
C
C  Purpose:    Compute optimal locally constrained step
C
C  Usage:      CALL U8IAH (N, GC, H, LDH, D, HDIAG, STEPMX, RAD, KA,
C                          NEWTON, LTRY1, SC, PREDUC, WK1, WK2, WK3)
C  Arguments:
C     N      - Dimension of the problem.  (Input)
C     H      - Real N by N matrix.  (Input/Output)
C              On input, H is the symmetric positive definite matrix to
C                 be factored (with only the lower triangular part and
C                 the diagonal stored).
C              On output, H contains L of the Cholesky factorization of
C                 the perturbed matrix in the lower triangular part and
C                 diagonal and the perturbed matrix in the upper triang-
C                 ular part and DIAG respectively.
C     LDH    - Row dimension of H exactly as specified in the dimension
C              statement of the calling program.  (Input)
C     D      - Real vector of length N containing the diagonal scaling
C              matrix.  (Input)
C     HDIAG  - Real vector of length N containing the diagonal of the
C              perturbed symmetric positive definite matrix.  (Output)
C     STEPMX - Maximum allowable step size.  (Input)
C     RAD    - Current trust region radius.  (Input/Output)
C     KA     - Integer scalar indicating initial or restart call to
C              this routine.  (Input/Output)
C     NEWTON - Logical variable indicating newton step being taken
C              or not.  (Output)
C     LTRY1   - Logical variable indicating if entering U8IAH for the
C              first time.  (Input/Output)
C     SC     - Real vector of length N containing the computed step.
C                (Output)
C     PREDUC - Predicted reduction of function.  (Output)
C     WK1    - Real work vector of length N.
C     WK2    - Real work vector of length N.
C     WK3    - Real work vector of length N.
C
C  Remark:
C     This is based on NL2SOL routine GQTSTP.
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
      SUBROUTINE DU8IAH (N, GC, H, LDH, D, HDIAG, STEPMX, RAD, KA,
     &                   NEWTON, LTRY1, SC, PREDUC, WK1, WK2, WK3)
C                                  SPECIFICATIONS FOR ARGUMENTS
      INTEGER    N, LDH, KA
      DOUBLE PRECISION STEPMX, RAD, PREDUC, GC(*), H(LDH,*), D(*),
     &           HDIAG(*), SC(*), WK1(*), WK2(*), WK3(*)
      LOGICAL    NEWTON, LTRY1
C                                  SPECIFICATIONS FOR LOCAL VARIABLES
      INTEGER    I, INDEX, IRC, J, K, KALIM
      DOUBLE PRECISION AKI, AKK, ALK, ALOW, ALPHAK, BETA, DELTA,
     &           DGXFAC, DST, EPSFAC, EPSLON, EPSO6, HIGH, KAPPA,
     &           OLDPHI, PHI, PSIFAC, ROOT, SI, SK, SW, T, T1, TEMP,
     &           THETA, TWOPSI, UK
      LOGICAL    RESTRT
C                                  SPECIFICATIONS FOR SAVE VARIABLES
      DOUBLE PRECISION ALK0, DGGDMX, DGNORM, DST0, DSTNRM, DSTSAV,
     &           EMAX, EMIN, GTSTEP, PHIPIN, RAD0, REDUC, STPPAR, UK0
      SAVE       ALK0, DGGDMX, DGNORM, DST0, DSTNRM, DSTSAV, EMAX,
     &           EMIN, GTSTEP, PHIPIN, RAD0, REDUC, STPPAR, UK0
C                                  SPECIFICATIONS FOR INTRINSICS
C     INTRINSIC  DABS,DMAX1,DMIN1,DSQRT
      INTRINSIC  DABS, DMAX1, DMIN1, DSQRT
      DOUBLE PRECISION DABS, DMAX1, DMIN1, DSQRT
C                                  SPECIFICATIONS FOR SUBROUTINES
      EXTERNAL   DCOPY, DSCAL, DSET, DVCAL, DCSFRG, DU13NF, DU14NF,
     &           DU9IAH
C                                  SPECIFICATIONS FOR FUNCTIONS
      EXTERNAL   DMACH, IDAMAX, DDOT, DNRM2, DU10AH
      INTEGER    IDAMAX
      DOUBLE PRECISION DMACH, DDOT, DNRM2, DU10AH
C
      DATA STPPAR/0.0D0/
C                                  Set up initial parameters and
C                                  D**(-1)*G.
      IF (LTRY1) RAD0 = 0.0D0
      HIGH = 1.1D0
      ALOW = .9D0
      EPSLON = 0.1D0
      EPSFAC = 50.0D0
      KAPPA = 2.0D0
      DGXFAC = 0.0D0
      NEWTON = .FALSE.
      LTRY1 = .FALSE.
      DO 10  J=1, N
         GC(J) = GC(J)/D(J)
   10 CONTINUE
      DGNORM = DNRM2(N,GC,1)
C                                  OLDPHI IS USED TO DETECT LIMITS OF
C                                  NUMERICAL ACCURACY. IF WE
C                                  RECOMPUTE STEP AND IT DOES NOT
C                                  CHANGE, THEN WE ACCEPT IT.
      OLDPHI = 0.0D0
      EPSO6 = EPSLON/6.0D0
      IRC = 0
      RESTRT = .FALSE.
      KALIM = KA + 50
C                                  START OR RESTART, DEPENDING ON KA
      IF (KA .GE. 0) GO TO 220
C                                  FRESH START
      K = 0
      UK = -1.0D0
      KA = 0
      KALIM = 50
C                                  STORE DIAG(H) IN HDIAG
      CALL DCOPY (N, H, LDH+1, HDIAG, 1)
      DO 20  I=1, N - 1
         CALL DCOPY (N-I, H(I+1,I), 1, H(I,I+1), LDH)
   20 CONTINUE
C                                  DETERMINE DGGDMX, THE LARGEST
C                                  ELEMENT OF H
      T1 = 0.0D0
      DO 30  I=1, N
         INDEX = IDAMAX(I,H(1,I),1)
         IF (T1 .LT. H(INDEX,I)) T1 = H(INDEX,I)
   30 CONTINUE
      DGGDMX = T1
C                                  TRY ALPHA = 0
   40 CALL DU9IAH (N, H, LDH, IRC)
C
      IF (RAD .EQ. -1.0D0) THEN
         THETA = DGNORM*DGNORM
         BETA = 0.0D0
         DO 50  I=1, N
            TEMP = DDOT(N-I+1,GC(I),1,H(I,I),1)
            BETA = BETA + TEMP*TEMP
   50    CONTINUE
         RAD = THETA*DSQRT(THETA)/BETA
         RAD = DMIN1(RAD,STEPMX)
      END IF
      IF (IRC .EQ. 0) GO TO 60
C                                  INDEF. H -- UNDERESTIMATE SMALLEST
C                                  EIGENVALUE, USE THIS ESTIMATE TO
C                                  INITIALIZE LOWER BOUND ALK ON ALPHA
      T = H(IRC,IRC)
      H(IRC,IRC) = 1.0D0
      CALL DSET (IRC, 0.0D0, WK1, 1)
      WK1(IRC) = 1.0D0
      CALL DU14NF (IRC, H, LDH, WK1, WK1)
      T1 = DNRM2(IRC,WK1,1)
      ALK = -T/T1/T1
      DST0 = -ALK
      IF (RESTRT) GO TO 140
      REDUC = 0.0D0
      GO TO 70
C                                  POSITIVE DEFINITE H -- COMPUTE
C                                  UNMODIFIED NEWTON STEP.
   60 ALK = 0.0D0
      CALL DU13NF (N, H, LDH, GC, WK2)
      REDUC = 0.5D0*DDOT(N,WK2,1,WK2,1)
      CALL DU14NF (N, H, LDH, WK2, WK2)
      DST = DNRM2(N,WK2,1)
      DST0 = DST
      PHI = DST - RAD
      IF (DST .LE. HIGH*RAD) GO TO 190
      IF (RESTRT) GO TO 140
C                                  PREPARE TO COMPUTE GERSCHGORIN
C                                  ESTIMATES OF LARGEST (AND
C                                  SMALLEST) EIGENVALUES.
   70 DGNORM = DNRM2(N,GC,1)
      IF (DGNORM .EQ. 0.0D0) GO TO 300
      DO 90  I=1, N
         WK1(I) = 0.0D0
         DO 80  J=1, N
            IF (I .GT. J) THEN
               WK1(I) = WK1(I) + DABS(H(J,I))
            ELSE IF (I .LT. J) THEN
               WK1(I) = WK1(I) + DABS(H(I,J))
            END IF
   80    CONTINUE
   90 CONTINUE
C                                  (UNDER-)ESTIMATE SMALLEST EIGENVALUE
C                                  OF (D**-1)*H*(D**-1)
      K = 1
      T1 = HDIAG(1) - WK1(1)
      DO 100  I=2, N
         T = HDIAG(I) - WK1(I)
         IF (T .LT. T1) THEN
            T1 = T
            K = I
         END IF
  100 CONTINUE
C
      SK = WK1(K)
      AKK = HDIAG(K)
      T = 0.0D0
      DO 110  I=1, N
         IF (I .NE. K) THEN
            IF (I .GT. K) THEN
               AKI = DABS(H(K,I))
            ELSE
               AKI = DABS(H(I,K))
            END IF
            SI = WK1(I)
            T1 = 0.5D0*(AKK-HDIAG(I)+SI-AKI)
            T1 = T1 + DSQRT(T1*T1+SK*AKI)
            IF (T .LT. T1) T = T1
         END IF
  110 CONTINUE
C
      EMIN = AKK - T
      UK = DGNORM/RAD - EMIN
C                                  COMPUTE GERSCHGORIN (OVER-)ESTIMATE
C                                  OF LARGEST EIGENVALUE
      K = 1
      T1 = HDIAG(K) + WK1(K)
      DO 120  I=2, N
         T = HDIAG(I) + WK1(I)
         IF (T .GT. T1) THEN
            T1 = T
            K = I
         END IF
  120 CONTINUE
C
      SK = WK1(K)
      AKK = HDIAG(K)
      T = 0.0D0
      DO 130  I=1, N
         IF (I .NE. K) THEN
            IF (I .GT. K) THEN
               AKI = DABS(H(K,I))
            ELSE
               AKI = DABS(H(I,K))
            END IF
            SI = WK1(I)
            T1 = 0.5D0*(HDIAG(I)+SI-AKI-AKK)
            T1 = T1 + DSQRT(T1*T1+SK*AKI)
            IF (T .LT. T1) T = T1
         END IF
  130 CONTINUE
C
      EMAX = AKK + T
      ALK = DMAX1(ALK,DGNORM/RAD-EMAX)
C                                  ALPHAK = CURRENT VALUE OF ALPHA (SEE
C                                  ALG. NOTES ABOVE). WE USE MORE*S
C                                  SCHEME FOR INITIALIZING IT.
      ALPHAK = DABS(STPPAR)*RAD0/RAD
C
      IF (IRC .NE. 0) GO TO 140
C                                  COMPUTE L0 FOR POSITIVE DEFINITE H
      CALL DU13NF (N, H, LDH, WK2, WK1)
      T = DNRM2(N,WK1,1)
      PHIPIN = DST/T/T
      ALK = DMAX1(ALK,PHI*PHIPIN)
C                                  SAFEGUARD ALPHAK AND ADD ALPHAK*I TO
C                                  (D**-1)*H*(D**-1)
  140 KA = KA + 1
      IF (-DST0.GE.ALPHAK .OR. ALPHAK.LT.ALK .OR. ALPHAK.GE.UK)
     &    ALPHAK = UK*DMAX1(1.0D-3,DSQRT(ALK/UK))
      DO 150  I=1, N
         H(I,I) = HDIAG(I) + ALPHAK
  150 CONTINUE
C                                  TRY COMPUTING CHOLESKY DECOMPOSITION
      CALL DCSFRG (N, H, LDH)
      CALL DU9IAH (N, H, LDH, IRC)
      IF (IRC .EQ. 0) GO TO 160
C                                  (D**-1)*H*(D**-1) + ALPHAK*I IS
C                                    INDEFINITE -- OVERESTIMATE
C                                    SMALLEST EIGENVALUE FOR USE IN
C                                    UPDATING ALK
      T = H(IRC,IRC)
      H(IRC,IRC) = 1.0D0
      CALL DSET (IRC, 0.0D0, WK1, 1)
      WK1(IRC) = 1.0D0
      CALL DU14NF (IRC, H, LDH, WK1, WK1)
      T1 = DNRM2(IRC,WK1,1)
      ALK = ALPHAK - T/T1/T1
      DST0 = -ALK
      GO TO 140
C                                  ALPHAK MAKES (D**-1)*H*(D**-1)
C                                    POSITIVE DEFINITE. COMPUTE Q =
C                                    -D*STEP, CHECK FOR CONVERGENCE.
  160 CALL DU13NF (N, H, LDH, GC, WK2)
      CALL DU14NF (N, H, LDH, WK2, WK2)
      DST = DNRM2(N,WK2,1)
      PHI = DST - RAD
      IF (DST.LE.HIGH*RAD .AND. DST.GE.ALOW*RAD) GO TO 200
      IF (PHI .EQ. OLDPHI) GO TO 200
      OLDPHI = PHI
      IF (PHI .GT. 0.0D0) GO TO 170
C                                  CHECK FOR THE SPECIAL CASE OF H +
C                                  ALPHA*D**2 (NEARLY) SINGULAR.  DELTA
C                                  IS .GE. THE SMALLEST EIGENVALUE OF
C                                  (D**-1)*H*(D**-1) + ALPHAK*I.
      IF (DST0 .GT. 0.0D0) GO TO 170
C                                  EPSO6 AND PSIFAC ARE USED IN
C                                  CHECKING FOR THE SPECIAL CASE OF
C                                  (NEARLY) SINGULAR H + ALPHA*D**2
C                                  (SEE REF. 2).
      PSIFAC = 2.0D0*EPSLON/(3.0D0*(4.0D0*ALOW*(KAPPA+1.0D0)+KAPPA+
     &         2.0D0)*RAD**2)
      DELTA = ALPHAK + DST0
      TWOPSI = ALPHAK*DST*DST + DDOT(N,GC,1,WK2,1)
      IF (DELTA .LT. PSIFAC*TWOPSI) GO TO 180
C                                  UNACCEPTABLE ALPHAK -- UPDATE ALK,
C                                  UK, ALPHAK
  170 IF (KA .GE. KALIM) GO TO 200
      CALL DU13NF (N, H, LDH, WK2, WK1)
      T1 = DNRM2(N,WK1,1)
C                                  THE FOLLOWING DMIN1 IS NECESSARY
C                                  BECAUSE OF RESTARTS
      IF (PHI .LT. 0.0D0) UK = DMIN1(UK,ALPHAK)
      ALPHAK = ALPHAK + (PHI/T1)*(DST/T1)*(DST/RAD)
      ALK = DMAX1(ALK,ALPHAK)
      GO TO 140
C                                  DECIDE HOW TO HANDLE (NEARLY)
C                                  SINGULAR H + ALPHA*D**2 IF NOT YET
C                                  AVAILABLE, OBTAIN MACHINE
C                                  DEPENDENT VALUE DGXFAC.
  180 IF (DGXFAC .EQ. 0.0D0) DGXFAC = EPSFAC*DMACH(4)
C                                  NOW DECIDE.
      IF (DELTA .GT. DGXFAC*DGGDMX) GO TO 250
C                                  DELTA IS SO SMALL WE CANNOT HANDLE
C                                  THE SPECIAL CASE IN THE AVAILABLE
C                                  ARITHMETIC. ACCEPT STEP AS IT IS.
      GO TO 200
C                                  ACCEPTABLE STEP ON FIRST TRY
  190 ALPHAK = 0.0D0
      NEWTON = .TRUE.
C                                  SUCCESSFUL STEP IN GENERAL. COMPUTE
C                                  STEP = -(D**-1)*Q
  200 DO 210  I=1, N
         SC(I) = -WK2(I)/D(I)
  210 CONTINUE
      GTSTEP = -DDOT(N,GC,1,WK2,1)
      PREDUC = 0.5D0*(DABS(ALPHAK)*DST*DST-GTSTEP)
      GO TO 290
C                                  RESTART WITH NEW RADIUS
  220 IF (DST0.LE.0.0D0 .OR. DST0.GT.HIGH*RAD) GO TO 230
C                                  PREPARE TO RETURN NEWTON STEP
      RESTRT = .TRUE.
      KA = KA + 1
      CALL DCOPY (N, HDIAG, 1, H, LDH+1)
      CALL DCSFRG (N, H, LDH)
      UK = -1.0D0
      GO TO 40
C
  230 IF (KA .EQ. 0) GO TO 60
C
      DST = DSTSAV
      ALPHAK = DABS(STPPAR)
      PHI = DST - RAD
      T = DGNORM/RAD
      IF (RAD .GT. RAD0) GO TO 240
C                                  SMALLER RADIUS
      UK = T - EMIN
      ALK = 0.0D0
      IF (ALPHAK .GT. 0.0D0) ALK = ALK0
      ALK = DMAX1(ALK,T-EMAX)
      IF (DST0 .GT. 0.0D0) ALK = DMAX1(ALK,(DST0-RAD)*PHIPIN)
      GO TO 170
C                                  BIGGER RADIUS
  240 UK = T - EMIN
      IF (ALPHAK .GT. 0.0D0) UK = DMIN1(UK,UK0)
      ALK = DMAX1(0.0D0,-DST0,T-EMAX)
      IF (DST0 .GT. 0.0D0) ALK = DMAX1(ALK,(DST0-RAD)*PHIPIN)
      GO TO 170
C                                  HANDLE (NEARLY) SINGULAR H +
C                                  ALPHA*D**2 NEGATE ALPHAK TO
C                                  INDICATE SPECIAL CASE
  250 ALPHAK = -ALPHAK
C                                  USE INVERSE POWER METHOD WITH START
C                                  FROM DU10AH TO OBTAIN
C                                  APPROXIMATE EIGENVECTOR
C                                  CORRESPONDING TO SMALLEST
C                                  EIGENVALUE OF (D**-1)*H*(D**-1).
      DELTA = KAPPA*DELTA
      T = DU10AH(N,H,N,WK3,WK1)
C
      K = 0
C                                  NORMALIZE W
  260 CALL DSCAL (N, T, WK1, 1)
C                                  COMPLETE CURRENT INV. POWER ITER. --
C                                  REPLACE W BY (L**-T)*W.
      CALL DU14NF (N, H, LDH, WK1, WK1)
      T1 = 1.0D0/DNRM2(N,WK1,1)
      T = T1*T
      IF (T .LE. DELTA) GO TO 270
      IF (K .GT. 30) GO TO 200
      K = K + 1
C                                  START NEXT INV. POWER ITER. BY
C                                  STORING NORMALIZED W IN X.
      CALL DVCAL (N, T1, WK1, 1, WK3, 1)
C                                  COMPUTE W = (L**-1)*X.
      CALL DU13NF (N, H, LDH, WK3, WK1)
      T = 1.0D0/DNRM2(N,WK1,1)
      GO TO 260
C
  270 CALL DSCAL (N, T1, WK1, 1)
C                                  NOW W IS THE DESIRED APPROXIMATE
C                                  (UNIT) EIGENVECTOR AND T*X =
C                                  ((D**-1)*H*(D**-1) + ALPHAK*I)*W.
      SW = DDOT(N,WK2,1,WK1,1)
      T1 = (RAD+DST)*(RAD-DST)
      ROOT = DSQRT(SW*SW+T1)
      IF (SW .LT. 0.0D0) ROOT = -ROOT
      SI = T1/(SW+ROOT)
C                                  ACCEPT CURRENT STEP IF ADDING SI*W
C                                  WOULD LEAD TO A FURTHER RELATIVE
C                                  REDUCTION IN PSI OF LESS THAN
C                                  EPSLON/3.
      PREDUC = 0.5D0*TWOPSI
      T1 = 0.0D0
      T = SI*(ALPHAK*SW-0.5D0*SI*(ALPHAK+T*DDOT(N,WK3,1,WK1,1)))
      IF (T .GE. EPSO6*TWOPSI) THEN
         PREDUC = PREDUC + T
         DST = RAD
         T1 = -SI
      END IF
      DO 280  I=1, N
         WK2(I) = T1*WK1(I) - WK2(I)
         SC(I) = WK2(I)/D(I)
  280 CONTINUE
      GTSTEP = DDOT(N,GC,1,WK2,1)
C                                  SAVE VALUES FOR USE IN A POSSIBLE
C                                  RESTART
  290 DSTNRM = DST
      STPPAR = ALPHAK
      ALK0 = ALK
      UK0 = UK
      RAD0 = RAD
      DSTSAV = DST
      GO TO 310
C                                  SPECIAL CASE -- G = 0
  300 STPPAR = 0.0D0
      PREDUC = 0.0D0
      DSTNRM = 0.0D0
      GTSTEP = 0.0D0
      CALL DSET (N, 0.0D0, SC, 1)
C                                  RESTORE THE GRADIENT
  310 DO 320  I=1, N
         GC(I) = D(I)*GC(I)
  320 CONTINUE
C
      RETURN
      END
