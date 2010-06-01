!****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
      DOUBLE PRECISION FUNCTION GGBCRITP(N,R,ETA)
!*** REVISION 2010.03
!****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
! 
!       PROGRAM TO COMPUTE CRITICAL POINTS FOR A GENERALIZED GRUBBS-BECK
!         TEST
!       NOTES:  SEE 2010 MANUSCRIPT BY COHN, ENGLAND, ET AL.
! 
!       AUTHOR.........TIM COHN
!       DATE...........1 MAR 2010 (TAC)
!               MODIFIED....... 8 MAR 2010 (TAC)
!               MODIFIED.......29 APR 2010 (TAC) [OFFSET ADDED FOR SMALL N]
!
!KthOrderPValueOrthoT <- function(n,r,eta){
!  integrateV(
!    function(pzr,n,r,eta){
!                   zr       <- qnorm(qbeta(pzr,shape1=r,shape2=n+1-r))
!                   CV       <- VMS(n,r,qmin=pnorm(zr))
!                   lambda   <- CV[1,2]/CV[2,2]
!                   etap     <- eta + lambda
!                   EMp      <- EMS(n,r,qmin=pnorm(zr))
!                   muMp     <- EMp[1]-lambda*EMp[2]
!                   SigmaMp  <- sqrt(CV[1] - CV[1,2]^2/CV[2,2]);
!                   moms     <- CondMomsChi2(n,r,zr)
!                   shape    <- moms[1]^2/moms[2]
!                   scale    <- moms[2]/moms[1]
!                   SigmaS   <- sqrt(shape*scale)
!                   df       <- 2*shape
!                   ncp      <- (muMp-zr)/SigmaMp
!                   q        <- -(SigmaS/SigmaMp)*etap
!               (1 - pt(q,df=df, ncp=ncp, lower.tail = TRUE)) 
!     },
!       lower=1e-7,upper=1-1e-7,n=n,r=r,eta=eta)
!   }
!
!****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
!
      IMPLICIT NONE
      
      INTEGER
     1 LIMIT
     
      PARAMETER
     1 (LIMIT=40000)
     
      INTEGER
     * N,R,N_IN,R_IN,
     1 NEVAL,IER,LENW,LAST,IWORK(LIMIT),
     2 IFAULT,KEY
     
      DOUBLE PRECISION 
     * ETA,ETA_IN,
     1 FGGB,BOUND,EPSABS,EPSREL,RESULT,ABSERR,WORK(4*LIMIT),
     2 DF,DELTA

      EXTERNAL FGGB
      
      COMMON /GGB001/ETA_IN,N_IN,R_IN
 
      DATA EPSABS/1.D-5/,EPSREL/1.D-5/,KEY/3/,LENW/160000/
      
      DOUBLE PRECISION OFFSET(10,40)
      DATA OFFSET /90*-99.D0,
     1 -0.025, 0.013, 0.058, 0.118, 0.241,0.169,0.125,0.095,0.077,0.062,
     1 -0.028, 0.007, 0.036, 0.081, 0.145,0.114,0.091,0.073,0.063,0.054,
     1 -0.024, 0.003, 0.023, 0.053, 0.096,0.078,0.070,0.059,0.052,0.047,
     1 -0.020, 0.002, 0.019, 0.039, 0.067,0.058,0.054,0.048,0.044,0.040,
     1 -0.020,-0.002, 0.012, 0.029, 0.047,0.046,0.042,0.040,0.038,0.034,
     1 -0.017,-0.003, 0.010, 0.022, 0.037,0.035,0.034,0.033,0.031,0.029,
     1 -0.017,-0.002, 0.008, 0.017, 0.027,0.027,0.028,0.028,0.027,0.026,
     1 -0.012,-0.002, 0.005, 0.013, 0.021,0.023,0.024,0.023,0.023,0.023,
     1 -0.012,-0.005, 0.001, 0.010, 0.016,0.019,0.021,0.021,0.020,0.020,
     1 -0.013,-0.004, 0.002, 0.008, 0.014,0.015,0.018,0.018,0.018,0.018,
     1 -0.012,-0.005, 0.002, 0.007, 0.011,0.013,0.015,0.015,0.015,0.017,
     1 -0.010,-0.004, 0.001, 0.005, 0.009,0.011,0.012,0.013,0.014,0.015,
     1 -0.008,-0.004, 0.001, 0.004, 0.008,0.009,0.011,0.011,0.013,0.013,
     1 -0.008,-0.005, 0.000, 0.003, 0.006,0.008,0.009,0.010,0.012,0.011,
     1 -0.008,-0.006,-0.001, 0.002, 0.005,0.007,0.008,0.009,0.010,0.010,
     1 -0.007,-0.005,-0.001, 0.001, 0.005,0.006,0.007,0.008,0.009,0.010,
     1 -0.005,-0.003,-0.002, 0.001, 0.004,0.005,0.006,0.007,0.008,0.009,
     1 -0.007,-0.004,-0.001, 0.001, 0.004,0.004,0.005,0.006,0.007,0.008,
     1 -0.008,-0.004,-0.001, 0.001, 0.003,0.003,0.004,0.006,0.007,0.007,
     1 -0.008,-0.004,-0.002, 0.000, 0.002,0.002,0.004,0.005,0.006,0.007,
     1 -0.008,-0.004,-0.002,-0.001, 0.001,0.002,0.003,0.005,0.006,0.006,
     1 -0.008,-0.004,-0.003,-0.001, 0.001,0.001,0.003,0.004,0.005,0.006,
     1 -0.007,-0.004,-0.003,-0.001, 0.000,0.001,0.002,0.004,0.005,0.005,
     1 -0.007,-0.004,-0.003,-0.001, 0.000,0.000,0.002,0.003,0.004,0.005,
     1 -0.006,-0.004,-0.003,-0.002,-0.001,0.000,0.002,0.003,0.004,0.005,
     1 -0.006,-0.004,-0.004,-0.002,-0.001,0.000,0.001,0.003,0.004,0.004,
     1 -0.006,-0.004,-0.003,-0.002,-0.001,0.000,0.001,0.003,0.003,0.004,
     1 -0.006,-0.004,-0.003,-0.002,-0.001,0.000,0.001,0.002,0.003,0.004,
     1 -0.006,-0.004,-0.002,-0.002,-0.001,0.000,0.001,0.002,0.003,0.003,
     1 -0.006,-0.004,-0.002,-0.002, 0.000,0.000,0.001,0.002,0.003,0.003,
     1 -0.005,-0.004,-0.001,-0.002, 0.000,0.000,0.001,0.002,0.003,0.003/
     
      IF(N .LT. 10 .OR. R .GT. N/2) THEN
        WRITE(*,*) ' INVALID INPUT DATA/GRUBBS BECK (N,R): ', N,R
        GGBCRITP = 0.5D0
        RETURN
      ELSE
          N_IN   = N
          R_IN   = R
        IF(N .LE. 40 .AND. R .LE. 10) THEN  !  MAKE SMALL SAMPLE CORRECTION
            ETA_IN = ETA-OFFSET(R,N)
        ELSE
            ETA_IN = ETA
        ENDIF
      ENDIF
      
      CALL DQAG(FGGB,0.D0,1.D0,EPSABS,EPSREL,KEY,RESULT,ABSERR,
     *    NEVAL,IER,LIMIT,LENW,LAST,IWORK,WORK)

      GGBCRITP   =  RESULT
      
      RETURN
      END
            
!****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
      DOUBLE PRECISION FUNCTION FGGB(PZR)
!*** REVISION 2010.03
!****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
!
!****|==|====-====|====-====|====-====|====-====|====-====|====-====|==////////
!
      IMPLICIT NONE
      
      INTEGER
     * N,R,IFAULT
     
      DOUBLE PRECISION 
     * PZR,ETA,DF,DELTA,MuM, MuS2,VarM,VarS2,CovMS2,EX1,EX2,EX3,EX4,
     2 ES,CovMS,VarS,
     3 MuMP,EtaP,H,Lambda,MuS,ncp,q,VarMP,PR,ZR,N2,
     3 FP_B_ICDF,FP_Z_ICDF,FP_Z_CDF,FP_Z_PDF,TNC,FP_T_CDF,FP_NCT_CDF

      COMMON /GGB001/ETA,N,R
!
!  Compute the value of the r-th smallest obs. based on its order statistic
!
      N2   = dble(N-R)
      PR   = FP_B_ICDF(PZR,dble(R),dble(N+1-R))
      ZR   = FP_Z_ICDF(PR)

!
!  Calculate the expected values of M, S2, S and their variances/covariances
!
      H        =  FP_Z_PDF(ZR)/(Max(1.d-10,1.d0-PR))
      EX1      =  H
      EX2      =  1 + H * ZR
      EX3      =  2 * EX1 + H * ZR**2
      EX4      =  3 * EX2 + H * ZR**3
      
      MuM      =  EX1
      MuS2     =  (EX2-EX1**2)
      VarM     =  MuS2/N2
      VarS2    =  ((EX4 - 4*EX3*EX1 + 6*EX2*EX1**2 - 3*EX1**4) - 
     1                  MuS2**2)/(N2-1.d0)
     
      CovMS2   =  (EX3 - 3*EX2*EX1 + 2*EX1**3)/SQRT(N2*(N2-1.D0))
      
      MuS      =  SQRT(MuS2) - VarS2/(8*MuS2**1.5)
      CovMS    =  CovMS2/(2*SQRT(MuS2))
      VarS     =  MuS2 - MuS**2
      
        Lambda = CovMS/VarS
        EtaP   = Eta + Lambda
        MuMP   = MuM - Lambda * MuS
        VarMP  = VarM - CovMS**2/VarS
        df     = 2.d0*MuS2**2/VarS2
        ncp    = (MuMP -zr)/SQRT(VarMP)
        q      = -sqrt(MuS2/VarMP) * EtaP
      FGGB = 1.d0 - tnc(q,df,ncp,ifault)

      return
      end