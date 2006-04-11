C
C-----Common block mcstr to store VADOFT outputs for Monte Carlo:
C
      REAL*8  WFLXMC,ADVMC(3,MXNSZO),DISMC(3,MXNSZO),DKMC(3,MXNSZO)
      COMMON /MCSTRR4/ WFLXMC
C
      COMMON /MCSTRR41/ ADVMC,DISMC,DKMC
