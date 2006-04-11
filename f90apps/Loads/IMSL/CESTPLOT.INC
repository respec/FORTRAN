C
C     include file cestplot.inc
C     common block for storage plot data structure
C
      COMMON/CESTPLOT/NPLOT,ND,NCRV,DX,RMM,DA,NBPLOT,NG,DBX,NIB
      INTEGER         NPLOT,ND(5),NCRV(5),NBPLOT,NG(4),NIB(100,4)
      REAL*4          RMM(4)
      REAL*8          DX(5000,5),DA(5000,10,5),DBX(3000,4)
C
      COMMON/CESTPLOC/XT,YT,CT,SY,CBT
      CHARACTER*80    XT(5),YT(5),CT(5),CBT(4)
      CHARACTER*10    SY(5)
C
