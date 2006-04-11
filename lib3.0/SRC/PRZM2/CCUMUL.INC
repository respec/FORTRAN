C
C       CUMMUL common to allow VADOFT output to have annual summary
C       like PRZM output
C
      REAL*8       ANNMEF(MXNSZO,3),ANNCYV(MXNSZO,3),ANNMIF(MXNSZO,3),
     1             ANNCCP(MXNSZO,3),ANNCCW(MXNSZO),ANNWIF(MXNSZO),
     2             ANNWEF(MXNSZO)
C
C
      COMMON /CUMMUL/  ANNMEF,ANNCYV,ANNMIF,ANNCCP,ANNCCW,ANNWIF,ANNWEF
C
