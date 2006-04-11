C
C     CUSRPM.INC 2.1 9/4/91
C
C     common block to store users parameters for ANNIE
C
      COMMON /CUSRPM/UPRMCT,UPARMS
      INTEGER        UPRMCT,UPARMS(MXPRM)
C
      SAVE   /CUSRPM/
