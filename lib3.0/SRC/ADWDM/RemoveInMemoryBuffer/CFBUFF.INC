C
C     This common block contains buffers for in memory storage of
C     records from multiple watershed files.
C
C     + + + PARAMETERS + + +
      INTEGER     MXWDM
      PARAMETER   (MXWDM=20)
C
      COMMON /CFBUFF/ WIBUFF,RECNO,WDMFUN,
     1                WDMCNT,MAXREC
      INTEGER     RECNO(MXWDM),WDMFUN(MXWDM),
     2            WDMCNT,MAXREC(MXWDM)
      INTEGER*4   WIBUFF(512,MXWDM)
C
      SAVE   /CFBUFF/
C
      EQUIVALENCE (WIBUFF,WRBUFF)
      REAL        WRBUFF(512,MXWDM)
C
