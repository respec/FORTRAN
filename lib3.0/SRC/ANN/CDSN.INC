C
C     This common block contains the dsn buffer that is usually filled
C     with the find routines.
C
C     + + + PARAMETERS + + +
      INTEGER   DSNBMX
      PARAMETER (DSNBMX=300)
C
      COMMON /CDSN/ DSNIND, DSNCNT, DSNBUF
      INTEGER       DSNIND, DSNCNT, DSNBUF(DSNBMX)
C
      SAVE   /CDSN/
C
