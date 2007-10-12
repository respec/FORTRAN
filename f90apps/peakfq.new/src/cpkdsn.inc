C
C     This common block contains the DSN buffer.
C
      COMMON /CPKDSN/ DSNIND, DSNCNT, DSNBUF
C
      INTEGER   DSNBMX
      PARAMETER (DSNBMX=300)
C
      INTEGER   DSNCNT, DSNIND, DSNBUF(DSNBMX)
C
