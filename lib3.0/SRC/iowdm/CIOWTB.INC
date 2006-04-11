C
C     table time-series arrays, sizes, and pointers
      INTEGER     MXPK, MXCL
      PARAMETER   ( MXPK=300,  MXCL=14 )
C
      COMMON /CIOTBN/ PKYR, PKMO, PKDY, PKHI, GGMO, GGDY, PK, PKG, GG,
     >                NPT
C
      INTEGER     PKYR(MXPK), PKMO(MXPK), PKDY(MXPK), PKHI(MXPK),
     >            GGMO(MXPK), GGDY(MXPK), NPT
      REAL        PK(MXPK), PKG(MXPK), GG(MXPK)
C
      COMMON /CIOTBC/ PKQ, PKGQ, GGQ
C
      CHARACTER*1 PKQ(12,MXPK), PKGQ(4,MXPK), GGQ(4,MXPK)
