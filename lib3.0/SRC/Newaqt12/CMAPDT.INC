C
C     data structure for attributes from WDM file
      COMMON /MAPDAT/STDSN,STSYR,STEYR,STLAT,STLNG,STISTA,STHUC,
     $               STINAM,STATID,STATVL,STRATI,
     $               STCNT,STALBX,STALBY
C
      INTEGER*4   STDSN,STSYR,STEYR,STISTA,STHUC,
     $            STINAM(12),STATID(2),
     $            STCNT,STALBX,STALBY
      REAL        STLAT,STLNG,STATVL(2),STRATI
C
C     + + + PARAMETERS + + +
      INTEGER     STASZE
      PARAMETER  (STASZE=27)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (STDSN,STAREC),(STDSN,STARCR)
      INTEGER     STAREC(STASZE)
      REAL        STARCR(STASZE)
C
