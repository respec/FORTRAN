C**********************************************************************
      COMMON/FTABLE/ FTPNT, FTAB

      INTEGER FTPNT(MFTNUM)

      REAL FTAB(0:MRFTAB)

      INTEGER ITMP, ITAB(0:MRFTAB)

      REAL RTMP
      
      EQUIVALENCE (FTAB, ITAB)

      EQUIVALENCE (ITMP, RTMP)
C**********************************************************************

