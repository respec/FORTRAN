C**********************************************************************
      INTEGER EFLAG, IN, STDOUT, OUTPUT, PROUT

      REAL SSEPS, GRAV, TWOG, HALFG

      REAL*8 TIME

      COMMON/XSCOM/ TIME, EFLAG, SSEPS, IN, STDOUT, OUTPUT, GRAV, PROUT,
     A              TWOG, HALFG
C**********************************************************************

