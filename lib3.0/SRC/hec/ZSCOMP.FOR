      SUBROUTINE ZSCOMP (ICOMP, BASE, LBASE, LHIGH, NPRE)
C
C     This routine sets compression values for storing time-
C     series data for routines ZSRTS and ZSITS.  If compression
C     is to be used (and not the default compression), this
C     routine must be called each time before one of these
C     routines (i.e., compression settings are not remembered).
C
C     Written by Bill Charley at HEC, 1989
C
C
CADD C.ZDSSMZ                                                           H
      INCLUDE 'zdssmz.h'                                                MLu
C
      COMMON /ZDSSCP/ JCOMP, BASEV, LBASEV, LDHIGH, NPREC
      LOGICAL LBASEV, LDHIGH, LBASE, LHIGH
C
C
      IF (MLEVEL.GE.9) WRITE (MUNIT, 20) ICOMP, BASE, LBASE,            D
     * LHIGH, NPRE                                                      D
 20   FORMAT (T11,'-----DSS---DEBUG:  Enter ZSCOMP',/,                  D
     * T11,'COMP:',I5,',  BASEV:',F8.2,',  LBASEV:',L2,/                D
     * T11,'LDHIGH:',L2,',  NPREC:',I4)                                 D
C
      JCOMP = ICOMP
      BASEV = BASE
      LBASEV = LBASE
      LDHIGH = LHIGH
      NPREC = NPRE
C
      RETURN
      END
