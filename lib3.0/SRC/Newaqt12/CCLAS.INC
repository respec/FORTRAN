C
C     include file CCLAS.INC
C     common block for storage of class data structure
C
      INCLUDE  'pmxcls.inc'
      INCLUDE  'pmxts.inc'
C
      COMMON/CCLAS/NCLAS,CACT,CSYM,CCUN,CCSL,CDSNCT,CDSN,CADSNC,CLOCCT,
     $             CMSIZE
      INTEGER      NCLAS,CACT(MXCLAS),CSYM(MXCLAS),
     $             CCUN(MXCLAS),CCSL(MXCLAS),CDSNCT(MXCLAS),
     $             CDSN(MXTS,MXCLAS),CADSNC(MXCLAS),CLOCCT(MXCLAS)
      REAL         CMSIZE(MXCLAS)
C
      COMMON/CCLASC/CSEN,CLOC,CCON
      CHARACTER*8  CSEN(MXCLAS),CLOC(MXCLAS),CCON(MXCLAS)
C
