C     from CRIN1.INC   
C     run interpreter common
C       first version
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxopn.inc'
      INCLUDE 'pmxftb.inc'
      INCLUDE 'pmxmdt.inc'
C
C     + + + HISTORY + + +
C     2009/06/23 - JLK - more space for file unit numbers
C
      COMMON/CRINTP/FILE,WCOUNT,ECOUNT,OUTLEV,
     $              RESMFG,NIVLS,IVLLIB,NXGRPS,EXGTAB,NGRPS,GRPTAB,
     $              NOPNS,OPNO,OPNTAB,NFTABS,TABINX,NMDATS,MDTINX,DUM
      INTEGER       FILE(18),WCOUNT(10),ECOUNT,
     $              OUTLEV,RESMFG,NIVLS,IVLLIB(20),NXGRPS,
     $              EXGTAB(5,10),NGRPS,GRPTAB(5,10),NOPNS,OPNO,
     $              OPNTAB(20,MAXOPN),NFTABS,TABINX(MAXFTB,3),NMDATS,
     $              MDTINX(MAXMDT,3)
      INTEGER       DUM(10192)
C     end byte:  88000 word:22000
