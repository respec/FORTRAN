C     common block CBIO.INC
C
      REAL*8    AM,AC,AS,AR,KE,KSM,KCM
      REAL*8    KC,MKS,KR,KIN,KSK,KLDM,KLDC,KLDS,KLDR,KL1
      REAL*8    KL2,USM,UCM,MUC,US,UR,Q,CM
      REAL*8    Y,YSM,YCM,YC,YS,YR,C12
      INTEGER*4 BIOFLG
C
C
      COMMON /BIDGI/  BIOFLG
      COMMON /BIDGR/  AM,AC,AS,AR,KE,KSM,KCM,
     1                  KC,MKS,KR,KIN,KSK,KLDM,KLDC,KLDS,KLDR,KL1,
     2                  KL2,USM,UCM,MUC,US,UR,
     4                  YSM,YCM,YC,YS,YR
      COMMON /BIDGR1/ Q(NCMPTS),CM(NCMPTS)
      COMMON /BIDGR2/ Y(6,3,NCMPTS),C12(3,NCMPTS)
C
C
