C
C     include file CDATE.INC
C     common block for storage of date set data structure
C
      INCLUDE  'pmxdat.inc'
C
      COMMON/CDATE/NDATE,DACT,SDATE,EDATE,SNSDAT,SNEDAT,DATETU,DATETS,
     $             DATAGG
      INTEGER      NDATE,DACT(MXDATE),SDATE(6,MXDATE),EDATE(6,MXDATE),
     $             SNSDAT(2,MXDATE),SNEDAT(2,MXDATE),DATETU(MXDATE),
     $             DATETS(MXDATE),DATAGG(MXDATE)
C
      COMMON/CDATEC/CDATID
      CHARACTER*8   CDATID(MXDATE)
C
