      MODULE StationData

      PUBLIC StnDat

      TYPE StnDat
        CHARACTER (LEN=80) :: HEADER
        CHARACTER (LEN=5)  :: XQUAL(200)
        CHARACTER (LEN=4)  :: GBTYPE
        INTEGER            :: NPLOT
        INTEGER            :: NPKS
        INTEGER            :: NPKPLT
        INTEGER            :: NTHRESH
        INTEGER            :: NINTRVL
        INTEGER            :: HSTFLG
        INTEGER            :: IPKSEQ(200)
        INTEGER            :: THRSYR(200)
        INTEGER            :: THREYR(200)
        INTEGER            :: THRNOB(200)
        INTEGER            :: INTYR(200)
        INTEGER            :: NLOW
        INTEGER            :: NZERO
        INTEGER            :: NOBS
        INTEGER            :: OPKSEQ(25000)
        REAL               :: WEIBA
        REAL               :: PKS(200)
        REAL               :: PKLOG(200)
        REAL               :: SYSPP(200)
        REAL               :: WRCPP(200)
        REAL               :: SYSRFC(32)
        REAL               :: WRCFC(32)
        REAL               :: TXPROB(32)
        REAL               :: CLIML(32)
        REAL               :: CLIMU(32)
        REAL               :: INTLWR(200)
        REAL               :: INTUPR(200)
        REAL               :: INTPPOS(200)
        REAL               :: THRLWR(200)
        REAL               :: THRUPR(200)
        REAL               :: THRPP(200)
        REAL               :: GBCRIT
        REAL               :: WRCSKW
        REAL               :: RMSEGS
        REAL               :: ALLPOS(25000)
        REAL               :: UQL(25000)
        REAL               :: UQU(25000)
        REAL               :: EQL(25000)
        REAL               :: EQU(25000)
        REAL               :: UTL(25000)
        REAL               :: UTU(25000)
        REAL               :: ETL(25000)
        REAL               :: ETU(25000)
        CHARACTER*80       :: THRCOM(200)
        CHARACTER*80       :: INTCOM(200)
      END TYPE

      TYPE (StnDat), ALLOCATABLE :: STNDATA(:)

      END
