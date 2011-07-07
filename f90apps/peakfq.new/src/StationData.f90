      MODULE StationData

      PUBLIC StnDat

      TYPE StnDat
        CHARACTER (LEN=80) :: HEADER
        CHARACTER (LEN=5)  :: XQUAL(200)
        INTEGER            :: NPLOT
        INTEGER            :: NPKS
        INTEGER            :: NPKPLT
        INTEGER            :: NTHRESH
        INTEGER            :: NINTRVL
        INTEGER            :: HSTFLG
        INTEGER            :: IPKSEQ(200)
        INTEGER            :: THRSYR(20)
        INTEGER            :: THREYR(20)
        INTEGER            :: THRNOB(20)
        REAL               :: WEIBA
        REAL               :: PKS(200)
        REAL               :: PKLOG(200)
        REAL               :: SYSPP(200)
        REAL               :: WRCPP(200)
        REAL               :: SYSRFC(200)
        REAL               :: WRCFC(200)
        REAL               :: TXPROB(200)
        REAL               :: CLIML(200)
        REAL               :: CLIMU(200)
        REAL               :: THRLWR(20)
        REAL               :: THRUPR(20)
        REAL               :: THRPP(20)
      END TYPE

      TYPE (StnDat), ALLOCATABLE :: STNDATA(:)

      END
