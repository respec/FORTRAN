      MODULE StationData

      PUBLIC StnDat

      TYPE StnDat
        CHARACTER (LEN=80) :: HEADER
        CHARACTER (LEN=5)  :: XQUAL(200)
        INTEGER            :: NPLOT
        INTEGER            :: NPKPLT
        INTEGER            :: HSTFLG
        INTEGER            :: IPKSEQ(200)
        REAL               :: WEIBA
        REAL               :: PKLOG(200)
        REAL               :: SYSPP(200)
        REAL               :: WRCPP(200)
        REAL               :: SYSRFC(200)
        REAL               :: WRCFC(200)
        REAL               :: TXPROB(200)
        REAL               :: CLIML(200)
        REAL               :: CLIMU(200)
      END TYPE

      TYPE (StnDat), ALLOCATABLE :: STNDATA(:)

      END
