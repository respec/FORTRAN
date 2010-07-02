      MODULE EMAThresh

      PUBLIC ThreshSpec, IntervalSpec

      TYPE ThreshSpec
        INTEGER :: THRBYR, THREYR
        REAL    :: THRLWR, THRUPR
      END TYPE

      TYPE IntervalSpec
        INTEGER :: INTRVLYR
        REAL    :: INTRVLLWR, INTRVLUPR
      END TYPE

      TYPE (ThreshSpec), ALLOCATABLE :: THRESH(:)
      TYPE (IntervalSpec), ALLOCATABLE :: INTERVAL(:)
      INTEGER NTHRESH, NINTERVAL

      END

