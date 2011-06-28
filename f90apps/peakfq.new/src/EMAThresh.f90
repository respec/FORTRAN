      MODULE EMAThresh

      PUBLIC ThreshSpec, IntervalSpec

      TYPE ThreshSpec
        INTEGER :: THRBYR, THREYR, NOBS
        REAL    :: THRLWR, THRUPR, THPP
      END TYPE

      TYPE IntervalSpec
        INTEGER :: INTRVLYR
        REAL    :: INTRVLLWR, INTRVLUPR
      END TYPE

      TYPE (ThreshSpec), ALLOCATABLE :: THRESH(:)
      TYPE (IntervalSpec), ALLOCATABLE :: INTERVAL(:)
      INTEGER NTHRESH, THRDEF, NINTERVAL

      END

