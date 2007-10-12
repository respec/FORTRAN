      MODULE EMAThresh

      PUBLIC ThreshSpec

      TYPE ThreshSpec
        INTEGER :: THRBYR, THREYR
        REAL    :: THRLWR, THRUPR
      END TYPE

      TYPE (ThreshSpec), ALLOCATABLE :: THRESH(:)
      INTEGER NTHRESH

      END

