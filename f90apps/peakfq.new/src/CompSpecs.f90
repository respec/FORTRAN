      MODULE CompSpecs

      PUBLIC Spec, StaSpec

      TYPE Spec
        CHARACTER (LEN=80) :: STR
      END TYPE

      TYPE StaSpec
        CHARACTER (LEN=15)                 :: ID
        INTEGER                            :: NSPECS
        TYPE(SPEC), DIMENSION(:), POINTER  :: SPECS(:)
      END TYPE

      TYPE (StaSpec), ALLOCATABLE :: STASPECS(:)
      LOGICAL UPDATEFG  

      END
