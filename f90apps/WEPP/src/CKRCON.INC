
c     begin include file ckrcon.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /krcon/ krcrat(mxplan),kicrat(mxplan),tccrat(mxplan),
     1               bconsd(mxplan),ifrost(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

       real          krcrat,kicrat,tccrat,bconsd
       integer       ifrost

c     + + + VARIABLE DEFINITIONS + + +

c     krcrat(mxplan) : ratio of freshly tilled to fully consolidated
c                      rill erodibility (nondimensional)
c     kicrat(mxplan) : ratio of freshly tilled to fully consolidated
c                      interrill erodibility (nondimensional)
c     tccrat(mxplan) : ratio of freshly tilled to fully consolidated
c                      critical shear stress (nondimensional)
c     bconsd(mxplan) : consolidation decay coefficient (1/day)
c     ifrost(mxplan) : flag for frost adjustment to ki, kr, tauc
c                      0 - no adjustment
c                      1 - soil is frozen to surface
c                      2 - soil has thawed on surface, adjust ki, kr, tauc

c     end include file ckrcon.inc

