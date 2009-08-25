
c     begin include file csenes.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /senes/    isenes(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           isenes

c     + + + VARIABLE DEFINITIONS + + +

c     isenes(mxplan) : flag in call to RESUP to indicate
c                      type of update for residue
c
c                      10 - residue addition no disturbance
c                      12 - residue addition with senesence
c                      1 - harvest after senescence
c                      0 - harvest before senescence
c                     -1 - 1st freeze of perennial
c                     -2 - senescence

c     end include file csenes.inc
