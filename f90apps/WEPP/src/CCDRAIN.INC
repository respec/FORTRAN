
c     begin include file ccdrain.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /cdrain/  idrain(mxplan),ddrain(mxplan),drainc(mxplan),
     1                 sdrain(mxplan),drainq(mxplan),drdiam(mxplan),
     1                 satdep(mxplan),drseq(mxplan,mxplan),
     1                 drawat(mxnsl),unsdep(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           idrain,drseq
      real              ddrain,drainc,sdrain,drainq,drdiam,satdep,
     1                  drawat,unsdep

c     + + + VARIABLE DEFINITIONS + + +

c     idrain(mxplan)      : flag for tile drainage
c                          (0 - no tile 1 - tile drainage)
c     ddrain(mxplan)      : depth of drain tiles from surface, m
c     drainc(mxplan)      : drainage coefficient, m/day
c     sdrain(mxplan)      : drainage spacing, m
c     drainq(mxplan)      : drainage spacing, m
c     satdep(mxplan)      :
c     unsdep - unsaturated depth from surface to water table (m)
c     drdiam(mxplan)      : drain tile diameter, m
c     drseq(mxcrp,mxplan) : drainage sequence
c     drawat(mxnsl)       : potential water which can be drained
c                           from a soil layer

c     end include file ccdrain.inc

