
c     begin include file csolva2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : SOLVA2 is never used in SR ROUTE.

      common /solva2/   intsat,cec(mxnsl,mxplan),
     1                  solcon(mxnsl,mxplan),rfg(mxnsl,mxplan),
     1                  ki(mxplan),kr(mxplan),kt,sscadj(mxnsl,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           intsat
      real              solcon,rfg,ki,kr,kt,sscadj,cec

c     + + + VARIABLE DEFINITIONS + + +

c     intsat               : flag for saturation
c     cec(mxnsl,mxplan)    : cation exchange capacity
c     solcon(mxnsl,mxplan) : soil constant
c     rfg(mxnsl,mxplan)    : amount of rocks in soil (0-1)
c     kr(mxplan)           : initial rill detachment rate parameter (s/m)
c     ki(mxplan)           : initial interrill detachment parameter
c                            (baseline interrill erodibility, kg s m -4)
c     kt                   : sediment transport capacity coefficient=(kt1+kt2)/2
c     sscadj(mxnsl,mxplan) :

c
c     end include file csolva2.inc

