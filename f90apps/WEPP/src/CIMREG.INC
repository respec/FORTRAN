c     begin include file cimreg.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /imreg/ are common to impoundment
c     element  impint and impreg subroutines to modify the 
c     rearrangment of the flow regimes.

      common /imreg/ daf(mxstc,10,mximp), dbf(mxstc,10,mximp),
     1    dcf(mxstc,10,mximp), ddf(mxstc,10,mximp),
     1    def(mxstc,10,mximp), dhaf(mxstc,10,mximp),
     1    dhtf(mxstc,10,mximp), dhlf(mxstc,10,mximp), 
     1    dnfr(mxstc,mximp)

c     + + + VARIABLE DECLARATIONS + + +

      real daf, dbf, dcf, ddf, def, dhaf, dhtf, dhlf
      integer dnfr

c     + + + VARIABLE DEFINITIONS + + +

c     daf(mxstc,10,mximp)  : used for initial outflow functions -
c                            structure i, flow regime j
c     dbf(mxstc,10,mximp)  : used for initial outflow functions -
c                             structure i, flow regime j
c     dcf(mxstc,10,mximp)  : used for initial outflow functions -
c                             structure i, flow regime j
c     ddf(mxstc,10,mximp)  : used for initial outflow functions -
c                             structure i, flow regime j
c     def(mxstc,10,mximp)  : used for initial outflow functions -
c                             structure i, flow regime j
c     dhaf(mxstc,10,mximp) : adjusting stage used for initial outflow-
c                             function i, flow regime j
c     dhtf(mxstc,10,mximp) : transitional stage at which flow regime j- 
c                             begins for outflow function i
c     dhlf(mxstc,10,mximp) : limiting lowest stage used for initial- 
c                             outflow function i, flow regime j
c     dnfr(mxstc,mximp)    : number of flow regimes for structure i
c

c     end include file cimreg.inc
