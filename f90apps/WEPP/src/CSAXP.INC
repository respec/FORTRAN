
c     begin include file csaxp.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /saxp/ saxwp(mxnsl,mxplan),
     1   saxfc(mxnsl,mxplan), 
     1   saxenp(mxnsl,mxplan),saxpor(mxnsl,mxplan),
     1   saxA(mxnsl,mxplan), saxB(mxnsl,mxplan),
     1   saxks(mxnsl,mxplan)       
 
c     + + + VARIABLE DECLARATIONS + + +

      real  saxwp,saxfc,saxenp,saxpor,saxA,saxB,saxks

c     + + + VARIABLE DEFINITIONS + + +

c     Saxton K.E. and Rawls W.J., 2006. Soil water characteristics estimates 
c     by texture and organic matter for hydraologic solution.
c     Soil SCI. SOC. AM. J., 70, 1569--1578
c
c     saxwp: 1500 kpa soil water content (wilting point)
c     saxfc: 33 kpa soil water content (field capacity)
c     saxpor: saturated water content
c     saxenp: air entry pressure (kpa) 
c     saxA, saxB : moisture tension equition coefficients
c     saxks: saturated hydraulic conductivity (m/s) 

c     end include file csaxp.inc

