c     begin include file cimpr.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /impr/ are common to impoundment
c     element  impint and impreg subroutines to modify the 
c     rearrangment of the flow regimes.This part of variables
c     are used for dscribing the feature of perforated riser.

      common /impr/ pipr(mximp),
     1    phr(mximp),phb(mximp),phs(mximp),phd(mximp),
     1    pdiar(mximp),pas(mximp),pdiab(mximp), 
     1    phrh(mximp),plbl(mximp),psbl(mximp),pdiabl(mximp),
     1    pcb(mximp),pcoefw(mximp),pcoefo(mximp),pcs(mximp),
     1    pke(mximp),pkb(mximp),pkc(mximp)

c     + + + VARIABLE DECLARATIONS + + +
  
      integer pipr
      real phr,phb,phs,phd,pdiar,pas,pdiab,phrh,plbl,psbl,pdiabl,
     1     pcb,pcoefw,pcoefo,pcs,pke,pkb,pkc
c     + + + VARIABLE DEFINITIONS + + +


c 
c       pipr(mximp)     - flag for judging whether perforated riser
c                         exists in an impoundment.
c       phr(mximp)      - Stage of riser opening
c       phb(mximp)      - Height below the datum of the restricting
c                         orifice
c       phs(mximp)      - Height of the slots
c       phd(mximp)      - Stage of the datum(i.e. bottom of the slots).
c       pdiar(mximp)    - Diameter of the riser
c       as(mximp)       - Area of the slots
c       diab(mximp)     - Diameter of the restricting orifice
c       phrh(mximp)     - Height of the riser inlet above barrel bottom
c       plbl(mximp)     - Flow length of the barrel
c       psbl(mximp)     - Slope of barrel
c       pdiabl(mximp)   - Diameter of barrel
c       pcb(mximp)      - orifice coefficient for the restricting
c                         orifice 
c       pcoefw(mximp)   - weir coefficient.
c       pcoefo(mximp)   - orifice coefficient
c       pcs(mximp)      - orifice coefficient for the slots.
c       dkb(mximp)      - bend loss coefficient
c       dkc(mximp)      - friction loss coefficient
c       dke(mximp)      - entrance loss coefficient
c
c     end include file cimps.inc
