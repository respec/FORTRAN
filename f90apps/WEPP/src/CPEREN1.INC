
c     begin include file cperen1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /peren1/ partcf(ntype),fact(ntype),rtmmax(ntype),
     1                tmpmin(ntype),tmpmax(ntype),imngmt(mxcrop,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      real            partcf,fact,rtmmax,tmpmin,tmpmax
      integer         imngmt

c     + + + VARIABLE DEFINITIONS + + +

c     partcf(ntype)         : portion of vegetative biomass partitioned
c     fact(ntype)           : standing to flat residue adjustment factor
c     rtmmax(ntype)         : maximum root mass for a perennial crop
c                             (tons/acre)
c     tmpmin(ntype)         : minimum daily temperature (degrees C)
c     tmpmax(ntype)         : maximum daily temperature (degrees C)
c     imngmt(mxcrop,mxplan) : cropping system
c                             1) annual
c                             2) perennial
c                             3) fallow

c     end include file cperen1.inc

