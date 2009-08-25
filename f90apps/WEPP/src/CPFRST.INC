
c     begin include file cpfrst.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /pfrst/frzflg,
     1       LNfrst,FLNfrs,FLNbtm,qdry,qwet,qhtout,
     1       ktopf,kftill,kfutil,qoutdm,dmfrsn

c     + + + VARIABLE DECLARATIONS + + +

      integer   frzflg,LNfrst,FLNfrs,FLNbtm
      real      ktopf,kftill,kfutil,qdry,qwet,qhtout,qoutdm
     1          ,dmfrsn

c     + + + VARIABLE DEFINITIONS + + +

c     frzflg :flag to indicate freezing or thawing processes
c              0 - balanced, 
c              1 - bottom freezing, 
c              2 - top freezing and bottom thawing, 
c              3 - thawing from both end
c              4 - bottom thawing
c     LNfrst - the soil layer where the bottom of the frost is
c     FLNfrs - the fine layer number in LNfrst where frost bottom is
c     FLNbtm - fine layer number of the bottom of know soil profile.
c 
c     qdry   - Heat flow from stable soil temperature to the bottom
c               of the frost layer (W/m^2).
c     qwet   - Heat flow required to freeze H2O migrate to frozen front (W/m^2).
c     qhtout - Heat flow across the frozen layers (W/m^2).
c     qoutdm - the denominator for qout = surfaceT/qoutdm (1/(W/m C))
c     dmfrsn - the denominator form residue and snow on surface (1/(W/m C))
c
c     ktopf: thermal conductivity above the the working to surface (W/m C)
c     kftill - Thermal conductivity of frozen tilled soil (W/m C).
c     kfutil - Thermal conductivity of frozen untilled soil (W/m C).  

c     end include file cpfrst.inc

