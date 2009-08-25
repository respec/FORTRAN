
c     begin include file ccover.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /cover/    canhgt(mxplan),cancov(mxplan),inrcov(mxplan),
     1                  rilcov(mxplan),gcover(mxplan),kiadjf(mxplan),
     1                  kradjf(mxplan),daydis(mxplan),rilare(mxplan),
     1                  lanuse(mxplan),tcadjf(mxplan),prestr(mxplan),
     1                  ntill(mxtlsq),dsharv(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           lanuse,ntill,dsharv
      real              canhgt,cancov,inrcov,rilcov,gcover,kiadjf,
     1                  kradjf,daydis,rilare,tcadjf,prestr

c     + + + VARIABLE DEFINITIONS + + +

c     canhgt(mxplan)  : canopy height (m)
c     cancov(mxplan)  : canopy cover (0-1, unitless)
c     inrcov(mxplan)  : interrill cover (0-1, unitless)
c     rilcov(mxplan)  : rill cover (0-1, unitless)
c     gcover(mxplan)  : ground cover (0-1, unitless)
c     kiadjf(mxplan)  : ki adjustment factor
c     kradjf(mxplan)  : kr adjustment factor
c     daydis(mxplan)  : days since previous disturbance
c     rilare(mxplan)  : rill area (m^2)
c     lanuse(mxplan)  : flag for land use
c                       (1 - cropland; 2 - rangeland; 3 - forest )
c     tcadjf(mxplan)  : consolidation adjustment factor for
c                       critical shear stress
c     prestr(mxplan)  : maximum previous consolidation stress
c                       since last tillage (Pascals)
c     ntill(mxtlsq)   : number of tillage operations during a
c                       simulation year
c     dsharv(mxplan)  : number of days since last harvest date
c                       (used in decompostion routines)

c     end include file ccover.inc

