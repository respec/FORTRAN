
c     begin include file chydrol.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /hydrol/   prcp,rain(mxplan),stmdur,avrint,
     1                  runoff(0:mxelem),exrain,durexr,peakro(0:mxelem),
     1                  durrun,effdrn(mxplan),effint(mxplan),
     1                  remax(mxplan),dpress(mxplan),surdra(mxplan),
     1                  sbrunf(0:mxelem)

c     + + + VARIABLE DECLARATIONS + + +

      real              prcp,rain,stmdur,avrint,runoff,exrain,durexr,
     1                  peakro,durrun,effdrn,effint,remax,dpress,surdra,
     1                  sbrunf

c     + + + VARIABLE DEFINITIONS + + +

c     prcp             : daily precipitation amount (m)
c     rain(mxplan)     : daily rainfall amount (m)
c     stmdur           : storm duration (s)
c     avrint           : average rainfall intensity (m/s)
c     exrain           : cumulative amount of rainfall excess (m)
c     durexr           : duration of rainfall excess (s)
c     durrun           : duration of runoff (s)
c     effdrn(mxplan)   : effective duration
c     remax(mxplan)    : maximum rainfall excess (m/s)
c     dpress(mxplan)   : potential depression storage depth
c     peakro(0:mxelem) : peak runoff rate (m^3/s)
c     runoff(0:mxelem) : daily runoff amount (m)
c     effint(mxplan)   : effective rainfall intensity (m/s)
c     surdra(mxplan)   : surface drainage water (m)
cx    sbrunf(0:mxelem):daily subsurface lateral flow (m)
cx       ----- This array is added to hold the daily lateral flow which will be
cx             stored in hillslope pass file and passed to the master pass file.

c     end include file chydrol.inc

