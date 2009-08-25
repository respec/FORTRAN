c     begin include file csumout.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /sumout/   nraint,nrainy,nrainm,traint,trainy,
     1                  trainm,nrunot(mxplan),nrunoy(mxplan),
     1                  nrunom(mxplan),trunot(mxplan),
     1                  trunoy(mxplan),trunom(mxplan),mxevnt,
     1                  nmunot(mxplan),nmunoy(mxplan),
     1                  nmunom(mxplan),tmunot(mxplan),
     1                  tmunoy(mxplan),tmunom(mxplan),mxmelt,
     1                  mrro,miro,mrain,mirig,mmro,mmelt,
     1                  yrro,yiro,yrain,yirig,ymro,ymelt,
     1                  arro,airo,arain,airig,amro,amelt,
     1                  yrdet,yrdep,dirig,trro

c     + + + VARIABLE DECLARATIONS + + +

      integer           nraint,nrainy,nrainm,nrunot,nrunoy,nrunom,
     1                  mxevnt,nmunot,nmunoy,nmunom,mxmelt
      real              traint,trainy,trainm,trunot,trunoy,trunom,
     1                  tmunot,tmunoy,tmunom,yrdet,yrdep,
     1                  mrro,miro,mmelt,mrain,mirig,mmro,
     1                  yrro,yiro,ymelt,yrain,yirig,ymro,
     1                  arro,amro,airo,amelt,arain,airig,dirig,trro
c

c     + + + VARIABLE DEFINITIONS + + +

c     nraint         : number of total rainfall events during the
c                      simulation period
c     nrainy         : number of rainfall events per year during simulation
c     traint         : total rainfall amount during the simulation period(m)
c     trainy         : total rainfall amount per year during simulation (m)
c     trainm         : total rainfall amount per year during simulation(m)
c     nrainm         : number of rainfall events per month during
c                      simulation
c     nrunot(mxplan) : number of total runoff events during the
c                      simulation period
c     nrunoy(mxplan) : number of total runoff events per year during
c                      simulation
c     trunot(mxplan) : total runoff amount during the simulation
c                      period (mm)
c     trunoy(mxplan) : total runoff amount per year during
c                      simulation (mm)
c     nrunom(mxplan) : number of total runoff events per month
c                      during simulation
c     trunom(mxplan) : total runoff amount per month during
c                      simulation (mm)
c     nmunot(mxplan) : number of total runoff events during the
c                      simulation period from snow melt/winter events
c     nmunoy(mxplan) : number of total runoff events per year during
c                      simulation from snow melt/winter events
c     tmunot(mxplan) : total runoff amount during the simulation
c                      period (mm) from snow melt/winter events
c     tmunoy(mxplan) : total runoff amount per year during
c                      simulation (mm) from snow melt/winter events
c     nmunom(mxplan) : number of total runoff events per month
c                      during simulation from snow melt/winter events
c     tmunom(mxplan) : total runoff amount per month during
c                      simulation (mm) from snow melt/winter events
c     mxmelt         : maximum number of snow melt/winter events over
c                      all Overland Flow Elements
c     mxevnt         : maximum number of summer runoff events over
c                      all Overland Flow Elements over entire simulation
c
c
c     yrdep          : total deposition on hillslope for year
c
c     yrdet          : average detachment on hillslope for year
c
c     mrro           : monthly rain runoff for final summaries

c     miro           : monthly irrigation runoff for final summaries

c     mmelt          : monthly melt runoff for final summaries

c     mrain          : monthly rainfall for final summaries

c     mirig          : monthly irrigation amount for final summaries
c
c     yrro           : yearly rain runoff for final summaries

c     yiro           : yearly irrigation runoff for final summaries

c     ymelt          : yearly melt runoff for final summaries

c     yrain          : yearly rainfall for final summaries

c     yirig          : yearly irrigation amount for final summaries
c
c     trro           : total rain runoff for final summaries

c     tiro           : total irrigation runoff for final summaries

c     tmelt          : total melt runoff for final summaries

c     train          : total rainfall for final summaries

c     tirig          : total irrigation amount for final summaries
c
c     end include file csumout.inc
