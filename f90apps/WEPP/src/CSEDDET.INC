
c     begin include file cseddet.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /seddet/   avedet,maxdet,ptdet,avedep,maxdep,ptdep,
     1                  irdgdx(mxplan),tdep(mxhill),tdet(mxhill),
     1                  detm(mxhill),dety(mxhill),dett(mxhill),
     1                  depm(mxhill),depy(mxhill),dept(mxhill),
     1                  hrom(mxhill),hroy(mxhill),hrot(mxhill),
     1                  hsedm(mxhill),hsedy(mxhill),hsedt(mxhill)

c     + + + VARIABLE DECLARATIONS + + +

      real              avedet,maxdet,ptdet,avedep,maxdep,ptdep,irdgdx,
     1                  tdep,tdet,detm,dety,dett,depm,depy,dept,hrom,
     1                  hroy,hrot,hsedm,hsedy,hsedt

c     + + + VARIABLE DEFINITIONS + + +

c     avedet         : average soil detachment over all detachment regions on
c                      a hillslope  (kg/m^2)
c     maxdet         : maximum soil detachment over all detachment regions on
c                      a hillslope  (kg/m^2)
c     ptdet          : point of maximum soil detachment on a hillslope  (m)
c     avedep         : average sediment deposition over all deposition regions
c                      on a hillslope  (kg/m^2)
c     maxdep         : maximum sediment deposition over all deposition regions
c                      on a hillslope  (kg/m^2)
c     ptdep          : point of maximum sediment deposition on a hillslope  (m)
c     irdgdx(mxplan) : interrill contribution to dG/dx - passed to be allowed
c                      as output for single event model and event-by-event
c                      output in continuous simulation   (kg/m^2)
c     tdep(mxhill)   : total event deposition on hillslope (kg)
c     tdet(mxhill)   : total event detachment on hillslope (kg)
c     detm(mxhill)   : monthly hillslope detachment (kg)
c     dety(mxhill)   : yearly hillslope detachment (kg)
c     dett(mxhill)   : total hillslope detachment (kg)
c     depm(mxhill)   : monthly hillslope deposition (kg)
c     depy(mxhill)   : yearly hillslope deposition (kg)
c     dept(mxhill)   : total hillslope deposition (kg)
c     hrom(mxhill)   : monthly hillslope runoff (m^3)
c     hroy(mxhill)   : yearly hillslope runoff (m^3)
c     hrot(mxhill)   : total hillslope runoff (m^3)
c     hsedm(mxhill)  : monthly hillslope sediment loss (kg)
c     hsedy(mxhill)  : yearly hillslope sediment loss (kg)
c     hsedt(mxhill)  : total hillslope sediment loss (kg)

c     end include file cseddet.inc

