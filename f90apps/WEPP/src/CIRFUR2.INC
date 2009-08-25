
c     begin include file cirfur2.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     Variables in common /irfur2/ are specifically for furrow
c     irrigation.  The variables endofe, surge, and timtot must be
c     assigned prior to performing the irrigation for depletion level
c     scheduling.

      common /irfur2/   advdis,depsrg(mxplan),dtheta(mxplan),
     1                  endofe,endpln(mxplan),filrat(mxplan),
     1                  florat(mxplan),infvlm(mxplan),inoptm(0:xsteps),
     1                  irslp,nqsppl(mxsrg),ntend(mxsrg),ntstrt(mxsrg),
     1                  qspply(mxsrg),runosg,splyvm,srg,srgflg,surge,
     1                  surges(mxplan),tadvan(0:xsteps),
     1                  tend(mxsrg),timest(mxplan),timflg,timtot,
     1                  xpostn(0:xsteps),trec(0:xsteps)

c     + + + VARIABLE DECLARATIONS + + +

      integer           depsrg,endofe,endpln,srg,srgflg,surge,surges,
     1                  timflg
      real              advdis,dtheta,filrat,florat,infvlm,
     1                  inoptm,irslp,nqsppl,ntend,ntstrt,qspply,runosg,
     1                  splyvm,tadvan,tend,timest,timtot,xpostn,trec

c     + + + VARIABLE DEFINITIONS + + +

c     advdis         : advance distance for first surge, used for
c                      depletion level scheduling (m)
c     depsrg(mxplan) : number of inflow rate duration pairs (for
c                      depletion level scheduling only)
c     dtheta(mxplan) : field capacity soil water content minus current
c                      soil water content (m/m)
c     endofe         : last overland flow element for which irrigation
c                      water is intended for an irrigation that will
c                      occur on the current simulation day (used for
c                      depletion level scheduling only)
c     endpln(mxplan) : last overland flow element for which irrigation
c                      water is intended when water is introduced on
c                      overland flow element corresponding with the
c                      array element number
c     filrat(mxplan) : fill ratio for the lower end of the field (used
c                      for depletion level scheduling only) (no units)
c     florat(mxplan) : supply rate to furrow (used for depletion level
c                      scheduling only) (m*3/s)
c     infvlm(mxplan) : infiltrated volume per unit area (m)
c     inoptm(      ) : infiltration opportunity times at distance nodes
c                      (s)
c     irslp          : average slope of OFE used for kinematic wave
c                      analysis
c     nqsppl(mxsrg)  : estimated supply rate for the current surge on
c                      the next overland flow element (m^3/s)
c     ntend(mxsrg)   : estimated end time for current surge on the next
c                      overland flow element (s)
c     ntstrt(mxsrg)  : estimated start time for current surge on the
c                      next overland flow element (s)
c     qspply(mxsrg)  : irrigation water supply rate (m^3)/s)
c     runosg         : calculated runoff volume for current surge on
c                      current overland flow element (m^3)
c     splyvm         : total volume of water applied (m^3)
c     srg            : surge counter
c     srgflg         : flag indicating whether variable srg has recently
c                      been incremented
c     surge          : number of inflow rate duration pairs for an
c                      irrigation that will occur on the current
c                      simulation day
c     surges(mxplan) : number of inflow rate duration pairs (for fixed
c                      date scheduling only)
c     tadvan(      ) : advance times for distance nodes (s)
c     tend(mxsrg)    : end time for irrigation inflow (s)
c     timest(mxplan) : typical total inflow time to a furrow for a
c                      period (used for depletion level scheduling only)
c                      (s)
c     timflg         : flag indicating that advance might not reach
c                      current distance node before flow rate is changed
c     timtot         : infiltration opportunity time at the upper end of
c                      the overland flow element into which water is
c                      introduced (s)
c     trec(        ) : recession time for calculation nodes
c     xpostn(      ) : downslope distances to calculation nodes relative
c                      to upper end of furrow (m)

c     end include file cirfur2.inc

