c     begin include file cwint.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c
      common /wint/    deglat,azm(mxplan),snodpy(mxplan),snodpt(mxplan),
     1                 drift,wmelt(mxplan),snow,densg(mxplan),
     2                 wincnt(mxplan),frdp(mxplan),thdp(mxplan),
     3               fcycle(mxplan),frozen(mxnsl,mxplan),tfrdp(mxplan),
     4                 tthawd(mxplan),resdep(mxplan),surtmp(24),icap,
     5                 tilld(mxplan),amtfrz(mxplan),frzw(mxnsl,mxplan),
     6                 fdrift,gdrift,fgcycl(mxplan),frsttk(mxplan),
     9                 hrmlt(24,mxplan),ufutld(mxplan),wntflg(mxplan),
     2                 tens(mxplan),watpdg(mxplan),watbtm(mxplan),
     3                 wntdur,wnttim,sns,snsn,rans,wdayct(mxplan),
     4                 etm(mxplan),snmlty(mxplan),fgthwd(mxplan),
     5                 nwfzfg(mxplan),nwfrzz(mxnsl,mxplan),Fsdfg(mxplan)
     7                 ,kresf,ksnowf,kfactor

c     + + + VARIABLE DECLARATIONS + + +

      real             deglat,azm,snodpy,snodpt,drift,wmelt,snow,
     1                 densg,frdp,thdp,frozen,tfrdp,tthawd,resdep,
     2                 surtmp,icap,tilld,amtfrz,watpdg,watbtm,
     4                 fdrift,gdrift,hrmlt,frsttk,nwfrzz,frzw,
     5                 wntdur,wnttim,sns,snsn,rans,etm,snmlty,ufutld
     6                 ,kresf,ksnowf,kfactor
      integer          wincnt,fcycle,wntflg,tens,wdayct,fgcycl,
     1                 fgthwd,nwfzfg,Fsdfg


c     + + + VARIABLE DEFINITIONS + + +

c     deglat        : Latitude of the slope segment (Degrees).
c     azm           : Represents the aspect of the slope segment or
c                     which direction the slope is facing (Deg from N).
c     snodpt(mxplan): Snow pack depth (m).
c     drift         : Amount of snowdrift occurring (m).
c     wmelt(mxplan) : Amount of snow melt occurring in a day on an OFE
c                      calculated in units of water melted (m).
c     snow          : Amount of daily precip in the form of snow (m).
c     densg(mxplan) : Density of the snow pack (Kg/m^3).
c     wincnt(mxplan): This variable currently remains unused.
c     frdp(mxplan)  : Depth of the frost layer (m).
c     frsttk(mxplan): Thickness of frozen soil in the sandwitched frost layer(m).
c     thdp(mxplan)  : Thaw depth of the frozen-layer system (m).
c     watpdg(mxplan): Ponding water due to surface thawing (m)
c     watbtm(mxplan): water at the bottom of soil profile (m)
c     fcycle(mxplan): Counter of # of freeze/thaw cycles occuring (*).
c     fgcycl(mxplan): flag to indicate if surface thawing or freezing exceeds top 1cm,
c                     1 for freezing exceeds 1 cm. 0 for thawing procedure.
c     fgthwd        : flag to indicate if all frost is thawed,
c                     1 for frost completely thawed, 0 for other cases.
c     frozen        : frost thickness of the soil layer(m).
c     frzw          : ice amount of water in the soil layer (m)
c     tfrdp(mxplan) : Depth of the top frost layer (m).
c     tthawd(mxplan): Top thaw depth (m).
c     resdep(mxplan): Depth of the residue layer on slope segment (m)
c     surtmp(24)    : Hourly calculated temperature at soil surface (C).
c     icap          : Infiltration capacity of water given a soil (m^3).
c     tilld(mxplan) : Tillage depth of the soil layer (m).
c     amtfrz(mxplan): Amount of frozen water as frost heave (m).
c     gdrft         : Ground drifting snow amount (m/hr).
c     fdrft         : Falling drift amount (m/hr).
c     hrmlt(24,mxplan): Hourly melt amounts throughout the day.  Given
c                       for each OFE in units of meters of water.  (m)
c     ufutld(mxplan): Unfrozen untilled soil depth (m).
c     wntflg(mxplan): Flag used to determine if previous day was winter (*).
c     hrthaw(24,mxplan): Hourly thaw depth values throughout the day (m/hr).
c     usatkt(mxplan): Unsat hydraulic conductivity of tild layer. (m/hr)
c     usatku(mxplan): Unsat hydraulic conductvty of untild layer. (m/hr)
c     tens(mxplan)  : Water tension value to be used in QWET calcs.  (m)
c     wntdur        : Duration of the winter rain/snowfall event.  (hrs)
c     wnttim        : Time of day that the precipitation began. (hr)
c     sns           : Snow and snow on the ground (mm).
c     snsn          : Snow and no snow on the ground (mm).
c     rans          : Rain and no snow on the ground (mm).
c     wdayct(mxplan): Integer hours for snow consolidation counter (*).
c     etm(mxplan)   : Daily sublimation of snow from melt equations (m/day).
c     nwfzfg(mxplan): flag for newly added water in the frozen zone
c                     1 for unfrozen water exists, 
c                     0 for no newly added water in the frozen zone
c     nwfrzz(mxnsl,mxplan): unfrozen water ammount in the frozen zone of a soil layer(m)
c     fsdfg(mxplan): flag for first day of simulation
c                    0 for first day,
c                    1 for no first day 
c     kresf: Adjustment factor for thermal conductivity of residue
c     ksnowf: Adjustment factor for thermal conductivity of snow 
c     kfactor: Control lower limit of hydraulic conductivity in frozen soil
c
c***********************************************************************
c                                                                      *
c     THE FOLLOWING ILLUSTRATES THE VARIABLE NAMES...                  *
c                                                                      *
c***********************************************************************





c   The Frozen Layered System:
c   --------------------------


c     *********************************   - Snow depth
c
c
c     _________________________________   - Residue depth
c     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   - Residue
c     =================================   - Surface
c
c
c     ---------------------------------   - Thaw depth
c
c     ---------------------------------   - Top Frost depth
c
c
c     ---------------------------------   - Top Thaw depth
c
c
c
c     ..................................  More sndwitch layer
c
c
c     ---------------------------------   - Frost depth
c
c
c
cNOTES:
c======
c
c   Tilled Layer -      Depth from soil surface to the primary tillage
c   ------------        depth for the season previous.
c
c   Untilled Layer -    Depth from bottom of the tilled layer to the
c   --------------      stable soil depth.
c
c   Stable Soil Depth - Depth @ which soil temperature is stable...
c   -----------------   this model assumes this depth is 1 meter below
c                       the lowest 0-degree isotherm depth.  If no
c                       frost is present, this depth is left at 1 m.

c     end include file cwint.inc
