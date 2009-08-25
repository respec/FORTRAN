
c     begin include file cwater.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /water/    salb(mxplan),ep(mxplan),es(mxplan),eo,fin,
     1                  st(mxnsl,mxplan),ul(mxnsl,mxplan),
     1                  ssc(mxnsl+1,mxplan),
     1                  hk(mxnsl),fc(mxnsl),ft(mxnsl),sep(mxplan),
     1                  su,j1,j2,s1(mxplan),
     1                  s2(mxplan),cv,tu(mxplan),thetdr(mxnsl,mxplan),
     1                  thetfc(mxnsl,mxplan),nsl(mxplan),tv(mxplan),
     1                  soilw(mxnsl,mxplan),yst(mxnsl,mxplan),
     1                  solthk(mxnsl,mxplan),ul4,watstr(mxplan),
     1                  dg(mxnsl,mxplan),et(mxplan),temstr(mxplan),
     1                  plaint(mxplan),resint(mxplan),sscmin(mxplan),
     1                  fctill(mxplan),fcutil(mxplan),
     1                  wiltil(mxplan),wilutl(mxplan),pintlv(mxplan),
     1                  sscv(mxnsl+1,mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           j1,j2,nsl
      real              salb,ep,es,eo,fin,st,ul,ssc,hk,fc,ft,
     1                  sep,su,s1,s2,cv,watstr,tv,
     1                  tu,thetdr,thetfc,soilw,solthk,dg,ul4,et,temstr,
     1                  plaint,resint,sscmin,fctill,fcutil,wiltil,
     1                  wilutl,pintlv,sscv,yst

c     + + + VARIABLE DEFINITIONS + + +

c     salb(mxplan) : soil albedo (0-1)
c     ep(mxplan) : plant transpiration (m/day)
c     es(mxplan) : soil evaporation (m/day)
c     eo: potential daily evapotranspiration (m/day)
c     fin : infiltrated water amount
c     st(mxnsl,mxplan) : current available water content per
c                        soil layer (m)
c     ul(mxnsl,mxplan) : upper limit of water content per soil layer
c     ssc(mxnsl,mxplan) : saturated hydraulic conductivity
c     sscv(mxnsl,mxplan) : Vertical saturated hydraulic conductivity
c     hk(mxnsl) : a parameter that causes SC approach zero
c                 as soil water approaches FC
c     fc(mxnsl) : soil field capacity (m)
c     ft(mxnsl) : soil temperature C
c     sep(mxplan) : seepage
c     su : soil water available for evaporation
c     j1 : soil layers subjected to soil evaporation
c     j2 : soil layers
c     s2(mxplan) : stage 2, "     "
c     cv : residue amount
c     thetdr(mxnsl,mxplan) : 15-bar soil water content (wilting
c                            point)
c     thetfc(mxnsl,mxplan) : 1/3-bar soil water content (field
c                            capacity)
c     nsl(mxplan) : number of soil layers
c     soilw(mxnsl,mxplan) : soil water content per layer (m)
c     yst(mxnsl,mxplan) : yesterday available soil water per layer (m)
c     solthk(mxnsl,mxplan) : cumulative thickness of soil layer (m)
c     ul4 : parameter to adjust potential water use by plants
c     watstr(mxplan) : water stress parameter for plant growth (0-1)
c     dg(mxnsl,mxplan) : depth of each soil layer, meters
c     s1(mxplan) : stage 1, soil evaporation
c     s2(mxplan) : stage 2, soil evaporation
c     tu(mxplan) : upper limit soil evaporation, stage 1
c     temstr(mxplan) : temp. stress param. for plant growth (0-1)
c     plaint(mxplan) : interception of rain water by live plants (m)
c     resint(mxplan) : interception of rain water by flat residue(m)
c     sscmin(mxplan) : minimum of soil profile's saturated hydraulic
c                      conductivity values  (m/s)
c     fctill(mxplan) : field capacity water content of tilled layer
c     fcutil(mxplan) : field capacity water content of untilled layer
c     wiltil(mxplan) : wilting point water content of tilled layer
c     wilutl(mxplan) : wilting point water content of untilled layer
c     pintlv(mxplan) : plant interception left over from the day before

c     end include file cwater.inc
