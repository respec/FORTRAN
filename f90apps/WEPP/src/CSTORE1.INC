
c     begin include file cstore1.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /store1/ peakin(0:mxelem), peakot(0:mxelem),
     1                runvol(0:mxelem), rvolat(0:mxelem),
     1                rvotop(0:mxelem), rvolon(0:mxelem),
     1                rvoimp(0:mxelem), roffon(0:mxelem),
     1                chnvol(0:mxelem), chnrun(0:mxelem),
     1                hlarea(0:mxelem), wsarea(0:mxelem),
     1                halpha(0:mxelem), walpha(0:mxelem),
     1                hildur(0:mxelem), rundur(0:mxelem),
     1                watdur(0:mxelem), htcs(0:mxelem),
     1                rtrans(mxelem),   sedcon(mxpart,0:mxelem),
     1                rofave(0:mxelem), htcc(0:mxelem),
     1                alphaq(0:mxelem), sbrunv(0:mxelem)

c     + + + VARIABLE DECLARATIONS + + +

      real            peakin,peakot,runvol,rvolat,rvotop,rvolon,rvoimp,
     1                roffon,chnvol,chnrun,hlarea,wsarea,halpha,walpha,
     1                hildur,rundur,htcs,rtrans,sedcon,rofave,
     1                htcc,alphaq,sbrunv, watdur

c     + + + VARIABLE DEFINITIONS + + +

c     peakin(0:mxelem)        :
c     peakot(0:mxelem)        :
c     runvol(0:mxelem)        :
c     rvolat(0:mxelem)        :
c     rvotop(0:mxelem)        :
c     rvolon(0:mxelem)        :
c     rvoimp(0:mxelem)        :
c     roffon(0:mxelem)        :
c     chnvol(0:mxelem)        :
c     chnrun(0:mxelem)        :
c     hlarea(0:mxelem)        : hillslope overland flow area (m^2)
c     wsarea(0:mxelem)        : area contributing to each element in
c                               the watershed (m^2)
c     halpha(0:mxelem)        : overland flow EPIC alpha parameter
c     walpha(0:mxelem)        : watershed (channel) EPIC alpha parameter
c     hildur(0:mxelem)        : hillslope storm duration (s)
c     rundur(0:mxelem)        :
c     watdur(0:mxelem)        : maximum storm duration on watershed
c                               element (s)
c     htcs(0:mxelem)          : overland flow time of concentration (hr)
c     rtrans(mxelem)          : transmission loss for channel
c                               elements (m)
c     sedcon(mxpart,0:mxelem) :
c     rofave(0:mxelem)        :
c     htcc(0:mxelem)          : element time of concentration (hr)
c     alphaq(0:mxelem)        :
c     sbrunv(0:mxelem)        : only subsurface runoff volume(m^3)

c     end include file cstore1.inc


