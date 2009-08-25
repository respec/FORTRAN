
c     begin include file csedld.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /sedld/    dstot(mxpts),stdist(mxpts),delxx(mxplan),
     1                  ibegin,iend,jflag(mxseg),lseg,ysdist(mxpts)

c     + + + VARIABLE DECLARATIONS + + +

      real              dstot,stdist,delxx,ysdist
      integer           ibegin,iend,jflag,lseg

c     + + + VARIABLE DEFINITIONS + + +

c     dstot(mxpts)  : sediment loss for all points down
c                     hillslope (all overland flow elements) (kg/m^2)
c     stdist(mxpts) : distance down hillslope at each point (m)
c     jflag(mxseg)  : flag for whether deposition/detachment is
c                     occurring
c     delxx(mxplan) : delta x increments between each point down
c                     hillslope (m)
c     ibegin        : beginning of deposition/detachment segment
c     iend          : ending of deposition/detachment segment
c     lseg          : flag for number of deposition/detachment
c                     segments on hillslope
c     ysdist(mxpts) :

c     end include file csedld.inc

