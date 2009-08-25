
c     begin include file cslpopt.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /slpopt/   ninpts,xdel(100),xslp(100),itop,fwidth(mxplan),
     1                  totlen(mxplan),harea(mxhill),hslop,hleng,hmann,
     1                  hydrad(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           ninpts,itop
      real              xdel,xslp,fwidth,totlen,harea,hslop,hleng,
     1                  hmann,hydrad

c     + + + VARIABLE DEFINITIONS + + +

c     ninpts         : number of slope length pairs
c     xdel(100)      : segment length (m)
c     xslp(100)      : slope of segment
c     itop           : flag for slope at top of field
c                      0 - slope equal to zero
c                      1 - slope not equal to zero
c     fwidth(mxplan) : width of field (m)
c     totlen(mxplan) : total hillslope length to current plane (m)
c     harea(mxhill)  : total hillslope area (m^2)
c     hslop          : average overland flow slope (m/m)
c     hleng          : total hillslope length (m)
c     hmann          : overland flow manning's n
c     hydrad(mxplan) : hydraulic radius (m)

c     end include file cslpopt.inc

