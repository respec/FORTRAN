c     begin include file cends4.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

c     NOTE : ENDS4 is never used in SR ROUTE.

      common /ends4/    tcend,rspace(mxplan),width(mxplan),qshear,
     1                  qsout, rwflag(mxplan), wdhtop(mxplan)

c     + + + VARIABLE DECLARATIONS + + +

      integer           rwflag
      real              tcend,rspace,width,qshear,qsout,wdhtop

c     + + + VARIABLE DEFINITIONS + + +

c     tcend          : transport capacity at the end of the
c                      average uniform slope profile that
c                      passes though the endpoints of an OFE (kg/s*m).
c     rspace(mxplan) : rill spacing (m).
c     width(mxplan)  : rill width (m) computed using flow at end of OFE
c     wdthop(mxplan) : rill width (m) computed using flow at top of OFE
c     qshear         : peak flow discharge (m^3/s), normally at the
c                      bottom of an OFE.
c     qsout          : sediment discharge at end of current
c                      OFE (kg/m of width per second).
c     rwflag         : flag to indicate if user desires WEPP to reset
c                    : rill width after each tillage operation and compute
c                    : new rill widths with each runoff event:
c                      rwflag = 1  have model compute varying rill widths
c                                  due to tillage and flow
c                             = 2  use input rill width always
c
c     end include file cends4.inc
c
