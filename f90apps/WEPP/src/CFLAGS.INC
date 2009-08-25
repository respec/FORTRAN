
c     begin include file cflags.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /flags/  bigflg,ichplt,iflag,ipdout,snoflg,wgrflg,yldflg,
     1                idflag,chkflg

c     + + + VARIABLE DECLARATIONS + + +

      integer         bigflg,ichplt,iflag,ipdout,snoflg,wgrflg,yldflg,
     1                idflag,chkflg

c     + + + VARIABLE DEFINITIONS + + +

c     bigflg : flag for hillslope large graphics output
c     ichplt : flag for watershed channel profile erosion plotting
c     iflag  : flag for hillslope initialization calls
c     ipdout : flag for impoundment output
c     snoflg : flag for hillslope winter processes output
c     wgrflg : flag for watershed large graphics output
c     yldflg : flag for hillslope crop yield output
c     idflag : flag for channel runoff-runon and impoundment cases
c
c              channel runoff-runon and impoundment cases:
c
c              idflag = 0 for no runoff from hillslope(s) or channel
c              idflag = 2 for channel runoff but no runon from hillslope(s)
c              idflag = 3 for no channel runoff but hillslope runon
c              idflag = 4 for both channel runoff and hillslope runon
c
c              impoundment flow cases:
c
c              idflag = 0 then hydraulic routing only
c              idflag > 0 then hydraulic and sediment routing
cx        chkflg: flag for event checking.
cx                chkflg = 0 NO EVENT
cx                chkflg = 1 EVENT
cx                chkflg = 2 SUBEVENT

c     end include file cflags.inc

