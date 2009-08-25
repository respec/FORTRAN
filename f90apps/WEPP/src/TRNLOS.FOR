      subroutine trnlos(xks,xsm,depr,volume)
c
c     + + + PURPOSE + + +
c
c     SR TRNLOS computes transmission losses using the same
c     algorithms as found in SR ROCHEK to calculate Case 3 and 4
c     (iuprun =1) runoff on a plane with no rainfall excess but
c     with runon from above.
c
c     Called from: SR WSHIRS
c     Author(s): Ascough II, Baffaut
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcsg.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real xks, xsm, depr, volume,avwat
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     depr   - depression storage of current ofe
c     xks    - saturated conductivity
c     xsm    - matric potential
c     volume - runon coming onto the plane from upstream elements
c     avwat  - available water from rainfall.
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchpar.inc'
c     read: chnlen(mxplan), chnwid
c
      include 'cefflen.inc'
c     modify: efflen(mxplan)
c
      include 'cchpek.inc'
c     read:   charea(mxplan)
c
      include 'cdiss3.inc'
c     read: p
c
      include 'chydrol.inc'
c     modify: runoff(mxelem)
c
      include 'cstore.inc'
c     read:   watdur(0:mxelem)
c
      include 'cstruc.inc'
c     read: iplane
c
      include 'cstruct.inc'
c     read:   ichan,ielmt
c     
      include 'cwater.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real fhat, potinf, runbef, timinf, tstar
c
c     + + + LOCAL DEFINITIONS + + +
c
c     fhat   - sum of runon from upper or lateral elements
c              (hillslopes, channels, impoundments) and
c              precipitation (rainfall, snowmelt, irrigation)
c     potinf - potential infiltration (solve the g-a equation for f)
c     runbef -
c     timinf - maximum of storm duration or runoff duration
c     tstar  -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     use the maximum duration contributing runon to the channel
c
      timinf = watdur(ielmt)
c
c     calculate by volumes
c
      tstar = timinf * xks / xsm
c
c     calculate potinf and fhat as volumes instead of
c     depth*length as is done in SR ROCHEK
c
      potinf = (xsm*(tstar+sqrt(2.0*tstar)-.02987*tstar**.7913)+depr) *
     1    chnlen(iplane) * chnwid(iplane)
cd    Modified by S. Dun July 13, 2003
cd    reason: water balance
cd      fhat = volume + p * chnlen(iplane) * chnwid(iplane)
      avwat = p - (plaint(iplane)+resint(iplane))
      fhat = volume + avwat * chnlen(iplane) * chnwid(iplane)
cd    End modifying
c
      runbef = volume
c
      if (fhat.lt.potinf) then
c
c       case 4 - no runoff from end of plane
c
c       estimate how far down the plane the surface
c       water advances (efflen)
c
        efflen(iplane) = chnlen(iplane) * fhat / potinf
        runoff(iplane) = 0.0
c
      else
c
        runoff(iplane) = (fhat-potinf) / charea(iplane)
c
      end if
c
c     calculate transmission losses (in m^3)
c
      rtrans(ielmt) = runbef - runoff(iplane) * charea(iplane)
c
      return
      end
