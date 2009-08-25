      subroutine depos(xb,xe,cdep,a,b,c,phi,theta,ilast,dl,ldlast)
c
c     + + + PURPOSE + + +
c     Calculates deposition in each segment of the hillslope.
c
c     Called from subroutine ROUTE.
c     Author(s): G. Foster, M. Nearing, D. Flanagan
c     Reference in User Guide:
c
c     Changes: Changed DEPEQ from a FUNCTION, into a SUBROUTINE
c              named DEPEQS, with an argument called DEPEQ.
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 01/14/91 to 02/26/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real xb, xe, cdep, a, b, c, phi, theta, dl, ldlast, xterm
      integer ilast
c
c     + + + ARGUMENT DEFINITIONS + + +
c     xb     - n.d. distance where deposition begins
c     xe     - n.d. distance where deposition ends
c     cdep   - portion of solution to deposition equation
c     a      - shear stress equation coefficient
c     b      - shear stress equation coefficient
c     c      - shear stress equation coefficient
c     phi    - n.d. deposition parameter
c     theta  - n.d. interill detachment parameter
c     ilast  - index counter at last point where load computed
c     dl     - n.d. deposition rate at distance x=xe
c     ldlast - n.d. sediment load at distance x=xe
c     xterm  -
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
      include 'pmxhil.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'cdist1.inc'
c       read: xinput(101,mxplan)
      include 'cends1.inc'
c       read: ktrato
      include 'cerdva1.inc'
      include 'cerdva2.inc'
c     modify: detach(101), load(101), tc(101)
      include 'cinfco1.inc'
c       read: qostar
      include 'cstruc.inc'
c       read: iplane
c
c     + + + LOCAL VARIABLES + + +
      integer ibeg, i, loopfg
      real tclast
c
c     ibeg   - counter variable value at first deposition point
c     tclast - n.d. transport capacity at distance x=xe
c     loopfg - flag.  1 = exit L3 Loop.
c
c     + + + SUBROUTINES CALLED + + +
c     depeqs
c
c     + + + END SPECIFICATIONS + + +
c
      ibeg = ilast + 1
c
c     *** L1 IF ***
      if (ibeg.lt.102) then
c
c       *** L2 IF ***
        if (xinput(ibeg,iplane).gt.xe) then
c         Original Code:
c         dl=depeq(xb,cdep,a,b,phi,theta,xe)
c
          if (qostar.le.-1.0.or.qostar.ge.0.0.or.xe.le.-qostar) then
            call depeqs(xb,cdep,a,b,phi,theta,xe,dl)
            xterm = a * xe ** 2 + b * xe + c
            tclast = xterm * ktrato
            if (tclast.le.0.0) tclast = 0.0
            ldlast = tclast - dl * (xe+qostar) / phi
          else
            tclast = 0.0
            ldlast = 0.0
          end if
        else
c
c         *** Begin L3 Loop ***
          i = ilast
          loopfg = 0
   10     continue
          i = i + 1
          if (xinput(i,iplane).le.xe) then
c
c           Check if point is past end of runoff on a Case 4 plane.
c
            if (qostar.le.-1.0.or.qostar.ge.0.0.or.xinput(i,iplane).le.
     1          -qostar) then
c
c             Original Code:
c             detach(i)=depeq(xb,cdep,a,b,phi,theta,
              call depeqs(xb,cdep,a,b,phi,theta,xinput(i,iplane),
     1            detach(i))
              xterm = a * xinput(i,iplane) ** 2 + b * xinput(i,iplane) +
     1            c
              tc(i) = xterm * ktrato
              if (tc(i).lt.0.0) tc(i) = 0.0
c
              load(i) = tc(i) - detach(i) * (xinput(i,iplane)+qostar) /
     1            phi
c
c             Added by dcf - 8/13/90  -  to prevent erroneous calculation
c             of detachment by deposition equation (for case 4 plane)
c
              if (theta.le.0.0.and.i.gt.1.and.load(i).gt.load(i-1))
     1            load(i) = load(i-1)
c
            else
              load(i) = 0.0
              tc(i) = 0.0
            end if
c
            if (load(i).lt.0.0) load(i) = 0.0
            ilast = i
            if (xinput(i,iplane).ge.1.0) loopfg = 1
          else
            loopfg = 1
          end if
c
c         *** End L3 Loop ***
          if (i.lt.101.and.loopfg.eq.0) go to 10
c
c
c
c         corrections made 5/17/91 by dcf to prevent bombing -
c         for case of CASE 4 plane where xe is greater than -qostar
c
c
c         CASE 1 is most typical - on an OFE on which flow does
c         not end.
c
          if (qostar.ge.0.0.or.qostar.le.-1.0) then
            call depeqs(xb,cdep,a,b,phi,theta,xe,dl)
            xterm = a * xe ** 2 + b * xe + c
            tclast = xterm * ktrato
            if (tclast.lt.0.0) tclast = 0.0
            ldlast = tclast - dl * (xe+qostar) / phi
c
          else
c
c           Case 2 is for an OFE on which flow does end, but
c           it does not end on the current slope segment.
c
            if (xe.lt.-qostar) then
              call depeqs(xb,cdep,a,b,phi,theta,xe,dl)
              xterm = a * xe ** 2 + b * xe + c
              tclast = xterm * ktrato
              if (tclast.lt.0.0) tclast = 0.0
              ldlast = tclast - dl * (xe+qostar) / phi
c
            else
c
c             Case 3 is for an OFE on which flow ends, and on which
c             it ends on the current slope segment.
c
              tclast = 0.0
              ldlast = 0.0
              dl = 0.0
c
            end if
c
          end if
c
c
          if (ldlast.lt.0.0) ldlast = 0.0
          if (tclast.lt.0.0) tclast = 0.0
c       if(dl.gt.0.0) dl = 0.0
c
c       *** L2 ENDIF ***
        end if
c
c     *** L1 ENDIF ***
      end if
c
      return
      end
