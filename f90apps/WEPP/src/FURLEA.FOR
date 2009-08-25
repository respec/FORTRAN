      subroutine   furlea(
     i                    spavac,time1,time0,
     m                    ii)
c
c     + + + PURPOSE + + +
c     This subprogram allows for lower end advance during the depletion
c     and recession phases.
c
c     Called from FURREC
c     Author(s): E. R. Kottwitz
c     Reference in User Guide: Chapter 12
c
c     NOTE:  THIS MODULE HAS NOT BEEN RECODED
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
      include 'pmxsrg.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer   ii
      real      spavac,time1,time0
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ii     - primary kinematic wave calculation counter
c     spavac - space averaging coefficient for flow area for current
c              time
c     time1  - time of current set of calculations (s)
c     time0  - time of previous set of calculations (s)
c
c     + + + COMMON BLOCKS + + +
      include 'ciraflo.inc'
c       read: aqcnst,aqexp
c
      include 'cirfurr.inc'
c       read: spavz
c     modify: aflow(0:xsteps,2),infltr(0:xsteps,2),inoptm(0:xsteps),
c             tadvan(0:xsteps),xpostn(0:xsteps)
c
      include 'cirinfl.inc'
c       read: kosta,kostf,kostk
c
c     + + + LOCAL VARIABLES + + +
      real      deltax,ratio,tiavq
c
c     + + + LOCAL DEFINITIONS + + +
c     deltax - advance distance during the current time step (m)
c     ratio  - ratio of deltax to (location of next node - location of
c              current node) (m/m)
c     tiavq  - time averaging coefficient for flow rate
c
c     + + + DATA INITIALIZATIONS + + +
      data      tiavq/0.35/
c
c     + + + END SPECIFICATIONS + + +
c
c     Determine advance distance (deltax) and ratio of this distance to
c     the interval between the next node and the current node
c
c      ---- (WEPP Equation 12.14, solved for DELTAX)
      deltax = (1.0-tiavq)*qflow(ii,2)*(time1-time0)/
     1         (spavac*aflow(ii,2)+spavz*(infltr(ii,2)-infltr(ii,1)))
   10 ratio = deltax/(xpostn(ii+1)-xpostn(ii))
c
c     If next statement is true then shift current calculation
c     boundary downslope.  Linearly interpolate for inoptm(i) and
c     adjust aflow, infltr, tadvan, and xpostn arrays.
c
      if(ratio.le.0.5 .or. (ratio.lt.1.0 .and. ii.eq.xsteps-1))then
        inoptm(ii) = inoptm(ii)+(inoptm(ii+1)-inoptm(ii))*ratio
        xpostn(ii) = xpostn(ii)+deltax
        tadvan(ii) = time1
        aflow(ii,2) = 0.0
        qflow(ii,2) = 0.0
c
c        ---- (WEPP Equation 12.4, where tau=inoptm)
        infltr(ii,1) = kostk*inoptm(ii)**kosta+kostf*inoptm(ii)
        infltr(ii,2) = infltr(ii,1)
c
c     If next statement is true then shift next calculation boundary
c     upslope.  Linearly interpolate for inoptm(i+1), adjust aflow,
c     infltr, tadvan, and xpostn arrays.
c
      elseif(ratio.gt.0.5 .and. ratio.lt.1.0 .and. ii.lt.(xsteps-1))then
        ii = ii+1
        inoptm(ii) = inoptm(ii-1)+(inoptm(ii)-inoptm(ii-1))*ratio
        xpostn(ii) = xpostn(ii-1)+deltax
        tadvan(ii) = time1
        aflow(ii,2) = 0.0
        qflow(ii,2) = 0.0
c
c        ---- (WEPP Equation 12.4, where tau=inoptm)
        infltr(ii,1) = kostk*inoptm(ii)**kosta+kostf*inoptm(ii)
        infltr(ii,2) = infltr(ii,1)
c
c     If none of the above statements is true then increment i.
c     Linearly interpolate for tadvan(i) and aflow(i,2) and adjust
c     infltr array
c
      else
        ii = ii+1
        tadvan(ii) = time0+(time1-time0)/ratio
        aflow(ii,2) = aflow(ii-1,2)*(ratio-1.0)/ratio
        qflow(ii,2) = aqcnst*aflow(ii,2)**aqexp
c
c        ---- (WEPP Equation 12.4, where tau=inoptm+time1-tadvan)
        infltr(ii,2) = kostk*(inoptm(ii)+time1-tadvan(ii))**kosta+
     1                 kostf*(inoptm(ii)+time1-tadvan(ii))
c
c        ---- (WEPP Equation 12.4 where tau=inoptm)
        infltr(ii,1) = kostk*inoptm(ii)**kosta+kostf*inoptm(ii)
c
c       If the next statement is true then adjust deltax and make next
c       round of calculation boundary adjustments.
c
        if(ii.lt.xsteps)then
          deltax = deltax-(xpostn(ii)-xpostn(ii-1))
          goto 10
        endif
      endif
c
      return
      end
