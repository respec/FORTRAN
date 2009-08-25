      subroutine hdrive(alpha,m,xlen,run,peak)
c
c     + + + PURPOSE + + +
c     Computes the runoff hydrograph on a plane using the
c     kinematic wave model.  The solution is by the method
c     of characteristics.
c     The lateral inflow (rainfall excess) is presented as a
c     positive step function up to a given time and is zero
c     thereafter. (If step value is given as zero, it is set
c     to 1.e-8.)
c
c     Called from IRS.
c     Author(s): Shirley, Stone
c     Reference in User Guide:
c
c     Changes:
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/26/91 - 04/29/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real alpha, m, xlen, run, peak
c
c     + + + ARGUMENT DEFINITIONS + + +
c     alpha  - depth discharge coefficient
c     m      - depth discharge exponent
c     xlen   - length of equivalent plane
c     run    - runoff volume
c     peak   - peak discharge
c
c     + + + PARAMETERS + + +
c
      include 'pmxelm.inc'
      include 'pmxtim.inc'
      include 'pmxpln.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'cdata3.inc'
c     modify: dt, nqt
c
      include 'chydrol.inc'
c       read: durexr
c      write: durrun
c
      include 'cintgrl.inc'
c      write: ii
c
      include 'cpass2.inc'
c        read: t(mxtime)
      include 'cpass3.inc'
c        read: ns
c       write: nq
      include 'cpass4.inc'
c      modify: tq1(mxtime),q(mxtime),qtot(mxtime)
c
c     + + + LOCAL VARIABLES + + +
      real qsum, qlast, qtmax, t1, t2, d
      integer begrun, i
c
c     + + + LOCAL DEFINITIONS + + +
c     begrun - flag for beginning of runoff
c     qsum   - sum of all Q(I)'s
c     qlast  - value of the last Q(I)
c     i      - counter in L0 Loop
c     qtmax  - 95% of runoff volume used to stop hydrograph computations
c     t1     - previous value of T2
c     t2     - current time step
c     d      - flow depth
c
c     + + + SUBROUTINES CALLED + + +
c     bgnrnd
c
c     + + + FUNCTION DECLARATIONS + + +
      real hdepth
c
c     + + + END SPECIFICATIONS + + +
c
c
c******************************************************************
c                                                                 *
c     Subroutine INIT reads in plane parameters and rainfall      *
c     excess values, c and initializes for calls to subroutine h. *
c     Subroutine h computes depth of flow on the plane.           *
c                                                                 *
c******************************************************************
c
c
      peak = 0.
      begrun = 0
      nqt = 0
      ii = 1
c
      call bgnrnd(1842.)
c
      dt = 60.
      nq = ns + 10
      qsum = 0.
      qlast = 0.
c
      do 10 i = 1, mxtime
        qtot(i) = 0.
   10 continue
c
      qtmax = .95 * run
c
c     *** BEGIN L0 LOOP ***
c
c     Step through the rainfall excess times, get the flow depth and compute
c     the hydrograph.
c
      i = 0
   20 i = i + 1
c
      if (i.le.(ns+1)) then
        t2 = t(i)
      else
        t2 = t2 + dt
      end if
c
      d = hdepth(t2,xlen)
      tq1(i) = t2
c
      if (begrun.eq.0.and.d.gt.0.) then
        begrun = 1
      end if
c
      q(i) = alpha * d ** m
c
      if (i.gt.1) then
        qsum = qsum + (q(i)+qlast) * (t2-t1) / 2.0
c       warning from ftnchek
c       Variables may be used before set in module HDRIVE:
c       T1
        qtot(i+1) = qsum
      end if
c
      qlast = q(i)
      t1 = t2
      if (q(i).gt.peak) peak = q(i)
c
c     *** END L0 LOOP ***
      if ((i.lt.ns+50).and.((qtot(i+1)/xlen).lt.qtmax)) go to 20
c
c
      nqt = i + 1
      tq1(nqt) = t2 + dt
      d = hdepth(tq1(nqt),len)
      q(nqt) = alpha * d ** m
c
      peak = peak / xlen
c
      return
      end
