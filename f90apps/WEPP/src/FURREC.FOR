      subroutine   furrec(
     m                    ii)
c
c     + + + PURPOSE + + +
c     This subprogram performs depletion and recession phase
c     calculations for the furrow irrigation component.
c
c     Called from FURRUN
c     Author(s): E. R. Kottwitz
c     Reference in User Guide: Chapter 12
c
c     Changes:
c          1) Changed dummy parameter I to more unique II.
c
c     Version: This module recoded from WEPP Version 93.06.
c     Date recoded: 07/07/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + PARAMETERS + + +
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxsrg.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer   ii
c
c     + + + ARGUMENT DEFINITIONS + + +
c     ii     - primary kinematic wave calculation counter
c
c     + + + COMMON BLOCKS + + +
      include 'ciraflo.inc'
c       read: aqcnst,aqexp,botwid,sidslp
c     modify: ircon2
c      write: ircon1
c
      include 'cirfurr.inc'
c       read: inoptm(0:xsteps),irslp,qspply(mxsrg),spavz,srg,
c             tadvan(0:xsteps),tend(mxsrg),xpostn(0:xsteps)
c     modify: aflow(0:xsteps,2),infltr(0:xsteps,2),trec(0:xsteps)
c
      include 'cirinfl.inc'
c       read: kosta,kostf,kostk
c
c     + + + LOCAL VARIABLES + + +
      integer   begnod,bisflg,endflg,j,k
      real      aest,aflmax(0:xsteps),atoler,denom,deltat,depth,excess,
     1          ircon3,ircon4,ircon5,qmax,spava,spavac,spavap,tiavq,
     2          tiavq1,trecn,ttoler,numer
      real      tmpvr1
c
c     + + + LOCAL DEFINITIONS + + +
c     begnod - largest numbered node having a flow area of zero, used
c              to increment the local variable k
c     bisflg - flag indication whether a modified bisection method is
c              required (0-use Newton technique, 1-use bisection method)
c     endflg - flag indicating status of recession (0-recession
c              complete, 1-recession not complete)
c     j      - kinematic wave calculation counter
c     k      - recession phase node counter
c     aest   - initial estimate of flow area (m**2)
c     aflmax - maximum expected flow area for each node
c     atoler - flow area tolerance value for Newton-Raphson iterations
c              (m**2)
c     denom  - derivative of the recession time function, used in
c              Newton-Raphson iterations
c     deltat - change in estimate of recession time (s)
c     depth  - estimate of depth of flow at node 1 based on a horizontal
c              water profile with zero depth at the upper end of the OFE
c     excess - volume of water in excess of that volume defined by
c              aflmax and the length of the calculation cell
c     ircon3 - combination of unchanging variables used to determine
c              depletion time
c     ircon4 - combination of unchanging variables used to determine
c              depletion time
c     ircon5 - combination of unchanging variables used to determine
c              recession time
c     numer  - the recession time function, used in Newton-Raphson
c              iterations (s)
c     qmax   - maximum expected flow rate at some downslope location
c              (m**3/s)
c     spava  - space averaging coefficient for flow area for advance
c              phase
c     spavac - space averaging coefficient for flow area for current
c              time
c     spavap - space averaging coefficient for flow area for previous
c              time
c     tiavq  - time averaging coefficient for flow rate
c     tiavq1 - 1.0-tiavq
c     trecn  - estimate of depletion or recession time, used in newton
c              solution (s)
c     ttoler - depletion and recession time tolerance value for Newton-
c              Raphson iterations (s)
c
c     + + + FUNCTION DECLARATIONS + + +
      external  irflow
c
c     + + + DATA INITIALIZATIONS + + +
      data      atoler/0.000001/,spava/0.65/,tiavq/0.35/,tiavq1/0.65/,
     1          ttoler/0.1/
c
c     + + + SUBROUTINES CALLED + + +
c     furlea
c     irflow (access is through subprogram newrap)
c     newrap
c
c     + + + END SPECIFICATIONS + + +
c
c     Initialize recession time for maximum advance distance
c
      trec(ii) = tend(srg)
c
c     -----  DEPLETION PHASE  -----
c
c     Initialize recession flag, counter, bisflg, and deltat
c
      begnod = 0
      k = 0
      bisflg = 0
      deltat = 1.0e6
c
c     Calculate maximum expected flow area for each node
c
      aflmax(0) = aflow(0,2)
      do 100  j = 1, xsteps
        qmax = qspply(srg)-kostf*(xpostn(j)-xpostn(0))
        if(qmax.gt.0.0)then
          aflmax(j) = (qmax/aqcnst)**(1.0/aqexp)
        else
          aflmax(j) = 0.0
        endif
  100 continue
c
c     Shift aflow and infltr arrays
c
      do 200  j = 0, xsteps
        aflow(j,1) = aflow(j,2)
        qflow(j,1) = qflow(j,2)
        infltr(j,1) = infltr(j,2)
  200 continue
c
c     Initialize space averaging coefficients for flow area
c
      spavac = 1.0-spava
      if(aflow(1,1).le.aflow(0,1))then
        spavap = spava
      else
        spavap = 1.0-spava
      endif
c
c     Set aflow(0,2) = 0 for depletion calculations
c
      aflow(0,2) = 0.0
c
c     Set qflow(0,2) = 0
c
      qflow(0,2) = 0.0
c
c     Estimate area of flow at xpostn(1) when depletion occurs
c
      depth = irslp*(xpostn(1)-xpostn(0))
      if((botwid+sidslp).gt.0.0001)then
        aflow(1,2) = min(aflow(1,1),(botwid+sidslp*depth)*depth)
      else
        aflow(1,2) = aflow(1,1)
      endif
      qflow(1,2) = aqcnst*aflow(1,2)**aqexp
c
c     Determine values for combinations of variables that will be
c     constant for a series of calculations
c
      ircon3 = (1.0-spavac)*aflow(1,2)-spavap*aflow(0,1)-(1.0-spavac)*
     1         aflow(1,1)-spavz*infltr(0,1)-(1.0-spavz)*infltr(1,1)
      ircon4 = tiavq*qflow(1,1)+tiavq1*qflow(1,2)
c
c     Make initial estimate of depletion time
c
      trec(0) = tend(srg)+aflow(0,1)*(xpostn(1)-xpostn(0))/
     1          (2.0*qflow(1,1))
c
c     Iterative calculations to determine depletion time
c
   10 continue
c
c      ---- (WEPP Equation 12.4, where tau = inoptm+trec-tadvan)
      infltr(0,2) = kostk*(inoptm(0)+trec(0)-tadvan(0))**kosta+kostf*
     1              (inoptm(0)+trec(0)-tadvan(0))
c
c      ---- (WEPP Equation 12.4 where tau = inoptm+trec-tadvan)
      infltr(1,2) = kostk*(inoptm(1)+trec(0)-tadvan(1))**kosta+kostf*
     1              (inoptm(1)+trec(0)-tadvan(1))
      trecn = tend(srg)-(ircon3+spavz*infltr(0,2)+(1.0-spavz)*
     1        infltr(1,2))*(xpostn(1)-xpostn(0))/ircon4
      if(abs(trecn-trec(0)).gt.ttoler)then
        if(abs(trecn-trec(0)).lt.deltat .and. bisflg.eq.0)then
          deltat = abs(trecn-trec(0))
          trec(0) = trecn
        else
          bisflg = 1
          trec(0) = 0.5*(trec(0)+trecn)
        endif
        goto 10
      endif
c
c     Note: With the test files the loop above seems to be executed
c           2-4 times before convergence. -- CRM -- 7/07/93.
c
c     Do loop to calculate aflow and infltr for rectangular cells
c
      do 300  j = 2, ii
c
c       Determine space averaging coefficient for flow area for previous
c       time
c
        if(aflow(j,1).le.aflow(j-1,1))then
          spavap = spava
        else
          spavap = 1.0-spava
        endif
c
c       Determine infiltration at the node of interest and calculate
c       value of combination of variables that will be constant for a
c       series of calculations
c
c        ---- (WEPP Equation 12.4, where tau = inoptm+trec-tadvan)
        infltr(j,2) = kostk*(inoptm(j)+trec(0)-tadvan(j))**kosta+kostf*
     1                (inoptm(j)+trec(0)-tadvan(j))
        tmpvr1 = xpostn(j)-xpostn(j-1)
  310   continue
c
c        ---- (WEPP Equation 12.13)
        ircon2 = (tiavq/aqcnst*(qflow(j,1)-qflow(j-1,1))-
     1           tiavq1/aqcnst*qflow(j-1,2)+(spavac*aflow(j-1,2)-
     2           spavap*aflow(j-1,1)-(1.0-spavap)*aflow(j,1)+spavz*
     3           (infltr(j-1,2)-infltr(j-1,1))+(1.0-spavz)*
     4           (infltr(j,2)-infltr(j,1)))*tmpvr1/
     5           aqcnst/(trec(0)-tend(srg)))/tiavq1
c
c       If next statement is true then area of flow is very small.  If
c       statement is false then calculate flow area and continue
c
        if(ircon2.ge.0.0)then
          aflow(j,2) = 0.0
          qflow(j,2) = 0.0
          trec(j) = trec(0)
        else
          aest = aflow(j-1,2)-aflow(j-1,1)+aflow(j,1)
          if(aest.le.0.0)aest = aflow(j,1)
c
c          ---- (WEPP Equation 12.12)
          ircon1 = (1.0-spavac)*tmpvr1/tiavq1/aqcnst/(trec(0)-tend(srg))
c
c         NOTE:  The call to IRFLOW from NEWRAP includes the expected
c                argument.  The argument is not required here.
c
          call newrap(aest,atoler,irflow,aflow(j,2))
          qflow(j,2) = aqcnst*aflow(j,2)**aqexp
        endif
c
c       If next statement is true then flow area is decreasing with
c       downslope distance starting with the previous-time calculation
c       cell boundary so change spavac accordingly
c
        if(spavac.eq.1.0-spava.and.aflow(j,2).lt.aflow(j-1,2))then
          spavac = spava
          goto 310
        endif
  300 continue
c
c     Check for lower end advance during the depletion phase
c
      if(aflow(ii,2).ge.0.0001 .and. ii.lt.xsteps)
     1  call furlea(spavac,trec(0),tend(srg),ii)
c
c     -----  RECESSION PHASE  -----
c
c     Label 20 marks the location where control is passed for
c     incrementing the recession phase node counter
c
   20 continue
      k = max(begnod+1,k+1)
      begnod = k
c
c     Shift aflow and infltr arrays
c
      do 400  j = 0, xsteps
        aflow(j,1) = aflow(j,2)
        qflow(j,1) = qflow(j,2)
        infltr(j,1) = infltr(j,2)
  400 continue
c
c     Set flow area for current node equal to zero
c
      aflow(k,2) = 0.0
      qflow(k,2) = 0.0
c
c     Estimate recession time
c
      if(k.ge.2)then
        trec(k) = max(2.0*trec(k-1)-trec(k-2),trec(k-1)+1.0)
      else
        trec(k) = trec(k-1)+trec(0)-tend(srg)
      endif
c
c     Initialize space averaging coefficients for area, endflg, and
c     calculate ircon5, a combination of variables that will not change
c
      spavap = 1.0-spava
      spavac = 1.0-spava
      endflg = 0
      ircon5 = (xpostn(k)-xpostn(k-1))/(tiavq*qflow(k,1))
c
c     Iterative calculations to determine recession time
c
   30 continue
c
c      ---- (WEPP Equation 12.4, where tau = inoptm+trec-tadvan)
      infltr(k,2) = kostk*(inoptm(k)+trec(k)-tadvan(k))**kosta+kostf*
     1              (inoptm(k)+trec(k)-tadvan(k))
      numer = trec(k)-trec(k-1)-ircon5*((1.0-spavap)*aflow(k,1)-(1.0-
     1        spavz)*(infltr(k,2)-infltr(k,1)))
      denom = 1.0+ircon5*(1.0-spavz)*(kostk*kosta*(inoptm(k)+trec(k)-
     1        tadvan(k))**(kosta-1.0)+kostf)
      trecn = trec(k)-numer/denom
      if(abs(trec(k)-trecn).ge.ttoler)then
        if(trecn.lt.0.0)stop 'Recession phase procedure failed.'
        trec(k) = trecn
        goto 30
      endif
c
c     Force trec(k) to be greater than trec(k-1)
c
      if(trec(k).le.trec(k-1))trec(k) = trec(k-1)+.1
c
c     Calculation loop to calculate aflow and infltr
c
      j = k
   40 continue
      if(j.lt.ii)then
        j = j+1
        tmpvr1 = xpostn(j)-xpostn(j-1)
c
c       Determine space averaging coefficient for area of flow for the
c       current time
c
        if(aflow(j,1).le.aflow(j-1,1))then
          spavap = spava
        else
          spavap = 1.0-spava
        endif
c
c       Determine infiltration at the node of interest and calculate
c       value of combination of variables that will be constant for a
c       series of calculations
c
c        ---- (WEPP Equation 12.4, where tau = inoptm+trec-tadvan)
        infltr(j,2) = kostk*(inoptm(j)+trec(k)-tadvan(j))**kosta+kostf*
     1                (inoptm(j)+trec(k)-tadvan(j))
   50   continue
c
c        ---- (WEPP Equation 12.13)
        ircon2 = (tiavq/aqcnst*(qflow(j,1)-qflow(j-1,1))-
     1           tiavq1/aqcnst*qflow(j-1,2)+(spavac*aflow(j-1,2)-
     2           spavap*aflow(j-1,1)-(1.0-spavap)*aflow(j,1)+spavz*
     3           (infltr(j-1,2)-infltr(j-1,1))+(1.0-spavz)*
     4           (infltr(j,2)-infltr(j,1)))*tmpvr1/
     5           aqcnst/(trec(k)-trec(k-1)))/tiavq1
c
c     Note: Using test files, the equation above was frequently
c           executed almost 400 times before exiting the 20-loop!
c           --- CRM -- 7/07/93.
c
c       If next statement is true then area of flow is very small.  If
c       statement is false then calculate flow area and continue
c
        if(ircon2.ge.0.0)then
          aflow(j,2) = 0.0
          qflow(j,2) = 0.0
          if(j.eq.begnod+1)begnod=begnod+1
          if(trec(j).lt.trec(0))trec(j)=trec(k)
        else
          endflg = 1
          aest = aflow(j-1,2)-aflow(j-1,1)+aflow(j,1)
          if(aest.le.0.0)aest = aflow(j,1)
c
c          ---- (WEPP Equation 12.12)
          ircon1 = (1.0-spavac)*tmpvr1/tiavq1/aqcnst/(trec(k)-trec(k-1))
c
c         NOTE:  The call to IRFLOW from NEWRAP includes the expected
c                argument.  The argument is not required here.
c
          call newrap(aest,atoler,irflow,aflow(j,2))
          qflow(j,2) = aqcnst*aflow(j,2)**aqexp
          trec(j) = 0.0
        endif
c
c       If next statement is true then flow area is decreasing with
c       downslope distance starting with the previous-time calculation
c       cell boundary so change spavac accordingly
c
        if(spavac.eq.1.0-spava.and.aflow(j,2).lt.aflow(j-1,2))then
          spavac = spava
          goto 50
        endif
c
c       Adjust flow area for node of interest and node immediately
c       upstream for the situation where calculated flow area is greater
c       than the maximum expected value.
c
        if(aflow(j,2).gt.aflmax(j))then
          if(j.le.xsteps-1)then
            excess = (aflow(j,2)-aflmax(j))*.5*(xpostn(j+1)-xpostn(j-1))
          else
            excess = (aflow(j,2)-aflmax(j))*.5*tmpvr1
          endif
          aflow(j-1,2) = aflow(j-1,2)+excess/(.5*(xpostn(j)-
     1                   xpostn(j-2)))
          qflow(j-1,2) = aqcnst*aflow(j-1,2)**aqexp
          aflow(j,2) = aflmax(j)
          qflow(j,2) = aqcnst*aflmax(j)**aqexp
        endif
c
c       Determine where to pass control
c
        if(j.lt.ii)then
c     Note: Most of the branching within the 20-loop occurs from here.
c           -- CRM -- 7/07/93.
          goto 40
        elseif(endflg.eq.1)then
c
c         Check for lower end advance during the recession phase
c
          if(j.eq.ii .and. aflow(ii,2).gt.0.0001 .and. ii.lt.xsteps)
     1      call furlea(spavac,trec(k),trec(k-1),ii)
c
c         If the next statement is true, continue with recession phase
c         calculations
c
          if(k.le.ii) goto 20
        endif
      endif
c
      do 500  k = 0, ii
        inoptm(k) = inoptm(k)+trec(k)-tadvan(k)
  500 continue
c
      return
      end
