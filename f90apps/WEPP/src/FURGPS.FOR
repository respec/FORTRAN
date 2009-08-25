      subroutine   furgps(
     i                    chezch)
c
c     + + + PURPOSE + + +
c     This subroutine calculates the coefficient and exponent for equation
c     relating flow rate to a power function of flow area.
c
c     Called from FURROW
c     Author(s): Eugene R. Kottwitz.
c     Reference in User Guide: Chapter 12
c
c     Changes:
c          1) Added local variables TMPVR1-7 to eliminate repeated & expensive
c             calculations, especially in "100" & "200" loops.  With test
c             files, the "100" loop was executed 4-9 times before convergence.
c          2) Restructured to eliminate a GOTO within an IF-ELSE_ENDIF
c             structure, which pointed outside the structure.
c          3) Added counter II to "100" loop to terminate loop if it doesn't
c             converge within 20 iterations.
c
c     Version: This module recoded from WEPP Version 93.06.
c     Date recoded: 06/17/93 - 06/18/93.
c                   06/30/93 - 07/01/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      real      chezch
c
c     + + + ARGUMENT DEFINITIONS + + +
c     chezch - Chezy friction coefficient (m**(0.5)/s)
c
c     + + + COMMON BLOCKS + + +
      include 'ccrpout.inc'
c       read: rh(mxplan)
c
      include 'ccrpprm.inc'
c       read: rw(mxcrop,mxplan)
c
      include 'cends4.inc'
c       read: rspace(mxplan)
c
      include 'ciraflo.inc'
c     modify: aqcnst,aqexp
c
      include 'cirfur2.inc'
c       read: irslp,qspply(mxsrg),surge
c     modify: srg
c
      include 'cirriga.inc'
c       read; noirr
c
      include 'cstruc.inc'
c       read: iplane
c
c     + + + LOCAL VARIABLES + + +
      integer   i,ii,nsteps
      real      area,deltaa,denom,flodep,flowd1,flowd2,flowr,
     1          flowr2,irsum1,irsum2,irsum3,irsum4,irsum5,natlna,natlnq,
     1          topwid,wperim
      real      tmpvr1,tmpvr2,tmpvr3,tmpvr4,tmpvr5,tmpvr6,tmpvr7
c
c     + + + LOCAL DEFINITIONS + + +
c     i      - do loop counter
c     ii     - counter on convergence loops
c     nsteps - number of flow area increments
c     area   - actual flow area based on the Chezy equation (m**2)
c     deltaa - flow area increment (m**2)
c     denom  - coefficient used as a denominator in calculating the
c              furrow geometry parameters (m**12/s**2)
c     flodep - flow depth (m)
c     flowd1 - current estimate of flow depth at the maximum flow rate,
c              used in a Newton-Raphson solution (m)
c     flowd2 - next estimate of flow depth at the maximum flow rate,
c              used in a Newton-Raphson solution (m)
c     flowr  - flow rate (m**3/s)
c     flowr2 - flowr*flowr (m**6/s**2)
c     irsum1 - summation of the square of flow rates (m**6/2**2)
c     irsum2 - summation of (the square of flow rates)*natlnt
c              (m**6/s**2)
c     irsum3 - summation of (the square of flow rates)*natlnt**2
c              (m**6/2**2)
c     irsum4 - summation of (the square of flow rates)*natlni
c              (m**6/2**2)
c     irsum5 - summation of (the square of flow rates)*natlnt*natlni
c              (m**6/2**2)
c     natlna - natural logarithm of flow area
c     natlnq - natural logarithm of flow rate
c     topwid - width of water surface (m)
c     wperim - wetted perimeter (m)
c
c     + + + DATA INITIALIZATIONS + + +
      data      nsteps/50/
c
c     + + + END SPECIFICATIONS + + +
c
c     Find the maximum of furrow supply rates
c
      flowr = qspply(1)
      if(noirr.eq.2 .and. surge.ge.2)then
        do 10 srg = 2, surge
          flowr = max(flowr,qspply(srg))
   10   continue
      endif
c
c     Determine flow depth corresponding to maximum furrow supply rate
c
      flowd2 = 0.5*rh(iplane)
      tmpvr1 = 2.0*sqrt(1.0+sidslp**2)
      tmpvr2 = flowr/chezch
      tmpvr4 = 2.0*sidslp
c
c      *** Begin L0 Loop ***
      ii = 0
  100 continue
        flowd1 = flowd2
        area = (botwid+sidslp*flowd1)*flowd1
        topwid = botwid+flowd1*tmpvr4
        wperim = botwid+flowd1*tmpvr1
c        Original Code:
c       flowd2 = flowd1-(wperim-(flowr*wperim/chezch)**2/irslp/
c    1           area**3)/(3.*topwid*wperim/area-2.*sqrt(1.+sidslp**2))
        flowd2 = flowd1-(wperim-(wperim*tmpvr2)**2/(irslp*area**3))/
     1           (3.0*topwid*wperim/area-tmpvr1)
        ii = ii + 1
c
c      *** End L0 Loop ***
      if(abs(flowd2-flowd1).gt.0.000001 .and. ii.lt.20) goto 100
c      ---- caution to user
      if(ii.ge.20) then
        write (6,*) ' FURGPS: Calculations suspended after 20',
     1             ' iterations.'
        write (6,*) ' Calculations DID NOT CONVERGE.'
      endif
c
c   NOTE:  When executing above loop and printing a counter with
c          test data sets, loop executes 4-9 times before converging.
c          The "effective duration" and point of maximum soil loss
c          seem to be REALLY SENSITIVE to the order of calculations
c          in the equation above.  Perhaps some double precision
c          calculations are in order? -- CRM -- 6/30/93.
c
c
      area = (botwid+sidslp*flowd2)*flowd2
c
c     Determine values for functions of flow rate and area
c
      natlnq = log(flowr)
      natlna = log(area)
c
c     Calculate summations
c
c      Original Code:
c     flowr2 = flowr**2
c     irsum1 = flowr2
c     irsum2 = flowr2*natlna
c     irsum3 = flowr2*natlna**2
c     irsum4 = flowr2*natlnq
c     irsum5 = flowr2*natlna*natlnq
c
      irsum1 = flowr**2
      irsum2 = irsum1*natlna
      irsum3 = irsum2*natlna
      irsum4 = irsum1*natlnq
      irsum5 = irsum2*natlnq
c
c     Determine flow area increment (deltaa)
c
      deltaa = area/float(nsteps)
      area = 0.0
c
      tmpvr3 = botwid**2
      tmpvr5 = 4.0*sidslp
      tmpvr6 = irslp/wperim
c
c     Calculation loop
c
      do 200  i = 1, nsteps-1
c
c       Increment flow area
c
        area = area+deltaa
c
c       Calculate flow depth, wetted perimeter, and flow rate
c       corresponding to flow area
c
c        Original Code:
c       flodep = (sqrt(botwid**2+4.*sidslp*area)-botwid)/2./sidslp
c       wperim = botwid+2.*flodep*sqrt(1.+sidslp**2)
c       flowr = chezch*sqrt(irslp*area**3/wperim)
        flodep = (sqrt(tmpvr3+tmpvr5*area)-botwid)/tmpvr4
        wperim = botwid+flodep*tmpvr1
c
c        ---- (WEPP Equation 12.9, using area/wperim in place of
c             hydraulic radius)
        flowr = chezch*sqrt(tmpvr6*area**3)
c
c       Determine values functions of flowr and area
c
        natlnq = log(flowr)
        natlna = log(area)
        flowr2 = flowr**2
c
c       Perform summations
c
c        Original Code:
c       irsum1 = irsum1+flowr2
c       irsum2 = irsum2+flowr2*natlna
c       irsum3 = irsum3+flowr2*natlna*natlna
c       irsum4 = irsum4+flowr2*natlnq
c       irsum5 = irsum5+flowr2*natlna*natlnq
c
        tmpvr7 = flowr2*natlna
        irsum1 = irsum1+flowr2
        irsum2 = irsum2+tmpvr7
        irsum3 = irsum3+tmpvr7*natlna
        irsum4 = irsum4+flowr2*natlnq
        irsum5 = irsum5+tmpvr7*natlnq
  200 continue
c
c     Determine constant and exponent relating flow area to flow rate
c
      denom = irsum2**2-irsum1*irsum3
      aqexp = (irsum2*irsum4-irsum1*irsum5)/denom
      aqcnst = exp((irsum2*irsum5-irsum3*irsum4)/denom)
c
      return
      end
