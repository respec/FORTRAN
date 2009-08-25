       subroutine appmth(runoff,remax,efflen,ealpha,m,effdrr,peakro)
c
c     + + + PURPOSE + + +
c
c     Called from IRS to compute approximate peak discharge based on the
c     kinematic wave model.
c
c     There are three cases considered for peak discharge:
c
c     1. partial equilibirum              for  tstar > 1
c
c          qpstar = 1/tstar**m
c
c     2. quasi equilibrium a              for  td < tstar < 1
c
c          qpstar = 1/tstar
c
c     3. quasi equlibirum b               for  0 < tstar < td
c
c          qpstar = 1/vstar - .6*(1-vstar)/vstar * tstar
c
c     where td = (1-sqrt(1-2.4*(1-vstar)*vstar))/(1.2*(1-vstar))
c
c     and where
c        qpstar = peakro/remax                             (nd)
c        vstar  = vave/remax                               (nd)
c        tstar  = te/effdrr                                (nd)
c        peakro = peak discharge                           (m/s)
c        vave   = average rainfall excess rate             (m/s)
c        remax  = maximum rainfall excess rate             (m/s)
c        effdrr = duration of rainfall excess              (s)
c        te     = time to kinematic equilibrium            (s)
c        m      = kinematic depth-discharge exponent       (nd)
c
c     Author: J.J. Stone
c     Reference in User Guide: Chapter 4.
c
c     Version: Created March 1994
c
c     + + + KEYWORDS + + +
c
c     kinematic wave model, approximation, time to kinematic equilibrium,
c     partial equilibrium
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real     runoff,efflen,ealpha,m,effdrr,remax,peakro
      real     tc
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     runoff  - rainfall excess or runoff volume
c     efflen  - ofe length
c     ealpha  - depth-discharge coefficient
c     m       - depth-discharge exponent
c     effdrr  - duration of rainfall excess
c     remax   - maximum rainfall excess rate
c     peakro  - approximat peak discharge
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      real             te,vave,tstar,vstar,qpstar,a,b,c
c
c     + + + LOCAL DEFINITIONS + + +
c
c     te      - time to kinematic equilibrium using vave
c     vave    - average rainfall excess rate
c     vstar   - vave/remax
c     tstar   - te/effdrr
c     qpstar  - peakro/vave
c
c     compute dimensionless discharge parameters
c
      vave  = runoff/effdrr
      te    = (efflen/(ealpha*vave**(m-1)))**(1./m)
      tstar = te/effdrr
      vstar = vave/remax
c
c     compute non dimensional peak flow
c
      if (tstar .ge. 1.) then
c
c       equation 1 - time to equilibrium is less than the duration
c                    of rainfall excess (partial equilibrium)
c
        qpstar = 1./tstar**m
c
      else
        if (vstar .lt. 1.) then
c
c         equation 2 or 3 - time to equilibrium is greater than the duration
c                           of rainfall excess
c
c         compute dividing time for eq.2 and 3
c
          a  = .6*(1.-vstar)
          b  = -1.
          c  = vstar
          tc = (-b-sqrt(b**2-4*a*c))/(2*a)
c
          if (tstar .gt. tc) then
c
c           equation 2
c
            qpstar = 1./tstar
c
          else
c
c           equation 3
c
            qpstar = 1./vstar - .6*(1.-vstar)/vstar * tstar
          end if
        else
c
c         trivial case - constant rainfall excess
c
          qpstar = 1.
        end if
c
      end if
c
c     dimensional peak runoff
c
      peakro = vave*qpstar
c
      return
      end
