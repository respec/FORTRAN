      subroutine qinf(m,ealpha,efflen,aveks,effdrr,f,runoff)
c
c     + + + PURPOSE + + +
c
c     Called from IRS to compute the reduction of runoff volume caused
c     by infiltration during the recession for partial equilibrium
c     hydrographs.  The reduction is computed as
c
c     qvstar = 1/(m+1) * (fstar+1)/fstar * tstar**(-m)
c                for     tstar >= ((fstar+1)/fstar)**(1/m)
c     and
c
c     qvstar = 1 - m/(m+1) * (fstar/(fstar+1))**(1/m) * tstar
c                for     tstar <  ((fstar+1)/fstar)**(1/m)
c
c     where
c        qvstar = runoff/recum(ns)                         (nd)
c        fstar  = f(nstemp-1)/vave                         (nd)
c        tstar  = te/effdrr                                (nd)
c        vave   = average rainfall excess rate             (m/s)
c        effdrr = duration of rainfall excess              (s)
c        f      = infiltration rate at end of event        (m/s)
c        runoff = adjusted runoff                          (m)
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
c     kinematic wave model, adjusted runoff volume,
c     time to kinematic equilibrium, recession infiltration,
c     partial equilibrium
c
c     + + + PARAMETERS + + +
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real        m,ealpha,efflen,aveks,effdrr,f,runoff
c
c     + + + ARGUMENT DEFINTIONS + + +
c
c     m       - depth-discharge exponent
c     ealpha  - effective depth-discharge coefficient
c     efflen  - effective length
c     aveks   - average effective saturated conductivity
c     effdrr  - effective rainfall excess duration
c     f       - final infiltration rate
c     runoff  - adjusted runoff volume
c
c     + + + COMMON BLOCKS + + +
c
      include 'ctemp.inc'
c       read solwpv
c
c     + + + LOCAL VARIABLES + + +
c
      real             vave,te,fstar,ftest,qvstar,tstar
c
c     + + + LOCAL DEFINITIONS + + +
c
c     vave    - average rainfall excess rate
c     te      - time to kinematic equilibrium
c     tstar   - dimensionless time
c     fstar   - dimensionless infiltration rate
c     ftest   - ((fstar+1)/fstar)**(1/m)
c     qvstar  - dimensionless runoff volume
c
      vave  = runoff/effdrr
      te    = (efflen/(ealpha*vave**(m-1)))**(1/m)
      tstar = te/effdrr
      fstar = aveks/vave
c
      if (f .gt. 0.) then
        fstar = f/vave
c
        ftest = ((fstar+1)/fstar)**(1/m)
c
c       Add new IF in order to activate computation of recession
c       infiltration for partial equilibrium events when version
c       control value in the soil input file is equal to or
c       greater than 2001.0 (reflag=1)     dcf  2/6/2001
c       if ((solwpv.gt.2000.and.solwpv.ne.7777)
c    1       .or. tstar .ge. 1 ) then
c
c       Make test to use recession infiltration as originally
c       provided from Jeff Stone (no version control). dcf 3-17-04
        if (tstar .ge. 1.0) then
c
c         partial equilibrium - reduce runoff volume
c
          if (tstar .ge. ftest) then
            qvstar = 1/(m+1)*(fstar+1)/fstar*tstar**(-m)
          else
            qvstar = 1-m/(m+1)*(fstar/(fstar+1))**(1/m) * tstar
          end if
          runoff = runoff*qvstar
        end if
      end if
c
      return
      end
