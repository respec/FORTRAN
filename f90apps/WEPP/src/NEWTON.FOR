      subroutine newton(time,ffpast,aveks,avesm,ffnow)
c
c     + + + PURPOSE + + +
c     Calculates cumulative infiltration via Newton's method.
c
c     Called from GRNA.
c     Author(s): Stone
c     Reference in User Guide:
c
c     Changes:
c          1) Local variables: NU1, DEL, & XX deleted.
c          2) 3rd parameter moved to end of list, since it is an
c             "output" parameter, and the others are "input" ones.
c             Corresponding changes required in GRNA and KOSTIA.
c          3) Calculation for TEST simplified.  This caused the
c             familiar annoying changes in the predicted location
c             of max. detach. with a nearly-constant detach. rate.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/24/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real time, ffpast, aveks, avesm, ffnow
c
c     + + + ARGUMENT DEFINITIONS + + +
c     time   - current time
c     ffpast - infiltration depth of last time
c     aveks  - average KS
c     avesm  - average SM
c     ffnow  - infiltration depth of current time
c
c     + + + LOCAL VARIABLES + + +
      real test, yy
c
c     + + + LOCAL DEFINITIONS + + +
c     test   - value which tells when to exit loop.
c     yy     - current computed value for FFNOW.
c
c     + + + END SPECIFICATIONS + + +
c
c     Solution for sm=0.0
c
      if (avesm.le.0.0) then
        yy = time * aveks
      else
c
c       setting the initial yy value to prevent divide by zero
c
        if (ffpast.gt.0.0) then
          yy = ffpast
        else
          yy = 0.1
        end if
c
c       Keep reevaluating test until it becomes very small (<.000001)
c
        test = 0.1
   10   if (abs(test).gt.0.000001) then
          test = (time*aveks+avesm*log(1.0+yy/avesm)-yy) * (1.0+avesm/yy
     1        )
          yy = yy + test
          go to 10
        end if
      end if
c
      ffnow = yy
c
      return
      end
