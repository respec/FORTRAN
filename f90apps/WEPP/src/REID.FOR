      subroutine reid(nstemp,re,tf,smrate,rrate,durre)
c
c     + + + PURPOSE + + +
c     Computes the duration of rainfall excess (used to compute peak discharge
c     and interrill erosion duration) and the rainfall intensity used
c     to compute interrill erosion.
c
c     Called from GRNA after depression storage is computed
c     Author: Stone
c     Reference in User Guide:
c
c     Version: Created May 1993 for version 93.0
c
c     + + + KEYWORDS + + +
c     duration of rainfall excess, rainfall intensity squared
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
      include 'pmxtim.inc'
c
c
c     + + + ARGUMENT DECLARATIONS
      integer nstemp
      real re(mxtime), tf(mxtime), durre, smrate, rrate(mxtime)
c
c     + + + ARGUMENT DEFINITIONS
c     nstemp - index of end time of rainfall excess
c     re     - rainfall excess rate
c     r      - rainfall rate
c     tf     - time associated with re and r
c     durre  - duration of rainfall excess excluding times of zero
c              rainfall excess between two bursts of rainfall excess
c     rrate  - array to hold rainfall intensity after time to ponding
c              has been inserted
c     smrate - rate of snow melt
c     sumint - time integral of the rainfall intensities over times of
c              non-zero rainfall
c
c     + + + COMMON BLOCKS + + +
      include 'cdata1.inc'
      include 'cdiss11.inc'
c
c     + + + LOCAL DECLARATIONS + + +
      integer i
c
c     initialization
c
      durre = 0.0
      sumint = 0.0
c
c     loop through time steps
c
      do 10 i = 1, nstemp - 1
c
c       when rainfall excess > 0 compute duration and intensity
c
        if (re(i).gt.0.) then
c
c XXX     Changed code so that on a day with snow melt, the intensity
c         sum value will be zero (since the code has been changed
c         so that you either have snow melt OR rainfall that are
c         contributing water to surface runoff, but not both).
c         If melt water present, then assume no interrill detachment
c         due to raindrops.  This corrects a bug that was present for
c         days of snow melt in which interrill detachment and sediment
c         load were being computed negative.     dcf 4/12/96
c
          durre = durre + tf(i+1) - tf(i)
          if (smrate.le.0.0) then
            sumint = sumint + (tf(i+1)-tf(i)) * rrate(i)
          end if
        end if
c
   10 continue
c
      return
      end
