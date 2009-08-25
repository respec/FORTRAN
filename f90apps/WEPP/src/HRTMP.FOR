      subroutine hrtmp(hour,halfdy)
c
c     +++PURPOSE+++
c     This function distributes the diurnal (daily) temperature
c     on an hourly basis.  The actual routine to do so was
c     written by DeWet, etal.,1978 and tested by Reicosky etal 1988.
c     DeWet was taken from "simulation of Simulation, Respiration,
c     and Transpiration" while Reicosky is from "Accuracy of
c     Hourly Air Temperatures Calculated from Daily Minima and Maxima.

c     Authors(s):  John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 04/01/93

c     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                  August 1994
c
c     +++ARGUMENT DECLARATIONS+++
      real    halfdy
      integer hour
c
c     +++ARGUMENT DEFINITIONS+++
c     halfdy - The amount of time from sunrise until noon (hrs).
c     hour   - The hour of the day that is currently being run.
c
c     + + + PARAMETERS + + +
c
      include 'pmxpln.inc'
      include 'pmxhil.inc'
c
c     +++COMMON BLOCKS+++
      include  'cclim.inc'
c       read:  tave,tmax,tmin,tdpt
c       modify: hrtemp,hrdewp
c
c     +++LOCAL VARIABLES+++
      save
      real    pi,adjhr,amp,sunris
c
c     +++LOCAL DEFINITIONS+++
c     pi     - Estimated value for 22/7.
c     adjhr  - Hour value adjustment made to deal with middle of hour.
c     amp    - The ampilitude of the temperature graph.
c     sunris - Hour of the day which sunrise occures (hr).
c
c     +++DATA INITIALIZATIONS+++
      data   pi/3.14159/
c
c     +++END SPECIFICATIONS+++

      sunris = 12.0 - halfdy
      tave = (tmax + tmin) / 2
      amp  = (tmax - tmin) / 2

      if ((hour .lt. sunris) .or. (14.0 .lt. hour)) then
        if (hour .lt. sunris) then
          adjhr = (hour - 0.5) + 10.0
        else
          adjhr = (hour - 0.5) - 14.0
        endif
cd    It is wrong to calculate dewpoint temperature this way!
cd    S. Dun, Aug 11, 2007
        hrtemp = tave + amp * (cos((pi * adjhr) / (10 + sunris)))
cd        hrdewp = tdpt + amp * (cos((pi * adjhr) / (10 + sunris)))
      else
        hrtemp = tave - amp * (cos(pi * (hour - 0.5 - sunris) /
     1            (14 - sunris)))
cd        hrdewp = tdpt - amp * (cos(pi * (hour - 0.5 - sunris) /
cd     1            (14 - sunris)))
      endif

      return
      end
