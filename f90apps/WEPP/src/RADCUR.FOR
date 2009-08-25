      subroutine radcur(day,hr,rdlat,decl,ratio)

c
c     +++PURPOSE+++
c     This function is from Jensen etal. 1990 ACSE No. 70
c     "ET and Irrigation Water Requirements".  We are using
c     it to find the ratio between the hourly potential radiation
c     and the estimated radiation on a sloping surface for the day.
c     This function returns an hourly value which, in turn is used
c     in the sunmap routine to find the ratio of hourly to
c     daily.  The daily radiation value is then broken up into
c     an hourly value distributed over a bell shaped curve.
c
c     Author(s):  John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 03/24/93

c     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                  August 1994
c
c     +++ARGUMENT DECLARATIONS+++
      real     ratio,rdlat,decl
      integer  day,hr
c
c     +++ARGUMENT DEFINITIONS+++
c     ratio  - The calculated factor with which to multiply daily
c              radiation to calculate hourly radiation.
c     rdlat  - The latitude in radian units.
c     day    - The julian day of year.
c     hr     - The hour of the day being run.
c     decl   - Declination of the sun in radian units.(+)North (-)Sth.
c
c     +++COMMON BLOCKS+++
c
c     +++LOCAL VARIABLES+++
      real   pi,slrtm,hasun,haset,harise,solcon,rdsun,dfact
c
c     +++LOCAL DEFINITIONS+++
c     dfact  - Day factor used in solar time correction equation.
c     pi     - Estimated value of 22/7.
c     slrtm  - Seasonal correction for solar time (hrs).
c     hasun  - Hour angle of the sun given the day of year (degrees).
c     haset  - Hour angle at sunset (deg).
c     harise - Hour angle at sunrise (deg).
c     solcon - Solar constant--this time units are:  (MJ/m^2.min).
c     rdsun  - Relative distance between earth and sun.
c
c     +++END SPECIFICATIONS+++
c

      pi = 3.141593
      solcon = 0.082

      dfact = ((2 * pi) * (day - 81)) / 365

      slrtm = (0.1645 * sin(2 * dfact)) - (0.1255 * cos(dfact)) -
     1        (0.025 * sin(dfact))

      hasun = ((hr + slrtm) - 12) * (pi / 12)

c -- Could possibly add a longitude adjustment for locations between
c -- longitudinal coordinates.  However, such an adjustment is so
c -- minute that Bob decided we didn't need it here.

      haset = hasun - (pi / 24)

      harise = hasun + (pi / 24)

      rdsun = 1 + 0.033 * cos((2 * pi * day) / 365)

      ratio = ((12 * 60) / pi) * solcon * rdsun * (cos(rdlat) *
     1         cos(decl) * (sin(harise) - sin(haset)) +
     2         (harise - haset) * sin(rdlat) * sin(decl))

      if (ratio .lt. 0.0) ratio = 0.0

      return

      end
