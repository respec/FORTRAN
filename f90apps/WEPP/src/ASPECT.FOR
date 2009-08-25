      subroutine aspect(deglat,azm,avgsl)

c
c     +++PURPOSE+++
c     This subroutine calculates the aspect of the hill slope
c     relative to the sun angle which impinges upon it.  This
c     information is needed in order to calculate the amount of
c     solar radiation striking the sloping surface.
c
c     Author(s): Cully Hession and Bruce Lucord, USDA-ARS-NCSRL
c                Revised by John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 03/22/93

c     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                          August 1994
c     +++ARGUMENT DECLARATIONS+++
      real avgsl, azm, deglat, D1
cd    Added by S. Dun, Jan 17, 2006
cd    For multiple OFEs lateral flow
cd    End adding
c
c     +++ARGUMENT DEFINITIONS+++
c     avgsl  - The slope of the equivalent slope surface (m/m).
c     azm    - Direction slope is facing. Units are degrees from North.
c     deglat - Latitude of the slope segment. Measured in degrees from
c              the equator (positive values = N; negative values =S)
c
c     +++COMMON BLOCKS+++
cd    Added by S. Dun, Jan 17, 2006
cd    For multiple OFEs lateral flow
      include 'pmxelm.inc'
cd    End adding
      include  'cangie.inc'
c     modify:  radinc, radlat, eqlat, delong
      include 'cstruc.inc'
c
c     +++LOCAL VARIABLES+++
      real     rdaz, pi
c
c     +++LOCAL DEFINITIONS+++
c     rdaz   - Land aspect of the slope segment in units of radians.
c     pi     - The estimated value of pi = 22/7.
c
c     +++END SPECIFICATIONS+++


c -- Convert avgsl and azimuth to radians...
c -- Make sure that avgsl is in units of decimal percent!

      pi = 3.141593
cd    Modified by S. Dun 01/08/2004, then 01/17/2006 for adding (iplane)
cd      radinc = atan(avgsl) * pi / 180.0
      radinc(iplane) = atan(avgsl)
cd    End modifying 
      rdaz  = azm * pi / 180.0
      radlat = deglat * pi / 180.0

c -- Calculate the Equivalent Lat. and Long values...

      eqlat = asin(cos(radinc(iplane)) * sin(radlat) + 
     1         (sin(radinc(iplane))* cos(radlat) * cos(rdaz)))
c
cd    Modified by S. Dun, Nov 07, 2006 
c     In responsing Erin Brooks PMET crash, adding in the alternative routines 
c     (supplemental functions) for steep slope at high latitude 
c
c      delong = atan( (sin(radinc(ipofe)) * sin(rdaz)) / 
c     1        (cos(radinc(ipofe))* cos(radlat) - 
c     1         sin(radinc(ipofe)) * sin(radlat) * cos(rdaz)) )
c
      D1 = cos(radinc(iplane))* cos(radlat) - 
     1         sin(radinc(iplane)) * sin(radlat) * cos(rdaz)
c
      If (D1.lt. 1.e-10) D1 =  1.e-10
c
       delong = atan( (sin(radinc(iplane)) * sin(rdaz)) / D1)
c
      if (D1 .lt. 0) delong = delong + pi
cd    End modifying
      return
      end
