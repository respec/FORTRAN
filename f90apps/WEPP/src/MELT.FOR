      subroutine melt(irtype,wrain,hour)
c
c
c     +++PURPOSE+++
c     This subroutine calculates the amount of snow melt that
c     occurs on an hourly basis given the hourly weather and
c     surface conditions.  The amount of melted water is calculated
c     in units of meters.
c
c     Author(s): Cully Hession and Bruce Lucord, USDA-ARS-NCSRL
c                Revised by John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 03/23/93

cc     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c     Modified 11/25/96 by Dennis Flanagan

c
c     +++ARGUMENT DECLARATIONS+++
      real wrain,cancvf
      integer  hour,irtype
c
c     +++ARGUMENT DEFINITIONS+++
c     wrain  - Amount of daily measured rainfall (m/day).
c     hour   - Hour of the day (hr).
c     irtype - crop residue type index
c
c     +++PARAMETERS+++
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxhil.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pmxres.inc'
      include 'pmxelm.inc'
c
c     +++COMMON BLOCKS+++
      include  'ccrpout.inc'
c       read:  ccr
      include  'cclim.inc'
c       read:  hradmj,tmax,vwind,hrdewp,hrtemp,rpoth
      include  'cdecvar.inc'
c       read:  cuthgt(ntype)
      include  'cstruc.inc'
c       read:  iplane
      include  'ccover.inc'
c       read:  canhgt,cancov,lanuse
      include  'cupdate.inc'
c       read:  sdate
      include  'cwint.inc'
c       read:  snodpy,densg
      include  'crinpt6.inc'
c       read:  rrough(mxplane)
cd      Added by S. Dun, Nov. 21, 2006
      include 'CANGIE.INC'
c      read : solef (solar energy adjust fator for sloped surface)
cd      End adding
c
c     +++LOCAL VARIABLES+++
      save
      real     amelt1,bmelt1,cmelt1,dmelt1,m1
      real     height,rough,disp,cldpct,amelt,bmelt,cmelt,windh,
     1         x,tmptrm,davgtf,dmelt,rainin,hrtef,adj,vwmph,hrdtf

c
c     +++LOCAL DEFINITIONS+++
c     height - Height of the canopy which is above the snow layer (m).
c     rough  - Surface roughness of the top layer of the system (m).
c     disp   - Zero-plane displacement of the system's surface (m).
c     cldpct - Percent cloud cover for the given hour (decimal %).
c     amelt  - First part of WEPP eqn 3.2.1 - represents radiation term.
c     bmelt  - Second part of WEPP 3.2.1 - represents cloud cover term.
c     tmptrm - Temporary term used in equations.
c     cmelt  - Third partof WEPP 3.2.1 - represents wind term.
c     windh  - Height at which the wind speed is measured, assume 2.0 m.
c     dmelt  - Fourth part of WEPP 3.2.1 - represents temperature term.
c     x      - Temporary variable.
c     adj    - adjustment for wind height and roughness calculations
c     +++DATA INITIALIZATIONS+++
c
c    NOTE - The wind generated
c    by CLIGEN is for a 10 meter height 
c
cd       data   windh/2.0/
      data   windh/10.0/
c
c     +++END SPECIFICATIONS+++

cd      Added by S. Dun, June 22, 2007
      amelt = 0
      bmelt = 0
      cmelt = 0
      dmelt = 0
      rainin = 0
      cancvf = 1
cd      End adding

c -- Calculate the surface conditions --

      if(lanuse(iplane).eq.1)then
           height = cuthgt(irtype)
           if(height.lt.canhgt(iplane))height = canhgt(iplane)
      else
           height = canhgt(iplane)
      endif
c
      rough = 0.0

      if(height .lt. 0.001) rough = 0.0002
      if(snodpy(iplane) .lt. 0.01) rough = rrc(iplane)
      if(height .lt. 0.001) goto 10

      height = height - snodpy(iplane)
      if(height .lt. 0.001) then
          height = 0.0
          rough = 0.0002
      else
          rough = 0.13 * height
      endif

10    disp = 0.77 * height
c
c     Savabi correction - need to convert delta temperature
c     from degrees C to degrees F -  dcf  11/25/96
c
      hrtef=hrtemp*(9./5.)
      davgtf=(tmax + tmin)/2*(9./5.)

c -- Calculating decimal percent cloud cover --
cd      Modified by S. Dun, Jan 12, 2007
      cldpct = cloudC

cd      if (rpoth .lt. 0.0001) then
cd        cldpct = 1.0
cd      else
cd        cldpct=(1.0 - radmj /rpoth) / 0.7
cd      endif

cd      if (cldpct .gt. 1.0) cldpct = 1.0
cd      End modifying
c
c -- Aug 1989 WEPP manual. First term of 3.2.1 for melt. --
c -- NOTE that 0.0607 is a conversion from MJ to Ly units. --
cd     0.5 snow albedo is assumed 0.00508*23.889*(1-0.5) = 0.0607

c      if(hrtemp.gt.0) then
      amelt = 0.0607 * hradmj * (1.0 - cancov(iplane)*cancvf)
c      else
c      amelt = 0
c      endif

c -- NOTE: Canopy may not be the correct variable to use in the
c --        WEPP version of winter. -JW
c -- Check with Bob to see if cutght(iplane) should be used...

c -- Allow some melt when max temp > 0 but mean temperature < 0  --

cd      Modified by S. Dun, June 13,2007
c      To exactly follow the equation in Hendrick et al., 1971
c
cd      if(hrtemp .gt. -4.0 .and. hrtemp .le. 0.0) then
cd        tmptrm = amelt
cd        amelt = tmptrm * (0.36 * hrtemp + 1.0)
cd      endif
c      if (hrtef.le. -5) then
c            amelt = 0
c      elseif(hrtef .le. 0) then
c         amelt = amelt * (0.2 * hrtef + 1.0)
c      endif

c      Condition L1
c      If (hrtef.gt.0)  Then
cd      End of modifying

c -- Aug 1989 WEPP manual. Second term of 3.2.1 for melt. --
cd      Modified by S. Dun, June 14, 2007
cd      bmelt =( 0.84 * (1.0 - cldpct))/24.
c
      bmelt = 0.025/24 * hrtef
     1       -( 0.84 * (1.0 - cldpct))*(1.0 - cancov(iplane)*cancvf)/24.
cd      End modifying
      etm(iplane)  = etm(iplane) + (.0254 * bmelt)

c -- Aug 1989 WEPP manual. Third term of 3.2.1 for melt.
c -- This safety check was placed in here to account for case
c -- of rough being unset and defaulted to 0.0.
c
cd      Modified by S. Dun, Jan 17, 2008
c      the wind adjustment factor equation seems wrong
c      Now we use 5-22 from EM1110-2-1406 vind measurement height assumed 15.2m 
c     when using unit meter the coefficent would be 1.57 = (15.2)^(1/6)
cd      if (rough .lt. 0.0001) then
cd        cmelt = 0.0
cd      else
cd        xtem = windh - disp + rough
cd        if(xtem .gt. 0.0)then
cd          adj = 1.0 -  1.0 / log(xtem/rough)
cd        else
cd          adj = 1.0
cd        endif
c
c    NOTE - The wind generated
c    by CLIGEN is for a 10 meter height 
c
        adj = 1.57 * windh**(-1/6)        
        if (adj.lt.0) adj = 0
c
c       Savabi correction to convert wind velocity from m/sec
c       to miles/hour.  Assume 0.1 tx=0.22 hrtef     -  dcf  11/25/96
        vwmph=(vwind*3600)/1609.
        hrdtf=tdpt*(9./5.)
c
cd      Added by S. Dun, Jan 15, 2008
c      for condtion when wind effect can be considered negligible
        if(vwmph.gt.0) then
           cmelt = (0.0084/24.) * vwmph*(1.0-0.8*cancov(iplane)*cancvf)*
     1             ((0.22*hrtef)+(0.78*hrdtf))*adj
     1             + 0.8*cancov(iplane)*cancvf * 0.045/24 * hrtef
        else
           cmelt = 0.045/24 * hrtef
        endif
cd      endif

c -- Aug 1989 WEPP manual.  Fourth term of 3.2.1 for melt.
c    Equation commented out.  New equation for dmelt1 from
c    Savabi is given below.   -   dcf  11/25/96
c       dmelt1 = hrtemp * (0.0382 + 0.014 * cldpct) + hrtemp *
c    1        0.000496 * hrrain(hour)

c      Unit conversion from meter to inch
      rainin=hrrain(hour)*39.37
cd      Modified by S. Dun. June 13, 2007
cd      Rain is hourly rainfall and first term belongs to longwave radiation
cd      dmelt2=(hrtef)*(0.025/24.+(0.007/24.*rainin))
c
c     Assume warm rain temperature equals dew point temperature  
c     if the dew point temperature is greater than 0 C,
c     otherwise eaqual the hourly air tempearture.
c
      if (hrdtf .gt. 0.0) then
          dmelt= 0.007* rainin * hrdtf
      else
          dmelt= 0.007* rainin * hrtef
      endif
c
c      End Condition L1
c      Endif
cd      End modifying

c -- Put terms into the following eqn calculates water melt (m). --
c -- NOTE that 0.0254 is a conversion factor from inches to meters. --
c
      wmelt(iplane) = 0.0254 * (amelt + bmelt + cmelt + dmelt)
c
c      if(wmelt(iplane) .lt. 0) wmelt(iplane) = 0.0
c
c -- This check converts the water depth to a depth of equal density --
c --  to that of the snowpack. --
c -- The "1000" is the density of water (kg/m^3).

c     Savabi new line to prevent negative melt values - dcf 11/25/96
cd    Modified by S. Dun, July 12, 2007
c     To allow negative vlues, due to using daily model in an hourly fasion
cd    if(wmelt(iplane).le.0.0)wmelt(iplane)=0.00
c
      If (wmelt(iplane).ge.0) then
cd    End modifying
c
      x = wmelt(iplane) * 1000.0 / densg(iplane)

      if(x .ge. snodpy(iplane))
     1  wmelt(iplane) = snodpy(iplane) * (densg(iplane) / 1000.0)

cd      if(wmelt(iplane).lt.0.0) wmelt(iplane) = 0.0
      endif

cd      write(62, 1000) sdate, hour, year, amelt,bmelt,cmelt,dmelt,
cd     1            cancov(iplane),hradmj,vwmph,adj
1000  format(1x, 3i6, 8e12.2)
c
      return
      end
