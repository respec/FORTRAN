      subroutine tmpadj(hour,halfdy)
c
c     +++PURPOSE+++
c     This function adjusts the surface temperature on an hourly
c     basis by using the daily minumum and maximum temperatures.
c     It also uses a similar method for computing the hourly
c     dewpoint based on the daily average.
c
c     Authors(s):  John Witte, UofMn WCES @ USDA-ARS-NCSRL
c     Date: 04/01/93

c     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                  August 1994
cd    The program is modiffied by S. Dun Aug 14, 2008 to direcly use SI units
c
c     +++ARGUMENT DECLARATIONS+++
      real    halfdy
      integer hour
c
c     +++ARGUMENT DEFINITIONS+++
c     halfdy - The amount of time from sunrise until noon (hrs).
c     hour   - The hour of the day that we are calculating.
c
c     +++PARAMETERS+++
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxhil.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
c
c     +++COMMON BLOCKS+++
      include  'cclim.inc'
c       read:  tave,vwind,hradmj,hrtemp,radmj
c
      include  'cwint.inc'
c       read:  snodpy(mxplane),tfrdp(mxplan),tthawd(mxplan),surtmp
c              frdp(mxplan),thdp(mxplan),densg(mxplan)
c
      include  'cwater.inc'
c       read:  salb(mxplan),
c
      include  'ccover.inc'
c       read:  canhgt(mxplan)
c
      include  'ccrpout.inc'
c       read:  rrc(mxplan)
c
      include  'cstruc.inc'
c       read:  iplane
cd      Added by S. Dun, Nov. 21, 2006
      include 'CANGIE.INC'
c      read : solef (solar energy adjust fator for sloped surface)
cd      End adding
c
c     +++LOCAL VARIABLES+++
c     save
      real    alb,rads,lytomj,clouds,ktemp,aemiss,displ,wsrgh,etrgh,
     1        convht,vkcons,denair,hcpair,htwind,radcof,semiss,sbcons,
     2        netrad,grdp,gutdp,sysdep,ksnow,kres,kftill,kfutil,
     3        numer,denom,surfcn,effk,bakup, gtdp, hrtave,factor
c    2        netrad,radadj,grdp,gutdp,sysdep,ksnow,kres,kftill,kfutil,
c
c     +++LOCAL DEFINITIONS+++
c     alb    - Albedo of the top layer for the system (Dec %).
c     rads   - Radiation on a sloping surface (W/m^2).
c     lytomj - Conversn factor of solar radiation (Langly's to MJ/m^2).
c     clouds - Estimated cloud cover (Dec %).
c     ktemp  - Average temperature in degrees Kelvin units.
c     aemiss - Atmospheric emissivity.
c     displ  - Zero-plane displacment, a function of canopy cover (m).
c     wsrgh  - Roughness parameter for wind speed (m).
c     etrgh  - Roughness parameter for energy transfer (m).
c     convht - Convective heat transfer coeff (J/m^3-K).
c     vkcons - Von Karman constant.
c     denair - Approximate density of air (Kg/m^3).
c     hcpair - Heat capacity of air (J/Kg-K).
c     htwind - Height at which wind velocity is measured (m).
c     radcof - Long_wave radiation transfer coefficient (W/m^2-K).
c     semiss - Surface emissivity for all layers.
c     sbcons - Stefan-Bolzman constant (W/m^2-K^4).
c     netrad - Calculated net radiation (W/m^2).
c     radadj - Daily radiation adjusted to hourly (MJ/m^2).
c     grdp   - Depth from soil's surface to first 0 degree isotherm (m).
c     gutdp  - Temperature gradient depth of untilled layer (m).
c     sysdep - Depth of the frozen layer system from surface to
c              first 0 degree isotherm (m).
c     ksnow  -   These are temporary variables representing the thermal
c     kres   --_ conductivity of the snow, residue, frozen tilled and
c     kftill --  frozen untilled layers.  Reason for temps is because
c     kfutil -   if no such layer exists, the values are set to zero.
c     numer  - Temporary numerator.
c     denom  - Temporary denominator.
c     surfcn - Surface conductivity of top layers (W/m-C).
c     effk   - Effective thermal conductivity of the entire layered
c              system to 0 degree isotherm (W/m-C).
c     bakup  - Temporary variable used in simplifying equations.
c      hrtave - hourly air temperature to aviod messing up with daily average temperature stored
c
c     +++DATA INITIALIZATIONS+++
c     data   sbcons/8.1247e-11/
c     data   vkcons/0.4/
c     data   hcpair/101.0/
c     data   denair/1.0/
c     data   htwind/2.0/
c     data   lytomj/0.04184/
c     data   kres/0.0232/
c
c     +++END SPECIFICATIONS+++
cd      sbcons = 8.1247e-11
      sbcons = 5.6697e-8
      vkcons = 0.4
      hcpair = 1012.0
      denair = 1.2
      htwind = 2.0
      lytomj = 0.04184
c
c XXX  WHY is the value for KRES here set to 0.0232, while in
c      subroutine FROST, kres = 0.168 ??????   dcf  2/23/94
c
cd      kres = 0.0232
        kres = 0.05
c       kres = 0.168
        kres = kres*kresf
       
      grdp = 0.0

      alb = salb(iplane)
      if (snodpy(iplane) .gt. 0.01) alb = 0.5

c -- Convert from MJ/m^2/hr to L/min.

cd      rads = (hradmj / 60) * (1 / lytomj)
c -- Convert from MJ/m^2/hr to W/m^2
      rads = (hradmj / 3600) *1E+6

c      Modified by S. Dun, Jan 12, 2007
      clouds = CloudC
c
cd      if (rpoth .lt. 0.0001) then
cd        clouds = 1.0
cd      else
cd        clouds = (1.0 - radmj /rpoth) / 0.7
cd      endif

cd      if (clouds .gt. 0.999) then
cd        clouds = 1.0
cd      endif
cd      End modifying

      call hrtmp(hour,halfdy)

      hrtave = hrtemp
      ktemp = hrtave + 273.16

      semiss = 1.0
      aemiss = (1.0 - 0.84 * clouds) * (1.0 - 0.261 *
     1         exp(7.77e-4  * (hrtave * hrtave))) + (0.84 * clouds)

      displ = 0.77 * canhgt(iplane)
c reza added next line 7/27/94
c      if(displ.gt.htwind)displ=0.77*htwind
c reza added the above line 7/27/94
c fix from John Laflen to prevent divide by zero below
       if(displ.ge.htwind)displ=0.77*htwind
c fix from John Laflen above - 10/2/2001
      if((snodpy(iplane) .lt. 0.01).and.(canhgt(iplane) .gt. 0.0)) then
        wsrgh = 0.13 * canhgt(iplane)
      elseif (snodpy(iplane) .lt. 0.01) then
        wsrgh = rrc(iplane)
      elseif (snodpy(iplane) .gt. canhgt(iplane)) then
        wsrgh = 0.0002
      else
        wsrgh = 0.13 * (canhgt(iplane) - snodpy(iplane))
      endif

      if (wsrgh .lt. 0.001) wsrgh = 0.001
c Reza added 7/27/94
      if(wsrgh.gt..26)wsrgh=0.26
c end Reza 7/27/94
      etrgh = 0.2 * wsrgh

      convht =((vkcons * vkcons) * denair * hcpair) /
     1         (log((htwind - displ + etrgh) / etrgh) *
     2          log((htwind - displ + wsrgh) / wsrgh))

      radcof = 4.0 * semiss * sbcons * (ktemp**3)
c
c XXX  WHY is RADADJ set equal to RADCOF here, then never used???????
c      Is the correct variable being used below (RADCOF) or should
c      RADADJ be used???  Can RADADJ be deleted????   dcf   2/22/94
c
c     radadj = radcof

cd      netrad = (1.0 - alb)*rads + (aemiss - semiss)*sbcons*hrtave**4
      netrad = (1.0 - alb)*rads + (aemiss - semiss)*sbcons*ktemp**4

c -- Now we calculate the gradient depth for both top frost and none...

c -- If average temperature is below zero.

      if (hrtave .lt. 0.0) then

c -- If top frost is present...

        if (tfrdp(iplane) .gt. 0.001) then
          if (thdp(iplane) .gt. 0.001) then
            grdp = 0.0
          else
            grdp = tfrdp(iplane)
          endif
        else

c -- If no top frost is present...

          if (frdp(iplane) .gt. 0.001) then
            if (thdp(iplane) .gt. 0.001) then
              grdp = 0.0
            else
              grdp = frdp(iplane)
            endif
          endif
        endif

c -- The above assumption comes directly out of Flershinger's notes
c -- and assumes frost exists all the way from the frost depth to
c -- the surface.

c -- Now, we consider the gradient depth is avg temperature is above 0.

      else

c -- If top frost is present...

        if (tfrdp(iplane) .gt. 0.001) then
            grdp = thdp(iplane)
        else

c -- If no top frost is present...

          if (frdp(iplane) .gt. 0.001) then
            if (thdp(iplane) .gt. 0.001) then
              grdp = thdp(iplane)
            else
              grdp = 0.0
            endif
          endif
        endif
      endif

      if ((grdp .le. tilld(iplane)) .or. (tilld(iplane) .gt. 1.0)) then
        gtdp = grdp
        gutdp = 0.0
      else
        gtdp = tilld(iplane)
        gutdp = grdp - tilld(iplane)
      endif

      if (gtdp .lt. 0.001)  gtdp = 0.0
      if (gutdp .lt. 0.001) gutdp = 0.0
      if (grdp .lt. 0.001)  grdp = 0.0

      sysdep = snodpy(iplane) + resdep(iplane) + grdp

c -- Now we make sure that each layer exists, if not we simply
c -- set the "tc" value of that layer to 1.0 so that multiplying
c -- by the "tc" will leave the final value unaffected by that
c -- absent layer.

      if (snodpy(iplane) .lt. 0.0001) then
        ksnow = 1.0
      else
c         Considering thermal conductivity of the snow pack
c         model from Sturm et al., 1997
          if (densg(iplane).lt. 156) then
                ksnow = 0.023 + 0.234 * (densg(iplane)/1000.)
          else
                ksnow = 0.138 - 1.01*(densg(iplane)/1000.)
     1                  + 3.233* (densg(iplane)/1000.)** 2
          endif
cd          ksnow = 0.046 + 0.0451 * (densg(iplane)/1000) + 0.3447 *
cd     1            ((densg(iplane)/1000) * (densg(iplane)/1000))
c
          ksnow = ksnow * ksnowf
      endif

      if (resdep(iplane) .lt. 0.0001) kres = 1.0
c
c XXX IS the following right???  Values for KFTILL when gtdp.ge.0.0001
c     were not defined.  I got the value of 1.75 from subroutine FROST
c     REZA ???
      if (gtdp .lt. 0.0001) then
        kftill = 1.0
      else
        kftill = 1.75
      endif
c
c XXX IS the following right???  Values for KFUTIL when gutdp.ge.0.0001
c     were not defined.  I got the value of 2.1 from subroutine FROST
c     REZA ???
      if (gutdp .lt. 0.0001) then
        kfutil = 1.0
      else
        kfutil = 2.1
      endif

c -- Now we use the harmonic mean method to calculate the surface
c -- temperature for the hour.  NOTE that the final calculation of
c -- numerator/denominator has been broken up into two parts... the
c -- "numer" and the "denom".

      numer = (ksnow * kres * kftill * kfutil) *
     1         (snodpy(iplane) + resdep(iplane) + grdp)

      denom = (ksnow * kres * kftill * grdp) +
     1         (ksnow * kres * gtdp * kfutil) +
     2          (ksnow * resdep(iplane) * kftill * kfutil) +
     3           (snodpy(iplane) * kres * kftill * kfutil)

c -- Now we must make sure we have no floating point errors...

      if ((denom .lt. -0.0001) .or. (denom .gt. 0.0001)) then
        surfcn = numer / denom

      else
        surfcn = 0.0
      endif

c -- If the second case is chosen, an error has occurred!

c -- The following line converts from MJoules to Langly's

cd      effk = surfcn / 0.0697333
c       Directly use the SI units
        effk = surfcn 

      if (sysdep .gt. 0.0) then
cd         surtmp(hour) = (netrad+(radcof+convht*(vwind*100))*hrtave+0.0)/
cd     1                   (radcof+convht*(vwind*100)+(effk/sysdep))
         surtmp(hour) = (netrad+(radcof+convht*(vwind))*hrtave)/
     1                   (radcof+convht*(vwind)+(effk/sysdep))
      else
cd         surtmp(hour) = (netrad+(radcof+convht*(vwind*100))*hrtave+0.0)/
cd     1                  (radcof+convht*(vwind*100))
         surtmp(hour) = (netrad+(radcof+convht*(vwind))*hrtave)/
     1                  (radcof+convht*(vwind))
      endif

      return
      end
