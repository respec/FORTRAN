      subroutine impeo(elevm,eo)
c
c     + + + PURPOSE + + +
c
c     SR IMPEO calculates the evaporation from bare soil in
c     an impoundment.  The amount of potential evaporation is
c     computed as if all the soil were bare (EO). Calculations
c     are performed using Ritchie's ET model.
c
c     This routine is based upon SR EVAP, simplified for
c     impoundments, and now in SI (meters).
c
c     Called from: SR WSHIMP
c     Author(s): Baffaut
c     Reference in User Guide:
c
c     Chapter 7.   See also EPIC model documentation (Sept. 1990)
c     and "Microclimate, the Biological Environment" by Rosenberg
c     et al., 1983.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxhil.inc'
      include 'pmxtls.inc'
      include 'pmxelm.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real elevm,eo
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     elevm - elevation of climate station in meters
c     eo - calculated potential evaporation
c
c     + + + COMMON BLOCKS + + +
c
      include 'cangie.inc'
c     read: radpot
c
      include 'cclim.inc'
c     read: tave, radly, tdpt, iwind
c     modify: vwind
c
      include 'pmxtil.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real tk, delta, gma, alb, xx, dlt, ed, ee, fwv, pb, ra,
     1    ralb1, rbo, rhd, rn, rso, xl
      integer ievap
c
c     + + + LOCAL DEFINITIONS + + +
c
c     tk     - daily average air temperature (degrees Kelvin)
c     delta  - slope of the saturated vapor pressure
c     gma    - the second part of Priestly Taylor equation
c     alb    - soil albedo (fraction)
c     xx     - soil evaporation (potential of stage 1 and 2)
c     dlt    - slope of the saturation vapor pressure curve
c     ed     - vapor pressure
c     ee     - saturation vapor pressure
c     fwv    - mean daily wind speed
c     pb     - baromatic pressure
c     ra     - radiation
c     ralb1  - radiation
c     rbo    - net outgoing longwave radiation
c     rhd    - relative humidity
c     rn     - net radiation
c     rso    -
c     xl     - latent heat of vaporization
c     ievap  - defined below
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     convert average daily temp to degrees Kelvin
c
      tk = tave + 273.0
c
c     compute vapor pressure slope (svp) (WEPP Equation 7.2.4
c     or EPIC Equation 2.53)
c
      delta = exp(21.255-5304.0/tk) * 5304.0 / tk ** 2
      gma = delta / (delta+0.68)
c
c     compute corrected soil albedo (alb) assuming lai = 0
c
      alb = 0.25
c
c     compute potential evaporation from bare soil (eo)
c
c     ievap - flag that determines which equation is used for
c     PET calculations
c
c     ievap = 2 for PRIESTLEY-TAYLOR  (wind data not available)
c     ievap = 3 for PENMAN (wind data available)
c
c     if wind data not available use the PRIESTLEY-TAYLOR equation
c
      if (iwind.eq.1) ievap = 2
c
c     if wind data available use the PENMAN equation
c
      if (iwind.eq.0) ievap = 3
c
c     priestley-taylor equation
c
      if (ievap.eq.2) then
c
        eo = 0.00128 * (radly*(1.0-alb)/58.3) * gma
c
      else
c
c       penman equation
c
        ra = radly / 23.9
c
c       estimate latent heat of vaporization (xl)
c       (EPIC Equation 2.38)
c
        xl = 2.501 - 0.0022 * tave
c
c       estimate saturation vapor pressure (ee) using the average
c       temperature in degrees Kelvin (tk) (EPIC Equation 2.39)
c
        ee = 0.1 * exp(54.879-5.029*alog(tk)-6790.5/tk)
c
c       calculate relative humidity (rhd) fraction from dewpoint
c       temperature (tdpt) and average temperature (tave)
c       (Rosenberg et al. (1983), Equations 5.17 & 5.18)
c
        rhd = exp((17.27*tdpt)/(237.3+tdpt)) /
     1      exp((17.27*tave)/(237.3+tave))
c
        if (rhd.gt.1.0) rhd = 1.0
c
c       calculate the vapor pressure (ed) from the saturation vapor
c       pressure (ee) and the relative humidity (rhd)
c       (EPIC Equation 2.40)
c
        ed = ee * rhd
        ralb1 = ra * (1-alb)
c
c       estimate the slope of the saturation vapor pressure
c       curve (dlt)  (EPIC Equation 2.41)
c
        dlt = ee * (6790.5/tk-5.029) / tk
c
c       compute barometric pressure in kpa (pb)
c       (Approximation of EPIC Equation 2.43)
c
        pb = 101.3 - (0.01055*elevm)
c
c       compute psychrometric constant kpa/c (gma)
c       (EPIC Equation 2.42)
c
        gma = 0.00066 * pb
        xx = dlt + gma
        rso = radpot / 23.9
c
c       calculate mean daily wind speed (vwind)
c       (Modified EPIC Equation 2.50)
c
c       the value 1.63 is used below because the wind generated
c       by CLIGEN is for a 10 meter height (not for a 2 meter
c       height as in EPIC equation 2.50)
c
        fwv = 2.7 + 1.63 * vwind
c
c       estimate the net outgoing longwave radiation (rbo)
c       (EPIC Equation 2.46)
c
        rbo = (0.34-0.14*sqrt(ed)) * 4.9e-9 * tk ** 4
c
c       calculate net radiation (rn)
c       (EPIC Equation 2.45)
c
        rn = ralb1 - rbo * (0.9*ra/rso+0.1)
        if (rn.le.0.0) rn = 0.0
c
c       compute potential evaporation (eo)
c
c       (approximation of EPIC Equation 2.37, assuming the
c       soil flux is close to zero, and thus negligible)
c
        eo = ((dlt*rn)/xl+gma*fwv*(ee-ed)) * 0.001 / xx
c
      end if
c
      return
      end
