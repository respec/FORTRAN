      subroutine evap(elevm)
c
c     + + + PURPOSE + + +
c     Calculate the evaporation from bare soil.  First, compute the
c     amount of potential evaporation as if all the soil were bare
c     (EO); second, prorate EO by the fraction of soil that actually
c     is bare (EAJ) to estimate water demand (EOS); third, determine
c     how much water is actually available to satisfy this demand.
c     Calculations are performed using Ritchie's model.
c
c   Note: this routine is now in SI (meters)
c
c
c     Called from WATBAL
c     Author(s): Williams, Savabi, Meyer
c     Reference in User Guide: Chapter 7.  Also see EPIC MOdel
c               Documentation printed Sept. 1990.  Also see
c               "Microclimate, the Biological Environment" by
c               Norman J. Rosenberg, et. al. 1983.
c
c     Changes:
c             1) Common block UPDATE not used.  Dereferenced.
c             2) Include files PMXTLS.INC and PMXTIL.INC not
c                used.  Dereferenced.
c             3) Combined some equations like:
c                        ramm = ralb1 / xl
c                        eo = 1.28 * ramm * dlt / xx
c                        eo = eo / 1000.0
c                to look like:
c                        eo = 0.00128 * ralb1 * dlt / (xx * xl)
c                This resulted in some minor differences in rounding
c                in TEST-2.
c             4) Eliminated the following local variables:
c                RAMM, TK2, TK4, X1, X2, X3, X4, HC, ZM, ZH, ZOM,
c                ZOH, D1, K, EA, VPD, R1 & R2.
c             5) The line:
c                        hc = 0.5
c                was replaced with:
c                        hc = canhgt(iplane)
c                as per Reza's 8/27/92 instructions.
c             6) Local variable TMPVR1 was introduced to reduce
c                computations required.  To make it agree more
c                closely with the original results, it was made
c                double precision.
c             7) Removed RTO from evaporation calculations.
c             8) The last time the following equation appeared:
c                        tv(iplane)=(s2(iplane)/.0035)**2
c                it was incorrectly entered as:
c                        tv(iplane)=(s2(iplane)/3.5)**2
c                This was corrected following 9/2/92 conversation
c                with Reza Savabi.
c             9) In the following initialization:
c                        data esd/.10/,tv/10*0./,cej/-2.9e-5/
c                '10' was changed to the parameter that value
c                represented (MXPLAN):
c                        data esd/.10/,tv/mxplan*0./,cej/-2.9e-5/
c            10) Savabi changes to use rainfall interception added
c                7/30/93.   dcf
c
c            11) esd, cej replaced since they were constants; tv(mxplan)
c                added to cwater.inc  jca2  8/31/93
c
c            12) added ability to determine what et equation to use
c                depending on the iwind flag. sjl 10/2/93
c
c            13) changed cej value to -0.5 from -2.9e-5
c                in equation 7.2.3
c
c            14) variable eo (daily pet) placed in common block
c                cwater.inc and removed from local variable list
c                2/1/94  jca2
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 08/25/92 - 09/02/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real elevm
c
c     + + + ARGUMENT DEFINITIONS + + +
c     elevm - elevation of the climate station in meters
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxhil.inc'
      include 'pmxtls.inc'
      include 'pmxelm.inc'

c
c     + + + COMMON BLOCKS + + +
c
      include 'cangie.inc'
c       read: radpot
c
      include 'cclim.inc'
c       read: tave, radly, tdpt, iwind
c     modify: vwind
c
      include 'ccrpout.inc'
c     modify: lai
c
      include 'pmxtil.inc'
c
      include 'cupdate.inc'
c     read mon=month
      include 'cobclim.inc'
c       read obmint obmaxt
c
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'cwater.inc'
c       read: salb, cv, tu, nsl, resint(mxplan)
c     modify: ep, es, fin, st, su, j1, j2, s1, s2, et, eo
c
      include 'ccover.inc'
c       read: canhgt(mxplan)
c
c
c     + + + LOCAL VARIABLES + + +
      real eaj, tk, delta, gma, alb, eos, sp, sb, esx, xx, yy, rto,
     1    d1, datd, dlt, ed, ee, eon, etoh, fwv, harrad, hc, pb,
     1    ra, ra1, ralb1, rbo, rc, rhd, rn, rso, ttdd, xl, xxll, zm, zom
      double precision tmpvr1
      integer xitflg, ievap
c
c     + + + LOCAL DEFINITIONS + + +
c     eaj    - soil cover index (actually, the fraction UN-covered.)
c     tk     - daily average air temperature (degree Kelvin)
c     delta  - slope of the saturated vapor pressure
c     gma    - the second part of Priestly Taylor equation
c     alb    - Albedo (fraction)
c     eos    - potential daily evapotranspiration adjusted for
c              cover m/day
c     sp     - soil evaporation - infiltrated water (stage 1)
c     sb     - adjusted soil evaporation for water infiltration
c     esx    - evaporation from the infiltrated water
c     xx     - soil evaporation (potential of stage 1 and 2)
c     yy     - depth of soil layer which provide water for soil
c              evaporation 7.3.1
c     rto    - available water for soil evaporation/m
c     ho     - net radiation, used in the Priestly Taylor equation (unused)
c     aph    - is 1.28, conversion factor ETp/ETeq (not used)
c     xitflg - flag. 1=exit do-140 loop.
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c ************************************************************************
c ** This section of the code computes evapotranspiration (EO).         **
c ************************************************************************
c
c ---- compute the fraction of soil that is uncovered (EAJ)
c      (WEPP Equation 7.2.3)
      eaj = exp(-0.5*(cv+.1))
c
c     ---- convert average daily temp to degrees Kelvin
      tk = tave + 273.
c
c     ... Compute vapor pressure slope, SVP
c     (WEPP Equation 7.2.4)
c     (EPIC Equation 2.53)
c
      delta = exp(21.255-5304.0/tk) * 5304.0 / tk ** 2
      gma = delta / (delta+0.68)
c
c     ---- Compute corrected soil albedo (ALB), using albedo input by user
c     (SALB) and soil cover index (EAJ).
c     (WEPP Eq. 7.2.2)
c
      if (lai(iplane).gt.0.0) then
        alb = 0.23 * (1.0-eaj) + salb(iplane) * eaj
      else
        alb = salb(iplane)
      end if
c
c     ... Compute potential evaporation from bare soil (EO)
c
c     ievap - flag that determines which equation is used
c     for PET calculations.
c     ievap = 1 HARGRAVES
c     ievap = 2 PRIESTLEY-TAYLOR
c     ievap = 3 PENMAN
c     ievap = 4 PENMAN-MONTIETH
c
c     if wind data not available use HARGRAVES equation
c     ---- (Select the HARGRAVES Equation)
c     if(iwind .eq. 1)ievap = 1
c
c     ---- (Select the Priestley-Taylor Equation)
      if (iwind.eq.1) ievap = 2
c
c     ---- (Select the Penman-Montieth Equation)
c     ievap = 4
c
c     ---- (Select the Penman Equation)
c     if wind data available use PENMAN equation
      if (iwind.eq.0) ievap = 3
c
c
c
c
c     ** L0 IF **
c     PRIESTLEY-TAYLOR EQUATION
c     ievap = 1 HARGRAVES
c     ievap = 2 PRIESTLEY-TAYLOR
c     ievap = 3 PENMAN
c     ievap = 4 PENMAN-MONTIETH
c
c     (WEPP Eq. 7.2.1)
c
c***************************************
c     HARGRAVES equation               *
c     iwind=1                            *
c***************************************
      if (ievap.eq.1) then
c       print*,'Hargraves'
        xxll = 2.501 - 0.002361 * tave
        if (radly.le.0.0) then
c
c
c       EOHA not used 12-16-93 02:04pm  sjl
c       eoha = 0.0
        else
          harrad = radly / xxll
c         ---- Note that harrad is extraterrestral radiation mm/day
c
          ttdd = sqrt(obmaxt(mon)-obmint(mon))
          datd = tave + 17.8
          if (datd.lt.0.0) datd = 0.0
          etoh = 0.0023 * harrad * datd * ttdd
c         ------ etoh is daily potential et in mm/day calculated by
c------- Hargraves 1985 equation
c
          eo = etoh / 1000.
c       eo = daily potential et in m/day
        end if
c
c     ** L0 ELSE IF**
      else if (ievap.eq.2) then
c       print*,'Priestley-Taylor'
        eo = 0.00128 * (radly*(1.0-alb)/58.3) * gma
c
cd    Added by S. Dun to test Prestley-Taylor
cd    write(60, 1600) year, mon, day, iplane, eo*1000
1600   format (1x, 4I6, E12.3)
c     calculate PENMAN and PENMAN-MONTIETH common portions
      else
        ra = radly / 23.9
c
c       ------ estimate latent heat of vaporization (XL)
c       (EPIC Equation 2.38)
        xl = 2.501 - 0.0022 * tave
c
c       Estimate Saturation vapor pressure (EE), using the average
c       temperature in degrees Kelvin (TK).
c       (EPIC Equation 2.39)
        ee = 0.1 * exp(54.879-5.029*alog(tk)-6790.5/tk)
c
c       ------ calculate relative humidity (RHD) [fraction] from dewpoint
c       temperature (TDPT) and average temperature (TAVE).
c       (Rosenberg, Equations 5.17 & 5.18)
        rhd = exp((17.27*tdpt)/(237.3+tdpt)) /
     1      exp((17.27*tave)/(237.3+tave))
        if (rhd.gt.1.0) rhd = 1.0
c
c       Calculate the vapor pressure (ED) from the saturation vapor
c       pressure (EE) and the relative humidity (RHD).
c       (EPIC Equation 2.40)
        ed = ee * rhd
c
        ralb1 = ra * (1-alb)
c
c       Estimate the slope of the saturation vapor pressure curve (DLT).
c       (EPIC Equation 2.41)
        dlt = ee * (6790.5/tk-5.029) / tk
c
c       ------ compute barometric pressure in kpa (PB)
c       (Approximation of EPIC Equation 2.43)
        pb = 101.3 - (0.01055*elevm)
c
c       ------ compute psychrometric constant kpa/c (GMA)
c       (EPIC Equation 2.42)
        gma = 0.00066 * pb
c
        xx = dlt + gma
c
      end if
c
      if (ievap.eq.3) then
c       print*,'Penman'
c       radpot is calculated in winter routines only in the winter
c       it needs to be calculated every day therefore the calculation
c       in sunmap must be called from contin not  in winter
c       as in this version JEF 3/20/91
        rso = radpot / 23.9
c
c       -------- calculate mean daily wind speed (vwind)
c       (Modified EPIC Equation 2.50)
c       fwv = 2.7 + 0.537 * vwind
c
c       NOTE - The value 1.63 is used below because the wind generated
c       by CLIGEN is for a 10 meter height (not at 2 meters which would
c       use the 0.537 value above).   dcf  6/2/94
c
        fwv = 2.7 + 1.63 * vwind
c
c       -------- estimate the net outgoing longwave radiation (RBO)
c       (EPIC Equation 2.46)
        rbo = (0.34-0.14*sqrt(ed)) * 4.9e-9 * tk ** 4
c
c       -------- calculate net radiation (RN)
c       (EPIC Equation 2.45)
        rn = ralb1 - rbo * (0.9*ra/rso+0.1)
        if (rn.le.0.0) rn = 0.0
c
c       -------- Compute potential evaporation (EO)
c       (Approximation of EPIC Equation 2.37, assuming the soil
c       flux is close to zero, and thus negligible.)
        eo = ((dlt*rn)/xl+gma*fwv*(ee-ed)) * 0.001 / xx

cd    write(60, 1500) year, mon, day, iplane, eo*1000
1500  format(1x, 4I6, f8.2)
c
c     ** L0 ELSE-IF **
c     PENMAN-MONTEITH EQUATION
c     (This is the only one of the four equations that takes plants
c     (and therefore, evapotranspiration, into consideration.)
      else if (ievap.eq.4) then
        if (lai(iplane).lt.1.0) then
          rc = 1.0
        else
          rc = 200.0 / lai(iplane)
        end if
c
        if (vwind.le.0.0) vwind = 1.0e-10
c
c
c       Logically Equivalent to:
c
c
c       changed back to old code - hc=0.5, because when canhgt is very
c       small or zero - causes et to go negative with Penman-Montieth
c       equation - (using strips.dat,soil2.dat,slope2.dat,clim.dat)
        hc = canhgt(iplane)
        if (hc.lt.1) hc = 0.01
        zm = 2.0
        zom = 0.123 * hc
        d1 = 0.67 * hc
        tmpvr1 = alog((zm-d1)/zom)
        ra1 = tmpvr1 * (tmpvr1+1.0) / (0.1681*vwind)
c
        eon = (dlt*ralb1+(gma*(1710.-6.85*tave)*(ee-ed))/ra1) / (dlt+gma
     1      *(1.+rc/ra1))
        eo = 0.00001712 * eon
c
c
c     ** L0 ENDIF **
      end if
c
c
c     ... Compute potential evaporation from bare soil (EOS), bare soil
c     water demand (EO), and fraction of soil bare (EAJ).
      eos = eo * eaj
c
c     7/27/93 total intercepted rain by residue needs
c     to evaporate first     reza
c
      eos = eos - resint(iplane)
      if (eos.le.0.0) eos = 0.0
c
c     ************************************************************************
c     ** The following section computes how much soil water is available to **
c     ** satisfy bare soil evaporation potential (EOS).  The physical pro-  **
c     ** cess is assumed to occur in two stages.  The first (stage 1) is    **
c     ** limited by the amount of energy stiking the soil surface.  During  **
c     ** stage 1 the amount of water evaporated is linearly related to the  **
c     ** amount of energy striking the soil surface.  During the second     **
c     ** stage (stage 2), the relationship becomes non-linear.  Increas-    **
c     ** ingly more energy is required per unit of water evaporated; ie,    **
c     ** evaporation is limited by the soil water content (the amount of    **
c     ** water available to be evaporated).  The variable TU represents the **
c     ** value on the current soil, at which this transition between stages **
c     ** occurs.  Presently TU is set to 6 mm for all soil types.  The var- **
c     ** iable S1 represents cumulative evaporation.  It is reset whenever  **
c     ** precipitation occurs.                                              **
c     ************************************************************************
c
c
c     ** M0 IF **
c     If the cumulative water evaporated doesn't go beyond stage 1....
      if (s1(iplane).lt.tu(iplane)) then
c       ------ compute adjusted stage 1 evaporation by subtracting infiltration.
        s2(iplane) = 0.0
        sp = s1(iplane) - fin
c
c       ------ if adjusted evaporation is greater than infiltration, set
c       stage 1 evaporation (S1) equal to the difference.
        if (sp.gt.0.0) then
          s1(iplane) = sp
        else
          s1(iplane) = 0.0
        end if
c
c       ------ add evapotranspiration (EOS) to stage 1 evaporation (S1)
        s1(iplane) = s1(iplane) + eos
c
c       ------ compute the water deficit (SU) by subtracting the upper limit
c       for soil evaporation (TU) from the stage 1 evaporation (S1).
        su = s1(iplane) - tu(iplane)
c
c       If there is a water deficit, compute stage 2 evaporation.
        if (su.gt.0) then
c         -------- set soil evaporation (ES) equal to evapotranspiration (EOS)
c         minus 0.4 of the water deficit (SU).
          es(iplane) = eos - .4 * su
c         -------- set stage 2 evaporation (S2) equal to 0.6 of the water deficit.
          s2(iplane) = .6 * su
c         -- XXX -- What does TV represent? -- CRM -- 9/2/92.
          tv(iplane) = (s2(iplane)/.0035) ** 2
c
c       If there is NOT a water deficit....
        else
c         ------- set soil evaporation (ES) equal to adjusted evapotranspiration
          es(iplane) = eos
        end if
c
c
c     ** M0 ELSE **
c     If the cumulative water evaporated DOES go beyond stage 1....
      else
c
        sb = fin - s2(iplane)
c
c       ** M1 IF **
c       Is stage 2 evap > 0?
c
        if (sb.lt.0) then
          tv(iplane) = tv(iplane) + 1
          es(iplane) = .0035 * sqrt(tv(iplane)) - s2(iplane)
c
c         If there is infiltration....
          if (fin.gt.0) then
            esx = 0.8 * fin
c           ---------- if soil evaporation (ES) exceeds 0.8 infiltration (FIN)
            if (es(iplane).gt.esx) esx = es(iplane) + fin
            if (esx.gt.eos) esx = eos
            es(iplane) = esx
c
c         Otherwise, if soil evaporation (ES) is greater than the
c         adjusted evapotranspiration (EOS)....
          else if (es(iplane).gt.eos) then
c           ---------- set soil evap. equal to evapo-trans.
            es(iplane) = eos
          end if
c
          s2(iplane) = s2(iplane) + es(iplane) - fin
          tv(iplane) = (s2(iplane)/.0035) ** 2
c
c       ** M1 ELSE **
c       Stage 2 evap <= 0
c
        else
          fin = sb
          s1(iplane) = tu(iplane) - fin
          tv(iplane) = 0.0
          s2(iplane) = 0.0
          if (s1(iplane).lt.0) s1(iplane) = 0.0
          s1(iplane) = s1(iplane) + eos
          su = s1(iplane) - tu(iplane)
c
c         Soil water avail for evap > 0?
c
          if (su.gt.0) then
            es(iplane) = eos - .4 * su
            s2(iplane) = .6 * su
            tv(iplane) = (s2(iplane)/.0035) ** 2
          else
            es(iplane) = eos
          end if
c
c       ** M1 ENDIF **
        end if
c
c     ** M0 ENDIF **
      end if
c
c     Add water intercepted by residue back onto soil evaporation
c
      es(iplane) = es(iplane) + resint(iplane)
c
      if (es(iplane).le.0) es(iplane) = 0.0
c
c     Compute plant transpiration
c     (WEPP Equation 7.2.10)
c     (EPIC Equation 2.56 & 2.57)
c
cd    Added by S. Dun, June 16, 2007 for debug
c     Write(61, 1505) sdate, year, iplane, resint(iplane),
c     1           plaint(iplane), pintlv(iplane)
1505  format(3I6, 3E12.3)
cd    End adding
c
      if (lai(iplane).gt.3.0) then
        ep(iplane) = eo
      else
        ep(iplane) = lai(iplane) * eo / 3.0
      end if
c
      et(iplane) = es(iplane) + ep(iplane)
      if (eo.lt.et(iplane)) then
        et(iplane) = eo
        es(iplane) = et(iplane) - ep(iplane)
      end if
      xx = es(iplane)
      if (resint(iplane).ge.0.0) then
        xx = es(iplane) - resint(iplane)
        resint(iplane) = 0.0    
      end if
      if (xx.lt.0.0) then        
        st(1,iplane) = st(1,iplane)-xx
        xx = 0.0        
        goto 60
      endif

c
c     Evaporate maximum potential water from each soil layer.
c
      xitflg = 0
      j2 = 0
   10 continue
      j2 = j2 + 1
c     ------ if cum. soil depth exceeds "soil evaporated depth"...
      if (solthk(j2,iplane).gt.0.10) then
        j1 = j2 - 1
        yy = 0.
        if (j1.gt.0) yy = solthk(j1,iplane)
        rto = st(j2,iplane) * (0.10-yy) / (solthk(j2,iplane)-yy)
c
        if (rto.gt.0) then
          if (rto.gt.xx) then
             st(j2,iplane) = st(j2,iplane) - xx
             xx = 0.0
             if (st(j2,iplane).lt.1e-10) st(j2,iplane) = 0.00
           else
             xx = xx - rto
             st(j2,iplane) = st(j2,iplane) - rto
              if (st(j2,iplane).lt.1e-10) st(j2,iplane) = 0.00
cd              es(iplane) = es(iplane) - xx
cd              et(iplane) = et(iplane) - xx
           end if
        endif
        xitflg = 1
c
c     ------ if water available in soil layer exceeds potential soil evap...
      else if (st(j2,iplane).gt.xx) then
        st(j2,iplane) = st(j2,iplane) - xx
        xx = 0.0
        if (st(j2,iplane).lt.1e-10) st(j2,iplane) = 0.00
        xitflg = 1
c
      else
        xx = xx - st(j2,iplane)
        st(j2,iplane) = 0.
      end if
c     ------ (force immediate exit from loop.)
      if (xitflg.ne.0) j2 = nsl(iplane)
      if (j2.lt.nsl(iplane).and.xitflg.eq.0) go to 10
c
cd      if (xitflg.eq.0) then
        es(iplane) = es(iplane) - xx
        et(iplane) = et(iplane) - xx
cd      end if
c
   60 continue
c
 1000    format(1x,i5,4f15.6)
 2000    format(1x,i5,6f12.3)
c
      return
      end
