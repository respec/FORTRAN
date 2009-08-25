      subroutine evappm(elevm,nowcrp)
c
c     + + + PURPOSE + + +
c     Using Penman-Monteith Method to Calculate the evapotranspirationation  
c
c   Note: this routine is now in SI (meters)
c
c
c     Called from WATBAL
c     Author(s): 
c     Reference: FAO 56
c
c     Date recoded: 04/10/2003 
c     Recoded by: Shuhui Dun.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real elevm
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     elevm - elevation of the climate station in meters
c    nowcrp - current crop
c
c     + + + PARAMETERS + + +
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'pmxhil.inc'
      include 'pmxtls.inc'
cx    add by Arthur,June,00
      include 'pmxelm.inc'
      include 'pxstep.inc'
      include 'pmxres.inc'
      include 'pmxcrp.inc'
      include 'PMXSRG.INC'
cx     end
c     reza added the next 2 lines 8/18/93
      include 'pmxtil.inc'
c
c     + + + COMMON BLOCKS + + + 
c
      include 'cclim.inc'
c       read: tave, radly, tdpt, iwind
c     modify: vwind
c
      include 'cangie.inc'
c       read: radpot
c
      include 'pntype.inc'
      include 'ccrpprm.inc'
c       read: itype,jdplt,jdharv,jdsene
c
      include 'cerrid.inc'
c        read: crpnam 
c
      include 'ccrpout.inc'
c     modify: lai,rescov,rtd
c
      include 'ccover.inc'
c       read: canhgt,cancov, gcover
c
      include 'ccrpvr3.inc'
c       read: sumgdd,gddmax,decfct,dropfc
c
      include 'ccrpet.inc'
c    read: kcb,rawp
c    read and save last kcb
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'cwater.inc'
c    read: thetdr,thetfc
c       read: salb, cv, tu, nsl, resint(mxplan)
c     modify: ep, es, fin, st, su, j1, j2, s1, s2, et, eo    

      include 'cupdate.inc'
c     read mon=month
      include 'cobclim.inc'
c       read obmint obmaxt
c
      include 'chydrol.inc'
cx     read: rain
cx
      include 'cwint.inc'
cx     read: wmelt
cx
      include 'cirriga.inc'
cx     read: irdept
cx
      include 'cirfur1.inc'
cx     read: irapld
c
c     + + + LOCAL VARIABLES + + +
      real gma,alb,xx,yy,rto,dlt,ed,ee,fwv,pb,
     1    ra, ralb1, rbo, rhd, rn, rso, xl,
     1    emaxt,emint,kcbadj,TEW,REW,wfevp,
     1    etke,etkr,etks,TAW,RAW,wftrp,etorc,etcsc,rawpaj,
     1    epdp,tpdp,etcadj,kcmax,eaj,kecon,potes,bpotes 
      integer xitflg,crpindx,i
c
c     + + + LOCAL DEFINITIONS + + +
c     eaj    - soil cover index (actually, the fraction UN-covered.)
c     tk     - daily average air temperature (degree Kelvin)
c     dlt  - slope of the saturated vapor pressure
c     gma    - the second part of Priestly Taylor equation
c     alb    - Albedo (fraction)
c     xx     - soil evaporation (potential of stage 1 and 2)
c     yy     - depth of soil layer which provide water for soil
c              evaporation 7.3.1
c     rto    - available water for soil evaporation/m
c     ho     - net radiation, used in the Priestly Taylor equation (unused)
c     aph    - is 1.28, conversion factor ETp/ETeq (not used)
c     xitflg - flag. 1=exit do-140 loop.
c     fphu   - ratio of total growing degree days received so far, to
c              total growing degree days expected at senescence (0-1).
c    rhd       - Relative humidity.
c    ed     - saturated water vapor pressure at dew point
c    ee     - daily average water vapor pressure.
c    emaxt  - saturated water vapor pressure at maximum temperature.
c    emint  - saturated water vapor pressure at minimum temperature.
c    kcbadj - adjust basal crop coefficient.
c    strstg - current crop development stage start date.
c    stgend - current crop development stage end date.
c    TEW       - maximum water that can be evaporated from soil (mm)
c    REW    - cumulative evaporation at the end of stage 1 (mm)
c    wfevp  - water available at the begining of the day for evaporation(mm)
c             wetting events are assumed occour before evaporation.
c    etke   - soil evaporation coefficient
c    etkr   - evaporation reduction coefficient
c    etfew  - exposed and wetted soil fraction
c    etfw   - soil fraction wetted by wetting events.
c    TAW       - total root zone soil water that can be extracted by crop (mm).
c    RAW    - raidly soil water in the root zone for crop to extract (mm).
c    wftrp  - water available for traspiration at the begining of the day
c             soil water recharge is countered next day.
c    etorc   - crop ET under refference condition (mm).
c    etcsc  - crop ET under standard condition (mm).
c    rawpaj - adjusted water stress coefficient.
c    etcadj - adjusted crop ET (mm)
c    kcbcon - basal crop coefficient.
c    kcmax  - maximum value of Kc following rain or irrigation
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
c      (WEPP Equation 7.2.3) cv : residue amount
cd      eaj = exp(-0.5*(cv+.1))
c
c     ---- Compute corrected soil albedo (ALB), using albedo input by user
c     (SALB) and soil cover index (EAJ).
c     (WEPP Eq. 7.2.2)
c
cd      if (lai(iplane).gt.0.0) then
cd        alb = 0.23 * (1.0-eaj) + salb(iplane) * eaj
cd      else
cd        alb = salb(iplane)
cd      end if

c ***************************************
c     S. Dun 11/10/2002 
c        FAO PENMAN-MONTIETH reference ET0
c
      kcmax = 0.0
      eaj = 0.0
      kecon = 0.0
      potes = 0.0
      bpotes = 0.0
c
      alb = 0.23 
c    albedo of hypothetical grass surface
      ra = radly / 23.9
      xl = 2.501 - 0.002361 * tave
      ralb1 = ra * (1-alb)
c
c    FAO 56 page 40.
      ed = 0.6108 * exp(17.27*tdpt/(tdpt+237.3))
      emaxt = 0.6108 * exp(17.27*tmax/(tmax+237.3))
      emint = 0.6108 * exp(17.27*tmin/(tmin+237.3))
      ee = 0.5*(emaxt+emint)
c
c    radpot is calculated in winter routines only in the winter
c    it needs to be calculated every day therefore the calculation
c    in sunmap must be called from contin not  in winter
c    as in this version JEF 3/20/91
      rso = radpot / 23.9
c
c    -------- estimate the net outgoing longwave radiation (RBO)
      rbo = (0.34-0.14*sqrt(ed)) * 4.9e-9 * ((tmax+273.2) ** 4 +
     1    (tmin+273.2)**4)/2*(1.35*ra/rso-0.35)
c
c    -------- calculate net radiation (RN)
      rn = ralb1 - rbo 
c
c    NOTE - The wind generated
c    by CLIGEN is for a 10 meter height 
c
      fwv = vwind    * 4.87/(alog(67.8*10.0-5.42))
c       FAO 56 (47)
      dlt = 4098.0/(tave+237.3)**2 *
     1    0.6108 * exp(17.27*tave/(tave+237.3))
c       FAO 56 (13)
      pb = 101.3*(1.0- 0.0065*elevm/293.0)**5.26
c       FAO 56 (7)
c    ------ compute psychrometric constant kpa/c (GMA)
c   
      gma = 0.000665 * pb
c       FAO 56 (8)
c
c    the unit of eo is (m/d)         
      etorc = (0.408 * dlt * rn + 
     1    gma*(900.0/(tave+273))*(ee-ed)*fwv)
     1    /(dlt+ gma *(1.+ 0.34* fwv))
c       FAO 56 (6)
c
      eo = etorc*0.001
c    Coded at 04/10/2003
c    Dual crop coefficient approach to crop evapotranspiration
c    under standard condition
c
      crpindx = itype(nowcrp,iplane)
cd      if(sdate.gt.jdplt(crpindx,iplane).and.
cd     1        sdate.lt.jdharv(crpindx,iplane)) then
c
      rhd = ed/emaxt*100
c
      if((lai(iplane).gt.0.0).and.(rtd(iplane).gt.0.0)) then
c       Adjustment of middle season crop coefficient
c       for climate condition
        kcbadj = kcb(crpindx) +(0.04*(fwv-2.)-0.004*(rhd-45))*
     1                (canhgt(iplane)/3.)**0.3
c       FAO 56 (62)
      else
cd        crpstg = 0
        kcbadj = 0            
      endif
cd    According to Stöckle et al
c    basal crop coefficient
      kcbcon = kcbadj*(1-exp(-0.45*lai(iplane)))
c    soil evaporation coefficient
      if(kcbadj.gt.0.0) then
        etke = kcbadj*exp(-0.45*lai(iplane))
      else
        etke = 1.2
      endif 
c    
c    calculation of soil evaporation reduction coefficient 
c    In the TEW formula 0.1m is the depth of the surface soil layer 
c        that is subject to drying by way of evaporation 0.10~0.15m.
c    REW is a formula from linear regression of Table 19 (FAO56 p144)
c    Notes for previous day water depletion calculation:
c        st(mxnsl,mxplan) is available water content per soil layer(m) 
c        at the end of previous day. This value is from the difference 
c        between soil water content and wilting point soil water 
c        content. Because the available water for evaporation is from
c        the difference between water content and half of wilting 
c        point water content. So we add half of wilting point water
c        content to calculate available water for using st value. 
c                        
      TEW = 0.0
      REW = 0.0
      wfevp = 0.0
c
      epdp = 0.1
c            
      do 10 i = 1, nsl(iplane)
        if (i.eq.1) then
            yy = 0.0
        else
            yy = solthk(i-1,iplane)
        endif            
        if (solthk(i,iplane).lt.epdp) then  
            TEW = TEW+1000*(thetfc(i,iplane)-
     1            0.3*thetdr(i,iplane))*(solthk(i,iplane)-yy)
            REW    = REW+(57.856*(thetfc(i,iplane)-
     1            thetdr(i,iplane))+0.280)*(solthk(i,iplane)-yy)/epdp
            wfevp = wfevp+1000*(st(i,iplane) +
     1            0.7*thetdr(i,iplane)*(solthk(i,iplane)-yy)) 
c           Frozen soil effects was not considered yet.   
        else
            TEW =TEW + 1000*(thetfc(i,iplane)-
     1            0.3*thetdr(i,iplane))*(epdp-yy)
            REW    = REW+(57.856*(thetfc(i,iplane)-thetdr(i,iplane))
     1            +0.280)*(epdp-yy)/epdp
            wfevp = wfevp+1000*(st(i,iplane)*
     1            (epdp-yy)/(solthk(i,iplane)-yy)+
     1            0.7*thetdr(i,iplane)*(epdp-yy))
            goto 20
        endif
10    continue
20    continue
c
c    adding in wetting event influence before evaporation
      wfevp = wfevp + fin*1000
c
      if ((TEW-wfevp).le.REW) then
        etkr = 1.0
      else
        etkr = (wfevp/(TEW - REW))**2
      endif
c
c    Soil water stress on crop
c    Double count the avaiable water for evaporation and transpiration
c    for the top 0.1~0.15m. But I think it get correction when water 
c    is actually substract from its availvabel water.
      TAW = 0.0
      RAW = 0.0
      wftrp = 0.0
c
      tpdp = rtd(iplane)
      if (solthk(nsl(iplane),iplane).lt.tpdp) then
        tpdp = solthk(nsl(iplane),iplane)
      endif
c            
      do 30 i = 1, nsl(iplane)
        if (i.eq.1) then
            yy = 0.0
        else
            yy = solthk(i-1,iplane)
        endif
        if (solthk(i,iplane).lt.tpdp) then  
            TAW = TAW+1000*(thetfc(i,iplane)-
     1            thetdr(i,iplane))*(solthk(i,iplane)-yy)                
            wftrp = wftrp+st(i,iplane)*1000    
        else
            TAW =TAW + 1000*(thetfc(i,iplane)-
     1            thetdr(i,iplane))*(tpdp-yy)                
            wftrp = wfevp+st(i,iplane)*1000*
     1            (tpdp-yy)/(solthk(i,iplane)-yy)
            goto 40
        endif
30    continue
40    continue
c
      etcsc = kcbadj*etorc
      rawpaj = rawp(crpindx) +0.04*(5.-etcsc) 
c       FAO 56 (83)
      RAW = rawpaj*TAW
c
      if ((TAW-wftrp).le.RAW) then
        etks = 1.0
      else
        etks = wftrp/(TAW - RAW)
      endif
cd      etcadj = etorc*(etks*kcbadj+etke*etkr)
c
c ************************************************************************
c ** This section of the code computes soil evaporation with residue    **
c ************************************************************************
cd      es(iplane) =  etorc*etke*etkr*0.001
c
      potes = etorc*etke*0.001
      if (potes.gt.resint(iplane)) then
        bpotes = potes - resint(iplane)
c ---- compute the fraction of soil that is uncovered (EAJ)
c      (WEPP Equation 7.2.3) cv : residue amount
        eaj = exp(-0.5*(cv+.1))
        kcmax = 1.2 +(0.04*(fwv-2.)-0.004*(rhd-45))*
     1       (canhgt(iplane)/3.)**0.3
c
        kecon = min(etke*etkr, eaj*kcmax)
        es(iplane) = kecon*bpotes/etke + resint(iplane)
      else
        es(iplane) = potes
      endif
c
      ep(iplane) =  etorc*etks*kcbcon*0.001
      et(iplane) = es(iplane) + ep(iplane)
c
c    water redistribution in the soil
      xx = es(iplane)
cx    Added by Arthur Xu, modified by S.Dun
cx    the condition is added to correct the water balance problem
      if (resint(iplane).ge.0.0) then
        xx = es(iplane) - resint(iplane)
        resint(iplane) = 0.0    
      end if
      if (xx.lt.0.0) then        
        st(1,iplane) = st(1,iplane)-xx
        xx = 0.0        
        goto 60
      endif
cx    <---------------------------------------------->
c     Evaporate maximum potential water from each soil layer.
cx    
      xitflg = 0
      j2 = 0
   50    continue
      j2 = j2 + 1
c     ------ if cum. soil depth exceeds "soil evaporated depth"...
      if (solthk(j2,iplane).gt.epdp) then
        j1 = j2 - 1
        yy = 0.
        if (j1.gt.0) yy = solthk(j1,iplane)
        rto = st(j2,iplane) * (epdp-yy) / (solthk(j2,iplane)-yy)
c
        if (rto.gt.xx) then
            st(j2,iplane) = st(j2,iplane) - xx
            if (st(j2,iplane).lt.1e-10) st(j2,iplane) = 0.00
            xx = 0.0
        else
            xx = xx - rto
            st(j2,iplane) = st(j2,iplane) - rto
            if (st(j2,iplane).lt.1e-10) st(j2,iplane) = 0.00
cd            es(iplane) = es(iplane) - xx
cd            et(iplane) = et(iplane) - xx
        end if
        xitflg = 1
c
c    ------ if water available in soil layer exceeds potential soil evap...
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
      if (j2.lt.nsl(iplane).and.xitflg.eq.0) go to 50
c
cd      if (xitflg.eq.0) then
        es(iplane) = es(iplane) - xx
        et(iplane) = et(iplane) - xx
cd      end if
60    continue
c
cd     added by DSH 2002/09/18
c     write (60,1000) sdate,et(iplane),es(iplane),ep(iplane),
c     1                eo, kcb(crpindx),rawp(crpindx)
cd    write(60,2000) sdate,rn,cancov(iplane),rescov(iplane),
cd    1        sumgdd(iplane),gddmax(crpindx),lai(iplane)    
1000    format(1x,i5,6f15.6)
c2000    format(1x,i5,6f12.3)
      return
      end
