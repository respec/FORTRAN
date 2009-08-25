      subroutine scon(uselan)
c
c     + + + PURPOSE + + +
c     Initializes soil parameters which are to remain constant
c     throught the simulation, i.e., parameters which are
c     independent of changes in bulk density.
c
c     Called from CONTIN.
c     Author(s): Savabi,Flanagan,Nearing
c     Reference in User Guide:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxnsl.inc'
      include 'ptilty.inc'
      include 'pmxpnd.inc'
      include 'pmxres.inc'
      include 'pmxpln.inc'
      include 'pmxgrz.inc'
      include 'pmxcrp.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
      include 'pmxelm.inc'
      include 'ctemp.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer uselan
c
c     + + + ARGUMENT DEFINITIONS + + +
c     uselan - type of land use (1-crop,2-range,3-forest)
c
c     + + + COMMON BLOCKS + + +
      include 'ccons.inc'
c
c********************************************************************
c                                                                   *
c    cons variables updated                                         *
c        bddry(mxnsl,mxplan),bdcons(mxnsl,mxplan),
c        coca(mxnsl,mxplan),thtdk1(mxnsl,mxplan),
c        thtdk2(mxnsl,mxplan),thetfk(mxnsl,mxplan),
c        wrdk(mxnsl,mxplan),ck1(mxnsl,mxplan),
c        ck2(mxnsl,mxplan),rre(mxplan)
c                                                                   *
c********************************************************************
c
      include 'ccover.inc'
c
      include 'ccrpout.inc'
c
c********************************************************************
c                                                                   *
c    crpout variables updated                                       *
c         bd(mxnsl,mxplan),lai(mxplan),rrc(mxplan)
c                                                                   *
c********************************************************************
c
c
      include 'ccrpprm.inc'
c
      include 'cke.inc'
c      modify ksflag
c
      include 'ckrcon.inc'
c      modify: krcrat(mxplan), bconsd(mxplan)
c
      include 'cparame.inc'
c
c********************************************************************
c                                                                   *
c    parame variables updated                                       *
c        por(mxnsl,mxplan)
c                                                                   *
c********************************************************************
c
      include 'cparval.inc'
c
c********************************************************************
c                                                                   *
c    parval variables updated                                       *
c        shcrit(mxplan)
c                                                                   *
c********************************************************************
c
c
      include 'crinpt1a.inc'
      include 'crinpt6.inc'
c
      include 'crout.inc'
c      input: rooty(mxnsl,mxplan)
c
      include 'ctillge.inc'
c
c********************************************************************
c                                                                   *
c tillge variables updated                                          *
c   trans                                                           *
c                                                                   *
c********************************************************************
c
      include 'cslinit.inc'
c
      include 'csolvar.inc'
c
c
c********************************************************************
c                                                                   *
c    solvar variables updated                                       *
c         solcon(mxnsl,mxplan),rfg(mxnsl,mxplan),                   *
c         ki(mxplan),kr(mxplan),sscadj(i,iplane)                    *
c                                                                   *
c********************************************************************
c
      include 'cstruc.inc'
c
      include 'cwater.inc'
      include 'cwint.inc'
c
c********************************************************************
c                                                                   *
c    water variables updated                                        *
c      st(mxnsl,mxplan),ul(mxnsl,mxplane),ssc(mxnsl,mxplan),                *
c      thetdr(mxnsl,mxplan),thetfc(mxnsl,mxplan),tu(mxplan),        *
c      fctill(mxplan),fcutil(mxplan),wiltil(mxplan),wilutl(mxplan)  *
c                                                                   *
c********************************************************************
c
c     + + + LOCAL VARIABLES + + +
      real al, bdl, bdu, bt1, bt2, crr
      real cecc, kconsd, oca, sm20c, ssl
      real s15, s33, te15, te33,t33,t15,te20
      real vfsand, eqclay, eqom,slo,temp,trans,orgc
      integer i,icnt
c
c     + + + LOCAL DEFINITIONS + + +
c     al     -
c     bdl    - bulk density near lower boundary (from EPIC)
c     bdu    - bulk density near upper boundary (from EPIC)
c     crr    - variable for calculating random roughness decay coefficient
c     bt1    - parameter for EPIC Ksat computation
c     bt2    - parameter for EPIC Ksat computation
c     cecc   - CEC of the clay fraction
c     kconsd - temporary variable used to compute fully consolidated
c              values for interrill erodibility, rill erodibility,
c              and critical shear stress
c     oca    - entrapped air
c     sm20c  - 200 cm water content
c     ssl    - soil strength factor for a layer (from EPIC)
c     s15    -
c     s33    -
c     te15   -
c     te33   -
c
c     + + + SUBROUTINES CALLED + + +
c
c********************************************************************
c
      do 10 i = 1, nsl(iplane)
c
c       calculate the cation exchange capacity of the
c       clay fraction (meq/100g)
c
        cecc = cec(i,iplane) - orgmat(i,iplane) * (142.+170.*
     1      dg(i,iplane))
        if (clay(i,iplane).le.0.0) then
          solcon(i,iplane) = 0.0
        else
          solcon(i,iplane) = cecc / (100.*clay(i,iplane))
        end if
        if (solcon(i,iplane).lt.0.15) solcon(i,iplane) = 0.15
        if (solcon(i,iplane).gt.0.65) solcon(i,iplane) = 0.65
c
c-----------------------------------------------------------------------
c
c       compute baseline bulk densities
c
c-----------------------------------------------------------------------
c       if measured bulk density has not been input than calculate bdcons
c
        if (bd(i,iplane).le.0.0) then
c
c         calculate the consolidated bulk density of the soil (kg/m**3)
c
          bdcons(i,iplane) = (1.5138+(0.25*sand(i,iplane))-(13.*
     1        sand(i,iplane)*orgmat(i,iplane))-(6.000001*
     1        clay(i,iplane)*orgmat(i,iplane))-(0.48*clay(i,iplane)*
     1        solcon(i,iplane))) * 1000.
c
          if (bdcons(i,iplane).lt.1000.0) bdcons(i,iplane) = 1000.0
          if (bdcons(i,iplane).gt.1800.0) bdcons(i,iplane) = 1800.0
          bd(i,iplane) = bdcons(i,iplane)
        end if
        bdcons(i,iplane) = bd(i,iplane)
c
c       calculate the bulk density of the soil at wilting
c       point (kg/m**3)
c
        bddry(i,iplane) = (-0.024+(1.003e-03*bdcons(i,iplane))+(1.55*
     1      clay(i,iplane)*solcon(i,iplane))+(1.0*(clay(i,iplane)**2)*(
     1      solcon(i,iplane)**2))-(1.1*(solcon(i,iplane)**2)*
     1      clay(i,iplane))-(1.4*orgmat(i,iplane))) * 1000.
c
c
        if (bddry(i,iplane).le.bd(i,iplane)) bddry(i,iplane) =
     1      bd(i,iplane)
c
c
c-----------------------------------------------------------------------
c
c       porosity correction factors
c
c-----------------------------------------------------------------------
c       compute entrapped air (baumer's equation) (percent)
c
        oca = 3.80 + 1.9 * (clay(i,iplane)**2) - (3.365*sand(i,iplane))
     1      + (12.6*solcon(i,iplane)*clay(i,iplane)) + (100.*
     1      orgmat(i,iplane)*((sand(i,iplane)/2.)**2))
c
c       compute entrapped air correction factor (fraction not containing
c       entrapped air)
c
        coca(i,iplane) = (1-oca/100.0)
c
c       rfg is %rocks by weight must convert rfg to volume reza 6/89
c        cpm(i,iplane) = 1.0 - ((rfg(i,iplane)*(bd(i,iplane)/1000.0))/(
c     1      2.65*(1-rfg(i,iplane))))
c
c       Corrected rock adjustment factor, provided by Susan Skirvin,
c       10/19/2004.
c
        cpm(i,iplane) = 1.0 - (rfg(i,iplane)*bd(i,iplane))/
     1    (rfg(i,iplane)*bd(i,iplane) + 2650.*(1.0-rfg(i,iplane)))     
c---------------------------------------------------------------------
c
c       water content calculations
c
c---------------------------------------------------------------------
c
c       calculate the 15-bar water content (m**3/m**3) if
c       value is not input
c
        thtdk1(i,iplane) = 0.00217 + (0.383*clay(i,iplane)) - ((0.5*
     1      clay(i,iplane)**2)*(sand(i,iplane)**2)) + (0.265*
     1      clay(i,iplane)*(solcon(i,iplane)**2))
        thtdk2(i,iplane) = (-6.0e-02*(clay(i,iplane)**2)) + (0.108*
     1      clay(i,iplane))
c
        if (thetdr(i,iplane).le.0.0) then
          thetdr(i,iplane) = thtdk1(i,iplane) + thtdk2(i,iplane) * ((
     1        bd(i,iplane)/1000.)**2)
        end if
c
c       calculate the 1/3-bar water content (m**3/m**3) if
c       value is not input
c
        thetfk(i,iplane) = .2391 - (0.19*sand(i,iplane)) + (2.1*
     1      orgmat(i,iplane))
c
        if (thetfc(i,iplane).le.0.0) thetfc(i,iplane) =
     1      thetfk(i,iplane) + .72 * thetdr(i,iplane)
c
c       constants for log log interpolation to compute moisture tension
c       curve from 3333 cm and 15300 cm moisture
c
c dcf   Following line is in question - 1/3 bar water content should be
c dcf   (1/3)*1000 = 333.0 cm, not 3333.0 cm (from Joan Wu 5/18/2004).
c dcf   te33 = 3333.0
        te33 = 333.3
        te15 = 15300.0
        al = log(10.0)
        t33 = (log(te33)) / al
        t15 = (log(te15)) / al
c dcf   Following line is in question - soil water content at 95% of
c dcf    saturation should be 10 cm, not 100 cm.
c dcf   (from Joan Wu 5/18/2004).
c dcf   te20 = 100.0
        te20 = 10.0
        s33 = (log(thetfc(i,iplane))) / al
        s15 = (log(thetdr(i,iplane))) / al
        slo = abs((s15-s33)/(t15-t33))
c
c       compute the 200 centimeters water content
c
        sm20c = 10. ** (slo*(t15-(log(te20))/al)+s15)
c
c       calculate the residual water content (m**3/m**3)
c       do we want to use bd or bddry?
c
        wrdk(i,iplane) = (2.0e-06+1.0e-04*orgmat(i,iplane)+2.5e-04*
     1      clay(i,iplane)*(solcon(i,iplane)**0.45))
c
c       compute porosity (m**3/m**3)
c
        por(i,iplane) = (2650.-bd(i,iplane)) / 2650.
c
c       correct porosity and residual water for entrapped air
c
        por(i,iplane) = por(i,iplane) * coca(i,iplane)
c
c       correct moisture content values for rocks and
c       entrapped air
c
        thetfc(i,iplane) = thetfc(i,iplane) * cpm(i,iplane)
        thetdr(i,iplane) = thetdr(i,iplane) * cpm(i,iplane)
        sm20c = sm20c * cpm(i,iplane)
c
c       correction for soil water values greater than porosity
c
        if (sm20c.ge.por(i,iplane)) sm20c = por(i,iplane) * 0.95
        if (thetfc(i,iplane).ge.sm20c) then
          temp = thetfc(i,iplane) - thetdr(i,iplane)
          thetfc(i,iplane) = sm20c * 0.99
          thetdr(i,iplane) = thetfc(i,iplane) - temp
          if (thetdr(i,iplane).lt.0.01) thetdr(i,iplane) = 0.01
          if (thetfc(i,iplane).lt.0.01) thetfc(i,iplane) = 0.01
        end if
c
c  NEW Corrections to make sure field capacity is not very close to
c  porosity as this causes too much saturation.  RISSE-7/1/94
c       if ((thetfc(i,iplane)/por(i,iplane)).gt.0.85) then
c         temp = thetfc(i,iplane)/(por(i,iplane)*0.85)
c         thetfc(i,iplane) = (por(i,iplane)*0.85)
c         thetdr(i,iplane) = thetdr(i,iplane)/temp
c       end if
c
c  Changed 0.85 to 0.75 at Risse's request.  dcf  7/28/94
c
c       if ((thetfc(i,iplane)/por(i,iplane)).gt.0.75) then
c         temp = thetfc(i,iplane)/(por(i,iplane)*0.75)
c         thetfc(i,iplane) = (por(i,iplane)*0.75)
c         thetdr(i,iplane) = thetdr(i,iplane)/temp
c       end if
c
c  Changed 0.75 to 0.83 at Risse's request.  dcf  8/16/94
c
        if ((thetfc(i,iplane)/por(i,iplane)).gt.0.83) then
          temp = thetfc(i,iplane)/(por(i,iplane)*0.83)
          thetfc(i,iplane) = (por(i,iplane)*0.83)
          thetdr(i,iplane) = thetdr(i,iplane)/temp
        end if
c
        if (thetdr(i,iplane).lt.0.01) thetdr(i,iplane) = 0.01
        if (thetfc(i,iplane).lt.0.01) thetfc(i,iplane) = 0.01


c -- Field capacity for tilled and untilled soil layers.

      fctill(iplane) = (thetfc(1,iplane) + thetfc(2,iplane)) / 2.
      temp = 0.
      do 100 icnt = 3, nsl(iplane), 1
        temp = temp + thetfc(icnt,iplane)
  100 continue
      if(nsl(iplane)-2 .gt. 0)then
        fcutil(iplane) = temp / float(nsl(iplane) - 2 )
      else
        fcutil(iplane) = thetfc(2,iplane)
      endif

c -- 1/3 bar water content values for both tilled and untilled.

      wiltil(iplane) = (thetdr(1,iplane) + thetdr(2,iplane)) / 2.
      temp = 0.
      do 200 icnt = 3, nsl(iplane), 1
        temp = temp + thetdr(icnt,iplane)
  200 continue
      if(nsl(iplane)-2 .gt. 0)then
        wilutl(iplane) = temp / float(nsl(iplane) - 2 )
      else
        wilutl(iplane) = thetdr(2,iplane)
      endif

c
c----------------------------------------------------------------------
c
c       saturated hydraulic conductivity calculations
c
c----------------------------------------------------------------------
c
c       K-sat for soil layers below the tillage layers are calculated here
c       These equations are from EPIC - Model Documentation p.11 and p.57
c       Unit conversions are made for bulk density, sand, and clay terms
c       Mark Risse and Mark Nearing - 7/6/93
c
c       For the current soil layer, if the user has input a value of
c       zero (or any value less than 0.0701 mm/hr), make estimates of
c       EFFECTIVE baseline hydrualic conductivity (soil layers 1 & 2)
c       of SATURATED hydrualic conductivity (soil layers 3 and greater).
c
cd    Modified by S. Dun Dec 05, 2003
cd    Set the hydraulic conductivity could be as low as 1e-14 m/s
c     This is only applied if the soil file version is 2006
       if (((solwpv.eq.2006).and.(ssc(i,iplane)*3.6e6.le.0.00000010801))
     1 .or.((solwpv.ne.2006).and.(ssc(i,iplane)*3.6e6.lt.0.0701))) then
c
cd    End Modifying.
c
c         For the sublayers (soil layers 3 and greater) use the EPIC
c         equation set to estimate SATURATED hydraulic conductivity
c         if the input value is zero.
c
          if (i.gt.2) then
c
c           EPIC Equations 2.241 and 2.242 with unit conversion for sand
c
            bdl = 1.15 + (0.445*(sand(i,iplane)))
            bdu = 1.50 + (0.500*(sand(i,iplane)))
c
c           EPIC Equations 2.243 and 2.244
c
            bt2 = (alog(0.112*bdl)-alog(8.0*bdu)) / (bdl-bdu)
            bt1 = alog(0.0112*bdl) - bt2 * bdl
c
c           EPIC Equation 2.240 with unit conversion for bulk density
c
            ssl = 0.1 + (0.0009*bd(i,iplane)) / (0.001*bd(i,iplane)+
     1          exp(bt1+bt2*0.001*bd(i,iplane)))
c
c           EPIC Equation 2.31 with unit conversion for clay
c
            ssc(i,iplane) = (12.7*(100.0-clay(i,iplane)*100.0)*ssl) / ((
     1          100.0-clay(i,iplane)*100.0)+
     1          exp(11.45-0.097*(100.0-clay(i,iplane)*100.0)))
cd    Modified by S. Dun. Dec 05, 2003. 
cd    (the reference we are using is "Physical and Chemical Hydrogeology"
cd     by P.A. Domenico and F.W. Schwartz Table 3.2 for unfractured igneous
cd     and metamorphic rocks.)
c
           if (solwpv.eq.2006) then 
           if (ssc(i,iplane).lt.0.000000108) ssc(i,iplane) = 0.000000108
           else
                if (ssc(i,iplane).lt.0.07) ssc(i,iplane) = 0.07
           end if  
cd    End modifying
c
c           Convert from mm/hr to meters/second
c
            ssc(i,iplane) = ssc(i,iplane) / 3.6e6
          else
c
c           ELSE if in the top 2 soil layers and the input value of
c           conductivity has been set to zero, estimate a value.
c           (2/7/94 dcf from nearing)
c
c           CONDUCTIVITY ESTIMATION FOR ALL LAND USES EXCEPT RANGE
c
            if(uselan.ne.2)then
c
              if (clay(i,iplane).le.0.4) then
                if (cec(i,iplane).gt.1.0) then
                  ssc(i,iplane) = -0.265 + 0.0086*(sand(i,iplane)*100.0)
     1                **1.80 + 11.46 * (cec(i,iplane)**(-0.75))
                else
                  ssc(i,iplane) = 11.195 + 0.0086*(sand(i,iplane)*100.0)
     1                **1.80
                end if
              else
                ssc(i,iplane) = 0.0066 *
     1              exp(244.0/(clay(i,iplane)*100.0))
              end if
c
c           RANGELAND CONDUCTIVITY ESTIMATION
c
            else
c
c             NEW KIDWELL EQUATION AS OF June 7, 1995   dcf
c
              if(rilcov(iplane).lt.0.45)then
                ssc(i,iplane) = 57.99
     1             - (14.05 * alog(cec(1,iplane)))
     1             + (6.20 * alog(rooty(1,iplane)))
     1             - (473.39 * (fbasr(iplane)*bascov(iplane))**2)
     1             + (4.78 * fresi(iplane)*rescov(iplane))
              else
                ssc(i,iplane) = -14.29
     1             - (3.40 * alog(rooty(1,iplane)))
     1             + (37.83 * sand(1,iplane))
     1             + (208.86 * orgmat(1,iplane))
     1             + (398.64 * rrough(iplane))
     1             - (27.39 * fresi(iplane)*rescov(iplane))
     1             + (64.14 * fbasi(iplane)*bascov(iplane))
              endif
c
c             Following line should be commented out because for
c             rangeland the adjustment equations should be used
c             daily - since rescov, bascov, and rooty can change.
c             dcf  11/29/95
c             ksflag=0
c
            endif
c
c           Limit EFFECTIVE baseline conductivity value to 0.2 mm/hr
c           minimum.
c           For cropland, set the ksflag to 1 (regardless of its
c           original input setting) so that the model will use the
c           internal adjustments to conductivity for the case of
c           model estimated ke for Cropland (uselan = 1).
c           For Range ksflag = 0 in all cases.  why?  dcf  11/29/95
c
            if (ssc(i,iplane).lt.0.2) ssc(i,iplane) = 0.2
            if (uselan.eq.1) ksflag = 1
c           Convert from mm/hr to meters/second
c
            ssc(i,iplane) = ssc(i,iplane) / 3.6e6
c
          end if
        end if
c
        if (sat(iplane).lt.(thetdr(i,iplane)/(por(i,iplane)*
     1      cpm(i,iplane)))) sat(iplane) = thetdr(i,iplane) / (
     1      por(i,iplane)*cpm(i,iplane))
c
        st(i,iplane) = (((sat(iplane)*por(i,iplane))*cpm(i,iplane))-
     1      thetdr(i,iplane)) * dg(i,iplane)
        if (st(i,iplane).lt.1e-10) st(i,iplane) = 0.00
c
        ul(i,iplane) = (por(i,iplane)-thetdr(i,iplane)) * dg(i,iplane)
c
c       The following code and computation of SSCMIN here causes
c       an error in computation in GRNA for rangeland conditions
c       where a user has input a value of effective conductivity.
c       The user input value will be set here to one value, and
c       then SSC will be recomputed in SOIL for rangeland.
c       dcf  10/6/99
c
cdcf    if (lanuse(iplane).eq.1) then
          if (i.eq.1) then
            sscmin(iplane) = ssc(i,iplane)
cd          Added by S. Dun, Dec 01, 2007
c           for infiltration on frozen soil
            if ((frdp(iplane) .gt. 0.0).and.(sscv(i,iplane).gt.0.0))
     1      then
               sscmin(iplane) = sscv(i,iplane)
              endif
cd          End adding   
          else
            if (ssc(i,iplane).lt.sscmin(iplane)) then
                  sscmin(iplane) = ssc(i,iplane)
cd              Added by S. Dun, Dec 01, 2007
c               for infiltration on frozen soil
                if ((frdp(iplane) .gt. 0.0).and.(sscv(i,iplane).gt.0.0))
     1          then
                    sscmin(iplane) = sscv(i,iplane)
                  endif
cd              End adding 
            endif
          end if
cdcf    end if
c
   10 continue
c
c     if the input bulk density after tilage is zero set to
c     the consolidated bulk density
c
      if (uselan.eq.1) then
        if (bdtill(iplane).le.0) bdtill(iplane) = bdcons(1,iplane)
      end if
c------------------------------------------------------------------
c
c     various coefficients relating to surface characteristics
c
c------------------------------------------------------------------
c     compute random roughness coefficient
c      rre(iplane) = 2.8 - (30.0*silt(1,iplane))
c
c ZZZ change RR decay function based on Potter' work (ASAE 1990), 10/7/94
      crr = 63.0 + 62.7*log(orgmat(1,iplane)*50) +
     1     1570.*clay(1,iplane) - 0.25*(clay(1,iplane)*100)**2
c
c     set the minimum of crr to 30, which produces the maximum decay
c     rate. Namely at this rate, the RR will be reduced to 13% of
c     initial RR after 100mm rain.
      if (crr .lt. 30.) crr = 30.
      rre(iplane) = -(1./crr)**0.6
c
c     correction added by dcf  1/31/92   can't allow rre to be positive
c     which can occur in V91.5 when silt content of soil is less than 10%
c
c     if(rre(iplane).gt.0.0) rre(iplane)= 0.0
c
c     changed 2/2/93 in accordance with page 6.2 of NSERL #2 - setting
c     rre equal to -0.1 if it is computed to be more than -0.1    dcf
c
c      if (rre(iplane).gt.-0.1) rre(iplane) = -0.10
c
c
c     fraction of soil compacted by planting, cutivation and harvesting
c
      trans = 4.165 + (2.456*sand(1,iplane)) - (1.703*clay(1,iplane)) -
     1    (4.*(sand(1,iplane)**2))
      if (trans.lt.3.0) trans = 3.0
      tu(iplane) = 0.009 * ((trans-3.0)**0.42)
c
      if (uselan.eq.1) then
c
c******************************************************************
c
c       cropland
c
c------------------------------------------------------------------
c
c       CROPLAND BASELINE INTERRILL ERODIBILITY FACTOR MUST BE
c       ENTERED IN THE SOIL INPUT FILE BY THE USER - if the user
c       enters a 0.0, WEPP sets the value to a constant 5300000.
c       These values represent an average of 43 soils of wide ranges
c       of properties.
c
c------------------------------------------------------------------
c
        if (ki(iplane).le.0.) then
c
c         Changes for model calculations of baseline interrill
c         erodibility follow.  No longer assume a constant
c         of 5.3e06 if user has input a 0 in soil file.
c         dcf  10-14-99
c         ki(iplane) = 5.3e06
c
c         for soils with less than 30 percent sand content

          if(sand(1,iplane).lt.0.30)then
            eqclay=clay(1,iplane)
            if(eqclay.lt.0.10)eqclay=0.10
            ki(iplane) = 6054000.0 - 5513000.0*eqclay
          else

c         for soils with 30 percent sand or greater
c
c           at present, assume very fine sand content is
c           25% of total sand content.  In the future, allow
c           user input of VFS content in soil input file.

            vfsand = 0.25 * sand(1,iplane)
            if(vfsand.gt.0.40)vfsand=0.40
            write(6,1000)vfsand,iplane
 1000       format(/,' *** WARNING ***',/,
     1      ' Very Fine Sand fraction of ',f4.2,' assumed and',/,
     1      ' used in computation of baseline interrill erodibility'
     1      ,/,' on OFE number ',i2,/,'*** WARNING ***',/)
            ki(iplane) = 2728000.0 + 19210000.0*vfsand
          end if
c
        end if
c
c       Compute variables needed for new consolidation calculations.
c
        kconsd = 1000. * (3042.0-3166.0*sand(1,iplane)-8816.*
     1      orgmat(1,iplane)-2477.*thetfc(1,iplane))
        if (kconsd.lt.10000.) kconsd = 10000.
        if (kconsd.gt.2000000.) kconsd = 2000000.
        kicrat(iplane) = kconsd / ki(iplane)
        if (kicrat(iplane).gt.1.0) kicrat(iplane) = 1.0
        if (kicrat(iplane).lt.0.1) kicrat(iplane) = 0.1
c
c
c       Compute the decay coefficient for the consolidation equation.
c       Model uses the same decay coefficient for Ki, Kr, and Tauc
c
        bconsd(iplane) = 0.02
c---------------------------------------------------------------------
c
c       CROPLAND BASELINE RILL ERODIBILITY AND CRITICAL SHEAR STRESS
c       MUST BE ENTERED IN THE SOIL INPUT FILE BY THE USER - if the
c       user enters a 0.0, WEPP sets the value of Kr to a constant 0.0115
c       and the value of TauC to a constant 3.1. These values represent an
c       average of 43 soils of wide ranges of properties.
c
c---------------------------------------------------------------------
        if (kr(iplane).le.0.) then
c         Changes for model calculations of baseline rill
c         erodibility follow.  No longer assume a constant
c         of 0.0115 if user has input a 0 in soil file.
c         dcf  10-14-99
c         kr(iplane) = 0.0115
c
c         for soils with less than 30 percent sand content

          if(sand(1,iplane).lt.0.30)then
            eqclay=clay(1,iplane)
            if(eqclay.lt.0.10)eqclay=0.10
            kr(iplane) = 0.0069 + 0.134*exp(-20.0*eqclay)
          else

c         for soils with 30 percent sand or greater
c
c           at present, assume very fine sand content is
c           25% of total sand content.  In the future, allow
c           user input of VFS content in soil input file.

            vfsand = 0.25 * sand(1,iplane)
            if(vfsand.gt.0.40)vfsand=0.40
            write(6,2000)vfsand,iplane
 2000       format(/,' *** WARNING ***',/,
     1      ' Very Fine Sand fraction of ',f4.2,' assumed and',/,
     1      ' used in computation of baseline rill erodibility'
     1      ,/,' on OFE number ',i2,/,'*** WARNING ***',/)
            eqom = orgmat(1,iplane)
            if(eqom.lt.0.0035)eqom=0.0035
            kr(iplane) = 0.00197 + 0.03*vfsand
     1                   + 0.03863*exp(-184.0*eqom)
          end if
        end if
c
        if (shcrit(iplane).le.0.) then
c         Changes for model calculations of baseline
c         critical shear follow.  No longer assume a constant
c         of 3.1 if user has input a 0 in soil file.
c         dcf  10-14-99
c         shcrit(iplane) = 3.1
c
c         for soils with less than 30 percent sand content

          if(sand(1,iplane).lt.0.30)then
            shcrit(iplane) = 3.5
          else

c         for soils with 30 percent sand or greater
c
c           at present, assume very fine sand content is
c           25% of total sand content.  In the future, allow
c           user input of VFS content in soil input file.

            vfsand = 0.25 * sand(1,iplane)
            if(vfsand.gt.0.40)vfsand=0.40
            write(6,3000)vfsand,iplane
 3000       format(/,' *** WARNING ***',/,
     1      ' Very Fine Sand fraction of ',f4.2,' assumed and',/,
     1      ' used in computation of baseline critical shear'
     1      ,/,' on OFE number ',i2,/,'*** WARNING ***',/)
            eqclay=clay(1,iplane)
            if(eqclay.gt.0.40)eqclay=0.40
            shcrit(iplane) = 2.67 + 6.5*eqclay - 5.8*vfsand
          end if
        end if
c
c       Nearing's new code to set coefficients for use in new
c       consolidation function equation for cropland
c
c       Question by Nearing on whether new Drungil equation correct
c
        kconsd = 0.00035 - 0.0014 * thetfc(1,iplane) + 0.00068 *
     1      silt(1,iplane) + 0.0049 * rfg(1,iplane)
c
        if (kconsd.lt.0.00001) kconsd = 0.00001
        if (kconsd.gt.0.004) kconsd = 0.004
c
        krcrat(iplane) = kconsd / kr(iplane)
c
c       limit the ratio values to between 0.05 and 1.0
c
        if (krcrat(iplane).gt.1.0) krcrat(iplane) = 1.0
        if (krcrat(iplane).lt.0.05) krcrat(iplane) = 0.05
c
c       Compute the critical shear stress consolidation parameters
c
        kconsd = 8.37 - 11.8 * thetfc(1,iplane) - 4.9 * sand(1,iplane)
c
        if (kconsd.lt.0.3) kconsd = 0.3
        if (kconsd.gt.7.0) kconsd = 7.0
c
        tccrat(iplane) = kconsd / shcrit(iplane)
c
        if (tccrat(iplane).lt.1.0) tccrat(iplane) = 1.0
        if (tccrat(iplane).gt.4.0) tccrat(iplane) = 4.0
c
c
      else
c**********************************************************************
c
c       rangeland
c
c**********************************************************************
c
c       compute rangeland rill erodibility and critical shear stress
c       factors - equations were modified in August of 1991 based on
c       re-evaluation of the rangeland experimental data by Carol
c       Drungil, Roger Simanton, and John Laflen.    dcf  9/10/91
c
c----------------------------------------------------------------------
c
c       orgc calculation moved outside of if statement in order for
c       following 2 if loops to be correct.  sjl
c
        orgc = orgmat(1,iplane) * 0.58
        if (kr(iplane).le.0.) then
c
c         kr equation  8/89    ala Simanton
c
          kr(iplane) = 0.0017 + (0.0024*clay(1,iplane)-0.014*orgc-
     1        0.00088*bddry(1,iplane)/1000.0-0.00048*rooty(1,iplane))
c
c
c
c
          if (kr(iplane).lt.0.00001) kr(iplane) = 0.00001
          if (kr(iplane).gt.0.004) kr(iplane) = 0.004
        end if
c
c
        if (shcrit(iplane).le.0.) then
c
c         shcrit equation 8/89     ala Simanton
c
          shcrit(iplane) = 3.23 - 5.6 * sand(1,iplane) - 39.1 * orgc +
     1        0.90 * bddry(1,iplane) / 1000.0
c
c
          if (shcrit(iplane).lt.0.3) shcrit(iplane) = 0.3
          if (shcrit(iplane).gt.7.0) shcrit(iplane) = 7.0
        end if
c
c----------------------------------------------------------------------
c
        if (ki(iplane).le.0.) then
c
c         New new ki equation - Simanton, 1994 (for I*Q model)
c
          ki(iplane) = 1000. * (1810.0-1910.0*sand(1,iplane)-6327.*
     1        orgmat(1,iplane)-846.*thetfc(1,iplane))
c
          if (ki(iplane).lt.10000.) ki(iplane) = 10000.
          if (ki(iplane).gt.2000000.) ki(iplane) = 2000000.
c
        end if
c
c
c----------------------------------------------------------------------
c
c
      end if
c
      return
      end
