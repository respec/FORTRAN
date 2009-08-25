      subroutine infpar(nowcrp,jdplt,plant,irsyst)
c
c     + + + PURPOSE + + +
c     Calculates sat. hydraulic conductivity and effective matric
c     potential from :
c      1) bare soil hyd. cond.
c      2) avg. potential across wetting front
c      3) effective porosity
c      4) percent ground cover
c      5) percent canopy cover
c      6) relative effective saturation
c
c     Called from SOIL
c     Author(s): Savabi,Risse,Zhang
c     Reference in User Guide: Chapter 4
c
c NOTE: Computation of the fraction of soil surface covered by both
c       canopy and ground cover (COVU) assumes location of ground
c       cover and canopy cover are independent.  Reza Savabi says
c       this is correct.  I would expect the location of the residue
c       to be somewhat dependent on the location of the plants that
c       generated it.  -- CRM (9/14/92 conversation with R. Savabi)
c
c     Changes:
c           1) Common block SOLVAR was not used.  It was de-referenced.
c           2) The generic "SAVE", which saves ALL local variables,
c              was eliminated.
c           3) Eliminated local variables TOTADJ, IPLUG, & NCOUNT.
c           4) Introduced intermediate local variables TMPVR1
c              to TMPVR4 to make calculations more efficient.
c           5) The statement:
c                   if (wetfrt.eq.tc) wetfrt=tc+.00001
c              was changed to:
c                   if(abs(wetfrt-tc) .lt. 0.00001) wetfrt=tc+.00001
c           6) Local variable SAT11 was computed, but the result was
c              never used.  It was eliminated.
c           7) Added local variable RFCUMX so INFPAR could tell if
c              rainfall had occurred.  In my test data sets, this
c              eliminated about 80% of the executions of the code for
c              soil crusting adjustments.
c           8) Changed statement at end of subroutine to prevent a
c              divide by zero occurring in SR ROCHEK - prevents the
c              value of sm(iplane) from becoming zero.  dcf  8/16/93
c           9) Moved RFCUMX to common block cifpar.inc jca2 8/31/93
c          10) Added new Ksat adjustments from Risse and Zhang
c              dcf  1/11/94
c          11) Changes made to Ksat adjustments of Risse and
c              Zhang -  dcf     2/4/94
c          12) Changes made to Ksat adjustment equations again
c              by Nearing - dcf  3/8/94
c          13) Change to Ksat adjustment equations for established
c              perennials from John Zhang - dcf  5/26/94
c          14) Change to Ksat adjustment equations for surface
c              cover(both residue and canopy) adjustments to
c              hydraulic conductivity and also for first year
c              of perennial growth.   dcf  12/14/94
c          15) Change to exclude canopy cover factor in the
c              adjustment for hydraulic conductivity for the case
c              of furrow irrigation water addition.  dcf  12/14/94
c          16) Change statement which checks for water content to
c              be above upper limit so that saturation can occur for
c              a single storm simulation.  savabi and dcf  4/95
c               FROM:     if (st(i,iplane).ge.ul(i)) then
c                 TO:     if (st(i,iplane).ge.0.95*ul(i)) then
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 09/03/92 & 10/20/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxres.inc'
      include 'pmxsrg.inc'
      include 'pntype.inc'
      include 'ptilty.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp, jdplt, plant, irsyst
c
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - index of current crop on the OFE
c     jdplt  - julian planting date of current crop on OFE
c     plant  - index of current plant type on the OFE
c     irsyst - the type of irrigation system: 0 - none
c                                             1 - sprinkler
c                                             2 - furrow
c
c     + + + COMMON BLOCKS + + +
c
      include 'cavepar.inc'
c     modify: avsat(mxplan), avbd(mxplan)
c
      include 'cclim.inc'
c       read: tave
c
      include 'ccons.inc'
c       read: cpm(mxnsl,mxplan)
c     modify: avpor(mxplan)
c
      include 'ccover.inc'
c       read: cancov(mxplan), lanuse(mxplan)
c     modify: gcover(mxplan)
c
      include 'ccrpout.inc'
c       read: rescov(mxplan), bd(mxnsl,mxplan), rtmass(mxplan)
c
      include 'ccrpvr1.inc'
c       read: rtm(mxres,mxplan)
c
      include 'cgcovr.inc'
c       read: gcvplt(mxplan),mantyp(mxplan)
c     modify: gcvplt(mxplan)
c
      include 'chydrol.inc'
c       read: rain(mxplan),prcp
c
      include 'cirfurr.inc'
c     modify: dtheta
c
      include 'cke.inc'
c       read: rkecum(mxplan)
c
      include 'cparame.inc'
c      write: ks(mxplan), sm(mxplan)
c
      include 'cperen1.inc'
c       read: imngmt(mxcrop,mxplan),rtmmax(ntype)
c
      include 'cstmflg.inc'
c       read: norain(mxplan)
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'ctemp.inc'
c       read: avclay(mxplan), avsand(mxplan)
c
      include 'ctillge.inc'
c       read: tillay(2,mxplan)
c
      include 'cwater.inc'
c       read: st(mxnsl,mxplan), ul(mxnsl,mxplan), thetdr(mxnsl,mxplan),
c             nsl(mxplan), dg(mxnsl,mxplan)
c
c     include 'cifpar.inc'
c     modify: rfcumx
c
      include  'cwint.inc'
c
cd    Added by S. Dun, Jan 08, 2008
      include 'cupdate.inc'
cd    End adding
c
c     + + + LOCAL VARIABLES + + +
      real avsm15, avks, sf, wetfrt, a, rra, bbbb, tc, crust(mxplan),
     1    eke, sc, cke, crstad, kbare, ktmp, ccovef, scovef, kcov
     1    ,fzul,ffi,tmpvr1,tmpvr2,tmpvr3,tmpvr4,avcpm
      integer i,tpfzfg
c
c    1    eke, sc, cke, crstad, kbare, ktmp, rtmt, rtmtef, ccovef,
c    1    scovef
c
c     + + + LOCAL DEFINITIONS + + +
c     avsm15 - average 1500 KPa (15 bar) soil water content
c     avks   - average saturated hydralic conductivity for the tillage
c              layer
c     sf     - matric potential across wetting front (m)
c     wetfrt - average depth of wetting front (m)
c     cf     - canopy cover adjustment for saturated hydraulic
c              conductivity
c     a      - macroporosity adjustment for saturated hydraulic
c              conductivity
c     bareu  - bare area under canopy (fraction)
c     bareo  - bare area outside canopy (fraction)
c     covu   - ground cover under canopy (fraction)
c     covo   - ground cover outside canopy (fraction)
c     tc     - crust thickness
c     crust  - crust adjustment for saturated hydraulic conductivity
c     eke    - effective hydraulic conductivity in fill layer
c              (m/sec)
c     sc     - reduction factor for subcrust hydraulic conductivity
c     avcpm  - average rock fragment correction factor for the tillage
c              layer
c     grdcov - ground cover value used in macroporosity calculations
c              for cropland annuals - assigned a value on date of
c              last planting.  For perennials and range - use actual
c              cover values
c     cke    - coefficient relating amount of kinetic energy since last
c              tillage to the speed of crust formation
c     rtmt   - total dead and live root mass in top 15 cm of soil
c              (kg/m**2) (NOT USED)
cc    rtmtef - transformed (live plus dead) root mass
c     ccovef - effective canopy cover corrected for the effect of
c              canopy height
c     scovef - effective total surface cover
c     ktmp   - same as AVKS, only in units of (mm/hr)
c     kbare  - effective AVKS after adjustment for crusting/tillage
c     kcov   - portion of equation to compute effective AVKS
c              (surface cover adjustment)
c     tpfzfg - top layer frozen flag
c              0: no influence of top frozen layer on infiltration
c              1: for there is influence of top frozen layer
c
c     + + + SAVES + + +
      save crust
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c      thickness.  The layers assumed to affect infiltration are the
c      primary (deepest), and [the average of] the secondary tillage
c      layers.)
c
      avbd(iplane) = 0.0
      avpor(iplane) = 0.0
      avsm15 = 0.0
      avcpm = 0.0
      tpfzfg = 0      
c
      do 10 i = 1, 2
c ------ ratio of tillage layer thick. to primary (deepest) til. depth
        tmpvr1 = dg(i,iplane) / tillay(2,iplane)
c ------ bulk density
        avbd(iplane) = avbd(iplane) + bd(i,iplane) * tmpvr1
c ------ porosity for infiltration
        avpor(iplane) = avpor(iplane) + por(i,iplane) * tmpvr1
c ------ 15-bar soil water content
        avsm15 = avsm15 + thetdr(i,iplane) * tmpvr1
c ------ correction factor for rock fragments
        avcpm = avcpm + cpm(i,iplane) * tmpvr1
   10 continue
c
c     Range checks:
      if (avpor(iplane).gt.1.0) avpor(iplane) = 1.0
      if (avpor(iplane).lt.0.0) avpor(iplane) = 0.0
      if (avbd(iplane).lt.800.0) avbd(iplane) = 800.0
      if (avbd(iplane).gt.2200.0) avbd(iplane) = 2200.0
c
c ---- Calculate the harmonic mean of Ks in the tillage layer (AVKS)
c      for the infiltration routine.
c
cd    Modified by S. Dun, April 17, 2008
c     for infiltration
c
      if ((frozen(1,iplane) .gt. 0.0).and. (sscv(1,iplane).gt.0.0)
     1     .and.(sscv(2,iplane).gt.0.0) ) then
c
      avks = solthk(2,iplane) / (solthk(1,iplane)/sscv(1,iplane)
     1     + (solthk(2,iplane)-solthk(1,iplane))/sscv(2,iplane))
c
      eke = avks
      tpfzfg = 1
c
      else
      avks = solthk(2,iplane) / (solthk(1,iplane)/ssc(1,iplane)+(
     1    solthk(2,iplane)-solthk(1,iplane))/ssc(2,iplane))
      endif
cd    End Modifying
c
c ---- Calculate average water content in tillage layer (AVSAT)
c      for the infiltration routine.  (ST is constant > 15 bars)
      avsat(iplane) = (st(1,iplane)+st(2,iplane)) / tillay(2,iplane) +
     1    avsm15
      if (avsat(iplane).gt.avpor(iplane)) avsat(iplane) =
     1    avpor(iplane) * .98
c
c ---- Compute the matric potential of the infiltration zone (SF)
c     (WEPP Equation 4.3.2 ff)
c -- XXX -- This equation needs a *number* in the User Document!
c     (See top of p 4.5) -- CRM -- 9/14/92.
      tmpvr2 = avclay(iplane) ** 2
      tmpvr3 = avsand(iplane) ** 2
      tmpvr4 = avpor(iplane) ** 2
c
      sf = 0.01 * exp(6.531-7.33*avpor(iplane)+15.8*tmpvr2+3.81*tmpvr4+
     1    avsand(iplane)*(3.4*avclay(iplane)-4.98*avpor(iplane))+tmpvr4*
     1    (16.1*tmpvr3+16.0*tmpvr2)-14.0*tmpvr3*avclay(iplane)-
     1    avpor(iplane)*(34.8*tmpvr2+8.0*tmpvr3))
      if (sf.gt.0.5) sf = 0.5
      
c
      if (tpfzfg .ne. 1) then
c     no influence of top frozen layer on infitration.
c     when top frozen layer influence exists, sealing adjudtment is skipped
c
c     *** L0 IF ***
c     CROPLAND
      if (lanuse(iplane).eq.1) then
c
c ------ ground cover (fraction)
        gcover(iplane) = rescov(iplane)
c
c ------ Compute average depth of the wetting front (meters)
c       (WEPP Equation 4.3.11)
        wetfrt = 0.147 - 0.15 * tmpvr3 - (0.0003*avclay(iplane)*
     1      avbd(iplane))
        if (wetfrt.lt.0.005) wetfrt = 0.005
c
c ------ crust thickness
        tc = 0.005
c
c     *** L0 ELSE-IF ***
c     RANGELAND
      else if (lanuse(iplane).eq.2) then
c
c ------ average wetting front depth (meters)
c       (WEPP Equation 4.3.11)
        wetfrt = 0.147 - 0.15 * tmpvr3 - (0.0003*avclay(iplane)*
     1      avbd(iplane))
        if (wetfrt.lt.0.01) wetfrt = 0.01
c
c ------ crust thickness
        tc = 0.01
c
c ------ canopy cover adjustment factor for sat. hydraulic conductivity
c       (WEPP Equation 4.3.12)
c       cf = 1.0 + cancov(iplane)
c
c ------ macroporosity adjustment factor for sat. hydraulic conductivity
        a = exp(6.10-(10.3*avsand(iplane))-(3.7*avclay(iplane)))
        if (a.lt.1.0) a = 1.0
        if (a.gt.10.0) a = 10.0
c
c     *** L0 ELSE ***
c     FOREST
      else
c
c     (This branch left intentionally blank.)
c
c     *** L0 ENDIF ***
      end if
c
c
c     *** M0 IF ***
c     If rainfall or tillage has occurred, compute saturated
c     hydraulic conductivity adjustment for crusted soil surface.
c     if (rfcum.ne.rfcumx) then
c
c ------ correction factor for partial saturation of the sub-crust layer
c     (WEPP Equation 4.3.8)
      sc = 0.736 + (0.19*avsand(iplane))
c
c ------ compute maximum potential crust adjustment fraction
c     (WEPP Equation 4.3.7)
      if (abs(wetfrt-tc).lt.0.00001) wetfrt = tc + .00001
c
c     Use new equation for forming crust - see Rawls et al. 1989
c     Note: This is now maximum adjustment
      ffi = 45.19 - (46.68*sc)
      crust(iplane) = sc / (1.0+(ffi/(wetfrt*100)))
      if (crust(iplane).lt.0.20) crust(iplane) = 0.20
c
c     ************************************************************
c     * Note: CRUST is multiplied times Ksat.  When there is no  *
c     *       crust, Ksat is not adjusted; ie, CRUST = 1.  CRUST *
c     *       is computed in 2 parts.  The second part is the    *
c     *       adjustment for cumulative rainfall since tillage.  *
c     *       It is performed ONLY if RF since tillage is less   *
c     *       than 0.1 m                                         *
c     ************************************************************
c
c-----for CROPLAND, if cumulative RF since tillage is less than
c     1/10 meter, update crust reduction factor for rainfall.
c     New equation inserted Risse
c     crust adjustment=maxadj+(1-maxadj) exp(-C kecum (1-rr/4))
c     where maxadj is the same as it was in previous version with
c     correction for units(crust), C is calculated based on analysis
c     by Risse, and instead of using a linear relationship between 0
c     and 100mm of rfcum we used the exponential relationship of
c     Brakensiek and Rawls, 1983
c     (WEPP Equation 4.3.10)
c     if(lanuse(iplane).eq.1) then
c     Original Code:
c     1                            crust=1.-(((1.-crust)/0.1)*rfcum)
c     1                            crust = 1.0 - (1.0-crust)*10.0*rfcum
c     cke is coeffient relating to speed with which crust forms
c     based on analysis of natural runoff plot data by Risse
c
c-----for CROPLAND, if cumulative RF since tillage is less than
c     1/10 meter, update crust reduction factor for rainfall.
c
      if (lanuse(iplane).eq.1) then
        cke = -0.0028 + 0.0113 * avsand(iplane) + 0.125 *
     1      avclay(iplane) / cec1(1,iplane)
        if (cke.gt.0.01) cke = 0.01
        if (cke.lt.0.0001) cke = 0.0001
c       make sure random roughness does not cause positive exponent
        if (rrc(iplane).le.0.04) then
          rra = rrc(iplane)
        else
          rra = 0.04
        end if
c
        bbbb = -cke * rkecum(iplane) * (1-rra/0.04)
        if (bbbb.lt.-25.0) bbbb = -25.0
c       Calculate crusting/tillage adjustment
        crstad = crust(iplane) + (1-crust(iplane)) * exp(bbbb)
      else
        crstad = 1.0
      end if
c
c     *** M0 ENDIF ***
c     endif
c
c     Adjust AVKS for soil surface characteristics (canopy & cover)
c     and for dead and live roots.    (from zhang 1/94)   dcf
c
        kbare = avks * crstad
        ktmp = avks * 3.6e6
c
c     EXCLUDE the adjustment for canopy cover for the case of
c     snow melt or furrow irrigation.   dcf  12/15/94
c
      if(tave.gt.0.0)then
        if(prcp.gt.0.001 .or. irsyst.eq.1)then
c
c         Adjust the effectiveness of canopy cover by canopy height
c
          ccovef = cancov(iplane) * exp(-0.3358*canhgt(iplane)/2.0)
c
c         Calculate the total effective surface cover
c
          scovef = ccovef + rescov(iplane) - ccovef * rescov(iplane)
c
        else
          scovef = rescov(iplane)
        endif
      else
        scovef = rescov(iplane)
      endif
c
c       Calculate the final effective conductivity
c
c     IF the user has indicated that he/she wants the internal
c     Ksat adjustments used in the SOIL input file - then
c     adjust final effective conductivity for crusting/tillage/
c     crop/rainfall                dcf  1/11/94
c
      if (ksflag.eq.1) then
c
        if(tave.gt.0.0)then
c
c XXX     NOTE - We should really add in the amount of sprinkler
c XXX            irrigation water here (if any exists for the day)
c XXX            but unfortunately at this point in the program we
c XXX            do not yet know what this amount will be (have not
c XXX            call subroutine IRRIG yet).    dcf  12/15/94
c
          kcov = (0.0534 + 0.01179*ktmp) * prcp * 1000.0 / 3.6e6
        else
          kcov = 0.0
        endif
c
c       Zhang change 12/9/94
c       if(kcov .lt. avks)kcov = avks
        if(kcov .lt. 0.5*avks) kcov = 0.5*avks  
c
        eke = kbare*(1.0 - scovef) + kcov*scovef
c
c       If crop adjusted eke is smaller than that of crust adjusted
c       set it back to crust adjusted value
c
        if (eke.lt.kbare) eke = kbare
c
c
c       ADJUST FOR ESTABLISHED PERENNIAL CROP (meadows, etc.)
c
c Note - Changed coefficient in equation below from 1.7965 to 1.81
c        when given change from Zhang.   7/1/94   dcf
c
        if(lanuse(iplane).eq.1)then
c
c         Changes to include perennial adjustment for first
c         year of perennial growth when plant is sufficiently
c         developed.   dcf  12/9/94
c         if(imngmt(nowcrp,iplane).eq.2 .and. jdplt.eq.0)
c    1      eke = 1.81 * eke
c
          if(imngmt(nowcrp,iplane).eq.2)then
            if(jdplt .eq. 0)then
              eke = 1.81 * eke
            else
c
c             If the root mass present for the perennial exceeds
c             half of the input maximum possible, then adjust the
c             EKE value by the full amount.  The 0.5 value is
c             arbitrary and could be changed here and below. dcf
c
              if(rtmass(iplane).gt.0.5*rtmmax(plant))then
                eke = 1.81*eke
              else
c
c               Linearly change the eke adjustment based upon the
c               ratio of root mass present to half the maximum
c               root mass possible (input) for the perennial.  The
c               use of the 0.5 value is arbitrary and could be
c               changed here and above.   dcf  12/9/94
c
                eke = eke + (1.62*eke/rtmmax(plant))*rtmass(iplane)
              endif
            endif
          endif
        endif
      else
        eke = avks
      end if
c     Added by S. Dun, May 17, 2008 
c     for considering the influence of top frozen layer on infiltration     
      endif
c     end adding

cd    Modified by S. Dun 06/20/2002
c     LIMIT MINIMUM KSAT TO 1.94E-08 m/s (0.07 mm/h) for pre 
c       2006 soil files.
c
      if (solwpv.eq.2006) then
           if (eke.le.1.0e-14) eke = 3.0e-14
      else
           if (eke.le.1.94e-08) eke = 1.94e-08
      endif
c
cd    adjust the lower limit to e-14 m/s 
cd    (the reference we are using is "Physical and Chemical Hydrogeology"
cd     by P.A. Domenico and F.W. Schwartz)
cd    end modifying.
c
c
c     *** BEGIN N0 LOOP ***
c
c     In case a restricted soil layer controls percolation and
c     infiltration....
c
      i = 0
   20 continue
      i = i + 1
c ---- If the water content is above the upper limit for this layer....
c
cd    Modified by S. Dun, April 17, 2008
c      for frozen soil effect
cd      if (st(i,iplane).ge.0.95*ul(i,iplane)) then
c
      fzul =ul(i,iplane) - frzw(i,iplane)
      if (st(i,iplane).ge.0.95*fzul) then
c ------ If this layer's Ksat is less than the average Ksat for the
c        plow layer ....
        if (ssc(i,iplane).le.eke) then
            eke = ssc(i,iplane)
        endif
c
        if ((frdp(iplane) .gt. 0.0).and. (sscv(i,iplane).gt.0.0)
     1     .and. (sscv(i,iplane).le.eke) ) then
c
            eke = sscv(i,iplane)
         endif
cd       End modifying
      else
c ------ (force exit from loop)
        i = nsl(iplane)
      end if
c     *** END N0 LOOP ***
      if (i.lt.nsl(iplane)) go to 20
c
      ks(iplane) = eke
c
c      Write(60, 1505) day, mon, year, eke, kbare, scovef,crstad
c     1                ,crust(iplane),bbbb
1505  format(1x, 3I6, 6E12.3)
c
c     Compute effective matric potential (SM), correcting for rock
c     fragments (using AVCPM).
c
      if (avsat(iplane).ge.(avpor(iplane)*avcpm)) avsat(iplane) = (
     1    avpor(iplane)*avcpm) * 0.99
c ---- compute water above field capacity
      dtheta(iplane) = avpor(iplane) * avcpm - avsat(iplane)
c ---- compute effective matric potential (SM)
      sm(iplane) = dtheta(iplane) * sf
c
c     rfcumx = rfcum
c
      return
      end
