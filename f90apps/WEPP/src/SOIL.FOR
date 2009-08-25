      subroutine soil(nowcrp)
c
c
c     + + + PURPOSE + + +
c     Initializes soil parameters such as bulk density,
c     porosity, etc.
c
c     Called from CONTIN & WATBAL
c     Author(s): Alberts, Ghiddey, Simanton, Nearing, Laflen, Elliot,
c                Young, Ferris, Flanagan, Savabi, Weltz
c     Reference in User Guide: Chapters 6 & 4.  Also see "A Compendium
c         of Soil Erodibility Data from WEPP Cropland Soil Field Erodi-
c         bility Experiments 1987 & 88", NSERL Report No. 3
c
c     Changes:
c           1) Include files PMXGRX.INC & PTILTY.INC were not used.
c              They were de-referenced.
c           2) Common blocks AVEPAR, CLIM, ENDS, & RINPT1 were not
c              used.  They were de-referenced.
c           3) Eliminated embedded "goto 100" with if-else structure.
c           4) Moved a lot of statements outside the "do-10 loop".
c           5) Added declarations for the following local variables:
c                   i1,i2,n2
c                   rhaft,ckiasa,ckiawc,bdirf,rraft,wrd,epor,smf,
c                   ckialr,ckiadr,ckiasc,ckiaft,bddiff,bdiwt,
c                   rprime,ckraft,ckrawc,rkiacr,rkialc,rkratl,
c                   rkracr,rkralc
c           6) Eliminated SAVE of: ao,bdtll,rh,bar13,bar110
c           7) Removed the line:
c                   imodel=1
c              from the bottom of the L0-IF, since IMODEL is read as a
c              user input in CONTIN, and SOIL doesn't seem to be executed
c              unless IMODEL=1 (continuous simulation) anyway.
c           8) The following 2 lines commented out since RHINIT is a
c              user input:
c                if((tilseq(nowcrp,iplane).gt.0) .and.
c             1     (iridge(tilseq(nowcrp,iplane)).eq.1)) rhinit(iplane)=0.10
c           9) To eliminate discontinuities the lines:
c                 if(sm20c.ge.por(i,iplane)) sm20c=por(i,iplane)*0.95
c                 if(thetfc(i,iplane).ge.sm20c) then
c                   temp=thetfc(i,iplane)-thetdr(i,iplane)
c                   thetfc(i,iplane) = sm20c*0.99
c              changed to:
c                 if(sm20c.ge.(por(i,iplane)*0.95)) sm20c=por(i,iplane)*0.95
c                 if(thetfc(i,iplane).ge.(sm20c*0.99)) then
c                   temp=thetfc(i,iplane)-thetdr(i,iplane)
c                   thetfc(i,iplane) = sm20c*0.99
c           10) Corrected error in the do-50 loop: ----\/
c                            if(ao(i,iplane).lt.0.0) ao(1,iplane)=0.0
c               should be:
c                            if(ao(i,iplane).lt.0.0) ao(i,iplane)=0.0
c               (DID make a difference in the outputs of Test1-3.)
c           11) IRDEPT was dimensioned to MXPLAN in version 92.332
c               (commom block IRRIGA).  This change is reflected in
c               this code.  -- CRM -- 1/7/93.
c           12) IFROST moved to common block CKRCON jca2 8/31/93
c           13) Savabi changes for frozen soil adjustments to
c               conductivity.   dcf  5/20/94
c
c     Version: This module recoded from WEPP Version 92.25.
c     Date recoded: 09/19/92 - 10/30/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxres.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - current crop
c
c     + + + COMMON BLOCKS + + +
c
      include 'cclim.inc'
c       read: tave
c
      include 'ccrpvr1.inc'
c       read: rtm(mxres,mxplan), smrm(mxres,mxplan)
c
      include 'ccrpvr3.inc'
c       read: hmax(ntype)
c
      include 'ccons.inc'
c       read: ck1(mxnsl,mxplan), ck2(mxnsl,mxplan), rre(mxplan),
c             bdcons(mxnsl,mxplan), coca(mxnsl,mxplan), wrdk(mxnsl,mxplan)
c     modify: cpm(mxnsl,mxplan)
c
      include 'ccontcv.inc'
c       read: tilseq(mxcrop,mxplan)
c
      include 'ccover.inc'
c       read: lanuse(mxplan),cancov(mxplan),canhgt(mxplan),
c             inrcov(mxplan)
c     modify: daydis(mxplan)
c      write: kiadjf(mxplan), kradjf(mxplan), tcadjf(mxplan)
c
      include 'ccrpout.inc'
c       read: rtm15(mxplan)
c     modify: rrc(mxplan),bd(mxnsl,mxplan),rh(mxplan)
c
      include 'ccrpprm.inc'
c       read: iresd(mxplan), itype(mxcrop,mxplan)
c
      include 'cends4.inc'
c     modify: rspace(mxplan)
c
      include 'cflags.inc'
c       read: iflag
c
      include 'chydrol.inc'
c       read: rain
c
      include 'cincon.inc'
c       read:irfcum,ibd
c
      include 'cirriga.inc'
c       read: irdept(mxplan),noirr
c
      include 'cke.inc'
c
      include 'ckrcon.inc'
c       read: krcrat(mxplan), kicrat(mxplan), tccrat(mxplan),
c             bconsd(mxplan)
c
      include 'cparame.inc'
c     modify: por(mxnsl,mxplan)
c
      include 'cridge.inc'
c       read: iridge(mxtlsq)
c
      include 'crinpt1a.inc'
c
      include 'crinpt3.inc'
c       read: spop(ntype), gpop(ntype)
c
      include 'crinpt5.inc'
c       read: tpop(ntype)
c
      include 'crinpt6.inc'
c       read: rrough(mxplan)
c
      include 'crout.inc'
c       read: rooty(mxnsl,mxplan)
c
      include 'cslinit.inc'
c       read: bdtill(mxplan)
c     modify: rrinit(mxplan),rhinit(mxplan),rfcum(mxplan)
c
      include 'cslope2.inc'
c       read: avgslp(mxplan)
c
      include 'csolvar.inc'
c       read: clay(mxnsl,mxelmt)
c
      include 'cstruc.inc'
c       read: iplane
c
      include 'ctemp.inc'
c       read: solth1(mxnsl,mxplan)
      include 'ctillge.inc'
c       read: tildep(mxtill,mxtlsq), typtil(mxtill,mxtlsq),
c             rro(mxtill,mxtlsq), rho(mxtill,mxtlsq)
c
      include 'cupdate.inc'
c       read: sdate, mdate(mxtill,mxtlsq), indxy(mxplan)
c
      include 'cwater.inc'
c       read: st(mxnsl,mxplan), thetdr(mxnsl,mxplan), thetfc(mxnsl,mxplan),
c             nsl(mxplan)
c     modify: ssc(mxnsl,mxplan)
c
      include 'cwint.inc'
c       read: frdp(mxplan), thdp(mxplan)
c     modify: frozen(mxnsl,mxplan),tens(mxplan)
c
c     + + + LOCAL VARIABLES + + +
      real daycnt(mxplan,2), denom, bdtll(2,mxplan), ao(2,mxplan),
     1    sscunf(mxnsl,mxplan)
      real acyc, produc, ckrasc, ctcasc, frsl, frzlay, rkiatl, smf,
     1    tcaft, pfc
      integer plant, i, kk
      integer i1, i2, n2
      real bdirf, ckiadr, ckiaft, ckialr, ckiasa, ckiasc, ckiawc,
     1    ckraft, ckrawc, ckrbgb, bddiff, bdiwt, rkiacr, rkialc, rkratl,
     1    rkracr, rkralc, ckiacc, ckiagc, rkiacc, rkiagc,
     1    ckradr, ckralr, ctcarr
      real slo, pwater, tenkpa, frof, cbr,fzrati

c
c     + + + LOCAL DEFINITIONS + + +
c     daycnt - counter to track days since last tillage disturbance
c              to layers 1 and 2
c     denom  - average slope of row sideslope
c     bdtll  - the bulk density after tillage for top two soil layers
c     ao     - maximum change in bulk density (compaction) due to
c              rainfall (assumed to occur after RFCUM reaches 0.1)
c     sscunf - unfrozen saturated hydraulic conductivity (SSC)
c     rh     - ridge height
c     acyc   - the main constant for the interrill erodibility adjustment
c              due to frost and thaw is based upon the number of freeze-thaw
c              cycles.  Once ACYC reaches 10 cycles it is set equal to 1.31.
c     sm20c  - the 20 centimeters water content
c     plant  - plant type
c     i1     - operation index of tillage sequence
c     i2     - tillage sequence index
c     n2     - number of layers to process in do-10 loop
c     bdirf  - bulk density increase due to rainfall
cC    rhaft  - ridge height adjustment for freeze/thaw
cC    rraft  - random roughness adjustment for freeze/thaw
c     wrd    - residual moisture content
c     epor   - effective porosity
c     smf    - soil moisture at freezing
c     ckiacc - adjustment to interrill erodibility for canopy cover
c     ckiadr - adjustment to interrill erodibility for dead root biomass
c     ckiaft - adjustment to interrill erodibility for freezing/thawing
c     ckiagc - adjustment to interrill erodibility for ground cover
c     ckialr - adjustment to interrill erodibility for live root biomass
c     ckiasa - adjustment to interrill erodibility for row sideslope
c     ckiasc - adjustment to interrill erodibility for soil consolidation
c     ckiawc - adjustment to interrill erodibility for wheel compaction
c     ckraft - adjustment to rill erodibility for freeze/thaw
c     ckrawc - adjustment to rill erodibility for wheel compaction
c     ckrbgb - adjustment to rill erodibility for burried residue mass
c     ckradr - adjustment to rill erodibility for dead root
c     ckralr - adjustment to rill erodibility for live root
c     ctcarr - adjustment to tc for random roughness
c     cbr    - adjustment of buried residue to random roughness
c              decay coefficient
c     bddiff - difference between consolidated bulk density (BDCONS) and
c              bulk density corrected for tillage and rainfall
c     bdiwt  - bulk density increase due to weathering
c     rkiacc - rangeland interrill adjustment for canopy cover
c     rkiacr - rangeland interrill adjustment for live & dead roots
c     rkiagc - rangeland interrill adjustment for ground cover
c     rkialc - rangeland interrill adjustment for livestock compaction
c     rkratl - rangeland interrill adjustment for tillage
c     rkracr - rangeland interrill adjustment for live & dead roots
c     rkralc - rangeland interrill adjustment for livestock compaction
c     slo    - approximate slope of the soil matric potential line
c     pwater - total soil water content in the topmost layer  (m/m)
c     tenkpa - soil matric potential in topmost layer (KiloPascals)
c
c     + + + SAVES + + +
      save ao, bdtll, daycnt, sscunf, frof
      data frof/1.0/
c
c     + + + SUBROUTINES CALLED + + +
c     INFPAR
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      plant = itype(nowcrp,iplane)
c
c     *** L0 IF ***
c     ---- NOT the first day of simulation....
      if (iflag.ne.0) then
c
c       Set space between rills (temporarily assumed constant):
c
c       -------- reset the ssc to the unfrozen value
        do 10 i = 1, nsl(iplane)
c       comment from ftnchek 01-12-94 08:39am  sjl
c       Variables may be used before set in module SOIL:
c         SSCUNF
          ssc(i,iplane) = sscunf(i,iplane)
   10   continue
c
c       *** M1 IF ***
c       CROPLAND
        if (lanuse(iplane).eq.1) then
c
c         *** M2 IF ***
c         Check if there is tillage for the year and if first management
c         date has been reached
          if (tilseq(nowcrp,iplane).gt.0.and.indxy(iplane).gt.0) then
c
c           ---------- indices for tillage operations
            i1 = indxy(iplane)
            i2 = tilseq(nowcrp,iplane)
c
c           *** M3 IF ***
c           If there is tillage today, adjust counter(s), bulk density,
c           random roughness and ridge height.
c
            if (sdate.eq.mdate(i1,i2).and.tildep(i1,i2).gt.0.) then
c
c             ------------ Update the days since disturbance counter which is
c             used in the computation of the erodibility parameter
c             adjustments.
c
              if (ivers.ne.3) write (6,*) 'OPERATION ON PLANE',
     1            iplane, ' ON DAY', sdate
c
              if (ivers.eq.3) write (6,*) 'OPERATION on channel ',
     1            iplane, ' on day ', sdate
c
c             NOTE:  changed equation for daydis so that it is reset to
c             zero when any tillage occurs that has an average tillage
c             depth that is greater than zero.
c             (suggested by Laflen and Nearing  4/12/93)  dcf
c             daydis(iplane) = daydis(iplane) *
c             1                         (1.0 - mfo(i1,i2,iresd(1,iplane)))
c
c             ------------ Average tillage depth is greater than zero
              if (tdmean(i1,i2).gt.0.0) then
c
                daydis(iplane) = (1.0-surdis(i1,i2)) * daydis(iplane)
c
c               Set kecum =0  (from Risse    dcf  1/3/94)
                rkecum(iplane) = 0.0
c
c               -------------- secondary tillage
                if (typtil(i1,i2).eq.2) then
                  n2 = 1
                  daycnt(iplane,1) = 0.0
                  daycnt(iplane,2) = daycnt(iplane,2) + 1.0
c               ------------ primary tillage
                else
                  n2 = 2
                  daycnt(iplane,1) = 0.0
                  daycnt(iplane,2) = 0.0
                end if
c
c               If the user has indicated temporary rill types - then rill
c               width is set back to zero here when tillage occurs.
c
                if (rwflag(iplane).eq.1) then
                  width(iplane) = 0.0
                  wdhtop(iplane) = 0.0
                endif
c
c             For the case of an implement with an average tillage depth of 0.0,
c             we still have to set a value for n2:  assume secondary tillage for
c             bulk density computations below - BUT, do not reset daycnt to 0.
              else
                n2 = 1
                daycnt(iplane,1) = daycnt(iplane,1) + 1.0
                daycnt(iplane,2) = daycnt(iplane,2) + 1.0
                if (rfcum(iplane).gt.0.01) daydis(iplane) =
     1              daydis(iplane) + 1.0
              end if
c
c             ------------ initial random roughness after tillage operation
c             XXX Note - When separate input value of MFO are available for both
c             interrill and rill areas, change needs to be made here.
c             Could use either the Interrill value of MFO, OR could
c             use the new SURDIS variable, OR could compute a weighted
c             average of MFO based on rill spacing and rill width.  A
c             more difficult option would be to compute separate
c             roughness values on both the interrill and rill areas.
c             dcf  8/25/93
c             Changed from use of MFO to SURDIS(for nearing) dcf 12/1/93
c             rrinit(iplane) = rro(i1,i2) * mfo(i1,i2,iresd(1,iplane)) +
c             1            rrc(iplane) * (1-mfo(i1,i2,iresd(1,iplane)))
c
              rrinit(iplane) = rro(i1,i2) * surdis(i1,i2) +
     1            rrc(iplane) * (1.0-surdis(i1,i2))
              if (rrinit(iplane).lt.0.006) rrinit(iplane) = 0.006
c
c             ------------ initial ridge height after tillage operation
              if (rhinit(iplane).lt.rho(i1,i2)) rhinit(iplane) =
     1            rho(i1,i2)
              if (rhinit(iplane).lt.0.006) rhinit(iplane) = 0.006
c
c
c             Adjust primary and secondary tillage layers for
c             tillage effect on bulk density
c
              do 20 i = 1, n2
c
c               XXX Note - When separate input value of MFO are available for both
c               interrill and rill areas, change needs to be made here.
c               Could use either the Interrill value of MFO, OR could
c               use the new SURDIS variable, OR could compute a weighted
c               average of MFO based on rill spacing and rill width.  A
c               more difficult option would be to compute separate
c               bulk density values on both the interrill and rill areas.
c               dcf  8/25/93
c               bdtll(i,iplane) = bd(i,iplane) - (bd(i,iplane)-0.667*
c    1              bdcons(i,iplane)) * mfo(i1,i2,iresd(1,iplane))
                bdtll(i,iplane) = bd(i,iplane) - (bd(i,iplane)-0.667*
     1              bdcons(i,iplane)) * surdis(i1,i2)
                if (bdtll(i,iplane).lt.500) bdtll(i,iplane) = 500
                if (bdtll(i,iplane).gt.bdcons(i,iplane))
     1              bdtll(i,iplane) = bdcons(i,iplane)
                bd(i,iplane) = bdtll(i,iplane)
c
c               -------------- maximum change in bulk density due to rainfall.
c               (It is assumed that this is equivalent to the
c               change caused by 0.1m. of cumulative rainfall.)
c               (WEPP Equation 6.7.14)
                ao(i,iplane) = 1650. - (2900.*clay(i,iplane)) + 3000. *
     1              (clay(i,iplane)**2) - (0.92*bdtll(i,iplane))
                if (ao(i,iplane).lt.0.0) ao(i,iplane) = 0.0
   20         continue
c
c
c             ------------ reset cumulative rainfall since last tillage
              rfcum(iplane) = 0.0
c
c           *** M3 ELSE ***
c           if no tillage, increment the days-since-tillage counters by 1
            else
              daycnt(iplane,1) = daycnt(iplane,1) + 1.0
              daycnt(iplane,2) = daycnt(iplane,2) + 1.0
c
c             increment the cumulative days-since-disturbance counter
c             only when the cumulative rainfall since tillage exceeds
c             10 mm.     dcf  2/3/93
c
              if (rfcum(iplane).gt.0.01) daydis(iplane) =
     1            daydis(iplane) + 1.0
c
c           *** M3 ENDIF ***
            end if
c
c         *** M2 ELSE ***
c         if no tillage, increment the days-since-tillage counters by 1
          else
            daycnt(iplane,1) = daycnt(iplane,1) + 1.0
            daycnt(iplane,2) = daycnt(iplane,2) + 1.0
            if (rfcum(iplane).gt.0.01) daydis(iplane) = daydis(iplane) +
     1          1.0
c
c         *** M2 ENDIF ***
          end if
c
c---------------------------------------------------------------------------
c         Bulk density consolidation section
c         (calculated on the primary and secondary tillage layers.)
c---------------------------------------------------------------------------
c
c         kottwitz 2-14-91
c         modification assumes that water from furrow irrigation should
c         not be considered since furrow irrigation is a much different
c         process from rainfall (or sprinkler irrigation)
c
c         -------- compute rainfall accumulated since latest tillage operation
c         (Add today's rainfall & depth of water applied by irrigation.)
c
c
          do 30 i = 1, 2
c
c           if current bulk density is fully consolidated skip to
c           porosity calculations
c           *** N2 IF ***
            if (bd(i,iplane).lt.bdcons(i,iplane)) then
c
c             Soil BD Increase Due to Rainfall
c
c             ------------ BD increase from rainfall using the bulk density for 0.1 m.
c             cumulative rainfall since last tillage (AO), and the current
c             cumulative rainfall since last tillage (RFCUM).
c             (WEPP Equation 6.7.13)
              bdirf = ao(i,iplane) * (rfcum(iplane)/(.01+rfcum(iplane)))
c
c             ********************************************************
c             * Note: This is *NOT* the *LINEAR* relationship Savabi *
c             *       indicated.  For what he indicated, you need:   *
c             *              bdirf=ao(i,iplane)*(rfcum(iplane)/0.01) *
c             *       CRM -- 10/28/92                                *
c             ********************************************************
c
c             ------------ difference between maximum bulk density (BDCONS), and the
c             current bulk density after tillage (BDTLL) plus BD after
c             0.1 m. rainfall (AO).
c             (WEPP Equation 6.7.15)
              bddiff = bdcons(i,iplane) - (bdtll(i,iplane)+
     1            ao(i,iplane))
c
c             ***********************************************************
c             * Note: In the User Doc, P(t) in this Eq. 6.7.15 is *NOT* *
c             *       the same as the P(t) in Eq. 6.7.12.  Re: 10/20/92 *
c             *       conversation with Reza Savabi -- CRM -- 10/21/92. *
c             ***********************************************************
c             daily increase in bulk density due to weathering and longer
c             term soil consolidation.
c             (Combination WEPP Equations 6.7.16 & 6.7.17)
              bdiwt = bddiff * (1.0-(exp(-0.005*daycnt(iplane,i))))
c
c             calculate bulk density from BD after tillage, increase due
c             to rainfall (BDIRF), and increase due to weathering (BDIWT).
              bd(i,iplane) = bdtll(i,iplane) + bdirf + bdiwt
c
c             ensure the current bulk density (BD) doesn't exceed the
c             maximum value for bulk density (BDCON).
              if (bd(i,iplane).gt.bdcons(i,iplane)) bd(i,iplane) =
     1            bdcons(i,iplane)
c
c-----------------------------------------------------------------------
c
c             surface roughness coefficients
c
c-----------------------------------------------------------------------
c
c             compute product of random roughness parameter (RRE) and
c             cumulative rainfall since last tillage (RFCUM).
c             (WEPP Equation 6.6.1)
c
c   ZZZ       added by zhang 10/5/94
c             modify random roughness decay coefficient
              cbr = 1. - .5 * (smrm(1,iplane)+smrm(2,iplane)+
     1                 smrm(3,iplane))
c             set minimum value of cbr to 0.3
              if (cbr .lt. 0.3) cbr = 0.3
c
              produc = rre(iplane) *cbr* (rfcum(iplane)*1000)**0.6
c
c             PRODUC value is used to prevent a numeric underflow
c             particularly for cases on long term no-till simulations.
              if (produc.gt.-10.0) then
c
c               compute daily random roughness coefficient from initial
c               ridge roughness
                rrc(iplane) = rrinit(iplane) * exp(produc)
                if (rrc(iplane).lt.0.006) rrc(iplane) = 0.006
c
c               compute daily ridge height coef. from initial ridge
c               height
                rh(iplane) = rhinit(iplane) * exp(produc)
                if (rh(iplane).lt.0.006) rh(iplane) = 0.006
              else
                rrc(iplane) = 0.006
                rh(iplane) = 0.006
              end if
c
c             adjustment factor for random roughness to account
c             for freeze-thaw
c
c             rraft=1.0
c             rrc(iplane)=rrc(iplane)*rraft
c
c
              if (rrc(iplane).lt.0.006) rrc(iplane) = 0.006
              if (rh(iplane).lt.0.006) rh(iplane) = 0.006
c
c             check minimum values for ridge height (RH) in ridge tillage
              if (tilseq(nowcrp,iplane).gt.0) then
                if (iridge(tilseq(nowcrp,iplane)).eq.1) then
                  if (rh(iplane).lt.0.10) rh(iplane) = 0.10
                else
                  if (rh(iplane).lt.0.006) rh(iplane) = 0.006
                end if
              end if
c
c
c             adjustment factor for ridge height to account for freeze-thaw
c
c             if(rh(iplane).gt.0.076)rhaft=1.0
c             -- XXX -- Huh?  Why the next line?  -- CRM -- 9/29/92
c             rhaft=1.0
c             rh(iplane)=rh(iplane)*rhaft
c
c-----------------------------------------------------------------------
c
c             porosity calculations
c
c-----------------------------------------------------------------------
c
c             ------------ compute total soil porosity (m**3/m**3)
c             (WEPP Equation 6.8.1)
              por(i,iplane) = ((2650.0-bd(i,iplane))/2650.0)
c
c             ------------ correct porosity for entrapped air
              por(i,iplane) = por(i,iplane) * coca(i,iplane)
c
c           *** N2 ENDIF ***
            end if
c
c           ---------- reset the unfrozen Ksat to newly-tilled Ksat
            sscunf(i,iplane) = ssc(i,iplane)
c
   30     continue
c
c       *** M1 ELSE ***
        elseif (lanuse(iplane).eq.2) then
c       RANGELAND
c
c           NEW KIDWELL EQUATION AS OF June 7, 1995   dcf
c
          if(ksflag.ne.0)then
            do 33 i=1,2
              if(rilcov(iplane).lt.0.45)then
                 ssc(i,iplane) = 57.99
     1           - (14.05 * alog(cec(1,iplane)))
     1           + (6.20 * alog(rooty(1,iplane)))
     1           - (473.39 * (fbasr(iplane)*bascov(iplane))**2)
     1           + (4.78 * fresi(iplane)*rescov(iplane))
              else
                 ssc(i,iplane) = -14.29
     1           - (3.40 * alog(rooty(1,iplane)))
     1           + (37.83 * sand(1,iplane))
     1           + (208.86 * orgmat(1,iplane))
     1           + (398.64 * rrough(iplane))
     1           - (27.39 * fresi(iplane)*rescov(iplane))
     1           + (64.14 * fbasi(iplane)*bascov(iplane))
              endif
c
c             Limit EFFECTIVE baseline conductivity value to 0.2 mm/hr
c             minimum.
c
              if (ssc(i,iplane).lt.0.2) ssc(i,iplane) = 0.2
c
c             Convert from mm/hr to meters/second
c
              ssc(i,iplane) = ssc(i,iplane) / 3.6e6
c
c             reset the unfrozen Ke to newly-computed Ke
              sscunf(i,iplane) = ssc(i,iplane)
 33         continue
          endif
c
c       *** M1 ENDIF ***
        end if
c
c       correct saturated hydraulic conductivity for frozen ground
c       frozen ground factor, smf= soil water at freezing

        do 35 kk = 1, nsl(iplane)-1
c
cd    Added by S. Dun, Dec 03, 2007
            sscv(kk,iplane) = 0.0
cd    End adding
c
cd          if((frdp(iplane)-thdp(iplane)) .le. 0.0) goto 35
cd          smf=amtfrz(iplane)/(frdp(iplane)-thdp(iplane))
cd          pfc=smf/thetfc(kk,iplane)*100.
cd          if(pfc.ge. 100) then
cd            frof = 0.1
cd          else
cd            frof=3.75*exp(-0.026*pfc)
cd            if(frof.gt.2.0)frof=2.0
cd          endif
c
   35   continue
c
cd    Added by S. Dun, Dec 03, 2007
        if (frdp(iplane) .gt. 0.) then
            call frsoil(sscunf)
        endif
cd    End adding
c
c
cd        do 40 i = 1, nsl(iplane)
cd         ---------- reset FROZEN to zero
cd          if (frozen(i,iplane).gt.0.0) frozen(i,iplane) = 0.0
cd   40   continue
c
c-----------------------------------------------------------------------
c
c
c     *** L0 ELSE ***
      else
c       ---- FIRST DAY IN MODEL SEQUENCE.
c
c       ------ initialize unfrozen Ksat's to initial Ksat's
        do 50 i = 1, nsl(iplane)
          sscunf(i,iplane) = ssc(i,iplane)
   50   continue
c
c       if there is tillage such that the initial ridge height >= 1/10 m
c       then set the initial ridge height to 1/10 m.
c       if((tilseq(nowcrp,iplane).gt.0) .and.
c       1     (iridge(tilseq(nowcrp,iplane)).eq.1)) rhinit(iplane)=0.10
c
c       *********************************************************************
c       * NOTE: The above lines set the initial ridge height to 1/10 meter  *
c       *       in a ridge-till system, REGARDLESS OF WHAT THE USER INPUTS. *
c       *       This was changed at the request of D. Flanagan following    *
c       *       10/28/92 conversation.  -- CRM -- 10/28/92.                 *
c       *********************************************************************
c
c
c       (Assume the bulk density of soil layers 1 and 2 are equal
c       on day 1.)
c
c       *** L1 IF ***
c       CROPLAND
        if (lanuse(iplane).eq.1) then
c
          do 60 i = 1, 2
            daycnt(iplane,i) = daydis(iplane)
            bdtll(i,iplane) = bdtill(iplane)
c
c           ---------- calculate maximum adjustment to bulk density due to
c           rainfall (the amount predicted to occur with 1/10 meter
c           cumulative rainfall; ie, when RFCUM = 0.1.)
c           (WEPP Equation 6.7.14)
            ao(i,iplane) = 1650. - (2900.*clay(i,iplane)) + 3000. * (
     1          clay(i,iplane)**2) - (0.92*bdtill(iplane))
            if (ao(i,iplane).lt.0.0) ao(i,iplane) = 0.0
c
c           ---------- BD increase from rainfall using the bulk density for 0.1 m.
c           cumulative rainfall since last tillage (AO), and the current
c           cumulative rainfall since last tillage (RFCUM).
c           (WEPP Equation 6.7.13)
            bdirf = ao(i,iplane) * (rfcum(iplane)/(.01+rfcum(iplane)))
c
c           ---------- difference between maximum bulk density (BDCONS), and the
c           current bulk density after tillage (BDTLL) plus BD after
c           0.1 m. rainfall (AO).
c           (WEPP Equation 6.7.15)
            bddiff = bdcons(i,iplane) - (bdtll(i,iplane)+ao(i,iplane))
c
c           ---------- daily increase in bulk density due to weathering and longer--
c           term soil consolidation.
c           (Combination WEPP Equations 6.7.16 & 6.7.17)
            bdiwt = bddiff * (1.0-(exp(-0.005*daycnt(iplane,i))))
c
c           ---------- calculate bulk density from BD after tillage, increase due
c           to rainfall (BDIRF), and increase due to weathering (BDIWT).
            bd(i,iplane) = bdtll(i,iplane) + bdirf + bdiwt
c
c           ---------- ensure the current bulk density (BD) doesn't exceed the
c           maximum value for bulk density (BDCON).
            if (bd(i,iplane).gt.bdcons(i,iplane)) bd(i,iplane) =
     1          bdcons(i,iplane)
c
   60     continue
c
c         -------- ridge height coefficient
          rh(iplane) = rhinit(iplane)
c         -------- random roughness coefficient
          rrc(iplane) = rrinit(iplane)
c
c
c       *** L1 ELSE ***
c       OTHER (Range or Forest)
        else
c
c         RANGELAND
          if (lanuse(iplane).eq.2) then
c           if there are shrubs or the maximum plant height exceeds
c           0.3 m, then calculate rill spacing.
            if (spop(plant).gt.0.0.or.hmax(plant).gt.0.30) then
              rspace(iplane) = 100.0 / (spop(plant)+gpop(plant)+
     1            tpop(plant)+1)
c           if there aren't shrubs and the maximum plant height is less
c           than 0.3 meter ...
            else
c             set space between rills (temporarily assumed constant):
              rspace(iplane) = 1.0
            end if
            if (rspace(iplane).gt.5.0) rspace(iplane) = 5.0
            if (rspace(iplane).lt.0.5) rspace(iplane) = 0.5
c
c           NEW KIDWELL EQUATION AS OF June 7, 1995   dcf
c
cdcf        do 70 i=1,2
cdcf          if(rilcov(iplane).lt.0.45)then
cdcf             ssc(i,iplane) = 57.99
cdcf 1           - (14.05 * alog(cec(1,iplane)))
cdcf 1           + (6.20 * alog(rooty(1,iplane)))
cdcf 1           - (473.39 * (fbasr(iplane)*bascov(iplane))**2)
cdcf 1           + (4.78 * fresi(iplane)*rescov(iplane))
cdcf          else
cdcf             ssc(i,iplane) = -14.29
cdcf 1           - (3.40 * alog(rooty(1,iplane)))
cdcf 1           + (37.83 * sand(1,iplane))
cdcf 1           + (208.86 * orgmat(1,iplane))
cdcf 1           + (398.64 * rrough(iplane))
cdcf 1           - (27.39 * fresi(iplane)*rescov(iplane))
cdcf 1           + (64.14 * fbasi(iplane)*bascov(iplane))
cdcf          endif
c
c             Limit EFFECTIVE baseline conductivity value to 0.2 mm/hr
c             minimum.
c
cdcf          if (ssc(i,iplane).lt.0.2) ssc(i,iplane) = 0.2
c
c             Convert from mm/hr to meters/second
c
cdcf          ssc(i,iplane) = ssc(i,iplane) / 3.6e6
c
c             CORRECTION - to fix incorrect setting of minimum
c             conductivity value that is used to compute the
c             profile saturation equations of Savabi in
c             subroutine GRNA.  dcf  10/6/99
c
cdcf          if (i.eq.1) then
cdcf            sscmin(iplane) = ssc(i,iplane)
cdcf          else
cdcf            if (ssc(i,iplane).lt.sscmin(iplane))
cdcf 1              sscmin(iplane) = ssc(i,iplane)
cdcf          end if
c
c70         continue
          end if
c
c         -------- ridge height coefficient
          rh(iplane) = rrough(iplane)
c         -------- random roughness coefficient
          rrc(iplane) = rrough(iplane)
c
c       *** L1 ENDIF ***
        end if
c
c     *** L0 ENDIF ***
      end if
c
c
c     Call the infiltration parameter subroutine (INFPAR) after
c     the saturated hydraulic conductivity loop has ended.
c
      call infpar(nowcrp,jdplt(nowcrp,iplane),plant,irsyst)
c
c     Update the cumulative rainfall since last tillage here -
c     after the infiltration parameters for the current day have
c     been computed - thus rainfall and irrigation today affects
c     crusting, infiltration, erodibility tomorrow.  dcf  2/3/93
c
c XXX PROBLEM HERE - Winter is called after SOIL - and it is WINTER
c XXX routines which now determine whether PRECIP is rain or snow.
c XXX Temporary fix  6/3/94   dcf
c     rfcum(iplane) = rfcum(iplane) + rain + irdept(iplane)
c
c XXX Another question here is whether FURROW irrigation water should
c XXX be added to RFCUM ?  Furrow water excluded here now.  dcf 12/15/94
c
      if(irsyst.le.1)then
        if(tave.gt.0.0)then
          rfcum(iplane) = rfcum(iplane) + prcp + irdept(iplane)
        else
          rfcum(iplane) = rfcum(iplane) + irdept(iplane)
        endif
      else
        if(tave.gt.0.0)rfcum(iplane)=rfcum(iplane) + prcp
      endif

c
c-----------------------------------------------------------------------
c
c     FREEZE AND THAW ADJUSTMENTS TO ERODIBILITY VALUES (Ki,Kr,Tauc)
c
c     7/9/91 add adjustment based on effective matric potential (sm)
c     Improved equations from Bob Young added 2/13/92 by dcf
c     Corrections added 4/21/93 to compute the correct soil matric
c     potential needed in these calculations (dcf and savabi)
c
cd    Modified by S. Dun, 04/09/2008 for frozen soil
      fzrati = (dg(1,iplane)-frozen(1,iplane))/dg(1,iplane)
      if (fzrati. gt.0.01) then
      pwater = (st(1,iplane)/fzrati + thetdr(1,iplane)*dg(1,iplane))
     1         /dg(1,iplane)
      else
      pwater = (st(1,iplane)+thetdr(1,iplane)*dg(1,iplane)) /
     1    dg(1,iplane)
      endif
cd   End modifying
c
      if (frdp(iplane).gt.0.0.and.thdp(iplane).le.0.0) then
        ifrost(iplane) = 1
        ckiaft = 0.0
        ckraft = 0.0
        tcaft = 1.0
      else if (pwater.le.thetfc(1,iplane)) then
        ifrost(iplane) = 0
        ckiaft = 1.0
        ckraft = 1.0
        tcaft = 1.0
      else if (ifrost(iplane).gt.0) then
        ifrost(iplane) = 2
      else
        ckiaft = 1.0
        ckraft = 1.0
        tcaft = 1.0
      end if
c
c     ---- Once thawing of a frozen soil is detected....
      if (ifrost(iplane).eq.2) then
c
c       compute the soil matric potential in kiloPascals.  The
c       following equation is an approximation valid only between
c       the range of saturation and field capacity. (from Savabi)
c
c       NOTE replaced Savabi code of setting te33=33 and t33=log10(te33)
c       with the constant value 1.518514 in equation for slo
c       dcf 5/14/93
c
        slo = (100.*(thetfc(1,iplane)-por(1,iplane))) / 1.518514
c
c       if the water present is less than the porosity compute
c       the matric potential in kilopascals
c
        if (pwater.lt.por(1,iplane)) then
          tenkpa = 10. ** ((100.*(por(1,iplane)-pwater))/abs(slo))
c
c       else if the water present is greater than or equal to the
c       porosity set the matric potential to 1.0 kilopascal
c
        else
          tenkpa = 1.0
        end if
c
c   The WINTER model needs to store water tension in units of m.
c
        tens(iplane) = tenkpa / 10.
c
c       The main constant for the interrill erodibility adjustment
c       due to frost and thaw is based upon the number of freeze-thaw
c       cycles - once reach 10 cycles - set equal to 1.31
c
        if (fcycle(iplane).lt.11) then
          acyc = 1.0 + 0.0586 * float(fcycle(iplane)) - 0.0027 *
     1        float(fcycle(iplane)**2)
        else
          acyc = 1.31
        end if
c
c       ------ compute interrill erodibility adjustment due to freeze-thaw
        ckiaft = acyc * exp((-alog(acyc))*tenkpa/33.0)
c
c       once 1/10 bar effective matric potential is reached the
c       adjustments for rill erodibility and critical shear are stopped
c
c       ------ if soil matric potential > 1/10-bar....  (10 kiloPascals)
        if (tenkpa.gt.10.0) then
          ckraft = 1.0
          tcaft = 1.0
c
c       ------ if soil matric potential <= 1/10-bar....  (10 kiloPascals)
        else
          ckraft = 2.0 * (0.933) ** tenkpa
          tcaft = 0.875 + 0.0543 * alog(tenkpa)
        end if
c
      end if
c
c
c     *** P0 IF ***
c     CROPLAND
      if (lanuse(iplane).eq.1) then
c
c-----------------------------------------------------------------
c       Interrill erodibility (Ki) adjustments for cropland
c-----------------------------------------------------------------
c
c       Move updating of days since disturbance parameter from SR CONTIN
c       to SR SOIL.  This will ensure that updating of all variables that
c       affect soil parameters (Ki, Kr, Tauc, etc.) adjustments are done
c       in the same place.   dcf  1/29/93
c
c       ------ canopy effects  (Laflen equation  4/13/93  dcf )
c
        if (canhgt(iplane).gt.0.0) then
          ckiacc = 1.0 - (2.941*cancov(iplane)/canhgt(iplane)) *
     1        (1.0-exp(-0.34*canhgt(iplane)))
        else
          ckiacc = 1.0 - cancov(iplane)
        end if
c
c       ------ ground cover effects (originally in function INRDET)
c
        ckiagc = exp(-2.5*inrcov(iplane))
c
c       ------ live root biomass (WEPP Equation 6.11.2)
        ckialr = exp(-0.56*rtm15(iplane))
c
c       ------ dead root biomass (WEPP Equation 6.11.1)
        ckiadr = exp(-0.56*(rtm(1,iplane)+rtm(2,iplane)+
     1      rtm(3,iplane)))
c
c
c       ------ sealing and crusting
c
c       Variable produc is used in a trap to prevent numeric underflows
c       It is also used below in Kr and Tauc consolidation equations
c
        produc = bconsd(iplane) * daydis(iplane)
c
        if (produc.lt.10.0) then
          ckiasc = kicrat(iplane) + (1-kicrat(iplane)) * exp(-produc)
        else
          ckiasc = kicrat(iplane)
        end if
c
c       Interrill slope adjustment (0 < denom < .707)
c       ------ equivalent of "sin(S)", assuming the furrows are triangular.
c
c       Change suggested by Nearing 11/22/93 to use OFE average slope
c       if it is steeper than row sideslopes for slope factor
c       adjustment.   dcf   11/22/93
c
        if ((rh(iplane)/(rspace(iplane)/2.)).gt.avgslp(iplane)) then
          denom = rh(iplane) /
     1        sqrt((rspace(iplane)/2.)**2+rh(iplane)**2)
        else
c
c         Logic added 3/4/97 to prevent a decrease in predicted
c         interrill detachment for extremely steep slopes (300%)
c         dcf
          if(avgslp(iplane) .lt. 0.7854)then
            denom = sin(avgslp(iplane))
          else
            denom = 0.707
          end if
        end if
c
c       ------ constrain row sideslope to <= 45 degrees
        if (denom.gt..707) denom = .707
c       ------ (Equation 3 -- NSERL Report No. 3)
        ckiasa = 1.05 - .85 * exp(-4.*denom)
c
c       ------ wheel compaction
        ckiawc = 1.0
c
c       ------ Total Ki adjustment factor
        kiadjf(iplane) = ckiacc * ckiagc * ckialr * ckiadr * ckiasc *
     1      ckiaft * ckiasa * ckiawc
        if (kiadjf(iplane).lt.0.03) kiadjf(iplane)=0.03
c
c-------------------------------------------------------------
c       cropland rill erodibility (Kr) and critical shear stress
c-------------------------------------------------------------
c
c       ------ incorporated residue and roots
c       (WEPP Eq. 6.14.1, adapted to include 3 types of residue)
c
c       ... adjustment of kr to buried residue
        ckrbgb = exp(-.40*(smrm(1,iplane)+smrm(2,iplane)+
     1      smrm(3,iplane)))
c
c       ... adjustment of kr to dead root
        ckradr = exp(-2.2 * (rtm(1,iplane)+rtm(2,iplane)+rtm(3,iplane)))
c       ... adjustment of kr to live root
        ckralr = exp(-3.5*rtm15(iplane))
c
c       ... adjustment of tc to random roughness
        ctcarr = 1. + 8.0 * (rrc(iplane)-0.006)
c
c       Consolidation model
c       ------ increase in soil resistance due to drying and time
c       -- XXX -- (WEPP Eq. -- NOT FOUND.  This equation is a function only
c       of "days since disturbance by tillage".  -- CRM -- 10/6/92)
c
c       ***********************************************************************
c       * NOTE: This equation is not included in the WEPP User Documentation. *
c       *       Nearing reports it is based on Larry Brown's thesis.  It is   *
c       *       presently a function only of "days since disturbance by til-  *
c       *       lage".  Intentions to also include drying effects were        *
c       *       stymied by inability to understand the water balance code.    *
c       *       CRM -- 10/29/92.                                              *
c       ***********************************************************************
c
c       ------ adjustment to Kr due to consolidation
c       (WEPP Eq. 6.14.2)
c       Changed by mark nearing 1/22/93
cccccccccccccccccccccccccccc
c       rprime=1.0+alog10(daydis(iplane)+1)
c       ckrasc = 1.0/rprime
c       ------ critical shear adj. factor for consolidation
c       (Combination of WEPP Eq. 6.14.2 and 6.16.3)
c       tcadjf(iplane) = (1.0 + rprime)/2.0 * tcaft
cccccccccccccccccccccccccccc
c       new adjustment by mark nearing
cccccccccccccccccccccccccccc
c       ------ adjustments to Kr and Tauc due to consolidation
c
c       Variable produc is computed above in interrill computations.
c       This is a trap used to prevent numeric underflow.
c

        if (produc.lt.10.0) then
          ckrasc = krcrat(iplane) + (1.0 - krcrat(iplane)) *
     1        exp(-produc)
          ctcasc = tccrat(iplane) - (tccrat(iplane)-1.0) *
     1        exp(-produc)
        else
          ckrasc = krcrat(iplane)
          ctcasc = tccrat(iplane)
        end if
c
c       ------ wheel compaction
        ckrawc = 1.0
c
c       ------ Total Kr adjustment factor
        kradjf(iplane) = ckrbgb * ckrasc * ckraft * ckrawc *
     1  ckradr * ckralr
        if (kradjf(iplane).lt.0.03) kradjf(iplane)=0.03
c
c       ------ Total critical shear stress adjustment factor
        tcadjf(iplane) = tcaft * ctcasc * ctcarr
        if (tcadjf(iplane).gt.2.0) tcadjf(iplane)=2.0
c
c     *** P0 ELSEIF ***
c     RANGELAND
      else if (lanuse(iplane).eq.2) then
c
c---------------------------------------------------------------
c       rangeland interrill erodibility
c---------------------------------------------------------------
c
c       ------ canopy cover  (originally in function INRDET)
        rkiacc = 1.0
c
c       ------ ground cover  (originally in function INRDET)
c
c       Following change in calculation of RKIAGC from
c       Mary Kidwell, 3/95
c
c       if (thgt(plant).gt.0.0.or.shgt(plant).gt.0.0) then
c         rkiagc = exp(-5.0*inrcov(iplane))
c       else
c         rkiagc = exp(-7.0*inrcov(iplane))
c       end if
        rkiagc = exp(-7.0*(inrcov(iplane) + cancov(iplane)))
c
c       ------ tillage
        rkiatl = 1.0
c
c       ------ live and dead root biomass
        rkiacr = 1.0
c
c       ------ freezing and thawing  (See adjustments made above)
c       rkiaft=ckiaft
c
c       ------ livestock compaction
        rkialc = 1.0
c
c       ------ Total Ki adjustment factor
        kiadjf(iplane) = rkiacc * rkiagc * rkiatl * rkiacr * ckiaft *
     1      rkialc
c
c------------------------------------------------------------------
c
c       rangeland rill erodibility
c
c------------------------------------------------------------------
c
c       ------ tillage
        rkratl = 1.0
c
c       ------ live and dead root biomass
        rkracr = 1.0
c
c       ------ freezing and thawing  (See adjustments made above)
c       rkraft=ckraft
c
c       ------ livestock compaction
        rkralc = 1.0
c
c       ------ Total Kr adjustment factor
        kradjf(iplane) = rkratl * rkracr * ckraft * rkralc
c
c       ------ Tc adjustment factor
        tcadjf(iplane) = tcaft
c
c     *** P0 ELSE ***
c     FOREST
      else
c
c     (This Loop Left Intentionally Blank.)
c
c     *** P0 ENDIF ***
      end if
c
c     saves bd of top layer on ofe 1 after last tillage for use in initial
c     condition scenario creation
c     saves rfcum of 1st ofe
c
      if (lanuse(iplane).eq.1) then
        ibd = bdtll(1,1)
      else
        ibd = bdcons(1,1)
      end if
c
      irfcum = rfcum(1)
c
c      Write(61, 1505) sdate, year,iplane,kradjf(iplane),
c     1                rkratl,rkracr,ckraft,rkralc,
c     1                ckrbgb,ckrasc,ckrawc,ckradr,ckralr
1505  format(1x, 3I6, 10E12.3)
      return
      end
