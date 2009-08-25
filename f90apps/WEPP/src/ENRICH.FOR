      subroutine enrich(kk,xtop,xbot,xdetst,ldtop,ldbot,lddend,theta,
     1    iendfg)
c
c     + + + PURPOSE + + +
c     Computes the new particle size distribution of sediment in
c     runoff following routing through a deposition region; the
c     equation is an analytical solution of the non-dimensional
c     sediment load equation for a depositional region.
c
c     Called from subroutine ROUTE.
c     Author(s): D. Flanagan
c     Reference in User Guide:
c
c     Changes: 1) Common blocks HYDROL was never used.  It was
c                 de-referenced.
c              2) The calculation "ktrato*tcf1(i,iplane)" is assigned
c                 to "tmpvr1", and relocated to the top of the "do 200"
c                 loop.  It cannot be taken OUTSIDE, because it depends
c                 upon I, the counter of the loop.
c              3) In the "do 200" loop, the statements:
c                 "if(phi.gt.100000.)phi = 100000." and
c                 "if(phi.lt.-100000.)phi = -100000." were moved
c                 from BELOW the first IF-ELSE-ENDIF, to the IF.
c              4) The calculations: "xtop + qostar", "xbot + qostar",
c                 and their squares are assigned to "tmpvrX", and
c                 done above the "do 200" loop.
c              5) At the "do 100" loop, the IF was taken outside the loop.
c                 CRM - 2/5/91.
c              6) PHIFLG added to take floating-point comparisons outside
c                 the "do 200" loop.  CRM - 2/5/91.
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 01/14/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer kk, iendfg, phiflg
      real xtop, xbot, xdetst, theta, ldtop, ldbot, lddend
c
c     + + + ARGUMENT DEFINITIONS + + +
c     kk     - slope segment number
c     xtop   - nondimensional horizontal distance where
c              deposition begins
c     xbot   - nondimensional horizontal distance where
c              deposition ends
c     xdetst - normalized distance at top of detachment region
c     ldtop  - nondimensional sediment load at xtop
c     ldbot  - nondimensional sediment load at xbot
c     lddend - nomalized sediment load at top of detachment
c                region.
c     theta  - nondimensional interrill detachment parameter
c     iendfg - flag to indicate last call to enrich at the end
c              of an overland flow element
c     phiflg - flag which determines value of PHI.
c
c     + + + PARAMETERS + + +
      include 'pmxelm.inc'
      include 'pmxhil.inc'
c     read: mxhill
      include 'pmxnsl.inc'
c       read: mxnsl
      include 'pmxpln.inc'
c       read: mxplan
      include 'pmxprt.inc'
      include 'pmxslp.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'cdist2.inc'
c       read: slplen(mxplan)
      include 'cends1.inc'
      include 'cends2.inc'
c       read: ktrato, qout, qin
      include 'cenrpa1.inc'
      include 'cenrpa2.inc'
c     modify: frcflw(mxpart,mxplan),enrato(mxplan),
      include 'chydrol.inc'
c       read: rain(mxplan)
      include 'cinfco1.inc'
      include 'cinfco2.inc'
c       read: qostar,ainftc(mxslp),binftc(mxslp),cinftc(mxslp)
      include 'cirriga.inc'
c       read: irdept(mxplan)
      include 'cpart1.inc'
      include 'cpart2.inc'
c       read: npart,frac(mxpart,mxplan),fall(mxpart,mxplan),
c             frcly(mxpart,mxplan),
c             frslt(mxpart,mxplan),frsnd(mxpart,mxplan),
c             frorg(mxpart,mxplan)
      include 'cpart4.inc'
c       read: fidel(mxpart)
      include 'csolva1.inc'
c       read: sand(mxnsl,mxplan), silt(mxnsl,mxplan),
c             clay(mxnsl,mxplan), orgmat(mxnsl,mxplan)
      include 'cstruc.inc'
c       read: iplane
      include 'ctcfrac.inc'
c       read: tcf1(mxpart,mxplan)
c
c     + + + LOCAL VARIABLES + + +
      real beta, ratio, expon, ratio2, coef1, coef2, term1, term2,
     1    term3, term4a, term4b, ratbot, gadd, aa, bb, cc, pkro,
     1    gu(mxpart), gend(mxpart), phi, ssasol, sumg, ssasnd, ssaslt,
     1    ssacly, ssaorg, sedmax(mxpart), ssased, sumssa,
     1    ftheta(mxpart), intlod, rillod, tmpvr1, tmpvr2, tmpvr3,
     1    tmpvr4, tmpvr5
      integer i, iiflag
c
c     beta   - raindrop induced turbulence coefficient
c              (equation 10.2.4)
c     sumssa - variable used to sum SSA for sediment
c              (equation 10.5.5)
c     sumg   - variable used to sum sediment loads predicted
c              by equation 10.5.2
c     ssasnd - specific surface area of sand in m**2/g
c     ssaslt - specific surface area of silt in m**2/g
c     ssacly - specific surface area of clay in m**2/g
c     ssaorg - specific surface area of organic carbon in
c              m**2/g
c     aa     - adjusted A transport coefficient for particle
c              class i
c     bb     - adjusted B transport coefficient for particle
c              class i
c     cc     - adjusted C transport coefficient for particle
c              class i
c     ftheta - fraction of interrill detachment attributed to
c              particle class i
c     gu     - nondimensional sediment load for particle
c              class i at x=xtop
c     intlod - interrill load contribution from detachment region
c              above deposition region
c     ratio  - ratio of x terms in the last component of
c              equation 10.5.2
c     phi    - nondimensional deposition parameter for a
c              particle class i (equation 10.3.13)
c     expon  - phi correction variable to prevent machine
c              overflow
c     ratio2 - ration correction variable to prevent machine
c              overflow
c     coef1  - coefficients and terms of equations 10.5.1 and
c              10.5.2
c     coef2  - coefficients and terms of equations 10.5.1 and
c              10.5.2
c     term1  - coefficients and terms of equations 10.5.1 and
c              10.5.2
c     term2  - coefficients and terms of equations 10.5.1 and
c              10.5.2
c     term3  - coefficients and terms of equations 10.5.1 and
c              10.5.2
c     term4a - coefficients and terms of equations 10.5.1 and
c              10.5.2
c     term4b - coefficients and terms of equations 10.5.1 and
c              10.5.2
c     gend(mxpart)   : predicted nondimensional sediment load for the
c                      particle class i at x=xbot
c     sedmax(mxpart) : maximum possible sediment load in a particle
c                      class i at x=xbot
c     ratbot - running sum of sediment in classes that have
c              not exceeded sedmax
c     iiflag - flag to indicate if reproportioning among
c              classes should be calculated
c     gadd   - additional sediment load added to particle
c              class i during reporportioning
c     ssased - specific surface area for particle class i in
c              the sediment
c     ssasol - total specific surface area for the insitu
c              soil on current plane
c     i      - counter variable used to indicate particle
c              class
c
c     + + + SUBROUTINES CALLED + + +
c     undflo
c
c     + + + END SPECIFICATIONS + + +
c
c     Change to computation of beta made 11/20/92 to
c     be consistent with similar computation in SR PARAM
c
      if (rain(iplane).le.0.0.and.irdept(iplane).le.0.00001) then
        beta = 1.0
      else
        beta = 0.5
      end if
c
      sumssa = 0.0
      sumg = 0.0
c
c     The specific surface area values for sand, silt, clay,
c     and organic carbon are those used in the CREAMS model,
c     the values given below have units of (m**2)/g
c
      ssasnd = 0.05
      ssaslt = 4.0
      ssacly = 20.0
      ssaorg = 1000.0
c
c     At the beginning of a depositional section (or the last time
c     through at the end of a plane) compute the fraction of each
c     particle type - using a weighted average of sediment inputs
c     from last depositional region and the erosional region between.
c
      if (ldtop.gt.0.00001.and.qout.gt.0.0) then
        intlod = theta * (xtop-xdetst)
        rillod = ldtop - lddend - intlod
        if(rillod.lt.0.0)rillod=0.0
        do 10 i = 1, npart
          frcflw(i,iplane) = (frcflw(i,iplane)*lddend+frac(i,iplane)*
     1        rillod+fidel(i)*intlod) / ldtop
   10   continue
      end if
c
c     Determine if this is the last call at the end of an overland flow
c     element if iendfg=1
c
c     *** L1 IF ***
      if (iendfg.ne.0) then
c
        do 20 i = 1, npart
          if (qout.le.0.0) frcflw(i,iplane) = 0.0
c
c         Calculate the specific surface area of the sediment.
c
          ssased = frcflw(i,iplane) * ((frsnd(i,iplane)*ssasnd+
     1        frslt(i,iplane)*ssaslt+frcly(i,iplane)*ssacly)/(1.0+
     1        frorg(i,iplane))+frorg(i,iplane)*ssaorg/1.73)
          sumssa = sumssa + ssased
   20   continue
c
c       Calculate the specific surface area of the surface soil
c       for the current plane.
c
        ssasol = (orgmat(1,iplane)*ssaorg/1.73) + (sand(1,iplane)*ssasnd
     1      +silt(1,iplane)*ssaslt+clay(1,iplane)*ssacly) / (1.0+
     1      orgmat(1,iplane))
c
c       Calculate an enrichment ratio of the specific surface area.
c
        enrato(iplane) = sumssa / ssasol + 0.005
c
c     *** L1 ELSE ***
      else
c
c
c
c       added 4/6/90 by dcf
c
c
c       If there is water flowing off of the Overland FLow Elements,
c       then calculate the particle sorting and enrichment ratio.
c       (Case 2 and 3 hydrologic planes.)
c       *** L2 IF ***
        if (qout.gt.0.0) then
          pkro = (qout-qin) / slplen(iplane)
c
c         Proportion transport capacity to the individual size classes.
c         Calculate the sediment load at the end of the depositional
c         region for each size class.
c
c         Calculations removed from within the "do 200" loop:
          tmpvr2 = xbot + qostar
          tmpvr3 = xtop + qostar
          tmpvr4 = tmpvr2 ** 2
          tmpvr5 = tmpvr3 ** 2
c
          if (abs(pkro).gt.1e-15) then
            phiflg = 1
          else if (qostar.ge.0.0) then
            phiflg = 2
          else
            phiflg = 3
          end if
c
          do 30 i = 1, npart
            tmpvr1 = ktrato * tcf1(i,iplane)
            aa = tmpvr1 * ainftc(kk)
            bb = tmpvr1 * binftc(kk)
            cc = tmpvr1 * cinftc(kk)
            ftheta(i) = fidel(i) * theta
            gu(i) = frcflw(i,iplane) * ldtop
c
            if (phiflg.eq.1) then
              phi = beta * fall(i,iplane) / pkro
              if (phi.gt.100000.) phi = 100000.
              if (phi.lt.-100000.) phi = -100000.
            else if (phiflg.eq.2) then
              phi = 100000.
            else
              phi = -100000.
            end if
c
            ratio = tmpvr3 / tmpvr2
c
c           new section calling undflo  8/2/89
c           SECTION CORRECTED 10/11/89 by dcf
c
            if (qostar.ge.0.0.and.ratio.gt.1.0) ratio = 1.0
            expon = phi
            ratio2 = ratio
            call undflo(ratio2,expon)
c
            coef1 = phi * aa / (phi+2.0)
            coef2 = (phi*bb+ftheta(i)-2.0*aa*phi*qostar) / (1.0+phi)
            term1 = coef1 * tmpvr4
            term2 = coef2 * tmpvr2
            term3 = aa * qostar ** 2 - bb * qostar + cc
c
            term4a = ratio2 ** expon
            if (term4a.lt.1.0e-08) term4a = 0.0
            term4b = gu(i) - coef1 * tmpvr5 - coef2 * tmpvr3 - term3
            gend(i) = term1 + term2 + term3 + term4a * term4b
            if (gend(i).lt.0.0) gend(i) = 0.0
            sumg = sumg + gend(i)
   30     continue
c
c
c         Correction added 6/16/90 by dcf - if enrichment routine
c         calculates that there is no load at all at end of region -
c         must still provide a size distribution for to agree with
c         other part of model calculations that predict a load.
c         (this likely will only happen when transport capacity
c         is zero and there is no interrill detachment on the OFE).
c         In this case - return to ROUTE.F - assuming that particle
c         size exiting the deposition region is the same that
c         entered.
c
c         *** L3 IF ***
          if (sumg.gt.0.0) then
c
c
c           Adjust the individual fraction sediment loads at
c           the end of the section so that they total to the
c           load computed in the deposition routines elsewhere
c           in the model.  Also, compute the maximum possible
c           sediment load in a particle class which is the sum
c           of the sediment entering at the top (gu(i)) plus the
c           interrill contribution (ftheta(i)*(xbot-xtop)).
c
            do 40 i = 1, npart
              gend(i) = gend(i) * ldbot / sumg
              sedmax(i) = gu(i) + ftheta(i) * (xbot-xtop)
              if (gend(i).lt.1.0e-15) gend(i) = 1.0e-15
   40       continue
c
c           Check that the mass of sediment in any particle class does
c           not exceed the total possible mass in that class which is
c           the sum of the sediment entering the segment at the top (gu(i))
c           plus the detached interrill sediment (ftheta(i)*(xbot - xtop))
c           If it does, set the mass in the class to sedmax, and then
c           reproportion the leftover mass to the remaining particle classes.
c
   50       ratbot = 0.0
            sumg = 0.0
            iiflag = 0
            do 60 i = 1, npart
              if (gend(i).gt.sedmax(i)) then
                gend(i) = sedmax(i)
                iiflag = 1
              else if (gend(i).lt.sedmax(i)) then
                ratbot = ratbot + gend(i)
              end if
              sumg = sumg + gend(i)
   60       continue
c
c           Here if at least one of the particle classes has been
c           set with gend(i) = sedmax(i) then the other classes that
c           have not been set gend(i) = sedmax(i) are reproportioned.
c
            if (iiflag.ne.0) then
              do 70 i = 1, npart
                if (gend(i).lt.sedmax(i)) then
                  gadd = (ldbot-sumg) * gend(i) / ratbot
                  gend(i) = gend(i) + gadd
                end if
   70         continue
            end if
c           XXX -- What prevents this from becoming an infinate loop ?
c           CRM -- 2/06/91
            if (iiflag.ne.0) go to 50
c
c           Compute fraction at the end of the depositional region.
c
c           Following code changed 3/23/93 by dcf to prevent divide by zero
c
            if (sumg.gt.0.0) then
              do 80 i = 1, npart
                frcflw(i,iplane) = gend(i) / sumg
   80         continue
            else
              do 90 i = 1, npart
                frcflw(i,iplane) = 0.0
   90         continue
            end if
c
c         *** The L3 ENDIF
          end if
c
c       *** The L2 ELSE
c       ...... If flow ends on the OFE (no outflow) set all fractions to zero,
c       since no sediment is leaving.  (Case 4 hydrologic plane.)
        else
          do 100 i = 1, npart
            frcflw(i,iplane) = 0.0
  100     continue
c
c       *** The L2 ENDIF
        end if
c
c     *** L1 ENDIF ***
      end if
c
      return
      end
