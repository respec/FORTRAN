      subroutine param(effdrr,nowcrp,frara)
c
c******************************************************************
c                                                                 *
c     Called from subroutine CONTIN                               *
c     Finds dimensionless rill and interrill soil erosion         *
c     parameters. One for interrill erosion (theta), two for      *
c     rill erosion (eata and tauc), and one for deposition (phi). *
c     Calls functions trcoef, and falvel.                         *
c     Calls subroutines shears and sheart.                        *
c                                                                 *
c******************************************************************
c
c     + + + PARAMETERS + + +
c
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxgrz.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxslp.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
c
c
c     + + + ARGUMENT DECLARATIONS + + +
      real effdrr(mxplan), frara
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     efflen  - effective OFE length (m)
c     effdrr  - effective duration of rainfall excess (s)
c     nowcrp  - index for current crop on OFE
c     frara   - ask Reza Savabi (winter stuff)
c
c--------------------------------------------------------------------*
c     The following common blocks are used in this subroutine:
c
c
c******************************************************************
c                                                                 *
c   Common Blocks                                                 *
c                                                                 *
c******************************************************************
c
      include 'ccntour.inc'
c
      include 'ccover.inc'
c
      include 'cdist.inc'
c
      include 'cconsta.inc'
c
      include 'ccrpout.inc'
c        read: rrc(mxplan)
      include 'cefflen.inc'
c        read: efflen(mxplan)
      include 'cends.inc'
c
c******************************************************************
c                                                                 *
c ends   variables updated                                        *
c   ktrato, tcend, strldn.                                        *
c                                                                 *
c******************************************************************
c
      include 'cffact.inc'
c
      include 'chydrol.inc'
c
      include 'cinfcof.inc'
c
      include 'cirriga.inc'
c
      include 'cirspri.inc'
c        read: nozzle(mxplan)
c
      include 'crinpt3.inc'
c
      include 'crinpt5.inc'
c
      include 'cpart.inc'
c
      include 'cpart4.inc'
c      modify: fidel(mxplan)
c
      include 'cparval.inc'
c
c******************************************************************
c                                                                 *
c parval variables updated                                        *
c   eata, tauc, theta, phi.                                       *
c                                                                 *
c******************************************************************
c
      include 'cslope.inc'
c
c******************************************************************
c                                                                 *
c slope  variables updated                                        *
c   slpend(mxplan)                                                *
c                                                                 *
c******************************************************************
c
      include 'csolvar.inc'
c
c******************************************************************
c                                                                 *
c solvar variables updated                                        *
c   kt                                                            *
c                                                                 *
c******************************************************************
c
      include 'cstruc.inc'
c
      include 'cwint.inc'
c
c----------------------------------------------------------------------*
      external trcoef, falvel, shears
c
c******************************************************************
c                                                                 *
c   Local Variables                                               *
c     drinti : interrill delivery ratio of each particle size     *
c              class. Computed as function of rif (unitless)      *
c     intdr  : Weighted interrill sediment delivery ratio         *
c              (unitless)                                         *
c     kt2    : transport coefficient calculated using the average *
c              of shrend and shrsol                               *
c     npart  : number of particle types to use in calculating     *
c              effective particle parameters                      *
c     shrend : shear stress calculated using actual slope at end  *
c              of OFE                                             *
c     shrsol : shear stress calculated using average slope of OFE *
c     detinr : interrill detchment rate                           *
c     diaeff : effective sediment diameter                        *
c     fall   : fall velocity of each particle size of each        *
c              OFE (m/s)                                          *
c     spgeff : effective sediment specific gravity                *
c     sumf   : sum of the mass fractions of npart fractions       *
c     veleff : effective particle full velocity                   *
c     beta   : rainfall induced turbulence factor                 *
c     pkro   : slope of the flow discharge line.  For 1-OFE hills *
c              pkro is the flow discharge per unit width          *
c     qi     : average unit discharge of runoff from interrill    *
c              over time of excess rainfall                       *
c     rif    : interrill roughness factor for calculating sediment*
c              delivery.  From Foster, 1981 modeling chapter      *
c                                                                 *
c******************************************************************
      save shrsol, anflst, bnflst, cnflst, atclst, btclst, ctclst
      real kt2, shrend, shrsol, detinr, diaeff, spgeff, sumf, veleff,
     1    beta, ktrprv, tcprev, anflst, bnflst, cnflst, atclst, btclst,
     1    ctclst, sterm1, sterm2, spart1, tpart1, tterm1, tterm2, tprod,
     1    denom, shrspv, qi, rif, intdr, drinti(mxpart), az,bz
      real trcoef,shrati,tcrati,falvel,pkro
      real xxstar,qpoint,shcalc,shrpnt,slppnt
      real qtop,ktop,shrtp1,ktop1,ktop2
      integer i,k,iclass
c
c Compute actual slope gradient at the end of slope (slpend):
c
c      *** CASE OF NO CONTOURS AND POSITIVE OUTFLOW ***
      if (conseq(nowcrp,iplane).eq.0.and.qout.gt.0.0) then
        slpend(iplane) = (a(nslpts(iplane),iplane)*1.0+
     1      b(nslpts(iplane),iplane)) * avgslp(iplane)
c
c     *** CASE OF NO CONTOURS AND NO OUTFLOW FROM OFE ***
      else if (conseq(nowcrp,iplane).eq.0.and.qout.le.0.0) then
c
        slpend(iplane) = b(2,iplane) * avgslp(iplane)
c
c     *** CASE OF CONTOURS ***
      else
        slpend(iplane) = cnslp(iplane)
      end if
c
c     Obtain variables needed for making shear stress and
c     Transport Capacity continuous at OFE breaks
c
      if (iplane.gt.1.and.qout.gt.0.0.and.qin.gt.0.0) then
        qtop = qin * rspace(iplane) 
        call sheart(qtop,slpend(iplane-1),shrtp1)
        if (shrtp1.lt.0.000001) shrtp1 = 0.000001
        call sheart(qtop,cnslp(iplane-1),shrspv)
        if (shrspv.lt.0.000001) shrspv = 0.000001
        ktop1 = trcoef(shrtp1)
        ktop2 = trcoef((shrtp1+shrspv)/2.0)
        ktop = ktop2 / ktop1
        tcprev = ktop1 * shrspv**1.5
        ktrprv = ktop
      endif
c
c     Compute shear stress at the end of slope (shrend) using actual
c     slope gradient:
c
c     shrend = shears(qshear,slpend(iplane))
      call shears(qshear,slpend(iplane),shrend)
      if (shrend.lt.0.000001) shrend = 0.000001
c
c     Compute shear stress at the end of the slope (shrsol) using
c     average slope gradient (avgslp):
c
c     shrsol = shears(qshear,cnslp(iplane))
      call shears(qshear,cnslp(iplane),shrsol)
      if (shrsol.lt.0.000001) shrsol = 0.000001

c
c     Compute the transport coefficient (kt) based on the average slope
c     for the purpose of normalizing:
c
      kt = trcoef(shrsol)
c
c     Compute transport coefficient (kt2) using the average of
c     shrend and shrsol:
c
      kt2 = trcoef((shrend+shrsol)/2.0)
c
c     Compute the normalized transport coefficient (ktrato)
c
      ktrato = kt2 / kt
c     ktrprv = ktrato
c
c     Compute sediment transport capacity (tcend) at end of avg slope:
c
      tcend = kt * shrsol ** 1.5
c
c     Limit tcend so that it is a very small number, but never 0.0 so
c     that model will not bomb for inputs of zero slopes:
c
      if (tcend.lt.1.0e-10) tcend = 1.0e-10
c
c     Compute the starting nondimensional sediment load at the top of
c     the current Overland Flow Element.
c
      if(qin.le.0.0 .or. qout.le.0.0)then
        strldn = qsout * rspace(iplane) / tcend / width(iplane)
      else
        strldn = qsout * rspace(iplane) / tcend / wdhtop(iplane)
      endif
c
c
      if (iplane.gt.1.and.qout.gt.0.0.and.qin.gt.0.0) then
c
c       Check to see that the shear stress at the end of the previous
c       OFE was not zero. If it is zero the solutions for the new shear
c       stress and transport coefficients are not valid and must use
c       original values calculated in XINFLO.
c
        spart1 = anflst + bnflst + cnflst
c
        if (spart1.gt.1.0e-5 .and. shrspv.gt.0.0) then
c
          sterm1 = b(2,iplane) / spart1
          sterm2 = (shrspv/shrsol) ** 1.5
c
          shrati = 1.0 / ((sterm1/sterm2)-1.0)
c
          tpart1 = atclst + btclst + ctclst
c
          if (tpart1.gt.1.0e-5) then
            tterm1 = b(2,iplane) / tpart1
          else
            tterm1 = b(2,iplane) / 1.0e-5
          end if
c
          tterm2 = ((tcend/tcprev)*(ktrato/ktrprv))
          tprod = (tterm1*tterm2) - 1.0
c
          if (abs(tprod).gt.1.0e-5) then
            tcrati = 1.0 / tprod
          else
            if (tprod.ge.0.0) then
              tcrati = 1.0 / 1.0e-5
            else
              tcrati = -1.0 / 1.0e-5
            end if
          end if
c
c
c
c       ELSE  -  have 0 transport capacity at beginning of OFE due to a
c       zero slope condition - if this is the case - best solution is to
c       use QOSTAR in place of shrati and tcrati since it will usually
c       still have a reasonable number - and when you have a zero slope
c       at an OFE boundary the shear and transport will still be
c       continuous even when using QOSTAR.
c
c
        else
          shrati = qostar
          tcrati = qostar
        end if
c
c
c       Re-calculate the shear stress and transport coefficients using
c       the just calculated shear stress and transport ratios.
c
        do 10 i = 2, nslpts(iplane)
c
c
          denom = (shrati+1.0)
c
c         prevent the denominator term for the coefficients from
c         becoming zero, which will cause model to bomb in
c         following computations
c
          if (abs(denom).lt.1.0e-3) then
            if (denom.ge.0.0) then
              denom = 0.001
            else
              denom = -0.001
            end if
          end if
c
          ainf(i) = a(i,iplane) / denom
          binf(i) = (a(i,iplane)*shrati+b(i,iplane)) / denom
          cinf(i) = (b(i,iplane)*shrati/denom)
c
          denom = (tcrati+1.0)
c
c         prevent the denominator term for the coefficients from
c         becoming zero, which will cause model to bomb in
c         following computations
c
          if (abs(denom).lt.1.0e-3) then
            if (denom.ge.0.0) then
              denom = 0.001
            else
              denom = -0.001
            end if
          end if
c
          ainftc(i) = a(i,iplane) / denom
          binftc(i) = (a(i,iplane)*tcrati+b(i,iplane)) / denom
          cinftc(i) = (b(i,iplane)*tcrati/denom)
c
          anflst = ainf(i)
          bnflst = binf(i)
          cnflst = cinf(i)
c
          atclst = ainftc(i)
          btclst = binftc(i)
          ctclst = cinftc(i)
c
   10   continue
c
      end if
c
      if (qin.le.0.0) then
        do 20 i = 2, nslpts(iplane)
          anflst = ainf(i)
          bnflst = binf(i)
          cnflst = cinf(i)
c
          atclst = ainf(i)
          btclst = binf(i)
          ctclst = cinf(i)
   20   continue
      end if
c
c     Compute dimensionless parameters for rill erosion (eata and tauc):
c
c     *** SOIL IS FROZEN TO THE SURFACE ***
      if (frdp(iplane).gt.0.0.and.thdp(iplane).le.0.0) then
        eata = 0.0
c
c     *** SURFACE SOIL IS UNFROZEN ***
      else
c
c XXX   Changed equation for rill parameter EATA so that KT parameter
c       is not used here, (ONLY tcend is used).  dcf  11/21/94
c       eata = cntlen(iplane) * kr(iplane) * kradjf(iplane) / ((
c    1      sqrt(shrsol))*kt)
        eata = cntlen(iplane) * kr(iplane) * kradjf(iplane) * shrsol
     1         / tcend
      end if
c
      tauc = tcadjf(iplane) * shcrit(iplane) / shrsol
c
      if(lanuse(iplane).eq.1)then
c       For CROPLAND situations only,
c       compute the interrill delivery ratio as function of
c       random roughness and particle size distribution
c
        rif = -23.0 * rrc(iplane) +1.14
c
        if (rif.lt.0.0) rif = 0.0
        if (rif.gt.1.0) rif = 1.0
c
        intdr = 0.0
        do 30 iclass = 1,npart
          if (fall(iclass,iplane).lt.0.01) then
            bz = 0.1286+2209.0*fall(iclass,iplane)
            az = exp(0.0672+659.0*fall(iclass,iplane))
            drinti(iclass)=az*rif**bz
          else
            drinti(iclass)=2.5*rif-1.5
          endif
          if (drinti(iclass).gt.1.0) drinti(iclass) = 1.0
          if (drinti(iclass).lt.0.0) drinti(iclass) = 0.0
          intdr = intdr + frac(iclass,iplane) * drinti(iclass)
   30   continue
c
        do 40 iclass = 1, npart
c
c XXX   Added check to prevent a divide by zero if no delivery of
c       interrill sediment.  This problem has been enhanced due
c       to the new equation added above to compute RIF (Now, for
c       an "rrc" value of 0.05 meters (2 inches) or greater, the
c       value of RIF is always zero - causing a bomb here).
c       dcf  11/18/94
c
          if(intdr.gt.0.0)then
            fidel(iclass) = frac(iclass,iplane) * drinti(iclass) / intdr
          else
            fidel(iclass) = 0.0
          endif
   40   continue
c
      else
c       NON-CROPLAND situation - do not do interrill sediment
c       delivery calculations.   dcf  12/2/96
        intdr=1.0
        do 45 iclass=1,npart
           fidel(iclass)=frac(iclass,iplane)
   45   continue
      endif
c
c     Compute dimensionless interrill erosion parameter (theta):
c
      if (effdrr(iplane).gt.0.0) then
        qi = runoff(iplane) / effdrr(iplane)
      else
        qi = 0.0
      end if
c
c
c     Code to incorporate a nozzle impact energy factor added below.
c     On OFE's which have a positive value for sprinkler irrigation
c     depth, we use the input value for nozzle impact energy for
c     that OFE.  When there is no irrigation water applied, we use
c     the value of 1.0 for nozzle impact energy factor (natural
c     rainfall).     dcf  3/8/94
c
      if (width(iplane).gt.0.0) then
c
c       NO IRRIGATION DEPTH TODAY
c
        if(irdept(iplane).le.0.0)then
          detinr = ki(iplane) * kiadjf(iplane) * effint(iplane) * qi
     1             * intdr * rspace(iplane) / width(iplane)
c
c       ELSEIF SPRINKLER IRRIGATION (irsyst = 1)
c
        elseif(irsyst.eq.1)then
c
c         Ratio of rain to irrigation depth is less than 1.0 - use
c         the input value of nozzle impact energy
c
          if( (rain(iplane) /irdept(iplane)) .lt. 1.0)then
            detinr = ki(iplane) * kiadjf(iplane) * effint(iplane) * qi
     1               * nozzle(iplane)
     1               * intdr * rspace(iplane) / width(iplane)
c
c         Ratio of rain to irrigation depth is 1.0 or greater - use
c         the assumed impact energy factor for natural rainfall
c         (nozzle = 1.0)
c
          else
            detinr = ki(iplane) * kiadjf(iplane) * effint(iplane) * qi
     1               * intdr * rspace(iplane) / width(iplane)
          endif
c
c       ELSE you have furrow irrigation - which should not have any
c       interrill detachment - set detinr to 0.0
c
        else
          detinr = 0.0
        endif
c
c     ELSE the rill width is zero (which is impossible on a day with
c     runoff) so set the value for detinr to zero.
c
      else
        detinr = 0.0
      end if
c
c     theta set to zero if the plane is covered by snow or the surface
c     layer is frozen or there is no rainfall or sprinkler irrigation
c
c
creza 4/19/94      if (snodpy(iplane).gt.0.0.or.(frdp(iplane).gt.0.0.and.
c     1    thdp(iplane).le.0.0).or.(rain.le.0..and.wmelt(iplane).gt.0.
c     1    .and.irdept(iplane).le.0.).or.(rain.le.0..and.irdept(iplane)
c     1    .gt.0..and.irsyst.eq.2)) then
creza 4/19
c        frozl=frdp(iplane)-thdp(iplane)
      if (snodpy(iplane).gt.0.0 .or.
     1      (frara.le.0.8 .and. wmelt(iplane).gt.0.
     1       .and. irdept(iplane).le.0.) .or.
     1      (rain(iplane).le.0. .and. irdept(iplane).gt.0.0 .and.
     1       irsyst.eq.2)) then
        theta = 0.0
      else
c
c       Time correction for interrill detachment (8/15/90)  dcf
c
        theta = cntlen(iplane) * detinr / tcend
        theta = theta * (effdrr(iplane)/effdrn(iplane))
c
c       For OFE's with no rainfall excess, set interrill detachment
c       parameter to 0.0
c
        if (qout.le.qin) theta = 0.0
      end if
c
c     Compute the effective particle diameter (diaeff), effective
c     specific gravity (spgeff), and effective particle fall
c     velocity (veleff) using three size classes (primary clay,
c     silt and sand):
c
      diaeff = 0.0
      spgeff = 0.0
      sumf = 0.0
c
c
c     NEEDS TO BE SOME CORRECTIONS MADE HERE - TO COMPUTE THE EFFECTIVE
c     DIAMETER, SPECIFIC GRAVITY, and FALL VELOCITY.  NEED TO ADDRESS
c     WHEN THERE ARE LARGE AMOUNTS OF LARGE AGGREGATES OR SAND PRESENT.
c     Currently using only smallest 3 size classes (clay, silt, s.agg.)
c
c     dcf 5/20/91
c
      do 50 k = 1, 3
        diaeff = diaeff + frac(k,iplane) * alog(dia(k,iplane))
        spgeff = spgeff + frac(k,iplane) * alog(spg(k))
        sumf = sumf + frac(k,iplane)
   50 continue
c
      if (sumf.gt.0.0) then
        diaeff = exp(diaeff/sumf)
        spgeff = exp(spgeff/sumf)
        veleff = falvel(spgeff,diaeff)
c
c       Compute dimensionless deposition parameter (phi):
c       Note: the turbulence factor beta is set to 0.5 for the moment
c       if there is rainfall. set to zero if no rainfall
c       phi=beta*veleff/peakro(iplane)
c
        if (rain(iplane).le.0.0.and.irdept(iplane).le.0.00001) then
          beta = 1.0
        else
          beta = 0.5
        end if
      end if
c
c     Compute the slope of the flow discharge line (PKRO)
c
      pkro = -1.0e-10
      if (qout.gt.0.0) then
        pkro = (qout-qin) / slplen(iplane)
      else
        if (qin.gt.0.0) then
          if (efflen(iplane).gt.1.e-10) then
            pkro = -qin / efflen(iplane)
          else
            pkro = -1.0e-10
          end if
        end if
      end if
c
      if (abs(pkro).ge.1.0e-15) then
        phi = beta * veleff / pkro
      else
c
c       Set maximum value of deposition parameter to +-100000
c       if slope of flow discharge line is almost 0.0
c
        if (qostar.ge.0.0) then
          phi = 100000.
        else
          phi = -100000.
        end if
      end if
c
c     Limit value of deposition parameter to absolute value of 100000
c
      if (phi.gt.100000.0) phi = 100000.0
      if (phi.lt.-100000.0) phi = -100000.0
c
      return
      end
