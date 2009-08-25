      subroutine chnrt(nptsc,sdate,ichplt,latvol)
c
c     + + + PURPOSE + + +
c
c     SR CHNRT routes sediment through channel elements.
c
c     Called from: SR CHNERO
c     Author(s): Ascough II, R. van der Zweep, V. Lopes, C. Baffaut
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcsg.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer nptsc, sdate, ichplt
      real latvol
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     nptsc  -
c     sdate  -
c     ichplt -
c     latvol - lateral inflow including channel runoff
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcas.inc'
      include 'cchcon.inc'
      include 'cchero.inc'
      include 'cchflo.inc'
      include 'cchpar.inc'
      include 'cchpek.inc'
      include 'cchprt.inc'
      include 'cchsed.inc'
      include 'cchtrl.inc'
      include 'cchvar.inc'
      include 'cenrpa1.inc'
      include 'cgully.inc'
      include 'chydrol.inc'
      include 'coutchn.inc'
      include 'cpart.inc'
      include 'cstore.inc'
      include 'cstruct.inc'
      include 'cstruc.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real lowerx, net, maxe, leff, uarea, tarea, y(mxcseg), cc3,
     1    checku, covsh, delx, detflo, dtot, eata, effshu,
     1    endman, excess, gt, gtot, qu, sfu, soloss, ssfb, ssfe, stot,
     1    topl, wfa, wleft, wright, xbeg, xchn, qtemp, carea, gpart
c
      integer flag1, flagct, i, ibeg, iseg, k, newp, nk, nt
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     lowerx    -
c     net       -
c     maxe      -
c     leff      -
c     uarea     -
c     tarea     -
c     y(mxcseg) -
c     cc3       -
c     checku    -
c     covsh     -
c     delx      -
c     detflo    -
c     dtot      -
c     eata      -
c     effshu    -
c     endman    -
c     excess    -
c     gt        -
c     gtot      -
c     qu        -
c     sfu       -
c     soloss    -
c     ssfb      -
c     ssfe      -
c     stot      -
c     topl      -
c     wfa       -
c     wleft     -
c     wright    -
c     xbeg      -
c     xchn      -
c     qtemp     -
c     carea     -
c     gpart     -
c
c     Integer Variables
c
c     flag1  -
c     flagct -
c     i      -
c     ibeg   -
c     iseg   -
c     k      -
c     newp   -
c     nk     -
c     nt     -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     case12
c     case34
c     dcap
c     frichn
c     fslpar
c     hydchn
c     trncap
c
c     + + + DATA INITIALIZATIONS + + +
c
      data covsh /1000.0/
c
c     + + + END SPECIFICATIONS + + +
c
c
c     override ichplt so that plotting sections of code not
c     entered
c
      ichplt = 0
c
c     give a initial value to flag1 so that it is set even if
c     SR FSLPAR is not called (flag1 set to 5 in SR FSLPAR)
c
      flag1 = 5
c
c     convert WEPP variable names into CREAMS variable names and
c     convert variable units from METRIC to ENGLISH
c
c     qe is the peak runoff rate exiting the channel element
c     (m^3/s --> ft^3/sec)
c
      qe = peakot(ielmt) * 35.31984
c
c     topl is the difference between the actual channel length and
c     the effective channel length (m --> ft)
c
c     chnlef is the effective channel length (m)
c     chnlen(ichan) is the actual channel length (m)
c
      topl = (chnlef-chnlen(ichan)) * 3.281
c
c     leff is the effective channel length (m --> ft)
c
      leff = chnlef * 3.281
c
c     uarea is the upper area draining into the channel
c     element (m^2 --> ft^2)
c
      uarea = uparea(ichan) * 10.76496
c
c     carea is the channel area (m^2 --> ft^2)
c
      carea = charea(ichan) * 10.76496
c
c     tarea is the total area draining into the channel
c     element (m^2 --> ft^2)
c
      tarea = toarea(ichan) * 10.76496
c
c     endman and chn are total manning's n
c     nbarch is bare soil manning's n
c
      endman = chnn(ichan)
      chn = chnn(ichan)
c
      nbarch = chnnbr(ichan)
c
      eata = 1.0
      tb = 2.0 * rundur(ielmt)
c
      crsh = chntcr(ichan) * .021
      depsid = chneds(ichan) * 3.281
      chz = chnz(ichan)
c
      ssfb = slope(1)
      sfe = ctlslp(ichan)
c
      flagct = ishape(ichan)
      cflags = ienslp(ichan)
c
c     determine the discharge rate at the top of the channel element
c     and the lateral inflow rate
c
c      if (tmpvol(nileft(ielmt))+tmpvol(nirght(ielmt))+
c     1    tmpvol(nhleft(ielmt))+tmpvol(nhrght(ielmt)).gt.0.0) then
       if (latvol.gt.0.0) then
c
c       lateral inflow from overland flow areas (runoff Case
c       I - top inflow and lateral inflow or runoff Case
c       II - no top flow but lateral inflow)
c
c       qu is the discharge at the top of the channel
c       element (ft^3/sec)
c
c       qlat is the effective lateral inflow (ft^3/sec/ft)
c
        qu = qe * topl / leff
        qlat = qe / leff
c
      else
c
c       no effective lateral inflow from overland flow areas or
c       impoundments (Case III runoff)
c
        qu = qe
        qlat = 0.0
c
      end if
c
      qb = qu
c
c     compute flow depth and friction slope at the end of channel
c     and at the outlet control
c
      if (runvol(ielmt).ge.0.001) call fslpar(ichan,chnz(ichan),endman,
     1    leff,ibeg,flag1,cc1,cc3,slope(nptsc),ssfe)
c
c     if channel has no lateral inflow and no outlet control set
c     friction slope equal to bed slope
c
      if (qlat.le.0.0) cflags = 2
c
c     if qu > 0.0 get starting position for erosion calculations and
c     compute friction slope at the top of current channel element
c
      if (qu.gt.0.001) then
        xbeg = topl / leff
        if (cflags.eq.1) call frichn(qu,chn,chnz(ichan),cc1,cc3,1,flag1,
     1      slope(1),xbeg,leff,ye,xbeg,ibeg,ssfb,ssfe,sfu)
        if (cflags.eq.2) sfu = slope(1)
        if (sfu.le.0.6e-4) sfu = 0.6e-4
c
      else
c
c       initialize variables for top segment of current channel element
c
        wfu = 0.0
        flagc = ishape(ichan)
c
c       if rectangular channel then set flow width equal to bottom width
c       of channel
c
        if (flagc.eq.2) wfu = widb(ichan,1)
c
        effshu = 0.0
        sfu = slope(1)
c
        do 10 k = 1, cnpart
          du(k) = 0.0
          tcu(k) = 0.0
          gsu(k) = 0.0
          gstu(k) = 0.0
   10   continue
c
      end if
c
c     compute a weighted average of the sediment inflow
c
c     sedcon is the sediment concentration (kg/m^3)
c     peakot is the peak runoff rate (m^3/s)
c
      gtot = 0.0
c
      do 20 k = 1, cnpart
c
c       gtot is the total sediment load entering the top channel
c       segment (lbs)
c
c       gpart is sediment load entering the top channel segment
c       for each particle size
c
        gpart = ((tmpvol(nitop(ielmt))*sedcon(k,nitop(ielmt)))+(
     1      tmpvol(nhtop(ielmt))*sedcon(k,nhtop(ielmt)))+(
     1      tmpvol(ncleft(ielmt))*sedcon(k,ncleft(ielmt)))+(
     1      tmpvol(ncrght(ielmt))*sedcon(k,ncrght(ielmt)))+(
     1      tmpvol(nctop(ielmt))*sedcon(k,nctop(ielmt)))) * 2.2064
c
        gtot = gtot + gpart
c
c       gstu is the sediment flux entering the top channel
c       segment (lbs/sec)
c
        gstu(k) = gpart / rundur(ielmt)
c
        if (rvolat(ielmt).gt.0.0) then
c
c         lateral inflow occurs - determine weighting factor for
c         each lateral area
c
          wleft = (tmpvol(nileft(ielmt))+tmpvol(nhleft(ielmt))) /
     1        rvolat(ielmt)
          wright = (tmpvol(nirght(ielmt))+tmpvol(nhrght(ielmt))) /
     1        rvolat(ielmt)
c
c         dlat is the  sediment flux per foot of channel length entering
c         the channel laterally, weighted as the average of the lateral
c         areas based upon the runoff volume (lbs/sec/ft)
c
          dlat(k) = ((wleft*qlat*sedcon(k,nileft(ielmt)))+(wright*qlat*
     1        sedcon(k,nirght(ielmt)))+(wleft*qlat*
     1        sedcon(k,nhleft(ielmt)))+(wright*qlat*
     1        sedcon(k,nhrght(ielmt)))) * 0.06241818
        else
c
c         if no lateral inflow then lateral sediment load is zero
c
          dlat(k) = 0.0
        end if
c
   20 continue
c
c     compute total sediment (t/a) entering channel element
c     and set channel contribution equal to zero (lbs/ft)
c
      if (uarea.gt.0.0) then
c        nsedm(1) = nsedm(1) + gtot / uarea * 19.75
c        nsedy(1) = nsedy(1) + gtot / uarea * 19.75
c        nsedt(1) = nsedt(1) + gtot / uarea * 19.75
        nsedm(1) = nsedm(1) + gtot / uarea * 21.78
        nsedy(1) = nsedy(1) + gtot / uarea * 21.78
        nsedt(1) = nsedt(1) + gtot / uarea * 21.78
c
      else
        nsedm(1) = 0.0
        nsedy(1) = 0.0
        nsedt(1) = 0.0
      end if
c
      csedm(1) = 0.0
      csedy(1) = 0.0
      csedt(1) = 0.0
c
c     segment by segment plotting file (event mode only)
c
      if ((imodel.eq.2).and.(ichplt.eq.1)) then
c
        write (37,1300) sdate
        iseg = nptsc
        y(iseg) = 0.0
c
        do 30 i = 1, nptsc - 1
          delx = x(iseg) - x(iseg-1)
          y(iseg-1) = y(iseg) + delx * slope(iseg)
          iseg = iseg - 1
   30   continue
c
        if (uarea.gt.0.0) then
          gt = gtot / uarea * 21.78
        else
          gt = 0.0
        end if
c
c       temporarily set xchn and dtot to zero to write to file
c
        xchn = 0.0
        dtot = 0.0
c
        write (37,1400) xchn, y(1), gt, dtot
c
      end if
c
c     start computational loop for current channel element
c
      newp = 1
c
      do 150 i = 2, nptsc
cd    Added by S. Dun, 04/10/2008 to avoid crash
        sfl = slope(i)
cd    end adding
c
        if (i.gt.2) then
c
c         initialize variables at the upper boundary
c
          newp = 0
          sfu = sfl
          qu = ql
          wfu = wfl
          effshu = effshl
c
          do 40 k = 1, cnpart
            du(k) = dl(k)
            tcu(k) = tcl(k)
            gsu(k) = gsl(k)
            gstu(k) = gstl(k)
   40     continue
c
        end if
c
c       x is the distance from the top of the effective channel (ft)
c
c       xbeg is the relative distance from the top of the
c       effective channel (ft)
c
c       lowerx is the distance from the top of the actual channel (ft)
c
        xbeg = x(i) / leff
        lowerx = x(i)
c
        if (qlat.gt.0.0) then
c
c         ql is the discharge rate at the lower end of the current
c         segment (ft^3/sec)
c
          ql = qe * x(i) / leff
c
        else
          ql = qb
        end if
c
        dx = x(i) - x(i-1)
c
c       get starting position and friction slope at the lower end
c       of current channel segment
c
        if (cflags.eq.1.and.i.lt.nptsc) call frichn(ql,chn,chnz(ichan),
     1      cc1,cc3,i,flag1,slope(i),xbeg,leff,ye,xbeg,ibeg,ssfb,ssfe,
     1      sfl)
c
        if (cflags.eq.2.and.i.lt.nptsc) sfl = slope(i)
        if (cflags.eq.2.and.i.eq.nptsc) sfl = sfe
        if (sfl.le.0.6e-4) sfl = 0.6e-4
        if (sfu.le.0.6e-4) sfu = 0.6e-4
c        write(6,*) i,'sfu = ',sfu,'; sfl = ',sfl
c
        if (.not.((newp.eq.0).or.(qu.le.0.0))) then
c
          flagc = ishape(ichan)
c
c         if erodible channel (flagc = 3) and depb (updated depth
c         of erodible layer at the lower boundary of the lower end
c         of previous segment) is zero, the x-section at the upper
c         end of current segment is updated to a rectangular shape
c         (flagc = 2) for hydraulic calculations
c
          if (flagc.eq.3.and.depb(ichan,i-1).le.1e-4) flagc = 2
c
c         call hydchn to calculate hydraulic parameters at the upper end
c         of current segment
c
          call hydchn(flagc,qu,sfu,cc1,chnz(ichan),widb(ichan,i-1),wfu,
     1        chn,crsh,covsh,effshu)
c
c         compute sediment load per unit width at the upper end of
c         segment
c
          do 50 k = 1, cnpart
            gsu(k) = gstu(k) / wfu
   50     continue
c
        end if
c
        flagc = ishape(ichan)
c
c       if erodible channel (flagc = 3) and depa (updated depth of
c       erodible layer at the upper boundary of lower end of previous
c       segment) is zero, the x-section at the lower end of current
c       segment is updated to a rectangular shape (flagc = 2) for
c       hydraulic calculations
c
        if (flagc.eq.3.and.depa(ichan,i).le.1e-4) flagc = 2
c
c       call hydchn to calculate hydraulic parameters at the lower end
c       of current segment
c
        call hydchn(flagc,ql,sfl,cc1,chnz(ichan),wida(ichan,i),wfl,chn,
     1      crsh,covsh,effshl)
c
c       compute flow width in channel (wfa) as the average of the
c       upper and lower widths
c
        wfa = (wfl+wfu) / 2.0
c
c       compute deposition parameter (phi) using wfa
c
c       if qlat is zero then compute a temporary qlat to use
c       in the phi equation
c
        if (qlat.le.0.0) qtemp = ((carea/(tarea-carea))*qe) / leff
        if (qlat.gt.0.0) qtemp = qlat
c
        do 60 k = 1, cnpart
          phi(k) = eata * crfall(k,ielmt) * wfa / qtemp
   60   continue
c
        if (.not.((newp.eq.0).or.(qu.le.0.00001))) then
c
c         compute transport capacity at the upper end of segment
c
          call trncap(effshu,gsu,ielmt,tcu)
c
c         assume there is a potential for detachment at the
c         upper end of segment (set excess = 1.0)
c
          excess = 1.0
c
          do 70 k = 1, cnpart
c
c           if the transport capacity of a particle size
c           is exceeded then compute deposition
c
            if (tcu(k).le.1e-8) then
              excess = 0.0
              go to 80
            end if
c
            excess = amin1(excess,(1.0-gsu(k)/tcu(k)))
   70     continue
c
c         detachment occurs when there is an excess in the
c         transport capacity of the flow
c
   80     if (excess.gt.0.) then
c
c           compute detachment rate at the upper end of segment. first,
c           set maximum allowable erosion rate without overfilling
c           then transport capacity (maxe)
c
            maxe = 1000.0
c
c           reset flagc
c
            flagc = ishape(ichan)
c
c           if erodible channel (flagc = 3) and depb (updated depth of
c           erodible layer at the lower boundary of lower end of previous
c           segment is zero, compute detachment and erode (widening)
c           channel cross section (assuming a rectangular shape)
c
            if (flagc.eq.3.and.depb(ichan,i-1).le.1e-4) flagc = 2
c
cd          Added by S. Dun, 04/10/2008 to avoid crash
            if (sfu.lt.0.00001) sfu = 0.00001
            
cd          end adding 
            call dcap(1,flagc,qu,sfu,cc1,chnz(ichan),effshu,depsid,
     1          depb(ichan,i-1),werb(ichan,i-1),wfu,chn,crsh,covsh,maxe,
     1          excess,tb,ielmt,df,ichan)
c
c           if channel is rectangular and of "frictious" width (computed
c           as a function of shear stress) at the lower boundary of
c           lower end of previous segment is greater than flow width at
c           the upper end of current segment, then make bottom of
c           rectangular channel (widb) equal to "eroded" bottom (werb)
c
            if (flagc.eq.2.and.werb(ichan,i-1).gt.wfu) widb(ichan,i-1) =
     1          werb(ichan,i-1)
c
c           compute total detachment capacity (du)
c
            do 90 k = 1, cnpart
              du(k) = df(k) * wfu
   90       continue
c
          else
c
c           compute deposition rate at the upper end of segment
c
            do 100 k = 1, cnpart
c              du(k) = (phi(k)/qu) * (tcu(k)*wfu-gstu(k))
              du(k) = crfall(k,ielmt)*wfu/qu * (tcu(k)*wfu-gstu(k))
              if (du(k).gt.0.0) du(k) = 0.0
  100       continue
c
          end if
c
c         reset excess
c
          excess = 1.0
        end if
c
c       compute "initial" potential load based on the total load from
c       upslope segment (gstu) and addition from lateral inflow (dlat)
c
        do 110 k = 1, cnpart
          potld(k) = (gstu(k)+dlat(k)*dx) / wfl
  110   continue
c        write(6,'(a11,5e12.4)') 'pot. load: ',(potld(k), k=1,cnpart)
c
c       compute transport capacity based on "initial" potential load
c
        call trncap(effshl,potld,ielmt,tcl)
c        write(6,'(a11,5e12.4)') 'ini. cap.: ',(tcl(k), k=1,cnpart)
c        write(6,*) crsh, covsh, effshu, effshl
c
c       initialize flags nt and nk
c
        nt = 0
        nk = 0
c
c       check for the possibility of detachment occurring at the
c       upper end of segment
c
        do 120 k = 1, cnpart
c
          dtcdx(k) = (tcl(k)*wfl-tcu(k)*wfu) / dx
c
c         check for the special case when there is no top
c         inflow, (ie. qu = 0.0), and the rate of lateral sediment
c         inflow, dlat, exceeds the transport capacity of the
c         channel segment, dtc/dx
c
c         if ((i.eq.2).and.(qu.eq.0.0).and.(dtcdx(k).lt.dlat(k)))
c
          if ((i.eq.2).and.(qu.lt.0.001).and.(dtcdx(k).lt.dlat(k)))
     1        du(k) = phi(k) / (1+phi(k)) * (dtcdx(k)-dlat(k))
c     1        du(k) = (phi(k)/(qlat+phi(k))*(dtcdx(k)-dlat(k)))
c
          checku = tcu(k) * wfu
c
          if (checku.gt.gstu(k)) nk = nk + 1
          if (du(k).gt.0.0) nt = nt + 1
  120   continue
c
        flagct = ishape(ichan)
c
c       if all tci's > gsi's (nk = cnpart) or all dui's > 0.0
c       (nt = cnpart) there is detachment at the upper end of
c       channel segment
c
c       four erosion cases are possible:
c
        if (nk.eq.cnpart.or.nt.eq.cnpart) then
c
c         Case III : du > 0.0 (detachment) and dl < 0.0 (deposition)
c         Case IV  : du > 0.0 (detachment) and dl > 0.0 (detachment)
c
          call case34(ichan,ielmt,cnpart,i,flagct)
c
        else
c
c         Case I : du < 0.0 (deposition) and dl < 0.0 (deposition)
c         Case II: du < 0.0 (deposition) and dl > 0.0 (detachment)
c
          call case12(ichan,ielmt,cnpart,i,flagct)
c
        end if
c
        do 130 k = 1, cnpart
          gstl(k) = gsl(k) * wfl
  130   continue
c
        if (flgout(ichan).eq.6) write (38,1000) lowerx - topl
c
        gtot = 0.0
        stot = 0.0
        dtot = 0.0
c
        do 140 k = 1, npart
          gtot = gtot + gstl(k) * rundur(ielmt)
          net = (gstl(k)-gstu(k)) * rundur(ielmt) / dx
          stot = stot + net
          detflo = (((gstl(k)-gstu(k))/dx)-dlat(k)) * rundur(ielmt)
          dtot = dtot + detflo
          if (flgout(ichan).eq.6) write (38,1100) k, net, detflo
  140   continue
c
c       nsedm(i) is the total sediment yield (t/a) passing
c       through channel segment i
c
c       csedm(i) is the weight of soil either eroded or deposited per
c       unit length from the i-th channel segment (lb/ft)
c
        nsedm(i) = nsedm(i) + gtot / tarea * 21.78
        nsedy(i) = nsedy(i) + gtot / tarea * 21.78
        nsedt(i) = nsedt(i) + gtot / tarea * 21.78
c
        csedm(i) = csedm(i) + dtot
        csedy(i) = csedy(i) + dtot
        csedt(i) = csedt(i) + dtot
c
c       sum soil loss and deposition totals for channel
c
        if (dtot.gt.0.0) then
          cdetm(ielmt) = cdetm(ielmt) + dtot * dx
          cdety(ielmt) = cdety(ielmt) + dtot * dx
          cdett(ielmt) = cdett(ielmt) + dtot * dx
        else if (dtot.lt.0.0) then
          cdepm(ielmt) = cdepm(ielmt) + dtot * dx
          cdepy(ielmt) = cdepy(ielmt) + dtot * dx
          cdept(ielmt) = cdept(ielmt) + dtot * dx
        end if
c
        if (flgout(ichan).eq.6) write (38,1200) stot, dtot
c
        if ((imodel.eq.2).and.(ichplt.eq.1)) then
c
c         write out channel profile plotting data
c
          xchn = x(i) - topl
          write (37,1400) xchn, y(i), gtot / tarea * 21.78, -dtot
        end if
c
c        write(6,'(5e12.4)') (gsl(k), k=1,npart)
c        write(6,*)
c
  150 continue
c
      soloss = 0.0
c
      do 160 k = 1, cnpart
c
c       compute sediment concentration (lb/ft^3) of particle class
c
        conc(k,ielmt) = gstl(k) / qe
c
c       compute sediment load (lbs) of particle class
c
        ggs(k,ielmt) = runvol(ielmt) * 35.319837 * conc(k,ielmt)
c
c       sum up total sediment yield (lb/ft^2)
c
        soloss = soloss + ggs(k,ielmt) / tarea
c
c       convert sediment concentration from lb/ft^3 to kg/m^3
c
        sedcon(k,ielmt) = conc(k,ielmt) * 16.020973
c
        if (soloss.ge.0.0) then
          tgst(k,ielmt) = tgst(k,ielmt) + ggs(k,ielmt)
          tgsy(k,ielmt) = tgsy(k,ielmt) + ggs(k,ielmt)
          tgsm(k,ielmt) = tgsm(k,ielmt) + ggs(k,ielmt)
          tgsd(k,ielmt) = ggs(k,ielmt)
        else
          tgsd(k,ielmt) = 0.0
        end if
c
  160 continue
c
      do 170 k = 1, cnpart
c
c       compute the fraction in each particle class
c
        if (soloss.gt.0.0) then
          frcflw(k,ielmt) = ggs(k,ielmt) / (soloss*tarea)
        else
          frcflw(k,ielmt) = 0.0
        end if
c
  170 continue
c
      return
 1000 format (' ',//,12x,'soil loss for the segment ',f6.1,' ft. from ',
     1    'the channel top',/,19x,
     1    'particle     net soil loss    chan soil loss',/,19x,
     1    '  type         (lbs/ft of channel segment)'/,19x,
     1    '  ----          -------           -------')
 1100 format (21x,i2,9x,f9.2,9x,f9.2)
 1200 format (/,21x,'total',7x,f9.2,9x,f9.2)
 1300 format (//,10x,'total soil loss for day',i3,//10x,
     1    '     x       y        sed yld      chan',/,10x,
     1    '   (ft)    (ft)       (t/ac)     (lbs/ft ch)'/,10x,
     1    '   ----    ----       -------     -------')
 1400 format (12x,f5.1,3x,f5.1,6x,f9.2,3x,f9.2)
      end
