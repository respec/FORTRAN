      subroutine bigout(bigcrp,iiyear)
c
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxres.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pxstep.inc'
c
      include 'cavepar.inc'
      include 'cavloss.inc'
      include 'cclim.inc'
      include 'ccons.inc'
      include 'ccover.inc'
      include 'ccrpout.inc'
      include 'ccrpprm.inc'
      include 'ccrpvr1.inc'
      include 'ccrpvr2.inc'
      include 'ccdrain.inc'
c
      include 'cefflen.inc'
c     read: efflen(mxplan)
c
      include 'cends4.inc'
      include 'cenrpa2.inc'
      include 'cffact.inc'

      include 'cflags.inc'
c     read: bigflg
c
      include 'chydrol.inc'
      include 'cirriga.inc'
      include 'cirfurr.inc'
      include 'cparame.inc'
      include 'cparva2.inc'
      include 'cseddet.inc'
      include 'cslpopt.inc'
      include 'csolva2.inc'
      include 'cstruc.inc'
      include 'cupdate.inc'
      include 'cwater.inc'
      include 'cwint.inc'
      include 'ccliyr.inc'
c
c
c     parameters
c
c
c     local values
c
c
c     static local (hence the `save')
c
      integer tint(8), bigcrp, iiyear, intmin(8), intmax(8), i, kk
      real treal(84), ralmax(84), ralmin(84), watcon
      save intmin, intmax, ralmin, ralmax
c
      data ralmin /84 * 1e6/
      data ralmax /84 * -1e6/
      data intmin /8 * 1e4/
      data intmax /8 * -1e4/
c
c
c     Daily: UPDATE CURRENT MINIMUMS AND MAXIMUMS AND WRITE
c
      if (bigflg.eq.0) then
c
c       very first time in
c
        if (sdate.eq.1.and.iiyear.eq.1) then
          intmin(1) = 1
          intmax(1) = 1
c
c       increment the date.
c
        else if (iplane.eq.1) then
          intmax(1) = intmax(1) + 1
        end if
c
c
c       determine total soil water content for all the OFEs this date
c
        watcon = 0.0
        do 10 i = 1, nsl(iplane)
          watcon = watcon + soilw(i,iplane)
   10   continue
c
c
c       determine the current minimums and maximums.
c       values which are constant for the hillslope come first.
c
        tint(1) = intmax(1)
c       tint(2) = itype(nowcrp,iplane)
        tint(2) = bigcrp
        do 20 kk = 1, 3
          tint(2+kk) = iresd(kk,iplane)
          tint(5+kk) = iroot(kk,iplane)
   20   continue
c
        treal(1) = prcp * 1000
        treal(2) = avedet
        treal(3) = maxdet
        treal(4) = ptdet
        treal(5) = avedep * (-1.0)
        treal(6) = maxdep * (-1.0)
        treal(7) = ptdep
        treal(8) = avsole
        treal(9) = tmnavg
        treal(10) = tmxavg
        treal(11) = tmin
        treal(12) = tmax
c
        if (irabrv.ne.0) then
          treal(13) = irdept(iplane) * 1000.0
          treal(14) = irapld(iplane) * 1000.0
        else
          treal(13) = 0.0
          treal(14) = 0.0
        end if
c
        treal(15) = (runoff(iplane)*efflen(iplane)/totlen(iplane)) *
     1      1000.0
        treal(16) = irdgdx(iplane)
        treal(17) = canhgt(iplane)
        treal(18) = cancov(iplane)
        treal(19) = lai(iplane)
        treal(20) = inrcov(iplane)
        treal(21) = rilcov(iplane)
        treal(22) = vdmt(iplane)
        treal(23) = rtmass(iplane)
        treal(24) = rtm15(iplane)
        treal(25) = rtm30(iplane)
        treal(26) = rtm60(iplane)
        treal(27) = rtd(iplane)
        treal(28) = rmagt(iplane)
c
        do 30 kk = 1, 3
          treal(28+kk) = rmogt(kk,iplane)
          treal(31+kk) = smrm(kk,iplane)
          treal(34+kk) = rtm(kk,iplane)
   30   continue
c
        treal(38) = avpor(iplane) * 100.0
        treal(39) = avbd(iplane) / 1000.0
        treal(40) = ks(iplane) * 3.6e6
        treal(41) = sm(iplane) * 1000.0
        treal(42) = et(iplane) * 1000.0
c       treal(39) = thetfc(1,iplane)
c       treal(40) = thetdr(1,iplane)
        treal(43) = drainq(iplane)
        treal(44) = solthk(nsl(iplane),iplane) - satdep(iplane)
        treal(45) = effint(iplane) * 3.6e6
        treal(46) = peakro(iplane) * 3.6e6
        treal(47) = effdrn(iplane) / 3600.0
        treal(48) = enrato(iplane)
        treal(49) = (ki(iplane)*kiadjf(iplane)) / 1000000.0
        treal(50) = (kr(iplane)*kradjf(iplane)) * 1000.0
        treal(51) = shcrit(iplane) * tcadjf(iplane)
        treal(52) = width(iplane)
        treal(53) = ep(iplane) * 1000.0
        treal(54) = es(iplane) * 1000.0
        treal(55) = sep(iplane) * 1000.0
        treal(56) = watstr(iplane)
        treal(57) = temstr(iplane)
        treal(58) = watcon * 1000.0
c
        do 40 kk = 1, mxnsl
          treal(58+kk) = soilw(kk,iplane) * 1000.0
   40   continue
c
        treal(69) = rrc(iplane) * 1000.0
        treal(70) = rh(iplane) * 1000.0
        treal(71) = frdp(iplane) * 1000.0
        treal(72) = thdp(iplane) * 1000.0
        treal(73) = snodpy(iplane) * 1000.0
        treal(74) = wmelt(iplane) * 1000.0
        treal(75) = densg(iplane)
        treal(76) = frccov(iplane)
        treal(77) = frlive(iplane)
        treal(78) = frctrl(iplane)
        treal(79) = frcteq(iplane)
        treal(80) = frrres(iplane)
        treal(81) = fribas(iplane)
        treal(82) = frican(iplane)
        treal(83) = daydis(iplane)
        treal(84) = ofelod(iplane)
        effdrn(iplane) = 0.0
        temstr(iplane) = 1.0
        frccov(iplane) = 0.0
        frlive(iplane) = 0.0
        frctrl(iplane) = 0.0
        frcteq(iplane) = 0.0
        frrres(iplane) = 0.0
        fribas(iplane) = 0.0
        frican(iplane) = 0.0
c       ofelod(iplane) = 0.0
c
c
c       determine the minimums and maximums.
c
        do 50 i = 1, 84
          call mxreal(treal(i),ralmin(i),ralmax(i))
   50   continue
        do 60 i = 1, 8
          call mxint(tint(i),intmin(i),intmax(i))
   60   continue
c
c
c       write the daily information
c
        write (40,1000) tint(1), (treal(i),i=1,83), (tint(i),i = 2,8),
     1         treal(84)
c
c
c     WEPP has completed - append min/max values.
c
      else
c
        write (40,*) '#'
        write (40,*) '#       Minimum/Maximum values:'
        write (40,*) '#'
c
        write (40,1000)intmin(1),(ralmin(i),i=1,83),(intmin(i),i = 2,8),
     1        ralmin(84)
        write (40,1000)intmax(1),(ralmax(i),i=1,83),(intmax(i),i = 2,8),
     1        ralmax(84)
c
      end if
      return
c
 1000 format (i6,83(1x,f10.5),7(1x,i2),1x,f10.5)
      end