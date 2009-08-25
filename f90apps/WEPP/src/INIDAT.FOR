      subroutine inidat(iniflg)
c
c     + + + PURPOSE + + +
c
c     Initializes common block variables at the beginning of a
c     hillslope version simulation and for every hillslope of a
c     hillslope/watershed simulation
c
c     Called from: MAIN
c     Author(s): Ascough II, Livingston
c     Reference in User Guide:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c
      include 'pmxcrp.inc'
      include 'pmxcsg.inc'
      include 'pmxcut.inc'
      include 'pmxelm.inc'
      include 'pmxgrz.inc'
      include 'pmxhil.inc'
      include 'pmximp.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxprt.inc'
      include 'pmxpts.inc'
      include 'pmxres.inc'
      include 'pmxseg.inc'
      include 'pmxslp.inc'
      include 'pmxsrg.inc'
      include 'pmxtil.inc'
      include 'pmxtim.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'ptilty.inc'
      include 'pxstep.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cavloss.inc'
c
      include 'cchcas.inc'
c     modify: effshl
      include 'cchpar.inc'
c     modify: ishape(mxplan),chnlen(mxplan),chnz(mxplan),
c             chnnbr(mxplan),ncsseg(mxplan),
c             chnx(mxplan,mxcseg),chnslp(mxplan,mxcseg),
c             chnwid(mxplan),flgout(mxplan)
c
      include 'cchpek.inc'
c     modify: uparea(0:mxplan),loarea(0:mxplan),
c             charea(0:mxplan),toarea(0:mxplan),
c             chleng(0:mxplan),chmann(mxplan),
c             chslop(mxplan)
c
      include 'cchprt.inc'
c     modify: crdia(mxpart,mxelem),crspg(mxpart),
c             crfrac(mxpart,mxelem),crfall(mxpart,mxelem),
c             solcly(mxelem),solsnd(mxelem),solslt(mxelem),
c             solorg(mxelem),sssoil(mxelem)
c
      include 'cchsed.inc'
c     modify: ggs(mxpart,mxelem),conc(mxpart,mxelem)
c
      include 'cchtrl.inc'
c     modify: icntrl(mxplan), ctlz(mxplan), ctln(mxplan),
c             ctlslp(mxplan), rccoef(mxplan),
c             rcexp(mxplan), rcoset(mxplan), ycntrl(mxplan),
c             ienslp(mxplan)
c
      include 'cchvar.inc'
c     modify: x(mxcseg),slope(mxcseg),chnlef,chnn(mxplan),
c             chnks(mxplan),chntcr(mxplan),chnedm(mxplan),
c             chneds(mxplan),chnk(mxplan)
c
      include 'cclim.inc'
c     modify: tmnavg, tmxavg, tave
c
      include 'ccdrain.inc'
      include 'ccntour.inc'
c
      include 'ccntfg.inc'
c     modify: cntflg
c
      include 'ccontcv.inc'
c     modify: tilseq(mxcrop,mxplan),tilflg
c
      include 'cconsta.inc'
c     modify: accgav,wtdens,kinvis,msdens
c
      include 'ccover.inc'
c     modify: kiadjf(mxplan),kradjf(mxplan),tcadjf(mxplan),
c             prestr,ntill,daydis(mxplan)
c
      include 'ccrpgro.inc'
c
      include 'ccrpout.inc'
c     modify: rtmass,rtd,rtm15,rtm30,rtm60
c
      include 'ccrpprm.inc'
      include 'ccrpvr1.inc'
      include 'ccrpvr2.inc'
      include 'ccrpvr3.inc'
      include 'ccrpvr5.inc'
      include 'cdata.inc'
      include 'cdecvar.inc'
      include 'cdetcom.inc'
      include 'cdiss2.inc'
      include 'cends.inc'
      include 'cenrpas.inc'
c
      include 'cefflen.inc'
c     modify: efflen(mxplan)
c
      include 'cerrid.inc'
c     modify: ifile
c
      include 'cfall.inc'
c     modify: cdre(9),cdre2(9),cddre(9),frcmm1(10),frcmm2(10),
c             frcyy1(10),frcyy2(10),frcff1(10),frcff2(10)
c
      include 'cffact.inc'
c     modify: frcsol,frctrl
c
      include 'cflags.inc'
c     modify: bigflg,ichplt,ipdout,snoflg,wgrflg,yldflg,idflag
c
      include 'cgully.inc'
c     modify: depa(mxplan,mxcseg),depb(mxplan,mxcseg),
c             wida(mxplan,mxcseg),widb(mxplan,mxcseg),
c             wera(mxplan,mxcseg),werb(mxplan,mxcseg)
c
      include 'chydrol.inc'
c
c     include 'cimcv1.inc'
c     modify: c1icv(mximp)
c
      include 'cimpnd.inc'
c     modify: ipond
c
      include 'cincon.inc'
      include 'ciplot.inc'
      include 'ciravlo.inc'
c
      include 'cirdepl.inc'
c     modify: iramt
c
      include 'cirfur1.inc'
      include 'cirriga.inc'
      include 'cirspri.inc'
c
      include 'coutchn.inc'
c     modify: tgst(mxpart,mxelem),tgsm(mxpart,mxelem),
c             tgsy(mxpart,mxelem),nrunm,nruny,nrunt,
c             trunm(mxelem),truny(mxelem),trunt(mxelem),
c             nsedm(mxcseg),csedm(mxcseg),nsedy(mxcseg),
c             csedy(mxcseg),nsedt(mxcseg),csedt(mxcseg),
c             tgsd(mxpart,mxelem)
c
      include 'cpart.inc'
c
c     cpart1.inc
c     modify: npart,frac(mxpart,mxelem)
c
c     cpart2.inc
c     modify: fall(mxpart,mxelem),frcly(mxpart,mxelem),
c             frslt(mxpart,mxelem),frsnd(mxpart,mxelem),
c             frorg(mxpart,mxelem)
c
c     cpart3.inc
c     modify: dia(mxpart,mxelem),spg(mxpart),eqsand(mxpart,mxelem)
c
      include 'cpass.inc'
      include 'cperen.inc'
c
      include 'cprams2.inc'
c     modify: alphay(mxplan)
c
      include 'crndm.inc'
c     modify: aa,mrnd
c
      include 'cridge.inc'
      include 'crinpt1.inc'
c
      include 'crinpt2.inc'
c     modify: prgc(mxplan)
c
      include 'crinpt3.inc'
      include 'crinpt5.inc'
      include 'crinpt6.inc'
      include 'crout.inc'
      include 'cseddet.inc'
      include 'csedld.inc'
c
      include 'csenes.inc'
c     modify: isenes(mxplan)
c
      include 'cslinit.inc'
      include 'cslope.inc'
c
      include 'cslpopt.inc'
c     modify: fwidth(mxplan),totlen(mxplan),hmann
c
      include 'cstore.inc'
c     modify: peakin(0:mxelem),peakot(0:mxelem),
c             runvol(0:mxelem),rvolat(0:mxelem),
c             rvotop(0:mxelem),rvolon(0:mxelem),
c             rvoimp(0:mxelem),roffon(0:mxelem),
c             chnvol(0:mxelem),chnrun(0:mxelem),
c             hlarea(0:mxelem),wsarea(0:mxelem),
c             halpha(0:mxelem),walpha(0:mxelem),
c             hildur(0:mxelem),rundur(0:mxelem),
c             watdur(0:mxelem),htcs(0:mxelem),
c             rtrans(mxelem),  sedcon(mxpart,0:mxelem),
c             rofave(0:mxelem),htcc(0:mxelem),
c             alphaq(0:mxelem), hsedm(mxhill), hsedy(mxhill),
c             hsedt(mxhill)
c
      include 'cstruc.inc'
c
      include 'cstruct.inc'
c     modify: ichan,nchan,ich(mxplan),ielmt,nelmt,
c             idelmt(0:mxelem),elmt(mxelem),nhleft(mxelem),
c             nhrght(mxelem),nhtop(mxelem),ncleft(mxelem),
c             ncrght(mxelem),nctop(mxelem),nileft(mxelem),
c             nirght(mxelem),nitop(mxelem),ieltyp(mxelem)
c
c
      include 'csumirr.inc'
c
      include 'csumout.inc'
c     modify: nraint,nrainy,nrainm(13),traint,trainy,trainm(13),
c             nrunot(mxplan),nrunoy(mxplan),nrunom(13,mxplan),
c             trunot(mxplan),trunoy(mxplan),trunom(13,mxplan),
c             avlost(mxplan),avlosy(mxplan),avlosm(13,mxplan)
c             mmelt
c
      include 'ctemp.inc'
c     modify: sand1(mxnsl,mxplan),clay1(mxnsl,mxplan),
c             orgma1(mxnsl,mxplan),rfg1(mxnsl,mxplan),
c             cec1(mxnsl,mxplan),
c             ssc1(mxnsl,mxplan),bd1(mxnsl,mxplan),
c             thetd1(mxnsl,mxplan),
c             thetf1(mxnsl,mxplan),solth1(mxnsl,mxplan),
c             aveke(mxplan)
c
      include 'ctillge.inc'
c     modify: resman(mxtill,mxtlsq)
c
      include 'cupdate.inc'
c
      include 'cwater.inc'
c     modify: ft(mxnsl),tv(mxplan)
c
      include 'cwint.inc'
c     modify: azm(mxplan),wmelt(mxplan)
c
c     include 'cerode.inc'
c
      include 'cyield.inc'
c     modify: sumyld(ntype,mxplan),iyldct(ntype,mxplan)
c
      include 'cbgout.inc'
c     modify: ralmin(80),ralmax(80),intmin(8),intmax(8)
c
      include 'ceqrot.inc'
c     modify: ax
c
      include 'cifpar.inc'
c     modify: rfcumx
c
      include 'cke.inc'
c     modify: rkecum
c
      include 'cparame.inc'
c     modify: tp(mxpond)
c
      include 'cpsis1.inc'
c     modify: u
c
      include 'cptgrow.inc'
c     modify: ngraz(mxplan),idecom(mxplan),ifreez(mxplan),
c             icount(mxplan),iburn(mxplan)
c
      include 'ckrcon.inc'
c     modify: ifrost(mxplan)
c
      include 'cstmgt.inc'
c     modify: nrec,tmp(6),tmpx(6),idtot
c
      include 'cver.inc'
c     modify: ver,vermon,veryr
c
      include 'cdat.inc'
c     modify: manchk,solchk,slpchk,hilchk,impchk,strchk,
c             chnchk,irdchk,irfchk
cd   Added by S. Dun, January 07, 2008
c
      include 'cflgfs.inc'
c
      include 'ctcurv.inc'
c     initiate: YpshfT
cd    End adding
c
c
c     + + + LOCAL VARIABLES + + +
c
      integer i, iniflg, j, k
c
c     + + + LOCAL DEFINITIONS + + +
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (iniflg.eq.1) then
        ver = 2008.907
        vermon = ' September 29,  '
        veryr = 2008
        return
      end if
c
      do 5 i=1,mxpond
        tp(i) = 0.0
  5   continue
c
cd    Added by S. Dun, January 07, 2008
c
      YpshfT = -1.
cd    end adding
c
c     ...set 0 value for (0:mxplan) dimensioned variables
c
      uparea(0) = 0.0
      loarea(0) = 0.0
      charea(0) = 0.0
      toarea(0) = 0.0
      chleng(0) = 0.0
      efflen(0) = 0.0
c
c
c     ...begin mxplan dimension loop
c
      do 130 i = 1, mxplan
        access(i) = 0
        alphay(i) = 0.0
        am(i) = 0.0
        avke(i) = 0.0
        avsolc(i) = 0.0
        azm(i) = 0.0
        bcover(i) = 0.0
        bdtill(i) = 0.0
        canhgt(i) = 0.0
        cntday(i) = 0
        cntend(i) = 0
        fcycle(i) = 0
        fgcycl(i) = 0
        fgthwd(i) = 0
        watpdg(i) = 0.
        watbtm(i) = 0.
        daydis(i) = 0.0
        ddrain(i) = 0
        delxx(i) = 0.0
        densg(i) = 100.0
        drainc(i) = 0
        drainq(i) = 0.0
        drdiam(i) = 0.0
        dsharv(i) = 0
        effdrn(i) = 0.0
        effint(i) = 0.0
        efflen(i) = 0.0
        enrato(i) = 0.0
        ep(i) = 0.0
        es(i) = 0.0
        et(i) = 0.0
        frccov(i) = 0.0
        frcsol(i) = 1.0
        frcteq(i) = 0.0
        frctrl(i) = 1.0
        fribas(i) = 0.0
        frican(i) = 0.0
        frlive(i) = 0.0
        fwidth(i) = 0.0
        frrres(i) = 0.0
        growth(i) = 0.0
        hia(i) = 0.0
        iburn(i) = 0
        icount(i) = 0
        idecom(i) = 0
        idrain(i) = 0
        ifreez(i) = 0
        ifrost(i) = 0
        indxy(i) = 0
        irapld(i) = 0.0
        iraplo(i) = 0.0
        irdept(i) = 0.0
        irend(i) = 0
        irrunm(i) = 0.0
        irint(i) = 0.0
        irrund(i) = 0.0
        irrunt(i) = 0.0
        irruny(i) = 0.0
        isenes(i) = 0
        it(i) = 1
        jgraz(i) = 0
        kiadjf(i) = 1.0
        kradjf(i) = 1.0
        lai(i) = 0.0
        ngraz(i) = 0
        ncount(i) = 0
        nmunom(i) = 0
        nmunot(i) = 0
        nmunoy(i) = 0
        nnc(i) = 0
        nozzle(i) = 1.0
        nrunom(i) = 0
        nrunot(i) = 0
        nrunoy(i) = 0
        ofelod(i) = 0.0
        plaint(i) = 0.0
        pintlv(i) = 0.0
        pop(i) = 0.0
        prestr(i) = 0.0
        prgc(i) = 0.0
        rain(i) = 0.0
        resint(i) = 0.0
        rfcum(i) = 0.0
        rkecum(i) = 0.0
        rmagt(i) = 0.0
        rowspc(i) = 0.0
        rtd(i) = 0.0
        rtm15(i) = 0.0
        rtm30(i) = 0.0
        rtm60(i) = 0.0
        rtmass(i) = 0.0
        runoff(i) = 0.0
        rwflag(i) = 1
        satdep(i) = 0.0
        sdrain(i) = 0
        senvin(i) = 0.0
        srmhav(i) = 0.1
        sumgdd(i) = 0.0
        suppmt(i) = 0.0
        tcadjf(i) = 1.0
        temstr(i) = 1.0
        tlive(i) = 0.0
        totfal(i) = 0
        totlen(i) = 0.0
        tmunom(i) = 0.0
        tmunot(i) = 0.0
        tmunoy(i) = 0.0
        trunom(i) = 0.0
        trunot(i) = 0.0
        trunoy(i) = 0.0
        tv(i) = 0.0
        unsdep(i) = 0.0
        vdmt(i) = 0.0
        watstr(i) = 1.0
        wdhtop(i) = 0.01
        width(i) = 0.01
        wincnt(i) = 1
        wmelt(i) = 0.0
        wntflg(i) = 0
        yrend(i) = 0
c
c       ...begin watershed variables mxplan loop
c
        ishape(i) = 0
        chnlen(i) = 0.0
        chnz(i) = 0.0
        chnnbr(i) = 0.0
        ncsseg(i) = 0
        chnwid(i) = 0.0
        flgout(i) = 0
c
        icntrl(i) = 0
        ctlz(i) = 0.0
        ctln(i) = 0.0
        ctlslp(i) = 0.0
        rccoef(i) = 0.0
        rcexp(i) = 0.0
        rcoset(i) = 0.0
        ycntrl(i) = 0.0
        ienslp(i) = 0
c
        chnn(i) = 0.0
        chnks(i) = 0.0
        chntcr(i) = 0.0
        chnedm(i) = 0.0
        chneds(i) = 0.0
        chnk(i) = 0.0
c
        ich(i) = 0
c
        uparea(i) = 0.0
        loarea(i) = 0.0
        charea(i) = 0.0
        toarea(i) = 0.0
        harea(i) = 0.0
        chleng(i) = 0.0
        chmann(i) = 0.0
        chslop(i) = 0.0
c
c       ...begin watershed variables (mxplan,mxcseg) loop
c
        do 10 j = 1, mxcseg
c
          chnx(i,j) = 0.0
          chnslp(i,j) = 0.0
c
          depa(i,j) = 0.0
          depb(i,j) = 0.0
          wida(i,j) = 0.0
          widb(i,j) = 0.0
          wera(i,j) = 0.0
          werb(i,j) = 0.0
c
   10   continue
c
c       ...end (mxplan,mxcseg) dimension loop
c
c       ...end watershed variables section
c
c       ...begin (ntype,mxplan) dimension loop
c
        do 20 j = 1, ntype
          gddmax(j) = 0.0
          iyldct(j,i) = 0
          sumyld(j,i) = 0.0
          plive(j,i) = 0.0
   20   continue
c
c       ...end (ntype,mplan) dimension loop
c
c       ...begin (mxnsl,mxplan) dimension loop
c
        nwfzfg(i) = 0.0
c
        do 30 j = 1, mxnsl
          frozen(j,i) = 0.0
          soilw(j,i) = 0.0
          frzw(j,i) = 0.0
          nwfrzz(j,i) = 0.0
          yst(j,i) = 0.0
          solth1(j,i) = 0.0
          bd1(j,i) = 0.0
          ssc1(j,i) = 0.0
          thetf1(j,i) = 0.0
          thetd1(j,i) = 0.0
          sand1(j,i) = 0.0
          clay1(j,i) = 0.0
          orgma1(j,i) = 0.0
          cec1(j,i) = 0.0
          rfg1(j,i) = 0.0
   30   continue
c
c       ...end (mxnsl,mxplan) dimension loop
c
c       ...begin (mxres,mxplan) dimension loop
c
        do 40 j = 1, mxres
          benvin(j,i) = 0.0
          fenvin(j,i) = 0.0
          iresd(j,i) = 0
          iroot(j,i) = 0
          rigrm(j,i) = 0.0
          rilrm(j,i) = 0.0
          rmogt(j,i) = 0.0
          rtm(j,i) = 0.0
          smrm(j,i) = 0.0
   40   continue
c
c       ...end (mxres,mxplan) dimension loop
c
c       ...begin (3,mxplan) dimension loop
c
        do 50 j = 1, 3
          fail(j,i) = 0
   50   continue
c
c       ...end (3,mxplan) dimension loop
c
c       ...begin (mxplan,100) dimension loop
c
        do 60 j = 1, 100
          dsavg(i,j) = 0.0
   60   continue
c
c       ...end (mxplan,100) dimension loop
c
c       ...begin (mxcrop,mxplan) dimension loop
c
        do 90 j = 1, mxcrop
          conseq(j,i) = 0
          contrs(j,i) = 0
          fbrnag(j,i) = 0.0
          fbrnog(j,i) = 0.0
          frcut(j,i) = 0.0
c          frmove(j,i) = 0.0
          jdburn(j,i) = 0
          jdcut(j,i) = 0
          jdharv(j,i) = 0
          jdsene(j,i) = 0
          jdherb(j,i) = 0
c          jdmove(j,i) = 0
          jdplt(j,i) = 0
          jdslge(j,i) = 0
          jdstop(j,i) = 0
          mgtopt(j,i) = 0
          ncut(j,i) = 0
          resmgt(j,i) = 0
          rw(j,i) = 0.0
          tilseq(j,i) = 0
          drseq(j,i) = 0
          tothav(j,i) = 0.0
c
c         ...begin (mxcut,mxcrop,mxplan) dimension loop
c
          do 70 k = 1, mxcut
            cutday(k,j,i) = 0
   70     continue
c
c         ...end (mxcut,mxcrop,mxplan) dimension loop
c
c         ...begin (mxgraz,mxcrop,mxplan) dimension loop
c
          do 80 k = 1, mxgraz
            gend(k,j,i) = 0
   80     continue
c
c       ...end (mxgraz,mxcrop,mxplan) dimension loop
c
   90   continue
c
c       ...end (mxcrop,mxplan) dimension loop
c
c       ...begin (mxslp,mxplan) dimension loop
c
        do 100 j = 1, mxslp
          xu(j,i) = 0.0
  100   continue
c
c       ...end (mxslp,mxplan) dimension loop
c
c       ...begin (mxpart,mxplan) dimension loop
c
        do 110 j = 1, mxpart
          frcflw(j,i) = 0.0
  110   continue
c
c       ...end (mxpart,mxplan) dimension loop
c
c       ...begin (mxplan,6) dimension loop
c
        do 120 j = 1, 6
          r5(i,j) = 0.0
  120   continue
c
c     ...end (mxplan,6) dimension loop
c
  130 continue
c
c     ...end mxplan dimension loop
c
c     ...begin ntype dimension loop
c
      do 140 i = 1, ntype
        diam(i) = 0.0
        sdiam(i) = 0.0
        tdiam(i) = 0.0
        gdiam(i) = 0.0
        yld(i) = 0.0
        strrgc(i) = 0.0
  140 continue
c
c     ...end ntype dimension loop
c
c     ...begin mxnsl dimension loop
c
      do 150 i = 1, mxnsl
        ft(i) = 0.0
        drawat(i) = 0.0
  150 continue
c
c     ...end mxnsl dimension loop
c
c     ...begin mxpart dimension loop
c
      do 170 i = 1, mxpart
c
        frcavg(i) = 0.0
        frcff1(i) = 0.0
        frcff2(i) = 0.0
        frcmm1(i) = 0.0
        frcmm2(i) = 0.0
        frcmon(i) = 0.0
        frcyr(i) = 0.0
        frcyy1(i) = 0.0
        frcyy2(i) = 0.0
c
        spg(i) = 0.0
c
c       ...begin watershed variables (mxpart) loop
c
        crspg(i) = 0.0
c
c       ...end watershed variables (mxpart) loop
c
c       ...begin (mxpart,mxelem) loop
c
c       ...begin watershed variables (mxpart,mxelem) loop
c
c       ...set 0 value for (mxpart,0:mxelem) dimensioned variables
c
        sedcon(i,0) = 0.0
c
        do 160 j = 1, mxelem
c
          tgst(i,j) = 0.0
          tgsm(i,j) = 0.0
          tgsy(i,j) = 0.0
          tgsd(i,j) = 0.0
c
          sedcon(i,j) = 0.0
c
          frac(i,j) = 0.0
c
          fall(i,j) = 0.0
          frcly(i,j) = 0.0
          frslt(i,j) = 0.0
          frsnd(i,j) = 0.0
          frorg(i,j) = 0.0
c
          dia(i,j) = 0.0
          eqsand(i,j) = 0.0
c
          crdia(i,j) = 0.0
          crfrac(i,j) = 0.0
          crfall(i,j) = 0.0
c
          ggs(i,j) = 0.0
          conc(i,j) = 0.0
c
  160   continue
c
c     end (mxpart,mxelem) loop
c
  170 continue
c
c     ...end mxpart dimension loop
c
c     ...begin mxsrg dimension loop
c
      do 180 i = 1, mxsrg
        tstart(i) = 0.0
  180 continue
c
c     ...end mxsrg dimension loop
c
c     ...begin mxtime dimension loop
c
      do 190 i = 1, mxtime
        rcum(i) = 0.0
        t(i) = 0.0
        rr(i) = 0.0
  190 continue
c
c     ...end mxtime dimension loop
c
c     ...begin mxtlsq dimension loop
c
      do 210 i = 1, mxtlsq
        iridge(i) = 0
        ntill(i) = 0
c
c       ...begin (mxtill,mxtlsq) loop
c
        do 200 j = 1, mxtill
          mdate(j,i) = 0
c          iresd(j,i) = 0.0
          resad(j,i) = 0.0
          frmove(j,i) = 0.0
          jdmove(j,i) = 0
          resman(j,i) = 0

  200   continue
c
c     ...end (mxtill,mxtlsq) loop
c
  210 continue
c
c     ...end mxtlsq dimension loop
c
c     ...begin mxpts dimension loop
c
      do 220 i = 1, mxpts
        dstot(i) = 0.0
        stdist(i) = 0.0
  220 continue
c
c     ...end mxpts dimension loop
c
c     ...begin watershed variables loop
c
c     ...begin mxcseg dimension loop
c
      do 230 i = 1, mxcseg
c
        x(i) = 0.0
        slope(i) = 0.0
c
        nsedm(i) = 0.0
        csedm(i) = 0.0
        nsedy(i) = 0.0
        csedy(i) = 0.0
        nsedt(i) = 0.0
        csedt(i) = 0.0
  230 continue
c
c     ...end mxcseg dimension loop
c
c     ...begin watershed variables mxelem dimension loop
c
c     ...set 0 values for (0:mxelem) dimensioned arrays
c
      peakin(0) = 0.0
      peakot(0) = 0.0
      peakro(0) = 0.0
      runvol(0) = 0.0
      sbrunv(0) = 0.0
      runoff(0) = 0.0
      rvolat(0) = 0.0
      rvotop(0) = 0.0
      rvolon(0) = 0.0
      rvoimp(0) = 0.0
      roffon(0) = 0.0
      chnvol(0) = 0.0
      chnrun(0) = 0.0
      hlarea(0) = 0.0
      wsarea(0) = 0.0
      hildur(0) = 0.0
      rundur(0) = 0.0
      watdur(0) = 0.0
      rofave(0) = 0.0
      htcc(0) = 0.0
      alphaq(0) = 0.0
      htcs(0) = 0.0
      halpha(0) = 0.0
      walpha(0) = 0.0
c
      idelmt(0) = 0
c
c     ...begin mxelem dimension loop
c
      do 240 i = 1, mxelem
c
        peakin(i) = 0.0
        peakot(i) = 0.0
        peakro(i) = 0.0
        trunm(i) = 0.0
        truny(i) = 0.0
        trunt(i) = 0.0
        runvol(i) = 0.0
        sbrunv(i) = 0.0
        runoff(i) = 0.0
        rvolat(i) = 0.0
        rvotop(i) = 0.0
        rvolon(i) = 0.0
        rvoimp(i) = 0.0
        roffon(i) = 0.0
        rundur(i) = 0.0
        watdur(i) = 0.0
        rofave(i) = 0.0
        htcc(i) = 0.0
        alphaq(i) = 0.0
        chnvol(i) = 0.0
        chnrun(i) = 0.0
        hlarea(i) = 0.0
        wsarea(i) = 0.0
        hildur(i) = 0.0
        htcs(i) = 0.0
        halpha(i) = 0.0
        walpha(i) = 0.0
        rtrans(i) = 0.0
        idelmt(i) = 0
        elmt(i) = 0
        nhleft(i) = 0
        nhrght(i) = 0
        nhtop(i) = 0
        ncleft(i) = 0
        ncrght(i) = 0
        nctop(i) = 0
        nileft(i) = 0
        nirght(i) = 0
        nitop(i) = 0
        ieltyp(i) = '           '
c
        solcly(i) = 0.0
        solsnd(i) = 0.0
        solslt(i) = 0.0
        solorg(i) = 0.0
        sssoil(i) = 0.0
c
        cdetm(i) = 0.0
        cdety(i) = 0.0
        cdett(i) = 0.0
c
        cdepm(i) = 0.0
        cdepy(i) = 0.0
        cdept(i) = 0.0
c
cx    Added by Arthur Xu, 06/2000
        sbrfty(i) = 0.0
        sbrvty(i) = 0.0
        sbrvtt(i) = 0.0
        tronvt(i) = 0.0
        sbrftm(i) = 0.0
cx    End adding

        hill(i) = 0

  240 continue
c
c     ...end mxelem dimension loop
c
c     ...begin xsteps+1 dimension loop
c
      do 260 i = 0, xsteps
        do 250 j = 1, 2
          aflow(i,j) = 0.0
          infltr(i,j) = 0.0
          qflow(i,j) = 0.0
  250   continue
  260 continue
c
c     ...end xsteps+1 dimension loop
c
c     ...begin miscellaneous loops
c
      do 270 i = 1, 20
        intdl(i) = 0.0
        timedl(i) = 0.0
  270 continue
c
      do 280 i = 1, 80
        ralmin(i) = 1.0e10
        ralmax(i) = 0.0
  280 continue
c
      do 290 i = 1, 8
        intmin(i) = 100
        intmax(i) = 1
  290 continue
c
      do 300 i = 1, 6
        tmp(i) = 0.0
        tmpx(i) = 0.0
  300 continue
c
c     initialize hillslope output variables
c
      do 310 i = 1, mxhill
c
        detm(i) = 0.0
        dety(i) = 0.0
        dett(i) = 0.0
c
        depm(i) = 0.0
        depy(i) = 0.0
        dept(i) = 0.0
c
        hrom(i) = 0.0
        hroy(i) = 0.0
        hrot(i) = 0.0
c
        hsedm(i) = 0.0
        hsedy(i) = 0.0
        hsedt(i) = 0.0
c
  310 continue

c     do 350 i = 1,mximp
c       c1icv(i) = 0.0
c 350 continue

c
c     ...nonlooping or nondimensional variables initialization
c
      cdre(1) = -6.90775
      cdre(2) = -4.60517
      cdre(3) = -2.30258
      cdre(4) = 0.0
      cdre(5) = 2.30258
      cdre(6) = 4.60517
      cdre(7) = 6.90775
      cdre(8) = 9.21034
      cdre(9) = 11.51292
c
      cdre2(1) = -4.50986
      cdre2(2) = -1.51413
      cdre2(3) = 0.78846
      cdre2(4) = 3.12676
      cdre2(5) = 6.04025
      cdre2(6) = 9.30565
      cdre2(7) = 13.08154
      cdre2(8) = 17.50439
      cdre2(9) = 22.29188
c
      cddre(1) = 16.83594
      cddre(2) = 12.23077
      cddre(3) = 7.62560
      cddre(4) = 3.17805
      cddre(5) = -0.89160
      cddre(6) = -4.55638
      cddre(7) = -7.75173
      cddre(8) = -10.12663
      cddre(9) = -12.33391
c
      rx(1) = 5.0
      rx(2) = 4.0
      rx(3) = 3.0
      rx(4) = 2.0
      rx(5) = 1.0
c
      aa = 16807.d0
      accgav = 9.807
      avsolf = 0.0
      ax = 0.0
      bigflg = 0
      cntflg = 0
      dcap = 0.0
      eatax = 0.0
      enravg = 0.0
      enrff1 = 0.0
      enrff2 = 0.0
      hmann = 0.0
      ianflg = 0
      ibd = 0.0
      icanco = 0.0
      icrypt = 0.0
      idaydi = 0
      idflag = 0
      idshar = 0
      idtot = 0
      ifile = 1
      ifrdp = 0.0
      iinrco = 0.0
      iiresd = 1
      ioutas = 0
      ioutpt = 0
      ioutss = 0
      iplane = 0
      ipond = 0
      ipptg = 0.0
      iramt = 0.0
      irdmax = 0.0
      irdur = 0.0
      irfcum = 0.0
      irilco = 0.0
      irhini = 0.0
      irmagt = 0.0
      irmogt = 0.0
      irofe = 0
      iroute = 0
      irqin = 0.0
      irrini = 0.0
      irsolm = 0.0
      irsolt = 0.0
      irsoly = 0.0
      irspac = 0.0
      irtype = 0
      isum = 0
      ifofe = 0
      ievt = 0
      isnodp = 0.0
      ithdp = 0.0
      itill1 = 0.1
      itill2 = 0.2
      iwidth = 0.0
      iwind = 0
      kinvis = 1.0e-06
      mmelt = 0.0
      mrro = 0.0
      mrnd = 2147483647.d0
      msdens = 1000.0
      ncommm = 0
      ncommt = 0
      ncommy = 0
      nirrm = 0
      nirrt = 0
      nirry = 0
      noirr = 0
      npart = 5
      nrainm = 0
      nraint = 0
      nrainy = 0
      nrec = 10
      ns = 0
      nsurge = 0
      qin = 0.0
      prcp = 0.0
      rfcumx = -1.0
      rkine = 0.0
      sdate = 0
      shape2 = 0.0
      shr = 0.0
      snoflg = 0
      spavz = 0.65
      taucx = 0.0
      tave = 0.0
      tilflg = 0
      tirrm = 0
      tirrt = 0
      tirry = 0
      tmnavg = 0.0
      tmxavg = 0.0
      trainm = 0.0
      traint = 0.0
      trainy = 0.0
      trro = 0.0
      u = 0.0
      yldflg = 0
      wtdens = 9807
      xx = 0.0
      manchk = 93.005
      solchk = 91.5
      slpchk = 91.5
c
      hilchk = 94.301
      strchk = 94.301
      chnchk = 94.301
      impchk = 94.301
c
      irdsch = 94.21
      irdfch = 91.5
      irfsch = 94.21
      irffch = 91.5
      idsver = 1
      idfver = 1
      ifsver = 1
      iffver = 1
      radmj = 0.0
      rngplt = 1
      rnganm = 1
c
c     ...begin nondimensioned watershed variables section
c
      chnlef = 0.0
c
      effshl = 0.0
c
      ichan = 0
      ichplt = 0
      ielmt = 0
c
      ipdout = 0
      ipeak = 0
c
      nchan = 0
      nelmt = 0
c
      nrunm = 0
      nrunt = 0
      nruny = 0
c
      watsum = 0
      wgrflg = 0
c
      return
      end
