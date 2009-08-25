      subroutine impint
c
c     + + + PURPOSE + + +
c
c     SR IMPINT is the front end user interface portion of the
c     impoundment element.  It develops stage-discharge,
c     stage-length, and stage-area functions from user-input
c     structure and geometry information.
c
c     Called from: MAIN
c     Author(s):  Mark Lindley, Jim Ascough II
c     Reference in User Guide:
c
c     Version:
c     Date recoded: 2/08/95
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmximp.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cflags.inc'
c     read: ipdout
c
      include 'cimday.inc'
      include 'cimeos.inc'
c
      include 'cimitf.inc'
c     write: a,b,c,d,e,ha,ht,hlm,a0,a1,a2,l0,l1,l2,qinf,isize
c
      include 'cimmon.inc'
      include 'cimpnd.inc'
c
      include 'cimqot.inc'
c     write: h,deltat
c
      include 'cimrou.inc'
c     write: bwes,sses,nes,pypos,zes,mdwit,dwt,rfrac,yc,qesr,
c            depth,ncross
c
      include 'cimsed.inc'
c     write: d0,di,d100,sg,ndiv,ctd,cdp,hmin,hset,co,vs,cot
c
      include 'cimyrs.inc'
c
c       added by S.Dun  March 16,1999
        include 'cimreg.inc'
c       write coefficients for rearrangment the flow regimes.
        include 'cimds.inc'
c       Write variables describing culvert 1's feature
        include 'cimcv1.inc'
c       Write variables describing culvert 1's feature
        include 'cimcv2.inc'
c       Write variables describing culvert 2's feature
        include 'cimrf.inc'
c       write variables describing Rock-fill check dam
        include 'cimff.inc'
c       write variables describing straw bales,filterfence,or trash barrier
        include 'cimpr.inc'
c       write variables describing perforated riser
        include 'cimflg.inc'
c       Write structure indicator of an impoundment
        include 'cimacl.inc'
c       impoundment automatically clean message
        include 'cimsre.inc'
c       Sediment stage at last rearrangement time.
c
c       end adding
c     + + + LOCAL VARIABLES + + +
c
      real af(15,10), bf(15,10), cf(15,10), df(15,10), ef(15,10),
     1    haf(15,10), htf(15,10), qes(100), hest(100), hlf(15,10)
c
      real hl(15,30), hal(100), length(100), lbl, sbl, hblot, ke, kb,
     1    kc, hrs, coefw, coefo, lenrs, widrs, diabl, hitbl, wdbl, arcv,
     1    hitcv, hcv, lcv, scv, hcvot, kus, mus, cs, ys, lnrf, hrf,
     1    hotrf, wdrf, diarf, arf, arf1, arf2
c
      real area(100), ltrns(100), atrns(100), brf, cores, aes(5), qesii,
     1    hes, hmxes, ses1, les1, ses2, les2, ses3, vsl, wdff, hff,
     1    hotff, aout(5), lout(5), diars, qesi
c
      real qpr(100), hpr(100), qb, hp, hpdel, qs, apr(5), hs, hr, as,
     1    diab, cb, y, ko, ab, qw, hf, hrh, yci, ynes
c
      real cl0, cl100, sl0, sl100, sa0, sa100, sd0, sd100, la0, la100,
     1    diar, hb, hd, qo11, a0tmp, hfltmp, hmntmp, hottmp, l0tmp,
     1    qnftmp, dltimp, htmp
c
      integer ids, icv, ies, nfr(15), is, itf, ir(15), itr, ncv, ipr,
     1    irf, nalpts, iff, i, npts, idcul, strcnt, isztmp, j, ndvtmp, k
c
      character*72 strdes, impdes(3)
c
c     + + + LOCAL DEFINITIONS + + +
c
c     a0tmp       - temprary variable for area power function coefficient
c     ab          - area of barrel
c     aes(i)      - coefficients for the fourth degree polynomial utilized in the
c                   open channel/emergency spillway function development as
c                   returned from SR POLY
c     af(i,j)     - used for initial outflow functions, structure i, flow regime j
c     aout(i)     - coefficients for the stage-area power function as returned
c                   from SR POWER
c     apr(i)      - power function from stage-discharge relationship
c     arcv        - flow area of a culvert
c     area(i)     - area corresponding to hal(i)
c     atrns(i)    - area at hal(i) transformed by the area at hmin
c     arf         - constant used in the porous rockfill outflow function
c     arf1        - variable used in the interpolation to get arf
c     arf2        - variable used in the interpolation to get arf
c     as          - area of slots in perforated riser
c     bf(i,j)     - used for initial outflow functions, structure i, flow regime j
c     brf         - constant used in the porous rockfill outflow function
c     cb          - orifice coefficient used for perforated riser restricting orifice
c     cf(i,j)     - used for initial outflow functions, structure i, flow regime j
c     cl0         - clay 0% particle size diameter
c     cl100       - clay 100% particle size diameter
c     coefo       - orifice flow coefficient (0.6)
c     coefw       - weir flow coefficient (3.2)
c     cores       - correction for entrance head loss for emergency spillway
c     cs          - constant used in culvert flow relationship
c     df(i,j)     - used for initial outflow functions, structure i, flow regime j
c     diab        - diameter of the restricting orifice used in perforated riser
c     diabl       - diameter of drop spillway or perforated riser barrel
c     diar        - diameter of the perforated riser
c     diarf       - average diameter of rock used in rockfill
c     diars       - diameter of drop inlet riser
c     dltimp      - temporary time step variable
c     ef(i,j)     - used for initial outflow functions, structure i, flow regime j
c     haf(i,j)    - adjustment stage used in initial outflow function i, flow regime j
c     hal(i)      - stage corresponding to area(i) and length(i)
c     hb          - height below hd of the restricting orifice in a perforated riser
c     hblot       - height of the barrel outlet above the outlet channel bottom
c     hcv         - stage of the culvert inlet
c     hcvot       - height of the culvert outlet above the outlet channel bottom
c     hd          - stage at which the slots begin in a perforated riser
c     hes         - stage of the emergency spillway
c     hest(i)     - stage coressponding to emergency spillway outflow qes(i)
c     hf          - dummy stage used in emergency spillway section
c     hff         - stage at the bottom of a filter fence or straw bale structure
c     hfltmp      - temporary variable for stage full of sediment variable
c     hitbl       - height of a rectangular barrel
c     hitcv       - height of a rectangular culvert
c     hl(i,j)     - limiting lowest stage for outflow function i, flow regime j
c     hlf(i,j)    - limiting lowest stage for initial outflow function i, flow regime j
c     hmntmp      - temporary variable for minimum stage variable
c     hmxes       - maximum stage for flow in the emergency spillway
c     hotff       - stage at which filter fence is overtopped
c     hottmp      - temporary variable for stage overtopping variable
c     hotrf       - stage at which porous rockfill is overtopped
c     hp          - temporary perforated riser stage height
c     hpdel       - change in stage for perforated riser stage discharge relationship
c     hpr(i)      - array of perforated riser stages
c     hr          - height of perforated riser inlet
c     hrf         - stage at which flow through porous rockfill begins
c     hrh         - height of the riser inlet above the barrel inlet
c     hrs         - stage of the riser inlet
c     hs          - height of the slotted portion of a perforated riser
c     htf(i,j)    - transition stage at which flow regime j begins for outflow function i
c     htmp        - temporary variable for stage
c     icv         - type of culvert
c     idcul       - flag for culvert #1 or culvert #2
c     ids         - type of drop spillway
c     ies         - emergency spillway indicator
c     iff         - filter fence or straw bale indicator
c     ipr         - perforated riser indicator
c     ir          - initial flow regime dummy variable
c     irf         - porous rock fill indicator
c     is          - dummy do loop variable
c     isztmp      - temporary variable for isize
c     itf         - dummy variable
c     itr         - flow regime indicator
c     kb          - bend loss coefficient
c     kc          - friction loss coefficient
c     ke          - entrance loss coefficient
c     ko          - orifice loss coefficient
c     kus         - constant used in culvert outflow function
c     l0tmp       - temporary variable for length power function variable
c     la0         - large aggregate 0% particle size diameters
c     la100       - large aggregate 100% particle size diameters
c     lbl         - length of the barrel
c     lcv         - length of culvert
c     length(i)   - length corresponding to hal(i)
c     ltrns(i)    - length at hal(i) transformed by the length at hmin
c     lenrs       - length of the riser opening for a rectangular riser
c     les1        - length of approach section of the emergency spilmway
c     les2        - length of the second section of the emergency spillway
c     lnrf        - length of porous rock fill
c     lout(i)     - coefficients for the stage-length power function as
c                   returned from SR POWER
c     mus         - constant used in culvert outflow function
c     nalpts      - number of stage-area-length points
c     ncv         - number of identical culverts
c     ndvtmp      - temporary variable for ndiv
c     nfr(i)      - number of flow regimes for outflow function i
c     npts        - number of user specified stage-discharge points
c     qb          - flow through bottom orifice of perforated riser
c     qes(i)      - emergency spillway outflow corresponding to hest(i)
c     qesi        - initial flow for emergency spillway stage-discharge relationship
c     qesii       - incremental flow for emergency spillway stage-discharge relationship
c     qnftmp      - temporary variable for infiltration variable
c     qo11        - emergency spillway flow used to check for control section
c     qpr(i)      - array of perforated riser flow rates
c     qs          - flow through slots of perforated riser
c     sbl         - slope of the barrel
c     scv         - slope of culvert
c     sa0         - small aggregate 0% particle size diameter
c     sa100       - small aggregate 100% particle size diameter
c     sd0         - sand 0% particle size diameter
c     sd100       - sand 100% particle size diameter
c     sl0         - silt 0% particle size diameter
c     sl100       - silt 100% particle size diameter
c     sg(i)       - specific gravity for size subclass i
c     ses1        - slope of emergency spillway approach
c     ses2        - slope of second emergency spillway section
c     ses3        - slope of exit emergency spillway section
c     strcnt      - counter for number of structures on an impoundment
c     strdes      - brief description of impoundment structure
c     vsl         - slurry flow rate for filter fence and straw bales
c     wdbl        - width of rectangular cross section barrel
c     wdff        - width of filter fence
c     wdrf        - width of rockfill
c     widrs       - width of rectangular cross section riser
c     y           - flow depth
c     yci         - initial critical depth
c     ynes        - test depth for emergency spillway control section
c     ys          - constant used in culvert outflow function
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     eatcom
c     imppol
c     imppow
c     imppro
c     impris
c
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     open hydraulic file
c
      open (unit=50,form='unformatted')
c
c     open auxiliary files if only one impoundment and
c     impoundment output requested
c
      if (npond.eq.1.and.ipdout.eq.1) then
        open (unit=54,file='sedout',status='unknown')
        open (unit=55,file='hydout',status='unknown')
        open (unit=56,file='sediment',status='unknown')
        open (unit=57,file='hydraulc',status='unknown')
      end if
c
      if (ipdout.eq.1) write (51,1200)
      if (ipdout.eq.1) write (51,1300)
c
c     initialize impoundment common block (cim*.inc) variables
c
c     initialize daily output array variables
c
      do 50 i = 1, mximp
c
        vi(i) = 0.0
        qiin(i) = 0.0
        qomx(i) = 0.0
        hmax(i) = 0.0
        totin(i) = 0.0
        clin(i) = 0.0
        slin(i) = 0.0
        sain(i) = 0.0
        sdin(i) = 0.0
        lain(i) = 0.0
        cout(i) = 0.0
        clout(i) = 0.0
        slout(i) = 0.0
        saout(i) = 0.0
        sdout(i) = 0.0
        laout(i) = 0.0
        ret(i) = 0.0
        clret(i) = 0.0
        slret(i) = 0.0
        saret(i) = 0.0
        sdret(i) = 0.0
        laret(i) = 0.0
        cpeak(i) = 0.0
        ca(i) = 0.0
        volo(i) = 0.0
        ciin(i) = 0.0
        pla(i) = 0.0
        psa(i) = 0.0
        psl(i) = 0.0
        pcl(i) = 0.0
        psd(i) = 0.0
        totco(i) = 0.0
c
c       initialize monthly output array variables
c
        vim(i) = 0.0
        qiinm(i) = 0.0
        qomxm(i) = 0.0
        hmaxm(i) = 0.0
        totinm(i) = 0.0
        clinm(i) = 0.0
        slinm(i) = 0.0
        sainm(i) = 0.0
        sdinm(i) = 0.0
        lainm(i) = 0.0
        coutm(i) = 0.0
        cloutm(i) = 0.0
        sloutm(i) = 0.0
        saoutm(i) = 0.0
        sdoutm(i) = 0.0
        laoutm(i) = 0.0
        retm(i) = 0.0
        clretm(i) = 0.0
        slretm(i) = 0.0
        saretm(i) = 0.0
        sdretm(i) = 0.0
        laretm(i) = 0.0
        cpeakm(i) = 0.0
        cpkim(i) = 0.0
        cam(i) = 0.0
        vom(i) = 0.0
        tem(i) = 0.0
        cainm(i) = 0.0
c
c       initialize yearly output array variables
c
        viy(i) = 0.0
        qiiny(i) = 0.0
        qomxy(i) = 0.0
        hmaxy(i) = 0.0
        totiny(i) = 0.0
        cliny(i) = 0.0
        sliny(i) = 0.0
        sainy(i) = 0.0
        sdiny(i) = 0.0
        lainy(i) = 0.0
        couty(i) = 0.0
        clouty(i) = 0.0
        slouty(i) = 0.0
        saouty(i) = 0.0
        sdouty(i) = 0.0
        laouty(i) = 0.0
        rety(i) = 0.0
        clrety(i) = 0.0
        slrety(i) = 0.0
        sarety(i) = 0.0
        sdrety(i) = 0.0
        larety(i) = 0.0
        cpeaky(i) = 0.0
        cpkiy(i) = 0.0
        cay(i) = 0.0
        voy(i) = 0.0
        tey(i) = 0.0
        cainy(i) = 0.0
c
c       initialize end of simulation output array variables
c
        vie(i) = 0.0
        qiine(i) = 0.0
        qomxe(i) = 0.0
        hmaxe(i) = 0.0
        totine(i) = 0.0
        cline(i) = 0.0
        sline(i) = 0.0
        saine(i) = 0.0
        sdine(i) = 0.0
        laine(i) = 0.0
        coute(i) = 0.0
        cloute(i) = 0.0
        sloute(i) = 0.0
        saoute(i) = 0.0
        sdoute(i) = 0.0
        laoute(i) = 0.0
        rete(i) = 0.0
        clrete(i) = 0.0
        slrete(i) = 0.0
        sarete(i) = 0.0
        sdrete(i) = 0.0
        larete(i) = 0.0
        cpeake(i) = 0.0
        cpkie(i) = 0.0
        cae(i) = 0.0
        voe(i) = 0.0
        tee(i) = 0.0
        caine(i) = 0.0
        rett(i) = 0.0
        coutt(i) = 0.0
        totint(i) = 0.0
c
c       initialize hydraulic outflow function array variables
c
        do 20 j = 1, mxstc
c
          do 10 k = 1, mxrgm
            a(j,k,i) = 0.0
            b(j,k,i) = 0.0
            c(j,k,i) = 0.0
            d(j,k,i) = 0.0
            e(j,k,i) = 0.0
            ha(j,k,i) = 0.0
   10     continue
c
   20   continue
c
        do 30 j = 1, mxrgm
          ht(j,i) = 0.0
          hlm(j,i) = 0.0
   30   continue
c
        a0(i) = 0.0
        a1(i) = 0.0
        a2(i) = 0.0
        l0(i) = 0.0
        l1(i) = 0.0
        l2(i) = 0.0
        qinf(i) = 0.0
        isize(i) = 0
        hot(i) = 0.0
        hfull(i) = 0.0
        nt(i) = 0
c
c       initialize numerical integration routine array variables
c
        qo(i) = 0.0
        qon(i) = 0.0
        h(i) = 0.0
        hn(i) = 0.0
        t(i) = 0.0
        tn(i) = 0.0
        dttry(i) = 0.0
        dtdid(i) = 0.0
        dtnext(i) = 0.0
        deltat(i) = 0.0
c
c       initialize sediment concentration array variables
c
        do 40 j = 1, mxsub
          d0(j,i) = 0.0
          di(j,i) = 0.0
          d100(j,i) = 0.0
          sg(j,i) = 0.0
          hset(j,i) = 0.0
          ctd(j,i) = 0.0
          cdp(j,i) = 0.0
          co(j,i) = 0.0
          vs(j,i) = 0.0
   40   continue
c
        ndiv(i) = 0
        hmin(i) = 0.0
        cot(i) = 0.0
c
c       initialize overtopping and impoundment fill flag variables
c
        filday(i) = 0
        filflg(i) = 0
        filyr(i) = 0
        otflg(i) = 0
        otyr(i) = 0
        oteos(i) = 0
c
c       initialize time of concentration variable
c
        tcf(i) = 0.0
c
   50 continue
c
c     initialize miscellaneous non-arry variables contained
c     in the impoundment cim*.inc common blocks
c
      cl50 = 0.0
      sl50 = 0.0
      sd50 = 0.0
      sa50 = 0.0
      la50 = 0.0
c
      tcf(0) = 0.0
c
      bwes = 0.0
      sses = 0.0
      nes = 0.0
      mdwit = 0
      dwt = 0.0
      rfrac = 0.0
      yc = 0.0
      qesr = 0.0
      ncross = 0
c
      do 60 i = 1, 30
        pypos(i) = 0.0
        zes(i) = 0.0
        depth(i) = 0.0
   60 continue
c       added by S.Dun  March 16,1999 
c       initialize variable for automatic cleanning
        do 65 i=1,mximp
          alnum(i)=0
          do 70 j=1,100
            alvol(i,j)=0.0
            aldate(i,j)=0
            alyear(i,j)=0
70        continue            
65      continue
c       initialize variable for sediment stage at rearrangement time.
        sstage=0.0
c       end adding
c
c     beginning of multiple impoundment loop
c
      do 390 ipond = 1, npond
c
c       open hydraulic/sediment input file(s)
c
c       open (unit=ipond+9,file=filein(ipond),status='old')
c
c       initialize outflow function and stage variables
c
        do 80 i = 1, 15
          nfr(i) = 1
c
          do 85 j = 1, 10
            af(i,j) = 0.0
            bf(i,j) = 0.0
            cf(i,j) = 0.0
            df(i,j) = 0.0
            ef(i,j) = 0.0
            haf(i,j) = 0.0
            hlf(i,j) = 0.0
            htf(i,j) = 0.0
   85     continue
c
   80   continue
c
c       initialize counter for number of impoundment structures
c
        strcnt = 0
c
c       read three line description of impoundment file
c
        do 90 i = 1, 3
          read (20,1000) impdes(i)
   90   continue
c
        if (ipdout.eq.1) write (51,1400) ipond
c
c       write three line description of impoundment file
c
        do 100 i = 1, 3
          if (ipdout.eq.1) write (51,1100) impdes(i)
  100   continue
c
c       drop spillway  (outflow functions #1 = weir flow,
c       #2 = orifice flow, #3 = pipe flow)
c
        read (20,*) ids
c
c       added by S.Dun  March 16,1999
        fids(ipond)=ids
c       end adding
c
c       setting outflow functions for no flow condition
c       (flow regime #1)
c
        cf(1,1) = 1.0
        cf(2,1) = 1.0
        cf(3,1) = 1.0
c       added by S.Dun April 19,1999
        bf(3,1)=10.0**8
c       end adding
c
        haf(3,1) = -200.0
c
        if (ids.ne.0) then
c
          if (ids.eq.1) then
c
            strcnt = strcnt + 1
c
c           setting outflow functions for flow in drop spillway with
c           circular riser and barrel (flow regime #2)
c
            read (20,1000) strdes
            read (20,*) diars, hrs, coefw, coefo
            read (20,*) diabl, hrh, lbl, sbl, hblot
            read (20,*) ke, kb, kc
c
            if (ipdout.eq.1) write (51,1500) strcnt
            if (ipdout.eq.1) write (51,1600) strdes
            if (ipdout.eq.1) write (51,1700) diars, hrs, coefw, coefo,
     1          diabl, hrh, lbl, sbl, hblot, ke, kb, kc
c
            diars = diars * 3.281
            hrs = hrs * 3.281
            diabl = diabl * 3.281
            hrh = hrh * 3.281
            lbl = lbl * 3.281
            hblot = hblot * 3.281
c
            af(1,2) = 1.0
            af(2,2) = 1.0
            af(3,2) = hblot + 0.6 * diabl
c
            bf(1,2) = coefw * 22.0 / 7.0 * diars
            bf(2,2) = coefo * 22.0 / 7.0 * diars ** 2.0 / 4.0 * (2*32.2)
     1          ** 0.5
            bf(3,2) = 22.0 / 7.0 * diabl ** 2.0 / 4.0 * (2*32.2) ** 0.5
     1          / (1.0+ke+kb+kc*(lbl+hrh)) ** 0.5
c
            cf(1,2) = 1.5
            cf(2,2) = 0.5
            cf(3,2) = 0.5
c
            haf(1,2) = hrs
            haf(2,2) = hrs
            haf(3,2) = hrs - (hrh+sbl*lbl-0.6*diabl)
c
          else if (ids.eq.2) then
c
            strcnt = strcnt + 1
c
c           setting outflow functions for flow in drop spillway with
c           rectangular riser and circular barrel (flow regime #2)
c
            read (20,1000) strdes
            read (20,*) lenrs, widrs, hrs, coefw, coefo
            read (20,*) diabl, hrh, lbl, sbl, hblot
            read (20,*) ke, kb, kc
c
            if (ipdout.eq.1) write (51,1500) strcnt
            if (ipdout.eq.1) write (51,1600) strdes
            if (ipdout.eq.1) write (51,1800) lenrs, widrs, hrs, coefw,
     1          coefo, diabl, hrh, lbl, sbl, hblot, ke, kb, kc
c
            lenrs = lenrs * 3.281
            widrs = widrs * 3.281
            hrs = hrs * 3.281
            diabl = diabl * 3.281
            hrh = hrh * 3.281
            lbl = lbl * 3.281
            hblot = hblot * 3.281
c
            af(1,2) = 1.0
            af(2,2) = 1.0
            af(3,2) = hblot + 0.6 * diabl
c
            bf(1,2) = coefw * 2.0 * (lenrs+widrs)
            bf(2,2) = coefo * lenrs * widrs * (2*32.2) ** 0.5
            bf(3,2) = 22.0 / 7.0 * diabl ** 2.0 / 4.0 * (2*32.2) ** 0.5
     1          / (1.0+ke+kb+kc*(lbl+hrh)) ** 0.5
c
            cf(1,2) = 1.5
            cf(2,2) = 0.5
            cf(3,2) = 0.5
c
            haf(1,2) = hrs
            haf(2,2) = hrs
            haf(3,2) = hrs - (hrh+sbl*lbl-0.6*diabl)
c
          else if (ids.eq.3) then
c
            strcnt = strcnt + 1
c
c           setting outflow functions for flow in drop spillway with
c           rectangular riser and barrel (flow regime #2)
c
            read (20,1000) strdes
            read (20,*) lenrs, widrs, hrs, coefw, coefo
            read (20,*) hitbl, wdbl, hrh, lbl, sbl, hblot
            read (20,*) ke, kb, kc
c
            if (ipdout.eq.1) write (51,1500) strcnt
            if (ipdout.eq.1) write (51,1600) strdes
            if (ipdout.eq.1) write (51,1900) lenrs, widrs, hrs, coefw,
     1          coefo, hitbl, wdbl, hrh, lbl, sbl, hblot, ke, kb, kc
c
            lenrs = lenrs * 3.281
            widrs = widrs * 3.281
            hrs = hrs * 3.281
            hitbl = hitbl * 3.281
            wdbl = wdbl * 3.281
            hrh = hrh * 3.281
            lbl = lbl * 3.281
            hblot = hblot * 3.281
c
            af(1,2) = 1.0
            af(2,2) = 1.0
            af(3,2) = hblot + 0.6 * hitbl
c
            bf(1,2) = coefw * 2.0 * (lenrs+widrs)
            bf(2,2) = coefo * lenrs * widrs * (2*32.2) ** 0.5
            bf(3,2) = hitbl * wdbl * (2*32.2) ** 0.5 / (1.0+ke+kb+kc*(
     1          lbl+hrh)) ** 0.5
c
            cf(1,2) = 1.5
            cf(2,2) = 0.5
            cf(3,2) = 0.5
c
            haf(1,2) = hrs
            haf(2,2) = hrs
            haf(3,2) = hrs - (hrh+sbl*lbl-0.6*hitbl)
c
          end if
c
          do 110 i = 1, 3
            hlf(i,2) = haf(i,2)
            htf(i,2) = haf(1,2)
c       added by S.Dun April 19,1999
            if(hlf(i,2).gt.htf(i,2)) htf(i,2)=hlf(i,2)
c       end adding
            nfr(i) = 2
  110     continue
c
c       added by S.Dun April 20,1999
c       initial variables in cimds.inc.
                dids(ipond)=ids
                dhrs(ipond)=hrs
                dcoefw(ipond)=coefw
                dcoefo(ipond)=coefo
                dhrh(ipond)=hrh
                dlbl(ipond)=lbl
                dsbl(ipond)=sbl
                dhblot(ipond)=hblot
                dke(ipond)=ke
                dkb(ipond)=kb
                dkc(ipond)=kc
                if(ids.eq.1) then
                  ddiars(ipond)=diars
                  ddiabl(ipond)=diabl
                endif
                if(ids.eq.2) then
                  dlenrs(ipond)=lenrs
                  dwidrs(ipond)=widrs
                  ddiabl(ipond)=diabl
                endif
                if(ids.eq.3) then
                  dlenrs(ipond)=lenrs
                  dwidrs(ipond)=widrs
                  dhitbl(ipond)=hitbl
                  dwdbl(ipond)=wdbl
                endif
c          end adding
c
        end if
c
c       culvert #1 (outflow functions #4 = inlet control unsubmerged,
c       #5 = inlet control submerged, #6 = pipe flow)
c
        read (20,*) icv, ncv
c
c       added by S.Dun  March 16,1999
        fcv1(ipond)=icv
c       end adding
c
c       setting outflow functions for no flow condition
c       (flow regime #1)
c
c       added 7/5/95 from M. Linley's suggestions
        af(5,1) = 10.0**8
c
        bf(4,1) = 1.0
        bf(5,1) = 1.0
c       added by S.Dun  March 16,1999
          bf(6,1)=10.0**8
c       end adding
c
        cf(4,1) = 1.0
c       added 7/5/95 from M. Linley's suggestions
        cf(5,1) = 10.0**8
        cf(6,1) = 1.0
c
        df(5,1) = 1.0
c
        haf(6,1) = -200.0
c
        if (icv.ge.1) then
c
          idcul = 1
          strcnt = strcnt + 1
c
c         setting outflow functions for flow in culvert (flow regime #2)
c
          read (20,1000) strdes
          read (20,*) arcv, hitcv, hcv, lcv, scv, hcvot
          read (20,*) ke, kb, kc, kus, mus, cs, ys
c
          if (ipdout.eq.1) write (51,1500) strcnt
          if (ipdout.eq.1) write (51,1600) strdes
          if (ipdout.eq.1) write (51,2000) idcul, ncv, arcv, hitcv, hcv,
     1        lcv, scv, hcvot, ke, kb, kc, kus, mus, cs, ys
c
          arcv = arcv * 3.281 ** 2.0
          hitcv = hitcv * 3.281
          hcv = hcv * 3.281
          lcv = lcv * 3.281
          hcvot = hcvot * 3.281
c       added by S.Dun March 16,1999
c       initial variables in cimcv1.inc
                c1icv(ipond)=icv
                c1ncv(ipond)=ncv
                c1ar(ipond)=arcv
                c1h(ipond)=hcv
                c1hit(ipond)=hitcv
                c1l(ipond)=lcv
                c1s(ipond)=scv
                c1hot(ipond)=hcvot
                c1ke(ipond)=ke
                c1kb(ipond)=kb
                c1kc(ipond)=kc
                c1kus(ipond)=kus
                c1mus(ipond)=mus
                c1cs(ipond)=cs
                c1ys(ipond)=ys
c       end adding
c
          nfr(4) = 2
          nfr(5) = 2
          nfr(6) = 2
c
          af(4,2) = arcv * hitcv ** 0.5 * float(ncv)
          af(5,2) = arcv * hitcv ** 0.5 * float(ncv)
          af(6,2) = hcvot + 0.6 * hitcv
c
          bf(4,2) = hitcv * kus
          bf(5,2) = hitcv
          bf(6,2) = arcv * (2.*32.2) ** 0.5 * float(ncv) / (1.0+ke+kb+kc
     1        *lcv) ** 0.5
c
          cf(4,2) = 1 / mus
          cf(5,2) = 0.5 * scv - ys
          cf(6,2) = 0.5
c
          df(5,2) = cs
c
          haf(4,2) = hcv
          haf(5,2) = hcv
          haf(6,2) = hcv - scv * lcv + 0.6 * hitcv
c
          hlf(4,2) = hcv
          hlf(5,2) = (haf(5,2)-cf(5,2)*bf(5,2)) + 0.0001
          hlf(6,2) = haf(6,2)
c
          htf(4,2) = hcv
c          htf(5,2) = haf(5,2) - cf(5,2) * bf(5,2) + 0.0001
          htf (5,2) = hcv + hitcv - 0.0001
c       added by DSH 1/11/99
                if(hlf(5,2).gt.htf(5,2)) htf(5,2)=hlf(5,2)
c       end adding
          htf(6,2) = hcv
c       added by DSH 1/11/99
          if (hlf(6,2).gt.htf(6,2)) htf(6,2)=hlf(6,2)
c       end adding
c
        end if
c
c       culvert #2 (outflow functions #7 = inlet control unsubmerged,
c       #8 = inlet control submerged, #9 = pipe flow)
c
        read (20,*) icv, ncv
c
c       added by S.Dun  March 16,1999
        fcv2(ipond)=icv
c       end adding
c
c       setting outflow functions for no flow condition
c       (flow regime #1)
c
        af(8,1) = 10.0**8
c
        bf(7,1) = 1.0
        bf(8,1) = 1.0
c       added by S.Dun March 19,1999
          bf(9,1)=10.0**8
c       end adding 
c
        cf(7,1) = 1.0
        cf(8,1) = 10.0**8
        cf(9,1) = 1.0
c
        df(8,1) = 1.0
c
        haf(9,1) = -100.0
c
        if (icv.ge.1) then
c
          idcul = 2
          strcnt = strcnt + 1
c
c         setting outflow functions for flow in culvert (flow regime #2)
c
          read (20,1000) strdes
          read (20,*) arcv, hitcv, hcv, lcv, scv, hcvot
          read (20,*) ke, kb, kc, kus, mus, cs, ys
c
          if (ipdout.eq.1) write (51,1500) strcnt
          if (ipdout.eq.1) write (51,1600) strdes
          if (ipdout.eq.1) write (51,2000) idcul, ncv, arcv, hitcv, hcv,
     1        lcv, scv, hcvot, ke, kb, kc, kus, mus, cs, ys
c
          arcv = arcv * 3.281 ** 2.0
          hitcv = hitcv * 3.281
          hcv = hcv * 3.281
          lcv = lcv * 3.281
          hcvot = hcvot * 3.281
c       added by S.Dun March 19,1999
c       initial variables in cimcv2.inc
                c2icv(ipond)=icv
                c2ncv(ipond)=ncv
                c2ar(ipond)=arcv
                c2h(ipond)=hcv
                c2hit(ipond)=hitcv
                c2l(ipond)=lcv
                c2s(ipond)=scv
                c2hot(ipond)=hcvot
                c2ke(ipond)=ke
                c2kb(ipond)=kb
                c2kc(ipond)=kc
                c2kus(ipond)=kus
                c2mus(ipond)=mus
                c2cs(ipond)=cs
                c2ys(ipond)=ys
c       end adding
c
          nfr(7) = 2
          nfr(8) = 2
          nfr(9) = 2
c
          af(7,2) = arcv * hitcv ** 0.5 * float(ncv)
          af(8,2) = arcv * hitcv ** 0.5 * float(ncv)
          af(9,2) = hcvot + 0.6 * hitcv
c
          bf(7,2) = hitcv * kus
          bf(8,2) = hitcv
          bf(9,2) = arcv * (2.*32.2) ** 0.5 * float(ncv) / (1.0+ke+kb+kc
     1        *lcv) ** 0.5
c
          cf(7,2) = 1 / mus
          cf(8,2) = 0.5 * scv - ys
          cf(9,2) = 0.5
c
          df(8,2) = cs
c
          haf(7,2) = hcv
          haf(8,2) = hcv
          haf(9,2) = hcv - scv * lcv + 0.6 * hitcv
c
          hlf(7,2) = hcv
          hlf(8,2) = (haf(8,2)-cf(8,2)*bf(8,2)) + 0.0001
          hlf(9,2) = haf(9,2)
c
          htf(7,2) = hcv
c          htf(8,2) = haf(8,2) - cf(8,2) * bf(8,2) + 0.0001
          htf(8,2) = hcv + hitcv - 0.0001
          htf(9,2) = hcv
c       added by S.Dun
                if(hlf(8,2).gt.htf(8,2)) htf(8,2)=hlf(8,2)
                if(hlf(9,2).gt.htf(9,2)) htf(9,2)=hlf(9,2)
c       end adding
c
        end if
c
c       porous rockfill (outflow function #10)
c
        read (20,*) irf
c       added by S.Dun  March 16,1999
        firf(ipond)=irf
c       end adding
c
c       setting outflow function for no flow condition
c       (flow regime #1)
c
        bf(10,1) = 1.0
        cf(10,1) = 1.0
c
        if (irf.ne.0) then
c
          strcnt = strcnt + 1
c
          read (20,1000) strdes
          read (20,*) lnrf, hrf, hotrf, wdrf, diarf
c
          if (ipdout.eq.1) write (51,1500) strcnt
          if (ipdout.eq.1) write (51,1600) strdes
          if (ipdout.eq.1) write (51,2100) lnrf, hrf, hotrf, wdrf, diarf
c
c         setting outflow functions for flow through the porous media
c         (flow regime #2) and for flow overtopping porous media
c         (flow regime #3)
c
          if (lnrf.lt.0.5) then
            arf1 = 3.041846 * diarf ** (-0.34677)
            arf2 = 1.910413 * diarf ** (-0.34935)
            arf = arf1 - (arf2-arf1) / 0.5 * (0.5-lnrf)
c
          else if (lnrf.lt.1.0) then
c
            arf1 = 3.041846 * diarf ** (-0.34677)
            arf2 = 1.910413 * diarf ** (-0.34935)
            arf = arf1 + (arf2-arf1) / 0.5 * (lnrf-0.5)
c
          else if (lnrf.lt.2.0) then
c
            arf1 = 1.910413 * diarf ** (-0.34935)
            arf2 = 1.19637 * diarf ** (-0.35422)
            arf = arf1 + (arf2-arf1) * (lnrf-1.0)
c
          else if (lnrf.lt.3.0) then
c
            arf1 = 1.19637 * diarf ** (-0.35422)
            arf2 = 0.909902 * diarf ** (-0.35705)
            arf = arf1 + (arf2-arf1) * (lnrf-2.0)
c
          else
c
            arf1 = 1.19637 * diarf ** (-0.35422)
            arf2 = 0.909902 * diarf ** (-0.35705)
            arf = arf2 + (arf2-arf1) * (lnrf-3.0)
          end if
c
          brf = 1.0 / (1.5005609-0.00013171905*log(diarf)/diarf)
c       added by S.Dun  March 31,1999
c       initial variables in cimrf.inc
                rirf(ipond)=irf
                rlnrf(ipond)=lnrf
                rhrf(ipond)=hrf
                rhotrf(ipond)=hotrf
                rwdrf(ipond)=wdrf
                rarf(ipond)=arf
                rbrf(ipond)=brf
c       end adding
c
          haf(10,2) = hrf * 3.281
          haf(10,3) = haf(10,2)
c
          htf(10,2) = hrf * 3.281
          htf(10,3) = hotrf * 3.281
c
          hlf(10,2) = haf(10,2)
          hlf(10,3) = htf(10,3)
c
          af(10,2) = wdrf * 3.281 ** 3.0
          af(10,3) = af(10,2)
c
          bf(10,2) = lnrf * arf * 3.281
          bf(10,3) = bf(10,2)
c
          cf(10,2) = 1 / brf
          cf(10,3) = cf(10,2)
c
          df(10,3) = 3.087 * wdrf * 3.281
c
          ef(10,3) = htf(10,3)
c
          nfr(10) = 3
c
        end if
c
c       emergency spillway (outflow function #11)
c       4th degree polynomial utilized for open channel outlets and
c       user defined stage-discharge relationships
c
        mdwit = 1500
        dwt = 0.1
        rfrac = 0.005
c
        read (20,*) ies
c       added by S.Dun  March 16,1999
        fies(ipond)=ies
c       end adding
        if (ies.eq.1) then
c
          strcnt = strcnt + 1
c
c         open channel outlet
c
          read (20,1000) strdes
          read (20,*) bwes, sses, nes, hes, hmxes
          read (20,*) ses1, les1, ses2, les2, ses3
c
          if (ipdout.eq.1) write (51,1500) strcnt
          if (ipdout.eq.1) write (51,1600) strdes
          if (ipdout.eq.1) write (51,2200) bwes, sses, nes, hes, hmxes,
     1        ses1, les1, ses2, les2, ses3
c
          bwes = bwes * 3.281
          hes = hes * 3.281
          hmxes = hmxes * 3.281
          les1 = les1 * 3.281
          les2 = les2 * 3.281
c
c         checking for control section using critical depth
c
          ynes = ((10.0*nes)/(1.49*ses3**0.5)) ** (3.0/5.0)
c
          if (ynes.lt.1.459756) then
c
c           control section exists
c
            ncross = 11
            pypos(1) = hes - ses1 * les1 - ses2 * les2
            zes(1) = 0.0
c
            do 120 i = 2, 6
              zes(i) = zes(i-1) + les1 / 5.0
              pypos(i) = pypos(i-1) + ses1 * les1 / 5.0
  120       continue
c
            do 130 i = 7, 11
              zes(i) = zes(i-1) + les2 / 5.0
              pypos(i) = pypos(i-1) + ses2 * les2 / 5.0
  130       continue
c
            yci = 0.1
            yc = yci
c
  140       i = 1
c
  150       qes(i) = (32.2*(bwes*yc+sses*yc**2.0)/(bwes+2.0*sses*yc)) **
     1          0.5 * (bwes*yc+sses*yc**2.0)
c
            qesr = qes(i)
c
c           run routing routine from control section with yc and qesr
c
            call imppro
c
            cores = (qes(i)/(bwes*depth(1)+sses*depth(1)**2)) ** 2 / (2.
     1          *32.2)
            hest(i) = depth(1) + cores + pypos(1) - hes
c
            if (hest(i).gt.(hmxes-hes)) then
c
              if (i.gt.19) then
c
c               run polynomial regression routines on hest and qes
c
                call imppol(i,hest,qes,aes)
c
              else
c
                yci = 0.5 * yci
                yc = yci
                go to 140
              end if
c
            else
c
              if (i.lt.90) then
                yc = yc + yci
                i = i + 1
                go to 150
              else
                yci = 2.0 * yci
                yc = yci
                go to 140
              end if
c
            end if
c
          else
c
c           control section does not exist: assume open channel and route
c           from 1000 ft downstream
c
            ncross = 16
            pypos(1) = hes - ses1 * les1 - ses2 * les2
            zes(1) = 0.0
c
            do 160 i = 2, 6
              zes(i) = zes(i-1) + les1 / 5.0
              pypos(i) = pypos(i-1) + ses1 * les1 / 5.0
  160       continue
c
            do 170 i = 7, 11
              zes(i) = zes(i-1) + les2 / 5.0
              pypos(i) = pypos(i-1) + ses2 * les2 / 5.0
  170       continue
c
            do 180 i = 12, 16
              zes(i) = zes(i-1) + 200.0
              pypos(i) = pypos(i-1) - ses3 * 200.0
  180       continue
c
            i = 1
            qesi = 2.0
            qes(i) = qesi
c
  190       qesr = qes(i)
c
            yc = ((qesr*nes)/(1.49*ses3)) ** (3.0/5.0)
c
c           run routing routine from 1000 ft downstream with yc and qesr
c
            call imppro
c
            cores = (qes(i)/(bwes*depth(1)+sses*depth(1)**2)) ** 2 / (2.
     1          *32.2)
            hest(i) = depth(1) + cores + pypos(1) - hes
c
            if (hest(i).gt.(hmxes-hes)) then
c
              if (i.gt.19) then
c
c               run polynomial regression routines on hest and qes
c
                call imppol(i,hest,qes,aes)
c
              else
c
                qesii = 0.5 * qesi
                qes(1) = qesii
                i = 1
                go to 190
c
              end if
c
            else
c
              if (i.lt.90) then
                i = i + 1
                qesii = qesii * 1.5
                qes(i) = qes(i-1) + qesii
                go to 190
              else
                qesii = 2.0 * qesi
                qes(1) = qesii
                i = 1
                go to 190
              end if
c
            end if
c
          end if
c
c         setting outflow functions for flow in channel (flow regime #2)
c
          af(11,2) = aes(1)
          bf(11,2) = aes(2)
          cf(11,2) = aes(3)
          df(11,2) = aes(4)
          ef(11,2) = aes(5)
c
          haf(11,2) = hes
          hlf(11,2) = hes
c
          hf = hes
c
  200     qo11 = af(11,2) + bf(11,2) * (hf-haf(11,2)) + cf(11,2) * (hf-
     1        haf(11,2)) ** 2.0 + df(11,2) * (hf-haf(11,2)) ** 3.0 +
     1        ef(11,2) * (hf-haf(11,2)) ** 4.0
c
          if (qo11.ge.0.0) then
            hlf(11,2) = hf
            htf(11,2) = hf
          else
            hf = hf + 0.001
            go to 200
          end if
c
          nfr(11) = 2
c
        else if (ies.eq.2) then
c
          strcnt = strcnt + 1
c
c         user specified stage-discharge relationship
c
          read (20,1000) strdes
          read (20,*) npts
          read (20,*) hes
c
          call eatcom(20)
c
          read (20,*) (hest(i),i = 1,npts)
c
          call eatcom(20)
c
          read (20,*) (qes(i),i = 1,npts)
c
          if (ipdout.eq.1) write (51,1500) strcnt
          if (ipdout.eq.1) write (51,1600) strdes
          if (ipdout.eq.1) write (51,2300) hes
c
          do 210 i = 1, npts
            if (ipdout.eq.1) write (51,2400) hest(i), qes(i)
  210     continue
c
          if (ipdout.eq.1) write (51,2500)
          hes = hes * 3.281
c
          do 220 i = 1, npts
            hest(i) = hest(i) * 3.281 - hes
            qes(i) = qes(i) * 3.281 ** 3
  220     continue
c
c         run polynomial regression routines on hest and qes
c
          call imppol(npts,hest,qes,aes)
c
c         setting outflow functions for flow (flow regime #2)
c
          af(11,2) = aes(1)
          bf(11,2) = aes(2)
          cf(11,2) = aes(3)
          df(11,2) = aes(4)
          ef(11,2) = aes(5)
c
          haf(11,2) = hes
          hlf(11,2) = hes
c
          hf = hes
c
  230     qo11 = af(11,2) + bf(11,2) * (hf-haf(11,2)) + cf(11,2) * (hf-
     1        haf(11,2)) ** 2.0 + df(11,2) * (hf-haf(11,2)) ** 3.0 +
     1        ef(11,2) * (hf-haf(11,2)) ** 4.0
c
          if (qo11.ge.0.0) then
            hlf(11,2) = hf
            htf(11,2) = hf
          else
            hf = hf + 0.001
            go to 230
          end if
c
          nfr(11) = 2
c
        end if
c
c       straw bales, filterfence, or trash barriers
c       (outflow function #12)
c
        read (20,*) iff
c       added by S.Dun  March 16,1999
        fiff(ipond)=iff
c       end adding
        if (iff.ne.0) then
c
          strcnt = strcnt + 1
c
          read (20,1000) strdes
          read (20,*) vsl, wdff, hff, hotff
c
          if (ipdout.eq.1) write (51,1500) strcnt
          if (ipdout.eq.1) write (51,1600) strdes
c
          if (ipdout.eq.1) then
            if (iff.eq.1) write (51,2600)
            if (iff.ne.1) write (51,2700)
          end if
c
          if (ipdout.eq.1) write (51,2800) vsl, wdff, hff, hotff
c
          vsl = vsl * 3.281
          wdff = wdff * 3.281
          hff = hff * 3.281
          hotff = hotff * 3.281
c       added by S.Dun  April 12,1999
c       initial variables in cimff.inc
                ffiff(ipond)=iff
                fvsl(ipond)=vsl
                fwdff(ipond)=wdff
                fhff(ipond)=hff
                fhotff(ipond)=hotff
c       end adding
c
c         setting outflow equation for flow through the filter fence
c         (flow regime #2)
c
          af(12,2) = wdff * vsl
c
          haf(12,2) = hff
          hlf(12,2) = hff
          htf(12,2) = hff
c
c         setting outflow function for flow overtopping filter fence
c         (flow regime #3)
c
          af(12,3) = wdff * vsl
c
          if (iff.eq.1) then
c
c           choosing sharp crested weir for filter fence
c
            bf(12,3) = 3.27 * wdff
            cf(12,3) = 0.4 / (hotff-hff) * wdff
          else
c
c           choosing broad crested weir for straw bales/trash barrier
c
            bf(12,3) = 3.087 * wdff
          end if
c
          df(12,3) = hotff
c
          haf(12,3) = hff
          hlf(12,3) = hotff
          htf(12,3) = hotff
c
          nfr(12) = 3
c
        end if
c
c       perforated riser (outflow functions #13 - flow does not
c       submerge riser, #14 - submerged flow controlled by the
c       perforater riser orifice, #15 - submerged flow controlled
c       by barrel
c
        read (20,*) ipr
c       added by S.Dun  March 16,1999
        fipr(ipond)=ipr
c       end adding
c
c       setting outflow functions for no flow condition
c       (flow regime #1)
c
        bf(13,1) = 1.0
c
        cf(13,1) = 1.0
        cf(15,1) = 1.0
c
        haf(13,1) = -0.01
c
        if (ipr.ne.0) then
c
          strcnt = strcnt + 1
c
          read (20,1000) strdes
          read (20,*) hr, hb, hs, hd, diar, as, diab
          read (20,*) hrh, lbl, sbl, diabl
          read (20,*) cb, coefw, coefo, cs
          read (20,*) ke, kb, kc
c
          if (ipdout.eq.1) write (51,1500) strcnt
          if (ipdout.eq.1) write (51,1600) strdes
          if (ipdout.eq.1) write (51,2900) hr, hb, hs, hd, diar, as,
     1        diab, hrh, lbl, sbl, diabl, cb, coefw, coefo, cs, ke, kb,
     1        kc
c
          hr = hr * 3.281
          hb = hb * 3.281
          hd = hd * 3.281
          hs = hs * 3.281
          as = as * 3.281 ** 2
          diab = diab * 3.281
          diar = diar * 3.281
          hrh = hrh * 3.281
          lbl = lbl * 3.281
          diabl = diabl * 3.281
          ko = exp(-0.60721+0.329229*diab/diar)
          ab = 22.0 / 7.0 * diab ** 2 / 4.0
c
c       added by S.Dun May 7,1999
c       initial variables in cimpr.inc
c
                pipr(ipond)=ipr
                phr(ipond)=hr
                phb(ipond)=hb
                phs(ipond)=hs
                phd(ipond)=hd
                pdiar(ipond)=diar
                pas(ipond)=as
                pdiab(ipond)=diab
                phrh(ipond)=hrh
                plbl(ipond)=lbl
                psbl(ipond)=sbl
                pdiabl(ipond)=diabl
                pcb(ipond)=cb
                pcoefw(ipond)=coefw
                pcoefo(ipond)=coefo
                pcs(ipond)=cs
                pke(ipond)=ke
                pkb(ipond)=kb
                pkc(ipond)=kc
c         end adding
c
c         determining stage discharge relationship for unsubmerged
c         flow in riser
c
          i = 1
          hpdel = 0.05
          hp = hpdel
          y = -hb
c
  240     qb = cb * ab * (64.4*(hb+y)) ** 0.5
c
          if (i.ge.100) then
            i = 1
            hpdel = hpdel * 2.0
            hp = hpdel
            y = -hb
            go to 240
          end if
c
          if (hp.lt.hs) then
c
            if (y.le.0.0) then
              qs = 2.0 / 3.0 * (cs*as/hs) * 64.4 ** 0.5 * hp ** 1.5
            else
              qs = (cs*as/hs) * 64.4 ** 0.5 * (y*(hp-y)**0.5+(2./3.)*(hp
     1            -y)**1.5)
            end if
c
          else
c
c       modified by S.Dun May 16,1999
c            if (hp.le.hr) then
                if(hp.le.(hr-hd)) then
c       end modifying
c
              if (y.le.0.0) then
                qs = (2./3.) * (cs*as/hs) * 64.4 ** 0.5 * (hp**1.5-(hp-
     1              hs)**1.5)
c
              else if (y.le.hs) then
c
                qs = (cs*as/hs) * 64.4 ** 0.5 * (y*(hp-y)**0.5+(2./3.)*(
     1              (hp-y)**1.5-(hp-hs)**1.5))
              else
                qs = (cs*as) * (64.4*(hp-y)) ** 0.5
              end if
c
            else
c        modified by S.Dun May 16,1999
c              qw = coefw * 22.0 / 7.0 * diar * (hp-hr) ** 1.5
c              qo(ipond) = coefo * 22.0 / 7.0 * diar ** 2 / 4.0 * (hp-hr)
c     1            ** 0.5
c
              qw = coefw * 22.0 / 7.0 * diar * (hp-(hr-hd)) ** 1.5
              qo(ipond) = coefo * 22.0 / 7.0 * diar ** 2 / 4.0 * 
     1            (hp-(hr-hd))** 0.5
c       end modifying
c
              if (y.le.0.0) then
                qs = (2./3.) * (cs*as/hs) * 64.4 ** 0.5 * (hp**1.5-(hp-
     1              hs)**1.5) + min(qw,qo(ipond))
c
              else if (y.le.hs) then
c
                qs = (cs*as/hs) * 64.4 ** 0.5 * (y*(hp-y)**0.5+(2./3.)*(
     1              (hp-y)**1.5-(hp-hs)**1.5)) + min(qw,qo(ipond))
              else
                qs = (cs*as) * (64.4*(hp-y)) ** 0.5 +
     1              min(qw,qo(ipond))
              end if
c
            end if
c
          end if
c
          if (qb.lt.qs) then
            y = y + 0.0001
c
            if (y.ge.hp) then
              qpr(i) = qb
              hpr(i) = hp
              i = i + 1
              hp = hp + hpdel
              go to 240
c
            else if (y.gt.(hr-hd)) then
c
              i = i - 1
              go to 250
            end if
c
            go to 240
c
          else
c
c            qpr(i) = (qb+qs) / 2.
                  qpr(i)=qs
            hpr(i) = hp
            i = i + 1
            hp = hp + hpdel
            go to 240
          end if
c
c         run regression routine on hpr and qpr to determine function
c         coefficients
c
  250     call impris(i,hpr,qpr,apr)
c
          af(13,2) = 1.0
          af(14,2) = cb * ab * 64.4 ** 0.5
c
          bf(13,2) = apr(1)
          bf(15,2) = 22.0 / 7.0 * diabl ** 2.0 / 4.0 * (2*32.2) ** 0.5 /
     1        (1.0+ke+kb+kc*lbl+ko)
c
          cf(13,2) = apr(2)
          cf(15,2) = 0.5
c
          haf(13,2) = hd
          haf(14,2) = hd-hb
          haf(15,2) = hr - (hrh+sbl*lbl-0.6*diabl)
c
          hlf(13,2) = hd
          hlf(14,2) = hd-hb
          hlf(15,2) = haf(15,2)
c
          htf(13,2) = hd
          htf(14,2) = hd
          htf(15,2) = hd
c
c         setting outflow functions for flow through a submerged riser
c         (flow regime #3)
c
          af(13,3) = 10000000.0
          bf(13,3) = 1.0
          cf(13,3) = 1.0
c
c          htf(13,3) = hpr(i)
          htf(13,3)=hpr(i)+hd

c
          nfr(13) = 3
          nfr(14) = 2
          nfr(15) = 2
c
        end if
c
c       ordering transition stages to define outflow functions #1 - #15
c       for the entire range of stages
c
        read (20,*) hottmp, hfltmp, htmp, dltimp, qnftmp
        read (20,*) isztmp, ndvtmp
c
        hot(ipond) = hottmp
        hfull(ipond) = hfltmp
        h(ipond) = htmp
        deltat(ipond) = dltimp
        qinf(ipond) = qnftmp
        isize(ipond) = isztmp
        ndiv(ipond) = ndvtmp
c
        if (ipdout.eq.1) write (51,3000) hot(ipond), hfull(ipond),
     1      h(ipond), deltat(ipond), qinf(ipond), isize(ipond),
     1      ndiv(ipond)
c
        qinf(ipond) = qinf(ipond) * 3.281
        hot(ipond) = hot(ipond) * 3.281
c       added by S.Dun  March 16,1999
          hfull(ipond)=hfull(ipond)*3.281
c       end adding
        h(ipond) = h(ipond) * 3.281
        itr = 1
c
        do 260 i = 1, 15
          ir(i) = 1
  260   continue
c
        ht(itr,ipond) = 0.0
c
        do 270 is = 1, 15
          af(is,(nfr(is)+1)) = af(is,nfr(is))
          bf(is,(nfr(is)+1)) = bf(is,nfr(is))
          cf(is,(nfr(is)+1)) = cf(is,nfr(is))
          df(is,(nfr(is)+1)) = df(is,nfr(is))
          ef(is,(nfr(is)+1)) = ef(is,nfr(is))
          htf(is,(nfr(is)+1)) = htf(is,nfr(is))
          haf(is,(nfr(is)+1)) = haf(is,nfr(is))
          hlf(is,(nfr(is)+1)) = hlf(is,nfr(is))
  270   continue
c
c       added by S.Dun March 16,1999
        do 400 i = 1,15
        do 410 j = 1,10
          daf(i,j,ipond)=af(i,j)
          dbf(i,j,ipond)=bf(i,j)
          dcf(i,j,ipond)=cf(i,j)
          ddf(i,j,ipond)=df(i,j)
          def(i,j,ipond)=ef(i,j)
          dhaf(i,j,ipond)=haf(i,j)
          dhtf(i,j,ipond)=htf(i,j)
          dhlf(i,j,ipond)=hlf(i,j)
410     continue
          dnfr(i,ipond)=nfr(i)
400     continue
c       end adding 
c
  280   itf = 0
c
        do 290 is = 1, 15
c
          if (ht(itr,ipond).ge.htf(is,ir(is))) then
              a(is,itr,ipond) = af(is,ir(is))
            b(is,itr,ipond) = bf(is,ir(is))
            c(is,itr,ipond) = cf(is,ir(is))
            d(is,itr,ipond) = df(is,ir(is))
            e(is,itr,ipond) = ef(is,ir(is))
            ha(is,itr,ipond) = haf(is,ir(is))
            hl(is,itr) = hlf(is,ir(is))
c
            if (ir(is).eq.(nfr(is)+1)) then
              a(is,itr,ipond) = af(is,(ir(is)-1))
              b(is,itr,ipond) = bf(is,(ir(is)-1))
              c(is,itr,ipond) = cf(is,(ir(is)-1))
              d(is,itr,ipond) = df(is,(ir(is)-1))
              e(is,itr,ipond) = ef(is,(ir(is)-1))
              ha(is,itr,ipond) = haf(is,(ir(is)-1))
              hl(is,itr) = hlf(is,(ir(is)-1))
            else
              ir(is) = ir(is) + 1
              itf = 1
            end if
c
          else
c
c           continue
c
            a(is,itr,ipond) = af(is,(ir(is)-1))
            b(is,itr,ipond) = bf(is,(ir(is)-1))
            c(is,itr,ipond) = cf(is,(ir(is)-1))
            d(is,itr,ipond) = df(is,(ir(is)-1))
            e(is,itr,ipond) = ef(is,(ir(is)-1))
            ha(is,itr,ipond) = haf(is,(ir(is)-1))
            hl(is,itr) = hlf(is,(ir(is)-1))
          end if
c
  290   continue
c
        if (itf.eq.1) then
          itr = itr + 1
          ht(itr,ipond) = ht(itr-1,ipond) + 0.005
        else
          ht(itr,ipond) = ht(itr,ipond) + 0.005
        end if
c
        if (ht(itr,ipond).ge.(hot(ipond)+5.0)) then
          ht(itr,ipond) = hot(ipond) + 200.0
          nt(ipond) = itr
          go to 300
        else
          go to 280
        end if
c
c       determining the stage below which one of the outflow functions
c       blows up
c
  300   do 320 itr = 1, nt(ipond)
          hlm(itr,ipond) = 0.0
c
          do 310 is = 1, 15
            if (hl(is,itr).ge.hlm(itr,ipond)) then
              hlm(itr,ipond) = hl(is,itr)
            end if
  310     continue
c
  320   continue
c
c       area and length power functions determined from user-input
c       stage-area-length points
c
        read (20,*) nalpts
        read (20,*) hmntmp, a0tmp, l0tmp
c
        call eatcom(20)
c
        read (20,*) (hal(i),i = 1,nalpts)
c
        call eatcom(20)
c
        read (20,*) (area(i),i = 1,nalpts)
c
        call eatcom(20)
c
        read (20,*) (length(i),i = 1,nalpts)
c
        hmin(ipond) = hmntmp
        a0(ipond) = a0tmp
        l0(ipond) = l0tmp
c
        if (ipdout.eq.1) write (51,3100) hmin(ipond), hmin(ipond),
     1      a0(ipond), l0(ipond)
c
        do 330 i = 1, nalpts
          if (ipdout.eq.1) write (51,3200) hal(i), area(i), length(i)
  330   continue
c
        hmin(ipond) = hmin(ipond) * 3.281
        a0(ipond) = a0(ipond) * 3.281 ** 2
        l0(ipond) = l0(ipond) * 3.281
c
        do 340 i = 1, nalpts
          hal(i) = hal(i) * 3.281
          area(i) = area(i) * 3.281 ** 2
          length(i) = length(i) * 3.281
          atrns(i) = area(i) - a0(ipond)
          ltrns(i) = length(i) - l0(ipond)
  340   continue
c
c       run power function regression on hal, atrns and ltrns
c
        call imppow(nalpts,hal,atrns,aout)
c
        a1(ipond) = 10. ** (aout(1))
        a2(ipond) = aout(2)
c
        call imppow(nalpts,hal,ltrns,lout)
c
        l1(ipond) = 10. ** (lout(1))
        l2(ipond) = lout(2)
c
c       setting initial impoundment conditions
c
        qo(ipond) = 0.0
        cot(ipond) = 0.0
c
        do 350 i = 1, 10 * ndiv(ipond)
          co(i,ipond) = 0.0
          hset(i,ipond) = h(ipond)
  350   continue
c
c       defining sediment size class subdivisions
c
        cl50 = 0.002
        cl100 = 0.004
        sl0 = 0.004
        sl50 = 0.016
        sl100 = 0.063
        sa0 = 0.0028
        sa50 = 0.014
        sa100 = 0.0408
        sd0 = 0.063
        sd50 = 0.2
        la0 = 0.0408
        la50 = 0.4
c
        sa50 = sa50 * (0.8/1.65) ** 0.5
        la50 = la50 * (0.6/1.65) ** 0.5
        cl0 = 10.0 ** (log10(cl50)-(log10(cl100)-log10(cl50)))
        la100 = 10.0 ** (log10(la50)+(log10(la50)-log10(la0)))
        sd100 = 10.0 ** (log10(sd50)+(log10(sd50)-log10(sd0)))
c
        do 360 i = 1, ndiv(ipond)
c
          di(i,ipond) = 10.0 ** (log10(cl0)+float(2*i-1)/
     1        float(2*ndiv(ipond))*(log10(cl50)-log10(cl0)))
          di(i+ndiv(ipond),ipond) = 10.0 ** (log10(cl50)+float(2*i-1)/
     1        float(2*ndiv(ipond))*(log10(cl100)-log10(cl50)))
          di(i+2*ndiv(ipond),ipond) = 10.0 ** (log10(sl0)+float(2*i-1)/
     1        float(2*ndiv(ipond))*(log10(sl50)-log10(sl0)))
          di(i+3*ndiv(ipond),ipond) = 10.0 ** (log10(sl50)+
     1        float(2*i-1)/float(2*ndiv(ipond))*(log10(sl100)-
     1        log10(sl50)))
          di(i+4*ndiv(ipond),ipond) = 10.0 ** (log10(sa0)+float(2*i-1)/
     1        float(2*ndiv(ipond))*(log10(sa50)-log10(sa0)))
          di(i+5*ndiv(ipond),ipond) = 10.0 ** (log10(sa50)+
     1        float(2*i-1)/float(2*ndiv(ipond))*(log10(sa100)-
     1        log10(sa50)))
          di(i+6*ndiv(ipond),ipond) = 10.0 ** (log10(sd0)+float(2*i-1)/
     1        float(2*ndiv(ipond))*(log10(sd50)-log10(sd0)))
          di(i+7*ndiv(ipond),ipond) = 10.0 ** (log10(sd50)+
     1        float(2*i-1)/float(2*ndiv(ipond))*(log10(sd100)-
     1        log10(sd50)))
          di(i+8*ndiv(ipond),ipond) = 10.0 ** (log10(la0)+float(2*i-1)/
     1        float(2*ndiv(ipond))*(log10(la50)-log10(la0)))
          di(i+9*ndiv(ipond),ipond) = 10.0 ** (log10(la50)+
     1        float(2*i-1)/float(2*ndiv(ipond))*(log10(la100)-
     1        log10(la50)))
c
          d100(i,ipond) = 10.0 ** (log10(cl0)+float(2*i)/
     1        float(2*ndiv(ipond))*(log10(cl50)-log10(cl0)))
          d100(i+ndiv(ipond),ipond) = 10.0 ** (log10(cl50)+float(2*i)/
     1        float(2*ndiv(ipond))*(log10(cl100)-log10(cl50)))
          d100(i+2*ndiv(ipond),ipond) = 10.0 ** (log10(sl0)+float(2*i)/
     1        float(2*ndiv(ipond))*(log10(sl50)-log10(sl0)))
          d100(i+3*ndiv(ipond),ipond) = 10.0 ** (log10(sl50)+
     1        float(2*i)/float(2*ndiv(ipond))*(log10(sl100)-
     1        log10(sl50)))
          d100(i+4*ndiv(ipond),ipond) = 10.0 ** (log10(sa0)+float(2*i)/
     1        float(2*ndiv(ipond))*(log10(sa50)-log10(sa0)))
          d100(i+5*ndiv(ipond),ipond) = 10.0 ** (log10(sa50)+
     1        float(2*i)/float(2*ndiv(ipond))*(log10(sa100)-
     1        log10(sa50)))
          d100(i+6*ndiv(ipond),ipond) = 10.0 ** (log10(sd0)+float(2*i)/
     1        float(2*ndiv(ipond))*(log10(sd50)-log10(sd0)))
          d100(i+7*ndiv(ipond),ipond) = 10.0 ** (log10(sd50)+
     1        float(2*i)/float(2*ndiv(ipond))*(log10(sd100)-
     1        log10(sd50)))
          d100(i+8*ndiv(ipond),ipond) = 10.0 ** (log10(la0)+float(2*i)/
     1        float(2*ndiv(ipond))*(log10(la50)-log10(la0)))
          d100(i+9*ndiv(ipond),ipond) = 10.0 ** (log10(la50)+
     1        float(2*i)/float(2*ndiv(ipond))*(log10(la100)-
     1        log10(la50)))
c
          sg(i,ipond) = 2.65
          sg(i+ndiv(ipond),ipond) = 2.65
          sg(i+2*ndiv(ipond),ipond) = 2.65
          sg(i+3*ndiv(ipond),ipond) = 2.65
          sg(i+4*ndiv(ipond),ipond) = 1.65
          sg(i+5*ndiv(ipond),ipond) = 1.65
          sg(i+6*ndiv(ipond),ipond) = 2.65
          sg(i+7*ndiv(ipond),ipond) = 2.65
          sg(i+8*ndiv(ipond),ipond) = 1.65
          sg(i+9*ndiv(ipond),ipond) = 1.65
c
  360   continue
c
        do 370 i = 1, ndiv(ipond)
c
          if (i.eq.1) then
            d0(i,ipond) = cl0
            d0(i+ndiv(ipond),ipond) = d100(i+ndiv(ipond)-1,ipond)
            d0(i+2*ndiv(ipond),ipond) = sl0
            d0(i+3*ndiv(ipond),ipond) = d100(i+3*ndiv(ipond)-1,ipond)
            d0(i+4*ndiv(ipond),ipond) = sa0
            d0(i+5*ndiv(ipond),ipond) = d100(i+5*ndiv(ipond)-1,ipond)
            d0(i+6*ndiv(ipond),ipond) = sd0
            d0(i+7*ndiv(ipond),ipond) = d100(i+7*ndiv(ipond)-1,ipond)
            d0(i+8*ndiv(ipond),ipond) = la0
            d0(i+9*ndiv(ipond),ipond) = d100(i+9*ndiv(ipond)-1,ipond)
          else
            d0(i,ipond) = d100(i-1,ipond)
            d0(i+ndiv(ipond),ipond) = d100(i+ndiv(ipond)-1,ipond)
            d0(i+2*ndiv(ipond),ipond) = d100(i+2*ndiv(ipond)-1,ipond)
            d0(i+3*ndiv(ipond),ipond) = d100(i+3*ndiv(ipond)-1,ipond)
            d0(i+4*ndiv(ipond),ipond) = d100(i+4*ndiv(ipond)-1,ipond)
            d0(i+5*ndiv(ipond),ipond) = d100(i+5*ndiv(ipond)-1,ipond)
            d0(i+6*ndiv(ipond),ipond) = d100(i+6*ndiv(ipond)-1,ipond)
            d0(i+7*ndiv(ipond),ipond) = d100(i+7*ndiv(ipond)-1,ipond)
            d0(i+8*ndiv(ipond),ipond) = d100(i+8*ndiv(ipond)-1,ipond)
            d0(i+9*ndiv(ipond),ipond) = d100(i+9*ndiv(ipond)-1,ipond)
          end if
c
  370   continue
c
        do 380 i = 1, 10 * ndiv(ipond)
c
          if (di(i,ipond).ge.0.08393) then
            vs(i,ipond) = 10.0 ** (-0.342463*(log10(di(i,ipond)))**2+
     1          0.989122*(log10(di(i,ipond)))-0.33801)
          else
            vs(i,ipond) = 2.81 * di(i,ipond) ** 2
          end if
c
  380   continue
c
c     end of multiple impoundment loop
c
  390 continue
c
      if (ipdout.eq.1) write (51,3300)
c
      if ((npond.eq.1).and.(ipdout.eq.1)) then
        write (56,3400)
        write (57,3500)
        write (57,3600)
      end if
c
      return
c
 1000 format (a)
 1100 format (5x,a)
 1200 format (5x,'WEPP SURFACE IMPOUNDMENT ELEMENT OUTPUT FILE'/)
 1300 format (5x,'SECTION I. SUMMARY OF WEPP IMPOUNDMENT ELEMENT ',
     1    'INPUTS'/)
 1400 format (//5x,'Description of Impoundment Number ',i2,':'/)
 1500 format (/5x,'Structure ',i1,/,5x,11('-')/)
 1600 format (5x,'Structure Description: ',a50/)
 1700 format (5x,'Type: Drop Spillway With Circular Riser and ',
     1    'Barrel',//,5x,'Dimensions:',//,5x,'riser diameter =',8x,f5.2,
     1    ' m',5x,'riser inlet stage =',3x,f5.2,' m',/,5x,
     1    'weir flow coef =',8x,f5.2,7x,'orifice flow coef =',3x,f5.2,/,
     1    5x,'barrel diameter =',7x,f5.2,' m',5x,'height of riser =',5x,
     1    f5.2,' m',/,5x,'length of barrel =',6x,f5.1,' m',5x,
     1    'slope of barrel =',5x,f5.2,' m/m',/,5x,
     1    'barrel outlet height =',2x,f5.2,' m',5x,
     1    'entrance loss coef =',2x,f5.2,/,5x,'bend loss coef =',8x,f5
     1    .2,7x,'friction loss coef =',2x,f5.2//)
 1800 format (5x,'Type: Drop Spillway With Rectangular Riser and ',
     1    'Circular Barrel',//,5x,'Dimensions:',//,5x,'riser length =',7
     1    x,f5.2,' m',5x,'riser width =',14x,f5.2,' m',/,5x,
     1    'riser inlet stage =',2x,f5.2,' m',5x,'weir flow coef =',11x,
     1    f5.2,/,5x,'orifice flow coef =',2x,f5.2,7x,
     1    'barrel diameter =',10x,f5.2,' m',/,5x,'height of riser =',4x,
     1    f5.2,' m',5x,'length of barrel =',9x,f5.1,' m',/,5x,
     1    'slope of barrel =',4x,f5.2,' m/m',3x,'barrel outlet ',
     1    'height =',5x,f5.2,' m',/,5x,'entrance loss coef = ',f5.2,7x,
     1    'bend loss coef =',11x,f5.2,/,5x,'friction loss coef = ',f5.2
     1    //)
 1900 format (5x,'Type: Drop Spillway With Rectangular Riser and ',
     1    'Barrel',//,5x,'Dimensions:',//,5x,'riser length =',13x,f5.2,
     1    ' m',3x,'riser width =',10x,f5.2,' m',/,5x,
     1    'riser inlet stage =',8x,f5.2,' m',3x,'weir flow coef =',7x,f5
     1    .2,/,5x,'orifice flow coef =',8x,f5.2,5x,'barrel height =',8x,
     1    f5.2,' m',/,5x,'barrel width =',13x,f5.2,' m',3x,
     1    'height of riser =',6x,f5.2,' m',/,5x,'length of barrel =',9x,
     1    f5.1,' m',3x,'slope of barrel =',6x,f5.2,' m/m',/,5x,
     1    'barrel outlet height =',5x,f5.2,' m',3x,
     1    'entrance loss coef = ',2x,f5.2,/,5x,'bend loss coef =',11x,f5
     1    .2,5x,'friction loss coef =',3x,f5.2//)
 2000 format (5x,'Type: Culvert # ',i1,5x,
     1    'Number of Identical Culverts: ',i3,//,5x,'Dimensions:',//,5x,
     1    'culvert flow area =',7x,f5.2,' m^2',2x,
     1    'cross sectional height =',3x,f5.2,' m',/,5x,
     1    'culvert inlet stage =',5x,f5.2,' m',4x,'length of culvert =',
     1    8x,f5.1,' m',/,5x,'slope of culvert =',8x,f5.2,' m',4x,
     1    'height of culvert outlet =',1x,f5.2,' m',/,5x,
     1    'entrance loss coef =',6x,f5.2,6x,'bend loss coef =',11x,f5.2,
     1    /,5x,'friction loss coef =',6x,f5.2,6x,
     1    'unsubmerged flow coef K =',2x,f5.2,/,5x,
     1    'unsubmerged flow coef M =',1x,f5.2,6x,
     1    'unsubmerged flow coef C =',2x,f5.2,/,5x,
     1    'unsubmerged flow coef Y =',1x,f5.2//)
 2100 format (5x,'Type: Rock Fill Checkdam',//,5x,'Dimensions:',//,5x,
     1    'length of rock fill =',6x,f5.2,' m',/,5x,
     1    'rock fill inlet stage =',4x,f5.2,' m',/,5x,
     1    'rock fill overtop stage =',2x,f5.2,' m',/,5x,
     1    'width of rock fill =',7x,f5.2,' m',/,5x,
     1    'average rock diameter =',4x,f5.3,' m'//)
 2200 format (5x,'Type: Emergency Spillway or Open Channel',//,5x,
     1    'Dimensions:',//,5x,'channel bottom width =',4x,f5.2,' m',3x,
     1    'channel side slopes =',4x,f5.2,' m/m',/,5x,
     1    'mannings n for surface =',2x,f5.2,5x,'channel inlet stage =',
     1    4x,f5.2,' m',/,5x,'channel maximum stage =',3x,f5.2,' m',3x,
     1    'entrance section slope =',1x,f5.2,' m/m',/,5x,
     1    'entrance section length =',1x,f5.1,' m',3x,
     1    'middle section slope =',3x,f5.2,' m/m',/,5x,
     1    'middle section length =',3x,f5.1,' m',3x,
     1    'exit section slope =',5x,f5.2,' m/m'//)
 2300 format (5x,'Type: Emergency Spillway or Open Channel With',/,11x,
     1    'User Specified Stage-Discharge Relationship',//,5x,
     1    'Dimensions:',//,5x,'Flow Begins at =',1x,f5.2,' m',//,5x,
     1    'Stage (m)',5x,'Discharge (m^3/s)',/,5x,9('-'),5x,17('-'))
 2400 format (7x,f5.2,13x,f5.2)
 2500 format (//)
 2600 format (5x,'Type: Filter Fence',//,5x,'Dimensions:'/)
 2700 format (5x,'Type: Straw Bales or Trash Barrier',//,5x,
     1    'Dimensions:'/)
 2800 format (5x,'slurry flow rate =',6x,f7.5,' m/s',/,5x,
     1    'cross sectional width =',1x,f7.2,' m',/,5x,'inlet stage =',11
     1    x,f7.2,' m',/,5x,'overtop stage =',9x,f7.2,' m'//)
 2900 format (5x,'Type: Perforated Riser',//,5x,'Dimensions:',//,5x,
     1    'stage of riser opening =',3x,f5.2,' m',3x,
     1    'datum to restrict orifice=',1x,f5.2,' m',/,5x,
     1    'height of slots =',10x,f5.2,' m',3x,'stage of datum =',11x,f5
     1    .2,' m',/,5x,'diameter of riser =',8x,f5.2,' m',3x,
     1    'area of the slots =',8x,f5.3,' m^2',/,5x,
     1    'restricting orifice dia =',2x,f5.2,' m',3x,
     1    'height of riser =',10x,f5.2,' m',/,5x,'length of barrel =',9
     1    x,f5.1,' m',3x,'slope of barrel =',10x,f5.2,' m',/,5x,
     1    'barrel diameter =',10x,f5.2,' m',3x,
     1    'restricting orifice coef =',1x,f5.2,/,5x,'weir flow coef =',
     1    11x,f5.2,5x,'orifice flow coef =',8x,f5.2,/,5x,
     1    'slot orifice coef =',8x,f5.2,5x,'entrance loss coef =',7x,f5
     1    .2,/,5x,'bend loss coef =',11x,f5.2,5x,'friction loss coef =',
     1    7x,f5.2//)
 3000 format (5x,'General Impoundment Characteristics',/,5x,35('-'),//,5
     1    x,'overtop stage =',12x,f5.2,' m',/,5x,
     1    'full of sediment stage =',3x,f5.2,' m',/,5x,
     1    'beginning stage =',10x,f5.2,' m',/,5x,'initial time step =',8
     1    x,f5.2,' hr',/,5x,'infiltration rate =',5x,f8.6,' m/d',/,5x,
     1    'structure size flag =',6x,i5,/,5x,'size class divisions =',5
     1    x,i5)
 3100 format (5x,'min stage =',16x,f5.2,' m',//,5x,'stage (m)',6x,
     1    'area (m^2)',6x,'length (m)',/,5x,9('-'),6x,10('-'),6x,10('-'
     1    ),/,1x,f11.4,6x,f11.4,5x,f11.4)
 3200 format (1x,f11.4,6x,f11.4,5x,f11.4)
 3300 format (//5x,'SECTION II. WEPP IMPOUNDMENT ELEMENT OUTPUT'/)
 3400 format (16x,'WEPP SURFACE IMPOUNDMENT ELEMENT SEDIMENT OUTPUT'//,
     1    18x,'SEDIMENT BREAKDOWN BY SIZE CLASS (KILOGRAMS)',///,29x,
     1    'TOTAL',49x,'CLAY',35x,'SILT',32x,'SMALL AGG',33x,'SAND',35x,
     1    'LARGE AGG',/,29x,5('-'),49x,4('-'),35x,4('-'),32x,9('-'),33x,
     1    4('-'),35x,9('-'),/,52x,'TOTAL IN',/,1x,'DATE',2x,'YEAR',6x,
     1    'IN',11x,'OUT',6x,'RETAINED',4x,'IMPOUNDMENT',8x,5('IN',11x,
     1    'OUT',6x,'RETAINED',9x),/,1x,4('-'),2x,4('-'),6x,2('-'),11x,3(
     1    '-'),6x,8('-'),4x,11('-'),8x,5(2('-'),11x,3('-'),6x,8('-'),9x)
     1    )
 3500 format (15x,'WEPP SURFACE IMPOUNDMENT ELEMENT HYDRAULIC OUTPUT'//,
     1    22x,'DEFINITIONS OF OUTPUT VARIABLES',/,22x,31('-'),//,22x,
     1    'QPO    = PEAK OUTFLOW RATE (M^3/S)',/,22x,
     1    'VOLO   = VOLUME OF OUTGOING FLOW (M^3)',/,22x,
     1    'QPIN   = PEAK INFLOW RATE (M^3/S)',/,22x,
     1    'VOLIN  = VOLUME OF THE INCOMING FLOW (M^3)',/,22x,
     1    'HMAX   = MAXIMUM STAGE ATTAINED (M)',/,22x,
     1    'COUTPK = PEAK OUTFLOW CONCENTRATION (MG/L)',/,22x,
     1    'COUTAV = AVERAGE OUTFLOW CONCENTRATION (MG/L)',/,22x,
     1    'CINAV  = AVERAGE INFLOW CONCENTRATION (MG/L)',/,22x,
     1    'TE     = OVERALL TRAPPING EFFICIENCY (%)',/,22x,
     1    'HMIN   = MINIMUM STAGE AFTER DEPOSITION (M)'//)
 3600 format ('YEAR',2x,'DATE',4x,'QPO',7x,'VOLO',6x,'QPIN',5x,'VOLIN',4
     1    x,'HMAX',3x,'OT?',2x,'COUTPK',2x,'COUTAV',2x,'CINAV',3x,'TE',4
     1    x,'HMIN',2x,'FULL?',/,4('-'),2x,4('-'),4x,3('-'),7x,4('-'),6x,
     1    4('-'),5x,5('-'),4x,4('-'),3x,3('-'),2x,6('-'),2x,6('-'),2x,5(
     1    '-'),3x,2('-'),4x,4('-'),2x,5('-')/)
      end
