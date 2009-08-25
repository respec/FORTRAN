      subroutine dcap(flagm,flagt,q,sf,c1,z,effsh,depsid,depmid,werod,
     1    wflow,n,crsh,covsh,maxe,excess,tb,ielmt,df,ichan)
c
c     + + + PURPOSE + + +
c
c     SR DCAP computes detachment capacity by concentrated
c     flow in channels.
c
c     Called from: SRS CHNRT, DETACH
c     Author(s): Ascough II, R. van der Zweep, V. Lopes
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
      include 'pmxprt.inc'
      include 'pmxpln.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real q, sf, c1, z, effsh, depsid, depmid, werod, wflow, n, crsh,
     1    covsh, maxe, excess, tb, df(mxpart)
      integer flagm, flagt, ielmt, ichan
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     Real Variables
c
c     q      -
c     sf     -
c     c1     -
c     z      -
c     effsh  -
c     depsid -
c     depmid -
c     werod  -
c     wflow  -
c     n      -
c     crsh   -
c     covsh  -
c     maxe   -
c     excess -
c     tb     -
c     df     -
c
c     Integer Variables
c
c     flagm -
c     flagt -
c     ielmt -
c     ichan -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
      include 'cchprt.inc'
      include 'cchvar.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real xxcf(17), ffxcf(17), xxb(27), fhxb(27), ab, ad, adjsh, dct,
     1    di, difsh, dwdti, eros, erosl, hxb, shdist, timex, timpot,
     1    timsh, tstar, we, wfin, wstar, xb, xcf
      integer ifrus, itrat, k
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     xxcf(17)  -
c     ffxcf(17) -
c     xxb(27)   -
c     fhxb(27)  -
c     ab        -
c     ad        -
c     adjsh     -
c     dct       -
c     di        -
c     difsh     -
c     dwdti     -
c     eros      -
c     erosl     -
c     hxb       -
c     shdist    -
c     timex     -
c     timpot    -
c     timsh     -
c     tstar     -
c     we        -
c     wfin      -
c     wstar     -
c     xb        -
c     xcf       -
c
c     Integer Variables
c
c     ifrus -
c     itrat -
c     k     -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     hydchn
c     shdist
c     table
c
c     + + + DATA INITIALIZATIONS + + +
c
      data xxcf /0.0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16,
     1    0.18, 0.20, 0.22, 0.24, 0.26, 0.28, 0.30, 0.32/
c
      data ffxcf /1000.0, 33.872, 12.571, 7.3030, 5.1102, 3.9575,
     1    3.2659, 2.8419, 2.5040, 2.2818, 2.1194, 1.9997, 1.9118,
     1    1.8489, 1.8068, 1.7829, 1.7758/
c
      data xxb /0.0, 0.01, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14,
     1    0.16, 0.18, 0.2, 0.22, 0.24, 0.26, 0.28, 0.3, 0.32, 0.34,
     1    0.36, 0.38, 0.4, 0.42, 0.44, 0.46, 0.48, 0.5/
c
      data fhxb /0.0, 0.000474, 0.00154, 0.00509, 0.0104, 0.0177,
     1    0.0269, 0.0384, 0.0524, 0.0693, 0.0897, 0.114, 0.1432, 0.1782,
     1    0.2207, 0.2724, 0.3361, 0.4159, 0.5176, 0.6506, 0.8307,
     1    1.0858, 1.4722, 2.1212, 3.4264, 7.3566, 10000.0/
c
c     + + + END SPECIFICATIONS + + +
c
c
      timpot = 0.0
c     adjsh = 1.35 * effsh
      adjsh = effsh
c
      if (adjsh.gt.crsh) go to 30
c
   10 continue
c
      do 20 k = 1, cnpart
        df(k) = 0.0
   20 continue
c
      dct = 0.0
      return
c
   30 continue
c
      timsh = tb * (1.0-(crsh/effsh))
      if (depmid.eq.0.0) go to 60
c
      if (flagt.ne.3) then
        call hydchn(4,q,sf,c1,z,wflow,werod,n,crsh,covsh,effsh)
      else
        werod = wflow
      end if
c
      difsh = effsh - crsh
      if (difsh.le.0.0) go to 10
c
      di = excess * chnk(ichan) * (effsh-crsh)
c
      timpot = depmid * wtdsoi / di
      if (timpot.lt.timsh) go to 60
c
   40 continue
c
      dct = di * timsh * werod / (tb*wflow)
c
      if (flagm.ne.1) then
c
        if (dct.ge.maxe) then
          di = di * maxe / dct
          dct = maxe
        end if
c
      end if
c
      do 50 k = 1, cnpart
        df(k) = dct * crfrac(k,ielmt)
   50 continue
c
      depmid = depmid - di * timsh / wtdsoi
      if (depmid.lt.0.005) depmid = 0.0
      return
c
   60 continue
c
      timex = timsh - timpot
      ab = (q*nbarch/(1.49*sqrt(sf)))
c
      if (werod.eq.0.0) call hydchn(4,q,sf,c1,z,wflow,werod,n,crsh,
     1    covsh,effsh)
      hxb = ab / (werod**(8.0/3.0))
      ifrus = 4
      itrat = 27

c dcf Fix to prevent program from stopping in
c     subroutine TABLE.  3-11-2004

      if (hxb.gt.9999.99) hxb = 9999.99

      call table(ifrus,itrat,xxb,fhxb,hxb,xb)

      difsh = effsh * shdist(xb) - crsh
c
      if (difsh.le.0.0) then
        if (depmid.le.0.0) go to 10
        timsh = timpot
        go to 40
      end if
c
      dwdti = excess * 2.0 * chnk(ichan) * (difsh) / wtdsoi
c     dwdti = excess * 2.0 * chnk(ichan) * (difsh**1.05) / wtdsoi
c
      ad = (ab**0.375) * wtdh2o * sf / crsh


      if (ad.le.1.7758) go to 10
c
      ifrus = 3
      itrat = 17
c
c dcf Added following line to prevent stop from occurring in TABLE
      if(ad.gt.999.999) ad = 999.999
c
      call table(ifrus,itrat,xxcf,ffxcf,ad,xcf)
      wfin = (ab**0.375) * ((xcf*(1.0-2.0*xcf)/xcf**(8.0/3.0))**0.375)
      if (wfin.le.werod) go to 10
      tstar = timex * dwdti / (wfin-werod)
      wstar = (1.0-exp(-1.0176*tstar)) / 1.0176
      we = wstar * (wfin-werod) + werod
      eros = (we-werod) * depsid + depmid * werod
      dct = eros * wtdsoi / (tb*wflow)
c
      if (flagm.ne.1) then
c
        if (dct.ge.maxe) then
          dct = maxe
          eros = dct * tb * wflow / wtdsoi
        end if
c
      end if
c
      do 70 k = 1, cnpart
        df(k) = dct * crfrac(k,ielmt)
   70 continue
c
      if (eros.ge.depmid*werod) then
        erosl = eros - depmid * werod
        werod = erosl / depsid + werod
        depmid = 0.0
        return
      end if
c
      depmid = depmid - eros / werod
c
      return
      end
