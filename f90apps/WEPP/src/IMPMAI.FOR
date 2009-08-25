      subroutine impmai(htw,pet)
c
c     + + + PURPOSE + + +
c
c     SR IMPMAI is the main driver for the impoundment element.
c     It utilizes the stage-discharge, stage-area and stage-length
c     functions defined in SR IMPINT and the incoming daily
c     rectangular input hydrograph and sediment graph.
c     It outputs daily peak outflow, peak volume, and average
c     sediment concentration.
c
c     Called from:
c     Author(s): Mark Lindley, Jim Ascough II
c     Reference in User Guide:
c
c     Version:
c     Date recoded: 3/05/94
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxelm.inc'
      include 'pmximp.inc'
      include 'pmxpln.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real htw, pet
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     htw -
c     pet -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cflags.inc'
c     read: ipdout, idflag
c
      include 'cimday.inc'
      include 'cimeos.inc'
c
      include 'cimitf.inc'
c     read: a,b,c,d,e,ha,ht,hlm,a0,a1,a2,l0,l1,l2,qinf,isize
c
      include 'cimmon.inc'
      include 'cimpnd.inc'
c
      include 'cimqot.inc'
c     read: deltat
c     modify: qo,qon,h,hn,t,tn,dttry,dtdid,dtnext
c
      include 'cimsed.inc'
c     read: d0,di,d100,sg,ndiv,vs
c     modify: ctd,cdp,hmin,hset,co,vs,cot
c
      include 'cimyrs.inc'
c
      include 'cstruct.inc'
c     read: ielmt
c        added by S.Dun  March 16,1999
        include 'cimflg.inc'
c        call the impoundment structure indicators
c
c        include 'cimcv1.inc'
c        include 'cimcv2.inc'
        include 'cimcln.inc'
c        write automatic cleaning flag for culvert 1
        include 'cimacl.inc'
c        write automatic cleaning message
        include 'cimdate.inc'
c        simulating date
c        end adding
c
c     + + + LOCAL VARIABLES + + +
c
      real ai, havg, hihr8, hti, qoqi, qoti, td, td100, vmx, qi, vol,
     1    ti, voli, l, ln, an, cont, w, rt(100), dep(100), voln,
     1    con(100), ci(100), ds, ar, pot(100), clt, sat, slt, sdt, lat,
     1    coit(100), totdep, dept, aset(100), cset(100), clset, slset,
     1    saset, sdset, laset, volset(100), volm, perc, perc1, perc2,
     1    hihr, qoaivs, qoaive, qoarvs, qoqi2, vmxvi, vold, voldep, vpi,
     1    hstart
c
      integer is, itr, icnt, n, i, ig, ip, iv, ntn
c
c     + + + LOCAL DEFINITIONS + + +
c
c     ai        - area of the impoundment at the average stage during the
c                 time of inflow, hti
c     an        - area at the stage at the end of a time step, hn
c     ar        - area at the stage at the beginning of a time step, h
c     aset(i)   - area at settling depth i
c     ci(i)     - concentration of sediment in the ith size subclass
c     clset     - lbs of clay size sediment in impoundment
c     clt       - dummy variable used in determining cl50o
c     coit(i)   - total outgoing sediment in size subclass i
c     con(i)    - outgoing sediment concentration for size subclass i
c                 at the end of a time step
c     cont      - total outgoing sediment concentration at the end of a
c                 time step
c     cset(i)   - settling concentration for particle size class i
c     dep(i)    - deposition of particle size subclass i over a time step
c     dept      - volume of sediment deposited over a time step
c     ds        - dead storage
c     havg      - variable used to determine the average stage
c     hihr      - dimensionless head variable
c     hihr8     - dimensionless head variable
c     hti       - average stage during the time of inflow
c     htw       - height above the outlet channel of the tail water
c     icnt      - particle size do-loop counter
c     ig        - hydrograph counter
c     ip        - percentage in particle class do-loop counter
c     is        - sediment graph counter
c     itr       - flow regime transition counter
c     iv        - flow volume do-loop counter
c     l         - length of the impoundment at the beginning of the time step
c     laset     - lbs of large aggregate size sediment in impoundment
c     lat       - dummy variable used in determining la50o
c     ln        - length of the impoundment at the end of the time step
c     ntn       - counter for adjusting time step
c     pot(i)    - dummy variable used in determining the outgoing d50's
c     pet       - potential evapotranspiration (pan evaporation)
c     perc      - dummy % variable used to determine ci(i)
c     perc1     - dummy % variable used to determine ci(i)
c     perc2     - dummy % variable used to determine ci(i)
c     qi        - inflow
c     qoaive    - variations on overflow rate for Ct and Cd calculations
c     qoaivs    - variations on overflow rate for Ct and Cd calculations
c     qoarvs    - variations on overflow rate for Ct and Cd calculations
c     qoqi      - dimensionless outflow/inflow ratio
c     qoqi2     - square of qoqi
c     qoti      - average outflow during the time of inflow
c     rt(i)     - ratio of the actual detention time to the detenion time
c                 required for 100% settling
c     saset     - lbs of small aggregate size sediment in impoundment
c     sat       - dummy variable used in determining sa50o
c     sdset     - lbs of sand size sediment in impoundment
c     sdt       - dummy variable used in determining sd50o
c     slset     - lbs of silt size sediment in impoundment
c     slt       - dummy variable used in determining sl50o
c     td        - actual detention time
c     td100     - detention time required for 100% settling
c     ti        - duration of inflow
c     totdep    - total sediment volume deposited over a day
c     vmx       - volume at the maximum stage
c     vmxvi     - volume ratio vmx/vi
c     vol       - volume at the beginning of the time step
c     voli      - volume that has entered the impoundment
c     vold      - deposition volume
c     voldep    - second deposition volume
c     volm      - volume of the deposited sediment (hmin)
c     voln      - volume of the new stage
c     volset(i) - volume of settling portion of impoundment for
c                 particle size class i
c     vpi       - average volume of the pond over the period of inflow
c     w         - average width of the impoundment over a time step
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     impflo
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     variable initialization
c
      n = 0
      voli = 0.0
      volo(ipond) = 0.0
      vol = 0.0
      volm = 0.0
c
c        added by S.Dun  March 16,1999
c        if (c1icv(ipond).ne.0.or.c2icv(ipond).ne.0) then
           atcln=0
c           if (hmin(ipond).gt.(c1h(ipond)+c1hit(ipond)/2.)) then
           if (hmin(ipond).gt.hfull(ipond)) then
                 alnum(ipond)=alnum(ipond)+1
                 if (alnum(ipond).le.100) then
                    aldate(ipond,alnum(ipond))=ddate
                        alyear(ipond,alnum(ipond))=dyear
                        alhs(ipond,alnum(ipond))=hmin(ipond)
              do 9 i = 1, 5
                alvol(ipond,alnum(ipond)) = alvol(ipond,alnum(ipond)) +
     1          ((a0(ipond)+a1(ipond)*(float(i)*hmin(ipond)/5.)**
     1      a2(ipond))+(a0(ipond)+a1(ipond)*(float(i-1)*hmin(ipond)/5.)
     1      **a2(ipond))) / 2.0 * (float(i)*hmin(ipond)/5.-float(i-1)*
     1      hmin(ipond)/5.)
    9                   continue
             endif
c                 
           hmin(ipond)=0.0
                 h(ipond)=0.01
                 qo(ipond)=0.0
                 atcln=1
             write (6,*) atcln
c             pause 
           endif
c        endif
c        end adding
c
      do 10 i = 1, 5
        volm = volm + ((a0(ipond)+a1(ipond)*(float(i)*hmin(ipond)/5.)**
     1      a2(ipond))+(a0(ipond)+a1(ipond)*(float(i-1)*hmin(ipond)/5.)
     1      **a2(ipond))) / 2.0 * (float(i)*hmin(ipond)/5.-float(i-1)*
     1      hmin(ipond)/5.)
   10 continue
c
      do 20 i = 1, 20
        vol = vol + ((a0(ipond)+a1(ipond)*(float(i)*h(ipond)/20.)**
     1      a2(ipond))+(a0(ipond)+a1(ipond)*(float(i-1)*h(ipond)/20.)**
     1      a2(ipond))) / 2.0 * (float(i)*h(ipond)/20.-float(i-1)*
     1      h(ipond)/20.)
   20 continue
c
      vol = vol - volm
c
c     adjusting the inlet stage for hmin
c
c
c        deavailabed by S.Dun March 31,1999
c
c      do 30 itr = 2, nt(ipond)
c        if (hmin(ipond).gt.ha(10,itr,ipond)) then
cc
c          if (a(10,itr,ipond).gt.0.0) then
c        ht(2,ipond)=hmin(ipond)+0.0025
c            ha(10,itr,ipond) = hmin(ipond)
c            if (hmin(ipond).gt.hlm(itr,ipond)) hlm(itr,ipond) =
c     1          hmin(ipond)
c            if (hmin(ipond).gt.e(10,itr,ipond)) e(10,itr,ipond) =
c     1          hmin(ipond)
c          end if
cc
c        end if
c
c        if (hmin(ipond).gt.ha(12,itr,ipond)) then
cc
c          if (a(12,itr,ipond).gt.0.0) then
c            ha(12,itr,ipond) = hmin(ipond)
c            if (hmin(ipond).gt.hlm(itr,ipond)) hlm(itr,ipond) =
c     1          hmin(ipond)
c            if (hmin(ipond).gt.d(12,itr,ipond)) d(12,itr,ipond) =
c     1          hmin(ipond)
c          end if
cc
c        end if
c
c        if (hmin(ipond).gt.ha(13,itr,ipond)) then
c          if (a(13,itr,ipond).gt.0.0) then
c            ha(13,itr,ipond) = hmin(ipond)
c            if (hmin(ipond).gt.hlm(itr,ipond)) hlm(itr,ipond) =
c     1          hmin(ipond) + 0.0001
c            if (hmin(ipond).gt.ht(itr,ipond)) ht(itr,ipond) =
c     1          hmin(ipond) + 0.0001
c          end if
c        end if
c
c   30 continue
c        added by added by S.Dun  March 16,1999
        call impreg
c        end adding
c
c
c     entering hydrograph info
c
c     read (ipond+9,*) qiin(ipond), vi(ipond), htw, pet
c
      qiin(ipond) = qiin(ipond) * 3.281 ** 3.0
      vi(ipond) = vi(ipond) * 3.281 ** 3.0
c
      htw = htw * 3.281
      pet = pet * 3.281
c
      if (qo(ipond).le.0.00001.and.qiin(ipond).le.0.00001) then
        go to 510
      else
        ig = 0
        totco(ipond) = 0.0
        havg = h(ipond)
        hmax(ipond) = h(ipond)
        hti = h(ipond)
        qomx(ipond) = qo(ipond)
        qoti = 0.0
        ai = a0(ipond) + a1(ipond) * hti ** a2(ipond)
        vmx = 0.0
        n = 0
        ntn = 0
        t(ipond) = 0.0
        ti = 0.0
c
        if (qiin(ipond).gt.0.00001) ti = vi(ipond) / qiin(ipond) /
     1      3600.0
c
        dttry(ipond) = deltat(ipond)
        itr = 1
c
        rewind (50)
c
        write (50) t(ipond), h(ipond), qo(ipond)
c
c       write (6,1050)
c
c       starting hydraulic simulation
c
        write (6,1100) ipond
c
   40   qi = 0.0
c
        if (t(ipond).lt.ti) qi = qiin(ipond)
c
c       determine flow regime and call ouflow subroutine to
c       calculate time step, new stage, and new outflow
c
   50   if (h(ipond).ge.ht(itr,ipond).and.h(ipond).le.ht(itr+1,ipond)) 
     1  then
                hstart=h(ipond)
c
          call impflo(itr,qi,htw)
c
c         checking for ti and 24 hr approach and setting max values
c
          ig = ig + 1
c
          if (hn(ipond).gt.hmax(ipond)) hmax(ipond) = hn(ipond)
c
          if (t(ipond).gt.ti.and.(hn(ipond).lt.
     1                (hmin(ipond)+0.001))) then
            hn(ipond) = hmin(ipond)
            qon(ipond) = 0.0
            if(qo(ipond).lt.1.0e-06) tn(ipond) = 24.000
          end if
c
c        added by S.Dun  March 16,1999
c        if None rock-fill check dam or emergence spillway or filter
c        fence or Strawbales  or trash barrier is present. when the 
c        stage highter than the stage at which the impoundment is 
c        overtopped. The overtop flow should occour.
c
             if (firf(ipond).eq.0.and.fies(ipond).eq.0.and.
     1          fiff(ipond).eq.0) then
                  if(hn(ipond).gt.hot(ipond).and.tn(ipond).lt.ti) then
                                qon(ipond)=qi
                                hn(ipond)=hot(ipond)
                                write(6,55) ddate,dyear,ipond
c                                pause
 55        format(1x,'Overtopflow occurred at day',i5,' year',i3
     1        ,' in the impoundment',i3)
                        endif
                  endif
c        end adding          
          if (abs(qo(ipond)-qi).lt.0.0001) then
            if (abs(qon(ipond)-qi).lt.0.0001) ntn = ntn + 1
            if (ntn.le.1.and.tn(ipond).le.ti) tn(ipond) = ti + 0.0001
          end if
c
          write (50) tn(ipond), hn(ipond), qon(ipond)
          if (qon(ipond).gt.qomx(ipond)) qomx(ipond) = qon(ipond)
c
          havg = ((hn(ipond)-h(ipond))/2.0+h(ipond)) * (tn(ipond)-
     1        t(ipond)) + havg
          volo(ipond) = volo(ipond) + (tn(ipond)-t(ipond)) * 3600. * (
     1        qon(ipond)+qo(ipond)) / 2.0
c
c        if (itr.gt.1.and.volo(ipond).lt.1.e-5) pause
c                                                          
          dttry(ipond) = dtnext(ipond)
          t(ipond) = tn(ipond)
          h(ipond) = hn(ipond)
          qo(ipond) = qon(ipond)
c
          if (itr.lt.3) itr = 1
          if (itr.ge.3) itr = itr - 2
c
          if (tn(ipond).lt.ti) then
            if ((tn(ipond)+dttry(ipond)).gt.ti) dttry(ipond) = ti -
     1          tn(ipond)
          end if
c
          if (tn(ipond).lt.24.0) then
            if ((tn(ipond)+dttry(ipond)).gt.24.0) dttry(ipond) = 24.0 -
     1          tn(ipond)
          end if
c
          if (tn(ipond).ge.ti) then
            n = n + 1
            if (n.eq.1) dttry(ipond) = deltat(ipond)
          end if
c
          if (tn(ipond).lt.(ti+0.00001)) then
            hti = havg / ti
            qoti = volo(ipond) / (ti*3600.)
            ai = a0(ipond) + a1(ipond) * hti ** a2(ipond)
          end if
c
          if (ti.lt.0.0001) then
            hti = hmax(ipond)
            qoti = qomx(ipond)
          end if
c
c
          if (tn(ipond).lt.24.0) go to 40
c
        else
c
          itr = itr + 1
c         write (6,1060) t(ipond),h(ipond),qi,qo(ipond),itr,
c         1       ht(itr+1,ipond)
c
          go to 50
        end if
c
c       calculate the time of concentration for the impoundment
c
        if (qomx(ipond).gt.0.0) then
          tcf(ipond) = ((((vi(ipond)/qomx(ipond))/2.0)/3600.0)-(ti/2.0))
     1        / 0.60
        else
          tcf(ipond) = 0.0
        end if
c
c       set tcf(ipond) to zero if less than zero - technically
c       this should never happen (ti > to) but does because of
c       instabilities in the numerical solution for low flow
c       structures (i.e., filter fences, straw bales, rock fill
c       check dams, etc.)
c
        if (tcf(ipond).lt.0.0) tcf(ipond) = 0.0
c
c       restrict tcf(ipond) to 24 hours if greater than 24 hours
c       because of 24 hour hydrograph limitation
c
        if (tcf(ipond).gt.24.0) tcf(ipond) = 24.0
c
c       write (6,1000) ielmt, vi(ipond), ipond, tcf(ipond)
c
c       sedimentation simulation
c
c       reading values and setting variables
c
c       read (ipond+9,*) ciin(ipond)
c       read (ipond+9,*) pcl(ipond), psl(ipond), psd(ipond),
c       1      psa(ipond), pla(ipond)
c       read (ipond+9,*) cl50, sl50, sd50, sa50, la50
c       read (ipond+9,*)
c
        if (ciin(ipond).gt.cpkim(ipond)) cpkim(ipond) = ciin(ipond)
        if (ciin(ipond).gt.cpkiy(ipond)) cpkiy(ipond) = ciin(ipond)
        if (ciin(ipond).gt.cpkie(ipond)) cpkie(ipond) = ciin(ipond)
c
        ciin(ipond) = ciin(ipond) * 0.00006241778465
c
        cout(ipond) = 0.0
        clout(ipond) = 0.0
        slout(ipond) = 0.0
        saout(ipond) = 0.0
        sdout(ipond) = 0.0
        laout(ipond) = 0.0
c
        clt = 0.0
        slt = 0.0
        sat = 0.0
        sdt = 0.0
        lat = 0.0
c
        ret(ipond) = 0.0
        clret(ipond) = 0.0
        slret(ipond) = 0.0
        saret(ipond) = 0.0
        sdret(ipond) = 0.0
        laret(ipond) = 0.0
c
        i = 1
c
   60   if (cl50.lt.d100(i,ipond)) then
          perc = (log10(cl50)-log10(d0(i,ipond))) / (
     1        log10(d100(i,ipond))-log10(d0(i,ipond)))
          perc1 = float(i-1) + perc
          perc2 = 2.0 * float(ndiv(ipond)) - perc1
c
          do 70 ip = 1, i - 1
            ci(ip) = ciin(ipond) * pcl(ipond) / (2.0*perc1)
   70     continue
c
          ci(i) = (perc/perc1+(1.0-perc)/perc2) * ciin(ipond) *
     1        pcl(ipond) / 2.0
c
          do 80 ip = i + 1, 2 * ndiv(ipond)
            ci(ip) = ciin(ipond) * pcl(ipond) / (2.0*perc2)
   80     continue
c
        else
          i = i + 1
          go to 60
        end if
c
        i = 2 * ndiv(ipond) + 1
c
   90   if (sl50.lt.d100(i,ipond)) then
          perc = (log10(sl50)-log10(d0(i,ipond))) / (
     1        log10(d100(i,ipond))-log10(d0(i,ipond)))
          perc1 = float(i-(2*ndiv(ipond)+1)) + perc
          perc2 = 2.0 * float(ndiv(ipond)) - perc1
c
          do 100 ip = 2 * ndiv(ipond) + 1, i - 1
            ci(ip) = ciin(ipond) * psl(ipond) / (2.0*perc1)
  100     continue
c
          ci(i) = (perc/perc1+(1.0-perc)/perc2) * ciin(ipond) *
     1        psl(ipond) / 2.0
c
          do 110 ip = i + 1, 4 * ndiv(ipond)
            ci(ip) = ciin(ipond) * psl(ipond) / (2.0*perc2)
  110     continue
c
        else
          i = i + 1
          go to 90
        end if
c
        i = 4 * ndiv(ipond) + 1
c
  120   if (sa50.lt.d100(i,ipond)) then
          perc = (log10(sa50)-log10(d0(i,ipond))) / (
     1        log10(d100(i,ipond))-log10(d0(i,ipond)))
          perc1 = float(i-(4*ndiv(ipond)+1)) + perc
          perc2 = 2.0 * float(ndiv(ipond)) - perc1
c
          do 130 ip = 4 * ndiv(ipond) + 1, i - 1
            ci(ip) = ciin(ipond) * psa(ipond) / (2.0*perc1)
  130     continue
c
          ci(i) = (perc/perc1+(1.0-perc)/perc2) * ciin(ipond) *
     1        psa(ipond) / 2.0
c
          do 140 ip = i + 1, 6 * ndiv(ipond)
            ci(ip) = ciin(ipond) * psa(ipond) / (2.0*perc2)
  140     continue
c
        else
          i = i + 1
          go to 120
        end if
c
        i = 6 * ndiv(ipond) + 1
c
  150   if (sd50.lt.d100(i,ipond)) then
          perc = (log10(sd50)-log10(d0(i,ipond))) / (
     1        log10(d100(i,ipond))-log10(d0(i,ipond)))
          perc1 = float(i-(6*ndiv(ipond)+1)) + perc
          perc2 = 2.0 * float(ndiv(ipond)) - perc1
c
          do 160 ip = 6 * ndiv(ipond) + 1, i - 1
            ci(ip) = ciin(ipond) * psd(ipond) / (2.0*perc1)
  160     continue
c
          ci(i) = (perc/perc1+(1.0-perc)/perc2) * ciin(ipond) *
     1        psd(ipond) / 2.0
c
          do 170 ip = i + 1, 8 * ndiv(ipond)
            ci(ip) = ciin(ipond) * psd(ipond) / (2.0*perc2)
  170     continue
c
        else
          i = i + 1
          go to 150
        end if
c
        i = 8 * ndiv(ipond) + 1
c
  180   if (la50.lt.d100(i,ipond)) then
          perc = (log10(la50)-log10(d0(i,ipond))) / (
     1        log10(d100(i,ipond))-log10(d0(i,ipond)))
          perc1 = float(i-(8*ndiv(ipond)+1)) + perc
          perc2 = 2. * float(ndiv(ipond)) - perc1
c
          do 190 ip = 8 * ndiv(ipond) + 1, i - 1
            ci(ip) = ciin(ipond) * pla(ipond) / (2.0*perc1)
  190     continue
c
          ci(i) = (perc/perc1+(1.0-perc)/perc2) * ciin(ipond) *
     1        pla(ipond) / 2.0
c
          do 200 ip = i + 1, 10 * ndiv(ipond)
            ci(ip) = ciin(ipond) * pla(ipond) / (2.0*perc2)
  200     continue
c
        else
          i = i + 1
          go to 180
        end if
c
        do 210 i = 1, 10 * ndiv(ipond)
          coit(i) = 0.0
  210   continue
c
c       determining Ct and Cd
c
        if (qiin(ipond).ge.0.00001) then
c
          if (isize(ipond).eq.1) then
            vpi = 0.0
            vmx = 0.0
c
            do 220 i = 1, 30
c
              vpi = vpi + ((a0(ipond)+a1(ipond)*(float(i)*hti/30.)**
     1            a2(ipond))+(a0(ipond)+a1(ipond)*(float(i-1)*hti/30.)**
     1            a2(ipond))) / 2. * (float(i)*hti/30.-float(i-1)*hti/
     1            30.)
c
              vmx = vmx + ((a0(ipond)+a1(ipond)*(float(i)*hmax(ipond)/
     1            30.)**a2(ipond))+(a0(ipond)+a1(ipond)*(float(i-1)*
     1            hmax(ipond)/30.)**a2(ipond))) / 2. * (float(i)*
     1            hmax(ipond)/30.-float(i-1)*hmax(ipond)/30.)
c
  220       continue
c
            vpi = vpi / 43560.0
            vmx = vmx / 43560.0
            hihr = (hti-ht(2,ipond))
            if(hihr.le.0.0) hihr = 0.0
            qoqi = qoti / qiin(ipond)
            qoqi2 = qoqi ** 2
            vmxvi = vmx / vi(ipond)
            qoaivs = (qoti/ai) / 0.00001124
c
            do 230 i = 1, 2 * ndiv(ipond)
              ctd(i,ipond) = 0.039954 + 0.011496 * qoaivs
              cdp(i,ipond) = 4.0665
  230       continue
c
            do 240 i = 2 * ndiv(ipond) + 1, 4 * ndiv(ipond)
              ctd(i,ipond) = 0.013980 + 0.110453 * qoqi
              cdp(i,ipond) = 0.755060 + 1.304780 * vmxvi + 0.131947 *
     1            vpi
  240       continue
c
            do 250 i = 4 * ndiv(ipond) + 1, 6 * ndiv(ipond)
              ctd(i,ipond) = 0.014586 + 0.127389 * qoqi2
              cdp(i,ipond) = 0.466084 + 2.752976 * vmxvi + 0.058282 *
     1            vmx
  250       continue
c
            qoaivs = (qoti/ai) / 0.0063578458
c
            do 260 i = 6 * ndiv(ipond) + 1, 8 * ndiv(ipond)
              ctd(i,ipond) = 0.005945 + 0.254620 * qoaivs
              cdp(i,ipond) = 0.63185 * qoaivs
  260       continue
c
            qoaivs = (qoti/ai) / 0.1637373
c
            do 270 i = 8 * ndiv(ipond) + 1, 10 * ndiv(ipond)
              ctd(i,ipond) = 0.005527 + 12.588091 * qoaivs
              cdp(i,ipond) = 41.670461 * qoaivs + 0.004962 * hihr
  270       continue
c
          else
c
            hihr8 = (hti-ht(2,ipond)) / hti
            ar = a0(ipond) + a1(ipond) * ht(2,ipond) ** a2(ipond)
            qoqi = qoti / qiin(ipond)
            vmx = 0.0
c
            do 280 i = 1, 30
              vmx = vmx + ((a0(ipond)+a1(ipond)*(float(i)*hmax(ipond)/
     1            30.)**a2(ipond))+(a0(ipond)+a1(ipond)*(float(i-1)*
     1            hmax(ipond)/30.)**a2(ipond))) / 2. * (float(i)*
     1            hmax(ipond)/30.-float(i-1)*hmax(ipond)/30.)
  280       continue
c
            vmxvi = vmx / vi(ipond)
c
            do 290 i = 1, 2 * ndiv(ipond)
              qoaivs = qoti / ai / 0.00001124
              ctd(i,ipond) = 0.100954 + 0.049397 * qoaivs - 0.117660 *
     1            hihr8
              if (ctd(i,ipond).lt.0.0005) ctd(i,ipond) = 0.0005
              cdp(i,ipond) = 1.0
  290       continue
c
            do 300 i = 2 * ndiv(ipond) + 1, 4 * ndiv(ipond)
              ctd(i,ipond) = 0.002119 + 0.125354 * qoqi
              cdp(i,ipond) = 0.001641 * vmx / 43560.0 + 3.830727 * hihr8
              if (ctd(i,ipond).lt.0.004) ctd(i,ipond) = 0.004
              if (cdp(i,ipond).lt.0.0005) cdp(i,ipond) = 0.0005
  300       continue
c
            do 310 i = 4 * ndiv(ipond) + 1, 6 * ndiv(ipond)
              ctd(i,ipond) = -0.040098 + 0.193138 * qoqi + 0.041245 *
     1            vmxvi
              cdp(i,ipond) = 0.004276 * vmx / 43560.0 + 3.123986 * hihr8
              if (ctd(i,ipond).lt.0.005) ctd(i,ipond) = 0.005
              if (cdp(i,ipond).lt.0.0005) cdp(i,ipond) = 0.0005
  310       continue
c
            do 320 i = 6 * ndiv(ipond) + 1, 8 * ndiv(ipond)
              qoarvs = qoti / ar / 0.063578458
              qoaive = 1.0 - exp(-qoti/ai/0.063578458)
              ctd(i,ipond) = 0.003719 + 3.104505 * qoaive - 0.004918 *
     1            hihr8
              cdp(i,ipond) = -0.074626 + 17.705171 * qoarvs + 0.000178 *
     1            vi(ipond) / 43560.0
              if (ctd(i,ipond).lt.0.002) ctd(i,ipond) = 0.002
              if (cdp(i,ipond).lt.0.0005) cdp(i,ipond) = 0.0005
  320       continue
c
            do 330 i = 8 * ndiv(ipond) + 1, 10 * ndiv(ipond)
              qoarvs = qoti / ar / 0.1637373
              qoaive = 1.0 - exp(-qoti/ai/0.1637373)
              ctd(i,ipond) = 0.008442 + 12.443736 * qoaive - 0.011869 *
     1            hihr8
              cdp(i,ipond) = -0.575718 + 0.358606 * vmxvi + 172.424594 *
     1            qoarvs
              if (ctd(i,ipond).lt.0.006) ctd(i,ipond) = 0.006
              if (cdp(i,ipond).lt.0.0005) cdp(i,ipond) = 0.0005
  330       continue
c
          end if
        end if
c
c       reading hydraulics file
c
        volo(ipond) = 0.0
c
        totco(ipond) = 0.0
        totdep = 0.0
        cpeak(ipond) = 0.0
c
        rewind (50)
        read (50) t(ipond), h(ipond), qo(ipond)
c
        cot(ipond) = cot(ipond) / 0.00006241778465
c
        if ((npond.eq.1).and.(ipdout.eq.1)) write (55,1000) t(ipond),
     1      h(ipond), qo(ipond), cot(ipond)
c
        cot(ipond) = cot(ipond) * 0.00006241778465
        ar = a0(ipond) + a1(ipond) * h(ipond) ** a2(ipond)
c        added by S.Dun July 1,1999
        if(h(ipond).le.0.0) h(ipond)=0.00001
c        end adding
        l = l0(ipond) + l1(ipond) * h(ipond) ** l2(ipond)
c
        do 380 is = 1, ig
c
          read (50) tn(ipond), hn(ipond), qon(ipond)
c
          qi = 0.0
          if (t(ipond).lt.ti) qi = qiin(ipond)
c
c         determining deposition
c
          an = a0(ipond) + a1(ipond) * hn(ipond) ** a2(ipond)
          ln = l0(ipond) + l1(ipond) * hn(ipond) ** l2(ipond)
c
          voln = 0.0
c
          do 340 i = 1, 20
            voln = voln + ((a0(ipond)+a1(ipond)*(float(i)*hn(ipond)/20.)
     1          **a2(ipond))+(a0(ipond)+a1(ipond)*(float(i-1)*
     1          hn(ipond)/20.)**a2(ipond))) / 2.0 * (float(i)*
     1          hn(ipond)/20.-float(i-1)*hn(ipond)/20.)
  340     continue
c
          voln = voln - volm
c
          if (voln.le.volm) voln = 0.00001
          voli = voli + (tn(ipond)-t(ipond)) * 3600. * qi
          volo(ipond) = volo(ipond) + (tn(ipond)-t(ipond)) * 3600. * (
     1        qon(ipond)+qo(ipond)) / 2.0
          w = ((ar+an)/2.0) / ((l+ln)/2.0)
c
          if ((((l+ln)/2.0)/w).ge.2.0) ds = 0.15
          if ((((l+ln)/2.0)/w).lt.2.0) ds = 0.25
c
          cont = 0.0
          dept = 0.0
c
          do 350 i = 1, 10 * ndiv(ipond)
c
            if (qo(ipond).le.0.0) then
              rt(i) = 1.0
            else
              td = ctd(i,ipond) * (1.0-ds) * ((vol+voln)/2.0) /
     1            qo(ipond)
              td100 = (1.0-ds) * ((vol+voln)/2.0) / ((ar+an)/2.0) /
     1            vs(i,ipond)
c
              if ((td/td100).ge.1.0) rt(i) = 1.0
              if ((td/td100).lt.1.0) rt(i) = td / td100
c
            end if
c
            if (qi.le.0.0) dep(i) = cdp(i,ipond) * rt(i) * co(i,ipond) *
     1          vs(i,ipond) * ((ar+an)/2.0) * (tn(ipond)-t(ipond)) *
     1          3600.0
c
c           if (qi.le.0.0) then
c           if (vs(i,ipond)*(ar+an)/2.0*(tn(ipond)-t(ipond))*3600.0
c           1            .le.(vol+voln)/2.0) then
c           dep(i) = cdp(i,ipond) * rt(i) * co(i,ipond) *
c           1       vs(i,ipond) * ((ar+an)/2.0) * (tn(ipond)-t(ipond)) *
c           1       3600.0
c           else
c           dep(i) = cdp(i,ipond) * rt(i) * co(i,ipond) *
c           1            (vol+voln)/2.0 * (tn(ipond)-t(ipond)) * 3600.0
c           endif
c           endif
c
            if (qi.gt.0.0) dep(i) = rt(i) * ci(i) * qi * (tn(ipond)-
     1          t(ipond)) * 3600.0
c
c           new overall and size class outflow concentrations
c
            con(i) = (co(i,ipond)*((1.0-ds)*(vol+voln)/2.-(tn(ipond)-
     1          t(ipond))/2.*3600.*qi)+qi*ci(i)*(tn(ipond)-t(ipond))*
     1          3600.-dep(i)) / ((tn(ipond)-t(ipond))*3600./2.*qi+(1.0-
     1          ds)*(vol+voln)/2.)
c
            if (con(i).le.0.0) then
              con(i) = 0.0
              dep(i) = co(i,ipond) * ((1.0-ds)*(vol+voln)/2.-(
     1            tn(ipond)-t(ipond))/2.*3600.*qi) + qi * ci(i) * (
     1            tn(ipond)-t(ipond)) * 3600.
            end if
c
            cont = cont + con(i)
            dept = dept + dep(i) / (sg(i,ipond)*62.4)
            coit(i) = coit(i) + 3600. * (tn(ipond)-t(ipond)) * (
     1          qon(ipond)+qo(ipond)) / 2. * (con(i)+co(i,ipond)) / 2.0
c
  350     continue
c
          totco(ipond) = ((cont+cot(ipond))/2.0) * (tn(ipond)-t(ipond))
     1        * 3600. * (qo(ipond)+qon(ipond)) / 2. + totco(ipond)
          totdep = totdep + dept
          cont = cont / 0.00006241778465
c
          if (cont.gt.cpeak(ipond)) cpeak(ipond) = cont
          if (cont.gt.cpeakm(ipond)) cpeakm(ipond) = cont
          if (cont.gt.cpeaky(ipond)) cpeaky(ipond) = cont
          if (cont.gt.cpeake(ipond)) cpeake(ipond) = cont
c
          if ((npond.eq.1).and.(ipdout.eq.1)) write (55,1000)
     1        tn(ipond), hn(ipond), qon(ipond), cont
c
          cont = cont * 0.00006241778465
          havg = ((hn(ipond)-h(ipond))/2.0+h(ipond)) * (tn(ipond)-
     1        t(ipond)) + havg
c
          do 360 i = 1, 2 * ndiv(ipond)
            clout(ipond) = clout(ipond) + 3600. * (tn(ipond)-t(ipond)) *
     1          (qon(ipond)+qo(ipond)) / 2. * (con(i)+co(i,ipond)) / 2.
            slout(ipond) = slout(ipond) + 3600. * (tn(ipond)-t(ipond)) *
     1          (qon(ipond)+qo(ipond)) / 2. * (con(i+2*ndiv(ipond))+
     1          co(i+2*ndiv(ipond),ipond)) / 2.
            saout(ipond) = saout(ipond) + 3600. * (tn(ipond)-t(ipond)) *
     1          (qon(ipond)+qo(ipond)) / 2. * (con(i+4*ndiv(ipond))+
     1          co(i+4*ndiv(ipond),ipond)) / 2.
            sdout(ipond) = sdout(ipond) + 3600. * (tn(ipond)-t(ipond)) *
     1          (qon(ipond)+qo(ipond)) / 2. * (con(i+6*ndiv(ipond))+
     1          co(i+6*ndiv(ipond),ipond)) / 2.
            laout(ipond) = laout(ipond) + 3600. * (tn(ipond)-t(ipond)) *
     1          (qon(ipond)+qo(ipond)) / 2. * (con(i+8*ndiv(ipond))+
     1          co(i+8*ndiv(ipond),ipond)) / 2.
  360     continue
c
          cot(ipond) = cont
c
          do 370 i = 1, 10 * ndiv(ipond)
            co(i,ipond) = con(i)
  370     continue
c
          ar = an
          l = ln
          vol = voln
          t(ipond) = tn(ipond)
          h(ipond) = hn(ipond)
          qo(ipond) = qon(ipond)
c
  380   continue
c
c       determining outputs, peak outflow, volume, and
c       average sediment concentration
c
        if (volo(ipond).le.0.0) ca(ipond) = 0.0
        if (volo(ipond).gt.0.0) ca(ipond) = (totco(ipond)/volo(ipond)) /
     1      0.00006241778465
c
        qomx(ipond) = qomx(ipond) / 3.281 ** 3.0
        volo(ipond) = volo(ipond) / 3.281 ** 3.0
c
        if ((npond.eq.1).and.(ipdout.eq.1)) write (54,1000)
     1      qomx(ipond), volo(ipond), ca(ipond)
c
        qomx(ipond) = qomx(ipond) * 3.281 ** 3.0
        volo(ipond) = volo(ipond) * 3.281 ** 3.0
c
        cout(ipond) = totco(ipond)
        if (cout(ipond).le.0.0) cout(ipond) = 0.00001
c
c       percent in size class
c
        clot = clout(ipond) / cout(ipond)
        slot = slout(ipond) / cout(ipond)
        saot = saout(ipond) / cout(ipond)
        sdot = sdout(ipond) / cout(ipond)
        laot = laout(ipond) / cout(ipond)
c
        if (clot.gt.1.0) then
          clot = 1.0
        else if (slot.gt.1.0) then
          slot = 1.0
        else if (saot.gt.1.0) then
          saot = 1.0
        else if (sdot.gt.1.0) then
          sdot = 1.0
        else if (laot.gt.1.0) then
          laot = 1.0
        end if
c
        if ((npond.eq.1).and.(ipdout.eq.1)) write (54,1000) clot, slot,
     1      sdot, saot, laot
c
c       determining new d50's
c
        icnt = 0
c
        do 390 i = 1, 2 * ndiv(ipond)
c
          if (icnt.lt.1) then
c
            if (clout(ipond).le.0.001) then
              cl50o = d0(1,ipond)
              go to 400
            else
              clt = clt + coit(i)
              pot(i) = clt / clout(ipond)
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.1) then
                  cl50o = 10.0 ** (log10(d0(i,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(i,ipond))))
                else
                  cl50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  390   continue
c
        icnt = 0
c
  400   do 410 i = (2*ndiv(ipond)+1), 4 * ndiv(ipond)
c
          if (icnt.lt.1) then
c
            if (slout(ipond).le.0.001) then
              sl50o = d0(2*ndiv(ipond)+1,ipond)
              go to 420
            else
              slt = slt + coit(i)
              pot(i) = slt / slout(ipond)
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.(2*ndiv(ipond)+1)) then
                  sl50o = 10.0 ** (log10(d0(i,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(i,ipond))))
                else
                  sl50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  410   continue
c
        icnt = 0
c
  420   do 430 i = (4*ndiv(ipond)+1), 6 * ndiv(ipond)
c
          if (icnt.lt.1) then
c
            if (saout(ipond).le.0.001) then
              sa50o = d0(4*ndiv(ipond)+1,ipond)
              go to 440
            else
              sat = sat + coit(i)
              pot(i) = sat / saout(ipond)
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.(4*ndiv(ipond)+1)) then
                  sa50o = 10.0 ** (log10(d0(i,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(i,ipond))))
                else
                  sa50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  430   continue
c
        icnt = 0
c
  440   do 450 i = (6*ndiv(ipond)+1), 8 * ndiv(ipond)
c
          if (icnt.lt.1) then
c
            if (sdout(ipond).le.0.001) then
              sd50o = d0(6*ndiv(ipond)+1,ipond)
              go to 460
            else
              sdt = sdt + coit(i)
              pot(i) = sdt / sdout(ipond)
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.(6*ndiv(ipond)+1)) then
                  sd50o = 10.0 ** (log10(d0(i,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(i,ipond))))
                else
                  sd50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  450   continue
c
        icnt = 0
c
  460   do 470 i = (8*ndiv(ipond)+1), 10 * ndiv(ipond)
c
          if (icnt.lt.1) then
c
            if (laout(ipond).le.0.001) then
              la50o = d0(8*ndiv(ipond)+1,ipond)
              go to 480
            else
              lat = lat + coit(i)
              pot(i) = lat / laout(ipond)
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.(8*ndiv(ipond)+1)) then
                  la50o = 10.0 ** (log10(d0(1,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(i,ipond))))
                else
                  la50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  470   continue
c
  480   sa50o = sa50o / (0.8/1.65) ** 0.5
        la50o = la50o / (0.6/1.65) ** 0.5
c
        if ((npond.eq.1).and.(ipdout.eq.1)) write (54,1000) cl50o,
     1      sl50o, sd50o, sa50o, la50o
c
c       adjusting stage for evaporation, infiltration, and deposition
c
        h(ipond) = h(ipond) - qinf(ipond) - 0.7 * pet
        vold = 0.0
c
  490   voldep = vold + 0.01 * ((a0(ipond)+a1(ipond)*(hmin(ipond)+0.01)
     1      **a2(ipond))+(a0(ipond)+a1(ipond)*(hmin(ipond))**a2(ipond)))
     1      / 2
c
        if (voldep.gt.totdep) then
          hmin(ipond) = hmin(ipond) + 0.01 * (totdep-vold) / (voldep-
     1        vold)
        else
          hmin(ipond) = hmin(ipond) + 0.01
          vold = voldep
          go to 490
        end if
c
c       need to put a maximum value on totdep or on hmin,
c       otherwise the variable totdep (total volume deposited) can
c       get very big which gives impossibly high values of hmin
c
        if (h(ipond).lt.hmin(ipond)) then
          h(ipond) = hmin(ipond)
          qo(ipond) = 0.0
        end if
c
      end if
c
      do 500 i = 1, 10 * ndiv(ipond)
        hset(i,ipond) = h(ipond)
  500 continue
c
c     convert outflow volume from ft^3 to m^3
c
      volo(ipond) = volo(ipond) / 3.281 ** 3
c
c     convert sediment from lbs to kg
c
      cout(ipond) = totco(ipond) * 0.4536
      clout(ipond) = clout(ipond) * 0.4536
      slout(ipond) = slout(ipond) * 0.4536
      saout(ipond) = saout(ipond) * 0.4536
      sdout(ipond) = sdout(ipond) * 0.4536
      laout(ipond) = laout(ipond) * 0.4536
c
      if (cpeak(ipond).lt.ca(ipond)) cpeak(ipond) = ca(ipond)
c
c     go to 690
      return
c
c     determining deposition during no flow conditions
c
  510 continue
c
      volo(ipond) = 0.0
      qomx(ipond) = 0.0
c
      cout(ipond) = 0.0
      clout(ipond) = 0.0
      slout(ipond) = 0.0
      saout(ipond) = 0.0
      sdout(ipond) = 0.0
      laout(ipond) = 0.0
c
      clot = 0.0
      slot = 0.0
      sdot = 0.0
      saot = 0.0
      laot = 0.0
c
      ret(ipond) = 0.0
      clret(ipond) = 0.0
      slret(ipond) = 0.0
      saret(ipond) = 0.0
      sdret(ipond) = 0.0
      laret(ipond) = 0.0
c
      ca(ipond) = 0.0
      cpeak(ipond) = 0.0
c
      cl50o = d0(1,ipond)
      sl50o = d0(2*ndiv(ipond)+1,ipond)
      sd50o = d0(6*ndiv(ipond)+1,ipond)
      sa50o = d0(4*ndiv(ipond)+1,ipond)
      la50o = d0(8*ndiv(ipond)+1,ipond)
c
      if ((npond.eq.1).and.(ipdout.eq.1)) then
        write (54,1000) qomx(ipond), volo(ipond), cout(ipond)
        write (54,1000) clot, slot, sdot, saot, laot
        write (54,1000) cl50o, sl50o, sd50o, sa50o, la50o
      end if
c
c     read (ipond+9,*) ciin(ipond)
c     read (ipond+9,*) pcl(ipond), psl(ipond), psd(ipond),
c     1    psa(ipond), pla(ipond)
c     read (ipond+9,*) cl50, sl50, sd50, sa50, la50
c     read (ipond+9,*)
c
      if (h(ipond).gt.hmin(ipond)) then
c
        totdep = 0.0
c
        clset = 0.0
        slset = 0.0
        saset = 0.0
        sdset = 0.0
        laset = 0.0
c
        clt = 0.0
        slt = 0.0
        sat = 0.0
        sdt = 0.0
        lat = 0.0
c
        volm = 0.0
c
        do 520 i = 1, 5
          volm = volm + ((a0(ipond)+a1(ipond)*(float(i)*hmin(ipond)/5.)
     1        **a2(ipond))+(a0(ipond)+a1(ipond)*(float(i-1)*
     1        hmin(ipond)/5.)**a2(ipond))) / 2.0 * (float(i)*
     1        hmin(ipond)/5.-float(i-1)*hmin(ipond)/5.)
  520   continue
c
        vol = 0.0
c
        do 530 i = 1, 20
          vol = vol + ((a0(ipond)+a1(ipond)*(float(i)*h(ipond)/20.)**
     1        a2(ipond))+(a0(ipond)+a1(ipond)*(float(i-1)*h(ipond)/20.)
     1        **a2(ipond))) / 2.0 * (float(i)*h(ipond)/20.-float(i-1)*
     1        h(ipond)/20.)
  530   continue
c
        vol = vol - volm
c
        do 550 i = 1, 10 * ndiv(ipond)
          hset(i,ipond) = hset(i,ipond) - vs(i,ipond) * 24.0 * 3600.0
c
          if (hset(i,ipond).lt.hmin(ipond)) then
            hset(i,ipond) = hmin(ipond)
            aset(i) = a0(ipond) + a1(ipond) * hset(i,ipond) **
     1          a2(ipond)
            volset(i) = 0.0
            cset(i) = 0.0
            dep(i) = co(i,ipond) * vol
            co(i,ipond) = 0.0
          else
            aset(i) = a0(ipond) + a1(ipond) * hset(i,ipond) **
     1          a2(ipond)
            volset(i) = 0.0
c
            do 540 iv = 1, 20
              volset(i) = volset(i) + ((a0(ipond)+a1(ipond)*(float(iv)*
     1            hset(i,ipond)/20.)**a2(ipond))+(a0(ipond)+a1(ipond)*(
     1            float(iv-1)*hset(i,ipond)/20.)**a2(ipond))) / 2.0 * (
     1            float(iv)*hset(i,ipond)/20.-float(iv-1)*
     1            hset(i,ipond)/20.)
  540       continue
c
            volset(i) = volset(i) - volm
            cset(i) = co(i,ipond) * vol / volset(i)
            dep(i) = cset(i) * vs(i,ipond) * aset(i) * 24. * 3600.
c
            if (dep(i).gt.(co(i,ipond)*vol)) then
              dep(i) = co(i,ipond) * vol
              co(i,ipond) = 0.0
            else
              co(i,ipond) = (co(i,ipond)*vol-dep(i)) / vol
            end if
c
          end if
c
          totdep = totdep + dep(i) / (sg(i,ipond)*62.4)
          ret(ipond) = ret(ipond) + dep(i) + co(i,ipond) * vol
  550   continue
c
        do 560 i = 1, 2 * ndiv(ipond)
          clset = clset + co(i,ipond) * vol
          clret(ipond) = clret(ipond) + dep(i) + co(i,ipond) * vol
          slset = slset + co(i+2*ndiv(ipond),ipond) * vol
          slret(ipond) = slret(ipond) + dep(i+2*ndiv(ipond)) +
     1        co(i+2*ndiv(ipond),ipond) * vol
          saset = saset + co(i+4*ndiv(ipond),ipond) * vol
          saret(ipond) = saret(ipond) + dep(i+4*ndiv(ipond)) +
     1        co(i+4*ndiv(ipond),ipond) * vol
          sdset = sdset + co(i+6*ndiv(ipond),ipond) * vol
          sdret(ipond) = sdret(ipond) + dep(i+6*ndiv(ipond)) +
     1        co(i+6*ndiv(ipond),ipond) * vol
          laset = laset + co(i+8*ndiv(ipond),ipond) * vol
          laret(ipond) = laret(ipond) + dep(i+8*ndiv(ipond)) +
     1        co(i+8*ndiv(ipond),ipond) * vol
  560   continue
c
c       determining new d50's
c
        icnt = 0
c
        do 570 i = 1, 2 * ndiv(ipond)
          if (icnt.lt.1) then
c
            if (clset.le.0.001) then
              cl50o = d0(1,ipond)
              go to 580
            else
              clt = clt + co(i,ipond) * vol
              pot(i) = clt / clset
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.1) then
                  cl50o = 10.0 ** (log10(d0(i,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(i,ipond))))
                else
                  cl50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  570   continue
c
        icnt = 0
c
  580   do 590 i = (2*ndiv(ipond)+1), 4 * ndiv(ipond)
c
          if (icnt.lt.1) then
c
            if (slset.le.0.001) then
              sl50o = d0(2*ndiv(ipond)+1,ipond)
              go to 600
            else
              slt = slt + co(i,ipond) * vol
              pot(i) = slt / slset
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.(2*ndiv(ipond)+1)) then
                  sl50o = 10.0 ** (log10(d0(i,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(i,ipond))))
                else
                  sl50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  590   continue
c
        icnt = 0
c
  600   do 610 i = (4*ndiv(ipond)+1), 6 * ndiv(ipond)
c
          if (icnt.lt.1) then
c
            if (saset.le.0.001) then
              sa50o = d0(4*ndiv(ipond)+1,ipond)
              go to 620
            else
              sat = sat + co(i,ipond) * vol
              pot(i) = sat / saset
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.(4*ndiv(ipond)+1)) then
                  sa50o = 10.0 ** (log10(d0(i,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(i,ipond))))
                else
                  sa50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  610   continue
c
        icnt = 0
c
  620   do 630 i = (6*ndiv(ipond)+1), 8 * ndiv(ipond)
c
          if (icnt.lt.1) then
c
            if (sdset.le.0.001) then
              sd50o = d0(6*ndiv(ipond)+1,ipond)
              go to 640
            else
              sdt = sdt + co(i,ipond) * vol
              pot(i) = sdt / sdset
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.(6*ndiv(ipond)+1)) then
                  sd50o = 10.0 ** (log10(d0(i,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(i,ipond))))
                else
                  sd50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  630   continue
c
        icnt = 0
c
  640   do 650 i = (8*ndiv(ipond)+1), 10 * ndiv(ipond)
c
          if (icnt.lt.1) then
c
            if (laset.le.0.001) then
              la50o = d0(8*ndiv(ipond)+1,ipond)
              go to 660
            else
              lat = lat + co(i,ipond) * vol
              pot(i) = lat / laset
c
              if (pot(i).ge.0.5) then
                icnt = icnt + 1
c
                if (i.gt.(8*ndiv(ipond)+1)) then
                  la50o = 10.0 ** (log10(d0(i,ipond))+(0.5-pot(i-1))/(
     1                pot(i)-pot(i-1))*(log10(d100(i,ipond))-
     1                log10(d0(1,ipond))))
                else
                  la50o = 10.0 ** (log10(d0(i,ipond))+0.5/pot(i)*(
     1                log10(d100(i,ipond))-log10(d0(i,ipond))))
                end if
c
              end if
            end if
          end if
  650   continue
c
  660   sa50o = sa50o / (0.8/1.65) ** 0.5
        la50o = la50o / (0.6/1.65) ** 0.5
c
c       adjusting stage for evaporation, infiltration, and deposition
c
        hmax(ipond) = h(ipond)
        h(ipond) = h(ipond) - qinf(ipond) - 0.7 * pet
        vold = 0.0
c
  670   voldep = vold + 0.01 * ((a0(ipond)+a1(ipond)*(hmin(ipond)+0.01)
     1      **a2(ipond))+(a0(ipond)+a1(ipond)*(hmin(ipond))**a2(ipond)))
     1      / 2
c
        if (voldep.gt.totdep) then
          hmin(ipond) = hmin(ipond) + 0.01 * (totdep-vold) / (voldep-
     1        vold)
        else
          hmin(ipond) = hmin(ipond) + 0.01
          vold = voldep
          go to 670
        end if
c
        if (h(ipond).lt.hmin(ipond)) h(ipond) = hmin(ipond)
c
        do 680 i = 1, 10 * ndiv(ipond)
          if (hset(i,ipond).ge.h(ipond)) hset(i,ipond) = h(ipond)
  680   continue
c
      else
c
        h(ipond) = hmin(ipond)
        hmax(ipond) = hmin(ipond)
      end if
c
      return
c
c1000 format ('ELEMENT = ',i3,' RUNOFF = ',f10.4,' IPOND = ',i3,
c     1    ' TOC = ',f10.4)
 1000 format (5(f12.6,','))
 1100 format (6x,'ROUTING runoff through impoundment ',i2)
      end
