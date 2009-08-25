      subroutine irs(nowcrp,wmelt,ibrkpt,runmax,pkrmax,pkefdn,effdrr)
c
c     + + + PURPOSE + + +
c     Controls the hydrologic routing.
c
c     Called from CONTIN.
c     Author(s): Stone
c     Reference in User Guide:
c
c     Changes:
c         1) Parameter QOUT (4th) not used.  Deleted.
c         2) Parameter order changed to conform to coding convention.
c            IUPRUN (2nd) & EFFLEN (3rd) moved after NBEG; RUNMAX
c            (6th), PKRMAX (7th), & PKEFDN (8th) moved after EFFLEN;
c            and EFFDUR (11th) moved after PKEFDN.
c         3) Parameter order and/or number changed in calls to:
c            RDAT, ROCHEK, & EPLANE.
c         4) Common blocks CNTOUR, ROZERO, & WATER not used.
c            They were de-referenced.
c         5) Local variable RUNON not used.  Deleted.
c         6) Local variable JUMPFG added to handle jumps to outside
c            of IF-THEN-ELSE structure.
c         7) Corrections from Stone for duration of rainfall excess.
c            May-June 1993.   dcf
c         8) Addition of TOTRUN argument and calculations to predict
c            correct runoff depth for output from multiple OFE's.
c            Stone request implemented by dcf.  6/11/93
c         9) Moved ALPHAY to common block cprams2.inc jca2  8/31/93
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 05/08/91.
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
      include 'pmxslp.inc'
      include 'pmxtim.inc'
      include 'pntype.inc'

      include 'pmxtil.inc'
      include 'pmxtls.inc'

c
c     + + + ARGUMENT DECLARATIONS + + +
      real wmelt(mxplan), runmax
      real pkrmax, pkefdn, effdrr(mxplan)
      integer nowcrp(mxplan),ibrkpt
c
c     + + + ARGUMENT DEFINITIONS + + +
c     wmelt  - amount snow melt
c     runmax - maximum runoff volume
c     pkrmax - maximum peak discharge
c     pkefdn - maximum runoff duration
c     effdrr - duration of rainfall excess
c     nowcrp - number of current crop
c
c     + + + COMMON BLOCKS + + +
      include 'cavepar.inc'
c     modify: aveks(mxplan), avesm(mxplan)
c
      include 'cdata1.inc'
c     MODIFY: TR(MXTIME), R(MXTIME), RR(MXTIME)
c
      include 'cdata2.inc'
c
      include 'cefflen.inc'
c
      include 'chydrol.inc'
c     modify: runoff(mxplan), remax, durexr, peakro(mxplan), effdrn
c      write: effint(mxplan)
c
      include 'cirriga.inc'
c     modify: irfrac
c       read: irdept(mxplan)
c
      include 'cirspri.inc'
c       read: irnit(mxplan)
c
      include 'cnew.inc'
c       read: elev
c
      include 'cpass1.inc'
c      write: s(mxtime)
c
      include 'cpass2.inc'
c      write: t(mxtime)
c
      include 'cpass3.inc'
c     modify: ns
c
      include 'cprams.inc'
c       read: m
c     modify: alpha(mxplan)
c      write: norun(mxplan)
c
      include 'cslpopt.inc'
c       read: totlen
c
      include 'cstruc.inc'
c     modify: iplane
c
      include 'csumirr.inc'
c     modify: irrund(mxplan),irrunm(13,mxplan),irrunt(mxplan),
c             irruny(mxplan)
c
      include 'ccntour.inc'
c     read: contrs(conseq(mxcrop,mxplan))
c
      include 'cconsts.inc'
c      write: a1,a2
c
      include 'ccrpout.inc'
c       read: rrc(mxplan)
c
      include 'cdata3.inc'
c       read: nf
c
      include 'cdiss3.inc'
c       read: p
c
      include 'cdist2.inc'
c       read: slplen(mxplan)
c
      include 'cparame.inc'
c       read: ks,km
c
      include 'cslope2.inc'
c       read: avgslp
c
      include 'cstmflg.inc'
c       read: nmon
c
      include 'coutfg.inc'
c       read: lunp,luns,lunw
c
      include 'cxmxint.inc'
c       read: xmxint(mxplan)
c
      include 'cupsfl.inc'
c     modify: iuprun(mxplan)
c
      include 'ccntfg.inc'
c       read: cntflg
c
c     Added by S. Dun, June 15, 2007 for debug
      include 'cupdate.inc'
c     End adding
c
c     + + + LOCAL VARIABLES + + +
      real aveksm, ealpha, sumks, sumsm, tf(mxtime)
      real dep(mxplan), sumdep, avedep, re(mxtime)
      real avere, runtmp(mxplan), wmlvar, wmllen
      real maxrun, surpls,drlast,durre
      integer ibpln, iepln, xnpln, i, apr, nstemp, kplane, jumpfg
      integer jmpfg2, k,it,ii,j,ipl,l
cd    for containing maximum runoff for calculating peak runoff
c
c     + + + LOCAL DEFINITIONS + + +
c     aveksm - average KS*SM
c     ealpha - alpha for the equivalent plane
c     sumks  - summation of KS for AVEKS
c     sumsm  - summation of SM for AVESM
c     tf     - time array for rainfall excess
c     dep    - potential depression storage depth
c     sumdep - summation of depression storage
c     avedep - average depressional storage for equivalent plane
c     re     - rainfall excess rate (m/s)
c     avere  - average rainfall excess rate
c     ibpln  - 1st OFE which has runoff
c     iepln  - last OFE for which there is runoff on all OFE's above
c     xnpln  - number of OFE's, from IBPLN to IEPLN.
c     i      - index for IPLANE
c     apr    - flag.  Indicates when approximate method should be used.
c     nstemp - index for the last time of cumulative rainfall excess.
c     kplane - counter to indicate last OFE for equivalent plane calcs.
c              For Case 2 & 3 hydrologic planes kplane is the last plane
c              runoff exits.  For a Case 4 situation, kplane is set to
c              the OFE number preceding the Case 4 OFE.
c     jumpfg - 0=execute next section of code; 1=skip to "M1 IF";
c              2=skip to end of "do 35" loop
c     jmpfg2 - 0=execute next section of code; 1=skip to end of L00 IF
c     drlast - variable to hold last value of DURRE for multiple OFE
c              hillslopes - this is to prevent divide by zero values
c              for multiple OFE hillslopes and Case 3 hydrologic planes
c     runtmp - temporary array to hold runoff volumes decreased by
c              infiltration recession until the peak flow is computed
c              by subroutine QINF.  (added by Jeff Stone, 2/98)
c     maxrun - added by S. Dun to contain maximum runoff for calculating
c              peak runoff.
c     surpls - added by S. Dun to contain maximum runoff for calculating
c              peak runoff.
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c     idat
c     grna
c     frcfac
c     rdat
c     rochek
c     eplane
c     appmth
c     hdrive
c     qinf
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
      drlast = 0.0
      apr = 0
      iuprun(1) = 0
      sumks = 0.0
      sumsm = 0.0
      sumdep = 0.0
      pkrmax = 0.0
      runmax = 0.0
      ibpln = 1
      iepln = 1
c
      do 10 l = 1, nplane
        efflen(l) = 0.0
        effint(l) = 0.0
        peakro(l) = 0.0
        effdrn(l) = 0.0
        alpha(l) = 0.0
        effdrr(l) = 0.0
        runtmp(l) = 0.0 
   10 continue
c
      do 15 i = 1, mxtime
        tf(i) = 0.0
        re(i) = 0.0
   15 continue
c
c     Loop through the OFEs and compute the infiltration parameters for
c     the equivalent planes.  Compute infiltration and rainfall excess.
c     Compute the peak discharge rate.
c
c     *** Begin L0 loop ***
      do 60 i = 1, nplane
c
        iplane = i
cd    Added by S. Dun July 15, 2003,Feb, 03, 2004
        durre = 0.0
        wmlvar    = 0.0
        wmllen = 0.0
cd    End adding
cd      Added by S. Dun May 31,2004
        maxrun = 0.0
cd      End adding
c
c       CALL IDAT IF RAINFALL, SPRINKLE IRRIGATION OR SNOWMELT OCCURS
        if ((norain(iplane).eq.1).or.(irint(iplane).ge.1.0e-8).or.
     1     (wmelt(iplane).gt.0.0)) then
cd    Modified by S. Dun Feb. 03, 2004, For snow melt on effective OFEs 
cd          call idat(xmxint(iplane),wmelt(iplane),ibrkpt,rain(iplane))
c
        if (wmelt(iplane).gt.0.0) then
           wmlvar    = wmelt(iplane)*slplen(iplane)
           wmllen = slplen(iplane)
           if (iuprun(iplane).gt.0) then
             k = 0
          do while ((iuprun(iplane-k).gt.0).and.((iplane-k-1).gt.0))
            k = k + 1
            wmlvar    = wmlvar + wmelt(iplane-k)*slplen(iplane-k)    
            wmllen = wmllen + slplen(iplane-k)
          end do
           endif
          wmlvar    = wmlvar   /wmllen
        endif
c
cd    The initiation of wmlvar is moved in current loop to solve 
cd    this problem. S. Dun 2-28-04.    
c dcf   PROBLEM HERE - calling IDAT with wmelt(iplane)=0
c dcf   that causes a divide by zero there?  dcf 2-25-04
        call idat(wmlvar,ibrkpt,rain(iplane))
c
cd    End Modifying
        else
          p = 0.0
          do 20 it = 1, mxtime
            tr(it) = 0.0
            r(it) = 0.0
            rr(it) = 0.0
   20     continue
        end if
c
c       Note:  If it is desired not to calculate and subtract off
c       depression storage, the user should set AVEDEP to
c       zero in GRNA.
c
        dep(iplane) = 0.112 * rrc(iplane) + 3.1 * rrc(iplane) *
     1      rrc(iplane) - 1.20 * rrc(iplane) * avgslp(iplane)
c
        if (dep(iplane).lt.0.0) dep(iplane) = 0.0
c
        dpress(iplane) = dep(iplane)
c
c       Check for contours.  Jeff Stone's changes for contours.
c
c       Check to see if contouring in effect for current day of year
c       sjl, 12/22/98
c
        jumpfg = 0
        jmpfg2 = 0
        cntflg = 0
        if (contrs(nowcrp(iplane),iplane).ne.0)then
          cntflg = 1
          call conrun(nowcrp(iplane),dep(iplane),effdrr,
     1        runmax,pkrmax,pkefdn,wmelt(iplane),drlast)
          if (iplane.ne.nplane) iuprun(iplane+1) = 0
          jumpfg = 2
          jmpfg2 = 1
        end if
c
c       *** L00 IF ***
        if (jmpfg2.eq.0) then
c
          iepln = iplane
c
c         Summation for average infiltration parameters for the
c         equivalent plane
c
          if (iuprun(iplane).eq.0) then
            ibpln = iplane
            sumks = ks(iplane) * slplen(iplane)
            sumsm = sm(iplane) * slplen(iplane)
            sumdep = dep(iplane) * slplen(iplane)
            efflen(iplane) = slplen(iplane)
          else
            sumks = sumks + ks(iplane) * slplen(iplane)
            sumsm = sumsm + sm(iplane) * slplen(iplane)
            sumdep = sumdep + dep(iplane) * slplen(iplane)
            efflen(iplane) = efflen(iplane-1) + slplen(iplane)
          end if
c
          xnpln = iepln - ibpln + 1
          aveks(iplane) = sumks / efflen(iplane)
          avesm(iplane) = sumsm / efflen(iplane)
          aveksm = aveks(iplane) * avesm(iplane)
c
          avedep = sumdep / efflen(iplane)
c
c********************************************************************************
c         The following runon-runoff cases are treated:                         *
c                                                                               *
c         case 1 : q(iplane-1) =  0        re(iplane) = 0       q(iplane) = 0   *
c         case 2 : q(iplane-1) >= 0        re(iplane) > 0       q(iplane) > 0   *
c         case 3 : q(iplane-1) >  0        re(iplane) = 0       q(iplane) > 0   *
c         case 4 : q(iplane-1) >  0        re(iplane) = 0       q(iplane) = 0   *
c******************************************************************************
c
cd    Added by S. Dun, June 15, 2007
c     For checking out winter K
cd    write(61,1500) year,mon,day,iplane,Xmxint(iplane),aveks(iplane)
1500  format(1x, 4i6, 2E12.3)
cd    End added
c
          if (xmxint(iplane).gt.aveks(iplane)) then
            call grna(aveksm,avedep,nstemp,tf,re,effdrr,
     1          durre)
c
c           New code inserted by Jeff Stone, 2/98, to correct error
c           with partial equilibrium storm events.  dcf  3/2/98
c
            runtmp(iplane) = runoff(iplane)
c
c           End of new code inserted by Jeff Stone, 2/98, dcf 3/2/98
c
            if(runoff(iplane).le.0.0) durre = 0.0
c
            if (runoff(iplane).gt.0.0) then
c
c             Case Two - rainfall excess > zero
c
              drlast = durre
              apr = 0
              ns = nstemp
c
c             Get rainfall excess into HDRIVE format.
c
              do 30 ii = 1, ns - 1
                t(ii) = tf(ii)
                s(ii) = re(ii)
   30         continue
c
              s(ns) = 0.
              t(ns) = tf(ns)
c
              call frcfac(nowcrp(iplane))
c
              call rdat(nowcrp(iplane))
c
              alphay(iplane) = alpha(iplane)
              ealpha = alphay(iplane)
              norun(iplane) = 1
c
              if (iplane.ne.nplane) iuprun(iplane+1) = 1
c
              kplane = iplane
c
c             New code inserted by Jeff Stone, 2/98, to correct error
c             with partial equilibrium storm events.  dcf  3/2/98
c
c             Get equivalent depth discharge coefficient for peak
c             discharge computations for multiple OFE situations.
              if (xnpln.gt.1) call
     1          eplane(ibpln,iepln,slplen,alphay,m,ealpha)
c
c             reduce runoff volume due to recession infiltration
              call qinf(m,ealpha,efflen(kplane),aveks(kplane),drlast,
     1                  f(nstemp-1),runtmp(kplane))
c
c             End of new code inserted by Jeff Stone, 2/98, dcf 3/2/98
c
              if (iplane.eq.nplane) then
                jumpfg = 1
              else
                jumpfg = 2
              end if
            end if
          end if
c
c       *** L00 ENDIF ***
        end if
c
c       *** L1 IF ***
        if (jumpfg.eq.0) then
          if (iuprun(iplane).eq.0) then
c
c           Case One - rainfall excess = zero
c
            norun(iplane) = 0
            if (iplane.ne.nplane) iuprun(iplane+1) = 0
            runoff(iplane) = 0.0
c
c           New code inserted by Jeff Stone, 2/98, to correct error
c           with partial equilibrium storm events.  dcf  3/2/98
c
            runtmp(iplane) = 0.0
c
c           End of new code inserted by Jeff Stone, 2/98, dcf 3/2/98
c
            jumpfg = 2
          else
c
c           Case Three or Four
c
            call rochek(ks(iplane),sm(iplane),dep(iplane))
c
c           New code inserted by Jeff Stone, 2/98, to correct error
c           with partial equilibrium storm events.  dcf  3/2/98
c
            runtmp(iplane) = runoff(iplane)
c
c           End of new code inserted by Jeff Stone, 2/98, dcf 3/2/98
c
            if (runoff(iplane).gt.0.0) then
c
c             Case Three - rainfall excess = 0, runoff > 0
cd      Added by S. Dun, June 03, 2004
c       using last OFE's t,s and ns data
              ns = ns + 1
              drlast = t(ns)
              apr = 0
c
              do 34 ii = 1,ns
                s(ii) = s(ii) / runoff(iplane-1) *
     1             runoff(iplane)
34            continue
cd      End adding
c
              call frcfac(nowcrp(iplane))
c
              call rdat(nowcrp(iplane))
c
              alphay(iplane) = alpha(iplane)
              ealpha = alphay(iplane)
              apr = 1
              norun(iplane) = 1
c
              if (iplane.ne.nplane) iuprun(iplane+1) = 1
c
              kplane = iplane
c
              if (iplane.ne.nplane) jumpfg = 2
c
c
            else
c
c             Case Four - rainfall excess = 0, runoff = 0
c
              norun(iplane) = 1
c
              if (iplane.ne.nplane) iuprun(iplane+1) = 0
c
              runoff(iplane) = 0.0
c
c             New code inserted by Jeff Stone, 2/98, to correct error
c             with partial equilibrium storm events.  dcf  3/2/98
c
              runtmp(iplane) = 0.0
c
c             End of new code inserted by Jeff Stone, 2/98, dcf 3/2/98
c
              iepln = iepln - 1
              xnpln = iepln - ibpln + 1
              kplane = iplane - 1
c
            end if
c
c           RAINFALL EXCESS DURATION = 0 FOR CASE 3 & 4 - JJS JUN 93
c
            effdrr(iplane) = 0.0
          end if
c
c       *** L1 ENDIF ***
        end if
c
c
cd      Added by S. Dun, May 31, 2004
          maxrun = runoff(iplane)
          if (jmpfg2.eq.0) runoff(iplane) = runtmp(iplane)
cd      End adding
cd        Added by S. Dun Jun 06,2003
cd        if ((xmxint(iplane).gt.0.0).and.(imodel.eq.1)) then
          if (imodel.eq.1) then
c            
              call watbal(lunp,luns,lunw,nowcrp(iplane),elev)
              watblf(iplane) = 1
              runtmp(iplane) = runoff(iplane)
              if(cntflg.eq.1) then
                surpls = surdra(iplane)
              else
                surpls = surdra(iplane) * slplen(iplane)/efflen(iplane)
              endif
              if(surpls.gt.1.0E-6) then
                if(durre.gt.0.0) then
                do 35 ii = 1, nstemp - 1
                  if(s(ii).gt.1.e-10)then
                    if (ii.eq.1) then
                      s(ii) = s(ii) + surpls/durre
                    else
                      s(ii) = s(ii) + surpls/durre
                    endif
                  endif
   35           continue
                else
                    durre = tr(nf)-tr(1)
                    if (iuprun(iplane).ne.0)
     1                    durre = max(effdrr(iplane-1),durre)
cd    Added by S. Dun, July 06, 2004
cd    for runoff caused by uphill OFE subsurface flow
                    if (durre.eq.0.0) durre = 86400.0
cd    End adding.
c
                    s(1) = surpls/durre
                    s(2) = 0.0
                    nstemp = 2
                    remax(iplane) = s(1)                   
c
                    t(1) = 0.0
                    t(2) = durre                                    
                    effdrr(iplane) = durre
cd    Modified by S. Dun 08/28/2004
c     This modification is to make sure the peak runoff calculation 
c     would use subroutine appmth instead of hdrive 
c     when the surface flow event is caused by soil water surplus only.
                    tp(2) = 0.0 
cd    End modifying. 08/28/2004                         
                endif
c
c               Case Two - rainfall excess > zero
c
                drlast = durre
                apr = 0
                ns = nstemp
c
c               Get rainfall excess into HDRIVE format.
c
c
                call frcfac(nowcrp(iplane))
c
                call rdat(nowcrp(iplane))
c
                alphay(iplane) = alpha(iplane)
                ealpha = alphay(iplane)
                norun(iplane) = 1
c
                if (iplane.ne.nplane) iuprun(iplane+1) = 1
                kplane = iplane
c
                if ((jmpfg2.eq.0).and.(iplane.eq.nplane)) then
                  jumpfg = 1
                else
                  jumpfg = 2
                end if
c
              endif
c
        endif
        runtmp(iplane) = runoff(iplane)
        runoff(iplane) = Max(maxrun, runoff(iplane))
cd      End adding
c
c       Get runoff parameters for equivalent plane.
c
c       *** M1 IF ***
          if (jumpfg.ne.2.and.ivers.ne.3) then
          avere = 0.0
c
c         Summation of rainfall excess for approximate hydrograph method
c
          do 40 j = ibpln, iepln
            avere = avere + remax(j) * slplen(j) / efflen(kplane)
   40     continue
c
cc        Get equivalent depth discharge coefficient for peak discharge
cc        computations.
c
c         Lines commented out by Jeff Stone, 2/98, to correct error
c         with partial equilibrium storm events.  dcf  3/2/98
c
c         if (xnpln.gt.1.) call
c     1       eplane(ibpln,iepln,slplen,alphay,m,ealpha)
c
c         End of commented out code by Jeff Stone, 2/98, dcf 3/2/98
c
          a1 = m * ealpha
          a2 = m - 1.d0
c
          if (ibrkpt.eq.1) then
            if (apr.eq.1) then
c
c             approximate method is always used for case three
c             situations, in both continuous and single storm versions.
c
c             WHEN CALLING APPMTH AND TESTING IF TO USE APPMTH OR
c             HDRIVE, USE DDRLAST - JJS JUN 93
c
              call appmth(runoff(kplane),remax(kplane),
     1                    efflen(kplane),ealpha,m,drlast,peakro(kplane))
            else
              call hdrive(ealpha,m,efflen(kplane),runoff(kplane),
     1            peakro(kplane))
            end if
          else
            if (apr.eq.1) then
c
c             Approximate method is always used for case three
c             situations, in both continuous and single storm versions.
c
              call appmth(runoff(kplane),remax(kplane),
     1                    efflen(kplane),ealpha,m,drlast,peakro(kplane))
            else if (imodel.eq.2) then
              call hdrive(ealpha,m,efflen(kplane),runoff(kplane),
     1            peakro(kplane))
c
c           Test for using the approximate method
c
            else if (tp(2) .gt. 0.) then
              call hdrive(ealpha,m,efflen(kplane),runoff(kplane),
     1                    peakro(kplane))
            else
              call appmth(runoff(kplane),remax(kplane),
     1                    efflen(kplane),ealpha,m,drlast,peakro(kplane))
            end if
          end if
c
          if (peakro(kplane).lt.3.6e-8) peakro(kplane) = 3.63e-8
c

c         Lines commented out by Jeff Stone, 2/98, to correct error
c         with partial equilibrium storm events.  dcf  3/2/98
c         Call to subroutine QINF has been moved earlier in this
c         subroutine.
c
cc        REDUCE RUNOFF VOLUME DUE TO RECESSION INFILTRATION - JJS 9-94
c
c         call qinf(m,ealpha,efflen(kplane),aveks(kplane),drlast,
c    1              f(nstemp-1),runoff(kplane))
c
c         End of commented out code by Jeff Stone, 2/98, dcf 3/2/98

c
c         get effective runoff duration = qvol/qpeak
c

c         Code change by Jeff Stone, 2/98, to correct error
c         with partial equilibrium storm events.  dcf  3/2/98
c
c         effdrn(kplane) = runoff(kplane) / peakro(kplane)
          effdrn(kplane) = runtmp(kplane) / peakro(kplane)
c
c         End of code change by Jeff Stone, 2/98, dcf 3/2/98

c
c         Limit EFFDRN less than or equal to one day (86400 seconds)
c
          if (effdrn(kplane).gt.86400.) effdrn(kplane) = 86400.
c
          do 50 l = ibpln, iplane
c
c           New code inserted by Jeff Stone, 2/98, to correct error
c           with partial equilibrium storm events.  dcf  3/2/98
c
            runoff(l) = runtmp(l)
c
c           End of new code inserted by Jeff Stone, 2/98, dcf 3/2/98
c
            effdrn(l) = effdrn(kplane)
            peakro(l) = runoff(l) / effdrn(l)
            if (runoff(l).gt.runmax) runmax = runoff(l)
            if (peakro(l).gt.pkrmax) pkrmax = peakro(l)
            if (effdrn(l).gt.pkefdn) pkefdn = effdrn(l)
   50     continue
c
c        else if(ivers.eq.3.and.runoff(iplane).gt.0.0) then
c
c          if (xnpln.gt.1.) call
c     1        eplane(ibpln,iepln,slplen,alphay,m,ealpha)
c          a1 = m * ealpha
c          a2 = m - 1.d0
c          call qinf(m,ealpha,efflen(iplane),aveks(iplane),drlast,
c     1              f(nstemp-1),runoff(iplane))
c
c       *** M1 ENDIF ***
        end if
c
c     *** End L0 loop ***
   60 continue
c
c     Estimate runoff due to irrigation
c
      if (noirr.ne.0) then
        do 70 ipl = 1, nplane
          if (ipl.lt.irofe) then
            irfrac = 0.
          else if (ipl.eq.irofe) then
c
c XXX NOTE - the setting of rainfall to zero in subroutine CONTIN
c            will likely cause errors in calculation of "irfrac",
c            "irrund", "irrunt", "irruny" and "irrunm" for multiple
c            OFE hillslopes.   dcf  5/26/94
c XXX Altered code to fix anticipated problem.  dcf  5/26/94
c
            if (ipl.eq.1) then
              if(wmelt(ipl).gt.0.0)then
                irfrac = irdept(ipl) / (wmelt(ipl)+irdept(ipl))
              else
                irfrac = irdept(ipl) / (rain(ipl) + irdept(ipl))
              endif
            else
              if(wmelt(ipl).gt.0.0)then
                irfrac = irdept(ipl) / (wmelt(ipl)+irdept(ipl)+
     1            runoff(ipl-1))
              else
                irfrac = irdept(ipl) / (rain(ipl)+irdept(ipl)+
     1            runoff(ipl-1))
              endif
            end if
          else
            if (runoff(ipl-1).gt.0.00001) then
              irfrac = irfrac * runoff(ipl-1) / (runoff(ipl-1)
     1            + rain(ipl) + wmelt(ipl))
            else
              irfrac = 0.
            end if
          end if
cd    Modified by S. Dun 03/15/2004
cd    Now the total irrigation attributed runoff depth is for 
cd    the total length of the OFE which menns counting form 
cd    top of the hillslope
          if(cntflg.eq.1) then
            irrund(ipl) = runoff(ipl) * irfrac
          else
            irrund(ipl) = runoff(ipl) * irfrac
     1                  * efflen(iplane) / totlen(iplane)
          endif
          irrunt(ipl) = irrunt(ipl) + irrund(ipl)
c
          irruny(ipl) = irruny(ipl) + irrund(ipl)
c
          irrunm(ipl) = irrunm(ipl) + irrund(ipl)
c
cd    End modifying
   70   continue
      else
        irfrac = 0.
      end if
c
      return
      end
