      subroutine wshirs(nowcrp,wmelt,ibrkpt,effdrr)
c
c     + + + PURPOSE + + +
c
c     Calculates the runoff volume on a channel element.
c
c     Called from: SR WSHDRV
c     Author(s): C. Baffaut
c     Reference in User Guide:
c
c     Version:
c     Date coded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxprt.inc'
      include 'pmxslp.inc'
      include 'pmxtim.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real wmelt(mxplan), effdrr(mxplan), drlast, durre
      integer nowcrp(mxplan), ibrkpt, ii, ipl, it
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     xmxint - maximum rainfall intensity
c     wmelt  - amount of snow melt
c     effdrr - duration of rainfall excess
c     nowcrp - number of current crop
c     iuprun - flag to indicate flow onto an OFE from above
c              ( 1 - yes ; 0  - no )
c
c     + + + COMMON BLOCKS + + +
c
      include 'cavepar.inc'
c     modify: aveks(mxplan), avesm(mxplan)
c
      include 'cdata1.inc'
c     modify: tr(mxtime), r(mxtime), rr(mxtime)
c
      include 'cdata2.inc'
c
      include 'cefflen.inc'
c
      include 'cflags.inc'
c     modify: idflag
c
      include 'chydrol.inc'
c     modify: runoff(mxplan), remax, durexr, peakro(mxplan), effdrn
c     write: effint(mxplan)
c
      include 'cirriga.inc'
c     modify: irfrac
c     read: irdept(mxplan)
c
      include 'cirspri.inc'
c     read: irnit(mxplan)
c
      include 'cpass1.inc'
c     write: s(mxtime)
c
      include 'cpass2.inc'
c     write: t(mxtime)
c
      include 'cpass3.inc'
c     modify: ns
c
      include 'cprams.inc'
c     read: m
c     modify: alpha(mxplan)
c     write: norun(mxplan)
c
      include 'cslpopt.inc'
c     read: totlen
c
      include 'cstore.inc'
c     modify: roffon, rvolon
c
      include 'cstruc.inc'
c     modify: iplane
c
      include 'csumirr.inc'
c     modify: irrund(mxplan),irrunm(13,mxplan),irrunt(mxplan),
c             irruny(mxplan)
c
      include 'ccntour.inc'
c     read: conseq(mxcrop,mxplan)
c
      include 'cconsts.inc'
c     write: a1,a2
c
      include 'ccrpout.inc'
c     read: rrc(mxplan)
c
      include 'cdata3.inc'
c     read: nf
c
      include 'cdiss3.inc'
c     read: p
c
      include 'cdist2.inc'
c     read: slplen(mxplan)
c
      include 'cparame.inc'
c     read: ks,km
c
      include 'cslope2.inc'
c     read: avgslp
c
      include 'cstmflg.inc'
c     read: nmon
c
      include 'cstruct.inc'
c     read: ielmt
c
cd    Added by S. Dun Feb. 04, 2004
      include 'cxmxint.inc'
      include 'cupsfl.inc'
cd    End adding
c
c     + + + LOCAL VARIABLES + + +
c
      real aveksm, ealpha, tf(mxtime), dep(mxplan), avedep, re(mxtime)
      integer nstemp, jumpfg
c
c     + + + LOCAL DEFINITIONS + + +
c
c     aveksm - ks*sm
c     ealpha - alpha for the equivalent plane
c     tf     - time array for rainfall excess
c     dep    - potential depression storage depth
c     avedep - average depressional storage for equivalent plane
c     re     - rainfall excess rate (m/s)
c     nstemp - index for the last time of cumulative rainfall excess
c     jumpfg - 0 = no excess rain produced ; 1 = excess rain and call qinf
c     drlast - variable to hold last value of durre for multiple ofe
c              hillslopes - this is to prevent divide by zero values
c              for multiple ofe hillslopes and case 3 hydrologic planes
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     frcfac
c     grna
c     idat
c     qinf
c     rdat
c     trnlos
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      idflag = 0
      drlast = 0.0
c
      alpha(iplane) = 0.0
c
      peakro(iplane) = 0.0
      runoff(iplane) = 0.0
c
      effdrn(iplane) = 0.0
      effdrr(iplane) = 0.0
c
      iuprun(iplane) = 0
c
      if (rvolon(ielmt).gt.0.001) iuprun(iplane) = 1
c
c     compute the infiltration parameters for the plane
c
c     compute infiltration and rainfall excess
c
c     call idat if rainfall, sprinkler irrigation, or snowmelt
c     occurs, or if there is runoff from upstream elements
c
cd    Modified by S.Dun, April 14, 2004
      if ((norain(iplane).eq.1).or.(irint(iplane).ge.1.0e-8).or.(
     1    wmelt(iplane).gt.0.0)) then
cd     1    wmelt(iplane).gt.0.0).or.(iuprun(iplane).eq.1)) then
cd    End modifying
c
cd        call idat(xmxint(iplane),wmelt(iplane),ibrkpt,rain(iplane))
        call idat(wmelt(iplane),ibrkpt,rain(iplane))
c
      else
c
        p = 0.0
c
        do 10 it = 1, mxtime
          tr(it) = 0.0
          r(it) = 0.0
          rr(it) = 0.0
   10   continue
c
      end if
c
c     if depression storage is not to be calculated and subtracted
c     off then the user should set avedep to zero in SR GRNA
c
      dep(iplane) = 0.112 * rrc(iplane) + 3.1 * rrc(iplane) *
     1    rrc(iplane) - 1.20 * rrc(iplane) * avgslp(iplane)
c
      if (dep(iplane).lt.0.0) dep(iplane) = 0.0
      dpress(iplane) = dep(iplane)
c
      jumpfg = 0
c
c     compute average infiltration parameters for the plane
c
      avedep = dep(iplane)
c      efflen(iplane) = slplen(iplane)
      aveksm = ks(iplane) * sm(iplane)
      aveks(iplane) = ks(iplane)
      avesm(iplane) = sm(iplane)
c
c     *********************************************************************
c     * The following runon-runoff cases are treated:                     *
c     *                                                                   *
c     * case 1 : q(upstream) =  0.0   re(iplane) = 0.0   q(iplane) = 0.0  *
c     * case 2 : q(upstream) >= 0.0   re(iplane) > 0.0   q(iplane) > 0.0  *
c     * case 3 : q(upstream) >  0.0   re(iplane) = 0.0   q(iplane) > 0.0  *
c     * case 4 : q(upstream) >  0.0   re(iplane) = 0.0   q(iplane) = 0.0  *
c     *********************************************************************
c
      if (xmxint(iplane).gt.ks(iplane)) then
c
        call grna(aveksm,avedep,nstemp,tf,re,effdrr,
     1            durre)
c
        if (runoff(iplane).gt.0.0) then
c
c         case 2 - rainfall excess > zero
c
          drlast = durre
          ns = nstemp
c
c         get rainfall excess into SR HDRIVE format
c
          do 20 ii = 1, ns - 1
            t(ii) = tf(ii)
            s(ii) = re(ii)
   20     continue
c
          s(ns) = 0.0
          t(ns) = tf(ns)
c
          call frcfac(nowcrp(iplane))
          call rdat(nowcrp(iplane))
c
          alphay(iplane) = alpha(iplane)
          ealpha = alphay(iplane)
c
          norun(iplane) = 1
          chnrun(iplane) = runoff(iplane)
c
          jumpfg = 1
c
          if (iuprun(iplane).eq.1) then
            idflag = 4
            runoff(iplane) = roffon(ielmt) + runoff(iplane)
          else
            idflag = 2
          end if
c
c         reduce runoff volume due to recession infiltration
c
          call qinf(m,ealpha,efflen(iplane),ks(iplane),drlast,f(ns-1),
     1        runoff(iplane))
c
        end if
      end if
c
      if (jumpfg.eq.0) then
c
        if (iuprun(iplane).eq.0) then
c
c         case 1 - rainfall excess = zero and no runoff from above
c
          norun(iplane) = 0
          runoff(iplane) = 0.0
          idflag = 0
c
        else
c
c         case 3 or 4 - rainfall excess = 0 but runoff from above
c
          call trnlos(ks(iplane),sm(iplane),dep(iplane),rvolon(ielmt))
c
          idflag = 3
c
          if (runoff(iplane).gt.0.0) then
c
c           case 3 - rainfall excess = 0, runoff > 0
c
            call frcfac(nowcrp(iplane))
            norun(iplane) = 1
c
          else
c
c           case 4 - rainfall excess = 0, runoff from above infiltrates
c
            norun(iplane) = 0
            runoff(iplane) = 0.0
c
          end if
c
c         rainfall excess duration = 0 for case 3 & 4
c
          effdrr(iplane) = 0.0
c
        end if
      end if
c
c     estimate fraction of runoff due to irrigation
c
      if (noirr.ne.0) then
c
        do 30 ipl = 1, nplane
c
          if (ipl.lt.irofe) then
            irfrac = 0.0
c
          else if (ipl.eq.irofe) then
c
            if (ipl.eq.1) then
c
              if (wmelt(ipl).gt.0.0) then
                irfrac = irdept(ipl) / (wmelt(ipl)+irdept(ipl))
              else
                irfrac = irdept(ipl) / (rain(ipl)+irdept(ipl))
              end if
c
            else
c
              if (wmelt(ipl).gt.0.0) then
                irfrac = irdept(ipl) / (wmelt(ipl)+irdept(ipl)+
     1              runoff(ipl-1))
              else
                irfrac = irdept(ipl) / (rain(ipl)+irdept(ipl)+
     1              runoff(ipl-1))
              end if
c
            end if
c
          else
c
            if (runoff(ipl-1).gt.0.00001) then
              irfrac = irfrac * runoff(ipl-1) / (runoff(ipl-1)+
     1            rain(ipl)+wmelt(ipl))
            else
              irfrac = 0.0
            end if
c
          end if
c
cd    Modified by S. Dun 03/15/2004
cd    Now the total irrigation attributed runoff depth is for 
cd    the total length of the OFE which menns conting form 
cd    top of the hillslope
          irrund(ipl) = runoff(ipl) * irfrac
          irrunt(ipl) = irrunt(ipl) + irrund(ipl)
     1    * efflen(iplane) / totlen(iplane)
          irruny(ipl) = irruny(ipl) + irrund(ipl)
     1    * efflen(iplane) / totlen(iplane)
          irrunm(ipl) = irrunm(ipl) + irrund(ipl)
     1    * efflen(iplane) / totlen(iplane)
cd   End modifying
   30   continue
c
      else
        irfrac = 0.0
      end if
c
      return
      end
