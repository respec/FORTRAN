      subroutine conrun(nowcrp,dep,effdrr,runmax,pkrmax,
     1    pkefdn,wmelt,drlast)
c
c
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxtim.inc'
c
      include 'cavepar.inc'
c
      include 'cconsts.inc'
c
c******************************************************************
c                                                                 *
c consts variables updated                                        *
c   a1, a2.                                                       *
c                                                                 *
c******************************************************************
      include 'ccntour.inc'
      include 'cdata2.inc'
      include 'cefflen.inc'
      include 'chydrol.inc'
      include 'cparame.inc'
      include 'cpass.inc'
      include 'cstruc.inc'
      include 'cprams.inc'
      include 'cxmxint.inc'
      include 'cupsfl.inc'
      

      real tf(mxtime), re(mxtime), durre, dep, effdrr(mxplan), aveksm,
     1    ealpha, pkefdn, pkrmax, runmax, wmelt,drlast
      integer apr, nowcrp, ii, nstemp
c
c
c
c
      runmax = 0.0
      pkrmax = 0.0
      pkefdn = 0.0
      aveksm = ks(iplane) * sm(iplane)
      aveks(iplane) = ks(iplane)
      avesm(iplane) = sm(iplane)
      efflen(iplane) = rowlen(conseq(nowcrp,iplane))
      if (xmxint(iplane).gt.aveks(iplane)) then
c
c       PASS BACK FROM GRNA DURRE - JJS MAY 93
        call grna(aveksm,dep,nstemp,tf,re,effdrr,durre)
c
        if (runoff(iplane).gt.0.0) then
c
c         case two - rainfall excess > zero
c
          apr = 0
          ns = nstemp
          drlast = durre
c
c         get rainfall excess into hdrive format
c
          do 10 ii = 1, ns - 1
            t(ii) = tf(ii)
            s(ii) = re(ii)
   10     continue
c
          s(ns) = 0.
          t(ns) = tf(ns)
c
c         Following statement commented out since DUREXR not used
c         anywhere.  (follows Stone changes in IRS)    dcf  5/25/93
c         durexr = tf(nt + 1)
c
          call frcfac(nowcrp)
          call rdat(nowcrp)
c
          ealpha = alpha(iplane)
          norun(iplane) = 1
c
c         avere = remax(iplane)
c
          a1 = m * ealpha
          a2 = m - 1.d0
c
c         PEAK DISCHARGE COMPUTATIONS
c
          if (apr.eq.1) then
c
c           approximate method is always used for case three
c           situations in both continuous and single event versions
c
c
c           PASS DURRE TO APPMTH - JJS MAY 93
c
c
            call appmth(runoff(iplane),remax(iplane),
     1                  efflen(iplane),ealpha,m,drlast,peakro(iplane))
c

          else if (imodel.eq.2) then
            call hdrive(ealpha,m,efflen(iplane),runoff(iplane),
     1          peakro(iplane))
c
c         test for using the approximate method
c
c
c         PASS DURRE TO APPMTH - JJS MAY 93
c
          else if (wmelt .gt. 0. .or. tp(2) .gt. 0.) then
            call hdrive(ealpha,m,efflen(iplane),runoff(iplane),
     1          peakro(iplane))
          else
            call appmth(runoff(iplane),remax(iplane),
     1                  efflen(iplane),ealpha,m,drlast,peakro(iplane))
          end if
c
          if (peakro(iplane).lt.3.6e-8) peakro(iplane) = 3.63e-8
c
c         reduce runoff volume due to recession infiltration
c
          call qinf(m,ealpha,efflen(iplane),aveks(iplane),drlast,
     1              f(nstemp-1),runoff(iplane))
c
c
c         get effective runoff duration = qvol/qpeak
c
          effdrn(iplane) = runoff(iplane) / peakro(iplane)
c
c         limit effdrn less than or equal to one day (86400 seconds)
c
          if (effdrn(iplane).gt.86400.) effdrn(iplane) = 86400.
c
          if (runoff(iplane).gt.runmax) runmax = runoff(iplane)
          if (peakro(iplane).gt.pkrmax) pkrmax = peakro(iplane)
          if (effdrn(iplane).gt.pkefdn) pkefdn = effdrn(iplane)
          call tfail(efflen(iplane),nowcrp)
        else
c
c         no runoff
c
          runoff(iplane) = 0.
c
        end if
      end if
c
      return
      end
