      subroutine xinflo(nowcrp)
c**********************************************************************
c                                                                     *
c   This subroutine controls the variables affected by the            *
c   runoff both leaving and entering the overland flow element        *
c                                                                     *
c**********************************************************************
c
      integer nowcrp
c**********************************************************************
c                                                                     *
c    Arguments                                                        *
c      nowcrp - flag indicating current crop index number             *
c                                                                     *
c**********************************************************************
c
      include 'pmxelm.inc'
      include 'pmxcrp.inc'
      include 'pmxgrz.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxsrg.inc'
      include 'pmxslp.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'pxstep.inc'
c
c**********************************************************************
c                                                                     *
c     Common Blocks                                                   *
c                                                                     *
c**********************************************************************
c
      include 'ccover.inc'
c
      include 'ccrpprm.inc'
c
      include 'ccrpvr3.inc'
c
      include 'ccrpvr5.inc'
c
c
c
      include 'ccntour.inc'
c
c*********************************************************************
c                                                                    *
c cntour variable updated                                            *
c    cntlen(mxplan), cnslp(mxplan)                                   *
c                                                                    *
c*********************************************************************
c
      include 'cdist.inc'
c
c**********************************************************************
c                                                                     *
c  dist variables updated                                             *
c     xinput(101,mxplan)                                              *
c                                                                     *
c**********************************************************************
c
      include 'cefflen.inc'
c      read: efflen(mxplan)
      include 'cends.inc'
c
c**********************************************************************
c                                                                     *
c   ends variables updated                                            *
c     rspace(mxplan),qshear,qout,qin,strldn                           *
c                                                                     *
c**********************************************************************
c
      include 'cffact.inc'
c
      include 'chydrol.inc'
c
      include 'cinfcof.inc'
c
c**********************************************************************
c                                                                     *
c   infcof variables updated                                          *
c     ainf(mxslp),binf(mxslp),cinf(mxslp),qostar                      *
c                                                                     *
c**********************************************************************
c
      include 'cirfurr.inc'
      include 'cirriga.inc'
      include 'crinpt1.inc'
c
      include 'crinpt3.inc'
c
      include 'crinpt5.inc'
c
      include 'cslope.inc'
c
      include 'cstruc.inc'
      include 'cupdate.inc'
c
c
c**********************************************************************
c                                                                     *
c Local Variables                                                     *
c  cnslp : contour slope if contours hold - or average hill slope if  *
c          contours fail or no contours used                          *
c  del   : difference between discharge out and into an OFE           *
c                                                                     *
c**********************************************************************
c
      save
      integer plant, i
      real del
c
      plant = itype(nowcrp,iplane)
c
      do 10 i = 2, 101
        xinput(i,iplane) = float(i-1) * .01
   10 continue
c
c
c     the contours failed or no contours
c
c     4/4/91  JEF
c     if(cnfail(iplane).eq.1) then
c
      if (conseq(nowcrp,iplane).eq.0) then
c       if(cnfail(iplane).ne.0.or.conseq(nowcrp,iplane).eq.0)then
c
        qin = qout
        if (irsyst.eq.2.and.iplane.eq.irofe) qin = irqin
c       rspace(iplane)=1.0
c
c       calculate rill spacing for rangeland
c
        if (lanuse(iplane).eq.2) then
          if (spop(plant).gt.0.0.or.hmax(plant).gt.0.30) then
            rspace(iplane) = 100.0 / (spop(plant)+gpop(plant)+
     1          tpop(plant)+1)
          else
            rspace(iplane) = 1.0
          end if
          if (rspace(iplane).gt.5.0) rspace(iplane) = 5.0
          if (rspace(iplane).lt.0.5) rspace(iplane) = 0.5
        end if
c
        cntlen(iplane) = slplen(iplane)
        cnslp(iplane) = avgslp(iplane)
c
        qout = peakro(iplane) * efflen(iplane)
        del = qout - qin
        if (qout.le.0.0) then
          qostar = -efflen(iplane) / slplen(iplane)
        else if (abs(del).gt.1.0e-10) then
          if (qin.le.0.0) then
            qostar = 0.0
          else
            qostar = qin / del
          end if
        else
c
c
c         corrected by dcf - 4/12/90
c
c         qostar=qin/1.0e-6
c
c         ELSE - the value of del is smaller than 1e-10 m**2/s
c
          if (del.ge.0.0) qostar = qin / 1.0e-10
          if (del.lt.0.0) qostar = -qin / 1.0e-10
        end if
c
        if (qout.gt.0.0) then
c
          if (qostar.eq.-1.0) qostar = -1.001
c
          do 20 i = 2, nslpts(iplane)
            ainf(i) = a(i,iplane) / (qostar+1.0)
            binf(i) = (a(i,iplane)*qostar+b(i,iplane)) / (qostar+1.0)
            cinf(i) = (b(i,iplane)*qostar/(qostar+1.0))
            ainftc(i) = ainf(i)
            binftc(i) = binf(i)
            cinftc(i) = cinf(i)
   20     continue
c
          qshear = qout * rspace(iplane)
c
        else
c
c         Change 4/6/90 by dcf
c
c         if(qout.le.0.0)qout=1.e-10
c         if(qostar.eq.0.0)qostar=0.001
c
          if (abs(qostar).lt.0.00001) qostar = -0.00001
c
          do 30 i = 2, nslpts(iplane)
            ainf(i) = a(i,iplane) / (qostar)
            binf(i) = (a(i,iplane)*qostar+b(i,iplane)) / (qostar)
            cinf(i) = b(i,iplane)
            ainftc(i) = ainf(i)
            binftc(i) = binf(i)
            cinftc(i) = cinf(i)
   30     continue
c
          qshear = qin * rspace(iplane)
c
c       Change made by dcf - 8/10/90
c
c       xfend=-qostar
c       do 40 i=2,101
c       if(xinput(i,iplane).gt.xfend)then
c       jend=int(xfend*100.)
c       xinput(i,iplane)=real(jend)/100.
c       endif
c       40     continue
c
c
        end if
c
      else
c
c       contours held
c
        rspace(iplane) = rowspc(conseq(nowcrp,iplane))
        cntlen(iplane) = rowlen(conseq(nowcrp,iplane))
        cnslp(iplane) = cntslp(conseq(nowcrp,iplane))
c
        do 40 i = 1, mxslp
          ainf(i) = 0.0
          binf(i) = 1.0
          cinf(i) = 0.0
          ainftc(i) = ainf(i)
          binftc(i) = binf(i)
          cinftc(i) = cinf(i)
   40   continue
c
        qshear = cntlen(iplane) * peakro(iplane) * rspace(iplane)
        qout = qshear
        qin = 0.0
        qostar = 0.0
        strldn = 0.0
c
      end if
c
      return
      end
