      subroutine sloss(idout,dslost,wmelt,nowcrp)
c*******************************************************************
c                                                                  *
c   This subroutine called from SR CONTIN calculates sediment      *
c   concentration and sediment yield on a storm-by-storm basis,    *
c   and prints out soil loss and sediment load information by      *
c   input segment and by erosion /deposition section. It also      *
c   prints out an abbreviated hydrology output for the event       *
c   under consideration, by calling SR HYDOUT.                     *
c                                                                  *
c*******************************************************************
c*******************************************************************
c                                                                  *
c   Arguments                                                      *
c     idout - flag for hydrology output                            *
c     dslost - net soil loss/gain at each point on hillslope for   *
c              a storm event (time*dG/dx) - (kg/m**2)              *
c                                                                  *
c*******************************************************************
c                                                                  *
      include 'pmxcrp.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
      include 'pmxprt.inc'
      include 'pmxelm.inc'
      include 'pmxpts.inc'
      include 'pmxseg.inc'
      include 'pmxpnd.inc'
c
c*******************************************************************
c                                                                  *
c    Common Blocks                                                 *
c                                                                  *
c*******************************************************************
c
      include 'cavloss.inc'
c
c*******************************************************************
c                                                                  *
c    avloss variables updated                                      *
c       dsmon(mxplan,100),dsyear(mxplan,100),dsavg(mxplan,100)     *
c       avsols,avsole,avsolm,avsoly,avsolf                         *
c                                                                  *
c*******************************************************************
c
      include 'ccntour.inc'
c
      include 'cconsta.inc'
c
      include 'ccover.inc'
c
      include 'ccrpout.inc'
c
      include 'cdist.inc'
c
      include 'cefflen.inc'
c     read: efflen(pxplan)
      include 'cends.inc'
c
c*******************************************************************
c                                                                  *
c    ends variables updated                                        *
c       qsout                                                      *
c                                                                  *
c*******************************************************************
c
      include 'cenrpas.inc'
c
c*******************************************************************
c                                                                  *
c    enrpas variables updated                                      *
c       enrmm1,enrmm2,enryy1,enryy2                                *
c       enrff1,enrff2                                              *
c       frcmm1(10),frcmm2(10)                                      *
c       frcyy1(10),frcyy2(10),frcff1(10),frcff2(10)                *
c                                                                  *
c*******************************************************************
c
      include 'cerdval.inc'
c
c*******************************************************************
c                                                                  *
c  erdval variables updated                                        *
c      load(101)                                                   *
c                                                                  *
c*******************************************************************
c
      include 'chydrol.inc'
c
      include 'ciravlo.inc'
c
c**********************************************************************
c                                                                     *
c  iravlo variables updated                                           *
c     irsold,irsolm, irsoly, irsolt                               *
c                                                                     *
c**********************************************************************
c
      include 'cirriga.inc'
c
      include 'cparame.inc'
c
      include 'cpart.inc'
c
      include 'cparval.inc'
c
      include 'cseddet.inc'
      include 'cslpopt.inc'
      include 'cstore.inc'
      include 'cstruc.inc'
      include 'cupdate.inc'
c
      include 'cwater.inc'
c
      integer idout, nowcrp
      real wmelt
c
c*******************************************************************
c                                                                  *
c    local variables                                               *
c     conc   - sediment concentration at OFE end - (kg/kg)         *
c     dslend - dimensional sediment load at OFE end - (kg/m)       *
c     dslod1 - dimensional sediment load at point - (kg/m)         *
c     dslod2 - dimensional sediment load at point - (kg/m)         *
c     dslost - net soil loss/gain at each point on hillslope for   *
c              a storm event (time*dG/dx) - (kg/m**2)              *
c     csedls - sediment loss from contoured OFEs in (kg)           *
c                                                                  *
c*******************************************************************
c
      save
c     real conc(mxplan)
      integer i, ipart, j, jun
      real dslend, dslod1, dslod2, sumslc
      real dslost(mxplan,100),csedls(mxplan)
      data csedls /mxplan * 0.0/
c
c*******************************************************************
c
      jun = 31
      if (imodel.eq.2) jun = 32
c
c     initialize load variables
c
      dslend = 0.0
      dslod1 = 0.0
      dslod2 = 0.0
c     conc(iplane)=0.0
      do 10 j = 2, 101
        dslost(iplane,j-1) = 0.0
   10 continue
c
      if (ioutpt.eq.1.and.imodel.eq.1) call hydout(jun,idout,wmelt)
c
c     Calculate dimensional sediment load at beginning of OFE (x=0)
c
      dslod1 = load(1) * effdrn(iplane) * tcend * width(iplane) /
     1    rspace(iplane)
c
c     If contours present, then compute an average detachment rate on
c     the OFE using the sediment discharge at the end of the contours.
c     contours are assumed not to fail if present.
c
c     START OF CONTOUR ELSE-IF
c
      ofelod(iplane)=0.0
      if (conseq(nowcrp,iplane).ne.0) then
        dslend = load(101) * effdrn(iplane) * tcend * width(iplane) /
     1      rspace(iplane)
        avsols = dslend / cntlen(iplane)
c
        do 20 j = 2, 101
          dslost(iplane,j-1) = avsols
   20   continue

c
c       following line added 2/4/91 to calculate interrill contribution
c       for an OFE on which contours have held
c
        irdgdx(iplane) = (theta*tcend*effdrn(iplane)*width(iplane)) / (
     1      rspace(iplane)*cntlen(iplane))
c
c       6-19-2002 Need to report the sediment leaving the sides
c       of each contoured OFE in the output file, as well as the
c       accumulated sediment leaving the sides of the OFEs. Add
c       new variable "avsolc(iplane)" to track the storm sediment
c       loss from the contoured OFE sides.   dcf

        avsolc(iplane) = dslend

c       Compute the total sediment leaving the contoured OFE to the
c       side, by finding an equivalent width (OFE area/contour lgth)
        csedls(iplane) = avsolc(iplane)*
     1                   (slplen(iplane)*fwidth(1)/cntlen(iplane))

c       Set the average sediment discharge off the plane equal to zero
c       since if contours hold no sediment reaches the next OFE from
c       above.
c
        avsols = 0.0
c
c     ELSE if no contours are present on the OFE
c     calculate the net sediment loss or gain at a point by taking
c     the difference between the sediment load at the points divided
c     by the distance between the points.
c
      else
c
        do 30 j = 2, 101
          dslod2 = load(j) * effdrn(iplane) * tcend * width(iplane) /
     1        rspace(iplane)
          dslost(iplane,j-1) = (dslod2-dslod1) / (slplen(iplane)*0.01)
          dslod1 = dslod2
   30   continue
        ofelod(iplane)=dslod1
c
c       ADDED 11/8/90 by dcf - to allow estimation of interrill
c       contribution to sediment loss.
c
        irdgdx(iplane) = (theta*tcend*effdrn(iplane)*width(iplane)) / (
     1      rspace(iplane)*slplen(iplane))
c
c
        if (cntlen(iplane).lt.0.01) cntlen(iplane) = slplen(iplane)
        avsols = dslod2 / cntlen(iplane)
c
c     END OF CONTOUR ELSE-IF
c
      end if
c
c     summing for monthly and annual dslost
c
      do 40 j = 2, 101
        dsmon(iplane,j-1) = dsmon(iplane,j-1) + dslost(iplane,j-1)
        dsyear(iplane,j-1) = dsyear(iplane,j-1) + dslost(iplane,j-1)
        dsavg(iplane,j-1) = dsavg(iplane,j-1) + dslost(iplane,j-1)
   40 continue
c
c     START OF IPLANE=NPLANE ELSE-IF  (this the last
c     OFE on the current hillslope)
c
      if (iplane.eq.nplane) then
c
c       If contours are not present the sediment reaching the end of
c       the hillslope is equal to the sediment load at the last point
c       of the last OFE on the current hillslope.
c       else - if contours are present then they are assumed to not
c       fail and all sediment is assumed routed to the side of the
c       hillslope and none reaches the end of the OFE.  
c       6/26/2002 - a new calculation for "avsole" is made for the
c       case of a contoured hillslope - that includes all sediment
c       leaving the profile in any direction.
c
        sumslc = 0.0
        do 45 j = 1,nplane
          sumslc = sumslc + csedls(j)
  45    continue
        if (qout.gt.0.0 .and. conseq(nowcrp,nplane).eq.0)then
          avsole = (sumslc + dslod2*fwidth(1))/fwidth(1)
        else
          avsole = sumslc/fwidth(1)
        endif
c
        if (avsole.lt.0.0) avsole = 0.0
c
        avsolm = avsolm + avsole
        avsoly = avsoly + avsole
        avsolf = avsolf + avsole
c
        irsold = avsole * irfrac
        irsolm = irsolm + irsold
        irsoly = irsoly + irsold
        irsolt = irsolt + irsold
c
c       summing for weighted enrichment ratio
c       sets single storm and sums for monthly and annual
c
        enrmm1 = enrmm1 + (enrato(nplane)*avsole)
        enrmm2 = enrmm2 + avsole
        enryy1 = enryy1 + (enrato(nplane)*avsole)
        enryy2 = enryy2 + avsole
        enrff1 = enrff1 + (enrato(nplane)*avsole)
        enrff2 = enrff2 + avsole
c
        do 50 i = 1, npart
c
          frcmm1(i) = frcmm1(i) + (frcflw(i,nplane)*avsole)
          frcmm2(i) = frcmm2(i) + avsole
          frcyy1(i) = frcyy1(i) + (frcflw(i,nplane)*avsole)
          frcyy2(i) = frcyy2(i) + avsole
          frcff1(i) = frcff1(i) + (frcflw(i,nplane)*avsole)
          frcff2(i) = frcff2(i) + avsole
c
   50   continue
c
c       compute sediment concentration (kg/m**3),
c       for each particle size leaving the hillslope,
c       avsole is in (kg/m) and runvol is in (m**3)
c
        if (iplane.eq.nplane) then
          do 60 ipart = 1, npart
            if (peakro(nplane).le.0.0) then
              sedcon(ipart,nplane) = 0.0
            else
              sedcon(ipart,nplane) = avsole / (runoff(nplane)*
     1            efflen(nplane)) * frcflw(ipart,nplane)
            end if
   60     continue
        end if
c
c
c     END OF IPLANE=NPLANE IF-ELSE
c
      end if
c
c
c     If contours hold, qsout equals zero since no sediment reaches
c     the next OFE.  If contours fail, qsout equals the sediment load
c     at the end of the current OFE.
c     if contours exist they are assumed not to fail
c
      if (conseq(nowcrp,iplane).eq.0) then
        if (qout.gt.0.0) then
          qsout = dslod2 / effdrn(iplane)
        else
          qsout = 0.0
        end if
      else
        qsout = 0.0
      end if
c
c
      return
      end
