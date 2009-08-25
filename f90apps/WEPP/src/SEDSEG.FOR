      subroutine sedseg(dslost,jun,iyear,noout)
c*******************************************************************
c                                                                  *
c     This subroutine is called from SR SEDOUT and breaks the      *
c     hillslope profile into detachment or deposition segments.    *
c     It calls SR SEDIST and SR SEDSTA.                            *
c     It builds abbrev.raw for OCP                                 *
c                                                                  *
c*******************************************************************
c                                                                  *
c     Arguments                                                    *
c     dslost  - array containing the net soil loss/gain at each    *
c               of the 100 points on each OFE                      *
c     jun     - unit number of file to write output to             *
c     iyear   - flag for printing out annual soil loss output      *
c     noout   - flag indicating printing of event by event         *
c               summary files is desired (see SEDOUT)              *
c                                                                  *
c*******************************************************************
c     *
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer jun, iyear, noout
      real dslost
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxpln.inc'
c     include 'pmxres.inc'
      include 'pmxseg.inc'
      include 'pmxpts.inc'
c     include 'pmxprt.inc'
      include 'pmxhil.inc'
c     include 'pmxtil.inc'
c     include 'pmxtls.inc'
c     include 'pntype.inc'
c
c*****************************************************************
c                                                                *
c     Common Blocks                                              *
c                                                                *
c*****************************************************************
c
      include 'cavloss.inc'
      include 'ccntour.inc'
c     include 'ccrpprm.inc'
c     include 'cenrpas.inc'
c     include 'ciravlo.inc'
c     include 'cirriga.inc'
c     include 'cpart.inc'
      include 'cseddet.inc'
c
c********************************************************************
c                                                                   *
c     seddet variables updated                                      *
c     avedet, maxdet, ptdep, ptdet, avedep, maxdep                  *
c                                                                   *
c********************************************************************
c
      include 'csedld.inc'
c
c********************************************************************
c                                                                   *
c     sedld variables updated                                       *
c     jflag(mxseg),ibegin,iend,lseg                                 *
c                                                                   *
c********************************************************************
c
      include 'cslpopt.inc'
      include 'cstmflg.inc'
      include 'cstruc.inc'
      include 'csumout.inc'
c
      include 'cunicon.inc'
c     read outopt, units
c     include 'csumirr.inc'
c
c*****************************************************************
c                                                                *
c     Local variables                                            *
c     deppt1 : distance where deposition section begins (m)      *
c     deppt2 : distance where deposition section ends (m)        *
c     dpavls : average deposition in section (kg/m**2)           *
c     detpt1 : distance where detachment section begins (m)      *
c     detpt2 : distance where detachment section ends (m)        *
c     dtavls : average detachment in section (kg/m**2)           *
c     detstd : standard deviation of detachment in sect.(kg/m**2)*
c     detmax : maximum detachment in section (kg/m**2)           *
c     pdtmax : point of maximum detachment (m)                   *
c     detmin : minimum detachment in section (kg/m**2)           *
c     pdtmin : point of minimum detachment (m)                   *
c     depstd : standard deviation of deposition in sect.(kg/m**2)*
c     depmax : maximum deposition in section (kg/m**2)           *
c     pdpmax : point of maximum deposition in section (m)        *
c     depmin : minimum deposition in section (kg/m**2)           *
c     pdpmin : point of minimum deposition in section (m)        *
c                                                                *
c*****************************************************************
c
c
      dimension dslost(mxplan,100)
c
      dimension detstd(mxseg), detmax(mxseg), pdtmax(mxseg),
     1    detmin(mxseg), pdtmin(mxseg)
c
      dimension depstd(mxseg), depmax(mxseg), pdpmax(mxseg),
     1    depmin(mxseg), pdpmin(mxseg)
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c        ADDED TOTFAL AND OFEFAL FOR NEW ABBREV.RAW
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      dimension detdis(mxseg), depdis(mxseg), idtsin(mxseg),
     1    idpsin(mxseg),
     1    detpt1(mxseg),detpt2(mxseg),dtavls(mxseg),
     1    deppt1(mxseg),deppt2(mxseg),dpavls(mxseg)
c
c
c     + + + LOCAL VARIABLE DECLARATIONS + + +
c
      integer jadet, jadep, jdet, jdep, itsin, ipsin, lend,
     1    i, icnt, icont, idpsin, idtsin, iii, j, kbeg, kk,
     1    lbeg
      real sum1, sum2, filoss, fidep, totmax, pdtmx, pdpmx,
     1    depdis, depmax, depmin, deppt1,detdis,
     1    deppt2, depstd, detmax, detmin, detpt1, detpt2, detstd,
     1    dpavls, dtavls, pdpmax,
     1    pdpmin, pdtmax, pdtmin, mtf, kgmtpa
      character*7 unit(9)
      data unit /'     mm', '  kg/m2','    in.','    t/a','    ft.'
     1,' lbs/ft','    lbs','   kg/m','      m'/
c     data unit
c     1=mm
c     2=kg/m2
c     3=in.
c     4=t/a
c     5=ft.
c     6=lbs/ft
c     7=lbs
c     8=kg/m
c*******************************************************************
c
      avedep = 0.0
      avedet = 0.0
      maxdep = 0.0
      maxdet = 0.0
      ptdep = 0.0
      ptdet = 0.0
      tdep(ihill) = 0.0
      tdet(ihill) = 0.0
c
      if (noout.le.1) then
        write (jun,2500)
        icont = 0
        do 10 iplane = 1, nplane
          if (iyear.eq.1) then
            if (fail(1,iplane).gt.0) then
              write (jun,2000) fail(1,iplane), iplane
              totfal(iplane) = totfal(iplane) + fail(1,iplane)
              icont = 1
            end if
            if (fail(2,iplane).gt.0) then
              write (jun,2100) fail(2,iplane), iplane
              totfal(iplane) = totfal(iplane) + fail(2,iplane)
              icont = 1
            end if
            if (fail(3,iplane).gt.0) then
              write (jun,2200) fail(3,iplane), iplane
              totfal(iplane) = totfal(iplane) + fail(3,iplane)
              icont = 1
            end if
          end if
   10   continue
        if (icont.eq.1) write (jun,1900)
      end if
c
      call sedist(dslost)
c
      lseg = 1
      jadep = 0
      jadet = 0
      sum1 = 0.0
      sum2 = 0.0
      jdet = 0
      jdep = 0
      itsin = 0
      ipsin = 0
      lend = nplane * 100
      kbeg = 0
      lbeg = 1
      icnt = 0
c
      do 20 j = 1, lend
        if (dstot(j).ne.0.0) then
          if (kbeg.eq.0) then
            lbeg = j
            kbeg = 1
          end if
          icnt = j
        end if
   20 continue
c
      ibegin = lbeg
      if (icnt.lt.lend) then
        lend = icnt + 1
      end if
c
      if (dstot(lbeg).gt.0.0) jflag(lseg) = 1
      if (dstot(lbeg).lt.0.0) jflag(lseg) = 0
c
c
c     Added by dcf 4/16/90 to cover possibility if dstot = 0
c
      if (dstot(lbeg).eq.0.0) jflag(lseg) = 2
c
      do 30 i = lbeg + 1, lend
        if ((jflag(lseg).eq.1.and.dstot(i).le.0.0).or.(i.eq.lend.and.
     1      dstot(i).gt.0.0)) then
c
          jadet = jadet + 1
          iend = i - 1
          if (i.eq.lend.and.dstot(i).gt.0.0) iend = i
c
c         if the beginning of the detachment is the first point on the slope
c         set the point to zero otherwise average i with the point before it
c
          if (ibegin.eq.1) then
            detpt1(jadet) = 0.0
          else
            detpt1(jadet) = stdist(ibegin-1)
          end if
c
          detpt2(jadet) = stdist(iend)
c
          detdis(jadet) = detpt2(jadet) - detpt1(jadet)
c
          if (i.eq.lend.and.dstot(i).lt.0.0) then
            jadep = jadep + 1
            idpsin(jadep) = 1
            dpavls(jadep) = dstot(lend)
            depstd(jadep) = 0.0
            deppt1(jadep) = stdist(lend-1)
            deppt2(jadep) = stdist(lend)
            depdis(jadep) = deppt2(jadep) - deppt1(jadep)
            depmax(jadep) = dstot(lend)
            pdpmax(jadep) = stdist(lend)
            depmin(jadep) = dstot(lend)
            pdpmin(jadep) = stdist(lend)
            ipsin = 1
            jdep = 1
          end if
          if (ibegin.eq.iend) then
            idtsin(jadet) = 1
            detpt1(jadet) = stdist(ibegin-1)
            detpt2(jadet) = stdist(ibegin)
            detdis(jadet) = detpt2(jadet) - detpt1(jadet)
            dtavls(jadet) = dstot(ibegin)
            detstd(jadet) = 0.0
            detmax(jadet) = dstot(ibegin)
            pdtmax(jadet) = stdist(ibegin)
            detmin(jadet) = dstot(ibegin)
            pdtmin(jadet) = stdist(ibegin)
            itsin = 1
            jdet = 1
          else
            idtsin(jadet) = 0
            call sedsta(jadet,dtavls,detstd,detmax,pdtmax,detmin,pdtmin)
            jdet = 1
          end if
c
          ibegin = iend + 1
          lseg = lseg + 1
          if (dstot(i).eq.0.0) jflag(lseg) = 2
          if (dstot(i).lt.0.0) jflag(lseg) = 0
c
        else if ((jflag(lseg).eq.0.and.dstot(i).ge.0.0).or.(i.eq.lend
     1      .and.dstot(i).lt.0.0)) then
c
          jadep = jadep + 1
          iend = i - 1
          if (i.eq.lend.and.dstot(i).lt.0.0) iend = i
          if (ibegin.eq.1) then
            deppt1(jadep) = 0.0
          else
            deppt1(jadep) = stdist(ibegin-1)
          end if
c
          deppt2(jadep) = stdist(iend)
          depdis(jadep) = deppt2(jadep) - deppt1(jadep)
c
          if (i.eq.lend.and.dstot(i).gt.0.0) then
            jadet = jadet + 1
            idtsin(jadet) = 1
            dtavls(jadet) = dstot(lend)
            detstd(jadet) = 0.0
            detpt1(jadet) = stdist(lend-1)
            detpt2(jadet) = stdist(lend)
            detdis(jadet) = detpt2(jadet) - detpt1(jadet)
            detmax(jadet) = dstot(lend)
            detmin(jadet) = dstot(lend)
            pdtmax(jadet) = stdist(lend)
            pdtmin(jadet) = stdist(lend)
            itsin = 1
            jdet = 1
          end if
          if (ibegin.eq.iend) then
            idpsin(jadep) = 1
            deppt1(jadep) = stdist(ibegin-1)
            deppt2(jadep) = stdist(ibegin)
            depdis(jadep) = deppt2(jadep) - deppt1(jadep)
            dpavls(jadep) = dstot(ibegin)
            depmin(jadep) = dstot(ibegin)
            depmax(jadep) = dstot(ibegin)
            depstd(jadep) = 0.0
            pdpmax(jadep) = stdist(ibegin)
            pdpmin(jadep) = stdist(ibegin)
            ipsin = 1
            jdep = 1
          else
            idpsin(jadep) = 0
            call sedsta(jadep,dpavls,depstd,depmax,pdpmax,depmin,pdpmin)
            jdep = 1
          end if
c
          ibegin = iend + 1
          lseg = lseg + 1
          if (dstot(i).eq.0.0) jflag(lseg) = 2
          if (dstot(i).gt.0.0) jflag(lseg) = 1
c
        else if (jflag(lseg).eq.2.and.dstot(i).ne.0.0) then
          iend = i - 1
          if (i.eq.lend) then
            if (jflag(lseg).eq.1) then
              jadet = jadet + 1
              idtsin(jadet) = 1
              detpt1(jadet) = stdist(lend-1)
              detpt2(jadet) = stdist(lend)
              detdis(jadet) = detpt2(jadet) - detpt1(jadet)
              dtavls(jadet) = dstot(lend)
              detstd(jadet) = 0.0
              detmax(jadet) = dstot(lend)
              detmin(jadet) = dstot(lend)
              pdtmin(jadet) = stdist(lend)
              pdtmax(jadet) = stdist(lend)
              itsin = 1
              jdet = 1
            else if (jflag(lseg).eq.0) then
              jadep = jadep + 1
              idpsin(jadep) = 1
              deppt1(jadep) = lend
              deppt2(jadep) = lend
              depdis(jadep) = deppt2(jadep) - deppt1(jadep)
              dpavls(jadep) = dstot(lend)
              depstd(jadep) = 0.0
              depmax(jadep) = dstot(lend)
              depmin(jadep) = dstot(lend)
              pdpmin(jadep) = stdist(lend)
              pdpmax(jadep) = stdist(lend)
              ipsin = 1
              jdep = 1
            end if
          end if
          lseg = lseg + 1
          ibegin = iend + 1
          if (dstot(i).gt.0.0) jflag(lseg) = 1
          if (dstot(i).lt.0.0) jflag(lseg) = 0
        end if
c
   30 continue
c
c
      if (jadet.gt.0) then
        if (noout.le.1) write (jun,2600)
        if (jdet.gt.0) then
          totmax = detmax(1)
          pdtmx = pdtmax(1)
          do 40 kk = 1, jadet
            sum1 = sum1 + (dtavls(kk)*detdis(kk))
            sum2 = sum2 + detdis(kk)
c
            if (detmax(kk).gt.totmax) then
              totmax = detmax(kk)
              pdtmx = pdtmax(kk)
            end if
c
   40     continue
c
          if (sum2.ne.0.0) then
            filoss = sum1 / sum2
            tdet(ihill) = sum2*fwidth(iplane)*filoss
c     kg/m2>t/a
      kgmtpa=4.4605
c     m>ft
      mtf=3.2808
            if (noout.le.1) then
              if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
                write (jun,2700) filoss*kgmtpa,unit(4)
                write (jun,2800) totmax*kgmtpa,unit(4),
     1               pdtmx*mtf,unit(5)
              else
c               metric units
                write (jun,2750) filoss
                write (jun,2850) totmax, pdtmx
              end if
              if (imodel.eq.2.or.(imodel.eq.1.and.iyear.ne.1.and.ioutpt
     1            .eq.1)) then
                do 50 iii = 1, nplane
                  if(outopt.eq.1.and.units.eq.1)then
c                   abbreviated english units
                    write (jun,2900) irdgdx(iii)*kgmtpa,unit(4), iii
                  else
                    write (jun,2950) irdgdx(iii), iii
                  end if
   50           continue
              end if
            end if
            avedet = filoss
            maxdet = totmax
            ptdet = pdtmx
          end if
          if (noout.le.1) then
                if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
                  if (ioutss.ne.2) write (jun,3500)unit(5),unit(4),
     1              unit(4),unit(4),unit(5),unit(4),unit(5)
                else
                  if (ioutss.ne.2) write (jun,3550)
                end if
          end if
c
          do 60 kk = 1, jadet
            if (ioutss.ne.2) then
              if (noout.le.1) then
                if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
                  write (jun,3300) detpt1(kk)*mtf,detpt2(kk)*mtf,
     1               dtavls(kk)*kgmtpa,detstd(kk)*kgmtpa,
     1               detmax(kk)*kgmtpa, pdtmax(kk)*mtf,
     1               detmin(kk)*kgmtpa, pdtmin(kk)*mtf
                else
c                metric units
                  write (jun,3350)detpt1(kk),detpt2(kk),
     1               dtavls(kk),detstd(kk),detmax(kk), pdtmax(kk),
     1               detmin(kk), pdtmin(kk)
                end if
              end if
            end if
c
c         endif
c
   60     continue
        end if
c
c       output summary of erosion and detatchment to abbrev.raw
c
c
        if (itsin.gt.0) then
          do 70 kk = 1, jadet
            if (idtsin(kk).eq.1) then
              if (ioutss.ne.2) then
                if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
                   if (noout.le.1) write (jun,3400)unit(5),unit(4)
                else
                   if (noout.le.1) write (jun,3450)
                end if

                go to 80
              end if
            end if
c
   70     continue
c
   80     do 90 kk = 1, jadet
            if (idtsin(kk).eq.1) then
              if (ioutss.ne.2) then
                if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
                  if (noout.le.1) write (jun,3600) detpt2(kk)*mtf,
     1              dtavls(kk)*kgmtpa
                else
                  if (noout.le.1) write (jun,3650) detpt2(kk),
     1              dtavls(kk)
                end if
              end if
            end if
   90     continue
        end if
      end if
c
c
      if (jadep.gt.0) then
        if (noout.le.1) write (jun,3000)
        if (jdep.gt.0) then
          sum1 = 0.0
          sum2 = 0.0
          totmax = depmax(1)
          pdpmx = pdpmax(1)
c
          do 100 kk = 1, jadep
            sum1 = sum1 + (dpavls(kk)*depdis(kk))
            sum2 = sum2 + depdis(kk)
            if (depmax(kk).lt.totmax) then
              totmax = depmax(kk)
              pdpmx = pdpmax(kk)
            end if
c
  100     continue
c
          if (sum2.ne.0.0) then
            fidep = sum1 / sum2
            tdep(ihill)=sum2*fwidth(iplane)*fidep
            if (noout.le.1) then
              if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
                write (jun,3100) fidep*kgmtpa,unit(4)
                write (jun,3200) totmax*kgmtpa,unit(4),pdpmx*mtf,unit(5)
              else
                write (jun,3150) fidep
                write (jun,3250) totmax, pdpmx
              end if
            end if
            avedep = fidep
            maxdep = totmax
            ptdep = pdpmx
          end if
          if (noout.le.1) then
            if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
              if (ioutss.ne.2) write (jun,3700)unit(5),unit(4),unit(4),
     1           unit(4),unit(5),unit(4),unit(5)
            else
              if (ioutss.ne.2) write (jun,3750)
            end if
          end if
c
          do 110 kk = 1, jadep
c           if(idpsin(kk).ne.1) then
            if (ioutss.ne.2) then
              if (noout.le.1) then
                if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
c
                  write (jun,3300) deppt1(kk)*mtf, deppt2(kk)*mtf,
     1              dpavls(kk)*kgmtpa, depstd(kk)*kgmtpa,
     1              depmax(kk)*kgmtpa, pdpmax(kk)*mtf,
     1              depmin(kk)*kgmtpa, pdpmin(kk)*mtf
                else
                  write (jun,3350) deppt1(kk), deppt2(kk), dpavls(kk),
     1              depstd(kk), depmax(kk), pdpmax(kk), depmin(kk),
     1              pdpmin(kk)
                end if
              end if
            end if
c         endif
  110     continue
        end if
        if (ipsin.gt.0) then
          do 120 kk = 1, jadep
            if (idpsin(kk).eq.1) then
              if (ioutss.ne.2) then
                if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
c
                  if (noout.le.1) write (jun,3800)unit(5),unit(4)
                else
                  if (noout.le.1) write (jun,3850)
                end if
                go to 130
              end if
            end if
  120     continue
c
  130     do 140 kk = 1, jadep
            if (idpsin(kk).eq.1) then
              if (ioutss.ne.2) then
                if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
c
                  if (noout.le.1) write (jun,3600) deppt2(kk)*mtf,
     1                dpavls(kk)*kgmtpa
                else
                  if (noout.le.1) write (jun,3650) deppt2(kk),
     1                dpavls(kk)
                end if

              end if
            end if
  140     continue
        end if
      end if
      return

 1900 format (1x,/,1x,
     1    ' The WEPP Hillslope model has detected the failure of',
     1    ' contours',/,
     1    ' during the simulation. The model has run the hillslope',
     1    ' assuming',/,
     1    ' no failure. However the contour inputs should be ',
     1    'reconfigured',/,
     1    ' so that the contours do not fail or the WEPP Watershed ',
     1    'version',/,' should be run with a concentrated flow channel.'
     1    )
 2000 format (1x,/,1x,'Contours Failed in ',i3,' cases on OFE ',i3,/,10
     1    x,' - contour slope is > = average slope')
 2100 format (1x,/,1x,'Contours Failed in ',i3,' cases on OFE ',i3,/,10
     1    x,'- contour row sideslope is less than the hill slope')
 2200 format (1x,/,1x,'Contours Failed in ',i3,' cases on OFE  ',i3,/,10
     1    x,' - contours under designed')
 2500 format (//'II.  ON SITE EFFECTS  ON SITE EFFECTS',
     1    '  ON SITE EFFECTS',/,5x,(3(15('-'),2x))/)
 2600 format (/2x,'A.  AREA OF NET SOIL LOSS')
c     data unit
c     1=mm
c     2=kg/m2
c     3=in.
c     4=t/a
c     5=ft.
c     6=lbs/ft
c     7=lbs
c     8=kg/m
 2700 format (/6x,'** Soil Loss (Avg. of Net Detachment',' Areas) = ',f8
     1    .1,a,' **')
 2750 format (/6x,'** Soil Loss (Avg. of Net Detachment',' Areas) = ',f8
     1    .3,' kg/m2 **')
 2800 format (6x,'** Maximum Soil Loss  = ',f8.1,a,' at ',f7.1,
     1    a,' **'/)
 2850 format (6x,'** Maximum Soil Loss  = ',f8.3,' kg/m2 at ',f7.2,
     1    ' meters **'/)
 2900 format (6x,'** Interrill Contribution = ',f8.1,a,
     1    ' for OFE #',i2)
 2950 format (6x,'** Interrill Contribution = ',f8.3,' kg/m2 ',
     1    ' for OFE #',i2)
 3000 format (/2x,'B.  AREA OF SOIL DEPOSITION')
 3100 format (/6x,'** Soil Deposition (Avg. of Net Deposition',
     1    ' Areas) = ',f9.1,a,' **')
 3150 format (/6x,'** Soil Deposition (Avg. of Net Deposition',
     1    ' Areas) = ',f9.3,' kg/m2 **')
 3200 format (6x,'** Maximum Soil Deposition  = ',f9.1,a,' at ',f7
     1    .1,a,' **'/)
 3250 format (6x,'** Maximum Soil Deposition  = ',f9.3,' kg/m2 at ',f7
     1    .2,' meters **'/)
 3300 format (f7.1,'-',f7.1,1x,f8.1,2x,f8.1,2x,f9.1,1x,f7.1,2x,f8.1,2x,
     1    f7.1)
 3350 format (f7.2,'-',f7.2,1x,f8.3,2x,f8.3,2x,f9.3,1x,f7.2,2x,f8.3,2x,
     1    f7.2)
 3400 format (/6x,'Single Point',5x,'Single Point',/,7x,'Soil Area',10x,
     1    'Loss',/,9x,a,13x,a,/,6x,30('-'))
 3450 format (/6x,'Single Point',5x,'Single Point',/,7x,'Soil Area',10x,
     1    'Loss',/,9x,'(m)',13x,'(kg/m2/)',/,6x,30('-'))
 3500 format (/,6x,'Area of',4x,'Soil Loss',3x,'Soil Loss',3x,'MAX',3x,
     1    'MAX Loss',3x,'MIN',3x,'MIN Loss',/,6x,'Net Loss',6x,'MEAN',6
     1    x,'STDEV',6x,'Loss',4x,'Point',4x,'Loss',3x,'Point',/,4x,
     1    a,5x,a,4x,a,3x,a,2x,a,1x,a,1x,a,/,72('-'))
 3550 format (/,6x,'Area of',4x,'Soil Loss',3x,'Soil Loss',3x,'MAX',3x,
     1    'MAX Loss',3x,'MIN',3x,'MIN Loss',/,6x,'Net Loss',6x,'MEAN',6
     1    x,'STDEV',6x,'Loss',4x,'Point',4x,'Loss',3x,'Point',/,8x,
     1    '(m)',7x,'(kg/m2)',5x,'(kg/m2)',2x,'(kg/m2)',4x,'(m)',4x,
     1    '(kg/m2)',2x,'(m)',/,72('-'))
 3600 format (6x,f7.1,9x,f9.1)
 3650 format (6x,f7.2,9x,f9.3)
 3700 format (6x,'Area of',4x,'Soil Dep',3x,'Soil Dep',5x,'MAX',4x,
     1    'MAX Dep',3x,'MIN',3x,'MIN Dep',/,6x,'Net Dep',7x,'MEAN',5x,
     1    'STDEV',7x,'Dep',5x,'Point',4x,'Dep',4x,'Point',/,4x,
     1    a,5x,a,4x,a,3x,a,2x,a,1x,a,1x,a,/,72('-'))
 3750 format (6x,'Area of',4x,'Soil Dep',3x,'Soil Dep',5x,'MAX',4x,
     1    'MAX Dep',3x,'MIN',3x,'MIN Dep',/,6x,'Net Dep',7x,'MEAN',5x,
     1    'STDEV',7x,'Dep',5x,'Point',4x,'Dep',4x,'Point',/,8x,'(m)',7x,
     1    '(kg/m2)',4x,'(kg/m2)',3x,'(kg/m2)',4x,'(m)',4x,'(kg/m2)',2x,
     1    '(m)',/,72('-'))
 3800 format (/6x,'Single Point',5x,'Single Point',/,7x,'Soil Area',10x,
     1    'Dep',/,9x,a,13x,a,/,6x,30('-'))
 3850 format (/6x,'Single Point',5x,'Single Point',/,7x,'Soil Area',10x,
     1    'Dep',/,9x,'(m)',13x,'(kg/m2/)',/,6x,30('-'))

      end
