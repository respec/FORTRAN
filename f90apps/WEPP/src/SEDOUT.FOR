      subroutine sedout(iyear,dslost,isum,ievt,ifofe,
     1                  lun1,noout,flag,nowcrp)
c***********************************************************************
c                                                                      *
c     This subroutine is called from SR CONTIN and controls the        *
c     printing of the output. It calls SR MONOUT, SR ANNOUT, SR        *
c     ENDOUT, SR SEDSEG, and SR ENRPRT.                                *
c                                                                      *
c***********************************************************************
c                                                                      *
c    + + + ARGUMENT DECLARATIONS + + +
      real dslost
      integer iyear, isum, ievt, ifofe, lun1, noout, nowcrp,
     1    flag
c     Arguments                                                        *
c     iyear  - flag that indicates getting average annual summaries    *
c              through sedout ( set in CONTIN or WSHDRV)               *
c     dslost - array containing net soil loss/gain at each of 100      *
c              points on each overland flow element                    *
c     isum   - flag to indicate if the information sent to sedout is   *
c              supposed to write to summary files                      *
c              (0 - means no,  1 - means yes )                         *
c     ievt   - flag to indicate if the information sent to sedout is   *
c              supposed to write to event line output                  *
c              summary files  (0 - means no,  1 - means yes )          *
c     ifofe   - flag to indicate if the information sent to sedout is  *
c              supposed element line output  (0 - no; 1 - yes)         *
c     lun1   - flag which indicates type of graphic output desired     *
c              0 - none, 1 - dG/dx plotting file only, 2 - dG/dx plot  *
c              file plus large graphics file for screen plotting       *
c              3 - large graphics file for screen plotting only        *
c     flag   - indicates call from CONTIN or WSHDRV                    *
c     noout  - flag indicating type of output desired                  *
c                    0 - event by event - no summary files             *
c                    1 - event by event - summary files                *
c                    2 - all other output - with summary or graphics   *
c     nowcrp - array containing index numbers for current crops on OFEs*
c     efflen - array containing the values of effective plane length   *
c              for runoff computations.  At the end of any given OFE,  *
c              volume of runoff per unit width is                      *
c                 efflen(iplane)*runoff(iplane)                        *
c              and flow discharge per unit width is                    *
c                 efflen(iplane)*peakro(iplane)                        *
c     avirdt - weighted average interrill detachment                   *
c     outopt - output option for basic output file.
c                  1 = Annual Average; abbreviated, English units
c                  2 = Annual Detailed: detailed, metric units
c                  3 = Event by event: abbreviated, metric units
c                  4 = Event by event: detailed, metric
c                  5 = Monthly: metric
c
c                                                                      *
c***********************************************************************
      include 'pmxcrp.inc'
      include 'pmxcsg.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxpnd.inc'
      include 'pmxprt.inc'
      include 'pmxpts.inc'
      include 'pmxres.inc'
      include 'pmxslp.inc'
      include 'pmxseg.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
      include 'ptilty.inc'
c
c********************************************************************
c                                                                   *
c     Common Blocks                                                 *
c                                                                   *
c********************************************************************
c
      include 'cavloss.inc'
c
      include 'ccliyr.inc'
c
      include 'ccntour.inc'
c
      include 'ccover.inc'
c
      include 'ccrpout.inc'
c
      include 'ccrpvr1.inc'
c
      include 'ccrpvr2.inc'
c
      include 'cdist.inc'
c
      include 'cefflen.inc'
c     read: efflen(mxplan)
c
      include 'cends.inc'
c
      include 'cenrpas.inc'
c
      include 'chydrol.inc'
c
      include 'ciravlo.inc'
c
      include 'cirriga.inc'
c
      include 'coutchn.inc'
c
      include 'cparame.inc'
c
      include 'cpart1.inc'
c
      include 'cparval.inc'
c
      include 'cseddet.inc'
c
      include 'csedld.inc'
c
      include 'cslope.inc'
c
      include 'cslpopt.inc'
c
      include 'csolvar.inc'
c
      include 'cstore1.inc'
c
      include 'cstruc.inc'
c
      include 'cstruct.inc'
c
      include 'csumirr.inc'
c
      include 'cunicon.inc'
c     read outopt, units
c
      include 'cupdate.inc'
c
      include 'cwater.inc'
c
c
c     + + + LOCAL DECLARATIONS + + +
c
      integer i, ilay, j, jend, jun, nelem1, nelem2, nelem3,m,k,jj,l
      integer nowres
      real ss1, ss2, ss3, watcon, x1, x2, x3, avirdt,kgmtpa,mtf,earea,
     1  marea,kgmlpf,sedyld
c     Places deposition and detachment data into column form
c
      dimension dslost(mxplan,100)
      integer ncol1(10), ncol2(10), ncol3(10), ncol4(10), ncol11,
     1    ncol22, ncol33, ncol44, nfelem(1000)
c********************************************************************
c                                                                   *
c     Local variables                                               *
c     jun   :                                                       *
c     ncol11:                                                       *
c     ncol22 :                                                      *
c     ncol33 :                                                      *
c     ncol44 :                                                      *
c     jend   :                                                      *
c     m      :                                                      *
c     x1     :                                                      *
c     ss1    :                                                      *
c     nelem1 :                                                      *
c     x2     :                                                      *
c     ss2    :                                                      *
c     nelem2 :                                                      *
c     x3     :                                                      *
c     ss3    :                                                      *
c     nelem3 :                                                      *
c                                                                   *
c********************************************************************
c
c
      character*4 mths(12)
      character*7 unit(13)
c
      data ncol1 /10 * 1/
      data ncol2 /35, 68, 101, 135, 168, 201, 235, 268, 301, 335/
      data ncol3 /69, 135, 201, 269, 335, 401, 469, 535, 601, 669/
      data ncol4 /33, 67, 101, 133, 167, 201, 233, 267, 301, 333/
      data nfelem /100 * 1, 100 * 2, 100 * 3, 100 * 4, 100 * 5, 100 * 6,
     1    100 * 7, 100 * 8, 100 * 9, 100 * 10/
      data mths /'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug',
     1    'sep', 'oct', 'nov', 'dec'/
      data unit /'     mm', '  kg/m2','    in.','    t/a','    ft.'
     1,' lbs/ft','    lbs','   kg/m','      m','     kg','   t/ha'
     2,'     ha','  acres'/
c     kg/m2>t/a
      kgmtpa=(0.2048*43560)/2000
c     m>ft
      mtf=3.2808
c     kg/m>lbs/ft
      kgmlpf=2.2046/3.2808
c     english hillslope area
      earea=totlen(nplane)*mtf*fwidth(1)*mtf
c     metric hillslope area
      marea=totlen(nplane)*fwidth(1)
c
c     data unit
c     1=mm
c     2=kg/m2
c     3=in.
c     4=t/a
c     5=ft.
c     6=lbs/ft
c     7=lbs
c     8=kg/m
c     9=m
c
c********************************************************************
c
      jun = 31
      if(imodel.eq.2)jun = 32
      if(flag.lt.2)then
        if(noout.le.1)then
          if (ioutpt.eq.2.and.iyear.ne.1) then
            write (jun,2600) ihill, mths(mon), year - ibyear + 1
            call monout
          end if
c
          if (ioutpt.eq.3.and.iyear.ne.1) then
            write (jun,2700) ihill, year - ibyear + 1
            call annout
          end if
c
          if (iyear.eq.1) then
            write (jun,2800)
            call endout
          end if
c
          ncol11 = ncol1(nplane)
          ncol22 = ncol2(nplane)
          ncol33 = ncol3(nplane)
          ncol44 = ncol4(nplane)
          jend = nplane * 100
        end if
c
        call sedseg(dslost,jun,iyear,noout)
c
        if (noout.le.1) then
          if (ioutss.ne.2) then
            write (jun,1300)
            write (jun,1400)
            if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
              write (jun,1500)unit(5),unit(5),unit(5)
              write (jun,1600)unit(4),unit(4),unit(4)
            else
              write (jun,1550)
              write (jun,1650)
            end if
            write (jun,1700)
          end if
c
c
          m = ncol22 - 1
c
          do 20 i = 1, m
c
            j = 0
c
            do 10 k = 1, jend
c
              j = j + 1
              if (k.eq.ncol11) then
                x1 = dstot(k)
                ss1 = stdist(k)
                nelem1 = nfelem(j)
              else if (k.eq.ncol22) then
                x2 = dstot(k)
                ss2 = stdist(k)
                nelem2 = nfelem(j)
              else if (k.eq.ncol33) then
                x3 = dstot(k)
                ss3 = stdist(k)
                nelem3 = nfelem(j)
              end if
c
   10       continue
c
            if (i.lt.ncol44) then
c
              if (ioutss.ne.2) then
                if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
                  write (jun,1200) ss1*mtf, x1*kgmtpa, nelem1, ss2*mtf,
     1              x2*kgmtpa, nelem2, ss3*mtf, x3*kgmtpa, nelem3
                else
                  write (jun,1250) ss1, x1, nelem1, ss2, x2, nelem2, ss3
     1              ,x3, nelem3
                end if
              end if
c
            else
c
              if (ioutss.ne.2) then
                if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
                  write (jun,1200) ss1*mtf, x1*kgmtpa, nelem1, ss2*mtf,
     1              x2*kgmtpa, nelem2
                else
                  write (jun,1250) ss1, x1, nelem1, ss2, x2, nelem2
                end if
              end if
c
            end if
c
            ncol11 = ncol11 + 1
            ncol22 = ncol22 + 1
            ncol33 = ncol33 + 1
c
   20     continue
c
          if (ioutss.ne.2) then
            write (jun,1800)
          end if
c
          write (jun,1900)
          if (iroute.eq.0) avsole = 0.0
c
          if (ioutpt.eq.1.and.iyear.ne.1) then
            write (jun,2000) mths(mon), day, year - ibyear + 1, avsole
            if (avsole.gt.0.0.and.irofe.ne.0) then
              write (jun,2400) irsold / avsole * 100.
            else
              if (conseq(nowcrp,nplane).ne.0) write (jun,3000)
              do 555 jj=1,nplane
                write(jun,2550)jj,avsolc(jj)
 555          continue
            end if
          end if
          if (ioutpt.eq.2.and.iyear.ne.1) then
            write (jun,2100) mths(mon), year - ibyear + 1, avsolm
            if (avsolm.gt.0.0.and.nirrm.ne.0) then
              write (jun,2400) irsolm / avsolm * 100.
              irsolm = 0.0
              nirrm = 0
            end if
          end if
          if (ioutpt.eq.3.and.iyear.ne.1) then
            write (jun,2200) year - ibyear + 1, avsoly
            if (avsoly.gt.0.0.and.nirry.ne.0) then
              write (jun,2400) irsoly / avsoly * 100.
              avsoly = 0.0
              irsoly = 0.
              nirry = 0
            end if
          end if
c
          if (iyear.eq.1) then
            if(outopt.eq.1.and.units.eq.1)then
c                abbreviated english units
c             per unit of width lbs/ft
              write (jun,2300) avsolf*kgmlpf,unit(6)
c             for entire hillslope lbs
              write (jun,2325) (avsolf*fwidth(1))*2.2046,unit(7),
     1            fwidth(1)*3.2808,unit(5)
c             per unit area t/a
              write (jun,2350) (avsolf*fwidth(1)/marea)*kgmtpa,unit(4),
     1            (marea/10000)*2.471,unit(13)
            else
              write (jun,2310) avsolf,unit(8)
              write (jun,2335) avsolf*fwidth(1),unit(10),fwidth(1),
     1            unit(9)
              write (jun,2360) (avsolf*fwidth(1)*0.001)/(marea*0.0001),
     1            unit(11),marea/10000,unit(12)
            end if
            if (avsolf.gt.0.0.and.nirrt.ne.0) write (jun,2400) irsolt /
     1          avsolf * 100. / float(nyear)
          end if
c
c
          write (jun,2500)
cc
        end if
cc
        if (noout.le.1) then
          call enrprt(jun,iyear,nowcrp)
        end if
c
c       open plotting files
c
        if (noout.le.1) then
          if ((lun1.eq.1.or.lun1.eq.2).and.(iyear.eq.1.or.imodel.eq.2))
     1      then
            do 30 i = 1, jend
              write (43,2900) stdist(i), ysdist(i), dstot(i)
   30       continue
          end if
        end if
c
        if (isum.eq.1 .or. ievt.eq.1 .or. ifofe.eq.1) then
          avirdt=0.0
          do 35 l=1,nplane
            avirdt=avirdt+(slplen(l)*irdgdx(l))
   35     continue
          avirdt=avirdt/totlen(nplane)
c
          if(ievt.eq.1)
     1      write (30,1100) day, mon, year - ibyear + 1, prcp * 1000.0,
     1      runoff(nplane) * efflen(nplane) * 1000.0 / totlen(nplane),
     1      avirdt, avedet, maxdet, ptdet, avedep, maxdep, ptdep,
     1      avsole, enrato(nplane)
          do 50 iplane = 1, nplane
            watcon = 0.0
            do 40 ilay = 1, nsl(iplane)
              watcon = watcon + soilw(ilay,iplane)
   40       continue
            nowres = 1
            if(ifofe.eq.1)
     1        write (33,1000) iplane, day, mon, year - ibyear + 1,
     1        prcp * 1000.0,
     1        runoff(iplane) * efflen(iplane) * 1000.0 / totlen(iplane),
     1        effint(iplane) * 3.6e06, peakro(iplane) * 3.6e06,
     1        effdrn(iplane) / 3600, enrato(iplane),
     1        ks(iplane) * 3.6e06, watcon * 1000, lai(iplane),
     1        canhgt(iplane), cancov(iplane) * 100, inrcov(iplane) *
     1        100, rilcov(iplane) * 100, vdmt(iplane), rmagt(iplane) +
     1        rmogt(nowres,iplane), ki(iplane) * kiadjf(iplane) /
     1        1000000., kr(iplane) * kradjf(iplane) * 1000.,
     1        shcrit(iplane) * tcadjf(iplane), width(iplane),
     1        ofelod(iplane)
   50     continue
        end if
c
c     Watershed event output
c
      else
c
        sedyld = 0.0
        do 60, i=1,npart
          sedyld=sedyld+tgsd(i,nelmt)
   60   continue
        if(runvol(nelmt).gt.0.005)
     1      write(30,3100)day, mon, year - ibyear + 1, prcp * 1000,
     1      runvol(nelmt), peakot(nelmt), sedyld * 0.4536
      end if
c
      return
c
c 1000 format (3(1x,i2),1x,i3,1x,f6.1,1x,f6.1,3(1x,f6.2),f5.2,1x,f6.2,1x,
c     1    f5.1,1x,f6.2,1x,f5.2,3(1x,f5.1),6(1x,f5.2))
 1000 format (3(1x,i2),1x,i4,1x,f8.3,1x,f8.3,3(1x,f7.3),f6.3,1x,f7.3,1x,
     1    f7.3,1x,f7.3,1x,f6.3,3(1x,f7.3),6(1x,f6.3),1x,f8.3)
 1100 format (2(1x,i4),1x,i5,1x,f5.1,1x,f7.1,1x,f7.3,2(1x,f6.2),1x,f6.1,
     1    2(1x,f7.2),1x,f6.1,1x,f7.1,1x,f5.2)
 1200 format (3(f7.1,1x,f9.1,1x,i2,5x))
 1250 format (3(f7.2,1x,f9.3,1x,i2,5x))
 1300 format (///2x,'C.  SOIL LOSS/DEPOSITION ALONG SLOPE PROFILE',//,10
     1    x,'Profile distances are from top to bottom of hillslope'//)
 1400 format (1x,'distance',1x,'soil',2x,'flow',4x,'distance',3x,'soil',
     1    2x,'flow',4x,'distance',3x,'soil',2x,'flow')
 1500 format (a,3x,'loss',2x,'elem',3x,a,5x,'loss',2x,'elem',
     1    3x,a,5x,'loss',2x,'elem')
 1550 format (4x,'(m)',3x,'loss',2x,'elem',7x,'(m)',5x,'loss',2x,'elem',
     1    7x,'(m)',5x,'loss',2x,'elem')
 1600 format (6x,a,18x,a,18x,a)
 1650 format (9x,'(kg/m2)',18x,'(kg/m2)',18x,'(kg/m2)')
 1700 format (72('-'),/)
 1800 format (/'note:  (+) soil loss - detachment     (-) soil loss',
     1    ' - deposition')
 1900 format (///'III. OFF SITE EFFECTS  OFF SITE EFFECTS',
     1    '  OFF SITE EFFECTS',/,5x,(3(16('-'),2x))/)
 2000 format (5x,'A.  SEDIMENT LEAVING PROFILE for ',a3,1x,i3,1x,i4,1x,
     1    f9.3,' kg/m'/)
 2100 format (5x,'A.  SEDIMENT LEAVING PROFILE for ',a3,1x,i4,1x,f9.3,
     1    ' kg/m'/)
 2200 format (5x,'A.  SEDIMENT LEAVING PROFILE for year ',i4,1x,f9.3,
     1    ' kg/m'/)
 2300 format (5x,'A.  AVERAGE ANNUAL SEDIMENT LEAVING PROFILE',/,
     1    9x,f9.1,a,' of width')
 2310 format (5x,'A.  AVERAGE ANNUAL SEDIMENT LEAVING PROFILE',/,
     1    9x,f9.3,a,' of width')
 2325 format (9x,f9.1,a,' (based on profile width of  ',f9.1,a,')')
 2335 format (9x,f9.3,a,' (based on profile width of  ',f9.3,a,')')
 2350 format (9x,f9.1,a,' (assuming contributions from ',f9.1,a,')')
 2360 format (9x,f9.3,a,' (assuming contributions from ',f9.3,a,')')
 2400 format (9x,'PERCENTAGE ATTRIBUTED TO IRRIGATION',2x,f5.1,' %'/)
 2500 format (//5x,'B.  SEDIMENT CHARACTERISTICS AND ENRICHMENT')
 2550 format (9x,'Predicted sediment leaving side of OFE ',i2,1x,
     1        'is ',f10.3,' kg/m width')
 2600 format (//5x,'HILLSLOPE ',i1,' MONTHLY SUMMARY ',a3,1x,i4,/,5x,34(
     1    '-'))
 2700 format (//5x,'HILLSLOPE ',i1,' YEARLY SUMMARY FOR ',i4,/,5x,34('-'
     1    ))
 2800 format (//5x,'ANNUAL AVERAGE SUMMARIES',/,5x,24('-'))
 2900 format (1x,f10.3,1x,f10.3,1x,f10.3)
 3000 format (/,9x,'NOTE - CONTOURING IS ACTIVE - SOME/ALL SEDIMENT ',/,
     1    9x,'reported here as leaving the profile is predicted to ',/,
     1    9x,'be exiting from sides.  Total sediment lost from the ',/,
     1    9x,'hillslope profile is divided by original hillslope width',
     1  /,9x,'to give an equivalent value for sediment loss in kg/m',//,
     1    9x,'Values below are sediment discharge rates by OFE',/,
     1    9x,'obtained by dividing contour sediment loss by',/,
     1    9x,'contour row width',/)
c3000 format (/,9x,'CONTOURS USED ON LAST OVERLAND FLOW ELEMENT',/,9x,
c    1    'sediment detached on hillslope does not reach end',
c    1    ' of profile',//)
 3100 format(2(1x,i4),1x,i5,1x,f5.1,1x,f12.2,1x,f12.5,1x,f12.1)
      end
