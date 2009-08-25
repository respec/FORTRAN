      subroutine endout
c
c******************************************************************
c                                                                 *
c     Called from subroutine sedout.                              *
c     Summarizes soil loss information for the entire simulation  *
c     period.                                                     *
c                                                                 *
c******************************************************************
c
      include 'pmxpln.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
c
c******************************************************************
c                                                                 *
c   Common Blocks                                                 *
c                                                                 *
c******************************************************************
c
      include 'ccliyr.inc'
c
      include 'cstmflg.inc'
c
      include 'cstruc.inc'
c
      include 'csumirr.inc'
c
c*********************************************************************
c                                                                    *
c    sumirr variables udated                                         *
c    irrunt                                                          *
c                                                                    *
c*********************************************************************
c
      include 'csumout.inc'
c
      include 'cunicon.inc'
c     read outopt, units
c
      include 'cupdate.inc'
c
c******************************************************************
c                                                                 *
c   Local Variables                                               *
c     avernf :                                                    *
c     averun :                                                    *
c     unit(2):                                                    *
c     ii     :                                                    *
c     aveirr :                                                    *
c                                                                 *
c******************************************************************
c
      real avernf, averun, aveirr, sumh2o, pctirg,avemlt
      integer ii, ipl, jkl, sumevt
c
      character*5 unit(5)
      data unit /'   mm', 'kg/m2','  in.', 'in/yr','mm/yr'/
c
      ii = 1
c
      mxevnt = 0
      mxmelt = 0
c
      do 10 jkl = 1, nplane
        if (nrunot(jkl).gt.mxevnt) mxevnt = nrunot(jkl)
        if (nmunot(jkl).gt.mxmelt) mxmelt = nmunot(jkl)
   10 continue
c
      if (nirrt.eq.0) then
        write (31,1100) ii, nyear
c       end new
c
        if(outopt.eq.1.and.units.eq.1)then
c
c       write english or metric units for abbreviated average annual output file
c
          write (31,1300) nraint, traint/25.4, unit(3), mxevnt,
     1      trunot(nplane)/25.4, unit(3), mxmelt,
     1      tmunot(nplane)/25.4, unit(3)
        else
          write (31,1350) nraint, traint, mxevnt,
     1      trunot(nplane), mxmelt, tmunot(nplane)
        end if
      else
        write (31,1000) ii, nyear
c
        if(outopt.eq.1.and.units.eq.1)then
c
c       write english or metric units for abbreviated average annual output file
c
          write (31,1200) nraint, traint/25.4, nirrt,
     1      tirrt/25.4, mxevnt, trunot(nplane)/25.4,
     1      mxmelt, tmunot(nplane)/25.4, ncommt
        else
          write (31,1250) nraint, traint, nirrt, tirrt,
     1      mxevnt, trunot(nplane), mxmelt,
     1      tmunot(nplane), ncommt
        end if
      end if
c
      avernf = traint / float(jyear)
      averun = trunot(nplane) / float(jyear)
      avemlt = tmunot(nplane) / float(jyear)
c
      if (nirrt.eq.0) then
c
        if(outopt.eq.1.and.units.eq.1)then
c
c       write english or metric units for abbreviated average annual output file
c
          write (31,1600) jyear,avernf/25.4,unit(4),averun/25.4,unit(4),
     1      avemlt/25.4,unit(4)
        else
        write (31,1650) jyear,avernf,unit(1),averun,unit(1),avemlt,
     1   unit(1)
        end if
      else
        aveirr = tirrt / float(jyear)
c
        if(outopt.eq.1.and.units.eq.1)then
c
c       write english or metric units for abbreviated average annual output file
c
          write (31,1400) jyear,avernf/25.4,unit(4),aveirr/25.4,unit(4),
     1      averun/25.4,unit(4),avemlt/25.4,unit(4)
        else
          write (31,1450) jyear,avernf,unit(1),aveirr,unit(1),averun,
     1      unit(1),avemlt,unit(1)
        end if
c
        do 20 ipl = 1, nplane
          sumevt = nrunot(ipl) + nmunot(ipl)
          sumh2o = trunot(ipl) + tmunot(ipl)
          if (sumevt.gt.0.0) then
            pctirg = ((irrunt(ipl)*1000.0) / sumh2o ) * 100.0
          else
            pctirg = 0.0
          end if
          write (31,1500) ipl, pctirg
   20   continue
        irper=pctirg/100
c
      end if
c
      return
 1000 format (//'I.   RAINFALL, IRRIGATION, AND RUNOFF SUMMARY',/,5x,8(
     1    '-'),2x,10('-'),2x,3('-'),1x,6('-'),1x,7('-'),//,6x,
     1    'total summary: ',' years ',i4,' - ',i4)
 1100 format (//'I.   RAINFALL AND RUNOFF SUMMARY',/,5x,8('-'),1x,3('-'
     1    ),1x,6('-'),1x,7('-'),//,6x,'total summary: ',' years ',i4,
     1    ' - ',i4)
 1200 format (/5x,i5,
     1    ' storms produced                       ',f9.1,1x,
     1    ' in. of precipitation',/,5x,i5,
     1    ' irrigations supplied                  ',f9.1,1x,
     1    ' in. of water',/,5x,i5,
     1    ' rain/irrigation runoff events produced',f9.1,1x,
     1    ' in. of runoff',/,5x,i5,
     1    ' snow melts and/or',/,10x,
     1    '   events during winter produced       ',f9.1,1x,
     1    ' in. of runoff',/,5x,i5,
     1    ' irrigations occurred on days with rainfall',/)
 1250 format (/5x,i5,
     1    ' storms produced                       ',f9.2,
     1    ' mm of precipitation',/,5x,i5,
     1    ' irrigations supplied                  ',f9.2,
     1    ' mm of water',/,5x,i5,
     1    ' rain/irrigation runoff events produced',f9.2,
     1    ' mm of runoff',/,5x,i5,
     1    ' snow melts and/or',/,10x,
     1    '   events during winter produced       ',f9.2,
     1    ' mm of runoff',/,5x,i5,
     1    ' irrigations occurred on days with rainfall',/)
 1300 format(/5x,i5,
     1    ' storms produced                       ',f9.1,1x,a,
     1    ' of precipitation',/,5x,i5,
     1    ' rain storm runoff events produced     ',f9.1,1x,a,
     1    ' of runoff',/,5x,i5,
     1    ' snow melts and/or',/,10x,
     1    '   events during winter produced       ',f9.1,1x,a,
     1    ' of runoff',/)
 1350 format(/5x,i5,
     1    ' storms produced                       ',f9.2,
     1    ' mm of precipitation',/,5x,i5,
     1    ' rain storm runoff events produced     ',f9.2,
     1    ' mm of runoff',/,5x,i5,
     1    ' snow melts and/or',/,10x,
     1    '   events during winter produced       ',f9.2,
     1    ' mm of runoff',/)
 1400 format (6x,'annual averages'/6x,'---------------'//6x,
     1    '  Number of years                              ',
     1    3x,i4,/,6x,
     1    '  Mean annual precipitation                    ',
     1    f7.1,1x,a,/,6x,
     1    '  Mean annual irrigation                       ',
     1    f7.1,1x,a,/,6x,
     1    '  Mean annual runoff from rainfall/irrigation  ',
     1    f7.1,1x,a,/,6x,
     1    '  Mean annual runoff from snow melt',/,6x,
     1    '    and/or events during winter                ',
     1    f7.1,1x,a,//,6x,
     1    '      Overland     Fraction of Mean Annual Runoff'/6x,
     1    '    Flow Element      Attributed to Irrigation,'/6x,
     1    '       Number                     %')
 1450 format (6x,'annual averages'/6x,'---------------'//6x,
     1    '  Number of years                              ',
     1    3x,i4,/,6x,
     1    '  Mean annual precipitation                    ',
     1    f7.2,1x,a,/,6x,
     1    '  Mean annual irrigation                       ',
     1    f7.2,1x,a,/,6x,
     1    '  Mean annual runoff from rainfall/irrigation  ',
     1    f7.2,1x,a,/,6x,
     1    '  Mean annual runoff from snow melt',/,6x,
     1    '    and/or events during winter                ',
     1    f7.2,1x,a,//,6x,
     1    '      Overland     Fraction of Mean Annual Runoff'/6x,
     1    '    Flow Element      Attributed to Irrigation,'/6x,
     1    '       Number                     %')
 1500 format (15x,i2,21x,f5.1)
 1600 format (6x,'annual averages'/6x,'---------------'//6x,
     1    '  Number of years                              ',
     1    3x,i4,/,6x,
     1    '  Mean annual precipitation                    ',
     1    f7.1,1x,a,/,6x,
     1    '  Mean annual runoff from rainfall             ',
     1    f7.1,1x,a,/,6x,
     1    '  Mean annual runoff from snow melt',/,6x,
     1    '    and/or rain storm during winter            ',
     1    f7.1,1x,a/)
 1650 format (6x,'annual averages'/6x,'---------------'//6x,
     1    '  Number of years                              ',
     1    3x,i4,/,6x,
     1    '  Mean annual precipitation                    ',
     1    f7.2,1x,a,/,6x,
     1    '  Mean annual runoff from rainfall             ',
     1    f7.2,1x,a,/,6x,
     1    '  Mean annual runoff from snow melt',/,6x,
     1    '    and/or rain storm during winter            ',
     1    f7.2,1x,a/)
      end