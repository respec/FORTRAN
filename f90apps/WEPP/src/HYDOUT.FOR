      subroutine hydout(jun,idout,wmelt)
c
c******************************************************************
c                                                                 *
c     Called from subroutine sloss.                               *
c     Prints out the abbreviated hydrology output.                *
c                                                                 *
c******************************************************************
c
c******************************************************************
c                                                                 *
c   Arguments                                                     *
c     jun    -                                                    *
c     idout  -                                                    *
c                                                                 *
c******************************************************************
c
      integer jun, idout
      real wmelt
c
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxpln.inc'
      include 'pmxsrg.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pxstep.inc'
c
c******************************************************************
c                                                                 *
c   Common Blocks                                                 *
c                                                                 *
c******************************************************************
c
      include 'ccliyr.inc'
c
      include 'chydrol.inc'
c
      include 'cefflen.inc'
      include 'cirfurr.inc'
      include 'cirriga.inc'
      include 'cirspri.inc'
c
      include 'cslpopt.inc'
c
      include 'cstruc.inc'
c
      include 'csumirr.inc'
c
      include 'cupdate.inc'
c
c******************************************************************
c                                                                 *
c   Local Variables                                               *
c     mths(12) :                                                  *
c     hyd1   :                                                    *
c     hyd2   :                                                    *
c     hyd3   :                                                    *
c     hyd4   :                                                    *
c     hyd5   :                                                    *
c     hyd6   :                                                    *
c     hyd7   :                                                    *
c     hyd8   :                                                    *
c     hyd9   :                                                    *
c     hyd10  :                                                    *
c                                                                 *
c******************************************************************
c
      save
      real hyd0, hyd1, hyd2, hyd3, hyd4, hyd5, hyd6, hyd7, hyd8, hyd9,
     1     hyd10

      character*4 mths(12)
c
      data mths /'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug',
     1    'sep', 'oct', 'nov', 'dec'/
c
      hyd0 = prcp * 1000.
      hyd1 = rain(iplane) * 1000.
      hyd2 = wmelt * 1000.
c
c dcf Correct error in runoff output for detailed storm by storm output
c dcf - 3-15-2004
c     hyd3 = runoff(iplane) * 1000.
      hyd3 = runoff(iplane) * efflen(nplane) * 1000. / totlen(nplane)
      hyd4 = stmdur / 60.
      hyd5 = effdrn(iplane) / 60.
      hyd6 = peakro(iplane) * 3.6e06
      hyd7 = efflen(iplane)
c
      if (jun.eq.31) then
        if (idout.eq.0) write (jun,1000) ihill
        write (jun,1100) iplane, mths(mon), day, year - ibyear + 1
        if (irsyst.eq.2.and.noirr.ne.0) then
          write (jun,1600) irapld(iplane) * 1000., hyd3,
     1        infvlm(iplane) * 1000., hyd6, hyd5, hyd7
        else
          if (iplane.lt.irofe.or.irofe.eq.0) then
            write (jun,1300) hyd0, hyd1, hyd2, hyd3, hyd4, hyd5,
     1                       hyd6, hyd7
          else
            hyd8 = irdept(iplane) * 1000.
            hyd9 = irdur / 60.
            if (runoff(iplane).gt.0.0) then
              hyd10 = irrund(iplane) / runoff(iplane) * 100.
            else
              hyd10 = 0.0
            end if
            write (jun,1200) hyd0, hyd1, hyd2, hyd3, hyd4, hyd5,
     1                       hyd6, hyd7, hyd8, hyd9, hyd10
          end if
c
          write (jun,1700)
        end if
      end if
c
c******************************************************************
c     *
c     Format statements                                             *
c     *
c******************************************************************
c
c
      return
 1000 format (///'HILLSLOPE ',i1,' RESULTS',/,19('-'),//
     1    'I.   ABBREVIATED EVENT-BY-EVENT HYDROLOGY',/,5x,11('-'),1x,14
     1    ('-'),1x,9('-'))
 1100 format (/7x,'Overland flow element number:',i3/7x,'Event date: ',1
     1    x,a3,1x,i2,', year ',i4)
 1200 format (/,7x,'precipitation amount',f8.2,7x,'rainfall amount    ',
     1    f8.2/,7x,'snow melt amount    ',f8.2,7x,'runoff amount      ',
     1    f8.2/,7x,'rain/melt duration  ',f8.2,7x,'effective duration ',
     1    f8.2/,7x,'peak runoff rate    ',f8.2,7x,'effective length   ',
     1    f8.2/,7x,'irrigation amount   ',f8.2,7x,'irrigation duration',
     1    f8.2,//,7x,f5.1,
     1    ' percent of runoff attributed to irrigation'/)
 1300 format (/,7x,'precipitation amount',f8.2,7x,'rainfall amount    ',
     1    f8.2/,7x,'snow melt amount    ',f8.2,7x,'runoff amount      ',
     1    f8.2/,7x,'rain/melt duration  ',f8.2,7x,'effective duration ',
     1    f8.2/,7x,'peak runoff rate    ',f8.2,7x,'effective length   ',
     1    f8.2/)
 1600 format (/,7x,'Furrow irrigation output:'/,7x,
     1    'volume of water supplied ',f8.2,7x,'depth of runoff  ',f8.2/,
     1 7x,'avg infiltrated depth    ',f8.2,7x,'peak runoff rate ',f8.2/,
     1 7x,'effective duration       ',f8.2,7x,'effective length ',f8.2,
     1    //,7x,
     1    'note:    depths = mm'/,7x,
     1    '           rate = mm/hr'/,7x,
     1    '       duration = min'/,7x,
     1    '         length = meters'/)
 1700 format (7x,'note: amounts = mm, durations = min, rates = mm/hr',
     1    ', length = meters'/)
      end
