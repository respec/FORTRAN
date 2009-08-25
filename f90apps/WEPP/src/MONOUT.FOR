      subroutine monout
c
c
c     + + + PURPOSE + + +
c
c     Generates the monthly output from the model
c
c     Called from SEDOUT
c     Author(s): Nearing,Ascough
c     Reference in User Guide:
c
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxpln.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'ccliyr.inc'
c
      include 'ciravlo.inc'
c
      include 'csumirr.inc'
c
      include 'cstruc.inc'
c
      include 'csumout.inc'
c
      include 'cupdate.inc'
c
c       read: nmon
c
c     + + + LOCAL VARIABLES + + +
c
      real irrunp
      character*4 mths(12)
      integer ipl
c
c     + + + LOCAL DEFINITIONS + + +
c
c     mths(12) : three letter abbreviation for month of year
c     irrunp   : fraction of runoff water contributed to irrigation
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
      data mths /'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug',
     1    'sep', 'oct', 'nov', 'dec'/
c
c     + + + END SPECIFICATIONS + + +
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
      if (nirrm.eq.0) then
        write (31,1100)
        write (31,1200) mths(mon), year - ibyear + 1
        write (31,1400)
        write (31,1600) nrainm, trainm, nrunom(nplane), trunom(nplane),
     1                  nmunom(nplane), tmunom(nplane)
        mrro=trunom(nplane)
        mrain=trainm
        mmelt=tmunom(nplane)
      else
        write (31,1000)
        write (31,1200) mths(mon), year - ibyear + 1
        write (31,1300)
        write (31,1500) nrainm, trainm, nirrm, tirrm, nrunom(nplane),
     1      trunom(nplane), nmunom(nplane), tmunom(nplane)
c
        mrain=trainm
        mmelt=tmunom(nplane)
        miro=irrunm(nplane)
        mirig=tirrm

        if (nirrm.ne.0) then
          write (31,1800) ncommm
c
          do 10 ipl = 1, nplane
c
            if (trunom(ipl).gt.0.0) then
c              irrunp = irrunm(ipl) / trunom(ipl) * 100000.
              irrunp = irrunm(ipl) / trunom(nplane) * 100000.
            else
              irrunp = 0.0
            end if
c
            write (31,1700) irrunp, ipl
   10     continue
          mrro=trunom(nplane)*(1-(irrunp/100))
c          trro=trro+mrro
c
        end if
c
      end if
c
      do 20 ipl = 1, nplane
        nrunom(ipl) = 0
        trunom(ipl) = 0.0
        irrunm(ipl) = 0.
        nmunom(ipl) = 0
        tmunom(ipl) = 0.0
   20 continue
c
      nrainm = 0
      trainm = 0.0
c
c     NOTE - comment out resetting of nirrm here - reset it only
c     in SR SEDOUT - so monthly irrigation impact on sediment
c     output will work properly.    dcf   4/27/93
c
c     nirrm(mon)=0
c
      tirrm = 0.
      ncommm = 0
c
      return
 1000 format (//'I.   RAINFALL, IRRIGATION, AND RUNOFF SUMMARY',/,5x,8(
     1    '-'),2x,10('-'),2x,3('-'),1x,6('-'),1x,7('-'))
 1100 format (//'I.   RAINFALL AND RUNOFF SUMMARY',/,5x,8('-'),1x,3('-'
     1    ),1x,6('-'),1x,7('-'))
 1200 format (/6x,'month and year: ',1x,a3,1x,i3)
 1300 format (/6x,
     1    ' Precipitation       Irrigation        Summer Runoff',
     1    '   Melt & Winter Runoff'/6x,
     1    'events   amount    events   amount    events   amount',
     1    '     events   amount',/6x,
     1    '          (mm)               (mm)               (mm) ',
     1    '               (mm)',/)
 1400 format (/6x,
     1    ' Precipitation      Summer Runoff      Melt & Winter',
     1    ' Runoff'/6x,
     1    'events   amount    events   amount    events   amount',/6x,
     1    '          (mm)               (mm)               (mm)'/)
 1500 format (7x,i3,5x,f8.2,3x,i2,4x,f8.2,3x,i3,5x,f8.2,3x,i3,5x,f8.2)
 1600 format (7x,i3,5x,f8.2,3x,i3,5x,f8.2,3x,i3,5x,f8.2)
 1700 format (6x,f5.1,' % of runoff from OFE ',i2,
     1    ' attributed to irrigation')
 1800 format (/8x,i3,' irrigation events occurred on days with rainfall'
     1    )
      end
