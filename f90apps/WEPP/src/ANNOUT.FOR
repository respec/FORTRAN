      subroutine annout
c
c
c******************************************************************
c                                                                 *
c     Called from subroutine sedout.                              *
c     Generates annual summaries of soil loss information from    *
c     the model.                                                  *
c                                                                 *
c******************************************************************
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
      include 'cavloss.inc'
c     read ioutpt, ioutas
      include 'ccliyr.inc'
      include 'ciravlo.inc'
c
c******************************************************************
c                                                                 *
c   iravlo variables updated                                      *
c   irsolm(13)                                                    *
c                                                                 *
c******************************************************************
c
      include 'cstruc.inc'
      include 'csumirr.inc'
c
c******************************************************************
c                                                                 *
c   sumirr variables updated                                      *
c   nirry, nirrm(13), tirry, tirrm(13), ncommy, ncommm(13),       *
c   irruny, irrunm(13,mxplan)                                     *
c                                                                 *
c******************************************************************
c
      include 'csumout.inc'
c
c******************************************************************
c                                                                 *
c   sumout variables updated                                      *
c   nrainm(13), trainm(13), nrunom(13,mxplan), trunom(13,mxplan), *
c   nrainy, trainy, nrunoy(mxplan), trunoy(mxplan)                *
c                                                                 *
c******************************************************************
c
      include 'cupdate.inc'
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c     local variables
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      integer ipl
c
c
      if (nirry.eq.0) then
        write (31,1100)
        write (31,1200) year - ibyear + 1
        write (31,1400)
        write (31,1600) nrainy, trainy, nrunoy(nplane), trunoy(nplane),
     1     nmunoy(nplane),tmunoy(nplane)
c        if(ioutpt.eq.2.and.ioutas.eq.1)then
          yrain=trainy
          yrro=trunoy(nplane)
          ymelt=tmunoy(nplane)
c        endif
      else
c
        write (31,1000)
        write (31,1200) year - ibyear + 1
        write (31,1300)
        write (31,1500) nrainy, trainy, nirry, tirry, nrunoy(nplane),
     1      trunoy(nplane)
        write (31,1700) ncommy
c
c        if(ioutpt.eq.2.and.ioutas.eq.1)then
          yrain=trainy
          yrro=trunoy(nplane)
          ymelt=tmunoy(nplane)
          yiro=trunoy(nplane)
          yirig=tirry
c        endif
        do 10 ipl = 1, nplane
c
          if (trunoy(ipl).gt.0.0) then
            irruny(ipl) = irruny(ipl) / trunoy(ipl) * 100000.0
          else
            irruny(ipl) = 0.0
          end if
c
          write (31,1800) irruny(ipl), ipl
   10   continue
        irper=irruny(nplane)/100
c
      end if
c
c      do 30 i = 1, 12
c
        do 20 ipl = 1, nplane
          nrunom(ipl) = 0
          trunom(ipl) = 0.0
          irrunm(ipl) = 0.0
          nmunom(ipl) = 0
          tmunom(ipl) = 0.0
   20   continue
c
        nrainm = 0
        trainm = 0.0
        nirrm = 0
        tirrm = 0.0
        ncommm = 0
        irsolm = 0.0
c
c   30 continue
c
      do 40 ipl = 1, nplane
        nrunoy(ipl) = 0
        trunoy(ipl) = 0.0
        nmunoy(ipl) = 0
        tmunoy(ipl) = 0.0
        irruny(ipl) = 0.0
   40 continue
c
      nrainy = 0
      trainy = 0.0
      tirry = 0.0
c
ccccccccccccccccccccccccccc
c
      ncommy = 0
c
c******************************************************************
c     *
c     Format statements                                           *
c     *
c******************************************************************
c
      return
 1000 format (//'I.   RAINFALL, IRRIGATION, AND RUNOFF SUMMARY',/,5x,8(
     1    '-'),2x,10('-'),2x,3('-'),1x,6('-'),1x,7('-'))
 1100 format (//'I.   RAINFALL AND RUNOFF SUMMARY',/,5x,8('-'),1x,3('-'
     1    ),1x,6('-'),1x,7('-'))
 1200 format (/6x,'year: ',1x,i5)
 1300 format (/6x,'   Rainfall          Irrigation          Runoff'/6x,
     1    'events   amount    events   amount    events   amount'/6x,
     1    '          (mm)               (mm)               (mm)'/)
 1400 format
     1(/6x,'   Rainfall         Storm Runoff        Melt Runoff'/6x,
     1     'events   amount   events   amount     events  amount'/6x,
     1     '          (mm)              (mm)               (mm)'/)
 1500 format (2x,3(5x,i3,4x,f7.2))
 1600 format (2x,3(5x,i3,4x,f7.2))
 1700 format (/8x,i3,' irrigations occurred on days with rainfall')
 1800 format (6x,f5.1,' % of runoff from overland flow element ',i2,
     1    ' attributed to irrigation')
      end