      subroutine winthd(snodp,frdp,thdp,n,type)
c********************************************************************

c     Verified and tested by Reza Savabi, USDA-ARS, NSERL 317-494-5051
c                  August 1994
c                                                                   c
c                                                                   c
c********************************************************************
c                                                                   c
c    Arguments                                                      c
c          snodp -                                                  c
c          n     -                                                  c
c                                                                   c
c********************************************************************
c
      include 'pmxpln.inc'
      include 'pmxnsl.inc'
      include 'CFlgFS.inc'
      integer n, i, type
      real snodp(mxplan), frdp(mxplan), thdp(mxplan)
cccccccccccccccccccccccccccc
c                          c
c     WEPP version control c
c                          c
cccccccccccccccccccccccccccc
      include 'cver.inc'
c
      write (42,1000) ver, vermon, veryr
      if(type.eq.1)write (42,1100)
      if(type.eq.2)write (42,1200)
      do 10 i = 1, n
        write (42,1300) i, snodp(i)*1000., frdp(i)*1000., thdp(i)*1000.
   10 continue
      write (42,1400)
c     added flags output to see what winter settings are
c     jrf - 8/1/2008      
      if (wintRed.eq.1) then
         write(42,1450) 'On'
      else
         write(42,1450) 'Off'
      endif
      write (42,1460) finetop,10./fineTop
      write (42,1470) fineBot,20./fineBot
      
      if(type.eq.1)write (42,1500)
      if(type.eq.2)write (42,1600)
      return
 1000 format ('USDA WATER EROSION PREDICTION PROJECT : HILLSLOPE ',
     1    ' AND WATERSHED VERSION',f7.3,/,10x,a10,1x,i4,/)
 1100 format (10x,'HILLSLOPE WINTER OUTPUT FILE',//,
     1    ' Initial conditions:',/,3x,
     1    'OFE',1x,'snow',3x,'frost',2x,'thaw',2x,/,7x,
     1    'depth',2x,'depth',2x,
     1    'depth',/,8x,'(mm)',4x,'(mm)',3x,'(mm)')

 1200 format (10x,'WATERSHED WINTER OUTPUT FILE',//,
     1    ' Initial conditions:',/,1x,
     1    'chan ',1x,'snow',3x,'frost',2x,'thaw',/,7x,'depth',2x,
     1    'depth',2
     1    x,'depth',/,8x,'(mm)',4x,'(mm)',3x,'(mm)')
 1300 format (2x,i3,3(f7.1,1x))
 1400 format (/,2x,'Initial Snow Density Assumed = 100 kg/m**3')
 1450 format (/,2x,'Water Redistribution in frost: ',3a)
 1460 format (/,2x,'Freeze/thaw fine layers (top): ',i2,', thickness: ',
     1    f5.1, 'cm')
 1470 format (/,2x,'Freeze/thaw fine layers (remainder): ',i2,
     1    ', thickness: ',f5.1,'cm')
 1500 format(/,
     1'date hr year   snow   rain  ground falling melt  snow  ',
     1    'snow   frost  thaw  frost    residue cycle OFE',/,
     1    '               fall   fall  drift  drift  water  depth ',
     1    'density depth depth thickness  depth  #    #',/,
     1    '              (mm)    (mm)  (mm)   (mm)   (mm)   (mm) ',
     1    '(kg/m^3) (mm)  (mm)    (mm)     (mm)')
 1600 format(/,
     1'date hr year   snow   rain  ground falling melt  snow  ',
     1    'snow   frost  thaw  frost  cycle CHN',/,
     1    '               fall   fall  drift  drift  water  depth ',
     1    'density depth depth thickness  #    #',/,
     1    '              (mm)    (mm)  (mm)   (mm)   (mm)   (mm) ',
     1    '(kg/m^3) (mm)  (mm)  (mm)')
      end
