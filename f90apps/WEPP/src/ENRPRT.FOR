      subroutine enrprt(jun,iyear,nowcrp)
c
c******************************************************************
c                                                                 *
c     Called from subroutine sedout.                              *
c     Prints out the particle size distributions of the sediment  *
c     in the runoff and the enrichment ratios for all overland    *
c     flow elements in a profile.                                 *
c                                                                 *
c******************************************************************
c
c    argument declarations
c
      integer iyear, jun, nowcrp
c******************************************************************
c                                                                 *
c   Arguments                                                     *
c     jun    -                                                    *
c     iyear  -                                                    *
c                                                                 *
c******************************************************************
c
      include 'pmxpln.inc'
      include 'pmxcrp.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pmxprt.inc'
      include 'pmxhil.inc'
      include 'pmxelm.inc'
c
c******************************************************************
c                                                                 *
c   Common Blocks                                                 *
c                                                                 *
c******************************************************************
c
      include 'cavloss.inc'
c
      include 'ccliyr.inc'
c
      include 'ccntour.inc'
c
      include 'cenrpas.inc'
c
      include 'cpart.inc'
      include 'cstruc.inc'
c
      include 'cupdate.inc'
c
c*******************************************************************
c                                                                  *
c   Local Variables                                                *
c     mths(12)  : names of months of year                          *
c     diam(mxpart)  : diameter of particle class i in mm           *
c     fsand(mxpart) : percent of sand in particle class i          *
c     fsilt(mxpart) : percent of silt in particle class i          *
c     fclay(mxpart) : percent of clay in particle class i          *
c     forg(mxpart)  : percent of organic matter in class i         *
c     i         : counter variable used to indicate particle class *
c                                                                  *
c*******************************************************************
c
c XXX Use of the global SAVE does not follow the WEPP coding conventions
c     This needs to be fixed so that only the local variables which
c     need to have their values retained are saved.  dcf  5/18/94
      save
      real fsand(mxpart), fsilt(mxpart), fclay(mxpart), forg(mxpart),
     1    diam(mxpart)
      character*4 mths(12)
      integer i
c
      data mths /'jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug',
     1    'sep', 'oct', 'nov', 'dec'/
c
c     if(iroute.eq.0) then
c        write(jun,50)
c        return
c     endif
c
c
      do 10 i = 1, npart
        diam(i) = dia(i,nplane) * 1000.
        fsand(i) = frsnd(i,nplane) * 100.
        fsilt(i) = frslt(i,nplane) * 100.
        fclay(i) = frcly(i,nplane) * 100.
        forg(i) = frorg(i,nplane) * 100.
   10 continue
c
      if (ioutpt.eq.1.and.iyear.ne.1) then
        if (iroute.ne.0) then
c         JEF 4/4/91
c         if(cnfail(nplane).ne.0)then
c         if (contrs(nowcrp,nplane).eq.0) then
          if (contrs(nowcrp,nplane).ne.0) write(jun,1800)
          write (jun,1100)
          do 20 i = 1, npart
            write (jun,1300) i, diam(i), spg(i), fsand(i), fsilt(i),
     1          fclay(i), forg(i), frac(i,nplane), frcflw(i,nplane)
   20     continue
          write (jun,1200)
          write (jun,1400) mths(mon), day, year - ibyear + 1,
     1        enrato(nplane)
c         else
c           write (jun,1800)
c         end if
        else
          write (jun,1000)
        end if
      end if
c
      if (ioutpt.eq.2.and.iyear.ne.1) then
c       if (contrs(nowcrp,nplane).eq.0) then
        if (contrs(nowcrp,nplane).ne.0) write(jun,1800)
          write (jun,1100)
          do 30 i = 1, npart
            write (jun,1300) i, diam(i), spg(i), fsand(i), fsilt(i),
     1          fclay(i), forg(i), frac(i,nplane), frcmon(i)
   30     continue
          write (jun,1200)
          write (jun,1500) mths(mon), year - ibyear + 1, enrmon
c       else
c         write (jun,1800)
c       end if
      end if
c
      if (ioutpt.eq.3.and.iyear.ne.1) then
c       if (contrs(nowcrp,nplane).eq.0) then
        if (contrs(nowcrp,nplane).ne.0) write(jun,1800)
          write (jun,1100)
          do 40 i = 1, npart
            write (jun,1300) i, diam(i), spg(i), fsand(i), fsilt(i),
     1          fclay(i), forg(i), frac(i,nplane), frcyr(i)
   40     continue
          write (jun,1200)
          write (jun,1600) year - ibyear + 1, enryr
c       else
c         write (jun,1800)
c       end if
      end if
c
      if (iyear.eq.1) then
        write (jun,1100)
        do 50 i = 1, npart
          write (jun,1300) i, diam(i), spg(i), fsand(i), fsilt(i),
     1        fclay(i), forg(i), frac(i,nplane), frcavg(i)
   50   continue
        write (jun,1200)
        write (jun,1700) enravg
c     write(34,900) enravg
      end if
c
c********************************************************************
c                                                                   *
c     Format statements                                             *
c                                                                   *
c********************************************************************
c
c900  format(f6.2)
c
      return
 1000 format (/5x,'Final element not routed - no enrichment',
     1    ' information')
 1100 format (/5x,'Sediment particle information leaving profile',/,
     1    '-------------------------------------------------------',
     1    '------------------------',/,
     1    '                                 Particle Composition',
     1    '         Detached Fraction',/,'Class  Diameter  Specific  ---
     1------------------------------','  Sediment  In Flow',/,9x,
     1    '(mm)    Gravity   % Sand   % Silt   % Clay   % O.M.',
     1    '  Fraction  Exiting',/,
     1    '-------------------------------------------------------',
     1    '------------------------')
 1200 format ('---------------------------------------------------',
     1    '----------------------------'/)
 1300 format (1x,i2,4x,f6.3,6x,f4.2,4x,f5.1,4x,f5.1,4x,f5.1,4x,f5.1,5x,
     1    f5.3,4x,f5.3)
 1400 format (/5x,'SSA enrichment ratio leaving profile for ',a3,1x,i3,1
     1    x,i4,' = ',f6.2)
 1500 format (/5x,'Weighted SSA enrichment ratio leaving profile',
     1    ' for ',a3,1x,i4,' = ',f6.2)
 1600 format (/5x,'Weighted SSA enrichment ratio leaving profile',
     1    ' for year ',i4,' = ',f6.2)
 1700 format (/5x,'Average annual SSA enrichment ratio leaving',
     1    ' profile = ',f6.2)
 1800 format (//5x,'CONTOURING IS ACTIVE - NOT ALL/ANY SEDIMENT LOSS',
     1         /5x,'exits from bottom of profile.  PS Values reported',
     1         /5x,'here are for last OFE on profile.')
c1800 format (//5x,'CONTOURS ON LAST OVERLAND FLOW ELEMENT',/,5x,
c    1    ' thus no sediment reaches end of profile and',/,5x,
c    1    ' sediment fractions and enrichment ratio are zero here')
      end
