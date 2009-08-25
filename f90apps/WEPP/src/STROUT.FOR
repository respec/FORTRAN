      subroutine strout(elem,flag,npart,darea,ielmt,ichan,sdate,nelmt)
c
c     + + + PURPOSE + + +
c
c     SR STROUT prints the channel and impoundment erosion output
c     for each runoff event.
c
c     Called from: SR CHNERO
c     Author(s): Ascough II, R. van der Zweep, V. Lopes
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by: Jim Ascough II
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
      include 'pmxprt.inc'
      include 'pmxelm.inc'
      include 'pmxcsg.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      real darea
      integer elem, flag, npart, ielmt, ichan, sdate,nelmt
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     darea -
c     elem  -
c     flag  -
c     npart -
c     ielmt -
c     ichan -
c     sdate -
c     nelmt -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcon.inc'
      include 'cchflo.inc'
      include 'cchsed.inc'
      include 'coutchn.inc'
      
c
c     + + + LOCAL VARIABLES + + +
c
      real concpc, dacree, dacres, frac, ppm, solose, soloss, tcncpc,
     1    tconc, tgs(mxelem), tppm
      integer k
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     concpc -
c     dacree -
c     dacres -
c     frac   -
c     ppm    -
c     solose -
c     soloss -
c     tcncpc -
c     tconc  -
c     tgs    -
c     tppm   -
c
c     Integer Variables
c
c     k -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     enrcmp
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      tgs(ielmt) = 0.0
c
      do 10 k = 1, npart
        tgs(ielmt) = tgs(ielmt) + ggs(k,ielmt)
   10 continue
c
      if (flag.ge.4) then
c
c       write heading
c
        if (elem.eq.2) write (38,1000) ichan
c     if (elem.eq.3) write (38,1100)
      end if
c
      if (flag.gt.4) then
c
        if (elem.eq.3) then
c
c       impoundment output not in yet
c
        else if (elem.eq.2) then
c
c         write channel information
c
          if (cflags.eq.1) write (38,1200)
          if (cflags.eq.2.and.ye.le.ynor) write (38,1300)
          if (cflags.eq.2.and.ye.gt.ynor) write (38,1400)
c
          write (38,1500) qb, qe, ycr, ynor, ye, sfe
c
        end if
c
      end if
c
      if (tgs(ielmt).gt.2.205) then
c
        if (flag.lt.4) go to 30
c
        if (flag.ne.4) then
          write (38,1600)
          tconc = 0.0
          tcncpc = 0.0
          tppm = 0.0
c
          do 20 k = 1, npart
            concpc = conc(k,ielmt) / wtdh2o
            ppm = concpc * 1.0e+06
            frac = ggs(k,ielmt) / tgs(ielmt)
            write (38,1700) k, frac, ggs(k,ielmt), conc(k,ielmt),
     1          concpc, ppm
            tconc = tconc + conc(k,ielmt)
            tcncpc = tcncpc + concpc
            tppm = tppm + ppm
   20     continue
c
        end if
c
c       convert soil loss from lbs/ft^2 to tons/acre and kgs/ha
c       (darea is in m^2)
c
        dacres = darea * 0.0001
        dacree = darea * ((3.281**2)/43560.0)
c
        soloss = (tgs(ielmt)*0.4536) / dacres
        solose = (tgs(ielmt)/2000.0) / dacree
c
        if (flag.eq.4) go to 30
        write (38,1800) tgs(ielmt), tconc, tcncpc, tppm, soloss
c
c       compute channel sediment enrichment
c
        call enrcmp(2,ielmt,nelmt,tgs,elem)
        write (38,1900)
c
        return
c
      end if
c
      if (flag.lt.4) return
c
      write (38,2000)
      return
c
   30 continue
c
c     compute channel sediment enrichment
c
      call enrcmp(1,ielmt,nelmt,tgs,elem)
c
      if (flag.lt.4) return
c
      if (tgs(ielmt).gt.2.205) then
        write (38,2100) soloss, solose, enrich
      else
        write (38,2000)
      end if
c
      return
 1000 format (/28x,'results from channel ',i3,/,28x,23('-'))
c1100 format (/29x,'results from impoundment',/,29x,24('-'))
 1200 format (19x,'friction slope from energy gradline curves',/)
 1300 format (25x,'friction slope = channel slope',/)
 1400 format (18x,'friction slope = channel slope except at end',/)
 1500 format (19x,'peak discharge upper end',f8.3,' cfs',/,20x,
     1    'peak discharge lower end',f8.3,' cfs',/,20x,
     1    'critical depth          ',f8.3,' ft',/,20x,
     1    'normal depth            ',f8.3,' ft',/,20x,
     1    'control depth           ',f8.3,' ft',/,20x,
     1    'friction slope at end   ',f9.4)
 1600 format (/21x,'the quantity of eroded sediment in runoff',//,6x,
     1    'part.   frac. in   soil loss  ',
     1    'concentrations (soil/water)    ',/,6x,
     1    'type   sed. load      lbs.    ',
     1    '  lbsf/ft**3    lbsf/lbsf     ppm (wt)',/,6x,
     1    '----     -----      -------   ',
     1    '    ------        -----       --------')
 1700 format (6x,i2,6x,f5.2,4x,f9.0,4x,f9.4,4x,f9.4,4x,f10.0)
 1800 format (/10x,'total',9x,f9.0,4x,f9.4,4x,f9.4,4x,f10.0,//,19x,
     1    'average soil loss for area ',f6.2,' tons/acre')
 1900 format (//)
 2000 format (/19x,'*** total soil loss < 1 kg (2.205 lbs) ***')
 2100 format (24x,'annual soil loss for area',/,24x,'--> ',f10.2,
     1    ' kgs/ha (',f10.2,' tons/acre)',//,24x,'enrichment ratio  ',f7
     1    .3,/)
      end
