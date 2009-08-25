      subroutine detach(ichan,ielmt,cnpart,i,flagct)
c
c     + + + PURPOSE + + +
c
c     SR DETACH computes soil particle detachment and sediment load.
c
c     Called from: SRS CASE12, CASE34
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
      include 'pmxcsg.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer ichan, ielmt, cnpart, i, flagct
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     ichan  -
c     ielmt  -
c     cnpart -
c     i      -
c     flagct -
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchcas.inc'
      include 'cchero.inc'
      include 'cchvar.inc'
      include 'cgully.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real maxe, covsh, excess, excold, ratex, sumdf, sumexd, sumpld,
     1    sumtcl, temdep, temwer
      integer j, k, nexces, nt2,nt3
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     maxe   -
c     covsh  -
c     excess -
c     excold -
c     ratex  -
c     sumdf  -
c     sumexd -
c     sumpld -
c     sumtcl -
c     temdep -
c     temwer -
c
c     Integer Variables
c
c     j      -
c     k      -
c     nexces -
c     nt2    -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     dcap
c     trncap
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
      covsh = 1000.0
c
c     examine possibility of a detachment or equilibrium
c     condition between xdemax and x(i)
c
c     for detachment set maximum allowable erosion rate
c     without overfilling transport capacity (maxe)
c
      maxe = 1000.0
c
c     assume there is a potential for detachment: excess = 1.0
c
      excess = 1.0
c
c     save current values of depa and wera at temporary locations
c
      temdep = depa(ichan,i)
      temwer = wera(ichan,i)
c
      flagc = flagct
c
c     if erodible channel (flagc = 3) and depa (updated depth of
c     erodible layer at the upper boundary of lower end of current
c     segment is zero, compute detachment and erode (widening) channel
c     cross section (assuming a rectangular shape)
c
      if (flagc.eq.3.and.depa(ichan,i).eq.0.0) flagc = 2
c
cd        Added by S. Dun, 04/10/2008 to avoid crash
          if (sfl.lt.0.00001) sfl = 0.00001
cd        end adding 
      call dcap(1,flagc,ql,sfl,cc1,chz,effshl,depsid,temdep,temwer,wfl,
     1    chn,crsh,covsh,maxe,excess,tb,ielmt,df,ichan)
c
c     compute total detachment and potential load as a sum of sediment
c     load from detachment on the segment (dl) and initial potential
c     load
c
c
c     set flag nt3
c
      nt3 = 0
      nt2 = 0
c
      do 10 k = 1, cnpart
        dl(k) = df(k) * wfl
        potld(k) = (gstde(k)+dlat(k)*(x(i)-xdemax)+((dl(k)+dde(k))*(
     1      x(i)-xdemax)/2.0)) / wfl
        if(dl(k).eq.0.0.and.potld(k).eq.0.0) nt3 = nt3 + 1
   10 continue
c
c     if all detachment rates (dli's) and potential loads (potldi's)
c     are equal to 0 (nt3 = cnpart), set sediment load equal to the
c     potential load. Otherwise compute transport capacity.
c
      if(nt3.lt.cnpart) then
c
c       compute transport capacity based on new potential load
c
        call trncap(effshl,potld,ielmt,tcl)
c
c       set flag nt2 = 0
c
        nt2 = 0
c
c       check if tci's >= potldi's
c
        do 20 k = 1, cnpart
          if (tcl(k).ge.potld(k)) nt2 = nt2 + 1
   20   continue
c
      end if
c
c     if all tci's >= potldi's (nt2 = cnpart) set sediment load equal
c     to the potential load
c
      if (nt2.eq.cnpart.or.nt3.eq.cnpart) then
c
        do 30 k = 1, cnpart
          gsl(k) = potld(k)
   30   continue
c
      else
c
c       compute detachment rate that will just fill transport
c       capacity - initialize sum of tcl and potld
c
        sumtcl = 0.0
        sumpld = 0.0
c
        do 40 k = 1, cnpart
          sumtcl = sumtcl + tcl(k)
          sumpld = sumpld + potld(k)
   40   continue
c
c       compute the amount of detachment (exdet) that will
c       just fill the transport capacity
c
        do 50 k = 1, cnpart
          exdet(k) = ((tcl(k)*wfl-gstde(k)-dlat(k)*(x(i)-xdemax))*(2.0/(
     1        x(i)-xdemax))-dde(k)) / wfl
   50   continue
c
c       initialize sum of df and exdet
c
        sumdf = 0.0
        sumexd = 0.0
c
        do 60 k = 1, cnpart
          sumdf = sumdf + df(k)
          sumexd = sumexd + exdet(k)
   60   continue
c
c       set maximum allowable erosion rate that will not overfill the
c       sediment transport capacity (maxe)
c
        maxe = 1000.0
c
c       compute excess and save value in a temporary location (excold)
c
        excess = sumtcl / sumpld
        excold = excess
c
c       initialize counter for iterations (nexces)
c
        nexces = 0
c
        do 90 j = 1, 100
c
          nexces = nexces + 1
c
          if (nexces.le.20) then
c
            if (excess.lt.0.) excess = 0.0
c
c           save current values of depa and wera at temporary locations
c
            temdep = depa(ichan,i)
            temwer = wera(ichan,i)
c
cd          Added by S. Dun, 04/10/2008 to avoid crash
            if (sfl.lt.0.00001) sfl = 0.00001
cd          end adding 
            call dcap(2,flagc,ql,sfl,cc1,chz,effshl,depsid,temdep,
     1          temwer,wfl,chn,crsh,covsh,maxe,excess,tb,ielmt,df,ichan)
c
            excess = 1.0
c
            maxe = 1000.0
c
c           compute total detachment dl(k) and sediment load gsl(k)
c
            do 70 k = 1, cnpart
              dl(k) = df(k) * wfl
              gsl(k) = (gstde(k)+dlat(k)*(x(i)-xdemax)+((dl(k)+dde(k))*(
     1            x(i)-xdemax)/2.0)) / wfl
              potld(k) = gsl(k)
   70       continue
c
c           compute transport capacity based on sediment load gsl(k)
c
            call trncap(effshl,potld,ielmt,tcl)
c
c           initialize sums
c
            sumtcl = 0.0
            sumpld = 0.0
            sumdf = 0.0
            sumexd = 0.0
c
            do 80 k = 1, cnpart
              sumtcl = sumtcl + tcl(k)
              sumpld = sumpld + potld(k)
              exdet(k) = ((tcl(k)*wfl-gstde(k)-dlat(k)*(x(i)-xdemax))*(
     1            2.0/(x(i)-xdemax))-dde(k)) / wfl
              sumexd = sumexd + exdet(k)
              sumdf = sumdf + df(k)
   80       continue
c
            if (abs(sumtcl-sumpld)/sumtcl.lt.0.01) then
              go to 100
            else
              if(abs(sumdf).gt.0.00000001)then
                ratex = sumexd / sumdf
              else
                ratex = sumtcl / sumpld
              endif
              if (ratex.le.0.0) ratex = sumtcl / sumpld
              excess = excold * ratex
              excold = excess
            end if
c
          else
            go to 100
          end if
c
   90   continue
c
  100   continue
c
        do 110 k = 1, cnpart
          gsl(k) = tcl(k)
  110   continue
c
      end if
c
c     reset erodible channel variables
c
      depa(ichan,i) = temdep
      wera(ichan,i) = temwer
c
      flagc = flagct
c
      if (flagc.eq.3.and.depa(ichan,i).eq.0.0) flagc = 2
      if (flagc.eq.2.and.wera(ichan,i).gt.wfl) wida(ichan,i) =
     1    wera(ichan,i)
c
      return
      end
