      subroutine wshimp(elevm)
c
c     + + + PURPOSE + + +
c
c     SR WSHIMP converts WEPP variables to impoundment variables
c     and controls the hydraulic and sediment routing through the
c     impoundment.
c
c     Called from: SR WSHRUN
c     Author(s): Ascough II, C. Baffaut
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENTS DECLARATIONS + + +
      real elevm
c
c     + + + ARGUMENTS DEFINITIONS + + +
c     elevm - elevation of climate station in meters
c
c     + + + PARAMETERS + + +
c
      include 'pmxcsg.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmximp.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cchprt.inc'
      include 'cenrpa1.inc'
      include 'cflags.inc'
      include 'chydrol.inc'
      include 'cimday.inc'
      include 'cimpnd.inc'
      include 'coutchn.inc'
      include 'cpart3.inc'
      include 'cstore.inc'
      include 'cstruct.inc'
      include 'cwater.inc'
c
c     + + + LOCAL VARIABLES + + +
c
      real diam(5), durimp, frac(5), htw
      integer i
c
c     + + + LOCAL DEFINITIONS + + +
c
c     diam(5) -
c     durimp  -
c     frac(5) -
c     htw     -
c     i       -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     impeo
c     impmai
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      if (idflag.eq.0) then
c
c       no runoff volume - hydraulic routing only
c
        qiin(ipond) = 0.0
        vi(ipond) = 0.0
        ciin(ipond) = 0.0
c
      else
c
        qiin(ipond) = peakin(ielmt)
        vi(ipond) = runvol(ielmt)
c
c       total sediment concentration entering the impoundment
c       depends on the contribution from the hillslope and
c       channel elements
c
        ciin(ipond) = 0.0
c
        do 10 i = 1, cnpart
c
          ciin(ipond) = ciin(ipond) + ((tmpvol(nhtop(ielmt))*
     1        sedcon(i,nhtop(ielmt)))+(tmpvol(nhleft(ielmt))*
     1        sedcon(i,nhleft(ielmt)))+(tmpvol(nhrght(ielmt))*
     1        sedcon(i,nhrght(ielmt)))+(tmpvol(ncleft(ielmt))*
     1        sedcon(i,ncleft(ielmt)))+(tmpvol(ncrght(ielmt))*
     1        sedcon(i,ncrght(ielmt)))+(tmpvol(nctop(ielmt))*
     1        sedcon(i,nctop(ielmt))))
c
   10   continue
c
c       divide by the runoff volume (m^3) and convert to
c       mg/l for input to main impoundment routine
c
        ciin(ipond) = (ciin(ipond)/runvol(ielmt)) * 1000.0
c
c       calculate average fraction in each particle type
c       entering impoundment
c
c       impoundments can fed by either one hillslope or multiple
c       channels (up to three) but not by both
c
        if ((ncleft(ielmt).eq.0).and.(ncrght(ielmt).eq.0).and.(
     1      nctop(ielmt).eq.0)) then
c
c         no channels are feeding the impoundment - determine
c         which hillslope is contributing runoff to the
c         impoundment
c
          if (nhleft(ielmt).gt.0) then
            do 20 i = 1, cnpart
              frac(i) = frcflw(i,nhleft(ielmt))
              diam(i) = crdia(i,nhleft(ielmt)) * (1000.0/3.281)
   20       continue
c
          else if (nhrght(ielmt).gt.0) then
            do 30 i = 1, cnpart
              frac(i) = frcflw(i,nhrght(ielmt))
              diam(i) = crdia(i,nhrght(ielmt)) * (1000.0/3.281)
   30       continue
c
          else if (nhtop(ielmt).gt.0) then
            do 40 i = 1, cnpart
              frac(i) = frcflw(i,nhtop(ielmt))
              diam(i) = crdia(i,nhtop(ielmt)) * (1000.0/3.281)
   40       continue
          end if
c
        else
c
c         one or more channels feeding the impoundment
c
          if (ncleft(ielmt).gt.0) then
c
            do 50 i = 1, cnpart
              frac(i) = frcflw(i,ncleft(ielmt))
              diam(i) = crdia(i,ncleft(ielmt)) * (1000.0/3.281)
   50       continue
c
          else if (ncrght(ielmt).gt.0) then
c
            do 60 i = 1, cnpart
              frac(i) = frcflw(i,ncrght(ielmt))
              diam(i) = crdia(i,ncrght(ielmt)) * (1000.0/3.281)
   60       continue
c
          else if (nctop(ielmt).gt.0) then
c
            do 70 i = 1, cnpart
              frac(i) = frcflw(i,nctop(ielmt))
              diam(i) = crdia(i,nctop(ielmt)) * (1000.0/3.281)
   70       continue
c
          end if
        end if
c
c       average fraction in each particle type entering impoundment
c
        pcl(ipond) = frac(1)
        psl(ipond) = frac(2)
        psa(ipond) = frac(3)
        pla(ipond) = frac(4)
        psd(ipond) = frac(5)
c
c       average particle size diameter entering impoundment (mm)
c
        cl50 = diam(1)
        sl50 = diam(2)
        sa50 = diam(3)
        la50 = diam(4)
        sd50 = diam(5)
c
      end if
c
c     depth of tailwater set to zero - need to come up with
c     a subroutine or function to calculate tailwater depth
c     based on downstream hydraulic conditions
c
      htw = 0.0
c
c     eo is pet and is calculated by SR IMPEO using simplifications
c     of the equations used in SR EVAP (called by SR WATBAL)
c
      call impeo(elevm,eo)
      call impmai(htw,eo)
c
c     convert impoundment routine output variables back into
c     WEPP variables
c
      runvol(ielmt) = volo(ipond)
      tmpvol(ielmt) = runvol(ielmt)
      peakot(ielmt) = qomx(ipond) / 3.281 ** 3
c
c     update the event duration variable (watdur)
c
      if (runvol(ielmt).gt.0.0) then
        durimp = runvol(ielmt) / peakot(ielmt)
        watdur(ielmt) = amax1(watdur(ielmt),durimp)
      else
        watdur(ielmt) = 0.0
      end if
c
c     calculate the average runoff leaving the impoundment
c
      rofave(ielmt) = runvol(ielmt) / wsarea(ielmt)
c
c     save monthly, annual, and total runoff from the
c     impoundment element
c
      trunm(ielmt) = trunm(ielmt) + runvol(ielmt)
      truny(ielmt) = truny(ielmt) + runvol(ielmt)
      trunt(ielmt) = trunt(ielmt) + runvol(ielmt)
c
c     return if hydraulic routing only
c
      if (runvol(ielmt).lt.0.001) return
c
c     sediment concentration exiting the impoundment (kg/m^3)
c
      if (runvol(ielmt).gt.0.0) then
c
        sedcon(1,ielmt) = clout(ipond) / runvol(ielmt)
        sedcon(2,ielmt) = slout(ipond) / runvol(ielmt)
        sedcon(3,ielmt) = saout(ipond) / runvol(ielmt)
        sedcon(4,ielmt) = laout(ipond) / runvol(ielmt)
        sedcon(5,ielmt) = sdout(ipond) / runvol(ielmt)
c
c       if the final watershed element is an impoundment then sum
c       the number of runoff events for each month, year, and total
c       at the outlet
c
        if (ielmt.eq.nelmt) then
          nrunm = nrunm + 1
          nruny = nruny + 1
          nrunt = nrunt + 1
        end if
c
      end if
c
c     average fraction in each particle type exiting impoundment
c
      frcflw(1,ielmt) = clot
      frcflw(2,ielmt) = slot
      frcflw(3,ielmt) = saot
      frcflw(4,ielmt) = laot
      frcflw(5,ielmt) = sdot
c
c     average particle size diameter exiting impoundment (mm)
c
      crdia(1,ielmt) = cl50o * (3.281/1000.0)
      crdia(2,ielmt) = sl50o * (3.281/1000.0)
      crdia(3,ielmt) = sa50o * (3.281/1000.0)
      crdia(4,ielmt) = la50o * (3.281/1000.0)
      crdia(5,ielmt) = sd50o * (3.281/1000.0)
c
      return
      end
