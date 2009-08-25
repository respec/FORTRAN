      subroutine case12(ichan,ielmt,cnpart,i,flagct)
c
c     + + + PURPOSE + + +
c
c     SR CASE12 routes sediment through a channel segment for cases
c     where deposition occurs at the upper end.
c
c     Two cases are possible:
c
c     Case I  : du < 0.0 (deposition) and dl < 0.0 (deposition)
c     Case II : du < 0.0 (deposition) and dl > 0.0 (detachment)
c
c     SR CASE12 contains the section of CREAMS SR ROUTE pertaining
c     to channel erosional Cases I and II.  Units are in ENGLISH.
c
c     Called from: SR CHNRT
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
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxcsg.inc'
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
      real expon, xrat
      integer k, nz
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Integer Variables
c
c     k  -
c     nz -
c
c     Real Variables
c
c     expon -
c     xrat  -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     detach
c     undflo
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c     If at least one particle size transport capacity, tc(i),
c     is less than the sediment load, gs(i), deposition has
c     occurred at the upper boundary of the channel segment,
c     i.e., du(i) < 0.0.  When this occurs these cases are
c     possible:
c
c     Case I : du < 0.0 (deposition) and dl < 0.0 (deposition)
c              Deposition occurs through the entire channel segment.
c
c     Case II: du < 0.0 (deposition) and dl > 0.0 (detachment)
c              Deposition occurs at the upper boundary of the
c              segment and stops somewhere within the segment.
c              Detachment of sediment occurs at the lower
c              boundary of the channel segment.
c
c     Sediment load, transport capacity, deposition rate, etc. at
c     the upper boundary of a given channel segment are equal to
c     respective variable values at the lower boundary of the
c     previous (upstream) channel segment.
c
c     first compute deposition rate of each particle class
c     at lower boundary of channel segment, dl()
c
      nz = 0
c
      do 10 k = 1, cnpart
c
        xrat = x(i-1) / x(i)
c
c       check value of expon here - move to qlat IF statement?
c
        expon = 1.0 + phi(k)
c
        if (qlat.gt.0.0) then
c
c         deposition rate for top and lateral sediment
c
          call undflo(xrat,expon)
c
c         Correction of the deposition rate (CB, nov.95)
c          dl(k) = (phi(k)/(qlat+phi(k))) * (dtcdx(k)-dlat(k)) * (1.0
          dl(k) = phi(k)/(1+phi(k)) * (dtcdx(k)-dlat(k)) * (1.0
     1      - xrat**expon)
        else
c
c         if no lateral inflow, the deposition rate is equal
c         to the change in transport capacity of the flow
c
          dl(k) = dtcdx(k)
        end if
c
        call undflo(xrat,expon)
c
        dl(k) = dl(k) + (du(k)*xrat**expon)
c
c       determine whether deposition is occurring at the lower
c       boundary (ie. dl < 0.0)
c
        if (dl(k).le.0.0) then
c
c         Case I: deposition occurs over the entire segment
c         so reduce sediment load at the lower boundary, gsl(),
c         by amount deposited
c
          nz = nz + 1
          xde(k) = x(i)
          dde(k) = dl(k)
c         Given the change of deposition rate, this has to be changed too.
c         (CB, nov. 95)
c          gsde(k) = tcl(k) - (dl(k)*ql/phi(k)) / wfl
          gsde(k) = tcl(k) - (dl(k)*x(i)/phi(k)) / wfl
          gstde(k) = gsde(k) * wfl
          gsl(k) = gsde(k)
        else
c
c         Case II: deposition at the upper end of segment and
c         possibility of detachment at the lower end
c
c         for each particle size if dl > 0 and du = 0 then
c
          if (du(k).eq.0.0) then
c
            xde(k) = x(i-1)
            gsde(k) = gsu(k)
            gstde(k) = gsde(k) * wfu
            dde(k) = 0.0
c
          else
c
c           deposition ends within segment at location xde(k)
c
            if (qlat.gt.0.0) then
c              And xde had to be changed too. The equation was rederived.
c              (CB, nov. 95)
c              xde(k) = x(i-1) * (1.0-((qlat+phi(k))/phi(k))*(du(k)/(
c     1            dtcdx(k)-dlat(k)))) ** (1.0/(1.0+phi(k)/qlat))
cd    abs is added by S. Dun Jun 01, 2005
              xde(k) = x(i-1) * abs(1.0 - (1+phi(k))/phi(k)*du(k)/(
     1            dtcdx(k)-dlat(k))) ** (1.0/(1.0+phi(k)))
            else
              xde(k) = x(i-1) * (1-du(k)/dtcdx(k))
            end if
c
            gstde(k) = dtcdx(k) * (xde(k)-x(i-1)) + tcu(k) * wfu
            dde(k) = 0.0
          end if
        end if
c
   10 continue
c
c     if all particles deposit at the end of segment (nz = cnpart)
c     then process next segment
c
      if (nz.eq.cnpart) return
c
c     if deposition of a given particle ends at a location xde not
c     equal to x(i) then examine if this location is greater than
c     or less than x(i)
c
c     xdemax = maximum xde for all particle sizes and begin with a
c     minimum possible value for xdemax which is x(i-1)
c
      xdemax = x(i-1)
c
      do 20 k = 1, cnpart
        xdemax = amax1(xdemax,xde(k))
   20 continue
c
c     if xdemax < x(i), then check the possibility of having
c     equilibrium or detachment from position xdemax to x(i)
c
      if (xdemax.ge.x(i)) then
c
c       otherwise, if xdemax >= x(i) then calculate gsl(k) and
c       process next segment
c
        do 30 k = 1, cnpart
c
          if (xde(k).ne.x(i)) then
            dl(k) = 0.0
            gsl(k) = (gstde(k)+dlat(k)*(x(i)-xde(k))) / wfl
          end if
c
   30   continue
c
c       process next segment
c
        return
c
      end if
c
c     set deposition rate at xde equal to zero (dde = 0.0) and
c     compute total sediment load at this point
c
      do 40 k = 1, cnpart
        dde(k) = 0.0
        gstde(k) = gstde(k) + dlat(k) * (xdemax-xde(k))
   40 continue
c
c     calculate detachment
c
      call detach(ichan,ielmt,cnpart,i,flagct)
c
      return
      end
