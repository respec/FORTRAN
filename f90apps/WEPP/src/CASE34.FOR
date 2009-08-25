      subroutine case34(ichan,ielmt,cnpart,i,flagct)
c
c     + + + PURPOSE + + +
c
c     SR CASE34 routes sediment through a channel segment for cases
c     where detachment occurs at the upper end.
c
c     Two cases are possible:
c
c     Case III: du > 0.0 (detachment) and dl < 0.0 (deposition)
c     Case IV : du > 0.0 (detachment) and dl > 0.0 (detachment)
c
c     SR CASE34 contains the part of CREAMS SR ROUTE pertaining
c     to channel erosional Cases III and IV.  Units are in ENGLISH.
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
c
c     + + + LOCAL VARIABLES + + +
c
      real expon, xrat
      integer k, nk, nt, nz
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Integer Variables
c
c     k  -
c     nk -
c     nt -
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
c     enddet
c     trncap
c     undflo
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      nt = 0
c
      do 10 k = 1, cnpart
        if (tcl(k).le.potld(k)) nt = nt + 1
   10 continue
c
c     if all tci's < potldi's (nt = cnpart) then Case III, and
c     there is a potential for deposition to occur at the end of
c     segment
c
      if (nt.eq.cnpart) then
c
c       Case III: detachment at the upper boundary and potential for
c       deposition at the lower boundary
c
        nz = 0
        nk = 0
c
        do 20 k = 1, cnpart
          if (du(k).gt.0.0) nz = nz + 1
          if (gsu(k).eq.tcu(k)) nk = nk + 1
   20   continue
c
        if (nz.eq.cnpart.and.nk.eq.cnpart) then
c
          do 30 k = 1, cnpart
c
            if (tcl(k).lt.potld(k)) then
              xdbeg(k) = dx * du(k) / (2.0*dlat(k)+du(k)) + x(i-1)
            end if
c
   30     continue
c
        else
c
          do 40 k = 1, cnpart
c
            if (tcl(k).lt.potld(k)) then
              xdbeg(k) = ((tcu(k)*wfu-gstu(k))/(du(k)/2.0+dlat(k)-
     1            dtcdx(k))) + x(i-1)
            end if
c
   40     continue
c
        end if
c
        do 50 k = 1, cnpart
c
          if (potld(k).le.tcl(k)) then
            gsl(k) = potld(k)
            dl(k) = 0.0
          else
c
            xrat = xdbeg(k) / x(i)
c
            if (qlat.gt.0.0) then
c
c             deposition rate for top and lateral sediment
c
c             corrected by CB (nov. 95)
c             expon = phi(k) / qlat + 1.0
              expon = phi(k) + 1.0
              call undflo(xrat,expon)
              dl(k) = phi(k)/(1+phi(k)) * (dtcdx(k)-dlat(k)) * (1.0
     1            -xrat**expon)
            else
c
c             if no lateral inflow, the deposition rate is equal
c             to the change in transport capacity of the flow
c
              dl(k) = dtcdx(k)
            end if
c
c           corrected by CB (nov.95)
c           gsl(k) = tcl(k) - dl(k) * ql / (phi(k)*wfl)
            gsl(k) = tcl(k) - dl(k) * x(i) / (phi(k)*wfl)
          end if
c
   50   continue
c
      else
c
c       otherwise, Case IV: detachment over entire segment
c
c       compute potential load as a sum of sediment load from
c       detachment on the segment (du) and initial potential load
c
        do 60 k = 1, cnpart
          potld(k) = (gstu(k)+dlat(k)*dx+du(k)*dx/2.0) / wfl
   60   continue
c
c       compute transport capacity based on potld
c
        call trncap(effshl,potld,ielmt,tcl)
c
c       determine if detachment ends at the end of segment or within
c       segment and stays in equilibrium up to the end of segment
c
c       set flag nt = 0
c
        nt = 0
c
        do 70 k = 1, cnpart
          dtcdx(k) = (tcl(k)*wfl-tcu(k)*wfu) / dx
          if (tcl(k).le.potld(k)) nt = nt + 1
   70   continue
c
c       if all tcli's <= potldi's (nt = cnpart) go ahead and find
c       position within segment where detachment ends
c
        if (nt.lt.cnpart) then
c
c         otherwise, rearrange variables to allow use of previous section
c         of code dealing with a possibility of some particles detaching
c         and others staying in equilibrium up to the end of segment
c
          do 80 k = 1, cnpart
            dde(k) = du(k)
            xdemax = x(i-1)
            gsde(k) = gsu(k)
            gstde(k) = gsde(k) * wfu
   80     continue
c
c         determine the detachment
c
          call detach(ichan,ielmt,cnpart,i,flagct)
c
c         process next segment
c
          return
c
        end if
c
c       detachment of all the particles ends within the segment -
c       find the position where detachment ends
c
        call enddet(cnpart,ielmt,i)
c
        do 90 k = 1, cnpart
          dl(k) = 0.0
          gsl(k) = tcl(k)
   90   continue
c
      end if
c
      return
      end
