      subroutine yalin(effsh,tottc)
c********************************************************************
c                                                                   *
c      This routine is called from FN TRCOEF to compute sediment    *
c      transport capacity using the Yalin equation. It is called    *
c      FN SHIELD.                                                   *
c                                                                   *
c********************************************************************
      real effsh
c                                                                   *
c      Arguments                                                    *
c         effsh - effective sheer stress                            *
c         tottc - total sediment transport capacity                 *
c                                                                   *
c********************************************************************
c
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxprt.inc'
      include 'pmxhil.inc'
      include 'pmxelm.inc'
c
c********************************************************************
c                                                                   *
c     Common Blocks                                                 *
c                                                                   *
c********************************************************************
c
      include 'cconsta.inc'
c
      include 'cpart.inc'
c
      include 'csolva1.inc'
c
      include 'cstruc.inc'
      include 'ctcfrac.inc'
c
c********************************************************************
c                                                                   *
c  tcfrac variables updated                                         *
c     tcf1(mxpart,mxplan)                                           *
c                                                                   *
c********************************************************************
c
c********************************************************************
c                                                                   *
c    local variables:                                               *
c       ws     : sediment transport capacity for particle calss k   *
c                (kg/m*s)                                           *
c       coef   : portion of yalin equation solution                 *
c       ycrit  : ordinate from shields diagram for dimensionless    *
c                critical shear for transport                       *
c       delta  : portion of yalin equation solution                 *
c       sigma  : portion of yalin equation solution                 *
c       p      : sediment transport capacity for particle class     *
c                k (nondimensional)                                 *
c       dltrat : portion of yalin equation solution                 *
c       tottc  : total sediment transport capacity (kg/m*s)         *
c                                                                   *
c********************************************************************
c
      save
      real ws(mxpart), coef(mxpart), ycrit(mxpart), delta(mxpart),
     1    sigma(mxpart), p(mxpart), dltrat(mxpart), tottc, reyn, shield,
     1    t, vstar, yalcon, oldtot, adjtc
      integer k
c
c Initialization:
c
      yalcon = 0.635
      t = 0.0
      tottc = 0.0
c
c     The constant 0.635 was derived empirically by Yalin
c
c     compute shear velocity (vstar):
c
      vstar = sqrt(effsh/msdens)
c
c     compute coefficient coef=vstar*msdens*dia*spg for each
c     particle classes:
c
      coef(npart) = vstar * msdens
      do 10 k = 1, npart
        coef(k) = coef(npart) * dia(k,iplane) * spg(k)
   10 continue
c
c     compute Reynold's number (reyn), dimensionless critical shear
c     parameter from the Shields diagram (ycrit), parameters delta and
c     sigma, and the dimensionless sediment transport capacity (p) for
c     each particle class:
c
      do 20 k = 1, npart
        reyn = vstar * dia(k,iplane) / kinvis
        ycrit(k) = shield(reyn)
        delta(k) = (vstar**2/(spg(k)-1.0)/accgav/dia(k,iplane)/
     1      ycrit(k)) - 1.0
c
        if (delta(k).gt.0.0) then
          sigma(k) = delta(k) * 2.45 * spg(k) ** (-0.4) *
     1        sqrt(ycrit(k))
          p(k) = yalcon * delta(k) * (1.0-1.0/sigma(k)*
     1        alog(1.0+sigma(k)))
          t = t + delta(k)
        else
          delta(k) = 0.0
          p(k) = 0.0
        end if
   20 continue
c
c
c
c     compute the transport capacity (mass per unit width per unit time)
c     ws for each particle class:
c
      if (t.eq.0.0) t = 1000.0
      do 30 k = 1, npart
        dltrat(k) = delta(k) / t
        ws(k) = p(k) * dltrat(k) * coef(k)
c
c RISSE 9/20/94 Add weighting scheme to transport capacity to account
c for the amount of each sediment class being transported
c NOTE: This will increase TC for clays and silts and decrease for sands
c
c       ws(k)=ws(k)*(frac(k,iplane)/0.2)
c Replaced above equation with following to take care of situation
c where other than 5 particle size classes have been used.
c dcf 11/18/94
c XXX Additional question on this is whether we want to use
c XXX "frac(k,iplane)"  OR  "frcflw(k,iplane)" for the weighting?????
c
        ws(k)=ws(k)*(frac(k,iplane)*float(npart))
c
        tottc = tottc + ws(k)
   30 continue
c
c
c XXX Add changes to include Nearing alteration to Tc that was
c XXX previously included in TCEND calculation in PARAM.FOR.  dcf
      oldtot = tottc
      if(sand(1,iplane).gt.0.5)then
        adjtc=0.3 + 0.7 * exp(-12.52*(sand(1,iplane)-0.5))
        if(adjtc.lt.0.30)adjtc=0.30
        tottc = tottc * adjtc
      endif
c
      do 35 k= 1,npart
        if(oldtot.gt.0.0) ws(k) = (ws(k)/oldtot)*tottc
   35 continue
c
      do 40 k = 1, npart
c
c       Lines added 10/16/89 to prevent divide by zero
c       if tottc is zero.
c
        if (tottc.gt.0.0) then
          tcf1(k,iplane) = ws(k) / tottc
        else
          tcf1(k,iplane) = 0.0
        end if
   40 continue
      return
      end
