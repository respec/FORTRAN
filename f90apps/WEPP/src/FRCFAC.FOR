      subroutine frcfac(nowcrp)
c
c     + + + PURPOSE + + +
c    Computes the soil grain friction factor (frcsol(iplane)), the total
c    rill friction factor (frctrl(iplane)), and the equivalent weighting
c    friction factor for rills (frcteq(iplane)).
c
c     Generates the interrill and rill friction factors for the
c     erosion component; computes the time-variant roughness
c     coefficients for overland flow routing.
c
c     Called from IRS.
c     Author(s):
c     Reference in User Guide:
c
c     Changes:
c           1) Common blocks: CLIYR, CRPVR1, & CRPVR5 were not accessed.
c              Reference to them was deleted.
c           2) Since INRCOV & RILCOV are just read in INPUT, used
c              in this routine, and not used anywhere else,  they
c              are no longer set to zero, if their original values
c              are negative.
c           3) RROUGH is handled in a similar fashion, for the same
c              reason.
c           4) Calculations for RILARE(MXPLAN) simplified, and
c              RILARE(MXPLAN) changed to local variable RILLAR.
c     ******************************************************************
c     *  NOTE:  "RILARE" SHOULD BE REMOVED FROM COMMON BLOCK "FFACT".  *
c     ******************************************************************
c           5) SAVE statement, which saves ALL local variables, removed.
c           6) Removed use of variables FRSPRV and FRTPRV because
c              variables FRCSOL and FRCTRL have now been dimensioned
c              to MXPLAN, and we can use the previous plane's value of
c              these in SR PARAM directly.  Also removed them from
c              common /ffact/.  Computation of FRSPRV and FRTPRV in
c              WEPP version 93.2 was in error.    dcf  10/14/93
c
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/22/91 - 4/23/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + PARAMETERS + + +
      include 'pntype.inc'
      include 'pmxnsl.inc'
      include 'ptilty.inc'
      include 'pmxpln.inc'
      include 'pmxcrp.inc'
      include 'pmxelm.inc'
      include 'pmxres.inc'
      include 'pmxtls.inc'
      include 'pmxtil.inc'
      include 'pmxhil.inc'
      include 'pmxgrz.inc'
c
c     + + + COMMON BLOCKS + + +
      include 'cffact.inc'
c     modify: frcsol(iplane), frctrl(iplane)
c      write: frcteq(iplane)
c
      include 'ccontcv.inc'
c       read: tilseq(mxcrop,mxplan)
c
      include 'ccrpvr2.inc'
c       read: ar(ntype),flivmx(nowcrp(iplane))
c
      include 'ccrpvr3.inc'
c       read: hmax(ntype)
c
      include 'ccover.inc'
c       read: lanuse(mxplan), canhgt(mxplan), inrcov(mxplan),
c       rilcov(mxplan)
c
      include 'ccrpout.inc'
c       read: rescov(mxplan), rrc(mxplan)
c
      include 'ccrpprm.inc'
c       read: itype, itill
c
      include 'cends.inc'
c       read: rspace(mxplan), width(mxplan)
c
      include 'cfripas.inc'
c
      include 'crinpt1.inc'
c       read: wcf(mxplan)
c
c     include 'crinpt1a.inc'
c
      include 'crinpt6.inc'
c       read: rufcov(mxplan), rrough(mxplan)
c
      include 'cslinit.inc'
c       read: rrinit(mxplan)
c
      include 'csolvar.inc'
c       read: sand, clay
c
      include 'cstruc.inc'
c       read: iplane,imodel
c
      include 'ctillge.inc'
c       read: rro
c
      include 'cupdate.inc'
c       read: indxy(mxplan)
c
c     + + + LOCAL VARIABLES + + +
c     real inrfso, inrfo, inrrou, inrfro, inrfco, inrfto, rroinr,
c    1    rrrinr, frrraf, frrrok, frirok, rillar, frires
      real inrfso, inrfo, inrrou, inrfco, rroinr, rrrinr
      integer i1, i2, idplnt
c
c     + + + LOCAL DEFINITIONS + + +
c     inrfso - friction due to soil texture
c     inrfo  - Darcy friction coefficient for interrill surface
c     inrrou -
c     inrfro - friction due to form roughness
c     inrfco - Darcy friction coefficient for interrill surface
c              area
c     inrfto - total Darcy friction factor for the interrill area
c     rroinr - initial random roughness of a freshly tilled
c              surface
c     rrrinr -
c     frccov(iplane) - Darcy friction coefficient for rill surface cover
c     frican(iplane) - friction due to canopy cover
c     flivmx -
c     frlive(iplane) -
c     frrraf - friction due to form roughness
c     frrrok - friction (rill) due to rock and gravel
c     frirok - friction (interrill) due to rock and gravel
c     frrres(iplane) - friction (rill) due to residue
c     frires(iplane) - friction (interrill) due to residue
c     fribas(iplane) - friction due to basal cover and cryptogram
c     fritot - sum of fribas(iplane) and frican under rangeland
c     rillar - rill area
c     i1     -
c     i2     -
c     idplnt -
c
c     + + + END SPECIFICATIONS + + +
c
c      *** L0 IF ***
      if (lanuse(iplane).eq.1) then
c
c       hydraulic roughness for interrill areas
c
        inrfro = 0.0
c
c       friction coefficient for smooth bare soil on an
c       interrill area
c
        inrfso = 4.07
c
c       For the case of a continuous simulation
c
        if(imodel.eq.1)then
          i1 = indxy(iplane)
          i2 = tilseq(nowcrp,iplane)
          if (i2.gt.0) then
            if (indxy(iplane).eq.0)then
              rroinr = rrinit(iplane)
            elseif(resman(i1,i2).eq.10 .or. resman(i1,i2).eq.11) then
              rroinr = rrinit(iplane)
            else
              rroinr = rro(i1,i2)
            end if
          else
            rroinr = rrinit(iplane)
          end if
c
c       ELSE for the case of a single storm simulation
c
        else
          rroinr=rrinit(iplane)
        end if
        rrrinr = rrc(iplane) / rroinr
        if (rrrinr.gt.1.0) rrrinr = 1.0
        inrfo = exp(3.024-5.042*exp(-161.*rroinr))
c
c         friction coefficient for interrill surface roughness
c
        inrrou = 0.5 * (inrfo**1.128) * exp(-3.088*(1.0-rrrinr))
        if (inrrou.lt.inrfso) inrrou = inrfso
        inrfro = inrrou - inrfso
c
        if (inrcov(iplane).gt.0.0) then
c
c         friction coefficient for interrill surface cover
c
          inrfco = 14.5 * (inrcov(iplane)) ** 1.5544
        else
          inrcov(iplane) = 0.0
          inrfco = 0.0
        end if
c
c       New section added  5/16/90 by dcf to account for roughness
c       introduced by living grasses, small grains, and alfalfa
c
        idplnt = itype(nowcrp,iplane)
c
        if (hmax(idplnt).gt.0.0) then
          frlive(iplane) = (canhgt(iplane)/hmax(idplnt)) *
     1        flivmx(idplnt)
        else
          frlive(iplane) = 0.0
        end if
c
c       total friction factor for interrill area
c
        inrfto = inrfro + inrfco + inrfso + frlive(iplane)
c
c       hydraulic roughness for rills
c
        frcsol(iplane) = 1.11
c
c       Gilley's new equation - 5/16/90
c       Changed by Nearing - 12/94
c
        if (rilcov(iplane).gt.0.0) then
          frccov(iplane) = 4.5 * (rilcov(iplane)) ** 1.5544
        else
          rilcov(iplane) = 0.0
          frccov(iplane) = 0.0
        end if
c
c------- total rill friction
        frctrl(iplane) = frccov(iplane) + frlive(iplane) +
     1      frcsol(iplane)
c
c
c     *** L0 ELSEIF ***
      else if (lanuse(iplane).eq.2) then
c
c-------rill friction calculations
c-------friction due to soil texture
c
c       frcsol(iplane)=(3.42**clay(1,iplane)/12.42**sand(1,iplane))**0.5
c
c       Use Gilley's value for bare soil frcsol(iplane) instead of present
c       range equation.
c
        frcsol(iplane) = 1.11
        if (rrough(iplane).gt.0.006) then
          frrraf = 42.76 * (1.0-exp(-77.3*rrough(iplane)))
        else
          frrraf = 42.76 * (1.0-exp(-77.3*0.006))
        end if
c
c       Changes from Mary Kidwell/Mark Weltz follow:
c       frrrok = 1.8467 * wcf(iplane)
c       frrres(iplane) = 113.73 * rescov(iplane) ** 3.0
c
        frrrok = 1.8467 * frokr(iplane) * rokcov(iplane)
        frrres(iplane) = 113.73 * (fresr(iplane)*rescov(iplane)) ** 3.0
        frrbas = 125.91 * (fbasr(iplane)*bascov(iplane)
     1           + fcryr(iplane)*crycov(iplane)) ** 0.8
        frrbas = frrbas/2.0

c       frccov(iplane) = frrres(iplane) + frrrok + frrraf
        frccov(iplane) = frrrok + frrres(iplane) + frrbas + frrraf
c
c-------total rill friction
c       frctrl(iplane) = (frcsol(iplane)+frrraf+frrrok+frrres(iplane)-
c    1      15.868)
c
        frctrl(iplane) = (frcsol(iplane)+frrraf+frrrok+frrres(iplane)+
     1                    frrbas - 15.868)
        if (frctrl(iplane).lt.frcsol(iplane)) frctrl(iplane) =
     1      frcsol(iplane)
c
c-------interrill friction calculations
c
c       inrfso=(3.42**clay(1,iplane)/12.42**sand(1,iplane))**0.5
c
c       Use Gilley's value for bare soil inrfso instead of present
c       range equation.
c
        inrfso = 4.07
c
        if (rrough(iplane).gt.0.006) then
          inrfro = 42.76 * (1.0-exp(-77.3*rrough(iplane)))
        else
          inrfro = 42.76 * (1.0-exp(-77.3*0.006))
        end if
c
c       XXX  Changes from Mary Kidwell and Mark Weltz  3/9/95
c       frirok = 1.8467 * wcf(iplane)
c       frires = 113.73 * rescov(iplane) ** 3.0
c       fribas(iplane) = 125.91 * rufcov(iplane) ** 0.8
        frirok = 1.8467 * froki(iplane)*rokcov(iplane)
        frires = 113.73 * (fresi(iplane)*rescov(iplane)) ** 3.0
        fribas(iplane) = 125.91 * (fbasi(iplane)*bascov(iplane) +
     1                   fcryi(iplane)*crycov(iplane)) ** 0.8
        frican(iplane) = 38.95 * cancov(iplane) ** 0.8
        inrfco = fribas(iplane) + frires + frirok + frican(iplane)
        inrfto = (inrfso+inrfro+frirok+frires+fribas(iplane)+
     1      frican(iplane)-15.868) / 0.64924
        if (inrfto.lt.inrfso) inrfto = inrfso
c
c     *** L0 ENDIF ***
      end if
c
c------compute rill area (RILLAR):
      rillar = width(iplane) / rspace(iplane)
c
c
c     Add the following IF/ELSE so that for the special case where
c     the user has specified that the rill width is equal to the
c     rill spacing (broad sheet flow assumption), then the model will
c     use the interrill friction factors only.    dcf   8/22/94
c
      if(rillar .lt. 1.0) then
c       equivalent total fric factor for composite area
        frcteq(iplane) = inrfto + rillar * (frctrl(iplane)-inrfto)
      else
        frctrl(iplane) = inrfto
        frcsol(iplane) = inrfso
        frcteq(iplane) = inrfto
      endif
c
      return
      end
