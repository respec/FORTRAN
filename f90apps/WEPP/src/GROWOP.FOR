      subroutine growop(imngmt,month,ityp,extinc,sumgd,alai,dm)
c
c     + + + PURPOSE + + +
c     Computes optimum biomass growth (dm) for a given day.
c
c     Called from YLDOPT and YOPTP.
c     Author(s): Arnold
c     Reference in User Guide:
c
c     Changes:
c           1) Substituted GDD for (TAVE - BTEMP(ITYP)) in EPIC
c              Equation 2.235.
c           2) Changed order of parameters to copnform to WEPP Coding
c              Convention.  (Swapped 1st & 5th (last).)
c
c     Version: This module recoded from Arnold's WEPP Post-version 92.25.
c     Date recoded: 07/09/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      real sumgd, alai, dm, extinc
      integer imngmt, month, ityp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     sumgd  - gdd accumulated thus far in the growing season
c     alai   - adjusted leaf area index
c     dm     - optimum above-ground biomass (kg/m^2)
c     extinc - radiation extinction coefficient for current crop
c     imngmt - current cropping system (annual or perennial)
c     month  - current month
c     ityp   - current crop
c
c     + + + COMMON BLOCKS + + +
      include 'ccrpgro.inc'
c       read: beinp(ntype), otemp(ntype)
c
      include 'ccrpprm.inc'
c -- XXX -- NOTE: DECLARATION in CCRPPRM.INC needs to be changed from
c                 "10" to a *PARAMETER* !! -- CRM -- 7/31/92
c       read: btemp(10)
c
      include 'ccrpvr3.inc'
c       read: xmxlai(ntype)
c     modify: gdd
c
      include 'cobclim.inc'
c       read: radave(12), obmint(12), obmaxt(12)
c
c     + + + END SPECIFICATIONS + + +
c
      real par, tave, temstr
      tave = (obmaxt(month)+obmint(month)) / 2.0
c
      if (tave.gt.btemp(ityp)) then
        gdd = tave - btemp(ityp)
        sumgd = sumgd + gdd
c
c       ------ Compute temperature stress parameter (temstr) from
c       ratio of growing degree days (gdd) to difference between
c       optimum temperature for plant growth (otemp) and base
c       daily air temperature (btemp).  TEMSTR is really the
c       *complement* of temperature stress; ie, 1 - temperature
c       stress.  By the way it is defined, TEMSTR must range
c       between zero and one.
c       (EPIC Equation 2.235)
c
c       Original Code:
c       temstr = sin(1.5708 * (tave - btemp(ityp)) / (
c       1           otemp(ityp) - btemp(ityp)))
        temstr = sin(1.5708*gdd/(otemp(ityp)-btemp(ityp)))
        if (temstr.lt.0.0) temstr = 0.0
c
c       ------ Compute photosynthetically active radiation (par) from
c       observed average monthly solar radiation (radave) and leaf
c       area index (?) (dlai).
c       (EPIC Equation 2.192 with modifications.  0.2092 is used to
c       convert the units from Langleys to MJ/m**2.  0.05 is added
c       to LAI to "get the equation started"; otherwise, the values
c       never seem to depart from the baseline.  CRM -- 6/25/92.)
c
        par = 0.02092 * radave(month) * (1.0-exp(-extinc*(alai+0.05)))
c
c       ------ Compute dry matter (dm) from inputted biomass conversion factor
c       (beinp) and PAR.
c       (EPIC Equation 2.193 with modifications.  Change in daylength
c       is ignored.  The right side is divided by 10 to implement a
c       change in units.  CRM -- 6/25/92.)
c
c       Original Code:
c       dm = dm + (beinp(ityp) * par / 10000.0) * temstr
        dm = dm + 0.0001 * beinp(ityp) * par * temstr
c
c       Annuals
        if (imngmt.eq.1) then
          alai = (xmxlai(ityp)*dm) / (dm+0.5512*exp(-6.8*dm))
c       Perennials
        else
          alai = (xmxlai(ityp)*dm) / (dm+0.2756*exp(-13.6*dm))
        end if
c
      end if
c
      return
      end
