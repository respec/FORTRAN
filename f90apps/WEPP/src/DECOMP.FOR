      subroutine decomp(nowcrp)
c
c     + + + PURPOSE + + +
c
c           XXX-NOTE:  The computation of POP looks like a real problem that
c           needs to be corrected!  The approach seems unsound,
c           and the location of this code in this subroutine seems
c           inappropriate.  (See note at end of DECOMP.)
c           -- CRM -- 12/01/92.
c
c     Estimates changes in residue and root masses, computes remaining
c     flat, standing, and buried residue, and root masses.  Computes
c     residue cover (COVCAL), and makes adjustments for tillage oper-
c     ations.  Also estimates mass and cover for flat and standing
c     residues.  Predicts buried residue mass and non-living root mass.
c     Considers up to three successive types of fallen and buried
c     residues: only one type of standing residue.
c
c
c  ADDENDUM:  Default parameter for wheat    ==> ORATEA = 0.0110
c                                   corn     ==>        = 0.0100
c                                   soybeans ==>        = 0.0200
c             For now, ORATER should continue to be set equal to
c             ORATEA, unless data determines otherwise.
c             -- D.E. Stott 9/2/92
c
c  ********************************************************************
c  *  NOTE: In the User's Documentation equation 8.5.3c is computed   *
c  *        on a weight basis.  OPTWAT (used in that equation) is     *
c  *        also given on a weight basis.  However, the computations  *
c  *        in equation 8.5.3c are done on a volume basis.  This      *
c  *        needs to be corrected. -- CRM -- 11/13/92.                *
c  ********************************************************************
c
c           XXX-NOTE:  Why are CRPPRM and PEREN separate common blocks?  They 
c           seem to contain the same sorts of variables -- some of which are
c           appropriate to BOTH perenials AND annuals -- variables like
c           JDPLT & JDHARV are in CRPPRM, whereas JDCUT, JDBURN, etc. are
c           in PEREN. -- CRM -- 2/19/92
c
c     Called from WATBAL (If the land use is cropland)
c     Author(s): Alberts, Stott, Ferris, Meyer
c     Reference in User Guide: Chapter 8
c
c     Changes:
c          1) Local variable NRES deleted, since it was always set to
c             '4'.
c          2) Parameter TILTYP not used.  Include file PTILTY.INC
c             dereferenced.
c          3) The variables in include files CCRPVR5.INC, CCOVER.INC,
c             CCRPOUT.INC, and CSOLVAR.INC were not used.  They were
c             dereferenced.
c          4) The variables in the SAVE statement below never have an
c             opportunity to change value after the DATA initialization.
c             The statement was deleted.
c                save optwat,tfc,opttmp,varind,ferind,pszind
c          5) Deleted second calculation of ENV  in "do 999" loop,
c             since it was identical to the first.
c          6) Deleted use of NOWRES as a local variable with a con-
c             stant value of '1'.
c          7) Created following local variables to avoid repeated recal-
c             culation of:
c               TMPVR1:
c                     oratea(iresd(nowres,iplane))*varind*ferind*pszind
c               TMPVR2:
c                     orater(iresd(nowres,iplane))*varind*ferind*pszind
c               TMPVR3:
c                     1.0 - fbrnog(4,iplane)
c                     (It was being computed 3 times for each trip
c                      through the "do 15" loop.)
c               TMPVR4:
c                     rmagy-rmagt(iplane)
c               TMPVR5:
c                     1.0 - frmove(4,iplane)
c          8) TMPVR* calculations taken outside their respective
c             loops.
c          9) VARIND was deleted at request of D.E. Stott.
c             CRM -- 3/27/92
c         10) Calculation of RMAGT was determined to be incorrect.
c             Calculations of SENVIN, FENVIN, & BENVIN were changed
c             from cumulative to daily to correct for this, as per
c             instructions from D.E. Stott.  -- CRM -- 3/27/92
c         11) Changed the "residue management" IF's to ELSE-IF's.
c         12) Since RAIN is expressed in *meters* (not millimeters),
c             Equation 8.3.5a was incorrect by a factor of 1000.
c             This was corrected by D.E. Stott -- 9/2/92.
c             Changed from:
c                 if(rain.lt.4.0) then
c                   swatfc = rain/4.0
c             to:
c                 if(rain.lt.0.004) then
c                   swatfc = rain/0.004
c               CRM -- 10/30/92
c         13) Since SOILW is the volume of soil water (meters) in the
c             layer, and Equation 8.3.5c uses percent saturation by
c             volume; ie, soil water (meters) / layer thickness
c             (meters), Equation 8.3.5c was incorrect.  This correct-
c             ed by D.E. Stott -- 9/2/92.
c             Changed by D. Stott from:
c                 if(soilw(1,iplane).lt.optwat) then
c                   fwatfc = soilw(1,iplane)/optwat
c             to:
c                 if((soilw(1,iplane)/dg(1,iplane)).lt.optwat) then
c                   fwatfc = (soilw(1,iplane)/dg(1,iplane))/optwat
c               CRM -- 10/30/92
c -- XXX -- Or WAS it fully corrected?  The note with the equation
c           indicates it needs percent by WEIGHT.  -- CRM -- 11/06/92.
c         14) Equation to calculate root decomposition was *supposed* to
c             use ORATE*R*.  It was being calculated using ORATE*A*.
c             this was corrected by D.E. Stott -- 9/2/92.
c         15) OPTWAT, TFC, & OPTTMP were changed from 0.35, 6.1, & 30.0
c             to 0.30, 6.4, & 33.0 respectively. -- CRM -- 11/09/92.
c         16) The following line was added to the "Flat residue removed"
c             section:
c                 rilrm(nowres,iplane)=rilrm(nowres,iplane)*tmpvr5
c             and the following lines were removed:
c                 if((tilseq(nowcrp,iplane).gt.0) .and.
c            1       (iridge(tilseq(nowcrp,iplane)).ne.1))
c            2         rilrm(nowres,iplane)=rigrm(nowres,iplane)
c         17) Since residue removal only deals the FLAT residue (not
c             standing), the following line was deleted from the residue
c             removal management section:
c                 rmagt(iplane)=rmagt(iplane)*tmpvr5
c         18) While vainly attempting to convert cover calculations
c             to weight calculations, I noticed we had residue falling
c             UP the furrow!  Added the following statement to correct
c             the problem. -- CRM -- 11/30/92
c                 if(deltrm .lt. 0.0) deltrm = 0.0
c         19) For ridge-till systems, residue movement is supposed to
c             be simulated for 60 days after harvest (not just 59).
c             the following line:
c                 elseif(dah.gt.0 .and. dah.lt.60)then
c             changed to:
c                 elseif(dah.gt.0 .and. dah.le.60)then
c         20) Updated the calculation of residue flat on the ground
c             today (RMOGT) to reflect actual interrill/rill fractions
c             rather than assuming a 50:50 split as per Livingston.
c         21) References to JDBURN(4,iplane), JDCUT(4,iplane), and
c             JDMOVE(4,iplane) were changed to JDBURN(NOWCRP,iplane),
c             JDCUT(NOWCRP,iplane), and JDMOVE(NOWCRP,iplane), since
c             these are the values actually acquired in RESUP.
c         22) The line:
c                 if(tilseq(nowcrp,iplane).gt.0) then
c             was added back to the M3 & N1 loops to check for
c             positive subscripts on IRIDGE. -- CRM -- 12/10/92.
c         23) The variable WGHT1 was added at the beginning of the
c             residue management/shifting section to handle S. Living-
c             ston's new calculation of RMOGT. -- CRM -- 12/10/92.
c         24) The lines:
c                rmogt(nowres,iplane)=wght1*rilrm(nowres,iplane) +
c               1                     (1.0-wght1)*rigrm(nowres,iplane)
c             were changed to:
c                rmogt(1,iplane)=wght1*rilrm(1,iplane) +
c               1                     (1.0-wght1)*rigrm(1,iplane)
c             This was to correct an oversight.  NOWRES was not defined.
c             CRM -- 12/10/92.
c         25) The lines:
c                rmogt(1,iplane)=wght1*rilrm(1,iplane) +
c               1                     (1.0-wght1)*rigrm(1,iplane)
c             were changed to:                      ^
c                rmogt(1,iplane)=wght1*rilrm(1,iplane) +
c               1                     (1.0-wght1)*rilrm(1,iplane)
c             CRM -- 12/10/92.
c         26) The lines:
c                rmogt(1,iplane)=wght1*rilrm(1,iplane) +
c               1                     (1.0-wght1)*rigrm(1,iplane)
c             were incorrect (D. Flanagan) They were changed to:
c                rmogt(1,iplane)=wght1*riGrm(1,iplane) +
c               1                     (1.0-wght1)*riLrm(1,iplane)
c             CRM -- 12/31/92.
c         27) Moved call to DECOMP from within SR WATBAL to SR CONTIN,
c             because we need to perform residue management and tillage
c             and update residue cover BEFORE we compute the
c             infiltration parameters for today.  Some questions remain
c             as to how this move will impact the actual decomposition
c             itself (will be using yesterday's soil moisture in soil
c             layer 1, with today's rainfall and temperature info).
c             Best solution would be to split DECOMP into two parts -
c             the first which has only decomposition and is called by
c             SR WATBAL at the end of the day, and the second which
c             would have the residue management code and would be
c             called by SR CONTIN before the current day's infiltration
c             and erodibility parameter adjustments have been made.
c             DCF -- 2/3/93
c         28) Changed 'optwat' from a constant to a percentage of the
c             soil porosity in the top soil layer, 'avpor' This allows
c             the optimum water content to be set equal to 60% of the
c             total porosity filled with water. This is an extablished
c             optimum for soil microbial activity.  This also allows
c             'optwat' to be dynamic, allowing for changes in bulk
c             density. 'avpor' is called from ccons.inc.
c                         optwat = avpor(iplane)*0.60
c             DES -- 4/3/93
c         29) Streamlined the calculation of the tmpfac.  See comments
c             and equations for tmpfac below.  DES -- 4/3/93
c         30) Deleted the M3 IF-ELSE and altered the code so that all
c             systems (ridged or non-ridged) use the same computations
c             for tillage-induced effects on surface residue.  This will
c             allow easy insertion of separate MFO values for interrill
c             and rill areas, as requested by SCS.   dcf  8/25/93
c         31) variable initialization in data statements moved to
c             code body  jca2  8/31/93
c         32) Altered the equation for calculating the watfac for
c             wet conditions to allow for the impact of oxygen
c             depletion.  For water contents (%WFP) > 60%
c
c              fwatfc = optwat/(soilw(1,iplane)/dg(1,iplane))
c
c             DES -- 1/30/94
c         33) Added code to use different MFO (tillage intensity)
c             values for interrill and rill areas.  dcf  2/3/94
c         34) Changed from using variable RAIN to variable PRCP
c             and now also use TAVE in calculation of SWATFC,
c             because new winter routines (contin.for) no longer
c             set RAIN to zero when TAVE is less than zero.
c             Perhaps we should use TMAX instead of TAVE??? dcf 6/3/94
c         37) Changed the calculation of the water function to
c             use soil water information for top 2 soil layers since
c             comparison was being made with AVPOR which is computed
c             based on these top 2 layers.  dcf  5/4/95
c         36) Changed limits for temperature function.
c             FROM  if (tave.gt.-1.5.and.tave.lt.44.9) then
c               TO  if (tave.gt.-6.1.and.tave.lt.49.2) then
c             based on newest DECOMP documentation and check
c             with Stott.   dcf  5/5/95
c
c
c     Version: This module recoded from WEPP version 91.50.  Later
c              updated to version 92.25.
c
c     Date recoded: 12/26/91 - 02/19/92.
c              and: 05/12/92 - 05/12/92.
c              and: 10/30/92 - 12/01/92.
c     Recoded by: Charles R. Meyer.
c     Changes dated in April 1993 made to WEPP version 93.0 (DES)
c     Changes dated in January 1994 made to WEPP version 93.13 (DES)
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxcut.inc'
      include 'pmxelm.inc'
      include 'pmxhil.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer nowcrp
c
c     + + + ARGUMENT DEFINITIONS + + +
c     nowcrp - index of the current crop
c
c     + + + COMMON BLOCKS + + +
      include 'ccons.inc'
c       read: avpor(mxplan)
c
      include 'ccrpvr1.inc'
c       read: mfo(mxtill,mxtlsq,mxcrop),rmfo(mxtill,mxtlsq,mxcrop)
c     modify: rmogt(mxres,mxplan),  rmagt(mxplan), smrm(mxres,mxplan),
c             rtm(mxres,mxplan).
c
      include 'ccrpvr2.inc'
c       read: cn(ntype), aca(ntype), cf(ntype), ar(ntype)
c
      include 'cclim.inc'
c       read: tave
c
      include 'ccontcv.inc'
c       read: tilseq(mxcrop,mxplan)
c
      include 'ccrpprm.inc'
c       read: jdplt(mxcrop,mxplan), jdharv(mxcrop,mxplan),
c             iresd(mxres,mxplan), iroot(mxres,mxplan)
c
      include 'cdecvar.inc'
c       read: oratea(ntype)
c     modify: senvin(mxplan), fenvin(mxres,mxplan), benvin(mxres,mxplan)
      include 'cerrid.inc'
c       read: crpnam(iplane)
c
      include 'chydrol.inc'
c
      include 'cnew.inc'
c       read: manver
c       read: prcp
      include 'csenes.inc'
c       modify csenes(iplane) if residue addition
c
      include 'cperen.inc'
c     modify: pop(mxplan)
c       read: popmat(mxplan), jdburn(mxcrop,mxplan),
c             jdcut(mxcrop,mxplan), jdmove(mxcrop,mxplan),
c             fbrnog(mxcrop,mxplan), fbrnag(mxcrop,mxplan),
c             frmove(mxcrop,mxplan), srmhav(mxplan), fact(ntype)
c
      include 'cridge.inc'
c       read: iridge(mxtlsq)
c     modify: rilrm(mxplan),rigrm(mxres,mxplan)
c
      include 'cstruc.inc'
c       read: iplane
      include 'ctillge.inc'
c       read: resman
c
      include 'cupdate.inc'
c       read: sdate, mdate(mxtill,mxtlsq)
c     modify: indxy(mxplan)
c
      include 'cwater.inc'
c       read: soilw(mxnsl,mxplan)
c
      include 'cends4.inc'
c       read: rspace(mxplan), width(mxplan)
c
c     + + + LOCAL VARIABLES + + +
      real flrcov, rmogy(mxres), optwat, atemp,
     1    t1, t2, tmpfac, swatfc, fwatfc, ferind, pszind,
     1    tmpvr1, tmpvr2, tmpvr3, tmpvr4, tmpvr5, wght1,
     1    env, rmagy, envinx,sumwat
      integer i1, i2, numres, i, nowres
c
c     + + + LOCAL DEFINITIONS + + +
c     flrcov - surface cover provided by flat residues
c     rcinit - initial residue cover on ridges at harvest
c     rigcvy - residue cover on ridges before shifting today
c     rigcvt - residue cover remaining on ridges after shifting today
c     rmogy  - "residue mass on ground yesterday"; ie, yesterday's value
c              of RMOGT
c     optwat - optimum soil water content; calculated as 60%
c              of the total soil porosity.
c     atemp  - constant used in temperature function; now set to 6.1
c     opttmp - optimum temperature used in function; now set to 33 (C)
c     t1     - (avg. daily temp. [Celsius] + atemp) ** 2
c     t2     - (opttmp [Celsius] + atemp) ** 2 = 1528.81
c     tmpfac - temperature factor for residues (same factor is used for
c                standing, flat, buried residues & roots); 0<tmpfac<1
c     swatfc - water content factor for standing residues; 0<swatfc<1
c     fwatfc - water content factor for flat residues; 0<fwatfc<1
c     ferind - index for soil fertility; now set to 1.0
c     wght1  - fraction of space not in rills (ie, fraction of
c              interrill space).
c     pszind - index for residue particle size influence; now = 1.0
c     i1     - index of operation within tillage sequence
c     i2     - index of tillage sequence
c     numres - number of residue types being tracked on current OFE
c
c --------- Temporary Variables to enhance computational efficiency:
c     envinx - temporary variable used for computing FENVIN & BENVIN
c     tmpvr1 - temporary variable = oratea(iresd(1,iplane))*ferind*pszind
c     tmpvr2 - temporary variable = orater(iresd(1,iplane))*ferind*pszind
c     tmpvr3 - temporary variable = 1.0 - fbrnog(4,iplane)
c     tmpvr4 - temporary variable = rmagy-rmagt(iplane)
c     tmpvr5 - temporary variable = 1.0 - frmove(4,iplane)
c
c     + + + SAVES + + +
c     ... not used at this time 12-15-94 02:04pm sjl
c      save rcinit
c
c     + + + SUBROUTINES CALLED + + +
c     covcal
c
c     + + + DATA INITIALIZATIONS + + +
c
c
c     + + + END SPECIFICATIONS + + +
c
c
c               *********************************
c                *  Begin DECOMPOSITION MODEL  *
c               *********************************
c
c     Initialize local variables
c
      atemp = 6.1
      t2 = 1528.81
      ferind = 1.0
      pszind = 1.0
c
c     Determine number of residue groups currently being tracked
c
      if (iresd(3,iplane).ne.0) then
        numres = 3
      else if (iresd(2,iplane).ne.0) then
        numres = 2
      else
        numres = 1
      end if
c
c     Daily water coefficient for standing residues
c     *************************************************************
c     **    ( WEPP Equation 8.3.5a)                              **
c     **                                                         **
c     **                   Daily Rainfall (mm)                   **
c     **      WCF(s(t)) = ---------------------                  **
c     **                           4                             **
c     **                                                         **
c     **  Assumption: 4 mm of rainfall (per day) will saturate   **
c     **              the standing residues.                     **
c     *************************************************************
c
c     ----- Use daily rainfall to determine saturation of standing residues.
c
      if (tave.gt.0.0 .and. prcp.lt.0.004) then
        swatfc = prcp / 0.004
      elseif (tave.le.0.0) then
        swatfc = 0.0
      else
        swatfc = 1.0
      end if
c
c     Daily water coefficient for 1st soil layer.  (Used
c     for fallen residues, AND buried residues & roots.)
c     Note: This is a corruption of Equations 8.3.5b and
c     8.3.5c. -- CRM -- 3/26/92
c     *************************************************************
c     **    (WEPP Equation 8.3.5b)                               **
c     **                                                         **
c     **                       Residue Water Content             **
c     **      WCF(f(t)) = -------------------------------        **
c     **                   Optimum Residue Water Content         **
c     **                                                         **
c     **                                                         **
c     **    (WEPP Equation 8.3.5c)                               **
c     **                                                         **
c     **  For each                                               **
c     **  soil layer:                                            **
c     **                       Soil Layer Water Content          **
c     **      WCF(g(t)) = ----------------------------------     **
c     **                   Optimum Soil Layer Water Content      **
c     **                                                         **
c     **                                                         **
c     **  NOTE: Ideally, "soil layer water content" and "opti-   **
c     **        mum soil layer water content" are computed as    **
c     **        percent by *weight*.  In the equations below,    **
c     **        they are computed by *volume*.  If the para-     **
c     **        meters become available to compute by weight,    **
c     **        these equations should be converted.             **
c     **        -- CRM -- 11/8/92.                               **
c     **                                                         **
c     **  Currently soil water content is used for both flat     **
c     **  and buried residues because the WEPP WATBAL routines   **
c     **  do not yet track moisture within the surface residue   **
c     **  layer.  When residue moisture is added to WATBAL,      **
c     **  the WCF(f(t)) equation needs to be altered.            **
c     **  == D.E. Stott 6/30/92                                  **
c     **                                                         **
c     **  % Water filled pore space is actually a preferred way  **
c     **  to relate microbial activity to water content of the   **
c     **  soil as it takes bulk density into consideration.      **
c     **  This is especially true for soils that are wet enough  **
c     **  for oxygen to become limiting.  Water potential is     **
c     **  better for dry conditions.  == D.E. Stott 12/31/93     **
c     **                                                         **
c     **  For water contents (%WFP) greater than the optimal     **
c     **  water content (60%), WCF(f(t)) and WCF(g(t)) equal     **
c     **                                                         **
c     **                       Optimum Water Content             **
c     **      WCF(g(t)) = ----------------------------------     **
c     **                        Actual Water Content             **
c     **                                                         **
c     **    Per Linn and Doran (1984)  == D.E. Stott 1/30/93     **
c     **                                                         **
c     *************************************************************
c
c     ----- Calculate the value for 60% water filled pore space.
      optwat = avpor(iplane) * 0.60
c
c     ----- Use soil water content (surface layer) as percent moisture
c     of flat residue.
c

      sumwat=(soilw(1,iplane) + soilw(2,iplane))/
     1       (dg(1,iplane) + dg(2,iplane))

      if(sumwat.le.optwat)then
        fwatfc = sumwat/optwat
      else
        fwatfc = optwat/sumwat
      end if
c
c     Daily temperature index
c     *************************************************************
c     **    (WEPP Equation 8.3.6)                                **
c     **                                                         **
c     **             2*(T(avg)+A)^2 * (T(m)+A)^2 - (T(avg)+A)^4  **
c     **      TCF = -------------------------------------------- **
c     **                              (T(m)+A)^4                 **
c     **                                                         **
c     **  If T(m) = 33 and A = atemp = 6.1, then a temperature   **
c     **    constant, ktemp = 33+6.1 = 39.1.  Therefore:         **
c     **                                                         **
c     **        2*(T(avg)+Atemp)^2 * ktemp^2 - (T(avg)+Atemp)^4  **
c     **   TCF = --------------------------------------------    **
c     **                           ktemp^4                       **
c     **                                                         **
c     **  if (T(avg)+Atemp)^2 = t1 and ktemp^2 = t2, then        **
c     **                                                         **
c     **         (2*t1*t2) -  t1^2              t1[(t2*2)-t1]    **
c     **   TCF = ------------------ ==> TFC = -----------------  **
c     **               t2^2                         t2^2         **
c     **                                                         **
c     *************************************************************
c
c
      if (tave.gt.-6.1.and.tave.lt.49.2) then
        t1 = (tave+atemp) ** 2
c       The following is now set as a constant in the DATA INITIALIZATION
c       statements above.
c       t2 = (opttmp + atemp) ** 2
        tmpfac = t1 * (2*t2-t1) / t2 ** 2
      else
        tmpfac = 0.0
      end if
c
c     **********************************************************************
c     *   NOTE: SENVIN, FENVIN, & BENVIN are set by either *water* or      *
c     *         *temperature* -- whichever is limiting to decomposition.   *
c     *  senvin - daily environmental index for standing residues.         *
c     *  fenvin - daily environmental index for flat residues              *
c     *  benvin - daily environmental index for buried residues and roots  *
c     **********************************************************************
c     Calculate environmental index: separate indices for
c     standing, flat, and buried residues; later calculations
c     for roots use the same index as the buried residues.
c
c     *************************************************************
c     ** (Revised form of Equation 8.3.4, altered to work        **
c     **  with *daily* values of "M" in equation 8.3.7)          **
c     **  -- for standing residues --                            **
c     **                                                         **
c     **      ENVIND(t)   =   Min(TCF, WCF)                      **
c     *************************************************************
      senvin(iplane) = min(tmpfac,swatfc)
c
c     *************************************************************
c     ** (Revised form of Equation 8.3.4, altered to work        **
c     **  with *daily* values of "M" in equation 8.3.7)          **
c     **  -- for flat & buried residues --                       **
c     **  -- for up to 3 residue types. --                       **
c     **                                                         **
c     **      ENVIND(t)   =   Min(TCF, WCF)                      **
c     *************************************************************
      envinx = min(tmpfac,fwatfc)
      do 10 i = 1, numres
        fenvin(i,iplane) = envinx
        benvin(i,iplane) = envinx
   10 continue
c
c
c     ***************************************************************
c     *  rmagt - residue [bio]mass above ground (standing) today    *
c     *  rmogt - residue [bio]mass on the ground (flat on the soil  *
c     *           surface) today                                    *
c     *  smrm  - submerged (buried) residue (from above ground)     *
c     *           [bio]mass today                                   *
c     *  rtm   - non-living root [bio]mass                          *
c     *  rigrm - flat residue mass on ridges                        *
c     *  rilrm - flat residue mass in rills/furrows                 *
c     ***************************************************************
c
c     Compute Changes in Residue and Root Masses.
c
c     ... Compute decomposition of standing material (residue mass
c     standing above ground)  (expanded version of Equation 8.3.7.
c     TMPVR1 takes the place of "ORateA * FI * PS" in Equation 8.3.7.)
c     *************************************************************
c     **  (Corrected form of Equation 8.3.7)                     **
c     **   -- for standing residues --                           **
c     **                                                         **
c     **      M(t) = M(t-1) * exp(-ENVIND(t) * ORateA * FI * PS) **
c     **                                                         **
c     *************************************************************
c
      tmpvr1 = oratea(iresd(1,iplane)) * ferind * pszind
      rmagt(iplane) = rmagt(iplane) * exp(-senvin(iplane)*tmpvr1)
      if (rmagt(iplane).lt.0.00001) rmagt(iplane) = 0.00001
c
c     ----- fraction of interrill area
      wght1 = (rspace(iplane)-width(iplane)) / rspace(iplane)
c
      do 20 nowres = 1, numres
c
c       ...... Compute decomposition of flat residue
c       *************************************************************
c       **  (Corrected form of Equation 8.3.7)                     **
c       **   -- for flat residues --                               **
c       **                                                         **
c       **      M(t) = M(t-1) * exp(-ENVIND(t) * ORateA * FI * PS) **
c       **                                                         **
c       *************************************************************
c
        env = exp(-fenvin(nowres,iplane)*tmpvr1)
c
        rilrm(nowres,iplane) = rilrm(nowres,iplane) * env
        rigrm(nowres,iplane) = rigrm(nowres,iplane) * env
        if (rilrm(nowres,iplane).lt.0.00001) rilrm(nowres,iplane) =
     1      0.00001
        if (rigrm(nowres,iplane).lt.0.00001) rigrm(nowres,iplane) =
     1      0.00001
        rmogt(nowres,iplane) = wght1 * rigrm(nowres,iplane) + (1.0-wght1
     1      ) * rilrm(nowres,iplane)
c
c
c       ...... Compute decomposition of buried mass
c       *************************************************************
c       **  (Corrected form of Equation 8.3.7)                     **
c       **   -- for buried residues --                             **
c       **                                                         **
c       **      M(t) = M(t-1) * exp(-ENVIND(t) * ORateR * FI * PS) **
c       **                                                         **
c       *************************************************************
c
        smrm(nowres,iplane) = smrm(nowres,iplane) * env
        if (smrm(nowres,iplane).lt.0.00001) smrm(nowres,iplane) =
     1      0.00001
c
c
c       ...... Compute decomposition of root mass
c       TMPVR2 takes the place of "ORateR * FI * PS" in Equation 8.3.7.)
c       *************************************************************
c       **  (Corrected form of Equation 8.3.7)                     **
c       **   -- for root residues --                               **
c       **                                                         **
c       **      M(t) = M(t-1) * exp(-ENVIND(t) * ORateA * FI * PS) **
c       **                                                         **
c       *************************************************************
c
        tmpvr2 = orater(iresd(1,iplane)) * ferind * pszind
        env = exp(-benvin(nowres,iplane)*tmpvr2)
        rtm(nowres,iplane) = rtm(nowres,iplane) * env
        if (rtm(nowres,iplane).lt.0.00001) rtm(nowres,iplane) = 0.00001
c
   20 continue
c
c
c     ******************************************************************
c     *  CHANGE IN STANDING & FLAT RESIDUE MASSES DUE TO MANAGEMENT  *
c     ******************************************************************
c
      rmagy = rmagt(iplane)
      rmagt(iplane) = rmagt(iplane) * fact(iresd(1,iplane))
c
c     **************************************************************
c     *  NOTE: In the next 2 statements, it may appear that *all*  *
c     *        the "delta-residue" is being used *twice*; ie, in   *
c     *        *both* RILRM and RIGRM.  However, this is not the   *
c     *        case since the residue masses are expressed *per    *
c     *        unit area*.   CRM -- 10/13/92.                      *
c     **************************************************************
c
c     ---- update ridge & rill residue masses
      rilrm(1,iplane) = rilrm(1,iplane) + (rmagy-rmagt(iplane))
      rigrm(1,iplane) = rigrm(1,iplane) + (rmagy-rmagt(iplane))
c     ---- update residue mass on the ground
      rmogt(1,iplane) = wght1 * rigrm(1,iplane) + (1.0-wght1) *
     1    rilrm(1,iplane)
c
c     *** L1 IF ***
c     Burning
c
      if (sdate.eq.jdburn(nowcrp,iplane).and.jdburn(nowcrp,iplane).ge.
     1    jdharv(nowcrp,iplane)) then
c       ------ fraction of residue mass remaining
        tmpvr3 = 1.0 - fbrnog(nowcrp,iplane)
        do 30 nowres = 1, numres
c         -------- update ridge & rill residue masses
          rigrm(nowres,iplane) = rigrm(nowres,iplane) * tmpvr3
          rilrm(nowres,iplane) = rilrm(nowres,iplane) * tmpvr3
c         -------- update residue mass on the ground
c         (WEPP Equation 8.4.12)
          rmogt(nowres,iplane) = rmogt(nowres,iplane) * tmpvr3
   30   continue
c       ------ update residue mass above the ground
c       (WEPP Equation 8.4.11)
        rmagt(iplane) = rmagt(iplane) * (1.-fbrnag(nowcrp,iplane))
c     ***********************************************************************
c     * NOTE: In the User Documentation AND TILAGE.F (where they are read   *
c     *       in) AND in common block PEREN (where they are stored) FBRNAG  *
c     *       & FBRNOG are BACKWARDS!!  In the equations immediately above, *
c     *       they follow the convention of 'A' designating "above the      *
c     *       ground; ie, standing" and 'O' designating "on the ground; ie, *
c     *       flat". -- CRM -- 11/16/92.                                    *
c     ***********************************************************************
c
c     *** L1 ELSE-IF ***
c     Standing residue cut
c
      else if (sdate.eq.jdcut(nowcrp,iplane)) then
c       ------ "remember" RMAGT
        rmagy = rmagt(iplane)
c       ------ re-compute standing residue mass
c       (WEPP Equation 8.4.13)
        rmagt(iplane) = rmagt(iplane) * (1.-frcut(nowcrp,iplane))
c       ------ compute decrease in standing residue mass
        tmpvr4 = rmagy - rmagt(iplane)
        do 40 nowres = 1, numres
c         -------- add mass of cut residue to ridge & rill residue masses
          rilrm(nowres,iplane) = rilrm(nowres,iplane) + tmpvr4
          rigrm(nowres,iplane) = rigrm(nowres,iplane) + tmpvr4
c         -------- add mass of cut residue to residue mass on the ground
c         (WEPP Equation 8.4.14)
          rmogt(nowres,iplane) = rmogt(nowres,iplane) + tmpvr4
   40   continue
c
c     *** L1 ELSE-IF ***
c     Flat residue removed option modified to be a tillage operation
c     handled below (for management files version 98.3 and up
c
      else if (manver.lt.98.3.and.sdate.eq.jdmove(nowcrp,iplane)) then
c       ------ fraction of flat residue mass remaining
        tmpvr5 = 1.0 - frmove(nowcrp,iplane)
        do 45 nowres = 1, numres
c         -------- update ridge & rill residue masses
          rilrm(nowres,iplane) = rilrm(nowres,iplane) * tmpvr5
          rigrm(nowres,iplane) = rigrm(nowres,iplane) * tmpvr5
c         -------- update residue mass on the ground
c         (WEPP Equation 8.4.15)
          rmogt(nowres,iplane) = rmogt(nowres,iplane) * tmpvr5
   45   continue
c
c     *** L1 END-IF ***
      end if
c
c
c     *********************************************
c     *  CHANGE IN RESIDUE MASS DUE TO TILLAGE  *
c     *********************************************
c
c     ---- index of tillage implement within sequence (scenario)
      i1 = indxy(iplane)
c     ---- index of tillage sequence (scenario)
      i2 = tilseq(nowcrp,iplane)
c
c     *** M1 IF ***
c     Check whether there is tillage on this crop, this OFE.
      if (i2.gt.0.and.i1.gt.0) then
c
c       *** M2 IF ***
c       Check whether this is a management date.
        if (sdate.eq.mdate(i1,i2)) then
          if(manver.lt.98.3.or.resman(i1,i2).le.4.)then
c   ********************************************************************
c   * OLD DECOMP CODE PRIOR TO MULTIPLE RESIDUE ADDITIONS AND REMOVALS *
c   * OR NO RESIDUE REMOVALS OR ADDITIONS TO A NEWER MANAGEMENT FILE   *
c   ********************************************************************
            do 60 nowres = 1, numres
              rmogy(nowres) = rmogt(nowres,iplane)
   60       continue
            rmagy = rmagt(iplane)
c           -------- Calculate standing residue mass today (RMAGT)
c           based on yesterday's value.
c           (WEPP Equation 8.4.6)
            rmagt(iplane) = rmagt(iplane) *
     1          exp(-8.535*mfo(i1,i2,iresd(1,iplane))**2)
c
c           Calculate change in residue mass due to tillage for
c           ridged or non-ridged systems
c
c           (WEPP Equation 8.4.7 applied to ridge and rill areas)
            rigrm(1,iplane) = rigrm(1,iplane) + (rmagy-rmagt(iplane))
            rilrm(1,iplane) = rilrm(1,iplane) + (rmagy-rmagt(iplane))
c
            rmogy(1) = wght1 * rigrm(1,iplane) + (1-wght1) *
     1          rilrm(1,iplane)
c
            do 70 nowres = 1, numres
c             ------------ cover from flat residues (on ridges)
c
              flrcov = (1.0-exp(-cf(iresd(nowres,iplane))*
     1            rigrm(nowres,iplane))) * (1.0-
     1            mfo(i1,i2,iresd(nowres,iplane)))
              if (flrcov.lt.0.0) flrcov = 0.0
              if (flrcov.gt.0.999) flrcov = 0.999
c             ------------ residue mass on ridges
c
              rigrm(nowres,iplane) = log(1-flrcov) / (-
     1            cf(iresd(nowres,iplane)))
c
c             ------------ cover from flat residues (in rills)
              flrcov = (1.0-exp(-cf(iresd(nowres,iplane))*
     1            rilrm(nowres,iplane))) * (1.0-
     1            rmfo(i1,i2,iresd(nowres,iplane)))
              if (flrcov.lt.0.0) flrcov = 0.0
              if (flrcov.gt.0.999) flrcov = 0.999
c
c             ------------ Compute residue mass in rills (RILRM) from flat
c             residue cover (FLRCOV) and flat residue cover coef. (CF).
c             (Revised WEPP Equation 8.3.13)
              rilrm(nowres,iplane) = log(1-flrcov) / (-
     1            cf(iresd(nowres,iplane)))
              rmogt(nowres,iplane) = wght1 * rigrm(nowres,iplane) +
     1            (1.0 - wght1) * rilrm(nowres,iplane)
   70       continue
c
c           Computation of new submerged residue masses due to tillage
c
            do 80 nowres = 1, numres
              smrm(nowres,iplane) = smrm(nowres,iplane) +
     1            rmogy(nowres) - rmogt(nowres,iplane)
   80       continue
          else
c           *********************************************************************
c           * NEW RESIDUE CODE ALLOWING MULTIPLE RESIDUE ADDITIONS AND REMOVALS *
c           *********************************************************************
c
c           resman(mxtill,mxtilsq): residue management option for this operation
c                             10 = residue addition with no disturbance
c                             11 = residue removal with no disturbance
c                             12 = residue addition with disturbance
c                             13 = residue removal with disturbance
c
c           if this is a residue addition date for this OFE
c
            if(resman(i1,i2).eq.10.or.resman(i1,i2).eq.12)then
c
c           update residue tracking
c
c

              do 90 nowres = 1, numres
                rmogy(nowres) = rmogt(nowres,iplane)
   90         continue
              iresd(1,iplane)=iresad(i1,i2)
              call resup(nowcrp,resman(i1,i2))
              if(resman(i1,i2).eq.10)
     1            write(6,1000)resad(i1,i2),crpnam(iresd(1,iplane)),
     1                iplane,mdate(i1,i2)
              if(resman(i1,i2).eq.12)
     1            write(6,1010)resad(i1,i2),crpnam(iresd(1,iplane)),
     1            iplane,mdate(i1,i2)
              rmogt(1,iplane)=resad(i1,i2)
              rilrm(1,iplane)=rmogt(1,iplane)
              rigrm(1,iplane)=rmogt(1,iplane)
c
c           if this is a residue removal date for this OFE
            else if(resman(i1,i2).eq.11.or.resman(i1,i2).eq.13)then
c           amount of residue removed if any
              if(resman(i1,i2).eq.11)
     1            write(6,1050)frmove(i1,i2)*100,iplane,mdate(i1,i2)
              if(resman(i1,i2).eq.13)
     1            write(6,1100)frmove(i1,i2)*100,iplane,mdate(i1,i2)

c             ------ fraction of flat residue mass remaining
              tmpvr5 = 1.0 - frmove(i1,i2)
              do 100 nowres = 1, numres
c           -------- update ridge & rill residue masses
                rmogt(nowres,iplane) = rmogt(nowres,iplane) * tmpvr5
                rilrm(nowres,iplane) = rilrm(nowres,iplane) * tmpvr5
                rigrm(nowres,iplane) = rigrm(nowres,iplane) * tmpvr5
c               -------- update residue mass on the ground
c               (WEPP Equation 8.4.15)
  100         continue
            end if
c
          rmagy = rmagt(iplane)
c         -------- Calculate standing residue mass today (RMAGT)
c         based on yesterday's value.
c         (WEPP Equation 8.4.6)
c
c         if residue removal with disturbance
          if(resman(i1,i2).eq.13)
     1        rmagt(iplane) = rmagt(iplane) *
     1        exp(-8.535*mfo(i1,i2,iresd(1,iplane))**2)
c
          if(resman(i1,i2).eq.12)
     1        rmagt(iplane) = rmagt(iplane) *
     1        exp(-8.535*mfo(i1,i2,nowcrp)**2)
c
c         Calculate change in residue mass due to tillage for
c         ridged or non-ridged systems
c
c         (WEPP Equation 8.4.7 applied to ridge and rill areas)
          rigrm(1,iplane) = rigrm(1,iplane) + (rmagy-rmagt(iplane))
          rilrm(1,iplane) = rilrm(1,iplane) + (rmagy-rmagt(iplane))
          rmogy(1) = wght1 * rigrm(1,iplane) + (1-wght1) *
     1        rilrm(1,iplane)

c
c
c         Computation of new submerged residue masses due to tillage
c
          do 110 nowres = 1, numres
            if(resman(i1,i2).eq.10.or.resman(i1,i2).eq.11)then
              smrm(nowres,iplane) = smrm(nowres,iplane)
            else
              smrm(nowres,iplane) = smrm(nowres,iplane)
     1            + rmogy(nowres) - rmogt(nowres,iplane)
              if(smrm(nowres,iplane).lt.0.00001)
     1              smrm(nowres,iplane)=0.00001
            end if
  110     continue
cc       *** M2 END-IF ***
         end if
c        *** M1 END-IF ***
       end if
c
       end if
c
c XXX  Problem here with residue moving from interrill to rill area
c      when rills are narrow - get unrealistic amounts of residue
c      piling up in the rills.   Commented out this code  (N1 IF)
c      until problem is resolved, based on Nearing recommendation.
c      dcf  8/25/94
c
c     ******************************************************************
c     *  RESIDUE MOVEMENT FROM RIDGE TO FURROW IN RIDGE-TILLAGE SYSTEMS
c     ******************************************************************
c
c     *** N1 IF's (Note check for positive subscript.) ***
c     if (i2.gt.0) then
c       if (iridge(i2).eq.1) then
cc        ------ "days after harvest"
c         dah = sdate - jdharv(nowcrp,iplane)
cc        *** N2 IF ***
cc        If this is a harvest date....
c
cc        NOTE - change code to if(dah.eq.1) because change in order of
cc        subroutine calls in SR CONTIN has messed this up ----  we call
cc        DECOMP at the beginning of the simulation day before WATBAL
cc        and PTGRA and RESUP have been called - since RESUP has not yet
cc        been called on an sdate=jdharv  we have not yet incremented
cc        iresd by 1, so on dah=0  numres may only equal 1 when it
cc        should be 2.
c         if (dah.eq.1) then
c           do 90 nowres = 1, numres
cc            ---------- compute ridge cover at harvest, from residue
cc            mass on ridges
cc            (Note: "1 - exp(-cf)" converts from *mass* to *cover*.)
c             rcinit(nowres,iplane) = 1 -
c    1            exp(-cf(iresd(nowres,iplane))*rigrm(nowres,iplane))
c  90       continue
cc        *** N2 ELSE-IF ***
cc        If it's less than 60 days after harvest, calculate residue
cc        redistribution from ridges to furrows by wind, rain & gravity.
c         else if (dah.gt.1.and.dah.le.60) then
c           do 100 nowres = 1, numres
cc            ---------- residue cover on ridges before redistribution
c             rigcvy = 1 - exp(-cf(iresd(nowres,iplane))*
c    1            rigrm(nowres,iplane))
cc            ---------- residue cover remaining on the ridges today
cc            (prorated over the 60 days following harvest, down to
cc            30 percent).
cc            (WEPP Equation 8.3.12)
c             rigcvt = rcinit(nowres,iplane) - (rcinit(nowres,iplane)-
c    1            0.30) / 60.0 * dah
cc            ---------- mass of residue moved from the ridges into the
cc            furrows today
cc            (WEPP Equation 8.3.13)
c             deltrm = log(1.-(rigcvy-rigcvt)) / (-
c    1            cf(iresd(nowres,iplane)))
c             if (deltrm.lt.0.0) deltrm = 0.0
cc            ---------- residue mass in the rills
cc            (WEPP Equation 8.3.14)
c             rilrm(nowres,iplane) = rilrm(nowres,iplane) + deltrm
cc            ---------- residue mass on the ridges
cc            (WEPP Equation 8.3.16)
c             rigrm(nowres,iplane) = rigrm(nowres,iplane) - deltrm
c             rmogt(nowres,iplane) = wght1 * rigrm(nowres,iplane) + (1.0
c    1            -wght1) * rilrm(nowres,iplane)
c 100       continue
cc        *** N2 END-IF ***
c         end if
cc      *** N1 END-IF's ***
c       end if
c     end if
c XXX End of commented out code for problem with residue movement
c     from interrill to rills for ridge tillage systems. dcf 8/25/94
c
c
c     --- Compute the fraction of flat surface residue cover, standing
c     residue cover, and average surface cover
      call covcal(iresd,iplane)
c
c     Compute (POP) population of standing stalks(?) from the ratio of
c     standing residue mass today (RMAGT) to standing residue at
c     harvest (SRMHAV), and population at maturity (POPMAT).
c
c       added trap to prevent divide by zero if user has input a
c       value of zero for cutting height (making partcf = 0.0 )
c       dcf   3/8/94
c
      if(srmhav(iplane) .gt. 0.0)then
        pop(iplane) = (rmagt(iplane)/srmhav(iplane)) * popmat(iplane)
      else
        pop(iplane) = 0.0
      endif
c
      if (pop(iplane).gt.popmat(iplane)) pop(iplane) = popmat(iplane)
c
c     -- XXX -- This computation of POP looks like a real problem to me!
c     The approach seems unsound -- POP will max out before
c     maturity is reached.  The location of this code seems
c     inappropriate -- the code has nothing to do with
c     computing residue or decomposition.  I suggest it be
c     moved to the rangeland (or perennial) plant growth
c     routines.  -- CRM -- 12/01/92.
c
      return
 1000 format(1x,f4.1,'(kg/m^2) ',A10,' RESIDUE ADDED TO PLANE '
     1    ,i2,', NO DISTURBANCE, ON DAY',i4)
 1010 format(1x,f4.1,'(kg/m^2) ',A10,' RESIDUE ADDED TO PLANE '
     1    ,i2,', WITH DISTURBANCE, ON DAY',i4)
 1050 format(1x,f4.1,'% RESIDUE REMOVED FROM PLANE '
     1    ,i2,', NO DISTURBANCE, ON DAY',i4)
 1100 format(1x,f4.1,'% RESIDUE REMOVED FROM PLANE '
     1    ,i2,', WITH DISTURBANCE, ON DAY',i4)
      end
