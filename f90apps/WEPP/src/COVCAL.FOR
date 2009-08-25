      subroutine covcal(iresd,iplane)
c
c     + + + PURPOSE + + +
c     Calculates the fraction of soil cover provided by flat residue,
c     and by standing stems (stubble).
c
c     Note: Flat residue cover fraction is calculated using a probability
c           equation developed by James M. Gregory for a single residue
c           type at a single point in time.  This method assumes uniform
c           pieces of residue and a spatially random distribution of those
c           pieces.
c           WEPP (and RUSLE) attempt to adapt Gregory's equation to multi-
c           ple residue types, assuming that the coefficient relating cover
c           to mass does not change with time.  It is intuitively obvious
c           that for multiple residue types, one can use Gregory's equation
c           for each type individually and subtract the overlap between
c           types to compute total cover.  Since the residue is assumed to
c           be randomly distributed, the amount to be subtracted is simply
c           the product of the current cover coefficient and the previous
c           one.  What is perhaps less obvious, is that the power of the
c           exponential in Gregory's equation is simply the amount of cover
c           that would result if no overlap occurs, and thus type and size
c           of the pieces make no difference -- the various residue masses
c           may simply be multiplied by their cover coefficients, and the
c           resulting sum plugged into Gregory's equation.  This was con-
c           firmed by performing the calculations both ways.  CRM -- 1/13/93.
c
c     Called from DECOMP
c     Author(s): Yoder, Stott, Laflen
c     Reference in User Guide: Chapter 8.
c                              Also see: "Soil Cover Prediction with
c                              Various Amounts and Types of Crop Residue".
c                              1982.  James M. Gregory.  Trans. ASAE.
c                              pp 1333-1337.
c
c     Changes:
c           1) Eliminated reference to PTILTY.INC.
c           2) Eliminated reference to CCONTCV.INC.
c           3) Moved Equation 8.3.9 inside do-10 loop.
c              (RE: 1/11/93 conversation with D. Flanagan.)
c              This resulted in TEST2 increases in: Rill & Interrill
c              Cover, Ksat, and Effective Duration.  Peak Runoff went
c              down.
c           4) Since variables TMASS and TMASS2 really represent
c              *area* and not *masses*, changed the variable names
c              to AREA and AREA2.
c
c     Version: This module recoded from WEPP Version 92.332
c     Date recoded: 01/11/93 to 01/12/93.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxcrp.inc'
      include 'pmxcut.inc'
      include 'pmxnsl.inc'
      include 'pmxpln.inc'
      include 'pmxres.inc'
      include 'pmxtil.inc'
      include 'pmxtls.inc'
      include 'pntype.inc'
c
c     + + + ARGUMENT DECLARATIONS + + +
      integer iresd(mxres,mxplan), iplane, numres
c
c     + + + ARGUMENT DEFINITIONS + + +
c     iresd  - array of residue types in this simulation
c     iplane - current OFE
c
c     + + + COMMON BLOCKS + + +
c
      include 'ccover.inc'
c      modify: inrcov, rilcov
c
      include 'ccrpout.inc'
c       write: rescov
c
      include 'ccrpvr1.inc'
c        read: rmagt
c
      include 'ccrpvr2.inc'
c        read: cf
c
      include 'ccrpvr5.inc'
c        read: basmat
c
      include 'cperen.inc'
c        read: srmhav
c
      include 'cridge.inc'
c        read: rilrm, rigrm
c      modify: rigcov
c
      include 'cends4.inc'
c        read: rspace
c
c     + + + LOCAL VARIABLES + + +
      integer nowres
      real strcov, area, area2
c
c     + + + LOCAL DEFINITIONS + + +
c     nowres -
c     strcov -
c     area  - area of the ridge that would be covered by residue, if no
c             overlap of the residue occurred; ie, maximum possible.
c     area2 - area of the ridge that would be covered by residue, if no
c             overlap of the residue occurred; ie, maximum possible.
c
c     + + + END SPECIFICATIONS + + +
c
c ---- Determine number of residues currently being tracked (NUMRES).
      if (iresd(3,iplane).ne.0) then
        numres = 3
      else if (iresd(2,iplane).ne.0) then
        numres = 2
      else
        numres = 1
      end if
c
c     Calculate STRCOV, Soil Cover due to Standing Residue Mass
c     (cross-sectional area of standing stalks).  For purpose of
c     this calculation, it is assumed that ratio of standing
c     residue mass (RMAGT) to standing residue mass at harvest
c     (SRMHAV), is the same as the ratio of current stubble
c     population to stubble population at harvest.  This ratio
c     is multipled by the stem basal area at maturity for the
c     crop of interest.
c     (Modified WEPP Equation 8.3.10)
c
c
c     added trap to prevent divide by zero if user has input a
c     value of zero for cutting height (making partcf = 0.0 )
c     dcf   3/8/94
c
      if(srmhav(iplane) .gt. 0.0)then
        strcov = rmagt(iplane) / srmhav(iplane) * basmat(iplane)
      else
        strcov = 0.0
      endif
      if(strcov.gt.basmat(iplane))strcov=basmat(iplane)
c
c
c     Sum up the flat residue cover provided by each residue type,
c     using Equation 11 from the Gregory paper.  CF is the average
c     cover per unit mass, assuming no overlap; ie, the average
c     cover per unit weight provided by a single piece of residue.
c     The sum represents the maximum area that could be covered if
c     all the residue were laid out side by side.  CRM -- 1/13/93.
c
      area = 0.0
      area2 = 0.0
      do 10 nowres = 1, numres
c       ------ sum of ridge flat residue cover
        area = area + cf(iresd(nowres,iplane)) * rigrm(nowres,iplane)
c       ------ sum of rill flat residue cover
        area2 = area2 + cf(iresd(nowres,iplane)) *
     1      rilrm(nowres,iplane)
   10 continue
c
c
c     Total soil cover in ridges & rills; ie, flat residue plus stubble.
c     (WEPP Equation 8.3.11)
      rigcov(iplane) = 1.0 - exp(-area) + strcov
      if (rigcov(iplane).lt.0.0) rigcov(iplane) = 0.0
      if (rigcov(iplane).gt.0.999) rigcov(iplane) = 0.999
c
      rilcov(iplane) = 1.0 - exp(-area2) + strcov
      if (rilcov(iplane).lt.0.0) rilcov(iplane) = 0.0
      if (rilcov(iplane).gt.0.999) rilcov(iplane) = 0.999
c
c
      inrcov(iplane) = rigcov(iplane)
      rescov(iplane) = ((rspace(iplane)-width(iplane))/rspace(iplane)) *
     1    inrcov(iplane) + (width(iplane)/rspace(iplane)) *
     1    rilcov(iplane)
c
      return
      end
