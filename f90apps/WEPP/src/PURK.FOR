      subroutine purk
c
c     + + + PURPOSE + + +
c     The master percolation component which routes the
c     percolated water down through the soil layers, and
c     and updates the water content of each soil layer.
c     For each layer above field capacity, the excess (VV)
c     is subjected to percolation.  The amount percolated
c     is the seepage (SEP).  The water content (ST) of
c     both layers is updated by the amount of seepage (SEP).
c     Immediately before PURK returns, any infiltration
c     from today's precip (FIN) is added to the top layer.
c
c     Called from WATBAL
c     Author(s): Williams, Savabi, Meyer
c     Reference in User Guide:
c
c     Changes:
c          1) Common block UPDATE was not used.  Dereferenced.
c          2) SAVE of all local variables was deleted.
c          3) Parameters MXPOND, MXTLSQ, & MXTILL were not
c             used.  Their include files were dereferenced.
c          4) Embedded RETURN converted to IF structure.
c          5) Local variables N1, N2, N3, SUM, & TOT were
c             eliminated, and VV was changed from an array to
c             a single variable.
c          6) In the original code, if water excess was negative,
c             it was set to zero.  Percolation and infiltration
c             was then added to the zero.  This was eliminated.
c             THIS CAUSES CHANGES IN THE OUTPUT.
c          7) The statement adding infiltration from precipitation
c             (FIN) to the top soil layer, was originally the first
c             executable statement of subroutine PURK.  This was
c             conceptually incorrect, since it caused percolation
c             in the top layer to be simulated for BOTH the water
c             already present before the precipitation event, AND
c             the precipitation water itself.  This statement was
c             moved to be the last executable statement before the
c             RETURN.  This also proved undesirable, since it post-
c             poned addition of the infiltrated water to the top
c             soil layer until the following day.  This also changes
c             the output.  The statement was moved back to the first
c             location, and finally removed altogether.  It is now
c             in subroutine GRNA.  CRM -- 12/3/91
c          8) The code was processing the top 4 mm. from each
c             layer, and then the NEXT 4mm from each layer,
c             etc.  This did not route the water from the top
c             of each layer, through that layer, and into the
c             next layer.  Also, since the code was slicing
c             each soil layer into 4 mm. slices, 1 meter layer
c             would require 250 passes through PERC.  The code
c             in PERC also appeared to be incorrect, since there
c             was no prorating applied to the partial layers.
c             Numbers used were the numbers for the entire
c             layer, not 4 mm. of it.  The code was revised to
c             process an entire layer at once.  Both changing
c             the order of processing the slices, and changing
c             the number of slices the layer is divided into
c             CAUSE CHANGES IN THE OUTPUT.
c          9) AMT was originally used to designate the top part
c             of a soil layer, which would not be considered
c             for lateral flow.  Since lateral flow was not
c             represented in the code, and the code that did
c             exist was not correct (items 7 & 8, above), AMT
c             was deleted.  If it is ever put back it should be
c             changed from a local real variable initialized
c             in a DATA statement, to a local real parameter.
c         10) Logic of the program radically changed (items
c             6-9).  This cut an 8.3 second run to 7.8 seconds
c             -- even WITH the EXP in PERC. -- CRM -- 8/30/90.
c         11) PERC was changed to receive the parameter VV in
c             lieu of getting SU from a common block.  This is
c             to make the code easier to follow, etc.  J1 was
c             changed into parameter K1 for the same reason.
c             SU, J1, & J2 SHOULD BE REMOVED FROM COMMON BLOCK
c             'WATER'.
c         12) PERC was changed to receive a parameter VV in
c             lieu of getting SU from a common block.  This
c             is to make the code easier to follow, etc.
c         13) Initialization of SEP removed from PURK, since
c             PERC always recomputes its value.
c         14) Order of percolation through the layers reversed
c             (done from bottom-up rather than top-down), to
c             avoid inaccuracies caused by adding the water
c             percolated *into* the layer, before subjecting
c             its *existing* water to percolation.  THIS DOES
c             SLIGHTLY INCREASE THE PREDICTED RUNOFF.
c
c     Version: This module originally recoded from WEPP version 91.20.
c     Date recoded: 08/22/91 - 09/18/91.
c     Version: This module updated from WEPP release version 91.50.
c     Date updated: 12/03/91.
c     Recoded by: Charles R. Meyer.
c
c     Date recoded: 08/28/91 - 09/12/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
      include 'pmxnsl.inc'
c       read: mxnsl
      include 'pmxhil.inc'
c       read: mxhill
      include 'pmxpln.inc'
c       read: mxplan
c
c     + + + COMMON BLOCKS + + +
      include 'cstruc.inc'
c       read: iplane
c
      include 'cwater.inc'
c       read: nsl(mxplan),fc(mxnsl),sep(iplane)
c     modify: st(mxnsl,mxplan)
      include 'cwint.inc'
c
c     + + + LOCAL VARIABLES + + +
      real vv, sepsav,vartmp, varufz,tolwat,varuf1
      integer k1, k2
c
c     + + + LOCAL DEFINITIONS + + +
c     vv     - Water excess beyond field capacity for the current
c              soil layer.
c     k1     - counter in main loop.
c     k2     - k1 + 1
c     sepsav - used to save SEP from bottom layer.  (Seepage from
c              the bottom layer is needed for water balance calcs.)
c
c     + + + SUBROUTINES CALLED + + +
c     perc
c
c     + + + END SPECIFICATIONS + + +
c
c
c      For each layer, starting with the BOTTOM layer,
c      percolate the water excess to the layer below.
c      (This approach is taken to avoid the compounding
c      effect caused by dumping percolated water from
c      the layer above, on top of existing water that
c      has not yet been subjected to percolation.)
c
      sepsav = 0.0
      varufz = 0.0
      varuf1 = 0.0
      do 10 k1 = nsl(iplane), 1, -1
c       ------- compute water excess
cd      Modified by S. Dun, March 04, 2008 for frozen soil
         if (frzw(k1,iplane).ge. fc(k1)) then
             vv = st(k1,iplane)
         else
             vv = st(k1,iplane) - (fc(k1)- frzw(k1, iplane))
         endif 
cd      end modifying
c       ------- when there is an excess....
        if (vv.gt.0.) then
          k2 = k1 + 1
c         --------- compute percolation through the layer.
          call perc(vv,k1)
c         --------- reduce water content of current layer
          st(k1,iplane) = st(k1,iplane) - sep(iplane)
          if (st(k1,iplane).lt.1e-10) st(k1,iplane) = 0.0
c
          if (k1.lt.nsl(iplane)) then
c           ----------- add seepage to layer below
            st(k2,iplane) = st(k2,iplane) + sep(iplane)
          else
c           ----------- "remember" seepage from bottom layer
            sepsav = sep(iplane)
          end if
        end if
c
   10 continue
      sep(iplane) = sepsav
c
      return
      end
