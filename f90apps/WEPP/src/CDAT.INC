
c     begin include file cdat.inc

c     + + + COMMON BLOCK DESCRIPTION + + +
c     + + + INSERT DESCRIPTION HERE  + + +

      common /dat/ manchk, solchk, slpchk, hilchk, strchk, chnchk,
     1    impchk, irdsch, irdfch, irfsch, irffch, idsver,
     1    idfver, ifsver, iffver

c     + + + VARIABLE DECLARATIONS + + +

      real manchk, solchk, slpchk, hilchk, strchk, chnchk, impchk,
     1    irdsch, irfsch, irffch, idsver, idfver, ifsver, iffver,
     1    irdfch

c     + + + VARIABLE DEFINITIONS + + +

c     manchk : wepp management file compatability check
c     slpchk : wepp slope file compatibility check
c     solchk : wepp soil file compatibility check
c     hilchk : hillslope to watershed pass file compatibility check
c     strchk : watershed structure file compatibility check
c     chnchk : watershed channel file compatibility check
c     impchk : watershed impoundment file compatibility check
c     irdsch : wepp depletion level sprinkler irrigation file
c              compatibility check
c     irdfch : wepp depletion level furrow irrigation file
c              compatibility check
c     irfsch : wepp fixed date sprinkler irrigation file
c              compatibility check
c     irffch : wepp fixed date furrow irrigation file
c              compatibility check
c     idsver : depletion sprinkler irrigation file version read in
c     idfver : depletion furrow irrigation file version read in
c     ifsver : fixed date sprinkler irrigation file version read in
c     iffver : fixed date furrow irrigation file version read in

c     end include file cdat.inc


