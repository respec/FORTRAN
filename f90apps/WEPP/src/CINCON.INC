
c     begin include file cincon.inc

c     + + + COMMON BLOCK DESCRIPTION + + +

c     This include file contains specifications for the initial condition
c     scenario building portion of the model.

      common /incon/ ibd,idshar,irfcum,irspac,ipptg,irroug,
     1               icanco,icrypt,idaydi,ifrdp,iinrco,iiresd,
     1               irilco,irhini,irmagt,irmogt,irrini,irtype,isnodp,
     1               ithdp,itill1,itill2,iwidth

c     + + + VARIABLE DECLARATIONS + + +

      real    ibd,irfcum,irspac,ipptg,irroug,icanco,icrypt,idaydi,
     1        ifrdp,iinrco,irilco,irhini,irmagt,irmogt,irrini,
     1        isnodp,ithdp,itill1,itill2,iwidth
     1
      integer idshar,iiresd,irtype

c     + + + VARIABLE DEFINITIONS + + +

c     All variables are associated with ofe # 1

c     ibd    : bulk density after last tillage (g/cc)
c     idshar : days from last harvest to end of simulation (days)
c     irfcum : cummulative rain from last tillage to end of simulation (m)
c     irspac : rill spacing (m)
c     ipptg  : average rainfall over simulation, range (m)
c     irroug : random roughness at end of simulation, range (m)
c     irmagt : residue mass above ground at end of
c              simulation, range, (kg/m**2)
c     irmogt : residue mass on the ground at end of
c              simulation, range, (kg/m**2)
c     irtype : rill type (0=temporary, 1=permanant)
c     isnodp : snow depth at end of simulation (m)
c     ithdp  : thaw depth at end of simulation (m)
c     itill1 : depth of secondary tillage layer at end of simulation (m)
c     itill2 : depth of primary tillage layer at end of simulation (m)
c     iwidth : width of rill at end of simulation (m)
c     icrypt : surface cover at end of simulation, range (0-1)
c     irhini : ridge hight after last tillage of simulation (m)
c     irilco : rill cover at end of simulation (0-1)
c     irrini : ridge roughness after last tillage (m)
c     ifrdp  : frost depth at end of simulation (m)
c     iinrco : interrill cover at end of simulation (0-1)
c     iiresd : newest residue on ofe at end of simulation
c     icanco : canopy cover at end of simulation (0-1)
c     idaydi : days from last disturbance to end of simulation (days)

c     end include file cincon.inc

