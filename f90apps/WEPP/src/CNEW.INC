c
c     begin include file cnew.inc
c
c     +++ COMMON BLOCK DESCRIPTION
c
c     Variables read in infile but not used at this time.
c
      common /new/ plunit, numof, rmfo1(ntype*2), rmfo2(ntype*2),
     1             deglon, elev, obsyrs, mancom(3), planam(ntype*2),
     1             manver
c
c     + + + VARIABLE DECLARATIONS + + +
c
      character*16 plunit
      character*51 planam
      character*60 mancom
      integer numof
      real obsyrs, deglon, elev, rmfo1, rmfo2, manver
c
c     + + + VARIABLE DEFINITIONS  + + +
c
c     mancom management file comments
c     manver: management file version number (> 98.3 has residue management enhancements)
c     planam: plant name
c     plunit: harvest units, not used in WEPP, used in SHELL
c     obsyrs: observed years from climate file
c     elev  : elevation of weather station
c     deglon: degrees longitude of weather station
c     deglat: degrees latitude of weather station
c     numof : number of coulters on tillage instrument
c     rmfo1 : tillage intensity value for fragile residues on rill areas (0-1)
c     rmfo2 : tillage intensity value for non-fragile residues
c             on rills. (0-1)