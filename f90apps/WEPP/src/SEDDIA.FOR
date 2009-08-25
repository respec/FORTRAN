      real function seddia(spg,vs,kinvis,agrav)
c
c     + + + PURPOSE + + +
c
c     Function SEDDIA is the CREAMS sediment diameter routine.
c
c     Called from: SR CHNPAR
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
c     + + + ARGUMENT DECLARATIONS + + +
c
      real spg, vs, kinvis, agrav
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     spg    -
c     vs     -
c     kinvis -
c     agrav  -
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      real reb(9), cddb(9), re(9), cddre(9), rey, rtsid
      integer ifrus, itrat, j
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Real Variables
c
c     reb(9)   -
c     cddb(9)  -
c     re(9)    -
c     cddre(9) -
c     rey      -
c     rtsid    -
c
c     Integer Variables
c
c     ifrus -
c     itrat -
c     j     -
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     table
c
c     + + + DATA INITIALIZATIONS + + +
c
      data reb /0.001, 0.01, 0.1, 1.0, 10.0, 100.0, 1000.0, 10000.0,
     1    100000.0/
c
      data cddb /2.05e+07, 2.05e+05, 2.05e+03, 24.0, 0.41, 1.05e-02,
     1    4.3e-04, 4.0e-05, 4.4e-06/
c
c     + + + END SPECIFICATIONS + + +
c
c
      do 10 j = 1, 9
        re(j) = alog(reb(j))
        cddre(j) = alog(cddb(j))
   10 continue
c
      rtsid = 1.3333 * agrav * (spg-1.0) * kinvis / (vs**3)
c
      if (rtsid.le.2.0e+06) then
        rtsid = alog(rtsid)
        ifrus = 3
        itrat = 9
        call table(ifrus,itrat,re,cddre,rtsid,rey)
        rey = exp(rey)
        seddia = rey * kinvis / vs
        return
      end if
c
      seddia = sqrt(18.0*vs*kinvis/((spg-1.0)*agrav))
c
      return
      end
