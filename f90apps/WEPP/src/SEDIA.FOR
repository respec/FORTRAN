      function sedia(spg,eqfall)
c*******************************************************************
c                                                                  *
c  This function calculates the equivalent sand diameter of a      *
c  particle class. It called by SR PRTCMP.                         *
c                                                                  *
c*******************************************************************
c                                                                  *
c  Argumants                                                       *
c     spg                                                          *
c     eqfall                                                       *
c                                                                  *
c*******************************************************************
c
c*******************************************************************
c                                                                  *
c  Common Blocks                                                   *
c                                                                  *
c*******************************************************************
c
      include 'cconsta.inc'
c
      include 'cfall.inc'

      real eqfall, rey, rtsid, sedia, spg
      integer i
c
c*******************************************************************
c                                                                  *
c  Local Variables                                                 *
c    rtsid :                                                       *
c    i     :                                                       *
c    rey   :                                                       *
c    sedia :                                                       *
c                                                                  *
c*******************************************************************
c
      rtsid = 1.3333 * accgav * (spg-1.0) * kinvis / (eqfall**3)
      if (rtsid.le.2.0e+06) then
        rtsid = alog(rtsid)
        do 10 i = 1, 9
          if (cddre(i).lt.rtsid) then
            rey = exp((rtsid-cddre(i-1))/(cddre(i)-cddre(i-1))*(
     1          cdre(i)-cdre(i-1))+cdre(i-1))
            sedia = rey * kinvis / eqfall
            return
          end if
   10   continue
c
        sedia = exp(cdre(9)) * kinvis / eqfall
        return
c
c     ** Sediment diameter using Stokes equation for small spheres. **
c
      else
        sedia = sqrt(18.0*eqfall*kinvis/((spg-1.0)*accgav))
      end if
c
      return
      end
