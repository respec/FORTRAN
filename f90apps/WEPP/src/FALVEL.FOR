      function falvel(spg,dia)
c
c******************************************************************
c                                                                 *
c     Called from subroutine param.                               *
c     Returns the fall velocity for a specific c particle given   *
c     its specific gravity (spg), diameter (dia), the kinematic   *
c     viscosity of the water (kinvis), and the acceleration       *
c     of gravity (accgav).                                        *
c                                                                 *
c******************************************************************
c
c
c******************************************************************
c                                                                 *
c   Arguments                                                     *
c     spg    -                                                    *
c     dia    -                                                    *
c                                                                 *
c******************************************************************
c
      real spg, dia
c
c******************************************************************
c                                                                 *
c   Common Blocks                                                 *
c                                                                 *
c******************************************************************
c
      include 'cconsta.inc'
c
      include 'cfall.inc'
c
c******************************************************************
c                                                                 *
c   Local Variables                                               *
c     rtsid  :                                                    *
c     rey    :                                                    *
c     i      :                                                    *
c                                                                 *
c******************************************************************
c
      real rtsid, rey, falvel
      integer i
c
c ... dimensionless parameter (rtsid=cd*rey*rey) for drag coeffient
c
      rtsid = ((spg-1.0)*accgav*(dia**3)/(kinvis**2)) * (8.0/6.0)
c
c     ... compute falvel from tabled values for larger particles
c
      if (rtsid.ge.0.024) then
        rtsid = alog(rtsid)
        do 10 i = 2, 9
          if (cdre2(i).gt.rtsid) then
            rey = exp((rtsid-cdre2(i-1))/(cdre2(i)-cdre2(i-1))*(
     1          cdre(i)-cdre(i-1))+cdre(i-1))
            falvel = rey * kinvis / dia
            return
          end if
   10   continue
c
c       -- trap values which are larger than table values
c
        write (6,*) ' cdre2 out of range.  spg=', spg, '   dia=', dia
        falvel = exp(cdre(9)) * kinvis / dia
c
c     ... falvel using Stokes solution for spheres for small particles
c
      else
        falvel = ((dia**2)*(spg-1.0)*(accgav)) / (kinvis*18.0)
      end if
c
      return
c
      end
