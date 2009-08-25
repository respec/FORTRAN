      subroutine imppro
c
c     + + + PURPOSE + + +
c
c     Channel water surface profile analysis subroutine.
c     SR IMPPRO uses the standard step method to obtain
c     the water surface profile and energy slopes at each
c     node in the channel.
c
c     Called from: IMPINT
c     Author(s): A. Fogel
c     Reference in User Guide:
c
c     Version:
c     Date recoded:
c     Recoded by:
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
      include 'cimrou.inc'
c     read: b,ss,n,pypos,z,mdwit,dwt,rfrac,yc,frt,ncross
c     write: depth
c
c     + + + LOCAL VARIABLES + + +
c
      real area, hrad, cntrdp, v, vhead, theadc, y1, y2, hr23, r23, s1,
     1    chr, dz, yy1, yy2, dy, ddx, stage, thead1, s2, sfavg, floss,
     1    thdif, hhead, thead2, g, b, frt, ss, z(30), n, ydif
      integer iup, icsec, numnp1, nod, i, iter
c
c     + + + LOCAL DEFINITIONS + + +
c
c     area     - the flow area of the channel
c     b        - channel bottom width
c     chr      - channel roughness coefficient
c     cntrdp   - depth at the control section
c     ddx      - distance downstream
c     dy       - incremental change in depth
c     dz       - incremental change in downstream distance
c     floss    - friction loss
c     frt      - flow rate
c     g        - gravitational constant
c     hhead    - head variable
c     hr23     - hydraulic radius ** 2/3
c     hrad     - hydraulic radius
c     icsec    - number of cross sections
c     iter     - number of iterations
c     iup      - upstream routing flag
c     n        - manning's n
c     nod      - normal depth counter
c     numnp1   - number of points in cross section
c     r23      - 2/3 in manning's n equation
c     s1       - friction slope
c     s2       - friction slope
c     sfavg    - friction slope
c     ss       - side slopes
c     stage    - stage of the flow depth
c     thdif    - convergence head difference
c     thead1   - total head
c     thead2   - total head
c     theadc   - total head
c     v        - average flow velocity
c     vhead    - velocity head
c     y1       - flow depth
c     y2       - flow depth
c     ydif     - flow depth difference
c     yy1      - control section position
c     yy2      - control section position + 1
c     z(i)     - horizontal location
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c     calculate the hydraulic radius for the control section
c
      b = bwes
      ss = sses
      n = nes
c
      do 10 i = 1, ncross
        z(i) = zes(i)
   10 continue
c
      frt = qesr
      iup = 0
      cntrdp = yc
      g = 32.2
c
      if (iup.eq.0) then
        icsec = ncross
        depth(icsec) = cntrdp
        area = b * depth(icsec) + ss * depth(icsec) ** 2
        hrad = area / (b+2.*depth(icsec)*(ss**2+1)**0.5)
      else
        icsec = 1
        depth(icsec) = cntrdp
        area = b * depth(icsec) + ss * depth(icsec) ** 2
        hrad = area / (b+2.*depth(icsec)*(ss**2+1)**0.5)
      end if
c
c     calculate the velocity head and the total head for the control
c     section
c
      v = frt / area
      vhead = v * v / (2.0*g)
      theadc = pypos(icsec) + depth(icsec) + vhead
      y1 = depth(icsec)
      y2 = y1
c
c     calculate the friction slope using manning's equation
c
      r23 = 2.0 / 3.0
      chr = n / 1.486
      hr23 = hrad ** r23
      s1 = (v*chr/hr23) ** 2.0
c
c     begin loop over the cross sections and set up point to change depth
c     toward normal depth
c
      numnp1 = ncross - 1
c
      do 30 nod = 1, numnp1
c
        if (iup.eq.0) then
          icsec = ncross - nod
        else
          icsec = nod + 1
        end if
c
c       calculate the distance downstream (or upstream)
c
        if (iup.eq.0) then
          dz = z(icsec+1) - z(icsec)
          yy1 = pypos(icsec)
          yy2 = pypos(icsec+1)
          dy = yy2 - yy1
          ddx = sqrt(dy*dy+dz*dz)
        else
          dz = z(icsec) - z(icsec-1)
          yy1 = pypos(icsec-1)
          yy2 = pypos(icsec)
          dy = yy2 - yy1
          ddx = sqrt(dy*dy+dz*dz)
        end if
c
c       set first guess at the stage to that of the previous cross section
c       and set the depth equal to that of the previous cross section
c
        iter = 0
c
   20   iter = iter + 1
c
        if (iter.gt.mdwit) then
          write (6,1000) iter, mdwit, icsec
          stop
        end if
c
        stage = pypos(icsec) + y2
        y1 = y2
c
c       calculate the cross sectional area, hydraulic radius, and velocity
c       and total head at the cross section
c
        area = b * y1 + ss * y1 ** 2.
        hrad = area / (b+2.*y1*(ss**2.+1)**0.5)
c
        v = frt / area
        vhead = v * v / (2.0*g)
        thead1 = stage + vhead
c
c       calculate the friction slope using manning's equation
c
        hr23 = hrad ** r23
        s2 = (v*chr/hr23) ** 2.0
c
c       calculate the average friction slope over the reach and
c       the friction loss
c
        sfavg = 0.5 * (s1+s2)
        floss = sfavg * ddx
c
c       calculate the new total head
c
        if (iup.eq.0) then
          thead2 = theadc + floss
        else
          thead2 = theadc - floss
        end if
c
c       check for convergence
c
        thdif = thead2 - thead1
c
        if (abs(thdif).lt.dwt) then
          theadc = thead2
          s1 = s2
          depth(icsec) = thead2 - vhead - pypos(icsec)
          y2 = yc
          go to 30
c
        else
c
          hhead = 0.5 * (thead1+thead2)
c
c         y2 = y1*(thead2/thead1)
c
          y2 = hhead - pypos(icsec) - vhead
          ydif = y2 - y1
c
          if (abs(ydif).gt.rfrac*y1) then
            if (ydif.ge.0.0) then
              y2 = y1 + rfrac * y1
            else
              y2 = y1 - rfrac * y1
            end if
          end if
c
          go to 20
        end if
c
   30 continue
c
      return
c
 1000 format (//12x,'!!!!!!!!!!!!!!     E R R O R     !!!!!!!!!!!!!!',//
     1    7x,'WATER SURFACE PROFILE ITERATIONS (',i3,
     1    ') EXCEEDED MAXIMUM (',i3,')',/28x,'PROGRAM HALTED'/19x,
     1    'AT CROSS SECTION ',i2//12x,
     1    '!!!!!!!!!!!!!!     E R R O R     !!!!!!!!!!!!!!')
      end
