      subroutine impsvd(a,m,n,w,v)
c
c     + + + PURPOSE + + +
c
c     SR IMPSVD is an impoundment element singular value
c     decomposition subroutine.  It is utilized in
c     developing least squares fits for a variety of
c     functions. It is taken directly from the book
c     "Numerical Recipes in Fortran".
c
c     Called from: IMPINT
c
c     Copyright (C) 1986, 1992 Numerical Recipes Software
c
c     Permission is granted for use only within USDA Water Erosion
c     Prediction Project code
c
c     + + + KEYWORDS + + +
c
c     + + + PARAMETERS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
c
      integer m, n
      real a(100,5), w(5), v(5,5)
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      real rv1(100), x, g, scale, y, anorm, h, z, c, f, s
      integer i, j, jj, k, nm, l, its
c
c     + + + LOCAL DEFINITIONS + + +
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      g = 0.0
      scale = 0.0
      anorm = 0.0
c
      do 140 i = 1, n
        l = i + 1
        rv1(i) = scale * g
        g = 0.0
        s = 0.0
        scale = 0.0
c
        if (i.le.m) then
c
          do 10 k = i, m
            scale = scale + abs(a(k,i))
   10     continue
c
          if (scale.ne.0.0) then
c
            do 20 k = i, m
              a(k,i) = a(k,i) / scale
              s = s + a(k,i) * a(k,i)
   20       continue
c
            f = a(i,i)
            g = -sign(sqrt(s),f)
            h = f * g - s
            a(i,i) = f - g
c
            if (i.ne.n) then
              do 50 j = l, n
                s = 0.0
c
                do 30 k = i, m
                  s = s + a(k,i) * a(k,j)
   30           continue
c
                f = s / h
c
                do 40 k = i, m
                  a(k,j) = a(k,j) + f * a(k,i)
   40           continue
c
   50         continue
            end if
c
            do 60 k = i, m
              a(k,i) = scale * a(k,i)
   60       continue
c
          end if
        end if
c
        w(i) = scale * g
        g = 0.0
        s = 0.0
        scale = 0.0
c
        if ((i.le.m).and.(i.ne.n)) then
c
          do 70 k = l, n
            scale = scale + abs(a(i,k))
   70     continue
c
          if (scale.ne.0.0) then
c
            do 80 k = l, n
              a(i,k) = a(i,k) / scale
              s = s + a(i,k) * a(i,k)
   80       continue
c
            f = a(i,l)
            g = -sign(sqrt(s),f)
            h = f * g - s
            a(i,l) = f - g
c
            do 90 k = l, n
              rv1(k) = a(i,k) / h
   90       continue
c
            if (i.ne.m) then
c
              do 120 j = l, m
                s = 0.0
c
                do 100 k = l, n
                  s = s + a(j,k) * a(i,k)
  100           continue
c
                do 110 k = l, n
                  a(j,k) = a(j,k) + s * rv1(k)
  110           continue
c
  120         continue
            end if
c
            do 130 k = l, n
              a(i,k) = scale * a(i,k)
  130       continue
c
          end if
        end if
c
        anorm = max(anorm,(abs(w(i))+abs(rv1(i))))
c
  140 continue
c
      do 200 i = n, 1, -1
c
        if (i.lt.n) then
c
          if (g.ne.0.0) then
c
            do 150 j = l, n
              v(j,i) = (a(i,j)/a(i,l)) / g
  150       continue
c
            do 180 j = l, n
              s = 0.0
c
              do 160 k = l, n
                s = s + a(i,k) * v(k,j)
  160         continue
c
              do 170 k = l, n
                v(k,j) = v(k,j) + s * v(k,i)
  170         continue
c
  180       continue
c
          end if
c
          do 190 j = l, n
            v(i,j) = 0.0
            v(j,i) = 0.0
  190     continue
c
        end if
c
        v(i,i) = 1.0
        g = rv1(i)
        l = i
  200 continue
c
      do 270 i = n, 1, -1
        l = i + 1
        g = w(i)
c
        if (i.lt.n) then
c
          do 210 j = l, n
            a(i,j) = 0.0
  210     continue
        end if
c
        if (g.ne.0.0) then
          g = 1.0 / g
c
          if (i.ne.n) then
c
            do 240 j = l, n
              s = 0.0
c
              do 220 k = l, m
                s = s + a(k,i) * a(k,j)
  220         continue
c
              f = (s/a(i,i)) * g
c
              do 230 k = i, m
                a(k,j) = a(k,j) + f * a(k,i)
  230         continue
c
  240       continue
c
          end if
c
          do 250 j = i, m
            a(j,i) = a(j,i) * g
  250     continue
c
        else
c
          do 260 j = i, m
            a(j,i) = 0.0
  260     continue
c
        end if
c
        a(i,i) = a(i,i) + 1.0
  270 continue
c
      do 390 k = n, 1, -1
c
        do 370 its = 1, 30
c
          do 280 l = k, 1, -1
            nm = l - 1
            if ((abs(rv1(l))+anorm).eq.anorm) go to 320
            if ((abs(w(nm))+anorm).eq.anorm) go to 290
  280     continue
c
  290     c = 0.0
c
          s = 1.0
c
          do 310 i = l, k
            f = s * rv1(i)
            rv1(i) = c * rv1(i)
            if ((abs(f)+anorm).eq.anorm) go to 320
            g = w(i)
            h = sqrt(f*f+g*g)
            w(i) = h
            h = 1.0 / h
            c = (g*h)
            s = -(f*h)
c
            do 300 j = 1, m
              y = a(j,nm)
              z = a(j,i)
              a(j,nm) = (y*c) + (z*s)
              a(j,i) = -(y*s) + (z*c)
  300       continue
c
  310     continue
c
  320     z = w(k)
c
          if (l.eq.k) then
c
            if (z.lt.0.0) then
              w(k) = -z
c
              do 330 j = 1, n
                v(j,k) = -v(j,k)
  330         continue
c
            end if
c
            go to 380
          end if
c
cdcf      Comment out following line - this line will cause model
cdcf      to potentially stop, with no graceful exit.
cdcf      Instead - alter to print a screen Error message
cdcf      dcf - 6/1/2000
cdcf      if (its.eq.30) pause 'No convergence in 30 iterations'
c
          if (its.ge.30) write(6,1000)
c
          x = w(l)
          nm = k - 1
          y = w(nm)
          g = rv1(nm)
          h = rv1(k)
          f = ((y-z)*(y+z)+(g-h)*(g+h)) / (2.0*h*y)
          g = sqrt(f*f+1.0)
          f = ((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h)) / x
          c = 1.0
          s = 1.0
c
          do 360 j = l, nm
            i = j + 1
            g = rv1(i)
            y = w(i)
            h = s * g
            g = c * g
            z = sqrt(f*f+h*h)
            rv1(j) = z
            c = f / z
            s = h / z
            f = (x*c) + (g*s)
            g = -(x*s) + (g*c)
            h = y * s
            y = y * c
c
            do 340 jj = 1, n
              x = v(jj,j)
              z = v(jj,i)
              v(jj,j) = (x*c) + (z*s)
              v(jj,i) = -(x*s) + (z*c)
  340       continue
c
            z = sqrt(f*f+h*h)
            w(j) = z
c
            if (z.ne.0.0) then
              z = 1.0 / z
              c = f * z
              s = h * z
            end if
c
            f = (c*g) + (s*y)
            x = -(s*g) + (c*y)
c
            do 350 jj = 1, m
              y = a(jj,j)
              z = a(jj,i)
              a(jj,j) = (y*c) + (z*s)
              a(jj,i) = -(y*s) + (z*c)
  350       continue
c
  360     continue
c
          rv1(l) = 0.0
          rv1(k) = f
          w(k) = x
c
  370   continue
c
  380   continue
c
  390 continue
c
 1000 format(/,' ***ERROR***',' In subroutine IMPSVD', 
     1       ' no convergence in 30 iterations',/)
      return
      end
