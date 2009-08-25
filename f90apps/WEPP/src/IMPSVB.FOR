      subroutine impsvb(u,w,v,m,n,b,x)
c
c     + + + PURPOSE + + +
c
c     SR IMPSVB is an impoundment element singular value
c     backsubstitution subroutine.  It is utilized in
c     developing least squares fits for a variety of
c     functions.  It is taken directly from the book
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
      real b(100), u(100,5), v(5,5), w(5), x(5)
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      integer i, j, jj
      real s, tmp(500)
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
      do 20 j = 1, n
        s = 0.0
c
        if (w(j).ne.0.) then
c
          do 10 i = 1, m
            s = s + u(i,j) * b(i)
   10     continue
c
          s = s / w(j)
        end if
c
        tmp(j) = s
c
   20 continue
c
      do 40 j = 1, n
        s = 0.0
c
        do 30 jj = 1, n
          s = s + v(j,jj) * tmp(jj)
   30   continue
c
        x(j) = s
   40 continue
c
      return
      end
