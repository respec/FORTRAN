      subroutine imppol(ndata,x,y,a)
c
c     + + + PURPOSE + + +
c
c     SR IMPPOL is an impoundment element subroutine used to
c     develop polynomial fit coefficients for a 4th degree
c     polynomial.   It is based upon the "Numerical Recipes
c     in Fortran" SR SVDFIT.
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
      integer ndata
      real x(100), y(100), a(5)
c
c     + + + ARGUMENT DEFINITIONS + + +
c
c     a(i)  - the fit coefficients returned to SR IMPINT
c     ndata - # of points to be fitted
c     x(i)  - stage values from SR IMPINT
c     y(i)  - discharge values from SR IMPINT
c
c     + + + COMMON BLOCKS + + +
c
c     + + + LOCAL VARIABLES + + +
c
      integer ma, mp, np, i, j, jf
      real chisq, u(100,5), v(5,5), w(5), tol, sum, thresh, wmax,
     1    afunc(5), b(100)
c
c     + + + LOCAL DEFINITIONS + + +
c
c     Variable definitions (note:  this is a canned subroutine
c     only the varibles entering and exiting are defined)
c
c     afunc(i) - the transformed x values
c     ma       - number of x values in the function (5)
c     mp       - number of pairs of points included in the regression
c     np       - set to ma
c     u(i,j)   - transformed matrix of x values
c
c     + + + SAVES + + +
c
c     + + + SUBROUTINES CALLED + + +
c
c     impsvb
c     impsvd
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
      ma = 5
      mp = ndata
      np = ma
      tol = 1.e-5
c
c     defining afunc and u
c
      do 30 i = 1, ndata
        afunc(1) = 1.0
c
        do 10 jf = 2, 5
          afunc(jf) = afunc(jf-1) * x(i)
   10   continue
c
        do 20 j = 1, ma
          u(i,j) = afunc(j)
   20   continue
c
        b(i) = y(i)
   30 continue
c
      call impsvd(u,ndata,ma,w,v)
c
      wmax = 0.0
c
      do 40 j = 1, ma
        if (w(j).gt.wmax) wmax = w(j)
   40 continue
c
      thresh = tol * wmax
c
      do 50 j = 1, ma
        if (w(j).lt.thresh) w(j) = 0.
   50 continue
c
      call impsvb(u,w,v,ndata,ma,b,a)
c
      chisq = 0.0
c
      do 80 i = 1, ndata
        afunc(1) = 1.0
c
        do 60 jf = 2, 5
          afunc(jf) = afunc(jf-1) * x(i)
   60   continue
c
        sum = 0.0
c
        do 70 j = 1, ma
          sum = sum + a(j) * afunc(j)
   70   continue
c
        chisq = chisq + (y(i)-sum) ** 2
   80 continue
c
      return
      end
