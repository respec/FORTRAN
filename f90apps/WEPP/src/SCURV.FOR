      subroutine scurv(x1,x2,x3,x4,x5,x6)
c
c     + + + PURPOSE + + +
c     Computes constants for an S-curve given the (X,Y) coordinates
c     of points represented by: (x3,x1), (x4,x2).
c
c     Implements a modification of EPIC equation 2.254, in the form:
c
c                              tmin
c          y = 100 * -------------------------   ;   tmin < -1.0;
c                     tmin + exp(a + b * tmin)
c
c
c   where:
c          the "100" is the upper limit (asymptote) of the curve,
c          "tmin" is the minimum daily temperature (Celsius), and
c          "a" and "b" are the constants computed by SCURV.
c
c      The "a" & "b" returned by SCURVE (x5 & x6) force the curve through
c      the points: (x3, x1) and (x4, x2); ie, (tmin1, y1) and (tmin2, y2).
c      The "100" is a scaling factor which alters the curvature *between*
c      those points.
c
c     FOR EXAMPLE:
c        For TMIN's of -5 and -15, with associated biomass loss
c        fractions of 0.01 and 0.95 respectively,
c        setting x1=0.01, x2=0.95, x3=5.0, x4=15.0
c        yields: x5=12.55, x6=0.3465 as values for "a" & "b" respectively.
c
c     Note: The values entered for TMIN's are the ABSOLUTE VALUES
c           of the actual TMIN's; ie, they are POSITIVE NUMBERS.
c           (TMIN is constrained to be less than -1 degrees Celsius.)
c           THIS SHOULD BE CHANGED IN "SCURV" AND IN "PTGRP" AND
c           "RANGE".
c
c
c     Called from RANGE, PTGRP, and PRGRA.
c     Author(s): Williams, Arnold, Weltz, Ferris, Meyer
c     Reference in User Guide:
c
c     Changes:
c
c     Version: This module recoded from WEPP version 92.25.
c     Date recoded: 07/02/92.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real x1, x2, x3, x4, x5, x6
c
c     + + + ARGUMENT DEFINITIONS + + +
c     x1     - frost damage fraction for 1st point.
c     x2     - frost damage fraction for 2nd point.
c     x3     - temperature associated with 1st point.
c     x4     - temperature associated with 2nd point.
c     x5     - 1st constant computed by SCURV.
c     x6     - 2nd constant computed by SCURV.
c
c     + + + LOCAL VARIABLES + + +
      real xx, yy
c
c     + + + END SPECIFICATIONS + + +
c
c
      xx = alog(x3/(x1/100.)-x3)
      yy = alog(x4/(x2/100.)-x4)
      x6 = ((xx-yy)/(x4-x3))
      x5 = (xx+x3*x6)
      return
      end
