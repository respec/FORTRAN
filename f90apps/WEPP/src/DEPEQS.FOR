      subroutine depeqs(xu,cdep,a,b,phi,theta,x,depeq)
c
c
c     + + + PURPOSE + + +
c     Solves the deposition equation.
c
c     Called from subroutine DEPOS.
c     Author(s): G. Foster, M. Nearing
c     Reference in User Guide:
c
c     Changes: 1) Put the second parameter at the END of the list,
c                 since it gets MODIFIED.  Made the analogous changes
c                 in DEPOS which calles it.
c              2) Made this FUNCTION DEPEQ into a SUBROUTINE DEPEQS,
c                 since it modifies one of its arguments, and accesses
c                 common blocks.
c              3) PROUTE1.INC de-referenced, since MXPLAN is never used.
c
c     Version: This module recoded from WEPP version 90.92.
c     Date recoded: 01/11/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real xu, cdep, a, b, phi, theta, x, depeq
c
c     + + + ARGUMENT DEFINITIONS + + +
c     xu     - nondimensional distance on an OFE where current
c              deposition region begins
c     cdep   -
c     a      - shear stress coefficient for current slope section
c     b      - shear stress coefficient for current slope section
c     phi    - nondimensional deposition parameter for current OFE
c     theta  - nondimensional interrill detachment parameter for
c              current OFE
c     x      -
c     depeq  -
c
c     + + + COMMON BLOCKS + + +
      include 'cends1.inc'
c       read: ktrato
      include 'cinfco1.inc'
c       read: qostar
c
c     + + + LOCAL VARIABLES + + +
      real ratio, expon, tmpvr1
c     ratio  - temp variable to store parts of the depeq equation
c     expon  - temp variable to store parts of the depeq equation
c
c     + + + SUBROUTINES CALLED + + +
c     undflo
c
c     + + + END SPECIFICATIONS + + +
c
      if (abs(qostar+x).lt.10e-8) x = -qostar - 0.000001
c
      ratio = (xu+qostar) / (x+qostar)
c     if(ratio.gt.1.0) ratio=1.0
      if (qostar.ge.0.0.and.ratio.gt.1.0) ratio = 1.0
c
      expon = 1.0 + phi
      call undflo(ratio,expon)
c
      tmpvr1 = 2.0 * a * ktrato
      depeq = (tmpvr1*phi*(x+qostar)/(2.0+phi)) + (phi/(1.0+phi)) * (b*
     1    ktrato-theta-(tmpvr1*qostar)) + cdep * ratio ** expon
c
      return
      end
