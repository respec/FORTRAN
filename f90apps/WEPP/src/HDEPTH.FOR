      real function hdepth(t2,x)
c
c     + + + PURPOSE + + +
c     Finds depth H on an overland flow element by solving the
c     partial differential equation:
c
c       h1+alpha*m*h**(m-1)*h2=s ,t and x non-negative    (1)
c
c    where:
c
c      h1    =  partial of h with respect to t,
c      h2    =  partial of h with respect to x,
c      alpha =  chezy depth-discharge coefficient,
c      m     =  chezy depth-discharge exponent,
c      s     =  step function with ns initial non-zero steps that
c               is zero after the last step, and h is zero when
c               either t or x is zero.
c
c XXX -- I cannot determine the structure of the equation being
c        discussed, from these comments. -- CRM 6/3/91.
c    Let phi(t) be psi(t,0), where psi(t,u) is defined to be the
c    integral from u to t with respect to l of the m-1th power of
c    ***the integral of s from time u to l.***
c XXX -- With respect to what variable?  S? TIME? -- CRM 6/3/91.
c    Psi(t,u) strictly decreases from
c    phi(t) to 0 for fixed t and for u between 0 and ***am1n(t,tstar)***
c XXX -- What does this mean? -- CRM 6/3/91.
c    (s>0 before tstar,s=0 after);  let psiinv(t,-) be the inverse
c    of psi(t,-). Then the general solution to (1) is given by:
c
c    for x >= x1, h(t,x)=integral of s from 0 to t             (2)
c    for x <= x1, h(t,x)=integral of s from psiinv(t,x2) to t  (3)
c
c    where x1 = alpha*m*phi(t) and x2 = x/(alpha*m).
c
c    Subroutine phi(-) computes phi, subroutine psiinv(t,-)
c    computes the inverse of psi(t,-), and subroutine sint(t)
c    computes the integral of s from 0 to t.
c
c
c     Called from HDRIVE
c     Author(s): Shirley, Stone
c     Reference in User Guide:
c
c     Changes:
c          1) SAVE, which saves all applicable local variables,
c             deleted.
c          2) Common block PASS never used.  It was de-referenced.
c          3) No parameters were used.  Their include files were
c             dereferenced.
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/24/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real t2, x
c
c     + + + ARGUMENT DEFINITIONS + + +
c     t2     - current time
c     x      - length of equivalent plane
c
c     + + + COMMON BLOCKS + + +
c
      include 'cconsts.inc'
c       read: a1
c
c     + + + LOCAL VARIABLES + + +
      real xx
c
c     + + + LOCAL DEFINITIONS + + +
c     xx     - X/A1
c
c     + + + FUNCTION DECLARATIONS + + +
c     real phi, sint, psiinv
      real phi, psiinv
      double precision sint
c
c     + + + END SPECIFICATIONS + + +
c
      xx = x / a1
c
      if (xx.lt.phi(t2)) then
        hdepth = sint(t2) - sint(psiinv(t2,xx))
      else
        hdepth = sint(t2)
      end if
c
c     Following line inserted 12/11/92 to prevent hdepth from becoming
c     a negative number for test data set 45     dcf
c
      if (hdepth.lt.0.0) hdepth = 0.0
c
      return
      end
