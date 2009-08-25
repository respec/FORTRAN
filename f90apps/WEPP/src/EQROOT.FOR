      subroutine eqroot(a,err,eqrt)
c
c     + + + PURPOSE + + +
c     Solves the following equation for U: 1 - exp(-u) = a*u, for
c     positive values of A less than 1, and positive values of U.
c     (If A=1, U=0).  Newton's method is used, with special
c     approximations for small values of A (A <= 0.06),  and large
c     values of A (A >= 0.999).
c
c     Results appear to be accurate to machine precision (real*4)
c     and require at most 2 iterations.
c
c     Called from DBLEX.
c     Author(s): Lane, Shirley, Lopez, and Stone
c     Reference in User Guide:
c
c     Changes:
c         1) Changed from a function to a subroutine, since it passes
c            more than one value back.
c         2) Comments re: small & large values of A, massively changed.
c         3) Since same value of A was observed repeatedly being passed
c            to routine, a check for this was added.
c
c        ******************************************************
c        *  NOTE:  THIS INDICATES THAT SOMETHING IS *WRONG*   *
c        *         THE VALUE OF 'A' SHOULD CHANGE EACH TIME!  *
c        ******************************************************
c
c         4) Most of the double precision calculations & comparisons
c            reduced to single precision.  However, 'E' must remain
c            double precision.
c         5) ax has been moved to common block ceqrot.inc jca2  8/31/93
c
c     Version: This module recoded from WEPP version 91.10.
c     Date recoded: 04/11/91 to 4/22/91.
c     Recoded by: Charles R. Meyer.
c
c     + + + KEYWORDS + + +
c
c     + + + ARGUMENT DECLARATIONS + + +
      real a, eqrt
      integer err
c
c     + + + ARGUMENT DEFINITIONS + + +
c     a      - constant A in the equation:  1 - exp(-u) = a*u.
c              (In this routine, A = 1/ip.)
c     err    - flag. 0: equation solved.
c                    1: no solution for given A.
c     eqrt   - solution returned for U in the equation:
c                    1 - exp(-u) = a*u.
c
c     + + + COMMON BLOCKS + + +
      include 'ceqrot.inc'
c
c     + + + LOCAL VARIABLES + + +
      double precision d, e, f, r, s, u, tmpvr1
      real eqrtx
      integer loopfg
c
c     + + + LOCAL DEFINITIONS + + +
c     d      - A - F
c     e      - exp(-U)
c     f      - (1-E)/U
c     r      - A/TMPVR1
c     s      - abs(D/A) or abs(D/TMPVR1)
c     u      - See definition of A
c     eqrtx  - saved value of EQRT from prev. call
c     loopfg - Flag.  Set to exit loop.
c
c     + + + SAVES + + +
      save eqrtx
c
c     + + + DATA INITIALIZATIONS + + +
c
c     + + + END SPECIFICATIONS + + +
c
c
c******************************************************************
c
c     FOR SMALL VALUES OF 'A' (A <= 0.06):
c
c       If a = (1 - exp(-u))/u,
c     then a = 1/u - exp(-u)/u,
c      and a = 1/u - 1/(u*exp(u)).
c
c   Thus, for a large value of U (suppose U >= 15), the value of the
c   last term; ie, 1/(u*exp(u)), gets very close to zero.  In this
c   situation, "a = 1/u" is a good approximation of the original
c   equation.  (From this equation, when U is large, A is small.)
c
c       When a = 1/u,  u=1/a.
c
c   For A <= .06, the error in U computed by this approach appears
c   to be less than real*4 precision.
c
c******************************************************************
c
c     FOR LARGE VALUES OF 'A' (A >= 0.999):
c
c   The Taylor's series expansion with a remainder for exp(-u) is:
c
c       exp(-u) = 1 - u + u**2/2 - u**3/6 + r(u),
c       where "r(u)" represents the remainder of the terms in the series.
c
c   Substituting in the equation:  a = 1/u - exp(-u)/u, one gets:
c
c       a = 1/u - (1 - u + u**2/2 - u**3/6 + r(u)) / u,
c
c   which simplifies to:
c
c       a = 1 - u/2 + u**2/6 - r(u)/u.
c
c   Since  0 < r(u) < u**4/24, then   0 < r(u)/u < u**3/24.  (When
c   a=0.9991, u=0.0018.  As A gets larger, U gets smaller still.)
c   Assuming "r(u)/u" to be negligible, and solving for U yields:
c
c       u = 3/2 +/- sqrt(6a - 15/4).
c
c   For .999 <= a, the u appears to be less than real*4 precision.
c
c******************************************************************
c
c     FOR ORDINARY VALUES OF 'A' (0.06 > A > 0.999):
c
c Between .06 and .999, Newton's method is used, using starting values.
c Let f(u1)=a and u near u1.  |(a - f(u))/a|  =  |(f(u1) - f(u))/a|  =
c |f'(c)*u1/a|*|(u1 - u)/u1|, for some c between u and u1.   Since u is
c near u1, c and u1 are approximated by u. Thus for r=|a/(f'(u)*u)|, we
c have |(u - u1)/u1| ~ r*|(a - f(u))/a|. The relative error in f and  u
c are small if the relative error in f and r times this error is small.
c Evaluating f'(u) gives r = a/((u - 1)*f(u) - 1).
c
c Because of the approximations in the above,  we use 1/2 the smallest
c real*4 number in testing for convergence.
c
c******************************************************************
c
c     See if value of A has changed since last call to EQROOT.
c
c      *** L0 IF ***
      if (a.ne.ax) then
c
c       Verify that A is within the valid range of values; ie,
c       0.0 < A <= 1.0.
c
c       *** L1 IF ***
        if ((a.gt.0.0).and.(a.le.1.0)) then
c
c         Small A:  0 < A <= 0.06.  (Answer good to machine precision).
c
c         *** L2 IF ***
          if (a.le.0.06) then
            err = 0
            eqrt = 1.0 / a
c
c         Usual Case:  0.06 < A < 0.999.
c
          else if (a.lt.0.999) then
c
c           Estimate starting value for U.
c
            if (a.le.0.2) then
              u = 1.0 / a
            else if (a.le.0.5) then
              u = .968732 / a - 1.55098 * a + .431653
            else if (a.le.0.94) then
              u = 1.13243 / a - .928240 * a - .207111
            else
              u = (3.0/2.0) - sqrt(6.0*a-(15.0/4.0))
            end if
c
c           Iterate.
c
c           *** Begin L3 LOOP ***
            loopfg = 0
   10       continue
            e = exp(-u)
            f = (1.d0-e) / u
            d = a - f
            tmpvr1 = ((u+1.d0)*f-1.d0)
            r = a / tmpvr1
c
            if (r.le.1.d0) then
              s = abs(d/a)
            else
              s = abs(d/tmpvr1)
            end if
c
            if (s.ge.0.59e-6) then
              u = u * (1.0+d/(e-f))
            else
              loopfg = 1
            end if
c
c           *** End L3 LOOP ***
            if (loopfg.eq.0) go to 10
c
c           Exit with solution.
c
c20         err=0
            err = 0
            eqrt = sngl(u)
c
c         Large A: 0.999 <= A < 1. (Answer good to about 10 places).
c
          else if (a.lt.1.0) then
            err = 0
            eqrt = (3.0/2.0) - sqrt(6.0*a-(15.0/4.0))
c
c         Special Case: A=1 (exact limiting solution).
c
          else
            err = 0
            eqrt = 0.
c
c         *** L2 ENDIF ***
          end if
c
c         Save values of A & EQRT.
c
c         AXCCCC not used  12-16-93 02:03pm  sjl
c
c         axcccc = a
          eqrtx = eqrt
c
c       Error: A outside range.  A <= 0  or  A > 1.
c
c       *** L1 ELSE ***
        else
          err = 1
c
c       *** L1 ENDIF ***
        end if
c
c     Value of 'A' same as last time EQROOT was called.
c
c     *** L0 ELSE ***
      else
        eqrt = eqrtx
c
c     *** L0 ENDIF ***
      end if
c
      return
      end
