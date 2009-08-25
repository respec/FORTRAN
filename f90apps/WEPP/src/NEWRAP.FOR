      subroutine   newrap(
     i                    est,tlrnce,
     m                    func,newval)
c
c     + + + PURPOSE + + +
c     This subroutine uses the Newton-Raphson method to determine
c     'newval' using func (the function subprogram specified in the
c     calling statement).
c
c     Called from FURADV and FURREC
c     Author(s): E. R. Kottwitz
c     Reference in User Guide: Chapter 12
c
c     Changes:
c
c     Version: This module recoded from WEPP Version 93.06.
c     Date recoded: 07/07/93.
c     Recoded by: Charles R. Meyer.
c
c
c     + + + ARGUMENT DECLARATIONS + + +
      real      est,func,newval,tlrnce
c
c     + + + ARGUMENT DEFINITIONS + + +
c     est    - estimate of function's root coming in
c     func   - external function subprogram to be used
c     newval - function's root returned to calling subprogram
c     tlrnce - tolerance value for Newton-Raphson iterations with units
c              that are the same as the function's root
c
c     + + + LOCAL VARIABLES + + +
      integer   countr
      real      oldval
c
c     + + + LOCAL DEFINITIONS + + +
c     countr - counter of iterations used for non-convergence
c     oldval - old estimate of function's root
c
c     + + + OUTPUT FORMATS + + +
 2010 format(' SR NEWRAP did not converge in 100 iterations.  Model'/
     1       ' will continue with best estimate of parameter value.'/)
c
c     + + + END SPECIFICATIONS + + +
c
c     Initialize variables
c
      oldval = est
      countr = 1
c
c     Iteration loop
c
   20 newval = oldval-func(oldval)
      if(abs(newval-oldval).gt.tlrnce)then
        if(countr.lt.100)then
          oldval = newval
          countr = countr+1
          goto 20
        else
          write (6,2010)
        endif
      endif
c
      return
      end
