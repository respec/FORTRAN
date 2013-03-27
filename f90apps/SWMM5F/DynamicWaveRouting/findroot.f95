module findroot
!-----------------------------------------------------------------------------
!   findroot.c
!
!   Finds solution of func(x) = 0 using either the Newton-Raphson
!   method or Ridder's Method.
!   Based on code from Numerical Recipes in C (Cambridge University
!   Press, 1992).
!
!   Date:     2/22/07
!   Author:   L. Rossman
!-----------------------------------------------------------------------------
!
!#include <math.h>
!#include "findroot.h"

!#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a)) !fortran has same function SIGN
!#define MAXIT 60

contains

integer function findroot_Newton(x1, x2, rts, xacc, aFunc)
                     !void (*func) (double x, double* f, double* df)
!
!  Using a combination of Newton-Raphson and bisection, find the root of a
!  function func bracketed between x1 and x2. The root, returned in rts,
!  will be refined until its accuracy is known within +/-xacc. func is a
!  user-supplied routine, that returns both the function value and the first
!  derivative of the function. The function returns the number of function
!  evaluations used or 0 if the maximum allowed iterations were exceeded.
!
! NOTES:
! 1. The calling program must insure that the signs of func(x1) and func(x2)
!    are not the same, otherwise x1 and x2 do not bracket the root.
! 2. If func(x1) > func(x2) then the order of x1 and x2 should be
!    switched in the call to Newton.
!
    use headers
    implicit none
    
    double precision, intent(in) :: x1, x2, xacc
    double precision, intent(inout) :: rts
    
    interface AFunc1
      subroutine aFunc (x, f, df)
         double precision, intent(in) :: x
         double precision, intent(inout) :: f, df
      end subroutine aFunc
    end interface AFunc1

    integer, parameter :: MAXIT = 60
    double precision :: df, dx, dxold, f, x
    double precision :: mTemp, xhi, xlo
    integer :: j, n
    j = 0
    n = 0

    ! Initialize the "stepsize before last" and the last step.
    x = rts
    xlo = x1
    xhi = x2
    dxold = abs(x2-x1)
    dx = dxold
    call aFunc(x, f, df)
    n = n + 1

    ! Loop over allowed iterations.
    do j=1, MAXIT
        ! Bisect if Newton out of range or not decreasing fast enough.
        if ( ( ( (x-xhi)*df-f)*((x-xlo)*df-f) >= 0.0 .or. &
          &(abs(2.0*f) > abs(dxold*df) ) ) ) then
            dxold = dx
            dx = 0.5*(xhi-xlo)
            x = xlo + dx
            if ( xlo == x ) exit
        ! Newton step acceptable. Take it.
        else
            dxold = dx
            dx = f/df
            mTemp = x
            x = x - dx
            if ( mTemp == x ) exit
        end if

        ! Convergence criterion.
        if ( abs(dx) < xacc ) exit
 
        ! Evaluate function. Maintain bracket on the root.
        call aFunc(x, f, df)
        n = n + 1
        if ( f < 0.0 ) then 
           xlo = x
        else
           xhi = x
        end if
    end do
    rts = x
    if ( n <= MAXIT) then
       findroot_Newton = n
    else 
       findroot_Newton = 0
    end if
end function findroot_Newton

double precision function findroot_Ridder(x1, x2, xacc, func) !double (*func)(double)

    use headers
    implicit none
    double precision, intent(in) :: x1, x2, xacc
    interface AFunc
      function func (y)
         double precision :: func
         double precision, intent(in) :: y
      end function func
    end interface AFunc
    
    integer :: j, MAXIT
    double precision :: ans, fhi, flo, fm, fnew, s, xhi, xlo, xm, xnew, lval
    MAXIT = 60

    flo = func(x1)
    fhi = func(x2)
    if ( abs(flo-0.0) < P_TINY ) then
       findroot_Ridder = x1
       return
    end if
    if ( abs(fhi-0.0) < P_TINY ) then
       findroot_Ridder = x2
       return
    end if
    ans = 0.5*(x1+x2)
    if ( (flo > 0.0 .and. fhi < 0.0) .or. (flo < 0.0 .and. fhi > 0.0) ) then
        xlo = x1
        xhi = x2
        do j=1, MAXIT
            xm = 0.5*(xlo + xhi)
            fm = func(xm)
            s = sqrt( fm*fm - flo*fhi )
            if (abs(s-0.0) < P_TINY) then
               findroot_Ridder = ans
               return
            end if
            !(flo >= fhi ? 1.0 : -1.0)
            if (flo >= fhi)  then
               lval = 1.0
            else
               lval = -1.0
            end if
            xnew = xm + (xm-xlo)*( lval *fm/s )
            if ( abs(xnew - ans) <= xacc ) exit !break
            ans = xnew
            fnew = func(ans)
            if ( SIGN(fm, fnew) /= fm) then
                xlo = xm
                flo = fm
                xhi = ans
                fhi = fnew
            else if ( SIGN(flo, fnew) /= flo ) then
                xhi = ans
                fhi = fnew
            else if ( SIGN(fhi, fnew) /= fhi) then
                xlo = ans
                flo = fnew
            else 
                findroot_Ridder = ans
                return
            end if
            if ( abs(xhi - xlo) <= xacc ) then
                findroot_Ridder = ans
                return
            end if
        end do
        
        findroot_Ridder = ans
        return
    end if
    findroot_Ridder = -1.e20
end function findroot_Ridder

end module