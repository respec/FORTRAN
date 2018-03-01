module forcemain
!-----------------------------------------------------------------------------
!   forcemain.c
!
!   Project:  EPA SWMM5
!   Version:  5.0
!   Date:     6/19/07   (Build 5.0.010)
!             2/4/08    (Build 5.0.012)
!   Author:   L. Rossman
!
!   Special Non-Manning Force Main functions
!-----------------------------------------------------------------------------
!#define _CRT_SECURE_NO_DEPRECATE
!
!#include <math.h>
!#include "headers.h"
integer, parameter :: dp = kind(1.d0)
integer, parameter :: K4 = selected_int_kind(4) !kind= 2

!-----------------------------------------------------------------------------
!  Constants
!-----------------------------------------------------------------------------
real(kind=dp), parameter :: VISCOS = 1.1E-5   ! Kinematic viscosity of water
!                                       ! @ 20 deg C (sq ft/sec)

!-----------------------------------------------------------------------------
!  External functions (declared in funcs.h)
!-----------------------------------------------------------------------------
! forcemain_getEquivN
! forcemain_getRoughFactor
! forcemain_getFricSlope

!-----------------------------------------------------------------------------
!  Local functions
!-----------------------------------------------------------------------------
!static double forcemain_getFricFactor(double e, double hrad, double re)
!static double forcemain_getReynolds(double v, double hrad)
contains
!=============================================================================

real(kind=dp) function forcemain_getEquivN(j, k)
!
!  Input:   j = link index
!           k = conduit index
!  Output:  returns an equivalent Manning's n for a force main
!  Purpose: computes a Mannng's n that results in the same normal flow
!           value for a force main flowing full under fully turbulent
!           conditions using either the Hazen-Williams or Dary-Weisbach
!           flow equations.
!
    use headers
    implicit none
    integer, intent(in) :: j
    integer(kind=K4), intent(in) :: k
    
    real(kind=dp) :: lVal
    
    !TXsect xsect = arrLink(j)%xsect
    real(kind=dp) :: f, d
    d = arrLink(j)%xsect%yFull
    select case ( ForceMainEqn )
      case (H_W)
        forcemain_getEquivN = 1.067 / arrLink(j)%xsect%rBot * ((d/Conduit(k)%slope) ** 0.04)
        return
      case (D_W)
        lVal = 1.0e12
        f = forcemain_getFricFactor(arrLink(j)%xsect%rBot, d/4.0, lVal)
        forcemain_getEquivN = sqrt(f/185.0) * (d ** (1./6.))
        return
    end select
    forcemain_getEquivN = Conduit(k)%roughness
end function forcemain_getEquivN
!
!=============================================================================

real(kind=dp) function forcemain_getRoughFactor(j, lengthFactor)
!
!  Input:   j = link index
!           lengthFactor = factor by which a pipe will be artifically lengthened
!  Output:  returns a roughness adjustment factor for a force main
!  Purpose: computes an adjustment factor for a force main that compensates for
!           any artificial lengthening the pipe may have received.
!
    use headers
    implicit none
    integer, intent(in) :: j
    real(kind=dp), intent(in) :: lengthFactor
    !TXsect xsect = arrLink(j)%xsect
    real(kind=dp) :: r
    select case ( ForceMainEqn )
      case (H_W)
        r = 1.318 * arrLink(j)%xsect%rBot * (lengthFactor ** 0.54)
        forcemain_getRoughFactor = GRAVITY / (r ** 1.852)
        return
      case (D_W)
        forcemain_getRoughFactor = 1.0/8.0/lengthFactor
        return
    end select
    forcemain_getRoughFactor = 0.0
end function forcemain_getRoughFactor
!
!=============================================================================

real(kind=dp) function forcemain_getFricSlope(j, v, hrad)
!
!  Input:   j = link index
!           v = flow velocity (ft/sec)
!           hrad = hydraulic radius (ft)
!  Output:  returns a force main pipe's friction slope 
!  Purpose: computes the headloss per unit length used in dynamic wave
!           flow routing for a pressurized force main using either the
!           Hazen-Williams or Darcy-Weisbach flow equations.
!  Note:    the pipe's roughness factor was saved in xsect.sBot in
!           conduit_validate() in LINK.C.
!
    use headers
    implicit none
    integer, intent(in) :: j
    real(kind=dp), intent(in) :: v, hrad
    real(kind=dp) :: re, f
    !type(TXsect) :: xsect = arrLink(j)%xsect
    select case ( ForceMainEqn )
      case (H_W)
        forcemain_getFricSlope = arrLink(j)%xsect%sBot * (v ** 0.852) / (hrad ** 1.1667)                 !(5.0.012 - LR)
        return
      case (D_W)
        re = forcemain_getReynolds(v, hrad)
        f = forcemain_getFricFactor(arrLink(j)%xsect%rBot, hrad, re)
        forcemain_getFricSlope = f * arrLink(j)%xsect%sBot * v / hrad
        return
    end select
    forcemain_getFricSlope = 0.0
end function forcemain_getFricSlope
!
!=============================================================================

real(kind=dp) function forcemain_getReynolds(v, hrad)
!
!  Input:   v = flow velocity (ft/sec)
!           hrad = hydraulic radius (ft)
!  Output:  returns a flow's Reynolds Number
!  Purpose: computes a flow's Reynolds Number
!
    use headers
    implicit none
    real(kind=dp), intent(in) :: v, hrad
    forcemain_getReynolds = 4.0 * hrad * v / VISCOS
end function forcemain_getReynolds 
!    
!=============================================================================

real(kind=dp) recursive function forcemain_getFricFactor(e, hrad, re) result(f)
!
!  Input:   e = roughness height (ft)
!           hrad = hydraulic radius (ft)
!           re = Reynolds number
!  Output:  returns a Darcy-Weisbach friction factor
!  Purpose: computes the Darcy-Weisbach friction factor for a force main
!           using the Swamee and Jain approximation to the Colebrook-White
!           equation.
!
    use headers
    implicit none
    
    real(kind=dp), intent(in) :: e, hrad, re
    real(kind=dp) :: mre
    mre = re
    if ( mre < 10.0 ) mre = 10.0
    if ( mre <= 2000.0 ) then
       f = 64.0 / mre
    else if ( mre < 4000.0 ) then
        f = forcemain_getFricFactor(e, hrad, 4000.0d00)
        f = 0.032 + (f - 0.032) * ( mre - 2000.0) / 2000.0
    else
        f = e/3.7/(4.0*hrad)
        if ( mre < 1.0e10 ) f = f + 5.74/(mre ** 0.9)
        f = log10(f)
        f = 0.25 / f / f
    end if
end function forcemain_getFricFactor
!
end module