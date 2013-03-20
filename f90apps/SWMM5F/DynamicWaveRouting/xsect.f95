module modXsect
!-----------------------------------------------------------------------------
!   xsect.c
!
!   Project:  EPA SWMM5
!   Version:  5.0
!   Date:     6/19/07   (Build 5.0.010)
!             2/4/08    (Build 5.0.012)
!             1/21/09   (Build 5.0.014)
!             04/20/11  (Build 5.0.022)
!   Author:   L. Rossman
!
!   Cross section geometry functions.
!
!   The primary functions are:
!      getAofY   -- returns area given depth
!      getWofY   -- returns top width given depth
!      getRofY   -- returns hyd. radius given depth
!      getYofA   -- returns flow depth given area
!      getRofA   -- returns hyd. radius given area
!      getSofA   -- returns section factor given area
!      getAofS   -- returns area given section factor
!      getdSdA   -- returns derivative of section factor w.r.t. area
!   where
!      Y = flow depth
!      A = flow area
!      R = hyd. radius
!      S = section factor = A*R^(2/3)
!-----------------------------------------------------------------------------
!use DataSizeSpecs
use headers
!private K2, K4, K8
!integer, parameter :: K2 = selected_int_kind(2) !kind= 1
!integer, parameter :: K4 = selected_int_kind(4) !kind= 2
!integer, parameter :: K8 = selected_int_kind(8)  !kind =4

!-----------------------------------------------------------------------------
! Constants
!-----------------------------------------------------------------------------
double precision, parameter :: RECT_ALFMAX       =0.97
double precision, parameter :: RECT_TRIANG_ALFMAX=0.98
double precision, parameter :: RECT_ROUND_ALFMAX =0.98

! Ratio of area at max. flow to full area
! (= 1.0 for open shapes, < 1.0 for closed shapes)
double precision, dimension(25) ::  Amax = (/ &
                    &1.0,    &!  DUMMY
                    &0.9756, &!  CIRCULAR
                    &0.9756, &!  FILLED_CIRCULAR
                    &0.97,   &!  RECT_CLOSED
                    &1.0,    &!  RECT_OPEN
                    &1.0,    &!  TRAPEZOIDAL
                    &1.0,    &!  TRIANGULAR
                    &1.0,    &!  PARABOLIC
                    &1.0,    &!  POWERFUNC
                    &0.98,   &!  RECT_TRIANG
                    &0.98,   &!  RECT_ROUND
                    &0.96,   &!  MOD_BASKET
                    &0.96,   &!  HORIZ_ELLIPSE
                    &0.96,   &!  VERT_ELLIPSE
                    &0.92,   &!  ARCH
                    &0.96,   &!  EGGSHAPED
                    &0.96,   &!  HORSESHOE
                    &0.96,   &!  GOTHIC
                    &0.98,   &!  CATENARY
                    &0.98,   &!  SEMIELLIPTICAL
                    &0.96,   &!  BASKETHANDLE
                    &0.96,   &!  SEMICIRCULAR
                    &1.0,    &!  IRREGULAR
                    &0.96,   &!  CUSTOM                                       !(5.0.010 - LR)
                    &0.9756 /)!  FORCE_MAIN                                   !(5.0.010 - LR)
                    
!-----------------------------------------------------------------------------
!  Shared variables
!-----------------------------------------------------------------------------
double precision, save ::  Sstar                 ! section factor 
type(TXsect), save :: Xstar                ! pointer to a cross section object        
double precision, save ::  Qcritical            ! critical flow

!-----------------------------------------------------------------------------
!  External functions (declared in funcs.h)
!-----------------------------------------------------------------------------
!  xsect_isOpen
!  xsect_setParams
!  xsect_setIrregXsectParams
!  xsect_setCustomXsectParams                                                 !(5.0.010 - LR)
!  xsect_getAmax
!  xsect_getSofA
!  xsect_getYofA
!  xsect_getRofA
!  xsect_getAofS
!  xsect_getdSdA
!  xsect_getAofY
!  xsect_getRofY
!  xsect_getWofY
!  xsect_getYcrit

contains
logical function xsect_isOpen(xtype)
!
!  Input:   type = type of xsection shape
!  Output:  returns 1 if xsection is open, 0 if not
!  Purpose: determines if a xsection type is open or closed.
!
    use headers
    implicit none
    integer(kind=K2), intent(in) :: xtype
    if (Amax(xtype) >= 1.0) then
       xsect_isOpen = .true.
    else
       xsect_isOpen = .false.
    end if
    return
end function xsect_isOpen

!=============================================================================

logical function xsect_setParams(xsect, datatype, p, aUcf)
!
!  Input:   xsect = ptr. to a cross section data structure
!           type = xsection shape type
!           p() = vector or xsection parameters
!           aUcf = units correction factor
!  Output:  returns .true. if successful, .false. if not
!  Purpose: assigns parameters to a cross section's data structure.
!
    use consts
    use enums
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(inout) :: xsect
    integer, intent(in) :: datatype
    double precision, dimension(:), intent(inout) :: p
    double precision, intent(in) :: aUcf
    
    integer :: index
    double precision :: maMax, theta

    double precision :: Rfull_Arch, Afull_Arch, Wmax_Arch, Yfull_Arch
    double precision :: MinorAxis_Ellipse, MajorAxis_Ellipse, Afull_Ellipse, Rfull_Ellipse
    
    if ( datatype /= DUMMY .and. p(1) <= 0.0 ) then
        xsect_setParams = .false.
        return
    end if
    xsect%datatype  = datatype
    select case ( xsect%datatype )
    case (DUMMY)
        xsect%yFull = P_TINY
        xsect%wMax  = P_TINY
        xsect%aFull = P_TINY
        xsect%rFull = P_TINY
        xsect%sFull = P_TINY
        xsect%sMax  = P_TINY
        !break

    case (CIRCULAR)
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = xsect%yFull
        xsect%aFull = PI / 4.0 * xsect%yFull * xsect%yFull
        xsect%rFull = 0.2500 * xsect%yFull
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = 1.08 * xsect%sFull
        !break

    case (FORCE_MAIN)                                                           !(5.0.010 - LR)
        xsect%yFull = p(1)/aUcf                                               !(5.0.010 - LR)
        xsect%wMax  = xsect%yFull                                           !(5.0.010 - LR)
        xsect%aFull = PI / 4.0 * xsect%yFull * xsect%yFull                 !(5.0.010 - LR)
        xsect%rFull = 0.2500 * xsect%yFull                                  !(5.0.010 - LR)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (0.63))                 !(5.0.010 - LR)
        xsect%sMax  = 1.06949 * xsect%sFull                                 !(5.0.010 - LR)

        ! --- save C-factor or roughness in rBot position                     !(5.0.010 - LR)
        xsect%rBot  = p(2)                                                   !(5.0.010 - LR)
        !break                                                                 !(5.0.010 - LR)

    case (FILLED_CIRCULAR)
        if ( p(2) >= p(1) ) then
            xsect_setParams = .false.
            return
        end if

        ! --- initially compute full values for unfilled pipe
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = xsect%yFull
        xsect%aFull = PI / 4.0 * xsect%yFull * xsect%yFull
        xsect%rFull = 0.2500 * xsect%yFull
        
        ! --- find:
        !     yBot = depth of filled bottom
        !     aBot = area of filled bottom
        !     sBot = width of filled bottom
        !     rBot = wetted perimeter of filled bottom
        xsect%yBot  = p(2)/aUcf
        xsect%aBot  = circ_getAofY(xsect, xsect%yBot)
        xsect%sBot  = xsect_getWofY(xsect, xsect%yBot)
        xsect%rBot  = xsect%aBot / (xsect%rFull * lookup(xsect%yBot/xsect%yFull, xs_R_Circ, N_R_Circ))

        ! --- revise full values for filled bottom
        xsect%aFull = xsect%aFull - xsect%aBot
        xsect%rFull = xsect%aFull / (PI*xsect%yFull - xsect%rBot + xsect%sBot)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = 1.08 * xsect%sFull
        xsect%yFull = xsect%yFull - xsect%yBot
        !break

    case (EGGSHAPED)
        xsect%yFull = p(1)/aUcf
        xsect%aFull = 0.5105 * xsect%yFull * xsect%yFull
        xsect%rFull = 0.1931 * xsect%yFull
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = 1.065 * xsect%sFull
        xsect%wMax  = 2./3. * xsect%yFull
        !break

    case (HORSESHOE)
        xsect%yFull = p(1)/aUcf
        xsect%aFull = 0.8293 * xsect%yFull * xsect%yFull
        xsect%rFull = 0.2538 * xsect%yFull
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = 1.077 * xsect%sFull
        xsect%wMax  = 1.0 * xsect%yFull
        !break

    case (GOTHIC)
        xsect%yFull = p(1)/aUcf
        xsect%aFull = 0.6554 * xsect%yFull * xsect%yFull
        xsect%rFull = 0.2269 * xsect%yFull
        xsect%sFull = xsect%aFull * (xsect%rFull ** ( 2./3.))
        xsect%sMax  = 1.065 * xsect%sFull
        xsect%wMax  = 0.84 * xsect%yFull
        !break

    case (CATENARY)
        xsect%yFull = p(1)/aUcf
        xsect%aFull = 0.70277 * xsect%yFull * xsect%yFull
        xsect%rFull = 0.23172 * xsect%yFull
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = 1.05 * xsect%sFull
        xsect%wMax  = 0.9 * xsect%yFull
        !break

    case (SEMIELLIPTICAL)
        xsect%yFull = p(1)/aUcf
        xsect%aFull = 0.785 * xsect%yFull * xsect%yFull
        xsect%rFull = 0.242 * xsect%yFull
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = 1.045 * xsect%sFull
        xsect%wMax  = 1.0 * xsect%yFull
        !break

    case (BASKETHANDLE)
        xsect%yFull = p(1)/aUcf
        xsect%aFull = 0.7862 * xsect%yFull * xsect%yFull
        xsect%rFull = 0.2464 * xsect%yFull
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = 1.06078 * xsect%sFull
        xsect%wMax  = 0.944 * xsect%yFull
        !break

    case (SEMICIRCULAR)
        xsect%yFull = p(1)/aUcf
        xsect%aFull = 1.2697 * xsect%yFull * xsect%yFull
        xsect%rFull = 0.2946 * xsect%yFull
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = 1.06637 * xsect%sFull
        xsect%wMax  = 1.64 * xsect%yFull
        !break

    case (RECT_CLOSED)
        if ( p(2) <= 0.0 ) then
           xsect_setParams = .false.
           return
        end if
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = p(2)/aUcf
        xsect%aFull = xsect%yFull * xsect%wMax
        xsect%rFull = xsect%aFull / (2.0 * (xsect%yFull + xsect%wMax))
        xsect%sFull = xsect%aFull * (xsect%rFull ** ( 2./3.))
        maMax = RECT_ALFMAX * xsect%aFull
        xsect%sMax = maMax * (rect_closed_getRofA(xsect, maMax) ** ( 2./3.))
        !break

    case (RECT_OPEN)
        if ( p(2) <= 0.0 ) then
            xsect_setParams = .false.
            return
        end if
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = p(2)/aUcf
        xsect%aFull = xsect%yFull * xsect%wMax
        xsect%rFull = xsect%aFull / (2.0 * xsect%yFull + xsect%wMax)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = xsect%sFull
        !break

    case (RECT_TRIANG)
        if ( p(2) <= 0.0 .or. p(3) <= 0.0 ) then
            xsect_setParams = .false.
            return
        end if
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = p(2)/aUcf
        xsect%yBot  = p(3)/aUcf

        ! --- area of bottom triangle
        xsect%aBot  = xsect%yBot * xsect%wMax / 2.0

        ! --- slope of bottom side wall
        xsect%sBot  = xsect%wMax / xsect%yBot / 2.0

        ! --- length of side wall per unit of depth
        xsect%rBot  = sqrt( 1. + xsect%sBot * xsect%sBot )   

        xsect%aFull = xsect%wMax * (xsect%yFull - xsect%yBot / 2.0)
        xsect%rFull = xsect%aFull / (2.0 * xsect%yBot * xsect%rBot + 2.0 * &
                       &(xsect%yFull - xsect%yBot) + xsect%wMax)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        maMax = RECT_TRIANG_ALFMAX * xsect%aFull
        xsect%sMax  = maMax * (rect_triang_getRofA(xsect, maMax) ** (2./3.))
        !break

    case (RECT_ROUND)
        if ( p(2) <= 0.0 ) then
           xsect_setParams = .false. !(5.0.014 -LR)
           return
        end if
        if ( p(3) < p(2)/2.0 ) p(3) = p(2)/2.0                                !(5.0.014 - LR)
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = p(2)/aUcf
        xsect%rBot  = p(3)/aUcf

        ! --- angle of circular arc
        theta = 2.0 * asin(xsect%wMax / 2.0 / xsect%rBot)

        ! --- area of circular bottom
        xsect%aBot  = xsect%rBot * xsect%rBot / 2.0 * (theta - sin(theta)) 

        ! --- section factor for circular bottom
        xsect%sBot  = PI * xsect%rBot * xsect%rBot * ((xsect%rBot/2.0) ** (2./3.))

        ! --- depth of circular bottom
        xsect%yBot  = xsect%rBot * (1.0 - cos(theta/2.0))

        xsect%aFull = xsect%wMax * (xsect%yFull - xsect%yBot) + xsect%aBot
        xsect%rFull = xsect%aFull / (xsect%rBot * theta + 2.0 * &
                       &(xsect%yFull - xsect%yBot) + xsect%wMax)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        maMax = RECT_ROUND_ALFMAX * xsect%aFull
        xsect%sMax = maMax * (rect_round_getRofA(xsect, maMax) ** (2./3.))
        !break

    case (MOD_BASKET)

!! --- The code below was modified to accommodate a more                     !(5.0.014 - LR)
!!     general type of modified baskethandle cross-section. 

        if ( p(2) <= 0.0 ) then
           xsect_setParams = .false.
           return
        end if
        if ( p(3) < p(2)/2.0 ) p(3) = p(2)/2.0
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = p(2)/aUcf

        ! --- radius of circular arc
        xsect%rBot = p(3)/aUcf

        ! --- angle of circular arc
        theta = 2.0 * asin(xsect%wMax / 2.0 / xsect%rBot)
        xsect%sBot = theta

        ! --- height of circular arc
        xsect%yBot = xsect%rBot * (1.0 - cos(theta/2.0))

        ! --- area of circular arc
        xsect%aBot = xsect%rBot * xsect%rBot / 2.0 * (theta - sin(theta)) 

        ! --- full area
        xsect%aFull = (xsect%yFull - xsect%yBot) * xsect%wMax + xsect%aBot

        ! --- full hydraulic radius & section factor
        xsect%rFull = xsect%aFull / (xsect%rBot * theta + 2.0 * &
                       &(xsect%yFull - xsect%yBot) + xsect%wMax)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))

        ! --- area corresponding to max. section factor
        xsect%sMax = xsect_getSofA(xsect, Amax(MOD_BASKET)*xsect%aFull)
        !break
       
    case (TRAPEZOIDAL)
        if ( p(2) < 0.0 .or. p(3) < 0.0 .or. p(4) < 0.0 ) then
            xsect_setParams = .false.
            return
        end if 
        xsect%yFull = p(1)/aUcf

        ! --- bottom width
        xsect%yBot = p(2)/aUcf

        ! --- avg. slope of side walls
        xsect%sBot  = ( p(3) + p(4) )/2.0

        ! --- length of side walls per unit of depth
        xsect%rBot  = sqrt( 1.0 + p(3)*p(3) ) + sqrt( 1.0 + p(4)*p(4) )

        ! --- top width
        xsect%wMax = xsect%yBot + xsect%yFull * (p(3) + p(4))

        xsect%aFull = ( xsect%yBot + xsect%sBot * xsect%yFull ) * xsect%yFull
        xsect%rFull = xsect%aFull / (xsect%yBot + xsect%yFull * xsect%rBot)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = xsect%sFull
        !break

    case (TRIANGULAR)
        if ( p(2) <= 0.0 ) then
           xsect_setParams  = .false.
           return
        end if
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = p(2)/aUcf

        ! --- slope of side walls
        xsect%sBot  = xsect%wMax / xsect%yFull / 2.

        ! --- length of side wall per unit of depth
        xsect%rBot  = sqrt( 1. + xsect%sBot * xsect%sBot )  

        xsect%aFull = xsect%yFull * xsect%yFull * xsect%sBot
        xsect%rFull = xsect%aFull / (2.0 * xsect%yFull * xsect%rBot)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = xsect%sFull
        !break

    case (PARABOLIC)
        if ( p(2) <= 0.0 ) then
            xsect_setParams = .false.
            return
        end if
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = p(2)/aUcf

        ! --- rBot :: 1/c^.5, where y = c*x^2 is eqn. of parabolic shape
        xsect%rBot  = xsect%wMax / 2.0 / sqrt(xsect%yFull)

        xsect%aFull = (2./3.) * xsect%yFull * xsect%wMax
        xsect%rFull = xsect_getRofY(xsect, xsect%yFull)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = xsect%sFull
        !break

    case (POWERFUNC)
        if ( p(2) <= 0.0 .or. p(3) <= 0.0 ) then
            xsect_setParams = .false.
            return
        end if
        xsect%yFull = p(1)/aUcf
        xsect%wMax  = p(2)/aUcf
        xsect%sBot  = 1.0 / p(3)
        xsect%rBot  = xsect%wMax / (xsect%sBot + 1) / (xsect%yFull ** xsect%sBot)
        xsect%aFull = xsect%yFull * xsect%wMax / (xsect%sBot+1)
        xsect%rFull = xsect_getRofY(xsect, xsect%yFull)
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = xsect%sFull
        !break

    case (HORIZ_ELLIPSE)
        if ( abs(p(2) - 0.0) < P_TINY ) then           ! std. ellipse pipe
            index = int(floor(p(1))) - 1  ! size code
            if ( index < 0 .or. index >= NumCodesEllipse ) then
               xsect_setParams  = .false.
               return
            end if
            xsect%yFull = MinorAxis_Ellipse(index)/12.
            xsect%wMax  = MajorAxis_Ellipse(index)/12.
            xsect%aFull = Afull_Ellipse(index)
            xsect%rFull = Rfull_Ellipse(index)
        else
            ! --- length of minor axis
            xsect%yFull = p(1)/aUcf

            ! --- length of major axis
            if ( p(2) < 0.0 ) then
               xsect_setParams = .false.
               return
            end if
            xsect%wMax = p(2)/aUcf
            xsect%aFull = 1.2692 * xsect%yFull * xsect%yFull
            xsect%rFull = 0.3061 * xsect%yFull
        end if
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = xsect%sFull
        !break

    case (VERT_ELLIPSE)
        if ( abs(p(2) - 0.0) < P_TINY ) then                 ! std. ellipse pipe
            index = int(floor(p(1))) - 1        ! size code
            if ( index < 0 .or. index >= NumCodesEllipse ) then
               xsect_setParams = .false.
               return
            end if
            xsect%yFull = MajorAxis_Ellipse(index)/12.
            xsect%wMax  = MinorAxis_Ellipse(index)/12.
            xsect%aFull = Afull_Ellipse(index)
            xsect%rFull = Rfull_Ellipse(index)
        else
            ! --- length of major axis
            if ( p(2) < 0.0 ) then
               xsect_setParams = .false.
               return
            end if

            ! --- length of minor axis
            xsect%yFull = p(1)/aUcf
            xsect%wMax = p(2)/aUcf
            xsect%aFull = 1.2692 * xsect%wMax * xsect%wMax
            xsect%rFull = 0.3061 * xsect%wMax
        end if
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = xsect%sFull
        !break

    case (ARCH)
        if ( abs(p(2)-0.0) < P_TINY ) then                      ! std. arch pipe
            index = int(floor(p(1))) - 1        ! size code
            if ( index < 0 .or. index >= NumCodesArch ) then
                xsect_setParams = .false.
                return
            end if
            xsect%yFull = Yfull_Arch(index)/12.     ! Yfull units are inches
            xsect%wMax  = Wmax_Arch(index)/12.      ! Wmax units are inches
            xsect%aFull = Afull_Arch(index)
            xsect%rFull = Rfull_Arch(index)
        else                                     ! non-std. arch pipe
            if ( p(2) < 0.0 ) then
                xsect_setParams = .false.
                return
            end if
            xsect%yFull = p(1)/aUcf
            xsect%wMax  = p(2)/aUcf
            xsect%aFull = 0.7879 * xsect%yFull * xsect%wMax
            xsect%rFull = 0.2991 * xsect%yFull
        end if
        xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
        xsect%sMax  = xsect%sFull
        !break
    end select

    xsect_setParams = .true.
    return
end function xsect_setParams
!
!=============================================================================

subroutine xsect_setIrregXsectParams(xsect)
!
!  Input:   xsect = ptr. to a cross section data structure
!  Output:  none
!  Purpose: assigns transect parameters to an irregular shaped cross section.
!
   use headers
   implicit none
   type(TXsect), intent(inout) :: xsect
    integer :: index
    index = xsect%transect
    xsect%yFull = Transect(index)%yFull
    xsect%wMax  = Transect(index)%wMax
    xsect%aFull = Transect(index)%aFull
    xsect%rFull = Transect(index)%rFull
    xsect%sFull = xsect%aFull * (xsect%rFull ** (2./3.))
    xsect%sMax = Transect(index)%sMax
    xsect%aBot = Transect(index)%aMax
end subroutine xsect_setIrregXsectParams
!=============================================================================
!
subroutine xsect_setCustomXsectParams(xsect)                                 !(5.0.010 - LR)
!                                                                             !(5.0.010 - LR)
!  Input:   xsect = ptr. to a cross section data structure                    !(5.0.010 - LR)
!  Output:  none                                                              !(5.0.010 - LR)
!  Purpose: assigns parameters to a custom-shaped cross section.              !(5.0.010 - LR)
!                                                                             !(5.0.010 - LR)
   use headers
   implicit none
   type(TXsect), intent(inout) :: xsect                                                                            !(5.0.010 - LR)
    integer ::    index                            !(5.0.010 - LR)
    double precision :: yFull                                             !(5.0.010 - LR)
    index = Curve(xsect%transect)%refersTo                            !(5.0.010 - LR)
    yFull = xsect%yFull                                               !(5.0.010 - LR)
    xsect%wMax  = Shape(index)%wMax * yFull                                  !(5.0.010 - LR)
    xsect%aFull = Shape(index)%aFull * yFull * yFull                         !(5.0.010 - LR)
    xsect%rFull = Shape(index)%rFull * yFull                                 !(5.0.010 - LR)
    xsect%sFull = xsect%aFull * ((xsect%rFull) ** (2./3.))                    !(5.0.010 - LR)
    xsect%sMax  = Shape(index)%sMax * yFull * yFull * (yFull ** (2./3.))      !(5.0.010 - LR)
    xsect%aBot  = Shape(index)%aMax * yFull * yFull                          !(5.0.010 - LR)
end subroutine xsect_setCustomXsectParams                                                                              !(5.0.010 - LR)
!
!!=============================================================================
!
double precision function xsect_getAmax(xsect)
!
!  Input:   xsect = ptr. to a cross section data structure
!  Output:  returns area (ft2)
!  Purpose: finds xsection area at maximum flow depth.
!
   use headers
   implicit none
   type(TXsect), intent(inout) :: xsect
!/*
!    if ( xsect%type == MOD_BASKET )
!    {
!        return xsect%yBot*xsect%wMax +
!            PI / 4.0 * xsect%wMax * xsect%wMax * (0.96-0.5)
!    }
!    else
!*/
    if ( xsect%datatype == IRREGULAR ) then
       xsect_getAmax = xsect%aBot
    else if ( xsect%datatype == CUSTOM ) then
       xsect_getAmax = xsect%aBot                      !(5.0.010 - LR)
    else 
       xsect_getAmax = Amax(xsect%datatype) * xsect%aFull
    end if
end function xsect_getAmax
!
!=============================================================================

double precision function xsect_getSofA(xsect, a)
!
!  Input   xsect = ptr. to a cross section data structure
!           a = area (ft2)
!  Output  returns section factor (ft^(8/3))
!  Purpose computes xsection's section factor at a given area.
!
    use enums
    use consts
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    double precision :: alpha, r
    
    double precision :: xsect_getRofA
    double precision :: rect_triang_getSofA, rect_round_getSofA, rect_open_getSofA, rect_closed_getSofA
        
    
    alpha = a / xsect%aFull
    
    select case ( xsect%datatype )
      case (FORCE_MAIN, CIRCULAR)      !(5.0.01 - LR)
        xsect_getSofA = circ_getSofA(xsect, a)
        return

      case (EGGSHAPED)
        xsect_getSofA = xsect%sFull * lookup(alpha, xs_S_Egg, N_S_Egg)
        return
  
      case (HORSESHOE)
        xsect_getSofA = xsect%sFull * lookup(alpha, xs_S_Horseshoe, N_S_Horseshoe)
        return

      case (GOTHIC)
        xsect_getSofA = xsect%sFull * lookup(alpha, xs_S_Gothic, N_S_Gothic)
        return

      case (CATENARY)
        xsect_getSofA = xsect%sFull * lookup(alpha, xs_S_Catenary, N_S_Catenary)
        return

      case (SEMIELLIPTICAL)
        xsect_getSofA = xsect%sFull * lookup(alpha, xs_S_SemiEllip, N_S_SemiEllip)
        return

      case (BASKETHANDLE)
        xsect_getSofA = xsect%sFull * lookup(alpha, xs_S_BasketHandle, N_S_BasketHandle)
        return

      case (SEMICIRCULAR)
        xsect_getSofA = xsect%sFull * lookup(alpha, xs_S_SemiCirc, N_S_SemiCirc)
        return

      case (RECT_CLOSED)
        xsect_getSofA = rect_closed_getSofA(xsect, a)
        return

      case (RECT_OPEN)
        xsect_getSofA = rect_open_getSofA(xsect, a)
        return

      case (RECT_TRIANG)
        xsect_getSofA = rect_triang_getSofA(xsect, a)
        return

      case (RECT_ROUND)
        xsect_getSofA = rect_round_getSofA(xsect, a)
        return

      case default
        if (abs(a - 0.0) < P_TINY) then
           xsect_getSofA = 0.0
           return
        end if
        
        r = xsect_getRofA(xsect, a)
        if ( r < P_TINY ) then
           xsect_getSofA = 0.0
           return
        end if
        
        xsect_getSofA = a * (r ** (2./3.))
    end select
end function xsect_getSofA
!
!=============================================================================

double precision function xsect_getYofA(xsect, a)
!
!  Input)   xsect = ptr. to a cross section data structure
!           a = area (ft2)
!  Output)  returns depth (ft)
!  Purpose) computes xsection's depth at a given area.
!
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: a
    double precision :: lVal
    double precision :: alpha
    alpha = a / xsect%aFull
    
    select case ( xsect%datatype )
      case (FORCE_MAIN, CIRCULAR)                                     !(5.0.010 - LR)
          lVal = circ_getYofA(xsect, a)

      case (FILLED_CIRCULAR)
        lVal = filled_circ_getYofA(xsect, a)
  
      case (EGGSHAPED)
        lVal = xsect%yFull * lookup(alpha, xs_Y_Egg, N_Y_Egg)
  
      case (HORSESHOE)
        lVal = xsect%yFull * lookup(alpha, xs_Y_Horseshoe, N_Y_Horseshoe)

      case (GOTHIC)
        lVal = xsect%yFull * lookup(alpha, xs_Y_Gothic, N_Y_Gothic)

      case (CATENARY)
        lVal = xsect%yFull * lookup(alpha, xs_Y_Catenary, N_Y_Catenary)

      case (SEMIELLIPTICAL)
        lVal = xsect%yFull * lookup(alpha, xs_Y_SemiEllip, N_Y_SemiEllip)

      case (BASKETHANDLE)
        lVal = xsect%yFull * lookup(alpha, xs_Y_BasketHandle, N_Y_BasketHandle)

      case (SEMICIRCULAR)
        lVal = xsect%yFull * lookup(alpha, xs_Y_SemiCirc, N_Y_SemiCirc)

      case (HORIZ_ELLIPSE)
        lVal = xsect%yFull * invLookup(alpha, xs_A_HorizEllipse, N_A_HorizEllipse)

      case (VERT_ELLIPSE)
        lVal = xsect%yFull * invLookup(alpha, xs_A_VertEllipse, N_A_VertEllipse)

      case (IRREGULAR)
        lVal = xsect%yFull * invLookup(alpha, &
           &Transect(xsect%transect)%areaTbl, N_TRANSECT_TBL)

      case (CUSTOM)                                                             !(5.0.010 - LR)
        lVal = xsect%yFull * invLookup(alpha, &                               !(5.0.010 - LR)
           &Shape(Curve(xsect%transect)%refersTo)%areaTbl, N_SHAPE_TBL)      !(5.0.010 - LR)

      case (ARCH)
        lVal = xsect%yFull * invLookup(alpha, xs_A_Arch, N_A_Arch)

      case (RECT_CLOSED) 
              lVal = a / xsect%wMax

      case (RECT_TRIANG) 
              lVal = rect_triang_getYofA(xsect, a)

      case (RECT_ROUND)  
              lVal = rect_round_getYofA(xsect, a)

      case (RECT_OPEN)   
              lVal = a / xsect%wMax

      case (MOD_BASKET)  
              lVal = mod_basket_getYofA(xsect, a)

      case (TRAPEZOIDAL) 
              lVal = trapez_getYofA(xsect, a)

      case (TRIANGULAR)  
              lVal = triang_getYofA(xsect, a)

      case (PARABOLIC)   
              lVal = parab_getYofA(xsect, a)

      case (POWERFUNC)   
              lVal = powerfunc_getYofA(xsect, a)

      case default
          lVal = 0.0
    end select
    
    xsect_getYofA = lVal
end function xsect_getYofA
!
!!=============================================================================
!
double precision function xsect_getAofY(xsect, y)
!
!  Input:   xsect = ptr. to a cross section data structure
!           y = depth (ft)
!  Output:  returns area (ft2)
!  Purpose: computes xsection's area at a given depth.
!
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: y
    double precision :: Val
    double precision :: yNorm
!    double precision :: powerfunc_getAofY, parab_getAofY, triang_getAofY
!    double precision :: mod_basket_getAofY, rect_round_getAofY, rect_triang_getAofY, filled_circ_getAofY
    yNorm = y / xsect%yFull
    
    if ( y <= 0.0 ) then
      xsect_getAofY = 0.0
      return
    end if
    select case ( xsect%datatype )
      case (FORCE_MAIN, CIRCULAR)                                 !(5.0.010 - LR)
        Val = xsect%aFull * lookup(yNorm, xs_A_Circ, N_A_Circ)
      case (FILLED_CIRCULAR)
        Val = filled_circ_getAofY(xsect, y)
      case (EGGSHAPED)
        Val = xsect%aFull * lookup(yNorm, xs_A_Egg, N_A_Egg)
      case (HORSESHOE)
        Val = xsect%aFull * lookup(yNorm, xs_A_Horseshoe, N_A_Horseshoe)
      case (GOTHIC)
        Val = xsect%aFull * invLookup(yNorm, xs_Y_Gothic, N_Y_Gothic)
      case (CATENARY)
        Val = xsect%aFull * invLookup(yNorm, xs_Y_Catenary, N_Y_Catenary)
      case (SEMIELLIPTICAL)
        Val = xsect%aFull * invLookup(yNorm, xs_Y_SemiEllip, N_Y_SemiEllip)
      case (BASKETHANDLE)
        Val = xsect%aFull * lookup(yNorm, xs_A_Baskethandle, N_A_Baskethandle)
      case (SEMICIRCULAR)
        Val = xsect%aFull * invLookup(yNorm, xs_Y_SemiCirc, N_Y_SemiCirc)
      case (HORIZ_ELLIPSE)
        Val = xsect%aFull * lookup(yNorm, xs_A_HorizEllipse, N_A_HorizEllipse)
      case (VERT_ELLIPSE)
        Val = xsect%aFull * lookup(yNorm, xs_A_VertEllipse, N_A_VertEllipse)
      case (ARCH)
        Val = xsect%aFull * lookup(yNorm, xs_A_Arch, N_A_Arch)
      case (IRREGULAR)
        Val = xsect%aFull * lookup(yNorm, Transect(xsect%transect)%areaTbl, N_TRANSECT_TBL)
      case (CUSTOM)                                                             !(5.0.010 - LR)
        Val = xsect%aFull * lookup(yNorm, Shape(Curve(xsect%transect)%refersTo)%areaTbl, N_SHAPE_TBL)      !(5.0.010 - LR)
      case (RECT_CLOSED) 
        Val = y * xsect%wMax
      case (RECT_TRIANG) 
        Val = rect_triang_getAofY(xsect, y)
      case (RECT_ROUND)  
        Val = rect_round_getAofY(xsect, y)
      case (RECT_OPEN)   
        Val = y * xsect%wMax
      case (MOD_BASKET)  
        Val = mod_basket_getAofY(xsect, y)
      case (TRAPEZOIDAL) 
        Val = trapez_getAofY(xsect, y)
      case (TRIANGULAR)  
        Val = triang_getAofY(xsect, y)
      case (PARABOLIC)   
        Val = parab_getAofY(xsect, y)
      case (POWERFUNC)   
        Val = powerfunc_getAofY(xsect, y)
      case default
        Val = 0.0
    end select
    xsect_getAofY = Val
end function xsect_getAofY
!
!!=============================================================================
!
double precision function xsect_getWofY(xsect, y)
!
!  Input:   xsect = ptr. to a cross section data structure
!           y = depth ft)
!  Output:  returns top width (ft)
!  Purpose: computes xsection's top width at a given depth.
!

    use enums
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    double precision :: val 
    double precision :: yNorm
    yNorm = y / xsect%yFull
    
    select case ( xsect%datatype )
      case (FORCE_MAIN,CIRCULAR)                                                        !(5.0.010 - LR)
        val = xsect%wMax * lookup(yNorm, xs_W_Circ, N_W_Circ)
      case (FILLED_CIRCULAR)
        yNorm = (y + xsect%yBot) / (xsect%yFull + xsect%yBot)
        val = xsect%wMax * lookup(yNorm, xs_W_Circ, N_W_Circ)
      case (EGGSHAPED)
        val = xsect%wMax * lookup(yNorm, xs_W_Egg, N_W_Egg)
      case (HORSESHOE)
        val = xsect%wMax * lookup(yNorm, xs_W_Horseshoe, N_W_Horseshoe)
      case (GOTHIC)
        val = xsect%wMax * lookup(yNorm, xs_W_Gothic, N_W_Gothic)
      case (CATENARY)
        val = xsect%wMax * lookup(yNorm, xs_W_Catenary, N_W_Catenary)
      case (SEMIELLIPTICAL)
        val = xsect%wMax * lookup(yNorm, xs_W_SemiEllip, N_W_SemiEllip)
      case (BASKETHANDLE)
        val = xsect%wMax * lookup(yNorm, xs_W_BasketHandle, N_W_BasketHandle)
      case (SEMICIRCULAR)
        val = xsect%wMax * lookup(yNorm, xs_W_SemiCirc, N_W_SemiCirc)
      case (HORIZ_ELLIPSE)
        val = xsect%wMax * lookup(yNorm, xs_W_HorizEllipse, N_W_HorizEllipse)
      case (VERT_ELLIPSE)
        val = xsect%wMax * lookup(yNorm, xs_W_VertEllipse, N_W_VertEllipse)
      case (ARCH)
        val = xsect%wMax * lookup(yNorm, xs_W_Arch, N_W_Arch)
      case (IRREGULAR)
        val = xsect%wMax * lookup(yNorm, Transect(xsect%transect)%widthTbl, N_TRANSECT_TBL)
      case (CUSTOM) !(5.0.010 - LR)
        val = xsect%wMax * lookup(yNorm,&                                    !(5.0.010 - LR)
           &Shape(Curve(xsect%transect)%refersTo)%widthTbl, N_SHAPE_TBL)     !(5.0.010 - LR) 
      case (RECT_CLOSED)
         val = xsect%wMax
      case (RECT_TRIANG)
         val = rect_triang_getWofY(xsect, y)
      case (RECT_ROUND)
         val = rect_round_getWofY(xsect, y)
      case (RECT_OPEN)
         val = xsect%wMax
      case (MOD_BASKET)
         val = mod_basket_getWofY(xsect, y)
      case (TRAPEZOIDAL)
         val = trapez_getWofY(xsect, y)
      case (TRIANGULAR)
         val = triang_getWofY(xsect, y)
      case (PARABOLIC)
         val = parab_getWofY(xsect, y)
      case (POWERFUNC)
         val = powerfunc_getWofY(xsect, y)
      case default
         val = 0.0
    end select
    xsect_getWofY = val
end function xsect_getWofY
!
!!=============================================================================
!
double precision function xsect_getRofY(xsect, y)
!
!  Input:   xsect = ptr. to a cross section data structure
!           y = depth (ft)
!  Output:  returns hydraulic radius (ft)
!  Purpose: computes xsection's hydraulic radius at a given depth.
!
    use consts
    use enums
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: y
    double precision :: yNorm, lVal
    
    yNorm = y / xsect%yFull
    
    select case ( xsect%datatype )
      case (FORCE_MAIN, CIRCULAR)
         lVal =  xsect%rFull * lookup(yNorm, xs_R_Circ, N_R_Circ) !(5.0.010 - LR)
      case (FILLED_CIRCULAR)
        if ( abs(xsect%yBot-0.0) < P_TINY ) then                                  !(5.0.022 - LR)
            lVal = xsect%rFull * lookup(yNorm, xs_R_Circ, N_R_Circ)             !(5.0.022 - LR)
        else
            lVal = filled_circ_getRofY(xsect, y)
        end if
      case (EGGSHAPED)
        lVal = xsect%rFull * lookup(yNorm, xs_R_Egg, N_R_Egg)
      case (HORSESHOE)
        lVal = xsect%rFull * lookup(yNorm, xs_R_Horseshoe, N_R_Horseshoe)
        return
      case (BASKETHANDLE)
         lVal = xsect%rFull * lookup(yNorm, xs_R_Baskethandle, N_R_Baskethandle)
      case (HORIZ_ELLIPSE)
        xsect_getRofY = xsect%rFull * lookup(yNorm, xs_R_HorizEllipse, N_R_HorizEllipse)
      case (VERT_ELLIPSE)
         lVal = xsect%rFull * lookup(yNorm, xs_R_VertEllipse, N_R_VertEllipse)
      case (ARCH)
        lVal = xsect%rFull * lookup(yNorm, xs_R_Arch, N_R_Arch)
      case (IRREGULAR)
        lVal = xsect%rFull * lookup(yNorm, Transect(xsect%transect)%hradTbl, N_TRANSECT_TBL)
      case (CUSTOM) !(5.0.010 - LR)
        lVal = xsect%rFull * lookup(yNorm,Shape(Curve(xsect%transect)%refersTo)%hradTbl, N_SHAPE_TBL) 
      case (RECT_TRIANG)
        lVal = rect_triang_getRofY(xsect, y)
      case (RECT_ROUND)
         lVal = rect_round_getRofY(xsect, y)
      case (TRAPEZOIDAL)
        lVal = trapez_getRofY(xsect, y)
      case (TRIANGULAR)
         lVal = triang_getRofY(xsect, y)
      case (PARABOLIC)
         lVal = parab_getRofY(xsect, y)
      case (POWERFUNC)
         lVal = powerfunc_getRofY(xsect, y)
      case default
         lVal = xsect_getRofA( xsect, xsect_getAofY(xsect, y) )
    end select
    xsect_getRofY = lVal
end function xsect_getRofY
!
!!=============================================================================
!
double precision function xsect_getRofA(xsect, a)
!
!  Input:   xsect = ptr. to a cross section data structure
!           a = area (ft2)
!  Output:  returns hydraulic radius (ft)
!  Purpose: computes xsection's hydraulic radius at a given area.
!
    use headers
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: a
    double precision :: lVal
    double precision :: cathy
    if ( a <= 0.0 ) then
       xsect_getRofA = 0.0
       return
    end if
    select case ( xsect%datatype )
      case (HORIZ_ELLIPSE,VERT_ELLIPSE,ARCH,IRREGULAR,FILLED_CIRCULAR,CUSTOM) !(5.0.010 - LR)
        lVal = xsect_getRofY( xsect, xsect_getYofA(xsect, a) )

      case (RECT_CLOSED)  
        lVal = rect_closed_getRofA(xsect, a)

      case (RECT_OPEN)  
        lVal = a / (xsect%wMax + 2. * a / xsect%wMax)

      case (RECT_TRIANG) 
       lVal = rect_triang_getRofA(xsect, a)

      case (RECT_ROUND) 
        lVal = rect_round_getRofA(xsect, a)

      case (MOD_BASKET) 
        lVal = mod_basket_getRofA(xsect, a)

      case (TRAPEZOIDAL) 
       lVal = trapez_getRofA(xsect, a)

      case (TRIANGULAR)  
       lVal = triang_getRofA(xsect, a)

      case (PARABOLIC)  
        lVal = parab_getRofA(xsect, a)

      case (POWERFUNC)  
        lVal = powerfunc_getRofA(xsect, a)

      case default
        cathy = xsect_getSofA(xsect, a)
        if ( cathy < P_TINY .or. a < P_TINY ) then
           lVal = 0.0
        else
           lVal = (cathy/a) ** (3./2.)
        end if
    end select
    xsect_getRofA = lVal
end function xsect_getRofA
!
!=============================================================================

double precision function xsect_getAofS(xsect, s)
!
!  Input:   xsect = ptr. to a cross section data structure
!           s = section factor (ft^(8/3))
!  Output:  returns area (ft2)
!  Purpose: computes xsection's area at a given section factor.
!
    use enums
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: s
    
    double precision :: psi
    
    double precision :: generic_getAofS, circ_getAofS
    psi = s / xsect%sFull
    
    if ( s <= 0.0 ) then 
        xsect_getAofS = 0.0 
        return
    end if
    select case (xsect%datatype )
      case (DUMMY)
         xsect_getAofS = 0.0
         return
      case (FORCE_MAIN, CIRCULAR)             !(5.0.010 - LR)
         xsect_getAofS = circ_getAofS(xsect, s)
         return 
      case (EGGSHAPED)
         xsect_getAofS = xsect%aFull * invLookup(psi, xs_S_Egg, N_S_Egg)
         return
      case (HORSESHOE)
         xsect_getAofS = xsect%aFull * invLookup(psi, xs_S_Horseshoe, N_S_Horseshoe)
         return
      case (GOTHIC)
         xsect_getAofS = xsect%aFull * invLookup(psi, xs_S_Gothic, N_S_Gothic)
         return
      case (CATENARY)
         xsect_getAofS = xsect%aFull * invLookup(psi, xs_S_Catenary, N_S_Catenary)
         return
      case (SEMIELLIPTICAL)
         xsect_getAofS = xsect%aFull * invLookup(psi, xs_S_SemiEllip, N_S_SemiEllip)
         return
      case (BASKETHANDLE)
         xsect_getAofS = xsect%aFull * invLookup(psi, xs_S_BasketHandle, N_S_BasketHandle)
         return
      case (SEMICIRCULAR)
         xsect_getAofS = xsect%aFull * invLookup(psi, xs_S_SemiCirc, N_S_SemiCirc)
         return
      case default 
         xsect_getAofS = generic_getAofS(xsect, s)
         return
    end select
end function xsect_getAofS
!
!=============================================================================

double precision function xsect_getdSdA(xsect, a)
!
!  Input:   xsect = ptr. to a cross section data structure
!           a = area (ft2)
!  Output:  returns derivative of section factor w.r.t. area (ft^2/3)
!  Purpose: computes xsection's derivative of its section factor with 
!           respect to area at a given area.
!
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: a
    double precision :: lVal
    select case ( xsect%datatype )
      case (FORCE_MAIN, CIRCULAR)          !(5.0.010 - LR)
        lVal = circ_getdSdA(xsect, a)

      case (EGGSHAPED)
        lVal = tabular_getdSdA(xsect, a, xs_S_Egg, N_S_Egg)
  
      case (HORSESHOE)
        lVal = tabular_getdSdA(xsect, a, xs_S_Horseshoe, N_S_Horseshoe)

      case (GOTHIC)
        lVal = tabular_getdSdA(xsect, a, xs_S_Gothic, N_S_Gothic)

      case (CATENARY)
        lVal = tabular_getdSdA(xsect, a, xs_S_Catenary, N_S_Catenary)

      case (SEMIELLIPTICAL)
        lVal =  tabular_getdSdA(xsect, a, xs_S_SemiEllip, N_S_SemiEllip)

      case (BASKETHANDLE)
        lVal =  tabular_getdSdA(xsect, a, xs_S_BasketHandle, N_S_BasketHandle)

      case (SEMICIRCULAR)
        lVal =  tabular_getdSdA(xsect, a, xs_S_SemiCirc, N_S_SemiCirc)

      case (RECT_CLOSED)
        lVal = rect_closed_getdSdA(xsect, a)

      case (RECT_OPEN)
        lVal = rect_open_getdSdA(xsect, a)

	  case (RECT_TRIANG)
		lVal = rect_triang_getdSdA(xsect, a)

	  case (RECT_ROUND)
		lVal = rect_round_getdSdA(xsect, a)

	  case (MOD_BASKET)
		lVal = mod_basket_getdSdA(xsect, a)

	  case (TRAPEZOIDAL)
		lVal = trapez_getdSdA(xsect, a)

	  case (TRIANGULAR)
		lVal = triang_getdSdA(xsect, a)

      case default
        lVal = generic_getdSdA(xsect, a)
    end select
    xsect_getdSdA = lVal
end function xsect_getdSdA
!
!=============================================================================

double precision function xsect_getYcrit(xsect, q)
!
!  Input:   xsect = ptr. to a cross section data structure
!           q = flow rate (cfs)
!  Output:  returns critical depth (ft)
!  Purpose: computes critical depth at a specific flow rate.
!
    use headers
    implicit none

    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: q
    
    double precision :: q2g, y, r

    q2g = SQR(q) / GRAVITY
    
    if ( abs(q2g - 0.0) < P_TINY ) then
      xsect_getYcrit = 0.0
      return
    end if
    select case ( xsect%datatype )
      case (DUMMY)
        xsect_getYcrit = 0.0
        return

      case (RECT_OPEN, RECT_CLOSED)
        ! --- analytical expression for yCritical is
        !     y = (q2g / w^2)^(1/3) where w = width
        y = (q2g / SQR(xsect%wMax)) ** (1./3.)
        !break

      case (TRIANGULAR)
        ! --- analytical expression for yCritical is
        !     y = (2 * q2g / s^2)^(1/5) where s = side slope
        y = (2.0 * q2g / SQR(xsect%sBot)) ** (1./5.)
        !break

      case (PARABOLIC)
        ! --- analytical expression for yCritical is
        !     y = (27/32 * q2g * c)^(1/4) where y = c*x^2
        !     is eqn. for parabola and 1/sqrt(c) = rBot
        y = (27./32. * q2g / SQR(xsect%rBot)) ** (1./4.)
        !break

      case (POWERFUNC)
        y = 1. / (2.0 * xsect%sBot + 3.0)
        y = ( q2g * (xsect%sBot + 1.0) / SQR(xsect%rBot)) ** y
        !break

      case default
        ! --- first estimate yCritical for an equivalent circular conduit
        !     using 1.01 * (q2g / yFull)^(1/4)
        y = 1.01 * (q2g / xsect%yFull) ** (1./4.)
        if (y >= xsect%yFull) y = 0.97 * xsect%yFull

        ! --- then find ratio of conduit area to equiv. circular area
        r = xsect%aFull / (PI / 4.0 * SQR(xsect%yFull))

        ! --- use interval enumeration method to find yCritical if 
        !     area ratio not too far from 1.0
        if ( r >= 0.5 .and. r <= 2.0 ) then
            y = getYcritEnum(xsect, q, y)
        ! --- otherwise use Ridder's root finding method
        else 
            y = getYcritRidder(xsect, q, y)
        end if
    end select

    ! --- do not allow yCritical to be > yFull
    xsect_getYcrit = MIN(y, xsect%yFull)
end function xsect_getYcrit
!
!=============================================================================

double precision function generic_getAofS(xsect, s)
!
!  Input:   xsect = ptr. to a cross section data structure
!           s = section factor (ft^8/3)
!  Output:  returns area (ft2)
!  Purpose: finds area given section factor by
!           solving S = A*(A/P(A))^(2/3) using Newton-Raphson iterations.
!
    use headers
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: s
    double precision :: a, a1, a2, tol

    if (s <= 0.0) then
       generic_getAofS = 0.0
       return
    end if

    ! --- if S is between sMax and sFull then
    !     bracket A between aFull and aMax
    if ( (s <= xsect%sMax .and. s >= xsect%sFull) .and. xsect%sMax /= xsect%sFull ) then
        a1 = xsect%aFull          ! do this because sFull < sMax
        a2 = xsect_getAmax(xsect)
    ! --- otherwise bracket A between 0 and aMax
    else
        a1 = 0.0
        a2 = xsect_getAmax(xsect)
    end if

    ! --- save S & xsect in global variables for access by evalSofA function
    Xstar = xsect
    Sstar = s

    ! --- compute starting guess for A
    a = 0.5 * (a1 + a2)

    ! use the Newton-Raphson root finder function to find A
    tol = 0.0001 * xsect%aFull
    call findroot_Newton(a1, a2, a, tol, evalSofA)
    generic_getAofS = a
end function generic_getAofS
!
!=============================================================================

subroutine evalSofA(a, f, df)
!
!  Input:   a = area
!  Output:  f = root finding function
!           df = derivative of root finding function
!  Purpose: function used in conjunction with getAofS() that evaluates 
!           f = S(a) - s and df = dS(a)/dA.
!
    implicit none
    double precision, intent(in) :: a
    double precision, intent(inout) :: f, df
    double precision :: s
    s = xsect_getSofA(Xstar, a)
    f = s - Sstar
    df = xsect_getdSdA(Xstar, a)
end subroutine evalSofA
!
!=============================================================================

double precision function tabular_getdSdA(xsect, a, table, nItems)
!
!  Input:   xsect = ptr. to cross section data structure
!           a = area (ft2)
!           table = ptr. to table of section factor v. normalized area
!           nItems = number of equally spaced items in table
!  Output:  returns derivative of section factor w.r.t. area (ft^2/3)
!  Purpose: computes derivative of section factor w.r.t area 
!           using geometry tables.
!

    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    double precision, dimension(:), intent(in) :: table
    integer, intent(in) :: nItems

    integer ::    i
    double precision :: alpha, delta, dSdA
    alpha = a / xsect%aFull
    delta = 1.0 / (nItems-1)

    ! --- find which segment of table contains alpha
    i = int(alpha / delta)
    if ( i >= nItems - 1 ) i = nItems - 2

    ! --- compute slope from this interval of table
    dSdA = (table(i+1) - table(i)) / delta

    ! --- convert slope to un-normalized value
    tabular_getdSdA = dSdA * xsect%sFull / xsect%aFull
end function tabular_getdSdA
!
!!=============================================================================
!
double precision function generic_getdSdA(xsect, a)
!
!  Input:   xsect = ptr. to cross section data structure
!           a = area (ft2)
!  Output:  returns derivative of section factor w.r.t. area (ft^2/3)
!  Purpose: computes derivative of section factor w.r.t area 
!           using central difference approximation.
!
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    
    double precision :: a1, a2, alpha, alpha1, alpha2

    alpha = a / xsect%aFull
    alpha1 = alpha - 0.001
    alpha2 = alpha + 0.001
    if ( alpha1 < 0.0 ) alpha1 = 0.0
    a1 = alpha1 * xsect%aFull
    a2 = alpha2 * xsect%aFull
    generic_getdSdA = (xsect_getSofA(xsect, a2) - xsect_getSofA(xsect, a1)) / (a2 - a1)
end function generic_getdSdA
!
!=============================================================================

double precision function lookup(x, table, nItems)
!
!  Input:   x = value of independent variable in a geometry table
!           table = ptr. to geometry table
!           nItems = number of equally spaced items in table
!  Output:  returns value of dependent table variable 
!  Purpose: looks up a value in a geometry table (i.e., finds y given x).
!

    implicit none
    double precision, intent(in) :: x
    integer, intent(in) :: nItems
    double precision, dimension(:), intent(in) :: table
    
    double precision ::  delta, x0, x1, y, y2
    integer ::     i

    ! --- find which segment of table contains x
    delta = 1.0 / (nItems-1)
    i = int(x / delta)
    !i = i + 1 !TODO: verify this is the correction needed from C
    if (i == 0) i = i + 1
    if ( i >= nItems - 1 ) then
       lookup = table(nItems-1) 
       return
    end if
    ! --- compute x at start and end of segment
    x0 = i * delta
    x1 = (i+1) * delta

    ! --- linearly interpolate a y-value
    y = table(i) + (x - x0) * (table(i+1) - table(i)) / delta

    ! --- use quadratic interpolation for low x value
    if ( i < 2 ) then
        y2 = y + (x - x0) * (x - x1) / (delta*delta) * &
            &(table(i)/2.0 - table(i+1) + table(i+2)/2.0) 
        if ( y2 > 0.0 ) y = y2
    end if
    if ( y < 0.0 ) y = 0.0
    lookup = y
    return
end function lookup
!
!=============================================================================

double precision function invLookup(y, table, nItems)
!
!  Input:   y = value of dependent variable in a geometry table
!           table = ptr. to geometry table
!           nItems = number of equally spaced items in table
!  Output:  returns value of independent table variable 
!  Purpose: performs inverse lookup in a geometry table (i.e., finds
!           x given y).
!
    implicit none
    double precision, intent(in) :: y
    double precision, dimension(:), intent(in) :: table
    integer, intent(in) :: nItems
    
    double precision :: delta, x, x0, x1
    integer ::    i
    integer :: locate
    ! --- locate table segment that contains y
    i = locate(y, table, nItems)
    if ( i >= nItems - 1 ) then
       invLookup = 1.0
       return
    end if
    ! --- compute x at start and end of segment
    delta = 1.0 / (nItems-1)
    x0 = i * delta
    x1 = x0 + delta

    ! --- linearly interpolate an x value
    x = x0 + (y - table(i)) * (x1 - x0) / (table(i+1) - table(i)) 
    if ( x < 0.0 ) x = 0.0
    invLookup = x
    return
end function invLookup
!
!!=============================================================================
!
integer function locate(y, table, nItems)
!
!  Input:   y = value of dependent variable in a geometry table
!           table = ptr. to geometry table
!           nItems = number of equally spaced items in table
!  Output:  returns index j of table such that table(j-1) <= y <= table(j)
!  Purpose: uses bisection method to locate the lowest table index whose
!           value does not exceed a given value.
!
    use headers
    implicit none
    double precision, intent(in) :: y
    integer, intent(in) :: nItems
    double precision, dimension(:), intent(in) :: table
    integer :: j, j1, j2

    j = 1
    j1 = 0
    j2 = nItems
    
    do j = 1, size(table, 1)
       if (y >= table(j - 1) .and. y <= table(j)) then
           locate = j
           return
       end if
    end do
    if (j -1 == 0) then
       locate = 1
    else
       locate = j - 1
    end if
    
!    do while ( j2 - j1 > 1)
!        j = (j1 + j2) >> 1
!        if ( y > table(j) ) then
!           j1 = j
!        else
!           j2 = j
!        end if
!    end do
!    locate = j - 1
end function locate
!
!!=============================================================================
!
double precision function getQcritical(yc)
!
!  Input:   yc = critical depth (ft)
!  Output:  returns flow difference value (cfs)
!  Purpose: finds difference between critical flow at depth yc and 
!           target value Qcritical
!
    use headers
    implicit none
    double precision, intent(in) :: yc
    double precision :: a, w, qc
    a = xsect_getAofY(Xstar, yc)
    w = xsect_getWofY(Xstar, yc)
    qc = -Qcritical
    
    if ( w > 0.0 )  qc = a * sqrt(GRAVITY * a / w) - Qcritical
    getQcritical = qc
end function getQcritical
!
!=============================================================================
 
double precision function getYcritEnum(xsect, q, y0)
!
!  Input:   xsect = ptr. to cross section data structure
!           q = critical flow rate (cfs)
!           y0 = estimate of critical depth (ft)
!  Output:  returns true critical depth (ft)
!  Purpose: solves a * sqrt(a(y)*g / w(y)) - q for y using interval
!           enumeration with starting guess of y0.
!
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: q, y0
    
    double precision ::  q0, dy, qc, yc
    integer ::     i1, i

    ! --- store reference to cross section in global pointer
    Xstar = xsect

    ! --- divide cross section depth into 25 increments and
    !     locate increment corresponding to initial guess y0
    dy = xsect%yFull / 25.
    i1 = int(y0 / dy)

    ! --- evaluate critical flow at this increment
    Qcritical = 0.0
    q0 = getQcritical(i1*dy)

    ! --- initial flow lies below target flow 
    if ( q0 < q ) then
        ! --- search each successive higher depth increment
        yc = xsect%yFull
        do i=i1+1, 25
            ! --- if critical flow at current depth is above target
            !     then use linear interpolation to compute critical depth
            qc = getQcritical(i*dy)
            if ( qc >= q ) then
                yc = ( (q-q0) / (qc - q0) + (i-1)*1.0 ) * dy
                !break
                exit
            end if
            q0 = qc
        end do
    ! --- initial flow lies above target flow
    else
        ! --- search each successively lower depth increment
        yc = 0.0
        do i=i1-1, 0, -1
            ! --- if critical flow at current depth is below target
            !     then use linear interpolation to compute critical depth
            qc = getQcritical(i*dy)
            if ( qc < q ) then
                yc = ( (q-qc) / (q0-qc) + i*1.0 ) * dy
                !break
                exit
            end if
            q0 = qc
        end do
    end if
    getYcritEnum = yc
end function getYcritEnum
!
!!=============================================================================

double precision function getYcritRidder(xsect, q, y0)
!
!  Input:   xsect = ptr. to cross section data structure
!           q = critical flow rate (cfs)
!           y0 = estimate of critical depth (ft)
!  Output:  returns true critical depth (ft)
!  Purpose: solves a * sqrt(a(y)*g / w(y)) - q for y using Ridder's
!           root finding method with starting guess of y0.
!
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: q, y0
    double precision ::  y1
    double precision ::  y2
    double precision :: yc
    double precision :: q0, q1, q2
    
    double precision :: findroot_ridder !TODO: this is for .NET compile
    
    y1 = 0.0
    y2 = 0.99 * xsect%yFull

    ! --- store reference to cross section in global pointer
    Xstar = xsect

    ! --- check if critical flow at (nearly) full depth < target flow
    Qcritical = 0.0
    q2 = getQcritical(y2)
    if (q2 < q ) then
       getYcritRidder = xsect%yFull
       return
    end if

    ! --- evaluate critical flow at initial depth guess y0
    !     and at 1/2 of full depth
    q0 = getQcritical(y0)
    q1 = getQcritical(0.5*xsect%yFull)

    ! --- adjust search interval on depth so it contains flow q
    if ( q0 > q ) then
        y2 = y0
        if ( q1 < q ) y1 = 0.5*xsect%yFull
    else
        y1 = y0
        if ( q1 > q ) y2 = 0.5*xsect%yFull
    end if

    ! --- save value of target critical flow in global variable
    Qcritical = q

    ! --- call Ridder root finding procedure with error tolerance
    !     of 0.001 ft. to find critical depth yc
    yc = findroot_Ridder(y1, y2, 0.001d00, getQcritical)
    getYcritRidder = yc
end function getYcritRidder
!
!
!!=============================================================================
!!  RECT_CLOSED fuctions
!!=============================================================================
!
double precision function rect_closed_getSofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: a
    
    ! --- if a > area corresponding to Smax then
    !     interpolate between sMax and Sfull
    double precision :: alfMax
    alfMax = RECT_ALFMAX
    if ( a / xsect%aFull > alfMax ) then
        rect_closed_getSofA = xsect%sMax + (xsect%sFull - xsect%sMax) * &
              &(a/xsect%aFull - alfMax) / (1.0 - alfMax)
    else
    ! --- otherwise use regular formula
        rect_closed_getSofA = a * (xsect_getRofA(xsect, a) ** (2./3.))
    end if
end function rect_closed_getSofA
!
double precision function rect_closed_getdSdA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: a
    
    double precision :: alpha, alfMax, r

    ! --- if above level corresponding to sMax, then
    !     use slope between sFull & sMax
    alfMax = RECT_ALFMAX
    alpha = a / xsect%aFull
    if ( alpha > alfMax ) then
        rect_closed_getdSdA = (xsect%sFull - xsect%sMax) / ((1.0 - alfMax) * xsect%aFull)
        return
    end if

    ! --- for small a/aFull use generic central difference formula
    if ( alpha <= 1.0e-30 ) then
        rect_closed_getdSdA = generic_getdSdA(xsect, a)
        return
    end if

    ! --- otherwise evaluate dSdA = (5/3 - (2/3)(dP/dA)R)R^(2/3)
    !     (where P = wetted perimeter & dPdA = 2/width)
    r = xsect_getRofA(xsect, a)
    rect_closed_getdSdA =  (5./3. - (2./3.) * (2.0/xsect%wMax) * r) * (r ** (2./3.))
end function rect_closed_getdSdA
!
double precision function rect_closed_getRofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    double precision :: p
    if ( a <= 0.0 ) then
        rect_closed_getRofA = 0.0
        return
    end if
    p = xsect%wMax + 2.*a/xsect%wMax ! Wetted Perim = width + 2*area/width
    if ( a/xsect%aFull > RECT_ALFMAX ) then
        p = p + (a/xsect%aFull - RECT_ALFMAX) / (1.0 - RECT_ALFMAX) * xsect%wMax
    end if
    
    rect_closed_getRofA = a / p
    return
end function rect_closed_getRofA
!
!
!!=============================================================================
!!  RECT_OPEN fuctions
!!=============================================================================
!
double precision function rect_open_getSofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision :: a
    
    double precision :: y, r
    y = a / xsect%wMax
    r = a / (2.0*y + xsect%wMax)
    
    rect_open_getSofA = a * (r ** (2./3.))
end function rect_open_getSofA
!

double precision function rect_open_getdSdA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision :: a

    double precision :: r, dPdA

    ! --- for small a/aFull use generic central difference formula
    if ( a / xsect%aFull <= 1.0e-30 )  then
       rect_open_getdSdA = generic_getdSdA(xsect, a)
       return
    end if

    ! --- otherwise evaluate dSdA = (5/3 - (2/3)(dP/dA)R)R^(2/3)
    !     (where P = wetted perimeter)
    r = xsect_getRofA(xsect, a)
    dPdA = 2.0 / xsect%wMax      ! since P = geom2 + 2a/geom2
    rect_open_getdSdA =  (5./3. - (2./3.) * dPdA * r) * (r ** (2./3.))
end function rect_open_getdSdA
!
!
!!=============================================================================
!!  RECT_TRIANG fuctions
!!=============================================================================
!
double precision function rect_triang_getYofA(xsect, a)
    !TXsect* xsect, double a
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    ! below upper section
    if ( a <= xsect%aBot ) then
       rect_triang_getYofA =sqrt(a / xsect%sBot)
    ! above bottom section
    else 
       rect_triang_getYofA = xsect%yBot + (a - xsect%aBot) / xsect%wMax
    end if
end function rect_triang_getYofA
!
double precision function rect_triang_getRofA(xsect, a)
    !TXsect* xsect, double a
    use headers
    implicit none
    
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    double precision :: y, p, alf

    if ( a <= 0.0 ) then
       rect_triang_getRofA = 0.0
       return
    end if
    y = rect_triang_getYofA(xsect, a)
    
    ! below upper section
    if ( y <= xsect%yBot ) then
       rect_triang_getRofA = a / (2. * y * xsect%rBot)
       return  
    end if
    ! wetted perimeter without contribution of top surface
    p = 2. * xsect%yBot * xsect%rBot + 2. * (y - xsect%yBot)

    ! top-surface contribution
    alf = (a / xsect%aFull) - RECT_TRIANG_ALFMAX
    if ( alf > 0.0 ) then
       p = p+ alf / (1.0 - RECT_TRIANG_ALFMAX) * xsect%wMax
    end if
    rect_triang_getRofA = a / p
    return
end function rect_triang_getRofA

double precision function rect_triang_getSofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: a
    
    ! --- if a > area corresponding to sMax, then
    !     interpolate between sMax and Sfull
    double precision :: alfMax
    alfMax = RECT_TRIANG_ALFMAX
    if ( a / xsect%aFull > alfMax ) then
      rect_triang_getSofA = xsect%sMax + (xsect%sFull - xsect%sMax) * &
              &(a/xsect%aFull - alfMax) / (1.0 - alfMax)
    ! --- otherwise use regular formula
    else 
      rect_triang_getSofA = a * (rect_triang_getRofA(xsect, a) **(2./3.))
    end if
end function rect_triang_getSofA
!
double precision function rect_triang_getdSdA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    double precision :: alpha, alfMax, dPdA, r

    ! --- if a > area corresponding to sMax, then
    !     use slope between sFull & sMax
    alfMax = RECT_TRIANG_ALFMAX
    alpha = a / xsect%aFull
    if ( alpha > alfMax ) then
        rect_triang_getdSdA = (xsect%sFull - xsect%sMax) / ((1.0 - alfMax) * xsect%aFull)
        return
    end if

    ! --- use generic central difference method for very small a
    if ( alpha <= 1.0e-30 ) then
       rect_triang_getdSdA = generic_getdSdA(xsect, a)
       return
    end if

    ! --- find deriv. of wetted perimeter
    if ( a > xsect%aBot ) then
        dPdA = 2.0 / xsect%wMax  ! for upper rectangle
    else 
        dPdA = xsect%rBot / sqrt(a * xsect%sBot)  ! for triang. bottom
    end if

    ! --- get hyd. radius & evaluate section factor derivative formula
    r = rect_triang_getRofA(xsect, a)
    rect_triang_getdSdA =  (5./3. - (2./3.) * dPdA * r) * (r ** (2./3.))
end function rect_triang_getdSdA
!
double precision function rect_triang_getAofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y

    if ( y <= xsect%yBot ) then
       rect_triang_getAofY = y * y * xsect%sBot         ! below upper section
    else 
       rect_triang_getAofY = xsect%aBot + (y - xsect%yBot) * xsect%wMax  ! above bottom section
    end if
end function rect_triang_getAofY

double precision function rect_triang_getRofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y

    double precision :: y1, p
    y1 = y - xsect%yBot
    if ( y1 <= 0.0 ) then                             ! below upper section
        rect_triang_getRofY = xsect%sBot / (2. * xsect%rBot)
    else                                         ! above bottom section
        p = (2. * xsect%yBot * xsect%rBot) + (2. * y1)
        if ( y >= xsect%yFull ) p = p + xsect%wMax
        rect_triang_getRofY = (xsect%aBot + y1 * xsect%wMax) / p
    end if
end function rect_triang_getRofY

double precision function rect_triang_getWofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    if ( y <= xsect%yBot ) then
        rect_triang_getWofY = 2.0 * xsect%sBot * y  ! below upper section
    else 
        rect_triang_getWofY = xsect%wMax            ! above bottom section
    end if
end function rect_triang_getWofY
!
!
!!=============================================================================
!!  RECT_ROUND fuctions
!!=============================================================================
!
!!! --- The functions below were re-written to correctly account              !(5.0.014 - LR)
!!!     for the case where the bottom curvature is less than that
!!!     of a half circle.
!
double precision function rect_round_getYofA(xsect, a)
    !TXsect* xsect, double a
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    double precision :: alpha
    double precision :: getYcircular

    ! --- if above circular bottom:
    if ( a > xsect%aBot ) then
        rect_round_getYofA = xsect%yBot + (a - xsect%aBot) / xsect%wMax
        return
    end if

    ! --- otherwise use circular xsection method to find height
    alpha = a / (PI * xsect%rBot * xsect%rBot)
    if ( alpha < 0.04 ) then
       rect_round_getYofA = (2.0 * xsect%rBot) * getYcircular(alpha)      !5.0.014 - LR)
       return
    end if
    
    rect_round_getYofA = (2.0 * xsect%rBot) * lookup(alpha, xs_Y_Circ, N_Y_Circ)
    return
end function rect_round_getYofA
!
double precision function rect_round_getRofA(xsect, a)
    !TXsect* xsect, double a
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    double precision :: y1, theta1, p, arg

    ! --- if above circular invert ...
    if ( a <= 0.0 ) then
       rect_round_getRofA = 0.0
       return
    end if
    if ( a > xsect%aBot ) then
        ! wetted perimeter without contribution of top surface
        y1 = (a - xsect%aBot) / xsect%wMax
        theta1 = 2.0 * asin(xsect%wMax/2.0/xsect%rBot)
        p = xsect%rBot*theta1 + 2.0*y1

        ! top-surface contribution
        arg = (a / xsect%aFull) - RECT_ROUND_ALFMAX
        if ( arg > 0.0 ) p = p + arg / (1.0 - RECT_ROUND_ALFMAX) * xsect%wMax
        rect_round_getRofA = a / p
        return
    end if

    ! --- if within circular invert ...
    y1 = rect_round_getYofA(xsect, a)
    theta1 = 2.0*acos(1.0 - y1/xsect%rBot)
    p = xsect%rBot * theta1
    rect_round_getRofA = a / p
    return
end function rect_round_getRofA
!
double precision function rect_round_getSofA(xsect, a)
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: a

    double precision :: alpha, aFull, sFull

    ! --- if a > area corresponding to sMax,
    !     interpolate between sMax and sFull
    double precision :: alfMax
    alfMax = RECT_ROUND_ALFMAX

    if ( a / xsect%aFull > alfMax ) then
        rect_round_getSofA = xsect%sMax + (xsect%sFull - xsect%sMax) * &
              &(a / xsect%aFull - alfMax) / (1.0 - alfMax)
    ! --- if above circular invert, use generic function
    else if ( a > xsect%aBot ) then
        rect_round_getSofA = a * (xsect_getRofA(xsect, a) ** (2./3.))
    ! --- otherwise use circular xsection function applied
    !     to full circular shape of bottom section
    else
        aFull = PI * xsect%rBot * xsect%rBot
        alpha = a / aFull
        sFull = xsect%sBot

        ! --- use special function for small a/aFull
        if ( alpha < 0.04 ) then
            rect_round_getSofA = sFull * getScircular(alpha)
        ! --- otherwise use table
        else 
            rect_round_getSofA = sFull * lookup(alpha, xs_S_Circ, N_S_Circ)
        end if
    end if
end function rect_round_getSofA
!
double precision function rect_round_getdSdA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    double precision :: alfMax, r, dPdA, lVal

    ! --- if a > area corresponding to sMax, then
    !     use slope between sFull & sMax
    alfMax = RECT_ROUND_ALFMAX
    if ( a / xsect%aFull > alfMax ) then
        lVal = (xsect%sFull - xsect%sMax) /((1.0 - alfMax) * xsect%aFull)
    ! --- if above circular invert, use analytical function for dS/dA
    else if ( a > xsect%aBot ) then
        r = rect_round_getRofA(xsect, a)
        dPdA = 2.0 / xsect%wMax       ! d(wet perim)/dA for rect.
        lVal =  (5./3. - (2./3.) * dPdA * r) * (r ** (2./3.))
    ! --- otherwise use generic finite difference function
    else 
        lVal = generic_getdSdA(xsect, a)
    end if
    rect_round_getdSdA = lVal
end function rect_round_getdSdA
!
double precision function rect_round_getAofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y

    double precision :: theta1

    ! --- if above circular invert...
    if ( y > xsect%yBot ) then
        rect_round_getAofY = xsect%aBot + (y - xsect%yBot) * xsect%wMax
        return
    end if

    ! --- find area of circular section
    theta1 = 2.0*acos(1.0 - y/xsect%rBot)
    rect_round_getAofY = 0.5 * xsect%rBot * xsect%rBot * (theta1 - sin(theta1))
end function rect_round_getAofY
!
double precision function rect_round_getRofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y

    double precision :: theta1

    ! --- if above top of circular bottom, use RofA formula
    if ( y <= 0.0 ) then
       rect_round_getRofY = 0.0
       return
    end if
    if ( y > xsect%yBot ) then
        rect_round_getRofY = rect_round_getRofA( xsect, rect_round_getAofY(xsect, y) )
        return
    end if

    ! --- find hyd. radius of circular section
    theta1 = 2.0*acos(1.0 - y/xsect%rBot)
    rect_round_getRofY = 0.5 * xsect%rBot * (1.0 - sin(theta1)) / theta1
end function rect_round_getRofY

double precision function rect_round_getWofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    
    ! --- return width if depth above circular bottom section
    if ( y > xsect%yBot ) then
        rect_round_getWofY = xsect%wMax
    else
    ! --- find width of circular section
        rect_round_getWofY = 2.0 * sqrt( y * (2.0*xsect%rBot - y) ) 
    end if
end function rect_round_getWofY
!
!
!!=============================================================================
!!  MOD_BASKETHANDLE fuctions
!!=============================================================================
!
!!! --- The functions below were modified to accommodate a more               !(5.0.014 - LR)
!!!     general type of modified baskethandle cross section
!!!     that is the same as an upside down round rectangular shape.
!
!
!! Note: the variables rBot, yBot, and aBot refer to properties of the
!!       circular top portion of the cross-section (not the bottom)
!
double precision function mod_basket_getYofA(xsect, a)
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    
    double precision :: alpha, y1

    ! --- water level below top of rectangular bottom
    if ( a <= xsect%aFull - xsect%aBot ) then
       mod_basket_getYofA = a / xsect%wMax
       return
    end if

    ! --- find unfilled top area / area of full circular top
    alpha = (xsect%aFull - a) / (PI * xsect%rBot * xsect%rBot)

    ! --- find unfilled height
    if ( alpha < 0.04 ) then
        y1 = getYcircular(alpha)
    else
        y1 = lookup(alpha, xs_Y_Circ, N_Y_Circ)
    end if
    
    y1 = 2.0 * xsect%rBot * y1

    ! --- return difference between full height & unfilled height
    mod_basket_getYofA = xsect%yFull - y1
end function mod_basket_getYofA
!
double precision function mod_basket_getRofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    
    double precision :: y1, p, theta1

    ! --- water level is below top of rectangular bottom
    !     return hyd. radius of rectangle
    if ( a <= xsect%aFull - xsect%aBot ) then
        mod_basket_getRofA = a / (xsect%wMax + 2.0 * a / xsect%wMax)
        return
    end if

    ! --- find height of empty area
    y1 = xsect%yFull - mod_basket_getYofA(xsect, a)

    ! --- find angle of circular arc corresponding to this height
    theta1 = 2.0 * acos(1.0 - y1 / xsect%rBot)

    ! --- find perimeter of wetted portion of circular arc
    !     (angle of full circular opening was stored in sBot)
    p = (xsect%sBot - theta1) * xsect%rBot

    ! --- add on wetted perimeter of bottom rectangular area
    y1 = xsect%yFull - xsect%yBot
    p =  p + 2.0*y1 + xsect%wMax

    ! --- return area / wetted perimeter
    mod_basket_getRofA = a / p
end function mod_basket_getRofA
!
double precision function mod_basket_getdSdA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    
    double precision :: r, dPdA

    ! --- if water level below top of rectangular bottom but not
    !     empty then use same code as for rectangular xsection
    if ( a <= xsect%aFull - xsect%aBot .and. a/xsect%aFull > 1.0e-30 ) then
        r = a / (xsect%wMax + 2.0 * a / xsect%wMax)
        dPdA = 2.0 / xsect%wMax
        mod_basket_getdSdA = (5./3. - (2./3.) * dPdA * r) * (r **(2./3.))
    ! --- otherwise use generic function
    else 
        mod_basket_getdSdA = generic_getdSdA(xsect, a)
    end if
end function mod_basket_getdSdA
!
double precision function mod_basket_getAofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    
    double precision :: a1, theta1, y1

    ! --- if water level is below top of rectangular bottom
    !     return depth * width
    if ( y <= xsect%yFull - xsect%yBot ) then
       mod_basket_getAofY = y * xsect%wMax
       return
    end if

    ! --- find empty top circular area
    y1 = xsect%yFull - y
    theta1 = 2.0*acos(1.0 - y1/xsect%rBot)
    a1 = 0.5 * xsect%rBot * xsect%rBot * (theta1 - sin(theta1))

    ! --- return difference between full and empty areas
    mod_basket_getAofY = xsect%aFull - a1
end function mod_basket_getAofY

double precision function mod_basket_getWofY(xsect,  y)

    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    double precision :: y1

    ! --- if water level below top of rectangular bottom then return width
    if ( y <= 0.0 ) then
      mod_basket_getWofY = 0.0
      return
    end if
    if ( y <= xsect%yFull - xsect%yBot ) then
      mod_basket_getWofY = xsect%wMax
      return
    end if

    ! --- find width of empty top circular section
    y1 = xsect%yFull - y
    mod_basket_getWofY = 2.0 * sqrt( y1 * (2.0*xsect%rBot - y1) ) 
end function mod_basket_getWofY
!
!
!!=============================================================================
!!  TRAPEZOIDAL fuctions
!!
!!  Note: yBot = width of bottom
!!        sBot = avg. of side slopes
!!        rBot = length of sides per unit of depth
!!=============================================================================
!
double precision function trapez_getYofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    if ( abs(xsect%sBot - 0.0) < P_TINY ) then
        trapez_getYofA = a / xsect%yBot                          !(5.0.012 - LR)
    else
        trapez_getYofA = ( sqrt( xsect%yBot*xsect%yBot + 4.*xsect%sBot*a )- xsect%yBot )/(2. * xsect%sBot)
    end if
end function trapez_getYofA

double precision function trapez_getRofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    trapez_getRofA = a / (xsect%yBot + trapez_getYofA(xsect, a) * xsect%rBot)
end function trapez_getRofA

double precision function trapez_getdSdA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    double precision :: r, dPdA
    ! --- use generic central difference method for very small a
    if ( a/xsect%aFull <= 1.0e-30 ) then
       trapez_getdSdA = generic_getdSdA(xsect, a)
       return
    end if

    ! --- otherwise use analytical formula:
    !     dSdA = (5/3 - (2/3)(dP/dA)R)R^(2/3)
    r = trapez_getRofA(xsect, a)
    dPdA = xsect%rBot /sqrt( xsect%yBot * xsect%yBot + 4. * xsect%sBot * a )
    trapez_getdSdA =  (5./3. - (2./3.) * dPdA * r) * (r ** (2./3.))
end function trapez_getdSdA

double precision function trapez_getAofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    trapez_getAofY = ( xsect%yBot + xsect%sBot * y ) * y
end function trapez_getAofY

double precision function trapez_getRofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y

    if ( abs(y -0.0) < P_TINY ) then
       trapez_getRofY = 0.0     !(5.0.022 - LR)
    else
       trapez_getRofY = trapez_getAofY(xsect, y) / (xsect%yBot + y * xsect%rBot)
    end if
end function trapez_getRofY
!
double precision function trapez_getWofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    trapez_getWofY = xsect%yBot + 2.0 * y * xsect%sBot
end function trapez_getWofY
!
!
!!=============================================================================
!!  TRIANGULAR fuctions
!!=============================================================================
!
double precision function triang_getYofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    triang_getYofA = sqrt(a / xsect%sBot)
end function triang_getYofA

double precision function triang_getRofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    triang_getRofA = a / (2. * triang_getYofA(xsect, a) * xsect%rBot)
end function triang_getRofA

double precision function triang_getdSdA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    double precision :: r, dPdA
    ! --- use generic finite difference method for very small a
    if ( a/xsect%aFull <= 1.0e-30 ) then
       triang_getdSdA = generic_getdSdA(xsect, a)
       return
    end if

    ! --- evaluate dSdA = (5/3 - (2/3)(dP/dA)R)R^(2/3)
    r = triang_getRofA(xsect, a)
    dPdA = xsect%rBot / sqrt(a * xsect%sBot)
    triang_getdSdA =  (5./3. - (2./3.) * dPdA * r) * (r** (2./3.))
end function triang_getdSdA

double precision function triang_getAofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y

    triang_getAofY = y * y * xsect%sBot
end function triang_getAofY

double precision function triang_getRofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y

    triang_getRofY = (y * xsect%sBot) / (2. * xsect%rBot)
end function triang_getRofY
!
double precision function triang_getWofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    triang_getWofY = 2.0 * xsect%sBot * y
end function triang_getWofY
!
!
!!=============================================================================
!!  PARABOLIC fuctions
!!=============================================================================
!
double precision function parab_getYofA(xsect, a)
   use headers
   implicit none
   type(TXsect), intent(in) :: xsect
   double precision, intent(in) :: a
    parab_getYofA = ((3./4.) * a / xsect%rBot) ** (2./3. )
end function parab_getYofA

double precision function parab_getRofA(xsect, a)
   use headers
   implicit none
   type(TXsect), intent(in) :: xsect
   double precision, intent(in) :: a
    if ( a <= 0.0 ) then
       parab_getRofA = 0.0
    else
       parab_getRofA = a / parab_getPofY( xsect, parab_getYofA(xsect, a) )
    end if
end function parab_getRofA

double precision function parab_getPofY(xsect, y)
   use headers
   implicit none
   type(TXsect), intent(in) :: xsect
   double precision, intent(in) :: y

    double precision :: x, t
    x = 2. * sqrt(y) / xsect%rBot
    t = sqrt(1.0 + x * x)
    
    parab_getPofY = 0.5 * xsect%rBot * xsect%rBot * ( x * t + log(x + t) )
end function parab_getPofY

double precision function parab_getAofY(xsect, y)
   use headers
   implicit none
   type(TXsect), intent(in) :: xsect
   double precision, intent(in) :: y
    parab_getAofY = (4./3. * xsect%rBot * y * sqrt(y))
end function parab_getAofY

double precision function parab_getRofY(xsect, y)
   use headers
   implicit none
   type(TXsect), intent(in) :: xsect
   double precision, intent(in) :: y
    if ( y <= 0.0 ) then
       parab_getRofY = 0.0
    else
       parab_getRofY = parab_getAofY(xsect, y) / parab_getPofY(xsect, y)
    end if
end function parab_getRofY

double precision function parab_getWofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y

    parab_getWofY = 2.0 * xsect%rBot * sqrt(y)
end function parab_getWofY
!
!
!!=============================================================================
!!  POWERFUNC fuctions
!!=============================================================================
!
double precision function powerfunc_getYofA(xsect, a)

    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    
    powerfunc_getYofA = (a / xsect%rBot) ** (1.0 / (xsect%sBot + 1.0))
end function powerfunc_getYofA
!
double precision function powerfunc_getRofA(xsect, a)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    if ( a <= 0.0 ) then
       powerfunc_getRofA = 0.0
    else
       powerfunc_getRofA = a / powerfunc_getPofY(xsect, powerfunc_getYofA(xsect, a))
    end if
end function powerfunc_getRofA

double precision function powerfunc_getPofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    double precision :: dy1 , h , m , p , y1 , x1
    double precision :: x2, y2, dx, dy

    dy1 = 0.02 * xsect%yFull
    h = (xsect%sBot + 1.0) * xsect%rBot / 2.0
    m = xsect%sBot
    p = 0.0
    y1 = 0.0
    x1 = 0.0

    do while (.true.)
        y2 = y1 + dy1
        if ( y2 > y ) y2 = y
        x2 = h * (y2 ** m)
        dx = x2 - x1
        dy = y2 - y1
        p = p + sqrt(dx*dx + dy*dy)
        x1 = x2
        y1 = y2

        if (y2 >= y) exit
    !} !while ( y2 < y )
    end do

    powerfunc_getPofY = 2.0 * p
end function powerfunc_getPofY

double precision function powerfunc_getAofY(xsect, y)
    use headers
    implicit none
    
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    
    powerfunc_getAofY = xsect%rBot * (y ** (xsect%sBot + 1.0))
end function powerfunc_getAofY

double precision function powerfunc_getRofY(xsect, y)
    use headers
    implicit none
    
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    if ( y <= 0.0 ) then
       powerfunc_getRofY = 0.0
    else
       powerfunc_getRofY = powerfunc_getAofY(xsect, y) / powerfunc_getPofY(xsect, y)
    end if
end function powerfunc_getRofY
!
double precision function powerfunc_getWofY(xsect, y)
    use headers
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y

    powerfunc_getWofY = (xsect%sBot + 1.0) * xsect%rBot * (y ** xsect%sBot)
end function powerfunc_getWofY
!
!
!!=============================================================================
!!  CIRCULAR functions 
!!=============================================================================
!
double precision function circ_getYofA(xsect, a)

    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    double precision :: alpha
    alpha = a / xsect%aFull

    ! --- use special function for small a/aFull
    if ( alpha < 0.04 )  then
         circ_getYofA = xsect%yFull * getYcircular(alpha)
    ! --- otherwise use table 
    else 
         circ_getYofA = xsect%yFull * lookup(alpha, xs_Y_Circ, N_Y_Circ)
    end if
end function circ_getYofA

double precision function circ_getAofS(xsect, s)
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: s
    double precision :: psi
    psi = s / xsect%sFull
    if (abs(psi - 0.0) < P_TINY) then
       circ_getAofS = 0.0
       return
    end if
    if (psi >= 1.0) then
       circ_getAofS = xsect%aFull
       return
    end if

    ! --- use special function for small s/sFull
    if (psi <= 0.015) then
         circ_getAofS = xsect%aFull * getAcircular(psi)
    ! --- otherwise use table
    else 
         circ_getAofS = xsect%aFull * invLookup(psi, xs_S_Circ, N_S_Circ)
    end if
end function circ_getAofS

double precision function circ_getSofA(xsect, a)

    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a
    double precision :: alpha
    alpha = a / xsect%aFull

    ! --- use special function for small a/aFull
    if ( alpha < 0.04 ) then
        circ_getSofA = xsect%sFull * getScircular(alpha)
    ! --- otherwise use table
    else
        circ_getSofA = xsect%sFull * lookup(alpha, xs_S_Circ, N_S_Circ)
    end if
end function circ_getSofA

double precision function circ_getdSdA(xsect, a)
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: a

    double precision :: alpha, theta, p, r, dPdA

    ! --- for near-zero area, use generic central difference formula
    alpha = a / xsect%aFull
    if ( alpha <= 1.0e-30 ) then
        circ_getdSdA = 1.0e-30  !generic_getdSdA(xsect, a)

    ! --- for small a/aFull use analytical derivative
    else if ( alpha < 0.04 ) then
        theta = getThetaOfAlpha(alpha)
        p = theta * xsect%yFull / 2.0
        r = a / p
        dPdA = 4.0 / xsect%yFull / (1. - cos(theta))
        circ_getdSdA =  (5./3. - (2./3.) * dPdA * r) * (r ** (2./3.))
    ! --- otherwise use generic tabular getdSdA
    else 
        circ_getdSdA = tabular_getdSdA(xsect, a, xs_S_Circ, N_S_Circ)
    end if
end function circ_getdSdA
!
!!!!!!!!!!!!!!!!!!!!!!!!!
!! This is an alternate method used in SWMM 4.4.
!!!!!!!!!!!!!!!!!!!!!!!!!
!/*
!double circ_getdSdA(TXsect* xsect, double a)
!{
!    double alpha, a1, a2, da, s1, s2, ds
!    alpha = a / xsect%aFull
!    if ( alpha <= 1.0e-30 ) return 1.0e-30
!    da = 0.002
!    a1 = alpha - 0.001
!    a2 = alpha + 0.001
!    if ( a1 < 0.0 )
!    {
!	    a1 = 0.0
!    	da = alpha + 0.001
!    }
!    s1 = getScircular(a1)
!    s2 = getScircular(a2)
!    ds = (s2 - s1) / da
!    if ( ds <= 1.0e-30 ) ds = 1.0e-30
!    return xsect%sFull * ds / xsect%aFull
!}
!*/
!
double precision function circ_getAofY(xsect, y)
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    double precision :: yNorm
    yNorm = y / xsect%yFull
    circ_getAofY = xsect%aFull * lookup(yNorm, xs_A_Circ, N_A_Circ)
    return
end function circ_getAofY
!
!
!=============================================================================
!  FILLED_CIRCULAR functions 
!=============================================================================

double precision function filled_circ_getYofA(xsect, a)
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: a

    double precision :: y, ma
    ma = a

    ! --- temporarily remove filled portion of circle
    xsect%yFull = xsect%yFull + xsect%yBot
    xsect%aFull = xsect%aFull + xsect%aBot
    ma = ma + xsect%aBot

    ! --- find depth in unfilled circle
    y = circ_getYofA(xsect, ma)

    ! --- restore original values
    y = y - xsect%yBot
    xsect%yFull = xsect%yFull - xsect%yBot
    xsect%aFull = xsect%aFull - xsect%aBot
    filled_circ_getYofA = y
end function filled_circ_getYofA
!
double precision function filled_circ_getAofY(xsect, y)
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: y

    double precision :: a, ym

    ! --- temporarily remove filled portion of circle
    xsect%yFull = xsect%yFull + xsect%yBot
    xsect%aFull = xsect%aFull + xsect%aBot
    ym = ym + xsect%yBot

    ! --- find area of unfilled circle
    a = circ_getAofY(xsect, ym)

    ! --- restore original values
    a = a - xsect%aBot
    xsect%yFull = xsect%yFull - xsect%yBot
    xsect%aFull = xsect%aFull - xsect%aBot
    filled_circ_getAofY = a
end function filled_circ_getAofY
!
double precision function filled_circ_getRofY(xsect, y)
!TXsect* xsect, double y
    use headers
    use xsectdat
    implicit none
    type(TXsect), intent(inout) :: xsect
    double precision, intent(in) :: y
    double precision :: a, r, p, ym
    ym = y

    ! --- temporarily remove filled portion of circle
    xsect%yFull =xsect%yFull + xsect%yBot
    xsect%aFull =xsect%aFull + xsect%aBot
    ym = ym + xsect%yBot

    ! --- get area,  hyd. radius & wetted perimeter of unfilled circle
    a = circ_getAofY(xsect, ym)
    r = 0.25 * xsect%yFull * lookup(y/xsect%yFull, xs_R_Circ, N_R_Circ)
    p = (a/r)

    ! --- reduce area and wetted perimeter by amount of filled circle
    !     (rBot = filled perimeter, sBot = filled width)
    a = a - xsect%aBot
    p = p - xsect%rBot + xsect%sBot

    ! --- compute actual hyd. radius & restore xsect parameters
    r = a / p
    xsect%yFull = xsect%yFull - xsect%yBot
    xsect%aFull = xsect%aFull - xsect%aBot
    
    filled_circ_getRofY = r
    return
end function filled_circ_getRofY
!
!
!!=============================================================================
!!  Special functions for circular cross sections
!!=============================================================================
!
double precision function getYcircular(alpha)
    implicit none
    double precision, intent(in) :: alpha
    double precision :: theta
    if ( alpha >= 1.0 ) then
       getYcircular = 1.0
       return
    end if
    if ( alpha <= 0.0 ) then
      getYcircular = 0.0
      return
    end if
    if ( alpha <= 1.0e-5 ) then
        theta = (37.6911*alpha) ** (1./3.)
        getYcircular = theta * theta / 16.0
    end if
    theta = getThetaOfAlpha(alpha)
    getYcircular = (1.0 - cos(theta/2.)) / 2.0
end function getYcircular
!
double precision function getScircular(alpha)
    use consts
    use headers
    implicit none
    double precision, intent(in) :: alpha
    double precision :: theta
    if ( alpha >= 1.0 ) then 
       getScircular = 1.0
       return
    end if
    if ( alpha <= 0.0 ) then
       getScircular = 0.0
       return
    end if
    if ( alpha <= 1.0e-5 ) then
        theta = (37.6911*alpha) ** (1./3.)
        getScircular = (theta ** (13./3.)) / 124.4797
        return
    end if
    theta = getThetaOfAlpha(alpha)
    getScircular = ((theta - sin(theta)) ** (5./3.)) / (2.0 * PI) / (theta ** (2./3.))
end function getScircular

double precision function getAcircular( psi)
    implicit none
    double precision, intent(in) :: psi
    double precision :: theta
    if ( psi >= 1.0 ) then
      getAcircular = 1.0
      return
    end if
    if ( psi <= 0.0 ) then
      getAcircular = 0.0
      return
    end if
    if ( psi <= 1.0e-6 ) then
        theta = (124.4797*psi) ** (3./13.)
        getAcircular = theta*theta*theta / 37.6911
    end if
    theta = getThetaOfPsi(psi)
    getAcircular = (theta - sin(theta)) / (2.0 * PI)
end function getAcircular

double precision function getThetaOfAlpha(alpha)
    use headers
    implicit none
    double precision, intent(in) :: alpha
    integer :: k
    double precision :: theta, theta1, ap, d

    if ( alpha > 0.04 ) then
       theta = 1.2 + 5.08 * (alpha - 0.04) / 0.96
    else 
       theta = 0.031715 - 12.79384 * alpha + 8.28479 * sqrt(alpha)
    end if
    theta1 = theta
    ap  = (2.0*PI) * alpha
    do k = 1, 40
        d = - (ap - theta + sin(theta)) / (1.0 - cos(theta))
        ! --- modification to improve convergence for large theta
        if ( d > 1.0 ) d = SIGN( 1.0, d )
        theta = theta - d
        if ( abs(d) <= 0.0001 ) then 
           getThetaOfAlpha = theta
           return
        end if
    end do
    getThetaOfAlpha = theta1
    return
end function getThetaOfAlpha
!
double precision function getThetaOfPsi(psi)
    implicit none
    double precision, intent(in) :: psi
    integer ::    k
    double precision :: theta, theta1, ap, tt, tt23, t3, d

    if      (psi > 0.90)  then
       theta = 4.17 + 1.12 * (psi - 0.90) / 0.176
    else if (psi > 0.5)   then
       theta = 3.14 + 1.03 * (psi - 0.5) / 0.4
    else if (psi > 0.015) then
       theta = 1.2 + 1.94 * (psi - 0.015) / 0.485
    else                  
       theta = 0.12103 - 55.5075 * psi + 15.62254 * sqrt(psi)
    end if
    theta1 = theta
    ap     = (2.0*PI) * psi

    do k = 1, 40
        theta    = abs(theta)
        tt       = theta - sin(theta)
        tt23     = tt ** (2./3.)
        t3       = theta ** (1./3.)
        d        = ap * theta / t3 - tt * tt23
        d        = d / ( ap*(2./3.)/t3 - (5./3.)*tt23*(1.0-cos(theta)) )
        theta    = theta - d
        if ( abs(d) <= 0.0001 ) then
           getThetaOfPsi = theta
           return
        end if
    end do
    getThetaOfPsi = theta1
end function getThetaOfPsi
!
!!=============================================================================

end module
