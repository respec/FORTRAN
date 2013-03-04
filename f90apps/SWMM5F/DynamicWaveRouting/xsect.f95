module modXsect
!-----------------------------------------------------------------------------
! Constants
!-----------------------------------------------------------------------------
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
contains
logical function xsect_isOpen(xtype)
!
!  Input:   type = type of xsection shape
!  Output:  returns 1 if xsection is open, 0 if not
!  Purpose: determines if a xsection type is open or closed.
!
    use DataSizeSpecs
    integer(kind=K2), intent(in) :: xtype
    if (Amax(xtype) >= 1.0) then
       xsect_isOpen = .true.
    else
       xsect_isOpen = .false.
    end if
    return
end function xsect_isOpen
end module
