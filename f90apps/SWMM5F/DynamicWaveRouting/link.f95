module modLink

contains
!=============================================================================
!!  This function was re-named and re-written for release 5.0.014.  !!     !(5.0.014 - LR)
logical function link_setFlapGate( j,  n1,  n2,  q)
!
!  Input:   j = link index
!           n1 = index of node on upstream end of link
!           n2 = index of node on downstream end of link 
!           q = signed flow value (value and units don't matter) 
!  Output:  returns TRUE if there is reverse flow through a flap gate
!           associated with the link. 
!  Purpose: based on the sign of the flow, determines if a flap gate
!           associated with the link should close or not.
!
    use headers
    integer, intent(in) :: j, n1, n2
    double precision, intent(in) :: q
    
    integer :: n
     n = -1

    ! --- check for reverse flow through link's flap gate
    if ( arrLink(j)%hasFlapGate ) then
        if ( q * arrLink(j)%direction < 0.0 ) then
            link_setFlapGate = .true.
            return
        end if
    end if

    ! --- check for Outfall with flap gate node on inflow end of link
    if ( q < 0.0 ) n = n2
    if ( q > 0.0 ) n = n1
    if ( n >= 0 .and. &
        &Node(n)%datatype == E_OUTFALL .and. &
        &Outfall(Node(n)%subIndex)%hasFlapGate ) then
        link_setFlapGate = .true.
        return
     end if
     link_setFlapGate = .false.
     return
end function link_setFlapGate

end module
