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

!=============================================================================

double precision function link_getYcrit(j, q)
!
!  Input:   j = link index
!           q = link flow rate (cfs)
!  Output:  returns critical depth (ft)
!  Purpose: computes critical depth for given flow rate.
!
    use headers
    integer, intent(in) :: j
    double precision, intent(in) :: q
   !link_getYcrit = xsect_getYcrit(&arrLink(j)%xsect, q)
    link_getYcrit = xsect_getYcrit(arrLink(j)%xsect, q)
end function link_getYcrit

!=============================================================================

double precision function link_getYnorm(j, q)
!
!  Input:   j = link index
!           q = link flow rate (cfs)
!  Output:  returns normal depth (ft)
!  Purpose: computes normal depth for given flow rate.
!
    use headers
    integer, intent(in) :: j
    double precision, intent(in) :: q
    integer ::    k
    double precision :: s, a, y, mq
    mq = q

    if ( arrLink(j)%datatype /= E_CONDUIT ) then
        link_getYnorm = 0.0
        return
    end if
    if ( arrLink(j)%xsect%datatype == DUMMY ) then
        link_getYnorm = 0.0
        return
    end if
    mq = fabs(mq)
    if ( mq <= 0.0 ) then
        link_getYnorm = 0.0
        return
    end if
    k = arrLink(j)%subIndex
    if ( mq > Conduit(k)%qMax ) then
        link_getYnorm = arrLink(j)%xsect%yFull
        return
    end if
    s = mq / Conduit(k)%beta
    a = xsect_getAofS(arrLink(j)%xsect, s) !xsect_getAofS(&arrLink(j)%xsect, s)
    y = xsect_getYofA(arrLink(j)%xsect, a) !xsect_getYofA(&arrLink(j)%xsect, a)
    link_getYnorm = y
    return
end function link_getYnorm

!=============================================================================

subroutine link_setOutfallDepth(j)
!
!  Input:   j = link index
!  Output:  none
!  Purpose: sets depth at outfall node connected to link j.
!
    use headers
    integer, intent(in) :: j
    integer ::     k                         ! conduit index
    integer ::     n                         ! outfall node index
    double precision ::  z                         ! invert offset height (ft)
    double precision :: q                         ! flow rate (cfs)
    double precision :: yCrit               ! critical flow depth (ft)
    double precision :: yNorm               ! normal flow depth (ft)

    yCrit = 0.0
    yNorm = 0.0

    ! --- find which end node of link is an outfall
    if ( Node(arrLink(j)%node2)%datatype == E_OUTFALL ) then
        n = arrLink(j)%node2
        z = arrLink(j)%offset2
    else if ( Node(arrLink(j)%node1)%datatype == E_OUTFALL ) then
        n = arrLink(j)%node1
        z = arrLink(j)%offset1
    else 
        return
    end if
    
    ! --- find both normal & critical depth for current flow
    if ( arrLink(j)%datatype == E_CONDUIT ) then
        k = arrLink(j)%subIndex
        q = fabs(arrLink(j)%newFlow / Conduit(k)%barrels)
        yNorm = link_getYnorm(j, q)
        yCrit = link_getYcrit(j, q)
    end if

    ! --- set new depth at node
    call node_setOutletDepth(n, yNorm, yCrit, z)
end subroutine link_setOutfallDepth

end module
