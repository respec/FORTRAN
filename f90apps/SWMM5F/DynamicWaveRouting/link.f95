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
    use enums
    use consts
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
    use enums
    use consts
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
    use enums
    use consts
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
    use enums
    use consts
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

!=============================================================================

subroutine link_setParams(j, datatype, n1, n2, k, x)
!
!  Input:   j   = arrLink index
!           type = arrLink type code
!           n1   = index of upstream node
!           n2   = index of downstream node
!           k    = index of arrLink's sub-type
!           x    = array of parameter values
!  Output:  none
!  Purpose: sets parameters for a arrLink.
!
    use consts
    use enums
    use headers
    
    integer, intent(in) :: j, n1, n2, k, datatype
    double precision, dimension(:), intent(in) :: x

    arrLink(j)%node1       = n1
    arrLink(j)%node2       = n2
    arrLink(j)%datatype    = datatype
    arrLink(j)%subIndex    = k
    arrLink(j)%offset1     = 0.0
    arrLink(j)%offset2     = 0.0
    arrLink(j)%q0          = 0.0
    arrLink(j)%qFull       = 0.0
    arrLink(j)%setting     = 1.0
    arrLink(j)%targetSetting = 1.0                                               !(5.0.010 - LR)
    arrLink(j)%hasFlapGate = .false. !0
    arrLink(j)%qLimit      = 0.0         ! 0 means that no limit is defined
    arrLink(j)%direction   = 1

    select case (datatype)
      case (E_CONDUIT)
        Conduit(k)%length    = x(1) / UCF(LENGTH)
        Conduit(k)%modLength = Conduit(k)%length
        Conduit(k)%roughness = x(2)
        arrLink(j)%offset1      = x(3) / UCF(LENGTH)
        arrLink(j)%offset2      = x(4) / UCF(LENGTH)
        arrLink(j)%q0           = x(6) / UCF(FLOW)
        arrLink(j)%qLimit       = x(6) / UCF(FLOW)
        !break

      case (E_PUMP)
        Pump(k)%pumpCurve    = int(x(1))
        arrLink(j)%hasFlapGate  = .FALSE.
        Pump(k)%initSetting  = x(2)                                           !(5.0.010 - LR)
        Pump(k)%yOn          = x(3) / UCF(LENGTH)                             !(5.0.012 - LR)
        Pump(k)%yOff         = x(4) / UCF(LENGTH)                             !(5.0.012 - LR)
        Pump(k)%xMin         = 0.0                                            !(5.0.014 - LR)
        Pump(k)%xMax         = 0.0                                            !(5.0.014 - LR)
        !break

      case (E_ORIFICE)
        Orifice(k)%datatype      = int(x(1))
        arrLink(j)%offset1      = x(2) / UCF(LENGTH)
        arrLink(j)%offset2      = arrLink(j)%offset1
        Orifice(k)%cDisch    = x(3)
        
        !arrLink(j)%hasFlapGate  = (x(3) > 0.0) ? 1 : 0
        if (x(4) >0.0) then
           arrLink(j)%hasFlapGate  = .true.
        else
           arrLink(j)%hasFlapGate  = .false.
        end if
        
        Orifice(k)%orate     = x(5) * 3600.0                                  !(5.0.010 - LR) 
        !break

      case (E_WEIR)
        Weir(k)%datatype         = int(x(1))
        arrLink(j)%offset1      = x(2) / UCF(LENGTH)
        arrLink(j)%offset2      = arrLink(j)%offset1
        Weir(k)%cDisch1      = x(3)
        !arrLink(j)%hasFlapGate  = (x(3) > 0.0) ? 1 : 0
        if (x(4) > 0.0) then
           arrLink(j)%hasFlapGate  = .true.
        else
           arrLink(j)%hasFlapGate  = .false.
        end if
        Weir(k)%endCon       = x(5)
        Weir(k)%cDisch2      = x(6)
        !break

      case (E_OUTLET)
        arrLink(j)%offset1      = x(1) / UCF(LENGTH)
        arrLink(j)%offset2      = arrLink(j)%offset1
        Outlet(k)%qCoeff     = x(2)
        Outlet(k)%qExpon     = x(3)
        Outlet(k)%qCurve     = int(x(4))
        !arrLink(j)%hasFlapGate  = (x(4) > 0.0) ? 1 : 0
        if (x(5) > 0.0) then
           arrLink(j)%hasFlapGate  = .true.
        else
           arrLink(j)%hasFlapGate  = .false.
        end if
        Outlet(k)%curveType  = int(x(6))                                      !(5.0.014 - LR)

        call xsect_setParams(arrLink(j)%xsect, DUMMY, NULL, 0.0)
        !break

    end select
end subroutine link_setParams

