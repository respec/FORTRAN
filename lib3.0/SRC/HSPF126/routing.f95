module modRouting
integer, parameter :: dpr = kind(1.d0)

!-----------------------------------------------------------------------------
!     Constants 
!-----------------------------------------------------------------------------
real(kind=dpr), parameter :: LATERAL_FLOW_TOL = 0.5  ! for steady state (cfs)         !(5.0.012 - LR)
real(kind=dpr), parameter :: FLOW_ERR_TOL = 0.05     ! for steady state               !(5.0.012 - LR)

!-----------------------------------------------------------------------------
! Shared variables
!-----------------------------------------------------------------------------
integer, save, dimension(:), allocatable :: SortedLinks
logical, save ::  InSteadyState

contains

!=============================================================================

subroutine removeStorageLosses()
!
!  Input:   routingStep = routing time step (sec)
!  Output:  none
!  Purpose: adds mass lost from storage nodes to evaporation & infiltration
!           over current time step to overall mass balance.
!
    use headers
    use modMassbal
    integer :: i, j, p
    real(kind=dp) :: vRatio, losses
    losses = 0.0

    ! --- check each storage node
    do i = 1, Nobjects(E_NODE)
        if (Node(i)%datatype == E_STORAGE) then
            ! --- update total system storage losses
            losses = losses + Storage(Node(i)%subIndex)%losses

            ! --- adjust storage concentrations for any evaporation loss
            if ( Nobjects(E_POLLUT) > 0 .and. Node(i)%newVolume > FUDGE ) then
                j = Node(i)%subIndex
                vRatio = 1.0 + (Storage(j)%evapLoss / Node(i)%newVolume)
                do p =1, Nobjects(E_POLLUT)
                    Node(i)%newQual(p) = Node(i)%newQual(p) * vRatio
                end do
            end if
        end if
    end do
    call massbal_addNodeLosses(losses)
end subroutine removeStorageLosses

subroutine removeOutflows()
!
!  Input:   none
!  Output:  none
!  Purpose: finds flows that leave the system and adds these to mass
!           balance totals.
!
    use headers
    use modMassbal
    implicit none
    integer :: i, p
    integer :: isFlooded
    real(kind=dpr) :: q, w
    
    real(kind=dpr) :: node_getSystemOutflow

    do i =1, Nobjects(E_NODE)
        ! --- determine flows leaving the system
        q = node_getSystemOutflow(i, isFlooded)
        if ( q /= 0.0 ) then
            call massbal_addOutflowFlow(q, isFlooded)
            do p =1, Nobjects(E_POLLUT)
                w = q * Node(i)%newQual(p)
                call massbal_addOutflowQual(p, w, isFlooded)
            end do
        end if
    end do
end subroutine removeOutflows

!!=============================================================================
!
subroutine addExternalInflows(currentDate)
!
!  Input:   currentDate = current date/time
!  Output:  none
!  Purpose: adds direct external inflows to nodes at current date.
!
    use headers
    use modInflow
    use modMassbal
    implicit none
    
    real(kind=dpr), intent(in) :: currentDate
    integer :: j, p
    real(kind=dpr) :: q, w
    
    type(TExtInflow), pointer :: inflow

    ! --- for each node with a defined external inflow
    do j =1, Nobjects(E_NODE)
        inflow => Node(j)%extInflow
        if ( .not. associated(inflow) ) cycle ! !inflow

        ! --- get flow inflow
        q = 0.0
        do while(associated(inflow))
            if ( inflow%datatype == FLOW_INFLOW ) then
                q = inflow_getExtInflow(inflow, currentDate)
                exit
            else
                if (associated(inflow%next)) then
                   inflow => inflow%next
                else
                   nullify(inflow)
                   exit
                end if
            end if
        end do
        if ( abs(q) < FLOW_TOL ) q = 0.0

        ! --- add flow inflow to node's lateral inflow
        Node(j)%newLatFlow = Node(j)%newLatFlow + q
        call massbal_addInflowFlow(EXTERNAL_INFLOW, q)

        ! --- add on any inflow (i.e., reverse flow) through an outfall       !(5.0.014 - LR)
        if ( Node(j)%datatype == E_OUTFALL .and. Node(j)%oldNetInflow < 0.0 ) then       !(5.0.014 - LR)
            q = q - Node(j)%oldNetInflow                                      !(5.0.014 - LR)
        end if                                                                !(5.0.014 - LR)

        ! --- get pollutant mass inflows
        inflow => Node(j)%extInflow
        do while ( associated(inflow) )
            if ( inflow%datatype /= FLOW_INFLOW ) then
                p = inflow%param
                w = inflow_getExtInflow(inflow, currentDate)
                if ( inflow%datatype == CONCEN_INFLOW ) w = w * q
                Node(j)%newQual(p) = Node(j)%newQual(p) + w
                call massbal_addInflowQual(EXTERNAL_INFLOW, p, w)
            end if
            if (associated(inflow%next)) then
                inflow => inflow%next
            else
                nullify(inflow)
                exit
            end if
        end do
    end do
end subroutine addExternalInflows

!=============================================================================

real(kind=dpr) function routing_getRoutingStep(routingModel, fixedStep)
!
!  Input:   routingModel = routing method code
!           fixedStep = user-supplied time step (sec)
!  Output:  returns a routing time step (sec)
!  Purpose: determines time step used for flow routing at current time period.
!
    use headers
    implicit none
    integer, intent(in) :: routingModel
    real(kind=dpr), intent(in) :: fixedStep
    
    real(kind=dpr) :: flowrout_getRoutingStep
    
    if ( Nobjects(LINK) == 0 ) then
        routing_getRoutingStep = fixedStep
    else 
        routing_getRoutingStep = flowrout_getRoutingStep(routingModel, fixedStep)
    end if
end function routing_getRoutingStep

!=============================================================================

integer function routing_open(routingModel)
!
!  Input:   routingModel = routing method code
!  Output:  returns an error code
!  Purpose: initializes the routing analyzer.
!
    use consts
    use enums
    use headers
    use modToposort
    use report
    use treatmnt
    implicit none
    integer, intent(in) :: routingModel
    integer :: mstat
    !  --- initialize steady state indicator
    InSteadyState = .FALSE.
    !write(24,*) ' in routing_open'

    ! --- open treatment system !NOTE: not doing this for this project
!    if ( .not. treatmnt_open() ) then 
!        routing_open = ErrorCode
!        return
!    end if

    !write(24,*) ' in routing_open 2'
    ! --- topologically sort the links
    if (allocated(SortedLinks)) then
      deallocate(SortedLinks)
    end if 
    if ( Nobjects(LINK) > 0 ) then
        allocate(SortedLinks(Nobjects(LINK)), stat=mstat)
        if ( mstat /= 0 ) then
            call report_writeErrorMsg(ERR_MEMORY, '')
            routing_open = ErrorCode
            return
        end if
        call toposort_sortLinks(SortedLinks)
        if ( ErrorCode /= 0 ) then
            routing_open = ErrorCode
            return
        end if
    end if
    !write(24,*) ' in routing_open 3'

    ! --- open any routing interface files
    !call iface_openRoutingFiles() !TODO: later
    if ( ErrorCode /=0 ) then
        routing_open = ErrorCode
        return
    end if

    ! --- open hot start files
!    if ( .not. openHotstartFile1() ) then
!       routing_open = ErrorCode
!       return
!    end if
!    if ( .not.openHotstartFile2() ) then
!        routing_open = ErrorCode
!        return
!    end if

    ! --- initialize the flow routing model
    call flowrout_init(routingModel)
    !write(24,*) ' in routing_open 4'
    routing_open = ErrorCode
    return
end function routing_open

subroutine routing_execute(routingModel, routingStep, outfl, sdatim)
!
!  Input:   routingModel = routing method code
!           routingStep = routing time step (sec)
!           outfl = hspf output file unit number
!           sdatim = starting date/time of hspf step
!  Output:  none
!  Purpose: executes the routing process at the current time period.
!
    use consts
    use enums
    use headers
    use modDateTime
    use modMassbal
    use modLink
    use modStats
    implicit none
    
    integer, intent(in) :: routingModel
    integer, intent(in) :: outfl
    integer, intent(in) :: sdatim(5)
    real(kind=dpr), intent(in) :: routingStep
    integer ::      j
    integer ::      mstepCount
    integer ::      actionCount
    integer ::      nSeci
    real(kind=dpr) :: currentDate,nSec
    real(kind=dpr) :: stepFlowError                                                    !(5.0.012 - LR)
    character*30   :: t30
    character*10, allocatable, dimension(:) :: t10
    integer ::      sdat(6)
    integer ::      newdat(6)
    integer ::      lastwrite
    integer ::      toutfl
    
    integer :: flowrout_execute !TODO: this is for .NET compile
    mstepCount = 1
    actionCount = 0
 
    ! --- update continuity with current state
    !     applied over 1/2 of time step
    if ( ErrorCode /= 0 ) return !has error
    call massbal_updateRoutingTotals(routingStep/2.)

    ! --- evaluate control rules at current date and elapsed time
    currentDate = getDateTime(StartDateTime, NewRoutingTime)
    do j=1, Nobjects(LINK)
       call link_setTargetSetting(j)                 !(5.0.010 - LR)
    end do
    
!    call controls_evaluate(currentDate, currentDate - StartDateTime, &        !(5.0.010 - LR)
!                     &routingStep/SECperDAY)                                  !(5.0.010 - LR)
    
    do j=1, Nobjects(LINK)
        if ( arrLink(j)%targetSetting /= arrLink(j)%setting ) then  !(5.0.010 - LR)         
            call link_setSetting(j, routingStep)                    !(5.0.010 - LR)
            actionCount = actionCount + 1                           !(5.0.010 - LR)
        end if                                                      !(5.0.010 - LR)
    end do                                                          !(5.0.010 - LR)

    ! --- update value of elapsed routing time (in milliseconds)
    OldRoutingTime = NewRoutingTime
    NewRoutingTime = NewRoutingTime + 1000.0 * routingStep
    currentDate = getDateTime(StartDateTime, NewRoutingTime)

    ! --- initialize mass balance totals for time step
    stepFlowError = massbal_getStepFlowError()                                !(5.0.012 - LR)
    call massbal_initTimeStepTotals()

    ! --- replace old water quality state with new state
    if ( Nobjects(E_POLLUT) > 0 ) then
        do j=1, Nobjects(E_NODE)
           call node_setOldQualState(j)
        end do
        do j=1, Nobjects(LINK)
           call link_setOldQualState(j)
        end do
    end if

    ! --- add lateral inflows to nodes
    do j = 1, Nobjects(E_NODE)
        Node(j)%oldLatFlow  = Node(j)%newLatFlow
        Node(j)%newLatFlow  = 0.0
    end do
    call addExternalInflows(currentDate)
!    call addDryWeatherInflows(currentDate)
!    call addWetWeatherInflows(NewRoutingTime)
!    call addGroundwaterInflows(NewRoutingTime)
!    call addRdiiInflows(currentDate)
!    call addIfaceInflows(currentDate)

    ! --- check if can skip steady state periods
    if ( SkipSteadyState ) then
        if ( abs(OldRoutingTime - 0.0) < tiny(1.0) .or. &
            &actionCount > 0 .or. &
            &abs(stepFlowError) > FLOW_ERR_TOL .or. &       !(5.0.012 - LR)
            &systemHasChanged(routingModel) ) then
            InSteadyState = .FALSE.
        else
            InSteadyState = .TRUE.
        end if
    end if

    ! --- find new hydraulic state if system has changed
    if ( .not.InSteadyState ) then
        ! --- replace old hydraulic state values with current ones
        do j = 1, Nobjects(LINK)
          call link_setOldHydState(j)
        end do
        
        do j = 1, Nobjects(E_NODE)
            call node_setOldHydState(j)
            call node_initInflow(j, routingStep)
        end do

        ! --- route flow through the drainage network
        if ( Nobjects(LINK) > 0 ) then
            mstepCount = flowrout_execute(Nobjects(LINK), SortedLinks, routingModel, routingStep)
        end if
    end if

    ! --- route quality through the drainage network
    if ( Nobjects(E_POLLUT) > 0 .and. .not.IgnoreQuality ) then                            !(5.0.014 - LR)
        call qualrout_execute(routingStep)
    end if

    nSec = (CurrentDate - 2.0) * 86400.0
    nSeci = nint(nSec)
    sdat(1) = sdatim(1)
    sdat(2) = sdatim(2)
    sdat(3) = sdatim(3)
    sdat(4) = sdatim(4)
    sdat(5) = sdatim(5)
    sdat(6) = 0
    call timadd(sdat,1,1,nSeci,newdat)

    if ( Nobjects(E_POLLUT) > 0 ) then
        if (outfl.LT.0) then
          if (nSec .LE. ReportStep) then
            toutfl = -1*outfl
            write(t30,'(6I5)') newdat(1),newdat(2),newdat(3),newdat(4),newdat(5),newdat(6)
            WRITE(toutfl,'(A30,400(F10.3))') t30,(node(j)%inflow,j=1,Nobjects(E_NODE)), &
                                                (node(j)%newDepth,j=1,Nobjects(E_NODE)), &
                                                (node(j)%newVolume,j=1,Nobjects(E_NODE)), &
                                                (node(j)%newQual(1),j=1,Nobjects(E_NODE))
          end if 
        elseif (outfl.GT.0) then
          lastwrite = (ReportStep*2) + 1
          if (nSec .GT. ReportStep .AND. nSec .LE. lastwrite) then
            write(t30,'(6I5)') newdat(1),newdat(2),newdat(3),newdat(4),newdat(5),newdat(6)
            WRITE(outfl,'(A30,400(F10.3))') t30,(node(j)%inflow,j=1,Nobjects(E_NODE)), &
                                                (node(j)%newDepth,j=1,Nobjects(E_NODE)), &
                                                (node(j)%newVolume,j=1,Nobjects(E_NODE)), & 
                                                (node(j)%newQual(1),j=1,Nobjects(E_NODE))
          end if 
        end if
    else
        if (outfl.LT.0) then
          if (nSec .LE. ReportStep) then
            toutfl = -1*outfl
            write(t30,'(6I5)') newdat(1),newdat(2),newdat(3),newdat(4),newdat(5),newdat(6)
            WRITE(toutfl,'(A30,300(F10.3))') t30,(node(j)%inflow,j=1,Nobjects(E_NODE)), &
                                                (node(j)%newDepth,j=1,Nobjects(E_NODE)), &
                                                (node(j)%newVolume,j=1,Nobjects(E_NODE))
          end if 
        elseif (outfl.GT.0) then
          lastwrite = (ReportStep*2) + 1
          if (nSec .GT. ReportStep .AND. nSec .LE. lastwrite) then
            write(t30,'(6I5)') newdat(1),newdat(2),newdat(3),newdat(4),newdat(5),newdat(6)
            WRITE(outfl,'(A30,300(F10.3))') t30,(node(j)%inflow,j=1,Nobjects(E_NODE)), &
                                                (node(j)%newDepth,j=1,Nobjects(E_NODE)), &
                                                (node(j)%newVolume,j=1,Nobjects(E_NODE))
          end if 
        end if
    end if 

    ! --- remove evaporation, infiltration & system outflows from nodes       !(5.0.015 - LR)
!    call removeStorageLosses()                                                     !(5.0.019 - LR)
    call removeOutflows()
	
    ! --- update continuity with new totals
    !     applied over 1/2 of routing step
    call massbal_updateRoutingTotals(routingStep/2.)

    ! --- update summary statistics
    if ( RptFlags%flowStats .and. Nobjects(LINK) > 0 ) then
        call stats_updateFlowStats(routingStep, currentDate, mstepCount, InSteadyState)
    end if
end subroutine routing_execute


!=============================================================================

logical function systemHasChanged(routingModel)
!
!  Input:   none
!  Output:  returns TRUE if external inflows or hydraulics have changed
!           from the previous time step
!  Purpose: checks if the hydraulic state of the system has changed from
!           the previous time step.
!
    use consts
    use enums
    use headers
    implicit none
    integer, intent(in) :: routingModel
    integer :: j                                                       !(5.0.012 - LR)
    real(kind=dpr) :: diff

    ! --- check if external inflows or outflows have changed           !(5.0.012 - LR)
    do j=1,Nobjects(E_NODE)
        diff = Node(j)%oldLatFlow - Node(j)%newLatFlow
        if ( abs(diff) > LATERAL_FLOW_TOL ) then
           systemHasChanged = .true. !(5.0.012 - LR)
           return                      
        end if
        if ( Node(j)%datatype == E_OUTFALL .or. Node(j)%degree == 0 ) then              !(5.0.012 - LR)
            diff = Node(j)%oldFlowInflow - Node(j)%inflow                     !(5.0.012 - LR)
            if ( abs(diff) > LATERAL_FLOW_TOL ) then
                systemHasChanged = .true. !(5.0.012 - LR)
                return
            end if
        end if                                                                      !(5.0.012 - LR)
    end do
    systemHasChanged = .false.
    return
end function systemHasChanged
end module
