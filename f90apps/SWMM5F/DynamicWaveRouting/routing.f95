module modRouting
!-----------------------------------------------------------------------------
!     Constants 
!-----------------------------------------------------------------------------
double precision, parameter :: LATERAL_FLOW_TOL = 0.5  ! for steady state (cfs)         !(5.0.012 - LR)
double precision, parameter :: FLOW_ERR_TOL = 0.05     ! for steady state               !(5.0.012 - LR)

!-----------------------------------------------------------------------------
! Shared variables
!-----------------------------------------------------------------------------
integer, save, dimension(:), allocatable :: SortedLinks
logical, save ::  InSteadyState

contains

!=============================================================================

double precision function routing_getRoutingStep(routingModel, fixedStep)
!
!  Input:   routingModel = routing method code
!           fixedStep = user-supplied time step (sec)
!  Output:  returns a routing time step (sec)
!  Purpose: determines time step used for flow routing at current time period.
!
    use headers
    implicit none
    integer, intent(in) :: routingModel
    double precision, intent(in) :: fixedStep
    
    double precision :: flowrout_getRoutingStep
    
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
    implicit none
    integer, intent(in) :: routingModel
    integer :: mstat
    !  --- initialize steady state indicator
    InSteadyState = .FALSE.

    ! --- open treatment system !NOTE: not doing this for this project
!    if ( .not. treatmnt_open() ) then 
!        routing_open = ErrorCode
!        return
!    end if

    ! --- topologically sort the links
    deallocate(SortedLinks)
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
    routing_open = ErrorCode
    return
end function routing_open

subroutine routing_execute(routingModel, routingStep)
!
!  Input:   routingModel = routing method code
!           routingStep = routing time step (sec)
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
    double precision, intent(in) :: routingStep
    integer ::      j
    integer ::      mstepCount
    integer ::      actionCount
    double precision :: currentDate
    double precision :: stepFlowError                                                    !(5.0.012 - LR)
    
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
!    call addExternalInflows(currentDate)
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
            stepCount = flowrout_execute(SortedLinks, routingModel, routingStep)
        end if
    end if

    ! --- route quality through the drainage network
    if ( Nobjects(E_POLLUT) > 0 .and. .not.IgnoreQuality ) then                            !(5.0.014 - LR)
        call qualrout_execute(routingStep)
    end if

    ! --- remove evaporation, infiltration & system outflows from nodes       !(5.0.015 - LR)
!    call removeStorageLosses()                                                     !(5.0.019 - LR)
!    call removeOutflows()
	
    ! --- update continuity with new totals
    !     applied over 1/2 of routing step
    call massbal_updateRoutingTotals(routingStep/2.)

    ! --- update summary statistics
    if ( RptFlags%flowStats .and. Nobjects(LINK) > 0 ) then
        call stats_updateFlowStats(routingStep, currentDate, stepCount, InSteadyState)
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
    integer ::    j                                                                  !(5.0.012 - LR)
    double precision :: diff

    ! --- check if external inflows or outflows have changed                  !(5.0.012 - LR)
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
