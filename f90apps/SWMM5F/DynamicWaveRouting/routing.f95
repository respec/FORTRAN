subroutine routing_execute(int routingModel, double routingStep)
!
!  Input:   routingModel = routing method code
!           routingStep = routing time step (sec)
!  Output:  none
!  Purpose: executes the routing process at the current time period.
!
    int      j;
    int      stepCount = 1;
    int      actionCount = 0;                                                  !(5.0.010 - LR)
    DateTime currentDate;
    double   stepFlowError;                                                    !(5.0.012 - LR)

    ! --- update continuity with current state
    !     applied over 1/2 of time step
    if ( ErrorCode ) return;
    massbal_updateRoutingTotals(routingStep/2.);

    ! --- evaluate control rules at current date and elapsed time
    currentDate = getDateTime(NewRoutingTime);
    for (j=0; j<Nobjects[LINK]; j++) link_setTargetSetting(j);                 !(5.0.010 - LR)
    controls_evaluate(currentDate, currentDate - StartDateTime,                !(5.0.010 - LR)
                      routingStep/SECperDAY);                                  !(5.0.010 - LR)
    for (j=0; j<Nobjects[LINK]; j++)                                           !(5.0.010 - LR)
    {                                                                          !(5.0.010 - LR)
        if ( Link[j].targetSetting != Link[j].setting )                        !(5.0.010 - LR)
        {                                                                      !(5.0.010 - LR)
            link_setSetting(j, routingStep);                                   !(5.0.010 - LR)
            actionCount++;                                                     !(5.0.010 - LR)
        }                                                                      !(5.0.010 - LR)
    }                                                                          !(5.0.010 - LR)

    ! --- update value of elapsed routing time (in milliseconds)
    OldRoutingTime = NewRoutingTime;
    NewRoutingTime = NewRoutingTime + 1000.0 * routingStep;
    currentDate = getDateTime(NewRoutingTime);

    ! --- initialize mass balance totals for time step
    stepFlowError = massbal_getStepFlowError();                                !(5.0.012 - LR)
    massbal_initTimeStepTotals();

    ! --- replace old water quality state with new state
    if ( Nobjects[POLLUT] > 0 )
    {
        for (j=0; j<Nobjects[NODE]; j++) node_setOldQualState(j);
        for (j=0; j<Nobjects[LINK]; j++) link_setOldQualState(j);
    }

    ! --- add lateral inflows to nodes
    for (j = 0; j < Nobjects[NODE]; j++)
    {
        Node[j].oldLatFlow  = Node[j].newLatFlow;
        Node[j].newLatFlow  = 0.0;
    }
    addExternalInflows(currentDate);
    addDryWeatherInflows(currentDate);
    addWetWeatherInflows(NewRoutingTime);
    addGroundwaterInflows(NewRoutingTime);
    addRdiiInflows(currentDate);
    addIfaceInflows(currentDate);

    ! --- check if can skip steady state periods
    if ( SkipSteadyState )
    {
        if ( OldRoutingTime == 0.0
        ||   actionCount > 0
        ||   fabs(stepFlowError) > FLOW_ERR_TOL                                !(5.0.012 - LR)
        ||   systemHasChanged(routingModel) ) InSteadyState = FALSE;
        else InSteadyState = TRUE;
    }

    ! --- find new hydraulic state if system has changed
    if ( InSteadyState == FALSE )
    {
        ! --- replace old hydraulic state values with current ones
        for (j = 0; j < Nobjects[LINK]; j++) link_setOldHydState(j);
        for (j = 0; j < Nobjects[NODE]; j++)
        {
            node_setOldHydState(j);
            node_initInflow(j, routingStep);
        }

        ! --- route flow through the drainage network
        if ( Nobjects[LINK] > 0 )
        {
            stepCount = flowrout_execute(SortedLinks, routingModel, routingStep);
        }
    }

    ! --- route quality through the drainage network
    if ( Nobjects[POLLUT] > 0 && !IgnoreQuality )                              !(5.0.014 - LR)
    {
        qualrout_execute(routingStep);
    }

    ! --- remove evaporation, infiltration & system outflows from nodes       !(5.0.015 - LR)
    removeStorageLosses();                                                     !(5.0.019 - LR)
    removeOutflows();
	
    ! --- update continuity with new totals
    !     applied over 1/2 of routing step
    massbal_updateRoutingTotals(routingStep/2.);

    ! --- update summary statistics
    if ( RptFlags.flowStats && Nobjects[LINK] > 0 )
    {
        stats_updateFlowStats(routingStep, currentDate, stepCount, InSteadyState);
    }
end subroutine routing_execute