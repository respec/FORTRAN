module modStats
!-----------------------------------------------------------------------------
!   stats.c
!
!   Project:  EPA SWMM5
!   Version:  5.0
!   Date:     6/19/07   (Build 5.0.010)
!             2/4/08    (Build 5.0.012)
!             10/9/09   (Build 5.0.017)
!             11/18/09  (Build 5.0.018)
!             07/30/10  (Build 5.0.019)
!             04/20/11  (Build 5.0.022)
!   Author:   L. Rossman (EPA)
!             R. Dickinson (CDM)
!
!   Simulation statistics functions.
!-----------------------------------------------------------------------------
!#define _CRT_SECURE_NO_DEPRECATE
!
!#include <stdlib.h>
!#include <math.h>
!#include "headers.h"
use headers

!-----------------------------------------------------------------------------
!  Shared variables
!-----------------------------------------------------------------------------
integer, parameter :: MAX_STATS = 5
type(TSysStats), save :: SysStats
type(TMaxStats), dimension(MAX_STATS), save :: MaxMassBalErrs
type(TMaxStats), dimension(MAX_STATS), save :: MaxCourantCrit
type(TMaxStats), dimension(MAX_STATS), save :: MaxFlowTurns    !(5.0.010 - LR)
double precision, save :: SysOutfallFlow

!-----------------------------------------------------------------------------
!  Exportable variables (shared with statsrpt.c)
!-----------------------------------------------------------------------------
type(TSubcatchStats), dimension(:), allocatable :: SubcatchStats
type(TNodeStats), dimension(:), allocatable :: NodeStats
type(TLinkStats), dimension(:), allocatable :: LinkStats
type(TStorageStats), dimension(:), allocatable :: StorageStats
type(TOutfallStats), dimension(:), allocatable :: OutfallStats
type(TPumpStats), dimension(:), allocatable :: PumpStats
double precision :: MaxOutfallFlow
double precision :: MaxRunoffFlow

!-----------------------------------------------------------------------------
!  Imported variables
!-----------------------------------------------------------------------------
double precision, dimension(:), allocatable :: NodeInflow     ! defined in massbal.c
double precision, dimension(:), allocatable :: NodeOutflow    ! defined in massbal.c

!-----------------------------------------------------------------------------
!  External functions (declared in funcs.h)
!-----------------------------------------------------------------------------
!  stats_open                    (called from swmm_start in swmm5.c)
!  stats_close                   (called from swmm_end in swmm5.c)
!  stats_report                  (called from swmm_end in swmm5.c)
!  stats_updateSubcatchStats     (called from subcatch_getRunoff)
!  stats_updateFlowStats         (called from routing_execute)
!  stats_updateCriticalTimeCount (called from getVariableStep in dynwave.c)

!-----------------------------------------------------------------------------
!  Local functions
!-----------------------------------------------------------------------------
!static void stats_updateNodeStats(int node, double tStep, DateTime aDate)
!static void stats_updateLinkStats(int link, double tStep, DateTime aDate)
!static void stats_findMaxStats(void)
!static void stats_updateMaxStats(TMaxStats maxStats(), int i, int j, double x)

contains
!=============================================================================

integer function stats_open()
!
!  Input:   none
!  Output:  returns an error code
!  Purpose: opens the simulation statistics system.
!
    use headers
    use report
    implicit none
    integer :: j, k, lStat1, lStat2

    ! --- set all pointers to NULL
    deallocate(NodeStats)
    deallocate(LinkStats)
    deallocate(StorageStats)
    deallocate(OutfallStats)
    deallocate(PumpStats)    !(5.0.012 - LR)

    ! --- allocate memory for & initialize subcatchment statistics
    deallocate(SubcatchStats)
    if ( Nobjects(E_SUBCATCH) > 0 ) then
        allocate(SubcatchStats(Nobjects(E_SUBCATCH)), stat=lStat1)
        if ( lStat1 /= 0 ) then !SubcatchStats
            call report_writeErrorMsg(ERR_MEMORY, '')
            stats_open = ErrorCode
            return
        end if
        do j=1, Nobjects(E_SUBCATCH)
            SubcatchStats(j)%precip  = 0.0
            SubcatchStats(j)%runon   = 0.0
            SubcatchStats(j)%evap    = 0.0
            SubcatchStats(j)%infil   = 0.0
            SubcatchStats(j)%runoff  = 0.0
            SubcatchStats(j)%maxFlow = 0.0
        end do
    end if

    ! --- allocate memory for node & link stats
    if ( Nobjects(LINK) > 0 ) then
        allocate(NodeStats(Nobjects(E_NODE)), stat=lStat1)
        allocate(LinkStats(Nobjects(LINK)), stat=lStat2)
        if ( lStat1 /= 0 .or. lStat2 /= 0 ) then !NodeStats .or. !LinkStats
            call report_writeErrorMsg(ERR_MEMORY, '')
            stats_open = ErrorCode
            return
        end if
    end if

    ! --- initialize node stats
    if ( allocated(NodeStats)  ) then !  NodeStats is allocated
      do j = 1, Nobjects(E_NODE)
        NodeStats(j)%avgDepth = 0.0
        NodeStats(j)%maxDepth = 0.0
        NodeStats(j)%maxDepthDate = StartDateTime
        NodeStats(j)%maxDepthChange = 0.0                                     !(5.0.012 - LR)
        NodeStats(j)%volFlooded = 0.0
        NodeStats(j)%timeFlooded = 0.0
        NodeStats(j)%timeSurcharged = 0.0                                     !(5.0.012 - LR)
        NodeStats(j)%timeCourantCritical = 0.0
        NodeStats(j)%totLatFlow = 0.0                                         !(5.0.012 - LR)
        NodeStats(j)%maxLatFlow = 0.0
        NodeStats(j)%maxInflow = 0.0
        NodeStats(j)%maxOverflow = 0.0
        NodeStats(j)%maxPondedVol = 0.0                                       !(5.0.012 - LR)
        NodeStats(j)%maxInflowDate = StartDateTime
        NodeStats(j)%maxOverflowDate = StartDateTime
      end do
    end if

    ! --- initialize link stats
    if ( allocated(LinkStats) ) then
      do j =1, Nobjects(LINK)
        LinkStats(j)%maxFlow = 0.0
        LinkStats(j)%maxVeloc = 0.0
        LinkStats(j)%maxDepth = 0.0
        LinkStats(j)%avgFlowChange = 0.0
        LinkStats(j)%avgFroude = 0.0
        LinkStats(j)%timeSurcharged = 0.0
        LinkStats(j)%timeFullUpstream = 0.0                                   !(5.0.012 - LR)
        LinkStats(j)%timeFullDnstream = 0.0                                   !(5.0.012 - LR)
        LinkStats(j)%timeFullFlow = 0.0                                       !(5.0.012 - LR)
        LinkStats(j)%timeCapacityLimited = 0.0                                !(5.0.012 - LR)
        LinkStats(j)%timeCourantCritical = 0.0
        do k=1, MAX_FLOW_CLASSES
            LinkStats(j)%timeInFlowClass(k) = 0.0
        end do
        LinkStats(j)%flowTurns = 0                                            !(5.0.010 - LR)
        LinkStats(j)%flowTurnSign = 0                                         !(5.0.010 - LR)
      end do
    end if

    ! --- allocate memory for & initialize storage unit statistics
    if ( Nnodes(E_STORAGE) > 0 ) then
        allocate(StorageStats(Nnodes(E_STORAGE)), stat=lStat1)
        if ( lStat1 /= 0 ) then
            call report_writeErrorMsg(ERR_MEMORY, '')
            stats_open = ErrorCode
            return
        else 
            do j =1, Nnodes(E_STORAGE)
               StorageStats(j)%avgVol = 0.0
               StorageStats(j)%maxVol = 0.0
               StorageStats(j)%maxFlow = 0.0
               StorageStats(j)%losses = 0.0                                      !(5.0.018-LR)
               StorageStats(j)%maxVolDate = StartDateTime
            end do
        end if
    end if

    ! --- allocate memory for & initialize outfall statistics
    if ( Nnodes(E_OUTFALL) > 0 ) then
        allocate(OutfallStats(Nnodes(E_OUTFALL)), stat=lStat1)
        if ( lStat1 /= 0 ) then
            call report_writeErrorMsg(ERR_MEMORY, '')
            stats_open = ErrorCode
            return
        else 
            do j =1, Nnodes(E_OUTFALL)
                OutfallStats(j)%avgFlow = 0.0
                OutfallStats(j)%maxFlow = 0.0
                OutfallStats(j)%totalPeriods = 0
                if ( Nobjects(E_POLLUT) > 0) then
!                    allocate(OutfallStats(j)%totalLoad(Nobjects(E_POLLUT)), stat=lStat1)
!                    if ( lStat1 /= 0 ) then !OutfallStats(j).totalLoad
!                        call report_writeErrorMsg(ERR_MEMORY, '')
!                        stats_open = ErrorCode
!                        return
!                    end if
                    do k=1, Nobjects(E_POLLUT)
                        if ( k <= MAX_NUM_POLLUTANTS) OutfallStats(j)%totalLoad(k) = 0.0
                    end do
!                else 
!                    deallocate(OutfallStats(j)%totalLoad)
                end if
            end do
        end if
    end if

    ! --- allocate memory & initialize pumping statistics                     !(5.0.012 - LR)
    if ( Nlinks(E_PUMP) > 0 ) then                                              !(5.0.012 - LR)
        allocate(PumpStats(Nlinks(E_PUMP)), stat=lStat1)   !(5.0.012 - LR)
        if ( lStat1 /= 0 ) then                                               !(5.0.012 - LR)
            call report_writeErrorMsg(ERR_MEMORY, '')                              !(5.0.012 - LR)
            stats_open = ErrorCode                                                  !(5.0.012 - LR)
            return
        else 
          do j =1, Nlinks(E_PUMP)
            PumpStats(j)%utilized = 0.0                                       !(5.0.012 - LR)
            PumpStats(j)%minFlow  = 0.0                                       !(5.0.022 - LR)
            PumpStats(j)%avgFlow  = 0.0                                       !(5.0.012 - LR)
            PumpStats(j)%maxFlow  = 0.0                                       !(5.0.012 - LR)
            PumpStats(j)%volume   = 0.0                                       !(5.0.012 - LR)
            PumpStats(j)%energy   = 0.0                                       !(5.0.012 - LR)
            PumpStats(j)%startUps = 0                                         !(5.0.022 - LR)
            PumpStats(j)%offCurveLow = 0.0                                    !(5.0.022 - LR)
            PumpStats(j)%offCurveHigh = 0.0                                   !(5.0.022 - LR)
          end do  
        end if                                                             !(5.0.012 - LR) 
    end if                                                                          !(5.0.012 - LR)

    ! --- initialize system stats
    MaxRunoffFlow = 0.0
    MaxOutfallFlow = 0.0
    SysStats%maxTimeStep = 0.0
    SysStats%minTimeStep = RouteStep
    SysStats%avgTimeStep = 0.0
    SysStats%avgStepCount = 0.0
    SysStats%steadyStateCount = 0.0
    
    stats_open = 0
end function stats_open

!=============================================================================

!!  This function was completely re-written !/                              !(5.0.012 - LR)

subroutine stats_report()
!
!  Input:   none
!  Output:  none
!  Purpose: reports simulation statistics.
!
   !use headers
   use report
   implicit none

    ! --- report flow routing accuracy statistics
    if ( Nobjects(LINK) > 0 .and. RouteModel /= NO_ROUTING ) then
        call stats_findMaxStats()
        call report_writeMaxStats(MaxMassBalErrs, MaxCourantCrit, MAX_STATS)
        call report_writeMaxFlowTurns(MaxFlowTurns, MAX_STATS)
        call report_writeSysStats(SysStats) !&SysStats
    end if

    ! --- report summary statistics
    call statsrpt_writeReport()
end subroutine stats_report
!=============================================================================

subroutine stats_close()
!
!  Input:   none
!  Output:  
!  Purpose: closes the simulation statistics system.
!
    use headers
    implicit none
    integer :: j

    deallocate(SubcatchStats)
    deallocate(NodeStats)
    deallocate(LinkStats)
    deallocate(StorageStats) 
    if ( ubound(OutfallStats, 1) > 0 ) then
!        do j=1, Nnodes(E_OUTFALL)
!            deallocate(OutfallStats(j)%totalLoad)
!        end do
        deallocate(OutfallStats)
    end if
    deallocate(PumpStats)                                              !(5.0.012 - LR)
end subroutine stats_close

!!=============================================================================
!
!!!  This function was completely re-written !/                              !(5.0.012 - LR)
!
!void  stats_report()
!!
!!  Input:   none
!!  Output:  none
!!  Purpose: reports simulation statistics.
!!
!{
!
!    ! --- report flow routing accuracy statistics
!    if ( Nobjects(LINK) > 0 .and. RouteModel != NO_ROUTING )
!    {
!        stats_findMaxStats()
!        report_writeMaxStats(MaxMassBalErrs, MaxCourantCrit, MAX_STATS)
!        report_writeMaxFlowTurns(MaxFlowTurns, MAX_STATS)
!        report_writeSysStats(&SysStats)
!    }
!
!    ! --- report summary statistics
!    statsrpt_writeReport()
!}
!
!!=============================================================================
!
!void   stats_updateSubcatchStats(int j, double rainVol, double runonVol,
!                                 double evapVol, double infilVol,
!                                 double runoffVol, double runoff)
!!
!!  Input:   j = subcatchment index
!
!!!  Volumes are now passed in as ft3 instead of ft3/ft2  !!                !(5.0.019 - LR)
!
!!           rainVol   = rainfall + snowfall volume (ft3)
!!           runonVol  = runon volume from other subcatchments (ft3)
!!           evapVol   = evaporation volume (ft3)
!!           infilVol  = infiltration volume (ft3)
!!           runoffVol = runoff volume (ft3)
!!           runoff    = runoff rate (cfs)
!!  Output:  none
!!  Purpose: updates totals of runoff components for a specific subcatchment.
!!
!{
!    SubcatchStats(j).precip += rainVol
!    SubcatchStats(j).runon  += runonVol
!    SubcatchStats(j).evap   += evapVol
!    SubcatchStats(j).infil  += infilVol
!    SubcatchStats(j).runoff += runoffVol
!    SubcatchStats(j).maxFlow = MAX(SubcatchStats(j).maxFlow, runoff)
!}
!
!!=============================================================================
!
!void  stats_updateMaxRunoff()
!!
!!   Input:   none
!!   Output:  updates global variable MaxRunoffFlow
!!   Purpose: updates value of maximum system runoff rate.
!!
!{
!    int j
!    double sysRunoff = 0.0
!    
!    for (j=0 j<Nobjects(SUBCATCH) j++) sysRunoff += Subcatch(j).newRunoff
!    MaxRunoffFlow = MAX(MaxRunoffFlow, sysRunoff)
!}    
!
!=============================================================================

subroutine stats_updateFlowStats(tStep, aDate, aStepCount, steadyState)
!
!  Input:   tStep = routing time step (sec)
!           aDate = current date/time
!           aStepCount = # steps required to solve routing at current time period
!           steadyState = TRUE if steady flow conditions exist
!  Output:  none
!  Purpose: updates various flow routing statistics at current time period.
!
    use headers
    implicit none
    double precision, intent(in) :: tStep, aDate
    integer, intent(in) :: aStepCount
    logical, intent(in) :: steadyState
    integer :: j

    ! --- update stats only after reporting period begins
    if ( aDate < ReportStart ) return
    SysOutfallFlow = 0.0

    ! --- update node & link stats
    do j=1, Nobjects(E_NODE)
        call stats_updateNodeStats(j, tStep, aDate)
    end do
    do j=1, Nobjects(LINK)
        call stats_updateLinkStats(j, tStep, aDate)
    end do

    ! --- update time step stats
    !     (skip initial time step for min. value)
    if ( StepCount > 1 ) then
        SysStats%minTimeStep = MIN(SysStats%minTimeStep, tStep)
    end if
    SysStats%avgTimeStep = SysStats%avgTimeStep + tStep
    SysStats%maxTimeStep = MAX(SysStats%maxTimeStep, tStep)

    ! --- update iteration step count stats
    SysStats%avgStepCount = SysStats%avgStepCount + aStepCount

    ! --- update count of times in steady state
    if (steadyState) SysStats%steadyStateCount = SysStats%steadyStateCount + 1

    ! --- update max. system outfall flow
    MaxOutfallFlow = MAX(MaxOutfallFlow, SysOutfallFlow)
end subroutine stats_updateFlowStats
!
!!=============================================================================
!   
!void stats_updateCriticalTimeCount(int node, int link)
!!
!!  Input:   node = node index
!!           link = link index
!!  Output:  none
!!  Purpose: updates count of times a node or link was time step-critical.
!!
!{
!    if      ( node >= 0 ) NodeStats(node).timeCourantCritical += 1.0
!    else if ( link >= 0 ) LinkStats(link).timeCourantCritical += 1.0
!}
!
!=============================================================================

subroutine stats_updateNodeStats(j, tStep, aDate)
!
!  Input:   j = node index
!           tStep = routing time step (sec)
!           aDate = current date/time
!  Output:  none
!  Purpose: updates flow statistics for a node.
!
    use headers
    implicit none
    integer, intent(in) :: j
    double precision, intent(in) :: tStep, aDate
    integer :: k, p
    double precision :: newVolume, newDepth, delta
    logical :: canPond !(5.0.019 - LR)
    if (AllowPonding .and. Node(j)%pondedArea > 0.0) Then
       canPond = .true.
    else
       canPond = .false.
    end if
    !canPond = (AllowPonding .and. Node(j).pondedArea > 0.0)               !(5.0.019 - LR)
    newVolume = Node(j)%newVolume
    newDepth = Node(j)%newDepth

    ! --- update depth statistics                                             !(5.0.019 - LR)
    NodeStats(j)%avgDepth = NodeStats(j)%avgDepth + newDepth
    if ( newDepth > NodeStats(j)%maxDepth ) then
        NodeStats(j)%maxDepth = newDepth
        NodeStats(j)%maxDepthDate = aDate
    end if
    delta = abs(newDepth - Node(j)%oldDepth)                                 !(5.0.012 - LR)
    if ( delta > NodeStats(j)%maxDepthChange ) NodeStats(j)%maxDepthChange = delta !(5.0.012 - LR)
    
!!  Following code segment was modified for release 5.0.019.  !!           !(5.0.019 - LR)
    ! --- update flooding, ponding, and surcharge statistics
    if ( Node(j)%datatype /= E_OUTFALL ) then
        if ( newVolume > Node(j)%fullVolume .or. Node(j)%overflow > 0.0 ) then
            NodeStats(j)%timeFlooded =NodeStats(j)%timeFlooded + tStep
            NodeStats(j)%volFlooded =NodeStats(j)%volFlooded + Node(j)%overflow * tStep
            if ( canPond ) NodeStats(j)%maxPondedVol = MAX(NodeStats(j)%maxPondedVol, &
                                                         &(newVolume - Node(j)%fullVolume))
        end if
        if ( newDepth + Node(j)%invertElev + FUDGE >= Node(j)%crownElev ) then
                NodeStats(j)%timeSurcharged = NodeStats(j)%timeSurcharged + tStep                          !(5.0.022 - LR)
        end if
    end if
!!  End of modified code segment.  !!                                      !(5.0.019 - LR)

    ! --- update storage statistics
    if ( Node(j)%datatype == E_STORAGE ) then
        k = Node(j)%subIndex
        StorageStats(k)%avgVol = StorageStats(k)%avgVol + newVolume
        StorageStats(k)%losses = StorageStats(k)%losses + Storage(Node(j)%subIndex)%losses            !(5.0.018-LR)
        newVolume = MIN(newVolume, Node(j)%fullVolume)                        !(5.0.019 - LR)
        if ( newVolume > StorageStats(k)%maxVol ) then
            StorageStats(k)%maxVol = newVolume
            StorageStats(k)%maxVolDate = aDate
        end if
        StorageStats(k)%maxFlow = MAX(StorageStats(k)%maxFlow, Node(j)%outflow)
    end if

    ! --- update outfall statistics
    if ( Node(j)%datatype == E_OUTFALL .and. Node(j)%inflow >= MIN_RUNOFF_FLOW ) then
        k = Node(j)%subIndex
        OutfallStats(k)%avgFlow = OutfallStats(k)%avgFlow + Node(j)%inflow
        OutfallStats(k)%maxFlow = MAX(OutfallStats(k)%maxFlow, Node(j)%inflow)
        OutfallStats(k)%totalPeriods = OutfallStats(k)%totalPeriods + 1
        do p=1, Nobjects(E_POLLUT)
            OutfallStats(k)%totalLoad(p) = OutfallStats(k)%totalLoad(p) + Node(j)%inflow * &
               &Node(j)%newQual(p) * LperFT3 * tStep * Pollut(p)%mcf
        end do
        SysOutfallFlow =SysOutfallFlow + Node(j)%inflow
    end if

    ! --- update inflow statistics                                            !(5.0.019 - LR)
    NodeStats(j)%totLatFlow =NodeStats(j)%totLatFlow + ( (Node(j)%oldLatFlow + Node(j)%newLatFlow) * &  !(5.0.012 - LR)
                                &0.5 * tStep )                                !(5.0.012 - LR)

    NodeStats(j)%maxLatFlow = MAX(Node(j)%newLatFlow, NodeStats(j)%maxLatFlow)
    if ( Node(j)%inflow > NodeStats(j)%maxInflow ) then
        NodeStats(j)%maxInflow = Node(j)%inflow
        NodeStats(j)%maxInflowDate = aDate
    end if

    ! --- update overflow statistics                                          !(5.0.019 - LR)
    if ( Node(j)%overflow > NodeStats(j)%maxOverflow ) then
        NodeStats(j)%maxOverflow = Node(j)%overflow
        NodeStats(j)%maxOverflowDate = aDate
    end if
end subroutine stats_updateNodeStats
!
!=============================================================================

subroutine stats_updateLinkStats(j, tStep, aDate)
!
!  Input:   j = link index
!           tStep = routing time step (sec)
!           aDate = current date/time
!  Output:  none
!  Purpose: updates flow statistics for a link.
!
    use headers
    use modLink
    implicit none
    integer, intent(in) :: j
    double precision, intent(in) :: tStep, aDate
    integer ::    k
    double precision :: q, v
    double precision :: dq       !(5.0.010 - LR)

    ! --- update max. flow
    dq = arrLink(j)%newFlow - arrLink(j)%oldFlow                                    !(5.0.010 - LR)
    q = abs(arrLink(j)%newFlow)
    if ( q > LinkStats(j)%maxFlow ) then
        LinkStats(j)%maxFlow = q
        LinkStats(j)%maxFlowDate = aDate
    end if

    ! --- update max. velocity
    v = link_getVelocity(j, q, arrLink(j)%newDepth)
    if ( v > LinkStats(j)%maxVeloc ) then
        LinkStats(j)%maxVeloc = v
        LinkStats(j)%maxVelocDate = aDate
    end if

    ! --- update max. depth
    if ( arrLink(j)%newDepth > LinkStats(j)%maxDepth ) then
        LinkStats(j)%maxDepth = arrLink(j)%newDepth
    end if

    if ( arrLink(j)%datatype == E_PUMP ) then
        if ( q >= arrLink(j)%qFull ) LinkStats(j)%timeFullFlow =LinkStats(j)%timeFullFlow + tStep  !(5.0.012 - LR)
        if ( q > MIN_RUNOFF_FLOW ) then                                        !(5.0.012 - LR)
            k = arrLink(j)%subIndex                                              !(5.0.012 - LR)
            PumpStats(k)%minFlow = MIN(PumpStats(k)%minFlow, q)               !(5.0.022 - LR)
            PumpStats(k)%maxFlow = LinkStats(j)%maxFlow                       !(5.0.012 - LR)
            PumpStats(k)%avgFlow =PumpStats(k)%avgFlow + q                    !(5.0.012 - LR)
            PumpStats(k)%volume = PumpStats(k)%volume + q*tStep               !(5.0.012 - LR)
            PumpStats(k)%utilized =PumpStats(k)%utilized + tStep              !(5.0.012 - LR)
            PumpStats(k)%energy =PumpStats(k)%energy + link_getPower(j)*tStep/3600.0  !(5.0.012 - LR)
            if ( arrLink(j)%flowClass == DN_DRY ) then                            !(5.0.022 - LR)
                PumpStats(k)%offCurveLow =PumpStats(k)%offCurveLow + tStep        !(5.0.022 - LR)
            end if
            if ( arrLink(j)%flowClass == UP_DRY ) then                           !(5.0.022 - LR)
                PumpStats(k)%offCurveHigh =PumpStats(k)%offCurveHigh + tStep     !(5.0.022 - LR)
            end if
            if ( arrLink(j)%oldFlow < MIN_RUNOFF_FLOW ) then                    !(5.0.022 - LR)
                PumpStats(k)%startUps = PumpStats(k)%startUps + 1               !(5.0.022 - LR)
            end if
            PumpStats(k)%totalPeriods = PumpStats(k)%totalPeriods + 1         !(5.0.012 - LR)
            LinkStats(j)%timeSurcharged =LinkStats(j)%timeSurcharged + tStep      !(5.0.012 - LR)
            LinkStats(j)%timeFullUpstream =LinkStats(j)%timeFullUpstream + tStep  !(5.0.012 - LR)
            LinkStats(j)%timeFullDnstream =LinkStats(j)%timeFullDnstream + tStep  !(5.0.012 - LR)
        end if                                                                !(5.0.012 - LR) 
    else if ( arrLink(j)%datatype == E_CONDUIT ) then
        ! --- update sums used to compute avg. Fr and flow change
        LinkStats(j)%avgFroude =LinkStats(j)%avgFroude + arrLink(j)%froude 
        LinkStats(j)%avgFlowChange =LinkStats(j)%avgFlowChange + abs(dq)
    
        ! --- update flow classification distribution
        k = arrLink(j)%flowClass
        if ( k >= 0 .and. k < MAX_FLOW_CLASSES ) then
            LinkStats(j)%timeInFlowClass(k) = LinkStats(j)%timeInFlowClass(k) + 1
        end if

        ! --- update time conduit is full
        k = arrLink(j)%subIndex
        if ( q >= arrLink(j)%qFull ) LinkStats(j)%timeFullFlow =LinkStats(j)%timeFullFlow + tStep  !(5.0.012 - LR) )
        if ( Conduit(k)%capacityLimited ) &                                    !(5.0.012 - LR)
           &LinkStats(j)%timeCapacityLimited =LinkStats(j)%timeCapacityLimited + tStep  !(5.0.012 - LR)
        if ( arrLink(j)%newDepth >= arrLink(j)%xsect%yFull ) then                   !(5.0.012 - LR)
            LinkStats(j)%timeSurcharged =LinkStats(j)%timeSurcharged + tStep
            LinkStats(j)%timeFullUpstream = LinkStats(j)%timeFullUpstream + tStep  !(5.0.012 - LR)
            LinkStats(j)%timeFullDnstream = LinkStats(j)%timeFullDnstream + tStep  !(5.0.012 - LR)
        else if ( Conduit(k)%a1 >= arrLink(j)%xsect%aFull ) then                  !(5.0.012 - LR)
            LinkStats(j)%timeFullUpstream =LinkStats(j)%timeFullUpstream + tStep  !(5.0.012 - LR)
        else if ( Conduit(k)%a2 >= arrLink(j)%xsect%aFull ) then                  !(5.0.012 - LR)
            LinkStats(j)%timeFullDnstream =LinkStats(j)%timeFullDnstream + tStep  !(5.0.012 - LR)
        end if
    end if

    ! --- update flow turn count                                              !(5.0.010 - LR)
    k = LinkStats(j)%flowTurnSign                                             !(5.0.010 - LR)
    LinkStats(j)%flowTurnSign = SGN(dq)                                       !(5.0.010 - LR)
    if ( abs(dq) > 0.001 .and.  k * LinkStats(j)%flowTurnSign < 0 ) &            !(5.0.010 - LR)
           &LinkStats(j)%flowTurns = LinkStats(j)%flowTurns + 1               !(5.0.010 - LR)
end subroutine stats_updateLinkStats
!
!!=============================================================================
!
!void  stats_findMaxStats()
!!
!!  Input:   none
!!  Output:  none
!!  Purpose: finds nodes & links with highest mass balance errors
!!           & highest times Courant time-step critical.
!!
!{
!    int    j
!    double x
!
!    ! --- initialize max. stats arrays
!    for (j=0 j<MAX_STATS j++)
!    {
!        MaxMassBalErrs(j).objType = E_NODE
!        MaxMassBalErrs(j).index   = -1
!        MaxMassBalErrs(j).value   = -1.0                                      !(5.0.010 - LR)
!        MaxCourantCrit(j).index   = -1
!        MaxCourantCrit(j).value   = -1.0                                      !(5.0.010 - LR)
!        MaxFlowTurns(j).index     = -1                                        !(5.0.010 - LR)
!        MaxFlowTurns(j).value     = -1.0                                      !(5.0.010 - LR)
!    }
!
!    ! --- find links with most flow turns                                     !(5.0.010 - LR)
!    if ( StepCount > 2 )                                                       !(5.0.010 - LR)
!    {                                                                          !(5.0.010 - LR) 
!        for (j=0 j<Nobjects(LINK) j++)                                       !(5.0.010 - LR)
!        {                                                                      !(5.0.010 - LR)
!            x = 100.0 * LinkStats(j).flowTurns / (2./3.*(StepCount-2))        !(5.0.010 - LR)
!            stats_updateMaxStats(MaxFlowTurns, LINK, j, x)                    !(5.0.010 - LR)
!        }                                                                      !(5.0.010 - LR)
!    }                                                                          !(5.0.010 - LR)
!
!    ! --- find nodes with largest mass balance errors
!    for (j=0 j<Nobjects(E_NODE) j++)
!    {
!        ! --- skip terminal nodes and nodes with negligible inflow
!        if ( Node(j).degree <= 0  ) continue
!        if ( NodeInflow(j) <= 0.1 ) continue
!
!        ! --- evaluate mass balance error
!        !     (Note: NodeInflow & NodeOutflow include any initial and final   !(5.0.018 - LR)
!        !            stored volumes, respectively).                           !(5.0.018 - LR)
!        if ( NodeInflow(j)  > 0.0 )
!            x = 1.0 - NodeOutflow(j) / NodeInflow(j)                          !(5.0.018 - LR)
!        else if ( NodeOutflow(j) > 0.0 ) x = -1.0
!        else                             x = 0.0
!        stats_updateMaxStats(MaxMassBalErrs, E_NODE, j, 100.0*x)
!    }
!
!    ! --- stop if not using a variable time step
!    if ( RouteModel != DW .or. CourantFactor == 0.0 ) return
!
!    ! --- find nodes most frequently Courant critical
!    for (j=0 j<Nobjects(E_NODE) j++)
!    {
!        x = NodeStats(j).timeCourantCritical / StepCount
!        stats_updateMaxStats(MaxCourantCrit, E_NODE, j, 100.0*x)
!    }
!
!    ! --- find links most frequently Courant critical
!    for (j=0 j<Nobjects(LINK) j++)
!    {
!        x = LinkStats(j).timeCourantCritical / StepCount
!        stats_updateMaxStats(MaxCourantCrit, LINK, j, 100.0*x)
!    }
!}
!
!!=============================================================================
!
!void  stats_updateMaxStats(TMaxStats maxStats(), int i, int j, double x)
!!
!!  Input:   maxStats() = array of critical statistics values
!!           i = object category (E_NODE or LINK)
!!           j = object index
!!           x = value of statistic for the object
!!  Output:  none
!!  Purpose: updates the collection of most critical statistics
!!
!{
!    int   k
!    TMaxStats maxStats1, maxStats2
!    maxStats1.objType = i
!    maxStats1.index   = j
!    maxStats1.value   = x
!    for (k=0 k<MAX_STATS k++)
!    {
!        if ( fabs(maxStats1.value) > fabs(maxStats(k).value) )
!        {
!            maxStats2 = maxStats(k)
!            maxStats(k) = maxStats1
!            maxStats1 = maxStats2
!        }
!    }
!}
!
!!=============================================================================
end module
