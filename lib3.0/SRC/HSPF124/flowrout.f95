!-----------------------------------------------------------------------------
!   flowrout.c
!
!   Project:  EPA SWMM5
!   Version:  5.0
!   Date:     6/19/07   (Build 5.0.010)
!             2/4/08    (Build 5.0.012)
!             3/11/08   (Build 5.0.013)
!             1/21/09   (Build 5.0.014)
!             4/10/09   (Build 5.0.015)
!             07/30/10  (Build 5.0.019)
!             09/30/10  (Build 5.0.021)
!   Author:   L. Rossman
!
!   Flow routing functions.
!
!   Note: The Link offset properties z1 and z2 have been renamed to
!         offset1 and offset2 for build 5.0.012.
!-----------------------------------------------------------------------------
!#include <stdlib.h>
!#include <math.h>
!#include "headers.h"

!-----------------------------------------------------------------------------
!  Constants
!-----------------------------------------------------------------------------
!double precision, parameter :: OMEGA_FLOW   = 0.55    ! under-relaxation parameter
!integer, parameter ::  MAXITER = 10      ! max. iterations for storage updating
!double precision, parameter :: STOPTOL = 0.005   ! storage updating stopping tolerance

!-----------------------------------------------------------------------------
!  External functions (declared in funcs.h)
!-----------------------------------------------------------------------------
!  flowrout_init            (called by routing_open)
!  flowrout_close           (called by routing_close)
!  flowrout_getRoutingStep  (called routing_getRoutingStep)
!  flowrout_execute         (called routing_execute)

!-----------------------------------------------------------------------------
!  Local functions
!-----------------------------------------------------------------------------
!static void   initLinkDepths(void);
!static void   initNodeDepths(void);
!static void   initNodes(void);
!static void   initLinks(void);
!static void   validateTreeLayout(void);      
!static void   validateGeneralLayout(void);
!static void   updateStorageState(int i, int j, int links[], double dt);
!static double getStorageOutflow(int node, int j, int links[], double dt);
!static double getLinkInflow(int link, double dt);
!static void   setNewNodeState(int node, double dt);
!static void   setNewLinkState(int link);
!static void   updateNodeDepth(int node, double y);
!static int    steadyflow_execute(int link, double* qin, double* qout);
!

!=============================================================================

subroutine flowrout_init(routingModel)
!
!  Input:   routingModel = routing model code
!  Output:  none
!  Purpose: initializes flow routing system.
!
    use consts
    use enums
    use headers
    use dynwave
    implicit none
    integer, intent(in) :: routingModel
    ! --- initialize for dynamic wave routing 
    if ( routingModel == DW ) then
        ! --- check for valid conveyance network layout
        call validateGeneralLayout()
        call dynwave_init()

        ! --- initialize node & link depths if not using a hotstart file
        if ( Fhotstart1%mode == NO_FILE ) then
            call initNodeDepths()
            call initLinkDepths()
        end if
    else     ! --- validate network layout for kinematic wave routing
       call validateTreeLayout()
    end if
    ! --- initialize node & link volumes
    call initNodes()
    call initLinks()
end subroutine flowrout_init

!=============================================================================

subroutine flowrout_close(routingModel)
!
!  Input:   routingModel = routing method code
!  Output:  none
!  Purpose: closes down routing method used.
!
    use consts
    use enums
    use headers
    use dynwave
    implicit none
    integer, intent(in) :: routingModel
    if ( routingModel == DW ) call dynwave_close()
end subroutine flowrout_close

!=============================================================================

double precision function flowrout_getRoutingStep( routingModel,  fixedStep)
!
!  Input:   routingModel = type of routing method used
!           fixedStep = user-assigned max. routing step (sec)
!  Output:  returns adjusted value of routing time step (sec)
!  Purpose: finds variable time step for dynamic wave routing.
!
    use consts
    use enums
    use headers
    use dynwave
    implicit none
    
    integer, intent(in) :: routingModel
    double precision, intent(in) :: fixedStep
    if ( routingModel == DW ) then
         flowrout_getRoutingStep = dynwave_getRoutingStep(fixedStep)
    else
         flowrout_getRoutingStep = fixedStep
    end if
end function flowrout_getRoutingStep

!=============================================================================

integer function flowrout_execute(nl, links, routingModel, tStep)
!
!  Input:   links = array of link indexes in topo-sorted order
!           routingModel = type of routing method used
!           tStep = routing time step (sec)
!  Output:  returns number of computational mSteps taken
!  Purpose: routes flow through conveyance network over current time step.
!
    use consts
    use enums
    use headers
    use dynwave
    implicit none
    integer, intent(in) :: nl
    integer, dimension(nl), intent(in) :: links
    integer, intent(in) :: routingModel
    double precision, intent(in) :: tStep
    integer ::   i, j
    integer ::   n1                          ! upstream node of link
    double precision :: qin                        ! link inflow (cfs)
    double precision :: qout                       ! link outflow (cfs)
    double precision :: mSteps                      ! computational step count
        
    integer :: steadyflow_execute, kinwave_execute
    double precision :: getLinkInflow

    !write(24,*) 'in flowrout_execute 1',links
    !write(24,*) 'in flowrout_execute 1',routingModel
    !write(24,*) 'in flowrout_execute 1',tStep
!!  The code below was modified to initialize overflows.  !!               !(5.0.012 - LR)
    ! --- set overflows to drain any ponded water
    if ( ErrorCode /= 0 ) then
        flowrout_execute = 0
        return
    end if
    do j = 1, Nobjects(E_NODE)
        Node(j)%updated = .FALSE.
        Node(j)%overflow = 0.0
        if ( Node(j)%datatype /= E_STORAGE .and. &
            &Node(j)%newVolume > Node(j)%fullVolume ) then
            Node(j)%overflow = (Node(j)%newVolume - Node(j)%fullVolume)/tStep
        end if
    end do
!!  End of modified code.  !!                                              !(5.0.012 - LR)

    !write(24,*) 'in flowrout_execute 2',links
    ! --- execute dynamic wave routing if called for
    if ( routingModel == DW ) then
        !write(24,*) 'in flowrout_execute 3',links
        mSteps = dynwave_execute(links, tStep)
        !write(24,*) 'in flowrout_execute 4',links
        flowrout_execute = int(mSteps)
        return
    end if

    ! --- otherwise examine each link, moving from upstream to downstream
    mSteps = 0.0
    do i = 1, Nobjects(LINK)
        ! --- see if upstream node is a storage unit whose state needs updating
        j = links(i)
        n1 = arrLink(j)%node1
        !if ( Node(n1)%datatype == E_STORAGE ) call updateStorageState(n1, i, links, tStep)

        ! --- retrieve inflow at upstream end of link
        qin  = getLinkInflow(j, tStep)

        ! route flow through link
        if ( routingModel == SF ) then
                !mSteps = mSteps + steadyflow_execute(j, qin, qout) !&qin, &qout)
        else 
                !mSteps = mSteps + kinwave_execute(j, qin, qout, tStep) !&qin, &qout, tStep)
        end if
        arrLink(j)%newFlow = qout

        ! adjust outflow at upstream node and inflow at downstream node
        Node( arrLink(j)%node1 )%outflow = Node( arrLink(j)%node1 )%outflow + qin
        Node( arrLink(j)%node2 )%inflow  = Node( arrLink(j)%node2 )%inflow + qout
    end do
    if ( Nobjects(LINK) > 0 ) mSteps = mSteps/Nobjects(LINK)

    ! --- update state of each non-updated node and link
    do j=1, Nobjects(E_NODE)
      !call setNewNodeState(j, tStep)
    end do
    do j=1, Nobjects(LINK)
      !call setNewLinkState(j)
    end do
    flowrout_execute = int(mSteps + 0.5)
    !return (int)(mSteps+0.5)
end function flowrout_execute

!=============================================================================
subroutine validateTreeLayout()
!
!  Input:   none
!  Output:  none
!  Purpose: validates tree-like conveyance system layout used for Steady
!           and Kinematic Wave flow routing
!
    use consts
    use enums
    use headers
    use report
    implicit none
    integer ::   j, node1, node2
    double precision :: elev1, elev2

    ! --- check nodes
    do j = 1, Nobjects(E_NODE)
        select case ( Node(j)%datatype )
          ! --- dividers must have only 2 outlet links
          case (E_DIVIDER)
            if ( Node(j)%degree > 2 ) then
                call report_writeErrorMsg(ERR_DIVIDER, Node(j)%ID)
            end if
            !break

          ! --- outfalls cannot have any outlet links
          case (E_OUTFALL)
            if ( Node(j)%degree > 0 ) then
                call report_writeErrorMsg(ERR_OUTFALL, Node(j)%ID)
            end if
            !break

          ! --- storage nodes can have multiple outlets
          case (E_STORAGE) 
            !break

          ! --- all other nodes allowed only one outlet link
          case default
            if ( Node(j)%degree > 1 ) then
                call report_writeErrorMsg(ERR_MULTI_OUTLET, Node(j)%ID)
            end if
        end select
    end do

    ! ---  check links 
    do j=1, Nobjects(LINK)
        node1 = arrLink(j)%node1
        select case ( arrLink(j)%datatype )
          ! --- non-dummy conduits cannot have adverse slope                  !(5.0.014 - LR)
          case (E_CONDUIT)
            node2 = arrLink(j)%node2
            elev1 = arrLink(j)%offset1 + Node(node1)%invertElev
            elev2 = arrLink(j)%offset2 + Node(node2)%invertElev
            if ( elev1 < elev2 .and. arrLink(j)%xsect%datatype /= DUMMY ) then                !(5.0.014 - LR)
                call report_writeErrorMsg(ERR_SLOPE, arrLink(j)%ID)
            end if
            !break

          ! --- regulator links must be outlets of storage nodes
          case (E_ORIFICE)
          case (E_WEIR)
          case (E_OUTLET)
            if ( Node(node1)%datatype /= E_STORAGE ) then
                call report_writeErrorMsg(ERR_REGULATOR, arrLink(j)%ID)
            end if
        end select
    end do
end subroutine validateTreeLayout

!=============================================================================
subroutine validateGeneralLayout()
!
!  Input:   none
!  Output:  nonw
!  Purpose: validates general conveyance system layout.
!
    use consts
    use enums
    use headers
    use report
    implicit none
    integer :: i, j
    integer :: outletCount
    outletCount = 0
    ! --- use node inflow attribute to count inflow connections
    do i=1, Nobjects(E_NODE)
       Node(i)%inflow = 0.0
    end do

    ! --- examine each link
    do j=1, Nobjects(LINK)
        ! --- update inflow link count of downstream node
        i = arrLink(j)%node1
        if ( Node(i)%datatype /= E_OUTFALL ) i = arrLink(j)%node2
        Node(i)%inflow = Node(i)%inflow + 1.0

        ! --- if link is dummy link or ideal pump then it must                !(5.0.010 - LR)
        !     be the only link exiting the upstream node 
        if ( (arrLink(j)%datatype == E_CONDUIT .and. arrLink(j)%xsect%datatype == DUMMY) .or. &       !(5.0.010 - LR)
            &(arrLink(j)%datatype == E_PUMP .and. &                                          !(5.0.010 - LR)
            & Pump(arrLink(j)%subIndex)%datatype == IDEAL_PUMP) ) then                !(5.0.010 - LR)
            i = arrLink(j)%node1
            if ( arrLink(j)%direction < 0 ) i = arrLink(j)%node2                    !(5.0.014 - LR)
            if ( Node(i)%degree > 1 ) then
                call report_writeErrorMsg(ERR_MULTI_DUMMY_LINK, Node(i)%ID)        !(5.0.014 - LR)
            end if
        end if
    end do

    ! --- check each node to see if it qualifies as an outlet node
    !     (meaning that degree = 0)
    do i=1, Nobjects(E_NODE)
        ! --- if node is of type Outfall, check that it has only 1
        !     connecting link (which can either be an outflow or inflow link)
        if ( Node(i)%datatype == E_OUTFALL ) then
            if ( Node(i)%degree + int(Node(i)%inflow) > 1 ) then
                call report_writeErrorMsg(ERR_OUTFALL, Node(i)%ID)
            else 
                 outletCount = outletCount + 1
            end if
        end if
    end do
    if ( outletCount == 0 ) call report_writeErrorMsg(ERR_NO_OUTLETS, "")

    ! --- reset node inflows back to zero
    do i=1, Nobjects(E_NODE)
        if ( abs(Node(i)%inflow - 0.0) < ZERO ) Node(i)%degree = -Node(i)%degree !Node(i)%inflow = 0
        Node(i)%inflow = 0.0
    end do
end subroutine validateGeneralLayout

!=============================================================================

subroutine initNodeDepths()
!
!  Input:   none
!  Output:  none
!  Purpose: sets initial depth at nodes for Dyn. Wave flow routing.
!
    use consts
    use enums
    use headers
    use modLink
    implicit none
    integer ::   i                           ! link or node index
    integer ::   n                           ! node index
    double precision :: y                          ! node water depth (ft)

    ! --- use Node().inflow as a temporary accumulator for depth in 
    !     connecting links and Node().outflow as a temporary counter
    !     for the number of connecting links
    do i=1, Nobjects(E_NODE)
        Node(i)%inflow  = 0.0
        Node(i)%outflow = 0.0
    end do

    ! --- total up flow depths in all connecting links into nodes
    do i=1, Nobjects(LINK)
        if ( arrLink(i)%newDepth > FUDGE ) then
                y = arrLink(i)%newDepth + arrLink(i)%offset1
        else 
                y = 0.0
        end if
        n = arrLink(i)%node1
        Node(n)%inflow = Node(n)%inflow + y
        Node(n)%outflow =  Node(n)%outflow + 1.0
        n = arrLink(i)%node2
        Node(n)%inflow = Node(n)%inflow + y
        Node(n)%outflow = Node(n)%outflow + 1.0
    end do

    ! --- if no user-supplied depth then set initial depth at non-storage/
    !     non-outfall nodes to average of depths in connecting links
    do i=1, Nobjects(E_NODE)
        if ( Node(i)%datatype == E_OUTFALL ) cycle
        if ( Node(i)%datatype == E_STORAGE ) cycle
        if ( Node(i)%initDepth > 0.0 ) cycle
        if ( Node(i)%outflow > 0.0 ) then
            Node(i)%newDepth = Node(i)%inflow / Node(i)%outflow
        end if
    end do

    ! --- compute initial depths at all outfall nodes
    do i=1, Nobjects(LINK)
       call link_setOutfallDepth(i)
    end do
end subroutine initNodeDepths

!=============================================================================
         
subroutine initLinkDepths()
!
!  Input:   none
!  Output:  none
!  Purpose: sets initial flow depths in conduits under Dyn. Wave routing.
!
    use consts
    use enums
    use headers
    implicit none
    integer ::    i                          ! link index
    double precision :: y, y1, y2                  ! depths (ft)

    ! --- examine each link
    do i=1,Nobjects(LINK)
        ! --- examine each conduit
        if ( arrLink(i)%datatype == E_CONDUIT ) then
            ! --- skip conduits with user-assigned initial flows
            !     (their depths have already been set to normal depth)
            !if ( arrLink(i)%q0 /= 0.0 ) cycle
            if (abs(arrLink(i)%q0) - 0 < tiny(1.0)) cycle

            ! --- set depth to average of depths at end nodes
            y1 = Node(arrLink(i)%node1)%newDepth - arrLink(i)%offset1
            y1 = MAX(y1, 0.0)
            y1 = MIN(y1, arrLink(i)%xsect%yFull)
            y2 = Node(arrLink(i)%node2)%newDepth - arrLink(i)%offset2
            y2 = MAX(y2, 0.0)
            y2 = MIN(y2, arrLink(i)%xsect%yFull)
            y = 0.5 * (y1 + y2)
            y = MAX(y, FUDGE)
            arrLink(i)%newDepth = y
        end if
    end do
end subroutine initLinkDepths

!=============================================================================

subroutine initNodes()
!
!  Input:   none
!  Output:  none
!  Purpose: sets initial inflow/outflow and volume for each node
!
    use consts
    use enums
    use headers
    implicit none
    integer :: i
    
    double precision :: node_getvolume !TODO: this is for .NET compile
    
    do i = 1,Nobjects(E_NODE)
        ! --- set default crown elevations here
        Node(i)%crownElev = Node(i)%invertElev
        !if ( Node(i).type == STORAGE )                                       !(5.0.012 - LR)
        !{                                                                    !(5.0.012 - LR)
        !    Node(i).crownElev += Node(i).fullDepth                          !(5.0.012 - LR)
        !}                                                                    !(5.0.012 - LR)

        ! --- initialize node inflow and outflow                              !(5.0.010 - LR)
        Node(i)%inflow = Node(i)%newLatFlow
        Node(i)%outflow = 0.0
        ! Node(i).newVolume = node_getVolume(i, Node(i).newDepth)            !(5.0.010 - LR)

        ! --- initialize node volume !(5.0.010 - LR)
        Node(i)%newVolume = 0.0
        if ( AllowPonding .and. Node(i)%pondedArea > 0.0 .and. &
            &Node(i)%newDepth > Node(i)%fullDepth ) then
            Node(i)%newVolume = Node(i)%fullVolume + &
                               &(Node(i)%newDepth - Node(i)%fullDepth) * &
                               &Node(i)%pondedArea
        else
            Node(i)%newVolume = node_getVolume(i, Node(i)%newDepth)
        end if
    end do

    ! --- update nodal inflow/outflow at ends of each link
    !     (needed for Steady Flow & Kin. Wave routing)
    do i = 1,Nobjects(LINK)
        if ( arrLink(i)%newFlow >= 0.0 ) then
            Node(arrLink(i)%node1)%outflow = Node(arrLink(i)%node1)%outflow + arrLink(i)%newFlow
            Node(arrLink(i)%node2)%inflow  = Node(arrLink(i)%node2)%inflow + arrLink(i)%newFlow
        else
            Node(arrLink(i)%node1)%inflow   = Node(arrLink(i)%node1)%inflow - arrLink(i)%newFlow
            Node(arrLink(i)%node2)%outflow  = Node(arrLink(i)%node2)%outflow - arrLink(i)%newFlow
        end if
    end do
end subroutine initNodes

!=============================================================================

subroutine initLinks()
!
!  Input:   none
!  Output:  none
!  Purpose: sets initial upstream/downstream conditions in links.
!
!  Note: initNodes() must have been called first to properly
!        initialize each node's crown elevation.
!
    use consts
    use enums
    use headers
    use modXsect
    use modLink
    implicit none
    integer ::    i                          ! link index
    integer ::    j                          ! node index
    integer ::    k                          ! conduit or pump index
    double precision :: z                          ! crown elev. (ft)

    ! --- examine each link
    do i = 1, Nobjects(LINK)
        ! --- examine each conduit
        if ( arrLink(i)%datatype == E_CONDUIT ) then
            ! --- assign initial flow to both ends of conduit
            k = arrLink(i)%subIndex
            Conduit(k)%q1 = arrLink(i)%newFlow / Conduit(k)%barrels
            Conduit(k)%q2 = Conduit(k)%q1

            Conduit(k)%q1Old = Conduit(k)%q1
            Conduit(k)%q2Old = Conduit(k)%q2

            ! --- find areas based on initial flow depth
            !Conduit(k)%a1 = xsect_getAofY(&arrLink(i)%xsect, arrLink(i)%newDepth)
            Conduit(k)%a1 = xsect_getAofY(arrLink(i)%xsect, arrLink(i)%newDepth)
            Conduit(k)%a2 = Conduit(k)%a1
            !Conduit(k).aMid = Conduit(k).a1                                 !(5.0.013 - LR)

            ! --- compute initial volume from area
            arrLink(i)%newVolume = Conduit(k)%a1 * link_getLength(i) * &          !(5.0.015 - LR)
                               &Conduit(k)%barrels
            arrLink(i)%oldVolume = arrLink(i)%newVolume
        end if                                                                      !(5.0.013 - LR)

        ! --- update crown elev. of nodes at either end
        j = arrLink(i)%node1
        z = Node(j)%invertElev + arrLink(i)%offset1 + arrLink(i)%xsect%yFull
        Node(j)%crownElev = MAX(Node(j)%crownElev, z)
        j = arrLink(i)%node2
        z = Node(j)%invertElev + arrLink(i)%offset2 + arrLink(i)%xsect%yFull
        Node(j)%crownElev = MAX(Node(j)%crownElev, z)
        !}                                                                    !(5.0.013 - LR)
    end do
end subroutine initLinks

!=============================================================================

double precision function getLinkInflow(j, dt)
!
!  Input:   j  = link index
!           dt = routing time step (sec)
!  Output:  returns link inflow (cfs)
!  Purpose: finds flow into upstream end of link at current time step under
!           Steady or Kin. Wave routing.
!
    use headers
    use modLink
    implicit none
    integer, intent(in) :: j
    double precision, intent(in) :: dt
    double precision :: q
    integer ::   n1
    double precision :: node_getMaxOutflow
    n1 = arrLink(j)%node1
    
    if ( arrLink(j)%datatype == E_CONDUIT .or. &
        &arrLink(j)%datatype == E_PUMP .or. &
        &Node(n1)%datatype == E_STORAGE ) then
         q = link_getInflow(j)
    else 
         q = 0.0
    end if
    getLinkInflow = node_getMaxOutflow(n1, q, dt)
end function getLinkInflow
!!
!!=============================================================================
!
!subroutine updateStorageState(int i, int j, int links[], double dt)
!!
!!  Input:   i = index of storage node
!!           j = current position in links array
!!           links = array of topo-sorted link indexes
!!           dt = routing time step (sec)
!!  Output:  none
!!  Purpose: updates depth and volume of a storage node using successive
!!           approximation with under-relaxation for Steady or Kin. Wave
!!           routing.
!!
!    int    iter;                       ! iteration counter
!    int    stopped;                    ! TRUE when iterations stop
!    double vFixed;                     ! fixed terms of flow balance eqn.
!    double v2;                         ! new volume estimate (ft3)
!    double d1;                         ! initial value of storage depth (ft)
!    double d2;                         ! updated value of storage depth (ft)
!    double outflow;                    ! outflow rate from storage (cfs)
!
!    ! --- see if storage node needs updating
!    if ( Node[i].type != STORAGE ) return;
!    if ( Node[i].updated ) return;
!
!    ! --- compute terms of flow balance eqn.
!    !       v2 = v1 + (inflow - outflow)*dt
!    !     that do not depend on storage depth at end of time step
!    vFixed = Node[i].oldVolume + 
!             0.5 * (Node[i].oldNetInflow + Node[i].inflow) * dt;
!    d1 = Node[i].newDepth;
!
!    ! --- iterate finding outflow (which depends on depth) and subsequent
!    !     new volume and depth until negligible depth change occurs
!    iter = 1;
!    stopped = FALSE;
!    while ( iter < MAXITER && !stopped )
!    {
!        ! --- find total flow in all outflow links
!        outflow = getStorageOutflow(i, j, links, dt);
!
!        ! --- find new volume from flow balance eqn.
!        v2 = vFixed - 0.5 * outflow * dt - node_getLosses(i, dt);              !(5.0.019 - LR)
!
!!!  The code below was modified to consider ponding.  !!                   !(5.0.012 - LR)
!
!        ! --- limit volume to full volume if no ponding
!        !     and compute overflow rate
!        v2 = MAX(0.0, v2);
!        Node[i].overflow = 0.0;
!        if ( v2 > Node[i].fullVolume )
!        {
!            Node[i].overflow = (v2 - MAX(Node[i].oldVolume,
!                                         Node[i].fullVolume)) / dt;
!            if ( !AllowPonding || Node[i].pondedArea == 0.0 )
!                v2 = Node[i].fullVolume;
!        }
!
!        ! --- update node's volume & depth 
!        Node[i].newVolume = v2;
!        if ( v2 > Node[i].fullVolume ) d2 = node_getPondedDepth(i, v2);
!        else d2 = node_getDepth(i, v2);
!        Node[i].newDepth = d2;
!
!!!  End of updated code.  !!                                               !(5.0.012 - LR)
!
!        ! --- use under-relaxation to estimate new depth value
!        !     and stop if close enough to previous value
!        d2 = (1.0 - OMEGA)*d1 + OMEGA*d2;
!        if ( abs(d2 - d1) <= STOPTOL ) stopped = TRUE;
!
!        ! --- update old depth with new value and continue to iterate
!        Node[i].newDepth = d2;
!        d1 = d2;
!        iter++;
!    }
!
!    ! --- mark node as being updated
!    Node[i].updated = TRUE;
!end subroutine updateStorageState
!
!!=============================================================================

double precision function getStorageOutflow( i,  j,  links,  dt)
!
!  Input:   i = index of storage node
!           j = current position in links array
!           links = array of topo-sorted link indexes
!           dt = routing time step (sec)
!  Output:  returns total outflow from storage node (cfs)
!  Purpose: computes total flow released from a storage node.
!
    use consts
    use enums
    use headers
    implicit none
    integer, intent(in) :: i, j
    integer, dimension(:), intent(in) :: links
    double precision, intent(in) :: dt
    
    integer ::   k, m
    double precision :: outflow
    double precision :: getlinkinflow
    outflow = 0.0

    do k = j, Nobjects(LINK)
        m = links(k)
        if ( arrLink(m)%node1 /= i ) exit
        outflow = outflow + getLinkInflow(m, dt)
    end do
    getStorageOutflow = outflow
    return
end function getStorageOutflow
!
!!=============================================================================
!!subroutine setNewNodeState(int j, double dt)
!!!
!!!  Input:   j  = node index
!!!           dt = time step (sec)
!!!  Output:  none
!!!  Purpose: updates state of node after current time step
!!!           for Steady Flow or Kinematic Wave flow routing.
!!!
!!{
!!    int   canPond;                     ! TRUE if ponding can occur at node  
!!    double newNetInflow;               ! inflow - outflow at node (cfs)
!!
!!    ! --- storage nodes have already been updated                             !(5.0.021 - LR)
!!    if ( Node[j].type == STORAGE ) return;                                     !(5.0.021 - LR)
!!
!!    ! --- update stored volume using mid-point integration
!!    newNetInflow = Node[j].inflow - Node[j].outflow;
!!    Node[j].newVolume = Node[j].oldVolume +
!!                        0.5 * (Node[j].oldNetInflow + newNetInflow) * dt;
!!    if ( Node[j].newVolume < FUDGE ) Node[j].newVolume = 0.0;                  !(5.0.010 - LR)
!!
!!    ! --- determine any overflow lost from system
!!    Node[j].overflow = 0.0;
!!    canPond = (AllowPonding && Node[j].pondedArea > 0.0);
!!
!!!!  The following code was updated to consider all overflow  !!            !(5.0.012 - LR)
!!!!  as flooding whether or not ponding occurs.               !!
!!    if ( Node[j].newVolume > Node[j].fullVolume )
!!    {
!!        Node[j].overflow = (Node[j].newVolume - MAX(Node[j].oldVolume,
!!                            Node[j].fullVolume)) / dt;
!!        if ( Node[j].overflow < FUDGE ) Node[j].overflow = 0.0;
!!        if ( !canPond ) Node[j].newVolume = Node[j].fullVolume;
!!    }
!!!!  End of update.  !!                                                     !(5.0.012 - LR)
!!
!!    ! --- compute a depth from volume
!!    !     (depths at upstream nodes are subsequently adjusted in
!!    !     setNewLinkState to reflect depths in connected conduit)
!!    Node[j].newDepth = node_getDepth(j, Node[j].newVolume);                    !(5.0.019 - LR)
!!end subroutine setNewNodeState
!
!!=============================================================================
!!subroutine setNewLinkState(int j)
!!!
!!!  Input:   j = link index
!!!  Output:  none
!!!  Purpose: updates state of link after current time step under
!!!           Steady Flow or Kinematic Wave flow routing
!!!
!!    int   k;
!!    double a, y1, y2;                                                          !(5.0.013 - LR)
!!
!!    Link[j].newDepth = 0.0;
!!    Link[j].newVolume = 0.0;
!!
!!    if ( Link[j].type == CONDUIT )
!!    {
!!        ! --- find avg. depth from entry/exit conditions
!!        k = Link[j].subIndex;
!!        a = 0.5 * (Conduit[k].a1 + Conduit[k].a2);                             !(5.0.013 - LR)
!!        Link[j].newVolume = a * link_getLength(j) * Conduit[k].barrels;        !(5.0.015 - LR)
!!        y1 = xsect_getYofA(&Link[j].xsect, Conduit[k].a1);
!!        y2 = xsect_getYofA(&Link[j].xsect, Conduit[k].a2);
!!        Link[j].newDepth = 0.5 * (y1 + y2);
!!
!!        ! --- update depths at end nodes
!!        updateNodeDepth(Link[j].node1, y1 + Link[j].offset1);
!!        updateNodeDepth(Link[j].node2, y2 + Link[j].offset2);
!!
!!        ! --- check if capacity limited                                       !(5.0.012 - LR)
!!        if ( Conduit[k].a1 >= Link[j].xsect.aFull )                            !(5.0.012 - LR)
!!             Conduit[k].capacityLimited = TRUE;                                !(5.0.012 - LR)
!!        else Conduit[k].capacityLimited = FALSE;                               !(5.0.012 - LR)
!!    }
!!end subroutine setNewLinkState
!
!!=============================================================================

subroutine updateNodeDepth( i,  y)
!
!  Input:   i = node index
!           y = flow depth (ft)
!  Output:  none
!  Purpose: updates water depth at a node with a possibly higher value.
!
    use consts
    use enums
    use headers
    implicit none
    integer, intent(in) :: i
    double precision, intent(in) :: y
    
    double precision :: ym
    ym = y
    
    ! --- storage nodes were updated elsewhere
    if ( Node(i)%datatype == E_STORAGE ) return

    ! --- if non-outfall node is flooded, then use full depth                 !(5.0.010 - LR)
    if ( Node(i)%datatype /= E_OUTFALL .and. &                                            !(5.0.010 - LR)
        &Node(i)%overflow > 0.0 ) ym = Node(i)%fullDepth                       !(5.0.010 - LR)

    ! --- if current new depth below y
    if ( Node(i)%newDepth < ym ) then
        ! --- update new depth
        Node(i)%newDepth = ym

        ! --- depth cannot exceed full depth (if value exists)
        if ( Node(i)%fullDepth > 0.0 .and. ym > Node(i)%fullDepth ) then
            Node(i)%newDepth = Node(i)%fullDepth
        end if
    end if
end subroutine updateNodeDepth

!=============================================================================

integer function steadyflow_execute(j, qin, qout)
!
!  Input:   j = link index
!           qin = inflow to link (cfs)
!  Output:  qin = adjusted inflow to link (limited by flow capacity) (cfs)
!           qout = link's outflow (cfs)
!           returns 1 if successful
!  Purpose: performs steady flow routing through a single link.
!
    use consts
    use enums
    use headers
    use modXsect
    implicit none
    
    integer, intent(in) :: j
    double precision, intent(inout) :: qin
    double precision, intent(inout) :: qout
    integer ::   k
    double precision :: s
    double precision :: q

    ! --- use Manning eqn. to compute flow area for conduits
    if ( arrLink(j)%datatype == E_CONDUIT ) then
        k = arrLink(j)%subIndex
        q = qin / Conduit(k)%barrels
        if ( arrLink(j)%xsect%datatype == DUMMY ) then
            Conduit(k)%a1 = 0.0                !(5.0.013 - LR)
        else 
            if ( q > arrLink(j)%qFull ) then                                          !(5.0.012 - LR)
                q = arrLink(j)%qFull                                             !(5.0.012 - LR)
                Conduit(k)%a1 = arrLink(j)%xsect%aFull                           !(5.0.013 - LR)
                qin = q * Conduit(k)%barrels
            else
                s = q / Conduit(k)%beta
                !TODO: need to use pointer here??
                !Conduit(k)%a1 = xsect_getAofS(&arrLink(j)%xsect, s)              !(5.0.013 - LR)
                Conduit(k)%a1 = xsect_getAofS(arrLink(j)%xsect, s)
            end if
        end if
        !Conduit(k).a1 = Conduit(k).aMid                                     !(5.0.013 - LR)
        Conduit(k)%a2 = Conduit(k)%a1
        Conduit(k)%q1 = q
        Conduit(k)%q2 = q
        qout = q * Conduit(k)%barrels
    else 
        qout = qin
    end if
    steadyflow_execute = 1
    return
end function steadyflow_execute
!=============================================================================
