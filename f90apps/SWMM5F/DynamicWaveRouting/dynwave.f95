include 'headers.f95'
include 'xsect.f95'
include 'link.f95'
module dynwave
! -----------------------------------------------------------------------------
!    dynwave.c
! 
!    Project:  EPA SWMM5
!    Version:  5.0
!    Date:     3/11/08  (5.0.013)
!              1/21/09  (5.0.014)
!              4/10/09  (5.0.015)
!              6/22/09  (5.0.016)
!              10/7/09  (5.0.017)
!              11/18/09 (5.0.018)
!              07/30/10 (5.0.019)
!              04/20/11 (5.0.022)
!    Author:   L. Rossman
!              R. Dickinson
! 
!    Dynamic wave flow routing functions.
! 
!    This module solves the dynamic wave flow routing equations using
!    Picard Iterations (i.e., a method of successive approximations)
!    to solve the explicit form of the continuity and momentum equations
!    for conduits.
! 
!    All previous change comments prior to release 5.0.013 were removed
!    to improve readability.
! 
! -----------------------------------------------------------------------------

! -----------------------------------------------------------------------------
!      Constants for dynwave
! -----------------------------------------------------------------------------
double precision, parameter :: MINSURFAREA =  12.566  !  min. nodal surface area (~4 ft diam.)
double precision, parameter :: MAXVELOCITY =  50.     !  max. allowable velocity (ft/sec)
double precision, parameter ::  MINTIMESTEP =  0.5     !  min. time step (sec)
double precision, parameter ::  POMEGA       =  0.5     !  under-relaxation parameter
double precision, parameter ::  STOP_TOL    =  0.005   !  Picard iteration stop criterion
integer, parameter :: MAXSTEPS    =  8       !  max. number of Picard iterations (5.0.019 - LR)

! -----------------------------------------------------------------------------
!   Data Structures
! -----------------------------------------------------------------------------
type TXnode 
    logical*1 :: converged                 !  TRUE if iterations for a node done
    double precision :: newSurfArea               !  current surface area (ft2)
    double precision :: oldSurfArea               !  previous surface area (ft2)
    double precision :: sumdqdh                   !  sum of dqdh from adjoining links
    double precision :: dYdT                      !  change in depth w.r.t. time (ft/sec)
end type TXnode

type TXlink
    logical*1 bypassed                  !  TRUE if can bypass calcs. for a link
    double precision :: surfArea1                 !  surf. area at upstrm end of link (ft2)
    double precision :: surfArea2                 !  surf. area at dnstrm end of link (ft2)
end type TXlink

! -----------------------------------------------------------------------------
!   Shared Variables
! -----------------------------------------------------------------------------
double precision ::  MinSurfAreaFt2         !  actual min. nodal surface area (ft2)
double precision ::  VariableStep           !  size of variable time step (sec)
double precision ::  Omega                  !  actual under-relaxation parameter
double precision ::  CriticalDepth          !  critical flow depth (ft)
double precision ::  NormalDepth            !  normal flow depth (ft)
double precision ::  Fasnh                  !  fraction between norm. & crit. depth
logical ::     Converged              !  TRUE if Picard iterations converged
integer ::    Steps                  !  number of Picard iterations
type(TXnode) :: Xnode
allocatable Xnode(:)
type(TXlink) :: Xlink
allocatable Xlink(:)

! -----------------------------------------------------------------------------
!   External functions (declared in funcs.h)
! -----------------------------------------------------------------------------
!   dynwave_init            (called by flowrout_init)
!   dynwave_getRoutingStep  (called by flowrout_getRoutingStep)
!   dynwave_execute         (called by flowrout_execute)

! -----------------------------------------------------------------------------
!   Function declarations
! -----------------------------------------------------------------------------
!static void   execRoutingStep(int links(), double dt)
!static void   initNodeState(int i)
!static void   findConduitFlow(int i, double dt)
!static void   findNonConduitFlow(int i, double dt)
!static double getModPumpFlow(int i, double q, double dt)
!static void   updateNodeFlows(int i, double q)
!
!static double getConduitFlow(int link, double qin, double dt)
!static int    getFlowClass(int link, double q, double h1, double h2,
!              double y1, double y2)
!static void   findSurfArea(int link, double length, double* h1, double* h2,
!              double* y1, double* y2)
!static double findLocalLosses(int link, double a1, double a2, double aMid,
!              double q)
!static void   findNonConduitSurfArea(int link)
!
!static double getWidth(TXsect* xsect, double y)
!static double getArea(TXsect* xsect, double y)
!static double getHydRad(TXsect* xsect, double y)
!static double checkNormalFlow(int j, double q, double y1, double y2,           ! (5.0.019 - LR)
!              double a1, double r1)                                           ! (5.0.019 - LR)
!
!static void   setNodeDepth(int node, double dt)
!static double getFloodedDepth(int i, int canPond, double dV, double yNew,      ! (5.0.014 - LR)
!              double yMax, double dt)                                         ! (5.0.014 - LR)
!
!static double getVariableStep(double maxStep)
!static double getLinkStep(double tMin, int *minLink)
!static double getNodeStep(double tMin, int *minNode)
!
!static void   checkCapacity(int j)


! =============================================================================
contains
subroutine dynwave_init()
! 
!   Input:   none
!   Output:  none
!   Purpose: initializes dynamic wave routing method.
! 
    use headers
    integer :: i

    VariableStep = 0.0
    if ( MinSurfArea == 0.0 ) then
       MinSurfAreaFt2 = MINSURFAREA
    else 
       MinSurfAreaFt2 = MinSurfArea / UCF(LENGTH) / UCF(LENGTH)
    end if
    allocate (Xnode(Nobjects(E_NODE)))
    allocate (Xlink(Nobjects(LINK)))

    !  --- initialize node surface areas
    do i = 1, Nobjects(E_NODE)
        Xnode(i)%newSurfArea = 0.0
        Xnode(i)%oldSurfArea = 0.0
    end do
    do i = 1, Nobjects(LINK)
        arrLink(i)%flowClass = DRY
        arrLink(i)%dqdh = 0.0
    end do
end subroutine dynwave_init
! =============================================================================

subroutine  dynwave_close()
! 
!   Input:   none
!   Output:  none
!   Purpose: frees memory allocated for dynamic wave routing method.
! 
    deallocate (Xnode)
    deallocate (Xlink)
end subroutine dynwave_close
! =============================================================================

double precision function dynwave_getRoutingStep(fixedStep)
! 
!   Input:   fixedStep = user-supplied fixed time step (sec)
!   Output:  returns routing time step (sec)
!   Purpose: computes variable routing time step if applicable.
! 
    double precision, intent (in) :: fixedStep
    !  --- use user-supplied fixed step if variable step option turned off
    !      or if its smaller than the min. allowable variable time step
    if ( CourantFactor .eq. 0.0 ) then
       dynwave_getRoutingStep = fixedStep
       return
    end if
    if ( fixedStep .lt. MINTIMESTEP ) then
      dynwave_getRoutingStep = fixedStep
      return
    end if

    !  --- at start of simulation (when current variable step is zero)
    !      use the minimum allowable time step
    if ( VariableStep == 0.0 ) then
        VariableStep = MINTIMESTEP
    !  --- otherwise compute variable step based on current flow solution
    else 
        VariableStep = getVariableStep(fixedStep)
    end if

    !  --- adjust step to be a multiple of a millisecond
    VariableStep = floor(1000.0 * VariableStep) / 1000.0
    
    dynwave_getRoutingStep = VariableStep
    return
end function dynwave_getRoutingStep

! =============================================================================

integer function dynwave_execute(links, tStep)
! 
!   Input:   links = array of topo sorted links indexes
!            tStep = time step (sec)
!   Output:  returns number of iterations used
!   Purpose: routes flows through drainage network over current time step.
! 

    use headers
    integer, intent (in) :: links(:)
    double precision, intent (in) :: tStep
    
    integer :: i

    !  --- initialize
    if ( ErrorCode /= 0 ) then
       dynwave_execute = 0
       return
    end if
    
    Steps = 0
    Converged = .FALSE.
    Omega = POMEGA
    do i=1, Nobjects(E_NODE)
        Xnode(i)%converged = .FALSE.
        Xnode(i)%dYdT = 0.0
    end do
    do i=1, Nobjects(LINK)
        Xlink(i)%bypassed = .FALSE.
        Xlink(i)%surfArea1 = 0.0
        Xlink(i)%surfArea2 = 0.0
    end do

    !  --- a2 preserves conduit area from solution at last time step
    do i=1, Nlinks(E_CONDUIT)
      Conduit(i)%a2 = Conduit(i)%a1
    end do
    !  --- keep iterating until convergence 
    do while ( Steps < MAXSTEPS )
        !  --- execute a routing step & check for nodal convergence
        call execRoutingStep(links, tStep)
        Steps = Steps + 1
        if ( Steps > 1 ) then
            if ( Converged ) exit !break

            !  --- check if link calculations can be skipped in next step
            do i=1, Nobjects(LINK)
                if ( Xnode(arrLink(i)%node1)%converged .and. &
                    &Xnode(arrLink(i)%node2)%converged ) then
                  Xlink(i)%bypassed = .true.
                else
                  Xlink(i)%bypassed = .false.
                end if
            end do
        end if
    end do

    !   --- identify any capacity-limited conduits
    do i=1, Nobjects(LINK)
      call checkCapacity(i)
    end do
    dynwave_execute = Steps
    return
end function dynwave_execute

! =============================================================================

subroutine execRoutingStep(links, dt)
! 
!   Input:   links = array of link indexes
!            dt    = time step (sec)
!   Output:  none
!   Purpose: solves momentum eq. in links and continuity eq. at nodes
!            over specified time step.
! 
    use headers
    integer, intent(in) :: links(:)
    double precision, intent (in) :: dt
    integer ::    i                          !  node or link index
    double precision :: yOld                       !  old node depth (ft)

    !  --- re-initialize state of each node
    do i = 1, Nobjects(E_NODE)
       call initNodeState(i)
    end do
    Converged = .true.

    !  --- find new flows in conduit links and non-conduit links
    do i=1, Nobjects(LINK)
      call findConduitFlow(links(i), dt)
    end do
    do i=1, Nobjects(LINK)
      call findNonConduitFlow(links(i), dt)
    end do

    !  --- compute outfall depths based on flow in connecting link
    do i=1, Nobjects(LINK)
      call link_setOutfallDepth(i)
    end do

    !  --- compute new depth for all non-outfall nodes and determine if
    !      depth change from previous iteration is below tolerance
    do i=1, Nobjects(E_NODE)
        if ( Node(i)%datatype .eq. E_OUTFALL ) cycle !continue
        yOld = Node(i)%newDepth
        call setNodeDepth(i, dt)
        Xnode(i)%converged = .true.
        if ( fabs(yOld - Node(i)%newDepth) .gt. STOP_TOL ) then
            Converged = .FALSE.
            Xnode(i)%converged = .false.
        end if
    end do
end subroutine execRoutingStep

! =============================================================================

subroutine initNodeState(i)
! 
!   Input:   i = node index
!   Output:  none
!   Purpose: initializes node's surface area, inflow & outflow
! 
    use headers
    integer, intent(in) :: i
    !  --- initialize nodal surface area
    if ( AllowPonding ) then
        Xnode(i)%newSurfArea = node_getPondedArea(i, Node(i)%newDepth)
    else
        Xnode(i)%newSurfArea = node_getSurfArea(i, Node(i)%newDepth)
    end if
    if ( Xnode(i)%newSurfArea < MinSurfAreaFt2 ) then
        Xnode(i)%newSurfArea = MinSurfAreaFt2
    end if

    !  --- initialize nodal inflow & outflow
    Node(i)%inflow = Node(i)%newLatFlow
    Node(i)%outflow = 0.0
    Xnode(i)%sumdqdh = 0.0
end subroutine initNodeState

! =============================================================================

subroutine findConduitFlow(i, dt)
! 
!   Input:   i = link index
!            dt = time step (sec)
!   Output:  none
!   Purpose: finds new flow in a conduit-type link
! 
    use headers
    integer, intent(in) :: i
    double precision, intent(in) :: dt
    
    double precision :: qOld                       !  old link flow (cfs)
    double precision :: barrels                    !  number of barrels in conduit

    !  --- do nothing if link not a conduit
    if ( arrLink(i)%datatype /= E_CONDUIT .or. arrLink(i)%xsect%datatype == DUMMY) return

    !  --- get link flow from last "full" time step
    qOld = arrLink(i)%oldFlow

    !  --- solve momentum eqn. to update conduit flow
    if ( .not.Xlink(i)%bypassed ) then
        arrLink(i)%dqdh = 0.0
        arrLink(i)%newFlow = getConduitFlow(i, qOld, dt)
    end if
    !  NOTE: if link was bypassed, then its flow and surface area values
    !        from the previous iteration will still be valid.

    !  --- add surf. area contributions to upstream/downstream nodes
    barrels = Conduit(arrLink(i)%subIndex)%barrels
    Xnode(arrLink(i)%node1)%newSurfArea = Xnode(arrLink(i)%node1)%newSurfArea + Xlink(i)%surfArea1 * barrels
    Xnode(arrLink(i)%node2)%newSurfArea = Xnode(arrLink(i)%node2)%newSurfArea + Xlink(i)%surfArea2 * barrels

    !  --- update summed value of dqdh at each end node
    Xnode(arrLink(i)%node1)%sumdqdh = Xnode(arrLink(i)%node1)%sumdqdh + arrLink(i)%dqdh
    Xnode(arrLink(i)%node2)%sumdqdh = Xnode(arrLink(i)%node2)%sumdqdh + arrLink(i)%dqdh

    !  --- update outflow/inflow at upstream/downstream nodes
    call updateNodeFlows(i, arrLink(i)%newFlow)
end subroutine findConduitFlow

! =============================================================================

subroutine findNonConduitFlow(i, dt)
! 
!   Input:   i = link index
!            dt = time step (sec)
!   Output:  none
!   Purpose: finds new flow in a non-conduit-type link
! 
    use headers
    integer, intent(in) :: i
    double precision, intent(in) :: dt

    double precision :: qLast                      !  previous link flow (cfs)
    double precision :: qNew                       !  new link flow (cfs)
    integer ::  k, m

    !  --- ignore non-dummy conduit links
    if ( arrLink(i)%datatype == E_CONDUIT .and. arrLink(i)%xsect%datatype /= DUMMY ) return

    !  --- update flow in link if not bypassed
    if ( .not.Xlink(i)%bypassed ) then
        !  --- get link flow from last iteration
        qLast = arrLink(i)%newFlow
        arrLink(i)%dqdh = 0.0

        !  --- get new inflow to link from its upstream node
        !      (link_getInflow returns 0 if flap gate closed or pump is offline)
        qNew = link_getInflow(i)
        if ( arrLink(i)%datatype == E_PUMP ) qNew = getModPumpFlow(i, qNew, dt)

        !  --- find surface area at each end of link
        call findNonConduitSurfArea(i)

        !  --- apply under-relaxation with flow from previous iteration
        !  --- do not allow flow to change direction without first being 0
        if ( Steps > 0 .and. arrLink(i)%datatype /= E_PUMP ) then                              ! (5.0.014 - LR)
            qNew = (1.0 - Omega) * qLast + Omega * qNew
            if ( qNew * qLast < 0.0 ) qNew = 0.001 * SGN(qNew)
        end if
        arrLink(i)%newFlow = qNew
    end if

    !  --- add surf. area contributions to upstream/downstream nodes
    Xnode(arrLink(i)%node1)%newSurfArea = Xnode(arrLink(i)%node1)%newSurfArea + Xlink(i)%surfArea1
    Xnode(arrLink(i)%node2)%newSurfArea = Xnode(arrLink(i)%node2)%newSurfArea + Xlink(i)%surfArea2

    !  --- update summed value of dqdh at each end node
    !      (but not for discharge node of Type 4 pumps)
    Xnode(arrLink(i)%node1)%sumdqdh = Xnode(arrLink(i)%node1)%sumdqdh + arrLink(i)%dqdh
    if ( arrLink(i)%datatype == E_PUMP ) then
        k = arrLink(i)%subIndex
        m = Pump(k)%pumpCurve
        if ( Curve(m)%curveType /= PUMP4_CURVE ) then 
            Xnode(arrLink(i)%node2)%sumdqdh = Xnode(arrLink(i)%node2)%sumdqdh + arrLink(i)%dqdh
        end if
    else 
        Xnode(arrLink(i)%node2)%sumdqdh = Xnode(arrLink(i)%node2)%sumdqdh + arrLink(i)%dqdh
    end if

    !  --- update outflow/inflow at upstream/downstream nodes
    call updateNodeFlows(i, arrLink(i)%newFlow)
end subroutine findNonConduitFlow

! =============================================================================

double precision function getModPumpFlow(i, q, dt)
! 
!   Input:   i = link index
!            q = pump flow from pump curve (cfs)
!            dt = time step (sec)
!   Output:  returns modified pump flow rate (cfs)
!   Purpose: modifies pump curve pumping rate depending on amount of water
!            available at pump's inlet node.
! 
    use headers
    integer, intent(in) :: i
    double precision, intent(inout) :: q
    double precision, intent(in) :: dt

    integer :: j           !  pump's inlet node index
    integer :: k           !  pump's index
    
    double precision :: newNetInflow               !  inflow - outflow rate (cfs)
    double precision :: netFlowVolume              !  inflow - outflow volume (ft3)
    double precision :: y                          !  node depth (ft)

    j = arrLink(i)%node1   
    k = arrLink(i)%subIndex

    if ( q == 0.0 ) then
       getModPumpFlow = q
       return
    end if

    !  --- case where inlet node is a storage node: 
    !      prevent node volume from going negative
    if ( Node(j)%datatype == E_STORAGE ) then 
        getModPumpFlow = node_getMaxOutflow(j, q, dt) 
        return
    end if

    !  --- case where inlet is a non-storage node
    select case ( Pump(k)%datatype )
      !  --- for Type1 pump, a volume is computed for inlet node,
      !      so make sure it doesn't go negative
      case (TYPE1_PUMP)
        getModPumpFlow = node_getMaxOutflow(j, q, dt)
        return

      !  --- for other types of pumps, if pumping rate would make depth
      !      at upstream node negative, then set pumping rate = inflow
      case (TYPE2_PUMP)
      case (TYPE4_PUMP)
      case (TYPE3_PUMP)
         newNetInflow = Node(j)%inflow - Node(j)%outflow - q
         netFlowVolume = 0.5 * (Node(j)%oldNetInflow + newNetInflow ) * dt
         y = Node(j)%oldDepth + netFlowVolume / Xnode(j)%newSurfArea
         if ( y <= 0.0 ) then
            getModPumpFlow = Node(j)%inflow
            return
         end if
    end select
    
    getModPumpFlow = q
    return
end function getModPumpFlow

! =============================================================================

subroutine  findNonConduitSurfArea(i)
! 
!   Input:   i = link index
!   Output:  none
!   Purpose: finds the surface area contributed by a non-conduit
!            link to its upstream and downstream nodes.
! 
    use headers
    integer, intent(in) :: i
    if ( arrLink(i)%datatype == E_ORIFICE ) then
        Xlink(i)%surfArea1 = Orifice(arrLink(i)%subIndex)%surfArea / 2.
	!  --- no surface area for weirs to maintain SWMM 4 compatibility
!    else if ( arrLink(i)%datatype == E_WEIR )
!    {
!        Xlink(i)%surfArea1 = Weir(arrLink(i)%subIndex)%surfArea / 2.
!    }
    else 
        Xlink(i)%surfArea1 = 0.0
    end if

! !   Following segment modified for release 5.0.019.  ! !                     ! (5.0.019 - LR)
    Xlink(i)%surfArea2 = Xlink(i)%surfArea1
    if ( arrLink(i)%flowClass == UP_CRITICAL .or. &
       &Node(arrLink(i)%node1)%datatype == E_STORAGE ) Xlink(i)%surfArea1 = 0.0
    if ( arrLink(i)%flowClass == DN_CRITICAL .or. &
       &Node(arrLink(i)%node2)%datatype == E_STORAGE ) Xlink(i)%surfArea2 = 0.0
end subroutine findNonConduitSurfArea

! =============================================================================

subroutine updateNodeFlows( i,  q)
! 
!   Input:   i = link index
!            q = link flow rate (cfs)
!   Output:  none
!   Purpose: updates cumulative inflow & outflow at link's end nodes.
! 
    use headers
    integer, intent(in) :: i
    double precision, intent(in) :: q
    if ( q >= 0.0 ) then
        Node(arrLink(i)%node1)%outflow = Node(arrLink(i)%node1)%outflow + q
        Node(arrLink(i)%node2)%inflow  = Node(arrLink(i)%node2)%inflow + q
    else
        Node(arrLink(i)%node1)%inflow   = Node(arrLink(i)%node1)%inflow - q
        Node(arrLink(i)%node2)%outflow  = Node(arrLink(i)%node2)%outflow - q
    end if
end subroutine updateNodeFlows

! =============================================================================

double precision function  getConduitFlow(j, qOld, dt)
! 
!   Input:   j        = link index
!            qOld     = flow from previous iteration (cfs)
!            dt       = time step (sec)
!   Output:  returns new flow value (cfs)
!   Purpose: updates flow in conduit link by solving finite difference
!            form of continuity and momentum equations.
! 
    use headers
    use modXsect
    use modLink
    integer, intent(in) :: j
    double precision, intent(in) :: qOld, dt
    
    integer :: k                          !  index of conduit
    integer :: n1, n2                     !  indexes of end nodes
    double precision :: z1, z2                     !  upstream/downstream invert elev. (ft)
    double precision ::  h1, h2                     !  upstream/dounstream flow heads (ft)
    double precision ::  y1, y2                     !  upstream/downstream flow depths (ft)
    double precision ::  a1, a2                     !  upstream/downstream flow areas (ft2)
    double precision ::  r1                         !  upstream hyd. radius (ft)
    double precision ::  yMid, rMid, aMid           !  mid-stream or avg. values of y, r, & a
    double precision ::  aWtd, rWtd                 !  upstream weighted area & hyd. radius
    double precision ::  qLast                      !  flow from previous iteration (cfs)
    double precision ::  aOld                       !  area from previous time step (ft2)
    double precision ::  v                          !  velocity (ft/sec)
    double precision ::  rho                        !  upstream weighting factor
    double precision ::  sigma                      !  inertial damping factor
    double precision ::  mlength                     !  effective conduit mlength (ft)
    double precision ::  dq1, dq2, dq3, dq4, dq5    !  terms in momentum eqn.
    double precision ::  denom                      !  denominator of flow update formula
    double precision ::  q                          !  new flow value (cfs)
    double precision ::  barrels                    !  number of barrels in conduit
    !TXsect* xsect = &arrLink(j).xsect    !  ptr. to conduit's cross section data
    type(TXsect), pointer :: xsect !use pointer as to not recreate a local copy
    logical :: isFull
    double precision :: qOldB

    isFull = .false. !  TRUE if conduit flowing full
    xsect => arrLink(j)%xsect !  ptr. to conduit's cross section data
    
    !  --- get most current heads at upstream and downstream ends of conduit
    k =  arrLink(j)%subIndex
    n1 = arrLink(j)%node1
    n2 = arrLink(j)%node2
    z1 = Node(n1)%invertElev + arrLink(j)%offset1
    z2 = Node(n2)%invertElev + arrLink(j)%offset2
    h1 = Node(n1)%newDepth + Node(n1)%invertElev
    h2 = Node(n2)%newDepth + Node(n2)%invertElev
    h1 = MAX(h1, z1)
    h2 = MAX(h2, z2)

    !  --- get unadjusted upstream and downstream flow depths in conduit
    !     (flow depth = head in conduit - elev. of conduit invert)
    y1 = h1 - z1
    y2 = h2 - z2
    y1 = MAX(y1, FUDGE)
    y2 = MAX(y2, FUDGE)

    !  --- flow depths can't exceed full depth of conduit
    y1 = MIN(y1, xsect%yFull)
    y2 = MIN(y2, xsect%yFull)

    !  --- get flow from last time step & previous iteration 
    barrels = Conduit(k)%barrels
    qOldB = qOld / barrels
    qLast = Conduit(k)%q1

    !  -- get area from solution at previous time step
    aOld = Conduit(k)%a2
    aOld = MAX(aOld, FUDGE)

    !  --- use Courant-modified mlength instead of conduit's actual mlength
    mlength = Conduit(k)%modLength

    !  --- find flow classification & corresponding surface area
    !      contributions to upstream and downstream nodes
    arrLink(j)%flowClass = getFlowClass(j, qLast, h1, h2, y1, y2)
    !call findSurfArea(j, mlength, &h1, &h2, &y1, &y2)
    call findSurfArea(j, mlength, h1, h2, y1, y2)

    !  --- compute area at each end of conduit & hyd. radius at upstream end
    a1 = getArea(xsect, y1)
    a2 = getArea(xsect, y2)
    r1 = getHydRad(xsect, y1)

    !  --- compute area & hyd. radius at midpoint
    yMid = 0.5 * (y1 + y2)
    aMid = getArea(xsect, yMid)
    rMid = getHydRad(xsect, yMid)

    !  --- alternate approach not currently used, but might produce better
    !      Bernoulli energy balance for steady flows
    ! aMid = (a1+a2)/2.0
    ! rMid = (r1+getHydRad(xsect,y2))/2.0

    !  --- check if conduit is flowing full
    if ( y1 >= xsect%yFull .and. y2 >= xsect%yFull) isFull = .true.

    !  --- set new flow to zero if conduit is dry or if flap gate is closed
    if ( arrLink(j)%flowClass == DRY .or. &
        &arrLink(j)%flowClass == UP_DRY .or. &
        &arrLink(j)%flowClass == DN_DRY .or. &
        &arrLink(j)%isClosed .or. &
        &aMid <= FUDGE ) then
        Conduit(k)%a1 = 0.5 * (a1 + a2)
        Conduit(k)%q1 = 0.0
        Conduit(k)%q2 = 0.0
        arrLink(j)%dqdh  = GRAVITY * dt * aMid / mlength * barrels
        arrLink(j)%froude = 0.0
        arrLink(j)%newDepth = MIN(yMid, arrLink(j)%xsect%yFull)
        arrLink(j)%newVolume = Conduit(k)%a1 * link_getLength(j) * barrels       ! (5.0.015 - LR)
        getConduitFlow = 0
        return
    end if

    !  --- compute velocity from last flow estimate
    v = qLast / aMid
    if ( fabs(v) > MAXVELOCITY )  v = MAXVELOCITY * SGN(qLast)

    !  --- compute Froude No.
    arrLink(j)%froude = link_getFroude(j, v, yMid)
    if ( arrLink(j)%flowClass == SUBCRITICAL .and. &
        &arrLink(j)%froude > 1.0 ) then
         arrLink(j)%flowClass = SUPCRITICAL
    end if

    !  --- find inertial damping factor (sigma)
    if      ( arrLink(j)%froude <= 0.5 ) then
         sigma = 1.0
    else if ( arrLink(j)%froude >= 1.0 ) then
         sigma = 0.0
    else    
         sigma = 2.0 * (1.0 - arrLink(j)%froude)
    end if

	!  --- get upstream-weighted area & hyd. radius based on damping factor
    !      (modified version of R. Dickinson's slope weighting)
    rho = 1.0
    if ( .not.isFull .and. qLast > 0.0 .and. h1 >= h2 ) rho = sigma
    aWtd = a1 + (aMid - a1) * rho
    rWtd = r1 + (rMid - r1) * rho

    !  --- determine how much inertial damping to apply
    if ( InertDamping == NO_DAMPING ) then
         sigma = 1.0
    else if ( InertDamping == FULL_DAMPING ) then
         sigma = 0.0
    end if

    !  --- use full inertial damping if closed conduit is surcharged
    if ( isFull .and. .not.xsect_isOpen(xsect%datatype) ) sigma = 0.0

    !  --- compute terms of momentum eqn.:
    !  --- 1. friction slope term
    if ( xsect%datatype == FORCE_MAIN .and. isFull ) then
         dq1 = dt * forcemain_getFricSlope(j, fabs(v), rMid)
    else
         dq1 = dt * Conduit(k)%roughFactor / pow(rWtd, 1.33333) * fabs(v)
    end if

    !  --- 2. energy slope term
    dq2 = dt * GRAVITY * aWtd * (h2 - h1) / mlength

    !  --- 3 & 4. inertial terms
    dq3 = 0.0
    dq4 = 0.0
    if ( sigma > 0.0 ) then
        dq3 = 2.0 * v * (aMid - aOld) * sigma
        dq4 = dt * v * v * (a2 - a1) / mlength * sigma
    end if

    !  --- 5. local losses term
    dq5 = 0.0
    if ( Conduit(k)%hasLosses ) then
        dq5 = findLocalLosses(j, a1, a2, aMid, qLast) / 2.0 / mlength * dt
    end if

    !  --- combine terms to find new conduit flow
    denom = 1.0 + dq1 + dq5
    q = (qOldB - dq2 + dq3 + dq4) / denom

    !  --- compute derivative of flow w.r.t. head
    arrLink(j)%dqdh = 1.0 / denom  * GRAVITY * dt * aWtd / mlength * barrels      ! (5.0.014 - LR)

    !  --- check if any flow limitation applies
    if ( q > 0.0 ) then
        !  --- open channels can't have more than full normal flow
        if ( isFull ) then
            if ( xsect_isOpen(xsect%datatype) ) q = MIN(q, arrLink(j)%qFull)
        end if

        !  --- check for inlet controlled culvert flow                         ! (5.0.014 - LR)
        if ( xsect%culvertCode > 0 .and. .not.isFull ) then                            ! (5.0.014 - LR)
            q = culvert_getInflow(j, q, h1)                                   ! (5.0.014 - LR)

        !  --- check for normal flow limitation based on surface slope & Fr
        else                                                                   ! (5.0.014 - LR)
            if ( y1 < arrLink(j)%xsect%yFull .and. &
                  &( arrLink(j)%flowClass == SUBCRITICAL .or. &
                    &arrLink(j)%flowClass == SUPCRITICAL)) then
                q = checkNormalFlow(j, q, y1, y2, a1, r1)                        ! (5.0.019 - LR)
            end if
        end if
    end if

    !  --- apply under-relaxation weighting between new & old flows
    !  --- do not allow change in flow direction without first being zero 
    if ( Steps > 0 ) then
        q = (1.0 - Omega) * qLast + Omega * q
        if ( q * qLast < 0.0 ) q = 0.001 * SGN(q)
    end if

    !  --- check if user-supplied flow limit applies
    if ( arrLink(j)%qLimit > 0.0 ) then
         if ( fabs(q) > arrLink(j)%qLimit ) q = SGN(q) * arrLink(j)%qLimit
    end if

    !  --- check for reverse flow with closed flap gate
    if ( link_setFlapGate(j, n1, n2, q) ) q = 0.0                             ! (5.0.014 - LR)

    !  --- do not allow flow out of a dry node
    !      (as suggested by R. Dickinson)
    if( q >  FUDGE .and. Node(n1)%newDepth <= FUDGE ) q =  FUDGE
    if( q < -FUDGE .and. Node(n2)%newDepth <= FUDGE ) q = -FUDGE

    !  --- save new values of area, flow, depth, & volume
    Conduit(k)%a1 = aMid
    Conduit(k)%q1 = q
    Conduit(k)%q2 = q
    arrLink(j)%newDepth  = MIN(yMid, xsect%yFull)
    aMid = (a1 + a2) / 2.0
    aMid = MIN(aMid, xsect%aFull)
    arrLink(j)%newVolume = aMid * link_getLength(j) * barrels                    ! (5.0.015 - LR)
    getConduitFlow =  q * barrels
    
    nullify(xsect) !TODO: I think we need to do it here
    return
end function getConduitFlow

! =============================================================================

integer function getFlowClass(j, q, h1, h2, y1, y2)
! 
!   Input:   j  = conduit link index
!            q  = current conduit flow (cfs)
!            h1 = head at upstream end of conduit (ft)
!            h2 = head at downstream end of conduit (ft)
!            y1 = upstream flow depth in conduit (ft)
!            y2 = downstream flow depth in conduit (ft)
!   Output:  returns flow classification code
!   Purpose: determines flow class for a conduit based on depths at each end.
! 
    use headers
    integer, intent(in) :: j
    double precision, intent(in) :: q, h1, h2, y1, y2

    integer ::    n1, n2                     !  indexes of upstrm/downstrm nodes
    integer ::    flowClass                  !  flow classification code
    double precision :: ycMin, ycMax               !  min/max critical depths (ft)
    double precision :: z1, z2                     !  offsets of conduit inverts (ft)

    !  --- get upstream & downstream node indexes
    n1 = arrLink(j)%node1
    n2 = arrLink(j)%node2

    !  --- get upstream & downstream conduit invert offsets
    z1 = arrLink(j)%offset1
    z2 = arrLink(j)%offset2

    !  --- base offset of an outfall conduit on outfall's depth
    if ( Node(n1)%datatype == E_OUTFALL ) z1 = MAX(0.0, (z1 - Node(n1)%newDepth))
    if ( Node(n2)%datatype == E_OUTFALL ) z2 = MAX(0.0, (z2 - Node(n2)%newDepth))

    !  --- default class is SUBCRITICAL
    flowClass = SUBCRITICAL
    Fasnh = 1.0

    !  --- case where both ends of conduit are wet
    if ( y1 > FUDGE .and. y2 > FUDGE ) then
        if ( q < 0.0 ) then
            !  --- upstream end at critical depth if flow depth is
            !      below conduit's critical depth and an upstream 
            !      conduit offset exists
            if ( z1 > 0.0 ) then
                NormalDepth   = link_getYnorm(j, fabs(q))
                CriticalDepth = link_getYcrit(j, fabs(q))
                ycMin = MIN(NormalDepth, CriticalDepth)
                if ( y1 < ycMin ) flowClass = UP_CRITICAL
            end if
        !  --- case of normal direction flow
        else
            !  --- downstream end at smaller of critical and normal depth
            !      if downstream flow depth below this and a downstream
            !      conduit offset exists
            if ( z2 > 0.0 ) then
                NormalDepth = link_getYnorm(j, fabs(q))
                CriticalDepth = link_getYcrit(j, fabs(q))
                ycMin = MIN(NormalDepth, CriticalDepth)
                ycMax = MAX(NormalDepth, CriticalDepth)
                if ( y2 < ycMin ) then
                    flowClass = DN_CRITICAL
                else if ( y2 < ycMax ) then
                    if ( ycMax - ycMin < FUDGE ) then
                        Fasnh = 0.0
                    else 
                        Fasnh = (ycMax - y2) / (ycMax - ycMin)
                    end if
                end if
            end if
        end if
    !  --- case where no flow at either end of conduit
    else if ( y1 <= FUDGE .and. y2 <= FUDGE ) then
        flowClass = DRY
    !  --- case where downstream end of pipe is wet, upstream dry
    else if ( y2 > FUDGE ) then
        !  --- flow classification is UP_DRY if downstream head <
        !      invert of upstream end of conduit
        if ( h2 < Node(n1)%invertElev + arrLink(j)%offset1 ) then
            flowClass = UP_DRY
        !  --- otherwise, the downstream head will be >= upstream
        !      conduit invert creating a flow reversal and upstream end
        !      should be at critical depth, providing that an upstream
        !      offset exists (otherwise subcritical condition is maintained)
        else if ( z1 > 0.0 ) then
            NormalDepth   = link_getYnorm(j, fabs(q))
            CriticalDepth = link_getYcrit(j, fabs(q))
            flowClass = UP_CRITICAL
        end if
    !  --- case where upstream end of pipe is wet, downstream dry
    else
        !  --- flow classification is DN_DRY if upstream head <
        !      invert of downstream end of conduit
        if ( h1 < Node(n2)%invertElev + arrLink(j)%offset2 ) then
            flowClass = DN_DRY
        !  --- otherwise flow at downstream end should be at critical depth
        !      providing that a downstream offset exists (otherwise
        !      subcritical condition is maintained)
        else if ( z2 > 0.0 ) then
            NormalDepth = link_getYnorm(j, fabs(q))
            CriticalDepth = link_getYcrit(j, fabs(q))
            flowClass = DN_CRITICAL
        end if
    end if
    getFlowClass = flowClass
    return
end function getFlowClass

! =============================================================================

subroutine findSurfArea(j, aLength, h1, h2, y1, y2)
! 
!   Input:   j  = conduit link index
!            q  = current conduit flow (cfs)
!            length = conduit length (ft)
!            h1 = head at upstream end of conduit (ft)
!            h2 = head at downstream end of conduit (ft)
!            y1 = upstream flow depth (ft)
!            y2 = downstream flow depth (ft)
!   Output:  updated values of h1, h2, y1, & y2
!   Purpose: assigns surface area of conduit to its up and downstream nodes.
! 
    use headers
    integer, intent(in) :: j
    double precision, intent(inout) :: h1, h2, y1, y2
    double precision, intent(in) :: aLength
    
    integer ::     n1, n2                    !  indexes of upstrm/downstrm nodes
    double precision :: flowDepth1                !  flow depth at upstrm end (ft)
    double precision :: flowDepth2                !  flow depth at downstrm end (ft)
    double precision :: flowDepthMid              !  flow depth at midpt. (ft)
    double precision :: width1                    !  top width at upstrm end (ft)
    double precision :: width2                    !  top width at downstrm end (ft)
    double precision :: widthMid                  !  top width at midpt. (ft)
    double precision :: surfArea1            !  surface area at upstream node (ft2)
    double precision :: surfArea2            !  surface area st downstrm node (ft2)
!   TXsect* xsect = &arrLink(j).xsect
    type(TXsect), pointer :: xsect
    xsect => arrLink(j)%xsect
    
    surfArea1 = 0.0
    surfArea2 = 0.0
    
    !  --- get node indexes & current flow depths
    n1 = arrLink(j)%node1
    n2 = arrLink(j)%node2
    flowDepth1 = y1
    flowDepth2 = y2

    !  --- add conduit's surface area to its end nodes depending on flow class
    select case ( arrLink(j)%flowClass )
      case (SUBCRITICAL)
        flowDepthMid = 0.5 * (flowDepth1 + flowDepth2)
        if ( flowDepthMid < FUDGE ) flowDepthMid = FUDGE
        width1 =   getWidth(xsect, flowDepth1)
        width2 =   getWidth(xsect, flowDepth2)
        widthMid = getWidth(xsect, flowDepthMid)
        surfArea1 = (width1 + widthMid) * length / 4.
        surfArea2 = (widthMid + width2) * length / 4. * Fasnh
        !break

      case (UP_CRITICAL)
        flowDepth1 = CriticalDepth
        if ( NormalDepth < CriticalDepth ) flowDepth1 = NormalDepth
        flowDepth1 = MAX(flowDepth1, FUDGE)                                   ! (5.0.022 - LR)
        h1 = Node(n1)%invertElev + arrLink(j)%offset1 + flowDepth1
        flowDepthMid = 0.5 * (flowDepth1 + flowDepth2)
        if ( flowDepthMid < FUDGE ) flowDepthMid = FUDGE
        width2   = getWidth(xsect, flowDepth2)
        widthMid = getWidth(xsect, flowDepthMid)
        surfArea2 = (widthMid + width2) * length * 0.5
        !break

      case (DN_CRITICAL)
        flowDepth2 = CriticalDepth
        if ( NormalDepth < CriticalDepth ) flowDepth2 = NormalDepth
        flowDepth2 = MAX(flowDepth2, FUDGE)                                   ! (5.0.022 - LR)
        h2 = Node(n2)%invertElev + arrLink(j)%offset2 + flowDepth2
        width1 = getWidth(xsect, flowDepth1)
        flowDepthMid = 0.5 * (flowDepth1 + flowDepth2)
        if ( flowDepthMid < FUDGE ) flowDepthMid = FUDGE
        widthMid = getWidth(xsect, flowDepthMid)
        surfArea1 = (width1 + widthMid) * length * 0.5
        !break

      case (UP_DRY)
        flowDepth1 = FUDGE
        flowDepthMid = 0.5 * (flowDepth1 + flowDepth2)
        if ( flowDepthMid < FUDGE ) flowDepthMid = FUDGE
        width1 = getWidth(xsect, flowDepth1)
        width2 = getWidth(xsect, flowDepth2)
        widthMid = getWidth(xsect, flowDepthMid)

        !  --- assign avg. surface area of downstream half of conduit
        !      to the downstream node
        surfArea2 = (widthMid + width2) * length / 4.

        !  --- if there is no free-fall at upstream end, assign the
        !      upstream node the avg. surface area of the upstream half
        if ( arrLink(j)%offset1 <= 0.0 ) then
            surfArea1 = (width1 + widthMid) * length / 4.
        end if
        !break

      case (DN_DRY)
        flowDepth2 = FUDGE
        flowDepthMid = 0.5 * (flowDepth1 + flowDepth2)
        if ( flowDepthMid < FUDGE ) flowDepthMid = FUDGE
        width1 = getWidth(xsect, flowDepth1)
        width2 = getWidth(xsect, flowDepth2)
        widthMid = getWidth(xsect, flowDepthMid)

        !  --- assign avg. surface area of upstream half of conduit
        !      to the upstream node
        surfArea1 = (widthMid + width1) * length / 4.

        !  --- if there is no free-fall at downstream end, assign the
        !      downstream node the avg. surface area of the downstream half
        if ( arrLink(j)%offset2 <= 0.0 ) then
            surfArea2 = (width2 + widthMid) * length / 4.
        end if
        !break

      case (DRY)
        surfArea1 = FUDGE * length / 2.0
        surfArea2 = surfArea1
        !break
    end select
    Xlink(j)%surfArea1 = surfArea1
    Xlink(j)%surfArea2 = surfArea2
    y1 = flowDepth1
    y2 = flowDepth2
end subroutine findSurfArea

! =============================================================================

double precision function findLocalLosses(j, a1, a2, aMid, q)
! 
!   Input:   j    = link index
!            a1   = upstream area (ft2)
!            a2   = downstream area (ft2)
!            aMid = midpoint area (ft2)
!            q    = flow rate (cfs)
!   Output:  returns local losses (ft/sec)
!   Purpose: computes local losses term of momentum equation.
! 
    use headers
    integer, intent(in) :: j
    double precision, intent(in) :: a1, a2, aMid, q
    double precision :: losses, mq
    losses = 0.0
    mq = fabs(q)
    if ( a1 > FUDGE ) losses = losses + arrLink(j)%cLossInlet  * (mq/a1)
    if ( a2 > FUDGE ) losses = losses + arrLink(j)%cLossOutlet * (mq/a2)
    if ( aMid  > FUDGE ) losses = losses + arrLink(j)%cLossAvg * (mq/aMid)
    findLocalLosses = losses
    return
end function findLocalLosses

! =============================================================================

double precision function getWidth(xsect, y)
! 
!   Input:   xsect = ptr. to conduit cross section
!            y     = flow depth (ft)
!   Output:  returns top width (ft)
!   Purpose: computes top width of flow surface in conduit.
! 
    use headers
    use modXsect
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    double precision :: yNorm
    double precision :: ym
    yNorm = y/xsect%yFull
!   if ( yNorm < 0.04 ) y = 0.04*xsect->yFull                                 ! (5.0.015 - LR)
    if ( yNorm > 0.96 .and. .not.xsect_isOpen(xsect%datatype) ) ym = 0.96*xsect%yFull
    getWidth = xsect_getWofY(xsect, ym)
    return
end function getWidth

! =============================================================================

double precision function getArea(xsect, y)
! 
!   Input:   xsect = ptr. to conduit cross section
!            y     = flow depth (ft)
!   Output:  returns flow area (ft2)
!   Purpose: computes area of flow cross-section in a conduit.
! 
    use headers
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    double precision :: ym
    ym = MIN(y, xsect%yFull)
    getArea = xsect_getAofY(xsect, ym)
    return
end function getArea
! =============================================================================

double precision function getHydRad(xsect, y)
! 
!   Input:   xsect = ptr. to conduit cross section
!            y     = flow depth (ft)
!   Output:  returns hydraulic radius (ft)
!   Purpose: computes hydraulic radius of flow cross-section in a conduit.
! 
    use headers
    type(TXsect), intent(in) :: xsect
    double precision, intent(in) :: y
    double precision :: ym
    ym = MIN(y, xsect%yFull)
    getHydRad = xsect_getRofY(xsect, ym)
    return
end function getHydRad

! =============================================================================

! !   This function was removed for release 5.0.014.  ! !                      ! (5.0.014 - LR)
! double checkFlapGate(int j, int n1, int n2, double q)                        ! (5.0.014 - LR)

! =============================================================================

!   =====  This function was completely re-written for release 5.0.019  =====  ! (5.0.019 - LR)

double precision function checkNormalFlow(j, q, y1, y2, a1, r1)
! 
!   Input:   j = link index
!            q = link flow found from dynamic wave equations (cfs)
!            y1 = flow depth at upstream end (ft)
!            y2 = flow depth at downstream end (ft)
!            a1 = flow area at upstream end (ft2)
!            r1 = hyd. radius at upstream end (ft)
!   Output:  returns modifed flow in link (cfs)
!   Purpose: checks if flow in link should be replaced by normal flow.
! 
    use headers
    double precision, intent(in) :: q, y1, y2, a1, r1
    integer, intent(in) :: j
    logical ::    check
    integer ::    k, n1, n2
    logical ::    hasOutfall
    double precision :: qNorm
    double precision :: f1

    if (Node(n1)%datatype == E_OUTFALL .or. Node(n2)%datatype == E_OUTFALL) then
       hasOutfall = .true.
    else
       hasOutfall = .false.
    end if
    check  = .FALSE.
    k = arrLink(j)%subIndex
    n1 = arrLink(j)%node1
    n2 = arrLink(j)%node2

    if (Node(n1)%datatype == E_OUTFALL .or. Node(n2)%datatype == E_OUTFALL) then
            hasOutfall = .true.
    else
            hasOutfall = .false.
    end if

    !  --- check if water surface slope < conduit slope
    if ( NormalFlowLtd == SLOPE .or. NormalFlowLtd == BOTH .or. hasOutfall ) then
        if ( y1 < y2 ) check = .true.
    end if

    !  --- check if Fr >= 1.0 at upstream end of conduit
    if ( .not.check .and. (NormalFlowLtd == FROUDE .or. NormalFlowLtd == BOTH) .and.  .not.hasOutfall) then
        if ( y1 > FUDGE .and. y2 > FUDGE ) then
            f1 = link_getFroude(j, q/a1, y1)
            if ( f1 >= 1.0 ) check = .true.
        end if
    end if

    !  --- check if normal flow < dynamic flow
    if ( check ) then
        qNorm = Conduit(k)%beta * a1 * pow(r1, 2./3.)
        checkNormalFlow = MIN(q, qNorm)
    else 
        checkNormalFlow = q
    end if
    return
end function checkNormalFlow

! =============================================================================

subroutine setNodeDepth(i, dt)
! 
!   Input:   i  = node index
!            dt = time step (sec)
!   Output:  none
!   Purpose: sets depth at non-outfall node after current time step.
! 
    use headers
    integer, intent(in) :: i
    double precision, intent(in) :: dt
    logical ::     canPond                   !  TRUE if node can pond overflows
    logical ::     isPonded                  !  TRUE if node is currently ponded     ! (5.0.016 - LR)
    double precision ::  dQ                        !  inflow minus outflow at node (cfs)
    double precision ::  dV                        !  change in node volume (ft3)
    double precision ::  dy                        !  change in node depth (ft)
    double precision ::  yMax                      !  max. depth at node (ft)
    double precision ::  yOld                      !  node depth at previous time step (ft)
    double precision ::  yLast                     !  previous node depth (ft)
    double precision ::  yNew                      !  new node depth (ft)
    double precision ::  yCrown                    !  depth to node crown (ft)
    double precision ::  surfArea                  !  node surface area (ft2)
    double precision ::  denom                     !  denominator term
    double precision ::  corr                      !  correction factor
    double precision ::  f                         !  relative surcharge depth

    !  --- see if node can pond water above it
    if (AllowPonding .and. Node(i)%pondedArea > 0.0) then
         canPond = .true.
    else
         canPond = .false.
    end if
    if (canPond .and. Node(i)%newDepth > Node(i)%fullDepth) then ! (5.0.016 - LR)
         isPonded = .true.
    else
         isPonded = .false.
    end if

    !  --- initialize values
    yCrown = Node(i)%crownElev - Node(i)%invertElev
    yOld = Node(i)%oldDepth
    yLast = Node(i)%newDepth
    Node(i)%overflow = 0.0
    surfArea = Xnode(i)%newSurfArea

    !  --- determine average net flow volume into node over the time step
    dQ = Node(i)%inflow - Node(i)%outflow
    dV = 0.5 * (Node(i)%oldNetInflow + dQ) * dt - node_getLosses(i, dt)       ! (5.0.019 - LR)

    !  --- if node not surcharged, base depth change on surface area        
    if ( yLast <= yCrown .or. Node(i)%datatype == E_STORAGE .or. isPonded ) then              ! (5.0.016 - LR)
        dy = dV / surfArea
        Xnode(i)%oldSurfArea = Xnode(i)%newSurfArea
        if ( .not.isPonded ) Xnode(i)%oldSurfArea = Xnode(i)%newSurfArea          ! (5.0.019 - LR)
        yNew = yOld + dy

        !  --- apply under-relaxation to new depth estimate
        if ( Steps > 0 ) then
            yNew = (1.0 - Omega) * yLast + Omega * yNew
        end if

        !  --- don't allow a ponded node to drop much below full depth         ! (5.0.019 - LR)
        if ( isPonded .and. yNew < Node(i)%fullDepth ) then ! (5.0.019 - LR)
            yNew = Node(i)%fullDepth - FUDGE                                  ! (5.0.019 - LR)
        end if
    else
    !  --- if node surcharged, base depth change on dqdh
    !      NOTE: depth change is w.r.t depth from previous
    !      iteration also, do not apply under-relaxation.

        !  --- apply correction factor for upstream terminal nodes
        corr = 1.0
        if ( Node(i)%degree < 0 ) corr = 0.6

        !  --- allow surface area from last non-surcharged condition
        !      to influence dqdh if depth close to crown depth
        denom = Xnode(i)%sumdqdh
        if ( yLast < 1.25 * yCrown ) then
            f = (yLast - yCrown) / yCrown
            denom = denom + (Xnode(i)%oldSurfArea/dt - Xnode(i)%sumdqdh) * exp(-15.0 * f)
        end if

        !  --- compute new estimate of node depth
        if ( denom == 0.0 )  then
            dy = 0.0
        else 
            dy = corr * dQ / denom
        end if
        yNew = yLast + dy
        if ( yNew < yCrown ) yNew = yCrown - FUDGE

        !  --- don't allow a ponded node to rise much above full depth         ! (5.0.019 - LR)
        if ( canPond .and. yNew > Node(i)%fullDepth ) then                            ! (5.0.019 - LR)
            yNew = Node(i)%fullDepth + FUDGE                                  ! (5.0.019 - LR)
        end if
    end if

    !  --- depth cannot be negative
    if ( yNew < 0 ) yNew = 0.0

    !  --- determine max. non-flooded depth
    yMax = Node(i)%fullDepth
    if ( .not.canPond) yMax = yMax + Node(i)%surDepth

    !  --- find flooded depth & volume
    if ( yNew > yMax ) then                                                         ! (5.0.019 - LR)
        yNew = getFloodedDepth(i, canPond, dV, yNew, yMax, dt)                ! (5.0.014 - LR)
    else 
        Node(i)%newVolume = node_getVolume(i, yNew)
    end if

! !   Computation of dy/dt moved to here  ! !                                  ! (5.0.017 - LR)
    !  --- compute change in depth w.r.t. time
    Xnode(i)%dYdT = fabs(yNew - yOld) / dt

    !  --- save new depth for node
    Node(i)%newDepth = yNew
end subroutine setNodeDepth

! =============================================================================

!   =====  This function was completely re-written for release 5.0.019  =====  ! (5.0.019 - LR)

double precision function getFloodedDepth(i, canPond, dV, yNew, yMax, dt)
! 
!   Input:   i  = node index
!            canPond = TRUE if water can pond over node
!            isPonded = TRUE if water is currently ponded
!            dV = change in volume over time step (ft3)
!            yNew = current depth at node (ft)
!            yMax = max. depth at node before ponding (ft)
!            dt = time step (sec)
!   Output:  returns depth at node when flooded (ft)
!   Purpose: computes depth, volume and overflow for a flooded node.
! 
    use headers
    integer, intent(in) :: i
    logical, intent(in) :: canPond
    double precision, intent(in) :: dV, yNew, yMax, dt
    double precision :: myNew

    if ( .not.canPond ) then
        Node(i)%overflow = dV / dt
        Node(i)%newVolume = Node(i)%fullVolume
        myNew = yMax
    else
        Node(i)%newVolume = MAX((Node(i)%oldVolume+dV), Node(i)%fullVolume)
        Node(i)%overflow = (Node(i)%newVolume - &
            &MAX(Node(i)%oldVolume, Node(i)%fullVolume)) / dt
    end if
    if ( Node(i)%overflow < FUDGE ) Node(i)%overflow = 0.0

    getFloodedDepth = myNew
    return
end function getFloodedDepth

! =============================================================================

double precision function getVariableStep(maxStep)
! 
!   Input:   maxStep = user-supplied max. time step (sec)
!   Output:  returns time step (sec)
!   Purpose: finds time step that satisfies stability criterion but
!            is no greater than the user-supplied max. time step.
! 

    double precision, intent(in) :: maxStep
    integer ::    minLink = -1                !  index of link w/ min. time step
    integer ::    minNode = -1                !  index of node w/ min. time step
    double precision :: tMin                        !  allowable time step (sec)
    double precision :: tMinLink                    !  allowable time step for links (sec)
    double precision :: tMinNode                    !  allowable time step for nodes (sec)

    !  --- find stable time step for links & then nodes
    tMin = maxStep
    tMinLink = getLinkStep(tMin, minLink)
    tMinNode = getNodeStep(tMinLink, minNode)

    !  --- use smaller of the link and node time step
    tMin = tMinLink
    if ( tMinNode < tMin ) then
        tMin = tMinNode 
        minLink = -1
    end if

    !  --- update count of times the minimum node or link was critical
    call stats_updateCriticalTimeCount(minNode, minLink)

    !  --- don't let time step go below an absolute minimum
    if ( tMin < MINTIMESTEP ) tMin = MINTIMESTEP

    getVariableStep = tMin
    return
end function getVariableStep

! =============================================================================

double precision function getLinkStep(tMin, minLink)
! 
!   Input:   tMin = critical time step found so far (sec)
!   Output:  minLink = index of link with critical time step
!            returns critical time step (sec)
!   Purpose: finds critical time step for conduits based on Courant criterion.
! 
    use headers
    double precision, intent(in) :: tMin
    integer, intent(inout) :: minLink

    integer ::    i                           !  link index
    integer ::    k                           !  conduit index
    double precision :: q                           !  conduit flow (cfs)
    double precision :: t                           !  time step (sec)
    double precision :: mtLink

    mtLink = tMin                !  critical link time step (sec)
    !  --- examine each conduit link
    do i = 1, Nobjects(LINK)
        if ( arrLink(i)%datatype == E_CONDUIT ) then
           !  --- skip conduits with negligible flow, area or Fr
            k = arrLink(i)%subIndex
            q = fabs(arrLink(i)%newFlow) / Conduit(k)%barrels
            if ( q <= 0.05 * arrLink(i)%qFull &
            &.or.   Conduit(k)%a1 <= FUDGE &
            &.or.   arrLink(i)%froude <= 0.01) cycle

            !  --- compute time step to satisfy Courant condition
            t = arrLink(i)%newVolume / Conduit(k)%barrels / q
            t = t * Conduit(k)%modLength / link_getLength(i)                  ! (5.0.015 - LR)
            t = t * arrLink(i)%froude / (1.0 + arrLink(i)%froude) * CourantFactor

            !  --- update critical link time step
            if ( t < mtLink ) then
                mtLink = t
                minLink = i
            end if
        end if
    end do

    getLinkStep = mtLink
    return
end function getLinkStep

! =============================================================================

double precision function getNodeStep(tMin, minNode)
! 
!   Input:   tMin = critical time step found so far (sec)
!   Output:  minNode = index of node with critical time step
!            returns critical time step (sec)
!   Purpose: finds critical time step for nodes based on max. allowable
!            projected change in depth.
! 

    use headers
    double precision, intent(in) :: tMin
    integer, intent(inout) :: minNode
    integer ::    i                           !  node index
    double precision :: maxDepth                    !  max. depth allowed at node (ft)
    double precision :: dYdT                        !  change in depth per unit time (ft/sec)
    double precision :: t1                          !  time needed to reach depth limit (sec)
    double precision :: mtNode                !  critical node time step (sec)
    mtNode = tMin

    !  --- find smallest time so that estimated change in nodal depth
    !      does not exceed safety factor * maxdepth
    do i = 0, Nobjects(E_NODE)
        !  --- see if node can be skipped
        if ( Node(i)%datatype == E_OUTFALL ) cycle
        if ( Node(i)%newDepth <= FUDGE) cycle
        if ( Node(i)%newDepth  + FUDGE >= Node(i)%crownElev - Node(i)%invertElev ) cycle

        !  --- define max. allowable depth change using crown elevation
        maxDepth = (Node(i)%crownElev - Node(i)%invertElev) * 0.25
        if ( maxDepth < FUDGE ) cycle
        dYdT = Xnode(i)%dYdT
        if (dYdT < FUDGE ) cycle

        !  --- compute time to reach max. depth & compare with critical time
        t1 = maxDepth / dYdT
        if ( t1 < mtNode ) then
            mtNode = t1
            minNode = i
        end if
    end do
    getNodeStep = mtNode
    return
end function getNodeStep

! =============================================================================

subroutine checkCapacity(j)
! 
!   Input:   j = link index
!   Output:  none
!   Purpose: determines if a conduit link is capacity limited.
! 
    use headers
    integer, intent(in) :: j
    integer ::    n1, n2, k
    double precision :: h1, h2

    !  ---- check only conduit links
    if ( arrLink(j)%datatype /= E_CONDUIT ) return

    !  --- check that upstream end is full
    k = arrLink(j)%subIndex
    Conduit(k)%capacityLimited = .FALSE.
    if ( Conduit(k)%a1 >= arrLink(j)%xsect%aFull ) then
        !  --- check if HGL slope > conduit slope
        n1 = arrLink(j)%node1
        n2 = arrLink(j)%node2
        h1 = Node(n1)%newDepth + Node(n1)%invertElev
        h2 = Node(n2)%newDepth + Node(n2)%invertElev
        if ( (h1 - h2) > fabs(Conduit(k)%slope) * Conduit(k)%length ) then          ! (5.0.018 - LR)
            Conduit(k)%capacityLimited = .TRUE.
        end if
    end if
end subroutine checkCapacity

end module
