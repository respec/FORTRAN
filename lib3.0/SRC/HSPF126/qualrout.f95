!-----------------------------------------------------------------------------
!   qualrout.c
!
!   Project:  EPA SWMM5
!   Version:  5.0
!   Date:     6/19/07   (Build 5.0.010)
!             1/21/09   (Build 5.0.014)
!             10/7/09   (Build 5.0.017)
!   Author:   L. Rossman
!
!   Water quality routing functions.
!
!   ===============================================================
!   NOTE: This module was completely re-written for release 5.0.014
!   ===============================================================
!
!-----------------------------------------------------------------------------
!#define _CRT_SECURE_NO_DEPRECATE
!
!#include <stdio.h>
!#include <stdlib.h>
!#include <math.h>
!#include "headers.h"

!-----------------------------------------------------------------------------
!  External functions (declared in funcs.h)
!-----------------------------------------------------------------------------
!  qualrout_execute         (called by routing_execute)
!  qualrout_getCstrQual     (called by getPondedQual in subcatch.c) 

!-----------------------------------------------------------------------------
!  Function declarations
!-----------------------------------------------------------------------------
!static void  findLinkMassFlow(int i)
!static void  findNodeQual(int j)
!static void  findLinkQual(int i, double tStep)
!static void  findSFLinkQual(int i, double tStep)                              !(5.0.017 - LR)
!static void  findStorageQual(int j, double tStep)
!static void  updateHRT(int j, double v, double q, double tStep)
!static double getReactedQual(int p, double c, double v1, double tStep)        !(5.0.017 - LR)
!static double getMixedQual(double c, double v1, double wIn, double qIn,        !(5.0.017 - LR)
!              double tStep)                                                   !(5.0.017 - LR)
!=============================================================================

subroutine qualrout_execute(tStep)
!
!  Input:   tStep = routing time step (sec)
!  Output:  none
!  Purpose: routes water quality constituents through the drainage
!           network over the current time step.
!
    use headers
    use treatmnt
    implicit none
    integer, parameter :: dp = kind(1.d0)
    real(kind=dp), intent(in) :: tStep
    integer :: i, j
    real(kind=dp) :: qIn, vAvg

    ! --- find mass flow each link contributes to its downstream node
    do i=1, Nobjects(LINK) 
      call findLinkMassFlow(i)
    end do

    ! --- find new water quality concentration at each node  
    do j=1, Nobjects(E_NODE)
        ! --- get node inflow and average volume
        qIn = Node(j)%inflow
        vAvg = (Node(j)%oldVolume + Node(j)%newVolume) / 2.0
        
        ! --- save inflow concentrations if treatment applied
        if ( associated(Node(j)%treatment) ) then
            if ( qIn < ZERO ) qIn = 0.0
            call treatmnt_setInflow(qIn, Node(j)%newQual)
        end if
       
        ! --- find new quality at the node 
        if ( Node(j)%datatype == E_STORAGE .or. Node(j)%oldVolume > FUDGE ) then
            call findStorageQual(j, tStep)
        else 
            call findNodeQual(j)
        end if

        ! --- apply treatment to new quality values
        if ( associated(Node(j)%treatment) .and. Node(j)%newDepth > FUDGE ) then                   !(5.0.017 - LR)
           !call treatmnt_treat(j, qIn, vAvg, tStep)
        end if
    end do

    ! --- find new water quality in each link
    do i=1, Nobjects(LINK) 
       call findLinkQual(i, tStep)
    end do
end subroutine qualrout_execute

!!=============================================================================
!
!double qualrout_getCstrQual(double c, double v, double wIn, double qIn, 
!                            double kDecay, double tStep)
!!
!!  Input:   c = pollutant concentration at start of time step (mass/ft3)
!!           v = average volume over time step (ft3)
!!           wIn = mass inflow rate (mass/sec)
!!           qIn = inflow rate (cfs)
!!           kDecay = first-order decay coeff. (1/sec)
!!           tStep = time step (sec)
!!  Output:  returns pollutant concentration at end of time step (mass/ft3)
!!  Purpose: finds pollutant concentration within a CSTR volume
!{
!    double f
!    if ( v < ZERO ) return 0.0
!    f = (qIn/v + kDecay)*tStep
!    if ( f <= ZERO || f > 15.0 ) return c
!    else f = exp(-f)
!    c = c*f
!    if ( qIn > ZERO ) c = c + wIn/qIn*(1.0-f)
!    return c 
!}
!
!!=============================================================================
!
!!!  New function added to release 5.0.017. !!                              !(5.0.017 - LR)
!
real(kind=kind(1.d0)) function getMixedQual(c, v1, wIn, qIn, tStep)
!
!  Input:   c = pollutant concentration at start of time step (mass/ft3)
!           v1 = volume at start of time step (ft3)
!           wIn = mass inflow rate (mass/sec)
!           qIn = flow inflow rate (cfs)
!           tStep = time step (sec)
!  Output:  returns pollutant concentration at end of time step (mass/ft3)
!  Purpose: finds pollutant concentration within a completely mixed volume
!
    use headers
    implicit none
    integer, parameter :: dp = kind(1.d0)
    real(kind=dp), intent(in) :: c, v1, wIn, qIn, tStep
    real(kind=dp) :: vIn, cIn, cMax, myc
    
    myc = c

    ! --- compute concentration of any inflow
    if ( qIn <= ZERO ) then
        getMixedQual= myc
    else
        vIn = qIn * tStep
        cIn = wIn * tStep / vIn

        ! --- find limit on final concentration
        cMax = MAX(myc, cIn)

        ! --- mix inflow with reacted contents
        myc = (myc*v1 + wIn*tStep) / (v1 + vIn)
        myc = MIN(myc, cMax)
        myc = MAX(myc, 0.0)
        getMixedQual = myc
    end if
    return
end function getMixedQual

!
!!=============================================================================
!
subroutine findLinkMassFlow(i)
!
!  Input:   i = link index
!  Output:  none
!  Purpose: adds constituent mass flow out of link to the total
!           accumulation at the link's downstream node.
!
!  Note:    Node().newQual(), the accumulator variable, already contains
!           contributions from runoff and other external inflows from
!           calculations made in routing_execute().

    use headers
    implicit none
    integer, parameter :: dp = kind(1.d0)
    integer, intent(in) :: i
    integer ::    j, p
    real(kind=dp) :: qLink

    ! --- find inflow to downstream node
    qLink = arrLink(i)%newFlow

    ! --- identify index of downstream node
    j = arrLink(i)%node2
    if ( qLink < 0.0 ) then 
       j = arrLink(i)%node1
    end if
    qLink = abs(qLink)

    ! --- add mass inflow from link to total at downstream node
    do p= 1, Nobjects(E_POLLUT)
        Node(j)%newQual(p) = Node(j)%newQual(p) + qLink * arrLink(i)%oldQual(p)
    end do
end subroutine findLinkMassFlow
!
!!=============================================================================
!
subroutine findNodeQual(j)
!
!  Input:   j = node index
!  Output:  none
!  Purpose: finds new quality in a node with no storage volume.
!
    use headers
    implicit none
    integer, parameter :: dp = kind(1.d0)
    integer, intent(in) :: j
    integer ::    p
    real(kind=dp) :: qNode

    ! --- if there is flow into node then concen. = mass inflow/node flow
    qNode = Node(j)%inflow
    if ( qNode > ZERO ) then
        do p=1, Nobjects(E_POLLUT)
            Node(j)%newQual(p) = Node(j)%newQual(p) / qNode
        end do
    else 
    ! --- otherwise concen. is 0
        do p=1, Nobjects(E_POLLUT)
           Node(j)%newQual(p) = 0.0
        end do
    end if
end subroutine findNodeQual
!
!!=============================================================================
!
!!!  This function was significantly modified for release 5.0.017. !!       !(5.0.017 - LR)
!
subroutine findLinkQual(i, tStep)
!
!  Input:   i = link index
!           tStep = routing time step (sec)
!  Output:  none
!  Purpose: finds new quality in a link at end of the current time step.
!
    use headers
    implicit none
    integer, parameter :: dp = kind(1.d0)
    integer, intent(in) :: i
    real(kind=dp), intent(in) :: tStep
    integer ::    j          ! upstream node index
    integer :: k             ! conduit index
    integer :: p             ! pollutant index
    real(kind=dp) :: wIn     ! pollutant mass inflow rate (mass/sec)
    real(kind=dp) :: qIn     ! inflow rate (cfs)
    real(kind=dp) :: qOut    ! outflow rate (cfs)
    real(kind=dp) :: v1      ! link volume at start of time step (ft3)
    real(kind=dp) :: v2      ! link volume at end of time step (ft3)
    real(kind=dp) :: c1      ! current concentration within link (mass/ft3)
    real(kind=dp) :: c2      ! new concentration within link (mass/ft3)
    
    real(kind=dp) :: getMixedQual
    real(kind=dp) :: getReactedQual

    ! --- identify index of upstream node
    j = arrLink(i)%node1
    if ( arrLink(i)%newFlow < 0.0 ) then
        j = arrLink(i)%node2
    end if

    ! --- link concentration equals that of upstream node when
    !     link is not a conduit or is a dummy link
    if ( arrLink(i)%datatype /= E_CONDUIT .or. arrLink(i)%xsect%datatype == DUMMY ) then
        do p=1,Nobjects(E_POLLUT)
            arrLink(i)%newQual(p) = Node(j)%newQual(p)
        end do
        return
    end if

    ! --- concentrations are zero in an empty conduit
    if ( arrLink(i)%newDepth <= FUDGE ) then
        do p=1,Nobjects(E_POLLUT)
            arrLink(i)%newQual(p) = 0.0
        end do
        return
    end if

    ! --- Steady Flow routing requires special treatment
    if ( RouteModel == SF ) then
        call findSFLinkQual(i, tStep)
        return
    end if

    ! --- get inlet & outlet flow
    k = arrLink(i)%subIndex
    qIn  = abs(Conduit(k)%q1) * Conduit(k)%barrels
    qOut = abs(Conduit(k)%q2) * Conduit(k)%barrels

    ! --- get starting and ending volumes
    v1 = arrLink(i)%oldVolume
    v2 = arrLink(i)%newVolume

    ! --- adjust inflow to compensate for volume change when inflow = outflow
    if (qIn == qOut) then
        qIn = qIn + (v2 - v1) / tStep 
        qIn = MAX(qIn, 0.0)
    end if

    ! --- for each pollutant
    do p=1, Nobjects(E_POLLUT)
        ! --- determine mass lost to first order decay
        c1 = arrLink(i)%oldQual(p)
        c2 = getReactedQual(p, c1, v1, tStep)

        ! --- mix inflow to conduit with previous contents
        wIn = Node(j)%newQual(p)*qIn
        c2 = getMixedQual(c2, v1, wIn, qIn, tStep)

        ! --- assign new concen. to link
        arrLink(i)%newQual(p) = c2
    end do
end subroutine findLinkQual
!
!!=============================================================================
!
!!!  New function added to release 5.0.017. !!                              !(5.0.017 - LR)
!
subroutine findSFLinkQual(i, tStep)
!
!  Input:   i = link index
!           tStep = routing time step (sec)
!  Output:  none
!  Purpose: finds new quality in a link at end of the current time step for
!           Steady Flow routing.
!
    use headers
    use modLink
    use modMassbal
    implicit none
    integer, parameter :: dpq = kind(1.d0)
    integer, intent(in) :: i
    real(kind=dpq), intent(in) :: tStep
    integer :: j, p
    real(kind=dpq) :: c1, c2
    real(kind=dpq) :: lossRate
    real(kind=dpq) :: u
    real(kind=dpq) :: t
    j = arrLink(i)%node1
    ! --- find time of travel through conduit
    u = link_getVelocity(i, arrLink(i)%newFlow, arrLink(i)%newDepth)
    if ( u > ZERO ) then
       t = link_getLength(i) / u
    else 
       t = tStep
    end if

    ! --- for each pollutant
    do p=1, Nobjects(E_POLLUT)
        ! --- conduit's quality equals upstream node quality
        c1 = Node(j)%newQual(p)
        c2 = c1

        ! --- apply first-order decay over travel time
        if ( Pollut(p)%kDecay > 0.0 ) then
            c2 = c1 * exp(-Pollut(p)%kDecay * t)
            c2 = MAX(0.0, c2)
            lossRate = (c1 - c2) * arrLink(i)%newFlow
            call massbal_addReactedMass(p, lossRate)
        end if
        arrLink(i)%newQual(p) = c2
    end do
end subroutine findSFLinkQual
!
!!=============================================================================
!
!!!  This function was significantly modified for release 5.0.017. !!       !(5.0.017 - LR)
!
subroutine findStorageQual(j, tStep)
!
!  Input:   j = node index
!           tStep = routing time step (sec)
!  Output:  none
!  Purpose: finds new quality in a node with storage volume.
!  
    use headers
    implicit none
    integer, parameter :: dpq = kind(1.d0)
    integer, intent(in) :: j
    real(kind=dpq), intent(in) :: tStep
    integer ::    p        ! pollutant index
    real(kind=dpq) :: qIn   ! inflow rate (cfs)
    real(kind=dpq) :: wIn   ! pollutant mass inflow rate (mass)
    real(kind=dpq) :: v1    ! volume at start of time step (ft3)
    real(kind=dpq) :: c1    ! initial pollutant concentration (mass/ft3)
    real(kind=dpq) :: c2    ! final pollutant concentration (mass/ft3)
    
    real(kind=dpq) :: getMixedQual
    real(kind=dpq) :: getReactedQual


    ! --- get inflow rate & initial volume
    qIn = Node(j)%inflow
    v1 = Node(j)%oldVolume

    ! --- update hydraulic residence time for storage nodes
    !     (HRT can be used in treatment functions)
    if ( Node(j)%datatype == E_STORAGE ) then
        call updateHRT(j, Node(j)%oldVolume, qIn, tStep)
    end if

    ! --- for each pollutant
    do p = 1, Nobjects(E_POLLUT)
        ! --- get current concentration 
        c1 = Node(j)%oldQual(p)

        ! --- apply first order decay only if no separate treatment function
        if ( (.not. associated(Node(j)%treatment))) then !.or. (.not.associated(Node(j)%treatment(p)%equation)) )
            c2 = getReactedQual(p, c1, v1, tStep)
        else 
            c2 = c1
        end if

        ! --- mix inflow with current contents (mass inflow rate was
        !     temporarily saved in Node(j).newQual)
        wIn = Node(j)%newQual(p)
        c2 = getMixedQual(c1, v1, wIn, qIn, tStep)

        ! --- assign new concen. to node
        Node(j)%newQual(p) = c2
    end do
end subroutine findStorageQual
!
!!=============================================================================
!
subroutine updateHRT(j, v, q, tStep)
!
!  Input:   j = node index
!           v = storage volume (ft3)
!           q = inflow rate (cfs)
!           tStep = time step (sec)
!  Output:  none
!  Purpose: updates hydraulic residence time (i.e., water age) at a 
!           storage node.
!
    use headers
    implicit none
    integer, parameter :: dpq = kind(1.d0)
    integer, intent(in) :: j
    real(kind=dpq), intent(in) :: v, q, tStep
    integer ::    k
    real(kind=dpq) :: hrt
    
    k = Node(j)%subIndex
    hrt = Storage(k)%hrt

    if ( v < ZERO ) then
       hrt = 0.0
    else 
       hrt = (hrt + tStep) * v / (v + q*tStep)
    end if
    Storage(k)%hrt = MAX(hrt, 0.0)
end subroutine updateHRT
!
!!=============================================================================
!
!!!  New function added to release 5.0.017. !!                              !(5.0.017 - LR)
!
real(kind=kind(1.d0)) function getReactedQual(p, c, v1, tStep)
!
!  Input:   p = pollutant index
!           c = initial concentration (mass/ft3)
!           v1 = initial volume (ft3)
!           tStep = time step (sec)
!  Output:  none
!  Purpose: applies a first order reaction to a pollutant over a given
!           time step.
!
    use headers
    use modMassbal
    implicit none
    integer, parameter :: dpq = kind(1.d0)
    integer, intent(in) :: p
    real(kind=dpq), intent(in) :: c, v1, tStep
    real(kind=dpq) :: c2, lossRate, kDecay
    kDecay = Pollut(p)%kDecay

    if ( abs(kDecay - 0.0) < P_TINY ) then
        getReactedQual = c
    else
        c2 = c * (1.0 - kDecay * tStep)
        c2 = MAX(0.0, c2)
        lossRate = (c - c2) * v1 / tStep
        call massbal_addReactedMass(p, lossRate)
        getReactedQual = c2
    end if
    return
end function getReactedQual
!
!!=============================================================================
