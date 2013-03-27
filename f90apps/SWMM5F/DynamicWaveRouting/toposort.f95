module modToposort
!!-----------------------------------------------------------------------------
!!   toposort.c
!!
!!   Project:  EPA SWMM5
!!   Version:  5.0
!!   Date:     6/19/07   (Build 5.0.010)
!!             12/5/08   (Build 5.0.014)
!!             07/30/10  (Build 5.0.019)
!!   Author:   L. Rossman
!!
!!   Topological sorting of conveyance network links
!!-----------------------------------------------------------------------------
!#define _CRT_SECURE_NO_DEPRECATE
!
!#include <stdlib.h>
!#include "headers.h"
!
!-----------------------------------------------------------------------------
!  Constants
!-----------------------------------------------------------------------------
!enum AdjListType {
integer, parameter :: UNDIRECTED = 1
integer, parameter :: DIRECTED = 2
!}    ! type of nodal adjacency list

!!-----------------------------------------------------------------------------
!!  Shared variables
!!-----------------------------------------------------------------------------
integer, dimension(:), allocatable :: InDegree    ! number of incoming links to each node
integer, dimension(:), allocatable :: StartPos    ! start of a node's outlinks in AdjList
integer, dimension(:), allocatable :: AdjList     ! list of outlink indexes for each node
integer, dimension(:), allocatable :: Stack       ! array of nodes "reached" during sorting

integer :: First    ! position of first node in stack
integer :: Last     ! position of last node added to stack

logical :: Examined                 ! TRUE if node included in spanning tree
integer :: InTree                   ! state of each link in spanning tree:
                                 ! 0 = unexamined,
                                 ! 1 = in spanning tree,
                                 ! 2 = chord of spanning tree
integer, dimension(:), allocatable ::  LoopLinks                ! list of links which forms a loop
integer ::   LoopLinksLast            ! number of links in a loop

save
!!-----------------------------------------------------------------------------
!!  External functions (declared in funcs.h)   
!!-----------------------------------------------------------------------------
!!  toposort_sortLinks (called by routing_open)
!
!!-----------------------------------------------------------------------------
!!  Local functions
!!-----------------------------------------------------------------------------
!static void createAdjList(int listType)
!static void adjustAdjList(void)
!static int  topoSort(int sortedLinks())
!static void findCycles(void)
!static void findSpanningTree(int startNode)
!static void evalLoop(int startLink)
!static int  traceLoop(int i1, int i2, int k)
!static void checkDummyLinks(void)                                             !(5.0.014 - LR)
!

contains
!=============================================================================

subroutine toposort_sortLinks(sortedLinks)
!
!  Input:   none
!  Output:  sortedLinks = array of link indexes in sorted order
!  Purpose: sorts links from upstream to downstream.
!
    use headers
    implicit none
    integer, dimension(:), intent(inout) :: sortedLinks
    integer :: i, n
    integer :: lStat1, lStat2, lStat3, lStat4
    i = 0
    n = 0

    ! --- no need to sort links for Dyn. Wave routing
    do i=1, Nobjects(LINK)
       sortedLinks(i) = i
    end do
    if ( RouteModel == DW ) then

        ! --- check for nodes with both incoming and outgoing                 !(5.0.014 - LR)
        !     dummy links (creates ambiguous ordering)                        !(5.0.014 - LR)
        call checkDummyLinks()                                                     !(5.0.014 - LR)
        if ( ErrorCode /= 0 ) return                                               !(5.0.014 - LR)

        ! --- find number of outflow links for each node
        do i=1, Nobjects(E_NODE)
           Node(i)%degree = 0
        end do
        do i=1, Nobjects(LINK)
            ! --- if upstream node is an outfall, then increment outflow
            !     count for downstream node, otherwise increment count
            !     for upstream node
            n = arrLink(i)%node1
            if ( arrLink(i)%direction < 0 ) n = arrLink(i)%node2           !(5.0.014 - LR)
            if ( Node(n)%datatype == E_OUTFALL ) then
                if ( arrLink(i)%direction < 0 ) then
                    n = arrLink(i)%node1       !(5.0.014 - LR)
                else 
                    n = arrLink(i)%node2                                  !(5.0.014 - LR)
                end if
                Node(n)%degree = Node(n)%degree + 1
            else 
                Node(n)%degree = Node(n)%degree + 1
            end if
        end do
        return
    end if

    ! --- allocate arrays used for topo sorting
    if ( ErrorCode /= 0 ) return
    allocate(InDegree(Nobjects(E_NODE)), stat=lStat1)
    allocate(StartPos(Nobjects(E_NODE)), stat=lStat2)
    allocate(AdjList(Nobjects(LINK)), stat=lStat3)
    allocate(Stack(Nobjects(E_NODE)), stat=lStat4)
!    if ( InDegree == NULL || StartPos == NULL ||
!         AdjList == NULL || Stack == NULL )
!    {
    if (lStat1 /= 0 .or. lStat2 /= 0 .or. lStat3 /= 0 .or. lStat4 /= 0) then
        call report_writeErrorMsg(ERR_MEMORY, '')
    else
        ! --- create a directed adjacency list of links leaving each node
        call createAdjList(DIRECTED)

        ! --- adjust adjacency list for DIVIDER nodes
        call adjustAdjList()

        ! --- find number of links entering each node
        do i =1, Nobjects(E_NODE)
           InDegree(i) = 0
        end do
        do i =1, Nobjects(LINK)
           InDegree( arrLink(i)%node2 ) = InDegree( arrLink(i)%node2 ) + 1
        end do

        ! --- topo sort the links
        n = topoSort(sortedLinks)
    end if   

    ! --- free allocated memory
    deallocate(InDegree)
    deallocate(StartPos)
    deallocate(AdjList)
    deallocate(Stack)

    ! --- check that all links are included in SortedLinks
    if ( ErrorCode /= 0 .and.  n /= Nobjects(LINK) ) then
        call report_writeErrorMsg(ERR_LOOP, '')
        call findCycles()
    end if
end subroutine toposort_sortLinks
!
!!=============================================================================
!
!void createAdjList(int listType)
!!
!!  Input:   lsitType = DIRECTED or UNDIRECTED
!!  Output:  none
!!  Purpose: creates listing of links incident on each node.
!!
!{
!    int i, j, k
!
!    ! --- determine degree of each node
!    !     (for DIRECTED list only count link at its upstream node
!    !      for UNDIRECTED list count link at both end nodes)
!    for (i = 0 i < Nobjects(NODE) i++) Node(i).degree = 0
!    for (j = 0 j < Nobjects(LINK) j++)
!    {
!        Node( arrLink(j).node1 ).degree++
!        if ( listType == UNDIRECTED ) Node( arrLink(j).node2 ).degree++
!    }
!
!    ! --- determine start position of each node in the adjacency list
!    !     (the adjacency list, AdjList, is one long vector containing
!    !      the individual node lists one after the other)
!    StartPos(0) = 0
!    for (i = 0 i < Nobjects(NODE)-1 i++)
!    {
!        StartPos(i+1) = StartPos(i) + Node(i).degree
!        Node(i).degree = 0
!    }
!    Node(Nobjects(NODE)-1).degree = 0
!
!    ! --- traverse the list of links once more,
!    !     adding each link's index to the proper 
!    !     position in the adjacency list
!    for (j = 0 j < Nobjects(LINK) j++)
!    {
!        i = arrLink(j).node1
!        k = StartPos(i) + Node(i).degree
!        AdjList(k) = j
!        Node(i).degree++
!        if ( listType == UNDIRECTED )
!        {
!            i = arrLink(j).node2
!            k = StartPos(i) + Node(i).degree
!            AdjList(k) = j
!            Node(i).degree++
!        }
!    }
!}
!
!!=============================================================================
!
!void adjustAdjList()
!!
!!  Input:   none
!!  Output:  none
!!  Purpose: adjusts adjacency list for Divider nodes so that non-
!!           diversion link appears before diversion link.
!!
!{
!    int i, j, k, m
!
!    ! --- check each node
!    for (i=0 i<Nobjects(NODE) i++)
!    {
!        ! --- skip nodes that are not Dividers
!        if ( Node(i)%datatype != DIVIDER ) continue
!        if ( Node(i).degree != 2 ) continue
!
!        ! --- switch position of outgoing links at the node if the
!        !     diversion link appears first in the adjacency list
!        k = Node(i).subIndex
!        m = StartPos(i)
!        j = AdjList(m)
!        if ( j == Divider(k).link )
!        {
!            AdjList(m) = AdjList(m+1)
!            AdjList(m+1) = j
!        }
!    }
!}
!
!=============================================================================

integer function topoSort(sortedLinks)
!
!  Input:   none
!  Output:  sortedLinks = array of sorted link indexes,
!           returns number of links successfully sorted
!  Purpose: performs a stack-based topo sort of the drainage network's links.
!
    use headers
    implicit none
    integer, dimension(:), intent(inout) :: sortedLinks
    integer :: i, j, k, n
    integer :: i1, i2, mk1, mk2

    ! --- initialize a stack which contains nodes with zero in-degree
    First = 0
    Last = -1
    do i = 1, Nobjects(E_NODE)
        if ( InDegree(i) == 0 ) then
            Last = Last + 1
            Stack(Last) = i
        end if
    end do

    ! --- traverse the stack, adding each node's outgoing link indexes
    !     to the SortedLinks array in the order processed
    n = 0
    do while ( First <= Last )
        ! --- determine range of adjacency list indexes belonging to 
        !     first node remaining on the stack
        i1 = Stack(First)
        mk1 = StartPos(i1)
        mk2 = mk1 + Node(i1)%degree

        ! --- for each outgoing link from first node on stack
        do k = mk1, mk2
            ! --- add link index to current position in SortedLinks
            j = AdjList(k)
            sortedLinks(n) = j
            n = n + 1

            ! --- reduce in-degree of link's downstream node
            i2 = arrLink(j)%node2
            InDegree(i2)  = InDegree(i2) - 1

            ! --- add downstream node to stack if its in-degree is zero
            if ( InDegree(i2) == 0 ) then
                Last = Last + 1
                Stack(Last) = i2
            end if  
        end do
        First = First + 1
    end do
    topoSort = n
end function topoSort
!
!!=============================================================================
!
!void  findCycles()
!!
!!  Input:   none
!!  Output:  none
!!  Purpose: finds all cycles in the drainage network (i.e., closed loops that
!!           start and end at the same node).
!!
!{
!    int i
!
!    ! --- allocate arrays
!    AdjList  = (int *) calloc(2*Nobjects(LINK), sizeof(int))
!    StartPos = (int *) calloc(Nobjects(NODE), sizeof(int))
!    Stack    = (int *) calloc(Nobjects(NODE), sizeof(int))
!    Examined = (char *) calloc(Nobjects(NODE), sizeof(char))
!    InTree   = (char *) calloc(Nobjects(LINK), sizeof(char))
!    LoopLinks = (int *) calloc(Nobjects(LINK), sizeof(int))
!    if ( StartPos && AdjList && Stack && Examined && InTree && LoopLinks )
!    {
!        ! --- create an undirected adjacency list for the nodes
!        createAdjList(UNDIRECTED)
!
!        ! --- set to empty the list of nodes examined and the list
!        !     of links in the spanning tree
!        for ( i=0 i<Nobjects(NODE) i++) Examined(i) = 0
!        for ( i=0 i<Nobjects(LINK) i++) InTree(i) = 0
!
!        ! --- find a spanning tree for each unexamined node
!        !     (cycles are identified as tree is constructed)
!        for ( i=0 i<Nobjects(NODE) i++)
!        {
!            if ( Examined(i) ) continue
!            Last = -1
!            findSpanningTree(i)
!        }
!    }
!    FREE(StartPos)
!    FREE(AdjList)
!    FREE(Stack)
!    FREE(Examined)
!    FREE(InTree)
!    FREE(LoopLinks)
!}
!
!!=============================================================================
!
!void  findSpanningTree(int startNode)
!!
!!  Input:   i = index of starting node of tree
!!  Output:  none
!!  Purpose: finds continuation of network's spanning tree of links.
!!
!{
!    int nextNode, j, k, m
!
!    ! --- examine each link connected to node i
!    for ( m = StartPos(startNode)
!          m < StartPos(startNode)+Node(startNode).degree m++ )
!    {
!        ! --- find which node (j) connects link k from start node
!        k = AdjList(m)
!        if ( arrLink(k).node1 == startNode ) j = arrLink(k).node2
!        else j = arrLink(k).node1
!
!        ! --- if link k is not in the tree
!        if ( InTree(k) == 0 )
!        {
!            ! --- if connecting node already examined,
!            !     then link k forms a loop mark it as a chord
!            !     and check if loop forms a cycle
!            if ( Examined(j) )
!            {
!                InTree(k) = 2
!                evalLoop(k)
!            }
!
!            ! --- otherwise mark connected node as being examined,
!            !     add it to the node stack, and mark the connecting
!            !     link as being in the spanning tree
!            else
!            {
!                Examined(j) = 1
!                Last++
!                Stack(Last) = j
!                InTree(k) = 1
!            }
!        }
!    }
!
!    ! --- continue to grow the spanning tree from
!    !     the last node added to the stack
!    if ( Last >= 0 )
!    {
!        nextNode = Stack(Last)
!        Last--
!        findSpanningTree(nextNode)
!    }
!}
!
!!=============================================================================
!
!void evalLoop(int startLink)
!!
!!  Input:   startLink = index of starting link of a loop
!!  Output:  none
!!  Purpose: checks if loop starting with a given link forms a closed cycle.
!!
!{
!    int i                             ! loop list index
!    int i1, i2                        ! start & end node indexes
!    int j                             ! index of link in loop
!    int kount                         ! items per line counter
!    int isCycle                       ! TRUE if loop forms a cycle
!
!    ! --- make startLink the first link in the loop
!    LoopLinksLast = 0
!    LoopLinks(0) = startLink
!
!    ! --- trace a path on the spanning tree that starts at the
!    !     tail node of startLink and ends at its head node
!    i1 = arrLink(startLink).node1
!    i2 = arrLink(startLink).node2
!    if ( !traceLoop(i1, i2, startLink) ) return
!
!    ! --- check if all links on the path are oriented head-to-tail
!    isCycle = TRUE
!    j = LoopLinks(0)
!    i2 = arrLink(j).node2
!    for (i=1 i<=LoopLinksLast i++)
!    {
!        j = LoopLinks(i)
!        i1 = arrLink(j).node1
!        if ( i1 == i2 ) i2 = arrLink(j).node2
!        else
!        {
!            isCycle = FALSE
!            break
!        }
!    }
!
!    ! --- print cycle to report file
!    if ( isCycle )
!    {
!        kount = 0
!        for (i = 0 i <= LoopLinksLast i++)
!        {
!            if ( kount % 5 == 0 ) fprintf(Frpt.file, "\n")
!            kount++
!            fprintf(Frpt.file, "  %s", arrLink(LoopLinks(i)).ID)
!            if ( i < LoopLinksLast ) fprintf(Frpt.file, "  -->")
!        }
!    }
!}
!
!!=============================================================================
!
!int traceLoop(int i1, int i2, int k1)
!!
!!  Input:   i1 = index of current node reached while tracing a loop
!!           i2 = index of final node on the loop
!!           k1 = index of link which extends loop to node i1
!!  Output:  returns TRUE if loop can be extended through node i1
!!  Purpose: tries to extend closed loop through current node.
!!
!{
!    int j, k, m
!
!    ! --- if current node equals final node then return with loop found
!    if ( i1 == i2 ) return TRUE
!
!    ! --- examine each link connected to current node
!    for (m = StartPos(i1) m < StartPos(i1) + Node(i1).degree m++)
!    {
!        ! --- ignore link if it comes from predecessor node or if
!        !     it is not on the spanning tree
!        k = AdjList(m)
!        if ( k == k1 || InTree(k) != 1 ) continue
!
!        ! --- identify other node that link is connected to
!        if ( arrLink(k).node1 == i1 ) j = arrLink(k).node2
!        else                       j = arrLink(k).node1
!
!        ! --- try to continue tracing the loop from this node
!        !     if successful, then add link to loop and return
!        if ( traceLoop(j, i2, k) )
!        {
!            LoopLinksLast++
!            LoopLinks(LoopLinksLast) = k
!            return TRUE
!        }
!    }
!
!    ! --- return false if loop cannot be continued from current node
!    return FALSE
!}
!
!=============================================================================

!!  ----  This is a new function added for Release 5.0.014  ----  !!       !(5.0.014 - LR)

subroutine checkDummyLinks()
!
!  Input:   none
!  Output:  none
!  Purpose: checks for nodes that have both incoming and outgoing dummy links.
!
    use headers
    implicit none
    integer ::   i, j, lStat
    integer, dimension(:), allocatable :: marked

    ! --- create an array that marks nodes
    !     (calloc initializes the array to 0)
    allocate(marked(Nobjects(E_NODE)), stat=lStat)
    if ( lStat /= 0 ) then
        call report_writeErrorMsg(ERR_MEMORY, '')
        return
    end if

    ! --- mark nodes that whose incoming links are all
    !     either dummy links or ideal pumps
    do i =1, Nobjects(LINK)
        j = arrLink(i)%node2
        if ( arrLink(i)%direction < 0 ) j = arrLink(i)%node1
        if ( (arrLink(i)%datatype == E_CONDUIT .and. arrLink(i)%xsect%datatype == DUMMY) .or. &       !(5.0.019 - LR)
            &(arrLink(i)%datatype == E_PUMP .and. &
             &Pump(arrLink(i)%subIndex)%datatype == IDEAL_PUMP) ) then
            if ( marked(j) == 0 ) marked(j) = 1
        else 
            marked(j) = -1
        end if
    end do

    ! --- find marked nodes with outgoing dummy links or ideal pumps
    do i =1, Nobjects(LINK)
        if ( (arrLink(i)%datatype == E_CONDUIT .and. arrLink(i)%xsect%datatype == DUMMY) .or. &       !(5.0.019 - LR)
            &(arrLink(i)%datatype == E_PUMP .and. & 
            & Pump(arrLink(i)%subIndex)%datatype == IDEAL_PUMP) ) then
            j = arrLink(i)%node1
            if ( marked(j) > 0 ) &
              & call report_writeErrorMsg(ERR_MULTI_DUMMY_LINK, Node(j)%ID)
        end if
    end do
    deallocate(marked)
end subroutine checkDummyLinks
!
!!=============================================================================
!
end module