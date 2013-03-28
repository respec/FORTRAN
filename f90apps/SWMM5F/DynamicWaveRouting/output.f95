module output
!-----------------------------------------------------------------------------
!   output.c
!
!   Project:  EPA SWMM5
!   Version:  5.0
!   Date:     6/19/07   (Build 5.0.010)
!             2/4/08    (Build 5.0.012)
!             1/21/09   (Build 5.0.014)
!             4/10/09   (Build 5.0.015)
!             6/22/09   (Build 5.0.016)
!   Author:   L. Rossman
!
!   Binary output file access functions.
!-----------------------------------------------------------------------------
!#define _CRT_SECURE_NO_DEPRECATE
!
!#include <stdlib.h>
!#include <string.h>
!#include "headers.h"
!
use headers
use swmm5futil
!
! Definition of 4-byte integer, 4-byte real and 8-byte real types             !(5.0.014 - LR)
!#define INT4  int
!#define REAL4 float
!#define REAL8 double
!
!enum InputDataType {
integer, parameter :: INPUT_TYPE_CODE = 1
integer, parameter :: INPUT_AREA = 2
integer, parameter :: INPUT_INVERT = 3
integer, parameter :: INPUT_MAX_DEPTH = 4
integer, parameter :: INPUT_OFFSET = 5
integer, parameter :: INPUT_LENGTH = 6
!}

!-----------------------------------------------------------------------------
!  Shared variables    
!-----------------------------------------------------------------------------
integer :: IDStartPos           !static INT4 starting file position of ID names
integer :: OutputStartPos       !static INT4 starting file position of output data
integer :: BytesPerPeriod       !static INT4 bytes saved per simulation time period
integer :: NsubcatchResults     !static INT4 number of subcatchment output variables
integer :: NnodeResults         !static INT4 number of node output variables
integer :: NlinkResults         !static INT4 number of link output variables
integer :: NumSubcatch          !static INT4 number of subcatchments reported on  !(5.0.014 - LR)
integer :: NumNodes             !static INT4 number of nodes reported on          !(5.0.014 - LR)
integer :: NumLinks             !static INT4 number of links reported on          !(5.0.014 - LR)
real :: SysResults(MAX_SYS_RESULTS)    !static REAL4 values of system output vars.

!time-value array holding outlet flow result
!real, dimension(:), pointer :: TSOutletVals
real, dimension(:), pointer :: TSDateTime
integer :: OutputSize
integer :: OutputCount

!-----------------------------------------------------------------------------
!  Exportable variables (shared with report.c), put in swmm5futil
!-----------------------------------------------------------------------------
!REAL4*           SubcatchResults
!REAL4*           NodeResults
!REAL4*           LinkResults

!-----------------------------------------------------------------------------
!  Local functions
!-----------------------------------------------------------------------------
!static void output_openOutFile(void)
!static void output_saveID(char* id, FILE* file)
!static void output_saveSubcatchResults(double reportTime, FILE* file)
!static void output_saveNodeResults(double reportTime, FILE* file)
!static void output_saveLinkResults(double reportTime, FILE* file)

!-----------------------------------------------------------------------------
!  External functions (declared in funcs.h)
!-----------------------------------------------------------------------------
!  output_open                   (called by swmm_start in swmm5.c)
!  output_end                    (called by swmm_end in swmm5.c)
!  output_close                  (called by swmm_close in swmm5.c)
!  output_saveResults            (called by swmm_step in swmm5.c)
!  output_checkFileSize          (called by swmm_report)                      !(5.0.015 - LR)
!  output_readDateTime           (called by routines in report.c)
!  output_readSubcatchResults    (called by report_Subcatchments)
!  output_readNodeResults        (called by report_Nodes)
!  output_readLinkResults        (called by report_Links)
save
!=============================================================================
contains
integer function output_open()
!
!  Input:   none
!  Output:  returns an error code
!  Purpose: writes basic project data to binary output file.
!
    use headers
    use swmm5futil
    use report
    implicit none
    integer ::   nPolluts, j, m
    integer ::  k
    real :: x
    double precision :: z
    nPolluts = Nobjects(E_POLLUT)
    ! --- open binary output file
!    output_openOutFile()
    if ( ErrorCode /= 0) then
       output_open = ErrorCode
       return
    end if

    ! --- subcatchment results consist of Rainfall, Snowdepth, Losses, Runoff, 
    !     GW Flow, GW Elev, and Washoff
    NsubcatchResults = MAX_SUBCATCH_RESULTS - 1 + nPolluts

    ! --- node results consist of Depth, Head, Volume, Lateral Inflow,
    !     Total Inflow, Overflow and Quality
    NnodeResults = MAX_NODE_RESULTS - 1 + nPolluts

    ! --- link results consist of Depth, Flow, Velocity, Froude No.,
    !     Capacity and Quality
    NlinkResults = MAX_LINK_RESULTS - 1 + nPolluts

    ! --- get number of objects reported on                                   !(5.0.014 - LR)
    NumSubcatch = 0
    NumNodes = 0
    NumLinks = 0
    do j=1, Nobjects(E_SUBCATCH)
       if (Subcatch(j)%rptFlag) NumSubcatch = NumSubcatch + 1
    end do
    do j=1, Nobjects(E_NODE)
       if (Node(j)%rptFlag) NumNodes = NumNodes + 1
    end do
    do j=1, Nobjects(LINK)
       if (arrLink(j)%rptFlag) NumLinks = NumLinks + 1
    end do


!    BytesPerPeriod = sizeof(REAL8)                                             !(5.0.014 - LR)
!        + NumSubcatch * NsubcatchResults * sizeof(REAL4)                       !(5.0.014 - LR)
!        + NumNodes * NnodeResults * sizeof(REAL4)                              !(5.0.014 - LR)
!        + NumLinks * NlinkResults * sizeof(REAL4)                              !(5.0.014 - LR)
!        + MAX_SYS_RESULTS * sizeof(REAL4)
        
    Nperiods = 0
    OutputSize = 100 !initial size, will expand as needed
    OutputCount = 0
    deallocate(SubcatchResults)
    deallocate(NodeResults)
    deallocate(LinkResults)
!    deallocate(TSOutletVals)
    deallocate(TSDateTime)
    allocate(SubcatchResults(NsubcatchResults))
    allocate(NodeResults(NnodeResults))
    allocate(LinkResults(NlinkResults))
!    allocate(TSOutletVals(OutputSize))
    allocate(TSDateTime(OutputSize))
    if ( .not. allocated(SubcatchResults) .or. .not. allocated(NodeResults) .or. .not. allocated(LinkResults )) then
        call report_writeErrorMsg(ERR_MEMORY, "")
        output_open = ErrorCode
        return
    end if

    allocate(onodes(Nobjects(E_NODE)))
    allocate(olinks(Nobjects(LINK)))
    do j= 1,Nobjects(E_NODE)
      onodes%datatype = E_NODE
      onodes%index = j
      allocate(onodes(j)%oflow(OutputSize))
      allocate(onodes(j)%odepth(OutputSize))
      allocate(onodes(j)%ovolume(OutputSize))
    end do
    do j= 1,Nobjects(LINK)
      olinks%datatype = LINK
      olinks%index = j
      allocate(olinks(j)%oflow(OutputSize))
      allocate(olinks(j)%odepth(OutputSize))
      allocate(olinks(j)%ovolume(OutputSize))
    end do

!    fseek(Fout.file, 0, SEEK_SET)
!    k = MAGICNUMBER
!    fwrite(&k, sizeof(INT4), 1, Fout.file)   ! Magic number
!    k = VERSION
!    fwrite(&k, sizeof(INT4), 1, Fout.file)   ! Version number
!    k = FlowUnits
!    fwrite(&k, sizeof(INT4), 1, Fout.file)   ! Flow units
!    k = NumSubcatch                                                           !(5.0.014 - LR)
!    fwrite(&k, sizeof(INT4), 1, Fout.file)   ! # subcatchments
!    k = NumNodes                                                              !(5.0.014 - LR)
!    fwrite(&k, sizeof(INT4), 1, Fout.file)   ! # nodes
!    k = NumLinks                                                              !(5.0.014 - LR) 
!    fwrite(&k, sizeof(INT4), 1, Fout.file)   ! # links
!    k = Nobjects(POLLUT)
!    fwrite(&k, sizeof(INT4), 1, Fout.file)   ! # pollutants
!
!    ! --- save ID names of subcatchments, nodes, links, & pollutants          !(5.0.014 - LR)
!    IDStartPos = ftell(Fout.file)
!    for (j=0 j<Nobjects(SUBCATCH) j++)
!    {
!        if ( Subcatch(j).rptFlag ) output_saveID(Subcatch(j).ID, Fout.file)
!    }
!    for (j=0 j<Nobjects(NODE)     j++)
!    {
!        if ( Node(j).rptFlag ) output_saveID(Node(j).ID, Fout.file)
!    }
!    for (j=0 j<Nobjects(LINK)     j++)
!    {
!        if ( arrLink(j).rptFlag ) output_saveID(arrLink(j).ID, Fout.file)
!    }
!    for (j=0 j<Nobjects(POLLUT)   j++) output_saveID(Pollut(j).ID, Fout.file)
!
!    ! --- save codes of pollutant concentration units
!    for (j=0 j<Nobjects(POLLUT) j++)
!    {
!        k = Pollut(j).units
!        fwrite(&k, sizeof(INT4), 1, Fout.file)
!    }
!
!    InputStartPos = ftell(Fout.file)
!
!    ! --- save subcatchment area
!    k = 1
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = INPUT_AREA
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    for (j=0 j<Nobjects(SUBCATCH) j++)
!    {
!         if ( !Subcatch(j).rptFlag ) continue                                 !(5.0.014 - LR)
!         SubcatchResults(0) = (REAL4)(Subcatch(j).area * UCF(LANDAREA))
!         fwrite(&SubcatchResults(0), sizeof(REAL4), 1, Fout.file)
!    }
!
!    ! --- save node type, invert, & max. depth
!    k = 3
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = INPUT_TYPE_CODE
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = INPUT_INVERT
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = INPUT_MAX_DEPTH
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
    do j=1, Nobjects(E_NODE)
        if ( .not. Node(j)%rptFlag ) cycle                                      !(5.0.014 - LR)
        k = Node(j)%datatype
        NodeResults(1) = (Node(j)%invertElev * UCF(LENGTH))
        NodeResults(2) = (Node(j)%fullDepth * UCF(LENGTH))
!        fwrite(&k, sizeof(INT4), 1, Fout.file)
!        fwrite(NodeResults, sizeof(REAL4), 2, Fout.file)
    end do

    ! --- save link type, offsets, max. depth, & length
!    k = 5
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = INPUT_TYPE_CODE
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = INPUT_OFFSET
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = INPUT_OFFSET
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = INPUT_MAX_DEPTH
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = INPUT_LENGTH
!    fwrite(&k, sizeof(INT4), 1, Fout.file)

    do j=1, Nobjects(LINK)
        if ( .not. arrLink(j)%rptFlag ) cycle                                      !(5.0.014 - LR)
        k = arrLink(j)%datatype
        if ( k == E_PUMP ) then
            do m=1, 3
               LinkResults(m) = 0.0 !f
            end do
        else
            LinkResults(1) = (arrLink(j)%offset1 * UCF(LENGTH))           !(5.0.012 - LR)
            LinkResults(2) = (arrLink(j)%offset2 * UCF(LENGTH))           !(5.0.012 - LR)
            if ( arrLink(j)%direction < 0 ) then
                x = LinkResults(1)
                LinkResults(1) = LinkResults(2)
                LinkResults(2) = x
            end if
            if ( k == E_OUTLET ) then
                LinkResults(3) = 0.0 !f
            else 
                LinkResults(3) = (arrLink(j)%xsect%yFull * UCF(LENGTH))
            end if
            if ( k == E_CONDUIT ) then
                m = arrLink(j)%subIndex
                LinkResults(4) = (Conduit(m)%clength * UCF(LENGTH))
            else 
                LinkResults(4) = 0.0 !f
            end if
        end if
!        fwrite(&k, sizeof(INT4), 1, Fout.file)
!        fwrite(LinkResults, sizeof(REAL4), 4, Fout.file)
    end do

    ! --- save number & codes of subcatchment result variables
!    k = NsubcatchResults
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = SUBCATCH_RAINFALL
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = SUBCATCH_SNOWDEPTH
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = SUBCATCH_LOSSES
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = SUBCATCH_RUNOFF
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = SUBCATCH_GW_FLOW
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = SUBCATCH_GW_ELEV
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    for (j=0 j<nPolluts j++)
!    {
!        k = SUBCATCH_WASHOFF + j
!        fwrite(&k, sizeof(INT4), 1, Fout.file)
!    }

!    ! --- save number & codes of node result variables
!    k = NnodeResults
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = NODE_DEPTH
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = NODE_HEAD
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = NODE_VOLUME
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = NODE_LATFLOW
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = NODE_INFLOW
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = NODE_OVERFLOW
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    for (j=0 j<nPolluts j++)
!    {
!        k = NODE_QUAL + j
!        fwrite(&k, sizeof(INT4), 1, Fout.file)
!    }
!
!    ! --- save number & codes of link result variables
!    k = NlinkResults
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = LINK_FLOW
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = LINK_DEPTH
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = LINK_VELOCITY
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = LINK_FROUDE
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = LINK_CAPACITY
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    for (j=0 j<nPolluts j++)
!    {
!        k = LINK_QUAL + j
!        fwrite(&k, sizeof(INT4), 1, Fout.file)
!    }
!
!    ! --- save number & codes of system result variables
!    k = MAX_SYS_RESULTS
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    for (k=0 k<MAX_SYS_RESULTS k++) fwrite(&k, sizeof(INT4), 1, Fout.file)

    ! --- save starting report date & report step
    !     (if reporting start date > simulation start date then               !(5.0.014 - LR)
    !      make saved starting report date one reporting period               !(5.0.014 - LR)
    !      prior to the date of the first reported result)                    !(5.0.014 - LR)
    z = ReportStep/86400.0 * 1.d00                                          !(5.0.014 - LR)
    if ( StartDateTime + z > ReportStart ) then
        z = StartDateTime                  !(5.0.014 - LR)
    else                                                                       !(5.0.014 - LR)
        z = floor((ReportStart - StartDateTime)/z) - 1.0                      !(5.0.014 - LR)
        z = StartDateTime + z* ReportStep/86400.0 * 1.d00                      !(5.0.014 - LR)
    end if                                                                          !(5.0.014 - LR)
!    fwrite(&z, sizeof(REAL8), 1, Fout.file)
!    k = ReportStep
!    if ( fwrite(&k, sizeof(INT4), 1, Fout.file) < 1)
!    {
!        report_writeErrorMsg(ERR_OUT_WRITE, "")
!        return ErrorCode
!    }
!    OutputStartPos = ftell(Fout.file)
!    if ( Fout.mode == SCRATCH_FILE ) output_checkFileSize()                   !(5.0.015 - LR)
     output_open = ErrorCode
end function output_open
!!=============================================================================
!
!!!  New function added for release 5.0.015.  !!                            !(5.0.015 - LR)
!
subroutine output_checkFileSize()
!
!  Input:   none
!  Output:  none
!  Purpose: checks if the size of the binary output file will be too big
!           to access using an integer file pointer variable.
!
    use headers
    implicit none
    
!    if ( RptFlags.subcatchments != NONE ||
!         RptFlags.nodes != NONE ||
!         RptFlags.links != NONE )
!    {
!        if ( (double)OutputStartPos + (double)BytesPerPeriod * TotalDuration
!             / 1000.0 / (double)ReportStep >= (double)MAXFILESIZE )
!        {
!            report_writeErrorMsg(ERR_FILE_SIZE, "")
!        }
!    }
end subroutine output_checkFileSize
!
!
!!=============================================================================
!
subroutine output_openOutFile()
!
!  Input:   none
!  Output:  none
!  Purpose: opens a project's binary output file.
!
    use headers
    implicit none
    
!    ! --- close output file if already opened
!    if (Fout.file != NULL) fclose(Fout.file) 
!
!    ! --- else if file name supplied then set file mode to SAVE
!    else if (strlen(Fout.name) != 0) Fout.mode = SAVE_FILE
!
!    ! --- otherwise set file mode to SCRATCH & generate a name
!    else
!    {
!        Fout.mode = SCRATCH_FILE
!        getTmpName(Fout.name)
!    }
!
!    ! --- try to open the file
!    if ( (Fout.file = fopen(Fout.name, "w+b")) == NULL)
!    {
!        writecon(FMT14)
!        ErrorCode = ERR_OUT_FILE
!    }
end subroutine output_openOutFile
!
!!=============================================================================
!
subroutine output_saveResults(aReportTime)
!
!  Input:   aReportTime = elapsed simulation time (millisec)
!  Output:  none
!  Purpose: writes computed results for current report time to binary file.
!
    use headers
    
    implicit none
    double precision, intent(in) :: aReportTime
    integer :: i
    double precision :: reportDate, date !REAL8 date

    reportDate = getDateTime(StartDateTime, aReportTime)
    if ( reportDate < ReportStart ) return
    do i=1, MAX_SYS_RESULTS
       SysResults(i) = 0.0 !f
    end do
    date = reportDate
!    fwrite(&date, sizeof(REAL8), 1, Fout.file)
!    if (Nobjects(E_SUBCATCH) > 0)
!        call output_saveSubcatchResults(aReportTime, Fout.file)
    if (Nobjects(E_NODE) > 0) then
        OutputCount = OutputCount + 1
        call output_saveNodeResults(aReportTime, Fout%fileHandle)
    end if
    if (Nobjects(LINK) > 0) then
        call output_saveLinkResults(aReportTime, Fout%fileHandle)
    end if
!    fwrite(SysResults, sizeof(REAL4), MAX_SYS_RESULTS, Fout.file)
!    if ( Foutflows.mode == SAVE_FILE .and. RouteModel /= NO_ROUTING ) &           !(5.0.014 - LR)
!       &iface_saveOutletResults(reportDate, Foutflows.file)
    Nperiods = Nperiods + 1
end subroutine output_saveResults
!
!!=============================================================================
!
subroutine output_end()
!
!  Input:   none
!  Output:  none
!  Purpose: writes closing records to binary file.
!
    use headers
    implicit none
    
!    INT4 k
!    fwrite(&IDStartPos, sizeof(INT4), 1, Fout.file)
!    fwrite(&InputStartPos, sizeof(INT4), 1, Fout.file)
!    fwrite(&OutputStartPos, sizeof(INT4), 1, Fout.file)
!    k = Nperiods
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = (INT4)error_getCode(ErrorCode)
!    fwrite(&k, sizeof(INT4), 1, Fout.file)
!    k = MAGICNUMBER
!    if (fwrite(&k, sizeof(INT4), 1, Fout.file) < 1)
!    {
!        report_writeErrorMsg(ERR_OUT_WRITE, "")
!    }
end subroutine output_end
!
!!=============================================================================
!
subroutine output_close()
!
!  Input:   none
!  Output:  none
!  Purpose: frees memory used for accessing the binary file.
!
    use swmm5futil
    implicit none
    integer :: i
    deallocate(SubcatchResults)
    deallocate(NodeResults)
    deallocate(LinkResults)
    
    deallocate(TSDateTime)
    do i=1, Nobjects(E_NODE)
       if (associated(onodes(i)%oflow)) then
          deallocate(onodes(i)%oflow)
          nullify(onodes(i)%oflow)
       end if
       if (associated(onodes(i)%odepth)) then
          deallocate(onodes(i)%odepth)
          nullify(onodes(i)%odepth)
       end if
       if (associated(onodes(i)%ovolume)) then
          deallocate(onodes(i)%ovolume)
          nullify(onodes(i)%ovolume)
       end if
    end do
    deallocate(onodes)
    
    do i=1, Nobjects(LINK)
       if (associated(olinks(i)%oflow)) then
          deallocate(olinks(i)%oflow)
          nullify(olinks(i)%oflow)
       end if
       if (associated(olinks(i)%odepth)) then
          deallocate(olinks(i)%odepth)
          nullify(olinks(i)%odepth)
       end if
       if (associated(olinks(i)%ovolume)) then
          deallocate(olinks(i)%ovolume)
          nullify(olinks(i)%ovolume)
       end if
    end do
    deallocate(olinks)
    
end subroutine output_close
!
!!=============================================================================
!
subroutine output_saveID(id, file)
!  char* id, FILE* file
!  Input:   id = name of an object
!           file = ptr. to binary output file
!  Output:  none
!  Purpose: writes an object's name to the binary output file.
!
    implicit none
    character(*), intent(in) :: id
    integer, intent(in) :: file
    
!    INT4 n = strlen(id)
!    fwrite(&n, sizeof(INT4), 1, file)
!    fwrite(id, sizeof(char), n, file)
end subroutine output_saveID
!
!!=============================================================================
!
subroutine output_saveSubcatchResults(aReportTime, file)
!
!  Input:   aReportTime = elapsed simulation time (millisec)
!           file = ptr. to binary output file
!  Output:  none
!  Purpose: writes computed subcatchment results to binary file.
!
    implicit none
    double precision, intent(In) :: aReportTime
    integer, intent(in) :: file
    
    integer ::      j
    double precision ::   f, area
    real :: totalArea
    double precision :: reportDate
    
!    totalArea = 0.0 !f
!    reportDate = getDateTime(aReportTime)
!
!    ! --- update reported rainfall at each rain gage
!    for ( j=0 j<Nobjects(GAGE) j++ )
!    {
!        gage_setReportRainfall(j, reportDate)
!    }
!
!    ! --- find where current reporting time lies between latest runoff times
!    f = (aReportTime - OldRunoffTime) / (NewRunoffTime - OldRunoffTime)
!
!    ! --- write subcatchment results to file
!    for ( j=0 j<Nobjects(SUBCATCH) j++)
!    {
!        ! --- retrieve interpolated results for reporting time & write to file
!        subcatch_getResults(j, f, SubcatchResults)
!        if ( Subcatch(j).rptFlag )                                             !(5.0.014 - LR)
!            fwrite(SubcatchResults, sizeof(REAL4), NsubcatchResults, file)    !(5.0.014 - LR)
!
!        ! --- update system-wide results
!        area = Subcatch(j).area * UCF(LANDAREA)
!        totalArea += (REAL4)area
!        SysResults(SYS_RAINFALL) +=
!            (REAL4)(SubcatchResults(SUBCATCH_RAINFALL) * area)
!        SysResults(SYS_SNOWDEPTH) +=
!            (REAL4)(SubcatchResults(SUBCATCH_SNOWDEPTH) * area)
!        SysResults(SYS_LOSSES) +=
!            (REAL4)(SubcatchResults(SUBCATCH_LOSSES) * area)
!        SysResults(SYS_RUNOFF) += (REAL4)SubcatchResults(SUBCATCH_RUNOFF)
!    }
!
!    ! --- normalize system-wide results to catchment area
!    if ( UnitSystem == SI ) f = (5./9.) * (Temp.ta - 32.0)
!    else f = Temp.ta
!    SysResults(SYS_TEMPERATURE) = (REAL4)f
!    SysResults(SYS_EVAPORATION) = (REAL4)(Evap.rate * UCF(EVAPRATE))
!    SysResults(SYS_RAINFALL)  /= totalArea
!    SysResults(SYS_SNOWDEPTH) /= totalArea
!    SysResults(SYS_LOSSES)    /= totalArea
end subroutine output_saveSubcatchResults
!
!!=============================================================================
!
subroutine output_saveNodeResults(aReportTime, file)
!
!  Input:   aReportTime = elapsed simulation time (millisec)
!           file = ptr. to binary output file
!  Output:  none
!  Purpose: writes computed node results to binary file.
!
    use headers
    use swmm5futil
    implicit none
    double precision, intent(In) :: aReportTime
    integer(kind=k4), intent(in) :: file

!    extern TRoutingTotals StepFlowTotals  ! defined in massbal.c
    integer :: j, lStat

    ! --- find where current reporting time lies between latest routing times
    double precision :: f, datatime
    f = (aReportTime - OldRoutingTime) / (NewRoutingTime - OldRoutingTime)
    datatime = getDateTime(StartDateTime, aReportTime)
    do j=1, Nobjects(E_NODE)
!       if (Node(j)%datatype == E_OUTFALL) then
!           exit !assume there is only one outlet
!       end if
       call node_getResults(j, f) !, NodeResults)
       if (OutputCount > SIZE(TSDateTime, 1)) then
!          lStat = ReDim(TSOutletVals)
!          if (lStat /= 0) exit
          lStat = ReDim(onodes(j)%oflow)
          lStat = ReDim(onodes(j)%odepth)
          lStat = ReDim(onodes(j)%ovolume)
          lStat = ReDim(TSDateTime)
          if (lStat /= 0) exit
       end if
       
       !TSOutletVals(OutputCount) = NodeResults(NODE_INFLOW)
       TSDateTime(OutputCount) = datatime
       onodes(j)%oflow(OutputCount) = NodeResults(NODE_INFLOW)
       onodes(j)%odepth(OutputCount) = NodeResults(NODE_DEPTH)
       onodes(j)%ovolume(OutputCount) = NodeResults(NODE_VOLUME)
    end do
!
!    ! --- write node results to file
!    for (j=0 j<Nobjects(NODE) j++)
!    {
!        ! --- retrieve interpolated results for reporting time & write to file
!        node_getResults(j, f, NodeResults)
!        if ( Node(j).rptFlag )                                                 !(5.0.014 - LR)
!            fwrite(NodeResults, sizeof(REAL4), NnodeResults, file)            !(5.0.014 - LR)
!
!        ! --- update system-wide storage volume                               !(5.0.012 - LR)
!        !SysResults(SYS_FLOODING) += NodeResults(NODE_OVERFLOW)              !(5.0.012 - LR)
!        SysResults(SYS_STORAGE) += NodeResults(NODE_VOLUME)
!        !if ( Node(j).degree == 0 )                                           !(5.0.012 - LR)
!        !{                                                                    !(5.0.012 - LR)
!        !    SysResults(SYS_OUTFLOW) += NodeResults(NODE_INFLOW)             !(5.0.012 - LR)
!        !}                                                                    !(5.0.012 - LR)
!    }
!
!    ! --- update system-wide flows                                            !(5.0.012 - LR)
!    SysResults(SYS_FLOODING) = (REAL4) (StepFlowTotals.flooding * UCF(FLOW))  !(5.0.012 - LR)
!    SysResults(SYS_OUTFLOW)  = (REAL4) (StepFlowTotals.outflow * UCF(FLOW))   !(5.0.012 - LR)
!    SysResults(SYS_DWFLOW) = (REAL4)(StepFlowTotals.dwInflow * UCF(FLOW))
!    SysResults(SYS_GWFLOW) = (REAL4)(StepFlowTotals.gwInflow * UCF(FLOW))
!    SysResults(SYS_IIFLOW) = (REAL4)(StepFlowTotals.iiInflow * UCF(FLOW))
!    SysResults(SYS_EXFLOW) = (REAL4)(StepFlowTotals.exInflow * UCF(FLOW))
!    SysResults(SYS_INFLOW) = SysResults(SYS_RUNOFF) +
!                             SysResults(SYS_DWFLOW) +
!                             SysResults(SYS_GWFLOW) +
!                             SysResults(SYS_IIFLOW) +
!                             SysResults(SYS_EXFLOW)
end subroutine output_saveNodeResults
!
!!=============================================================================
!
subroutine output_saveLinkResults(aReportTime, file)
!
!  Input:   aReportTime = elapsed simulation time (millisec)
!           file = ptr. to binary output file
!  Output:  none
!  Purpose: writes computed link results to binary file.
!
    use headers
    use swmm5futil
    use modLink
    implicit none
    double precision, intent(in) :: aReportTime
    integer(kind=K4), intent(in) :: file

    integer :: j, lStat
    double precision :: f
    double precision :: z, datatime

    ! --- find where current reporting time lies between latest routing times
    f = (aReportTime - OldRoutingTime) / (NewRoutingTime - OldRoutingTime)
    datatime = getDateTime(StartDateTime, aReportTime)
    ! --- write link results to file
    do j=1, Nobjects(LINK)
        ! --- retrieve interpolated results for reporting time & write to file
        call link_getResults(j, f, LinkResults)
!        if ( arrLink(j)%rptFlag ) &                                      !(5.0.014 - LR)
!           &fwrite(LinkResults, sizeof(REAL4), NlinkResults, file)       !(5.0.014 - LR)
        if (OutputCount > SIZE(TSDateTime, 1)) then
!          lStat = ReDim(TSOutletVals)
!          if (lStat /= 0) exit
          lStat = ReDim(olinks(j)%oflow)
          lStat = ReDim(olinks(j)%odepth)
          lStat = ReDim(olinks(j)%ovolume)
          lStat = ReDim(TSDateTime)
          if (lStat /= 0) exit
       end if
       
       !TSOutletVals(OutputCount) = NodeResults(NODE_INFLOW)
       TSDateTime(OutputCount) = datatime
       olinks(j)%oflow(OutputCount) = LinkResults(LINK_FLOW)
       olinks(j)%odepth(OutputCount) = LinkResults(LINK_DEPTH)
       olinks(j)%ovolume(OutputCount) = arrLink(j)%newVolume

        ! --- update system-wide results
        z = ((1.0-f)*arrLink(j)%oldVolume + f*arrLink(j)%newVolume) * UCF(VOLUME)
        SysResults(SYS_STORAGE) = SysResults(SYS_STORAGE) + z !(REAL4)
    end do
end subroutine output_saveLinkResults
!
!!=============================================================================
!
subroutine output_readDateTime(period, days)
!
!  Input:   period = index of reporting time period
!  Output:  days = date/time value, DateTime*
!  Purpose: retrieves the date/time for a specific reporting period
!           from the binary output file.
!
    implicit none
    integer, intent(in) :: period
    double precision, intent(inout) :: days
    
!    INT4 bytePos = OutputStartPos + (period-1)*BytesPerPeriod
!    fseek(Fout.file, bytePos, SEEK_SET)
!    *days = NO_DATE
!    fread(days, sizeof(REAL8), 1, Fout.file)
end subroutine output_readDateTime
!
!!=============================================================================
!
subroutine output_readSubcatchResults(period, index)
!
!  Input:   period = index of reporting time period
!           index = subcatchment index
!  Output:  none
!  Purpose: reads computed results for a subcatchment at a specific time
!           period.
!
    implicit none
    integer, intent(in) :: period, index
    
!    INT4 bytePos = OutputStartPos + (period-1)*BytesPerPeriod
!    bytePos += sizeof(REAL8) + index*NsubcatchResults*sizeof(REAL4)
!    fseek(Fout.file, bytePos, SEEK_SET)
!    fread(SubcatchResults, sizeof(REAL4), NsubcatchResults, Fout.file)
end subroutine output_readSubcatchResults
!
!!=============================================================================
!
subroutine output_readNodeResults(period, index)
!
!  Input:   period = index of reporting time period
!           index = node index
!  Output:  none
!  Purpose: reads computed results for a node at a specific time period.
!
    implicit none
    integer, intent(in) :: period, index
    
!    INT4 bytePos = OutputStartPos + (period-1)*BytesPerPeriod
!    bytePos += sizeof(REAL8) + NumSubcatch*NsubcatchResults*sizeof(REAL4)     !(5.0.014 - LR)
!    bytePos += index*NnodeResults*sizeof(REAL4)
!    fseek(Fout.file, bytePos, SEEK_SET)
!    fread(NodeResults, sizeof(REAL4), NnodeResults, Fout.file)
end subroutine output_readNodeResults
!
!!=============================================================================
!
subroutine output_readLinkResults(period, index)
!
!  Input:   period = index of reporting time period
!           index = link index
!  Output:  none
!  Purpose: reads computed results for a link at a specific time period.
!
    implicit none
    integer, intent(in) :: period, index
    
!    INT4 bytePos = OutputStartPos + (period-1)*BytesPerPeriod
!    bytePos += sizeof(REAL8) + NumSubcatch*NsubcatchResults*sizeof(REAL4)     !(5.0.014 - LR)
!    bytePos += NumNodes*NnodeResults*sizeof(REAL4)                            !(5.0.014 - LR)
!    bytePos += index*NlinkResults*sizeof(REAL4)
!    fseek(Fout.file, bytePos, SEEK_SET)
!    fread(LinkResults, sizeof(REAL4), NlinkResults, Fout.file)
!    fread(SysResults, sizeof(REAL4), MAX_SYS_RESULTS, Fout.file)
end subroutine output_readLinkResults

!=============================================================================
    
end module