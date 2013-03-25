!module statsrpt
!-----------------------------------------------------------------------------
!   statsrpt.c
!
!   Project:  EPA SWMM5
!   Version:  5.0
!   Date:     2/4/08   (Build 5.0.012)
!             1/21/09  (Build 5.0.014)
!             6/22/09  (Build 5.0.016)
!             10/7/09  (Build 5.0.017)
!             11/18/09 (Build 5.0.018)
!             07/30/10 (Build 5.0.019)
!             04/20/11 (Build 5.0.022)
!   Author:   L. Rossman
!
!   Report writing functions for summary statistics.
!-----------------------------------------------------------------------------
!#define _CRT_SECURE_NO_DEPRECATE
!
!#include <malloc.h>
!#include <string.h>
!#include <math.h>
!#include <time.h>
!#include "headers.h"
!#include "lid.h"                                                               !(5.0.019 - LR)
!use headers
!use swmm5futil
!use modStats
!!-----------------------------------------------------------------------------
!!  Imported variables
!!-----------------------------------------------------------------------------
!extern TSubcatchStats* SubcatchStats          ! defined in STATS.C
!extern TNodeStats*     NodeStats
!extern TLinkStats*     LinkStats
!extern TStorageStats*  StorageStats
!extern TOutfallStats*  OutfallStats
!extern TPumpStats*     PumpStats
!extern double          MaxOutfallFlow
!extern double          MaxRunoffFlow
!extern double*         NodeInflow             ! defined in MASSBAL.C
!
!!-----------------------------------------------------------------------------
!!  Local functions
!!-----------------------------------------------------------------------------
!void    writeSubcatchRunoff(void)
!void    writeSubcatchLoads(void)
!void    writeNodeDepths(void)
!void    writeNodeFlows(void)
!void    writeNodeSurcharge(void)
!void    writeNodeFlooding(void)
!void    writeStorageVolumes(void)
!void    writeOutfallLoads(void)
!void    writeLinkFlows(void)
!void    writeFlowClass(void)
!void    writeLinkSurcharge(void)
!void    writePumpFlows(void)
!
!#define WRITE(x) (report_writeLine((x))) !call report_writeLine directly
!character(6) :: FlowFmt !moved to swmm5futil
!double precision :: Vcf !moved to swmm5futil
!save
!
!contains
!/=============================================================================
!
subroutine WriteToRpt(x) 
   use report
   implicit none
   character(*), intent(in) :: x
   call report_writeLine(x)
end subroutine WRITEToRpt

subroutine statsrpt_writeReport()
!
!  Input:   none
!  Output:  none
!  Purpose: reports simulation summary statistics.
!
    use headers
    use swmm5futil
    use modStats
    implicit none

    ! --- set number of decimal places for reporting flow values
    if ( FlowUnits == MGD .or. FlowUnits == CMS ) then
        FlowFmt = '(f9.3)'
    else 
        FlowFmt = '(f9.2)'
    end if

    ! --- volume conversion factor from ft3 to Mgal or Mliters
    if (UnitSystem == US) then
       Vcf = 7.48 / 1.0e6
    else
       Vcf = 28.317 / 1.0e6
    end if


!!  Following code segment was modified for release 5.0.018.  !!           !5.0.018 - LR)
    ! --- report summary results for subcatchment runoff 
    if ( Nobjects(E_SUBCATCH) > 0 ) then
        if ( .not. IgnoreRainfall .or. &
            &(Nobjects(E_SNOWMELT) > 0 .and. .not. IgnoreSnowmelt) .or. &
            &(Nobjects(E_AQUIFER) > 0  .and. .not. IgnoreGwater) ) then
            call writeSubcatchRunoff()
            !call lid_writeWaterBalance()                                           !(5.0.019 - LR)
            if ( Nobjects(E_POLLUT) > 0 .and. .not. IgnoreQuality) call writeSubcatchLoads() !(5.0.018 - LR)
        end if
    end if
!!  End of modified code segment.  !!

    ! --- report summary results for flow routing
    if ( Nobjects(LINK) > 0 .and. .not. IgnoreRouting ) then                            !(5.0.018 - LR)
        call writeNodeDepths()
        call writeNodeFlows()
        call writeNodeSurcharge()
        call writeNodeFlooding()
        call writeStorageVolumes()
        call writeOutfallLoads()
        call writeLinkFlows()
        call writeFlowClass()
        call writeLinkSurcharge()
        call writePumpFlows()
    end if
end subroutine statsrpt_writeReport
!
!/=============================================================================
!
!!!  This function was heavily modified for release 5.0.019.  !!            !(5.0.019 - LR)
!
!void writeSubcatchRunoff()
!{
!    int    j
!    double a, x, r
!
!    if ( Nobjects(SUBCATCH) == 0 ) return
!    WriteToRpt("")
!    WriteToRpt("***************************")
!    WriteToRpt("Subcatchment Runoff Summary")
!    WriteToRpt("***************************")
!    WriteToRpt("")
!    write(Frpt%fileHandle,
!
!"\n  --------------------------------------------------------------------------------------------------------"
!"\n                            Total      Total      Total      Total      Total       Total     Peak  Runoff"
!"\n                           Precip      Runon       Evap      Infil     Runoff      Runoff   Runoff   Coeff")
!    if ( UnitSystem == US ) write(Frpt%fileHandle,
!"\n  Subcatchment                 in         in         in         in         in    %8s      %3s",
!        VolUnitsWords(UnitSystem), FlowUnitWords(FlowUnits))
!    else write(Frpt%fileHandle,
!"\n  Subcatchment                 mm         mm         mm         mm         mm    %8s      %3s",
!        VolUnitsWords(UnitSystem), FlowUnitWords(FlowUnits))
!    write(Frpt%fileHandle,
!"\n  --------------------------------------------------------------------------------------------------------")
!
!    for ( j = 0 j < Nobjects(SUBCATCH) j++ )
!    {
!        a = Subcatch(j).area
!        if ( a == 0.0 ) continue
!        write(Frpt%fileHandle, "\n  %-20s", Subcatch(j).ID)
!        x = SubcatchStats(j).precip * UCF(RAINDEPTH)
!        write(Frpt%fileHandle, " %10.2f", x/a)
!        x = SubcatchStats(j).runon * UCF(RAINDEPTH) 
!        write(Frpt%fileHandle, " %10.2f", x/a)
!        x = SubcatchStats(j).evap * UCF(RAINDEPTH)
!        write(Frpt%fileHandle, " %10.2f", x/a)
!        x = SubcatchStats(j).infil * UCF(RAINDEPTH) 
!        write(Frpt%fileHandle, " %10.2f", x/a)
!        x = SubcatchStats(j).runoff * UCF(RAINDEPTH)
!        write(Frpt%fileHandle, " %10.2f", x/a)
!        x = SubcatchStats(j).runoff * Vcf
!		write(Frpt%fileHandle, "%12.2f", x)
!        x = SubcatchStats(j).maxFlow * UCF(FLOW)
!        write(Frpt%fileHandle, " %8.2f", x)
!        r = SubcatchStats(j).precip + SubcatchStats(j).runon
!        if ( r > 0.0 ) r = SubcatchStats(j).runoff / r
!        write(Frpt%fileHandle, "%8.3f", r)
!    }
!    WriteToRpt("")
!}
!
!/=============================================================================
!
!void writeSubcatchLoads()
!{
!    int i, j, p
!    double x
!    double* totals 
!    char  units(15)
!    char  subcatchLine() = "--------------------"
!    char  pollutLine()   = "--------------"
!
!    ! --- create an array to hold total loads for each pollutant
!    totals = (double *) calloc(Nobjects(POLLUT), sizeof(double))
!    if ( totals )
!    {
!        ! --- print the table headings 
!        WriteToRpt("")
!        WriteToRpt("****************************")
!        WriteToRpt("Subcatchment Washoff Summary")
!        WriteToRpt("****************************")
!        WriteToRpt("")
!        write(Frpt%fileHandle, "\n  %s", subcatchLine)
!        for (p = 0 p < Nobjects(POLLUT) p++) write(Frpt%fileHandle, "%s", pollutLine)
!        write(Frpt%fileHandle, "\n                      ")
!        for (p = 0 p < Nobjects(POLLUT) p++) write(Frpt%fileHandle, "%14s", Pollut(p).ID)
!        write(Frpt%fileHandle, "\n  Subcatchment        ")
!        for (p = 0 p < Nobjects(POLLUT) p++)
!        {
!            i = UnitSystem
!            if ( Pollut(p).units == COUNT ) i = 2
!            strcpy(units, LoadUnitsWords(i))
!            write(Frpt%fileHandle, "%14s", units)
!            totals(p) = 0.0
!        }
!        write(Frpt%fileHandle, "\n  %s", subcatchLine)
!        for (p = 0 p < Nobjects(POLLUT) p++) write(Frpt%fileHandle, "%s", pollutLine)
!
!        ! --- print the pollutant loadings from each subcatchment
!        for ( j = 0 j < Nobjects(SUBCATCH) j++ )
!        {
!            write(Frpt%fileHandle, "\n  %-20s", Subcatch(j).ID)
!            for (p = 0 p < Nobjects(POLLUT) p++)
!            {
!                x = Subcatch(j).totalLoad(p)
!                totals(p) += x
!                if ( Pollut(p).units == COUNT ) x = LOG10(x)
!				write(Frpt%fileHandle, "%14.3f", x) 
!            }
!        }
!
!        ! --- print the total loading of each pollutant
!        write(Frpt%fileHandle, "\n  %s", subcatchLine)
!        for (p = 0 p < Nobjects(POLLUT) p++) write(Frpt%fileHandle, "%s", pollutLine)
!        write(Frpt%fileHandle, "\n  System              ")
!        for (p = 0 p < Nobjects(POLLUT) p++)
!        {
!            x = totals(p)
!            if ( Pollut(p).units == COUNT ) x = LOG10(x)
!			write(Frpt%fileHandle, "%14.3f", x) 
!        }
!        free(totals)
!        WriteToRpt("")
!    }
!}
!
!/=============================================================================
!
!void writeNodeDepths()
!!
!!  Input:   none
!!  Output:  none
!!  Purpose: writes simulation statistics for nodes to report file.
!!
!{
!    int j, days, hrs, mins
!
!    if ( Nobjects(LINK) == 0 ) return
!    WriteToRpt("")
!    WriteToRpt("******************")
!    WriteToRpt("Node Depth Summary")
!    WriteToRpt("******************")
!    WriteToRpt("")
!
!    write(Frpt%fileHandle,
!"\n  ---------------------------------------------------------------------"
!"\n                                 Average  Maximum  Maximum  Time of Max"
!"\n                                   Depth    Depth      HGL   Occurrence")
!    if ( UnitSystem == US ) write(Frpt%fileHandle,
!"\n  Node                 Type         Feet     Feet     Feet  days hr:min")
!    else write(Frpt%fileHandle,
!"\n  Node                 Type       Meters   Meters   Meters  days hr:min")
!    write(Frpt%fileHandle,
!"\n  ---------------------------------------------------------------------")
!
!    for ( j = 0 j < Nobjects(NODE) j++ )
!    {
!        write(Frpt%fileHandle, "\n  %-20s", Node(j).ID)
!        write(Frpt%fileHandle, " %-9s ", NodeTypeWords(Node(j)%datatype))
!        getElapsedTime(NodeStats(j).maxDepthDate, &days, &hrs, &mins)
!        write(Frpt%fileHandle, "%7.2f  %7.2f  %7.2f  %4d  %02d:%02d",
!            NodeStats(j).avgDepth / StepCount * UCF(LENGTH),
!            NodeStats(j).maxDepth * UCF(LENGTH),
!            (NodeStats(j).maxDepth + Node(j).invertElev) * UCF(LENGTH),
!            days, hrs, mins)
!    }
!    WriteToRpt("")
!}
!
!/=============================================================================

subroutine writeNodeFlows()
!
!  Input:   none
!  Output:  none
!  Purpose: writes flow statistics for nodes to report file.
!
    use headers
    use modStats
    use swmm5futil
    implicit none
    integer :: j
    integer :: days1, hrs1, mins1

    write(Frpt%fileHandle, *) ''
    write(Frpt%fileHandle, *) '*******************'
    write(Frpt%fileHandle, *) 'Node Inflow Summary'        !(5.0.017 - LR)
    write(Frpt%fileHandle, *) '*******************'
    write(Frpt%fileHandle, *) ''

    !(5.0.014 - LR) format print out
    write(Frpt%fileHandle, *) ''
write(Frpt%fileHandle, *) "  -------------------------------------------------------------------------------------"
write(Frpt%fileHandle, *) "                                  Maximum  Maximum                  Lateral       Total"
write(Frpt%fileHandle, *) "                                  Lateral    Total  Time of Max      Inflow      Inflow"
write(Frpt%fileHandle, *, advance='no') "                                   Inflow   Inflow   Occurrence      Volume      Volume"
write(Frpt%fileHandle, *) "  Node                 Type           ", FlowUnitWords(FlowUnits),'      ', FlowUnitWords(FlowUnits), &
   &'  days hr:min    ', VolUnitsWords(UnitSystem), '     ', VolUnitsWords(UnitSystem)
write(Frpt%fileHandle, *) "  -------------------------------------------------------------------------------------"

    do j =1, Nobjects(E_NODE)
        write(Frpt%fileHandle, '(/,2x,(A20))', advance='no') Node(j)%ID
        write(Frpt%fileHandle, '(A20)', advance='no') NodeTypeWords(Node(j)%datatype)
        call getElapsedTime(NodeStats(j)%maxInflowDate, days1, hrs1, mins1)
        write(Frpt%fileHandle, FlowFmt, advance='no') NodeStats(j)%maxLatFlow * UCF(FLOW)
        write(Frpt%fileHandle, FlowFmt, advance='no') NodeStats(j)%maxInflow * UCF(FLOW)
        write(Frpt%fileHandle, '(2x,i4,2x,i2,(A1),i2)', advance='no') days1, hrs1, ':', mins1
		write(Frpt%fileHandle, '(f12.3)', advance='no') NodeStats(j)%totLatFlow * Vcf           !(5.0.014 - LR)
		write(Frpt%fileHandle, '(f12.3)', advance='no') NodeInflow(j) * Vcf                     !(5.0.014 - LR)
    end do
    call WriteToRpt("")
end subroutine writeNodeFlows
!
!/=============================================================================
!
!void writeNodeSurcharge()
!{
!    int    j, n = 0
!    double t, d1, d2
!
!    WriteToRpt("")
!    WriteToRpt("**********************")
!    WriteToRpt("Node Surcharge Summary")
!    WriteToRpt("**********************")
!    WriteToRpt("")
!
!    for ( j = 0 j < Nobjects(NODE) j++ )
!    {
!        if ( Node(j)%datatype == OUTFALL ) continue
!        if ( NodeStats(j).timeSurcharged == 0.0 ) continue
!        t = MAX(0.01, (NodeStats(j).timeSurcharged / 3600.0))
!        if ( n == 0 )
!        {
!            WriteToRpt("Surcharging occurs when water rises above the top of the highest conduit.") 
!            write(Frpt%fileHandle, 
!"\n  ---------------------------------------------------------------------"
!"\n                                               Max. Height   Min. Depth"
!"\n                                   Hours       Above Crown    Below Rim")
!    if ( UnitSystem == US ) write(Frpt%fileHandle,
!"\n  Node                 Type      Surcharged           Feet         Feet")
!    else write(Frpt%fileHandle,
!"\n  Node                 Type      Surcharged         Meters       Meters")
!    write(Frpt%fileHandle,
!"\n  ---------------------------------------------------------------------")
!            n = 1
!        }
!        write(Frpt%fileHandle, "\n  %-20s", Node(j).ID)
!        write(Frpt%fileHandle, " %-9s", NodeTypeWords(Node(j)%datatype))
!        d1 = NodeStats(j).maxDepth + Node(j).invertElev - Node(j).crownElev
!        if ( d1 < 0.0 ) d1 = 0.0
!        d2 = Node(j).fullDepth - NodeStats(j).maxDepth                        !(5.0.017a - LR)
!        if ( d2 < 0.0 ) d2 = 0.0
!        write(Frpt%fileHandle, "  %9.2f      %9.3f    %9.3f",
!                t, d1*UCF(LENGTH), d2*UCF(LENGTH))
!    }
!    if ( n == 0 ) WriteToRpt("No nodes were surcharged.")
!    WriteToRpt("")
!}
!
!/=============================================================================
!
!void writeNodeFlooding()
!{
!    int    j, n = 0
!    int    days, hrs, mins
!    double t
!
!    WriteToRpt("")
!    WriteToRpt("*********************")
!    WriteToRpt("Node Flooding Summary")
!    WriteToRpt("*********************")
!    WriteToRpt("")
!
!    for ( j = 0 j < Nobjects(NODE) j++ )
!    {
!        if ( Node(j)%datatype == OUTFALL ) continue
!        if ( NodeStats(j).timeFlooded == 0.0 ) continue
!        t = MAX(0.01, (NodeStats(j).timeFlooded / 3600.0))
!
!!!  Following code segment was modified for release 5.0.019.  !!           !(5.0.019 - LR)
!        if ( n == 0 )
!        {
!            WriteToRpt("Flooding refers to all water that overflows a node, whether it ponds or not.")
!            write(Frpt%fileHandle, 
!"\n  --------------------------------------------------------------------------"
!"\n                                                             Total   Maximum"
!"\n                                 Maximum   Time of Max       Flood    Ponded"
!"\n                        Hours       Rate    Occurrence      Volume")
!            if ( RouteModel == DW ) write(Frpt%fileHandle, "     Depth")
!            else                    write(Frpt%fileHandle, "    Volume")
!            write(Frpt%fileHandle, 
!"\n  Node                 Flooded       %3s   days hr:min    %8s",
!                FlowUnitWords(FlowUnits), VolUnitsWords(UnitSystem))
!            if ( RouteModel == DW )      write(Frpt%fileHandle, "    %6s",
!                                         PondingUnitsWords(UnitSystem))
!            else if ( UnitSystem == US ) write(Frpt%fileHandle, "  1000 ft3")
!            else                         write(Frpt%fileHandle, "   1000 m3")
!            write(Frpt%fileHandle,
!"\n  --------------------------------------------------------------------------")
!            n = 1
!        }
!        write(Frpt%fileHandle, "\n  %-20s", Node(j).ID)
!        write(Frpt%fileHandle, " %7.2f ", t)
!        write(Frpt%fileHandle, FlowFmt, NodeStats(j).maxOverflow * UCF(FLOW))
!        getElapsedTime(NodeStats(j).maxOverflowDate, &days, &hrs, &mins)
!        write(Frpt%fileHandle, "   %4d  %02d:%02d", days, hrs, mins)
!		write(Frpt%fileHandle, "%12.3f", NodeStats(j).volFlooded * Vcf)
!        if ( RouteModel == DW )
!            write(Frpt%fileHandle, " %9.2f", NodeStats(j).maxDepth * UCF(LENGTH)) !(5.0.019 - LR)
!        else
!            write(Frpt%fileHandle, " %9.3f", NodeStats(j).maxPondedVol /
!                                         1000.0 * UCF(VOLUME))
!    }
!!!  End of modified code segment.  !!                                      !(5.0.019 - LR)
!
!    if ( n == 0 ) WriteToRpt("No nodes were flooded.")
!    WriteToRpt("")
!}
!
!/=============================================================================
!
!void writeStorageVolumes()
!!
!!  Input:   none
!!  Output:  none
!!  Purpose: writes simulation statistics for storage units to report file.
!!
!{
!    int    j, k, days, hrs, mins
!    double avgVol, maxVol, pctAvgVol, pctMaxVol, pctLoss                      !(5.0.018-LR)
!
!    if ( Nnodes(STORAGE) > 0 )
!    {
!        WriteToRpt("")
!        WriteToRpt("**********************")
!        WriteToRpt("Storage Volume Summary")
!        WriteToRpt("**********************")
!        WriteToRpt("")
!
!!!  Lines below were updated for release 5.0.018 !!                        !(5.0.018-LR)
!        write(Frpt%fileHandle,
!"\n  --------------------------------------------------------------------------------------------"
!"\n                         Average     Avg   E&I       Maximum     Max    Time of Max    Maximum"
!"\n                          Volume    Pcnt  Pcnt        Volume    Pcnt     Occurrence    Outflow")
!        if ( UnitSystem == US ) write(Frpt%fileHandle,
!"\n  Storage Unit          1000 ft3    Full  Loss      1000 ft3    Full    days hr:min        ")
!        else write(Frpt%fileHandle,
!"\n  Storage Unit           1000 m3    Full  Loss       1000 m3    Full    days hr:min        ")
!        write(Frpt%fileHandle, "%3s", FlowUnitWords(FlowUnits))
!        write(Frpt%fileHandle,
!"\n  --------------------------------------------------------------------------------------------")
!!!  End of updated lines.  !!
!
!        for ( j = 0 j < Nobjects(NODE) j++ )
!        {
!            if ( Node(j)%datatype /= STORAGE ) continue
!            k = Node(j).subIndex
!            write(Frpt%fileHandle, "\n  %-20s", Node(j).ID)
!            avgVol = StorageStats(k).avgVol / StepCount
!            maxVol = StorageStats(k).maxVol
!            pctMaxVol = 0.0
!            pctAvgVol = 0.0
!            if ( Node(j).fullVolume > 0.0 )
!            {
!                pctAvgVol = avgVol / Node(j).fullVolume * 100.0
!                pctMaxVol = maxVol / Node(j).fullVolume * 100.0
!            }
!
!!!  Lines below were updated for release 5.0.018  !!                       !(5.0.018-LR)
!            pctLoss = 0.0
!            if ( NodeInflow(j) > 0.0 )
!                pctLoss = StorageStats(k).losses / NodeInflow(j) * 100.0
!            write(Frpt%fileHandle, "%10.3f    %4.0f  %4.0f    %10.3f    %4.0f",
!                avgVol*UCF(VOLUME)/1000.0, pctAvgVol, pctLoss,
!                maxVol*UCF(VOLUME)/1000.0, pctMaxVol)
!!!  End of updated lines  !!
!
!            getElapsedTime(StorageStats(k).maxVolDate, &days, &hrs, &mins)
!            write(Frpt%fileHandle, "    %4d  %02d:%02d  ", days, hrs, mins)
!            write(Frpt%fileHandle, FlowFmt, StorageStats(k).maxFlow*UCF(FLOW))
!        }
!        WriteToRpt("")
!    }
!}
!
!/=============================================================================
!
!void writeOutfallLoads()
!!
!!  Input:   node
!!  Output:  none
!!  Purpose: writes simulation statistics for outfall nodess to report file.
!!
!{
!    char    units(15)
!    int     i, j, k, p
!    double  x
!    double  outfallCount, flowCount
!    double  flowSum, freqSum, volSum
!    double* totals
!
!    if ( Nnodes(OUTFALL) > 0 )
!    {
!        ! --- initial totals
!        totals = (double *) calloc(Nobjects(POLLUT), sizeof(double))
!        for (p=0 p<Nobjects(POLLUT) p++) totals(p) = 0.0
!        flowSum = 0.0
!        freqSum = 0.0
!		volSum  = 0.0
!
!        ! --- print table title
!        WriteToRpt("")
!        WriteToRpt("***********************")
!        WriteToRpt("Outfall Loading Summary")
!        WriteToRpt("***********************")
!        WriteToRpt("")
!
!        ! --- print table column headers
!        write(Frpt%fileHandle,
! "\n  -----------------------------------------------------------")                !(5.0.014 - LR)
!        for (p = 0 p < Nobjects(POLLUT) p++) write(Frpt%fileHandle, "--------------")
!        write(Frpt%fileHandle,
! "\n                        Flow       Avg.      Max.       Total")                !(5.0.014 - LR)
!        for (p=0 p<Nobjects(POLLUT) p++) write(Frpt%fileHandle,"         Total")     !(5.0.016 - LR)
!        write(Frpt%fileHandle,
! "\n                        Freq.      Flow      Flow      Volume")                !(5.0.014 - LR)
!        for (p = 0 p < Nobjects(POLLUT) p++) write(Frpt%fileHandle, "%14s", Pollut(p).ID)
!        write(Frpt%fileHandle,
! "\n  Outfall Node          Pcnt.       %3s       %3s    %8s",                 !(5.0.014 - LR)
!            FlowUnitWords(FlowUnits), FlowUnitWords(FlowUnits),
!			VolUnitsWords(UnitSystem))
!        for (p = 0 p < Nobjects(POLLUT) p++)
!        {
!            i = UnitSystem
!            if ( Pollut(p).units == COUNT ) i = 2
!            strcpy(units, LoadUnitsWords(i))
!            write(Frpt%fileHandle, "%14s", units)
!        }
!        write(Frpt%fileHandle,
! "\n  -----------------------------------------------------------")                !(5.0.014 - LR)
!        for (p = 0 p < Nobjects(POLLUT) p++) write(Frpt%fileHandle, "--------------")
!
!        ! --- identify each outfall node
!        for (j=0 j<Nobjects(NODE) j++)
!        {
!            if ( Node(j)%datatype /= OUTFALL ) continue
!            k = Node(j).subIndex
!            flowCount = OutfallStats(k).totalPeriods
!
!            ! --- print node ID, flow freq., avg. flow, max. flow & flow vol.
!            write(Frpt%fileHandle, "\n  %-20s", Node(j).ID)
!            x = 100.*flowCount/(double)StepCount
!            write(Frpt%fileHandle, "%7.2f", x)
!            freqSum += x
!            if ( flowCount > 0 )
!                x = OutfallStats(k).avgFlow*UCF(FLOW)/flowCount
!            else
!                x = 0.0
!            flowSum += x
!
!            write(Frpt%fileHandle, " ")
!            write(Frpt%fileHandle, FlowFmt, x)
!            write(Frpt%fileHandle, " ")
!            write(Frpt%fileHandle, FlowFmt, OutfallStats(k).maxFlow*UCF(FLOW))
!			write(Frpt%fileHandle, "%12.3f", NodeInflow(j) * Vcf)                 !(5.0.014 - LR)
!			volSum += NodeInflow(j)
!
!            ! --- print load of each pollutant for outfall
!            for (p=0 p<Nobjects(POLLUT) p++)
!            {
!                x = OutfallStats(k).totalLoad(p)
!                totals(p) += x
!                if ( Pollut(p).units == COUNT ) x = LOG10(x)
!				write(Frpt%fileHandle, "%14.3f", x) 
!            }
!        }
!
!        ! --- print total outfall loads
!        outfallCount = Nnodes(OUTFALL)
!        write(Frpt%fileHandle,
! "\n  -----------------------------------------------------------")                !(5.0.014 - LR)
!        for (p = 0 p < Nobjects(POLLUT) p++) write(Frpt%fileHandle, "--------------")
!
!        write(Frpt%fileHandle, "\n  System              %7.2f ",
!            freqSum/outfallCount)
!        write(Frpt%fileHandle, FlowFmt, flowSum)
!        write(Frpt%fileHandle, " ")
!        write(Frpt%fileHandle, FlowFmt, MaxOutfallFlow*UCF(FLOW))
!		write(Frpt%fileHandle, "%12.3f", volSum * Vcf)                            !(5.0.014 - LR)
!
!        for (p = 0 p < Nobjects(POLLUT) p++)
!        {
!            x = totals(p)
!            if ( Pollut(p).units == COUNT ) x = LOG10(x)
!			write(Frpt%fileHandle, "%14.3f", x) 
!        }
!        WriteToRpt("")
!        free(totals)
!    } 
!}
!
!/=============================================================================

subroutine writeLinkFlows()
!
!  Input:   none
!  Output:  none
!  Purpose: writes simulation statistics for links to report file.
!
    use headers
    use swmm5futil
    use modStats
    implicit none
    
    integer :: j, k, days, hrs, mins
    double precision :: v, fullDepth

    if ( Nobjects(LINK) == 0 ) return
    call WriteToRpt("")
    call WriteToRpt("********************")
    call WriteToRpt("arrLink Flow Summary")
    call WriteToRpt("********************")
    call WriteToRpt("")

    write(Frpt%fileHandle, *) "  -----------------------------------------------------------------------------"
write(Frpt%fileHandle, *) "                                 Maximum  Time of Max   Maximum    Max/    Max/"
write(Frpt%fileHandle, *) "                                  |Flow|   Occurrence   |Veloc|    Full    Full"  !(5.0.019 - LR)
    if ( UnitSystem == US ) then
write(Frpt%fileHandle,*) "  arrLink                 Type          ", &
  &FlowUnitWords(FlowUnits),"  days hr:min    ft/sec    Flow   Depth"
    else 
write(Frpt%fileHandle,*) "  arrLink                 Type          ", &
  &FlowUnitWords(FlowUnits), "  days hr:min     m/sec    Flow   Depth"
    end if
    write(Frpt%fileHandle, *) "  -----------------------------------------------------------------------------"

    do j =1, Nobjects(LINK)
        ! --- print link ID
        k = arrLink(j)%subIndex
        write(Frpt%fileHandle, "(/,2x,A20)", advance='no') arrLink(j)%ID

        ! --- print link type
        if ( arrLink(j)%xsect%datatype == DUMMY ) then
           write(Frpt%fileHandle, *, advance='no') " DUMMY   "
        else if ( arrLink(j)%xsect%datatype == IRREGULAR ) then
           write(Frpt%fileHandle, *, advance='no') " CHANNEL "
        else 
           write(Frpt%fileHandle, "(1x,A7,1x)", advance='no') LinkTypeWords(arrLink(j)%datatype)
        end if

        ! --- print max. flow & time of occurrence
        call getElapsedTime(LinkStats(j)%maxFlowDate, days, hrs, mins)
        write(Frpt%fileHandle, FlowFmt, advance='no') LinkStats(j)%maxFlow*UCF(FLOW)
        write(Frpt%fileHandle, "(2x,i4,2x,i2,A1,i2)", advance='no') days, hrs, ':', mins

        ! --- print max flow / flow capacity for pumps
        if ( arrLink(j)%datatype == E_PUMP .and. arrLink(j)%qFull > 0.0) then
            write(Frpt%fileHandle, *, advance='no') "          "
            write(Frpt%fileHandle, "(2x,f6.2)", advance='no') LinkStats(j)%maxFlow / arrLink(j)%qFull
            cycle
        end if

        ! --- stop printing for dummy conduits
        if ( arrLink(j)%xsect%datatype == DUMMY ) cycle

        ! --- stop printing for outlet links (since they don't have xsections)
        if ( arrLink(j)%datatype == E_OUTLET ) cycle

        ! --- print max velocity & max/full flow for conduits
        if ( arrLink(j)%datatype == E_CONDUIT ) then
            v = LinkStats(j)%maxVeloc*UCF(LENGTH)
            if ( v > 50.0 ) then
               write(Frpt%fileHandle, *, advance='no') "    >50.00"
            else 
               write(Frpt%fileHandle, '(3x,f7.2)', advance='no') v
            end if
            write(Frpt%fileHandle, "(2x,f6.2)", advance='no') &
                  LinkStats(j)%maxFlow / arrLink(j)%qFull / (Conduit(k)%barrels * 1.0d00)
        else 
           write(Frpt%fileHandle, *, advance='no') "                  "
        end if

        ! --- print max/full depth
        fullDepth = arrLink(j)%xsect%yFull
        if ( arrLink(j)%datatype == E_ORIFICE .and. &
            &Orifice(k)%datatype == BOTTOM_ORIFICE ) fullDepth = 0.0
        if ( fullDepth > 0.0 ) then
            write(Frpt%fileHandle, "(2x,f6.2)",advance='no') LinkStats(j)%maxDepth / fullDepth
        else 
            write(Frpt%fileHandle, *,advance='no') "        "
        end if
    end do
    call WriteToRpt("")
end subroutine writeLinkFlows
!
!/=============================================================================
!
!void writeFlowClass()
!!
!!  Input:   none
!!  Output:  none
!!  Purpose: writes flow classification fro each conduit to report file.
!!
!{
!    int   i, j, k
!
!    if ( RouteModel /= DW ) return
!
!    WriteToRpt("")
!    WriteToRpt("***************************")
!    WriteToRpt("Flow Classification Summary")
!    WriteToRpt("***************************")
!    WriteToRpt("")
!    write(Frpt%fileHandle,
!"\n  -----------------------------------------------------------------------------------------"
!"\n                      Adjusted    --- Fraction of Time in Flow Class ----   Avg.     Avg.  "
!"\n                       /Actual         Up    Down  Sub   Sup   Up    Down   Froude   Flow  "
!"\n  Conduit               Length    Dry  Dry   Dry   Crit  Crit  Crit  Crit   Number   Change"
!"\n  -----------------------------------------------------------------------------------------")
!    for ( j = 0 j < Nobjects(LINK) j++ )
!    {
!        if ( arrLink(j)%datatype /= CONDUIT ) continue
!        if ( arrLink(j).xsect%datatype == DUMMY ) continue
!        k = arrLink(j).subIndex
!        write(Frpt%fileHandle, "\n  %-20s", arrLink(j).ID)
!        write(Frpt%fileHandle, "  %6.2f ", Conduit(k).modLength / Conduit(k).length)
!        for ( i=0 i<MAX_FLOW_CLASSES i++ )
!        {
!            write(Frpt%fileHandle, "  %4.2f",
!                LinkStats(j).timeInFlowClass(i) /= StepCount)
!        }
!        write(Frpt%fileHandle, "   %6.2f", LinkStats(j).avgFroude / StepCount)
!        write(Frpt%fileHandle, "   %6.4f", LinkStats(j).avgFlowChange /
!                                       arrLink(j).qFull / StepCount)
!    }
!    WriteToRpt("")
!}
!
!/=============================================================================
!
!void writeLinkSurcharge()
!{
!    int    i, j, n = 0
!    double t(5)
!
!    WriteToRpt("")
!    WriteToRpt("*************************")
!    WriteToRpt("Conduit Surcharge Summary")
!    WriteToRpt("*************************")
!    WriteToRpt("")
!    for ( j = 0 j < Nobjects(LINK) j++ )
!    {
!        if ( arrLink(j)%datatype /= CONDUIT ) continue
!        t(0) = LinkStats(j).timeSurcharged / 3600.0
!        t(1) = LinkStats(j).timeFullUpstream / 3600.0
!        t(2) = LinkStats(j).timeFullDnstream / 3600.0
!        t(3) = LinkStats(j).timeFullFlow / 3600.0
!        if ( t(0) + t(1) + t(2) + t(3) == 0.0 ) continue
!        t(4) = LinkStats(j).timeCapacityLimited / 3600.0
!        for (i=0 i<5 i++) t(i) = MAX(0.01, t(i))
!        if (n == 0)
!        {
!            write(Frpt%fileHandle, 
!"\n  ----------------------------------------------------------------------------"
!"\n                                                           Hours        Hours "
!"\n                         --------- Hours Full --------   Above Full   Capacity"
!"\n  Conduit                Both Ends  Upstream  Dnstream   Normal Flow   Limited"
!"\n  ----------------------------------------------------------------------------")
!            n = 1
!        }
!        write(Frpt%fileHandle, "\n  %-20s", arrLink(j).ID)
!        write(Frpt%fileHandle, "    %8.2f  %8.2f  %8.2f  %8.2f     %8.2f",
!                t(0), t(1), t(2), t(3), t(4))
!    }
!    if ( n == 0 ) WriteToRpt("No conduits were surcharged.")
!    WriteToRpt("")
!}
!
!/=============================================================================
!
!!!  This function was re-written for release 5.0.022  !!                   !(5.0.022 - LR)
!
!void writePumpFlows()
!!
!!  Input:   none
!!  Output:  none
!!  Purpose: writes simulation statistics for pumps to report file.
!!
!{
!    int    j, k
!    double avgFlow, pctUtilized, pctOffCurve1, pctOffCurve2, totalSeconds
!
!    if ( Nlinks(PUMP) == 0 ) return
!
!    WriteToRpt("")
!    WriteToRpt("***************")
!    WriteToRpt("Pumping Summary")
!    WriteToRpt("***************")
!    WriteToRpt("")
!
!    write(Frpt%fileHandle,
!"\n  ---------------------------------------------------------------------------------------------------------"
!"\n                                                  Min       Avg       Max     Total     Power    %% Time Off"
!"\n                        Percent   Number of      Flow      Flow      Flow    Volume     Usage    Pump Curve"
!"\n  Pump                 Utilized   Start-Ups       %3s       %3s       %3s  %8s     Kw-hr    Low   High"
!"\n  ---------------------------------------------------------------------------------------------------------",
!        FlowUnitWords(FlowUnits), FlowUnitWords(FlowUnits),
!        FlowUnitWords(FlowUnits), VolUnitsWords(UnitSystem))
!    for ( j = 0 j < Nobjects(LINK) j++ )
!    {
!        if ( arrLink(j)%datatype /= PUMP ) continue
!        k = arrLink(j).subIndex
!        write(Frpt%fileHandle, "\n  %-20s", arrLink(j).ID)
!        totalSeconds = NewRoutingTime / 1000.0
!        pctUtilized = PumpStats(k).utilized / totalSeconds * 100.0
!        avgFlow = PumpStats(k).avgFlow
!        if ( PumpStats(k).totalPeriods > 0 )
!            avgFlow /=  PumpStats(k).totalPeriods
!        write(Frpt%fileHandle, " %8.2f  %10d %9.2f %9.2f %9.2f %9.3f %9.2f",
!            pctUtilized, PumpStats(k).startUps, PumpStats(k).minFlow*UCF(FLOW),
!            avgFlow*UCF(FLOW), PumpStats(k).maxFlow*UCF(FLOW), 
!            PumpStats(k).volume*Vcf, PumpStats(k).energy)
!        pctOffCurve1 = PumpStats(k).offCurveLow
!        pctOffCurve2 = PumpStats(k).offCurveHigh
!        if ( PumpStats(k).utilized > 0.0 )
!        {
!            pctOffCurve1 = pctOffCurve1 / PumpStats(k).utilized * 100.0
!            pctOffCurve2 = pctOffCurve2 / PumpStats(k).utilized * 100.0
!        }
!        write(Frpt%fileHandle, " %6.1f %6.1f", pctOffCurve1, pctOffCurve2) 
!    }
!    WriteToRpt("")
!}
!
!end module