module modInflow
integer, parameter :: dp = kind(1.d0)
!!-----------------------------------------------------------------------------
!!   inflow.c
!!
!!   Project:  EPA SWMM5
!!   Version:  5.0
!!   Date:     6/19/07   (Build 5.0.010)
!!             1/21/09   (Build 5.0.014)
!!             07/30/10  (Build 5.0.019)
!!   Author:   L. Rossman
!!
!!   Manages any Direct External or Dry Weather Flow inflows
!!   that have been assigned to nodes of the drainage system.
!!-----------------------------------------------------------------------------
!#define _CRT_SECURE_NO_DEPRECATE
!
!#include <stdlib.h>
!#include <string.h>
!#include "headers.h"
!
!!-----------------------------------------------------------------------------
!!  External Functions (declared in funcs.h)
!!-----------------------------------------------------------------------------
!!  inflow_initDwfPattern   (called createObjects in project.c)
!!  inflow_readExtInflow    (called by input_readLine)
!!  inflow_readDwfInflow    (called by input_readLine)
!!  inflow_deleteExtInflows (called by deleteObjects in project.c)
!!  inflow_deleteDwfInflows (called by deleteObjects in project.c)
!!  inflow_getExtInflow     (called by addExternalInflows in routing.c)
!!  inflow_getDwfInflow     (called by addDryWeatherInflows in routing.c)
!!  inflow_getPatternFactor
contains
!!=============================================================================
!
!int inflow_readExtInflow(char* tok(), int ntoks)
!!
!!  Input:   tok() = array of string tokens
!!           ntoks = number of tokens
!!  Output:  returns an error message
!!  Purpose: reads parameters of a direct external inflow from a line of input.
!!
!!  Formats of data line are:
!!     nodeID  FLOW      tSeriesID  (FLOW         1.0          scaleFactor  baseline  basePat)
!!     nodeID  pollutID  tSeriesID  (CONCEN/MASS  unitsFactor  scaleFactor  baseline  basePat)
!!
!{
!    int    j                          ! object index
!    int    param                      ! FLOW (-1) or pollutant index
!    int    type = CONCEN_INFLOW       ! FLOW, CONCEN or MASS inflow
!    int    tseries = -1               ! time series index
!    int    basePat = -1               ! baseline pattern                     !(5.0.014 - LR)
!    double cf = 1.0                   ! units conversion factor
!    double sf = 1.0                   ! scaling factor
!    double baseline = 0.0             ! baseline value
!    TExtInflow* inflow                ! external inflow object
!
!    ! --- find index of node receiving the inflow
!    if ( ntoks < 3 ) return error_setInpError(ERR_ITEMS, "")
!    j = project_findObject(NODE, tok(0))
!    if ( j < 0 ) return error_setInpError(ERR_NAME, tok(0))
!
!    ! --- find index of inflow pollutant or use -1 for FLOW
!    param = project_findObject(POLLUT, tok(1))
!    if ( param < 0 )
!    {
!        if ( match(tok(1), w_FLOW) ) param = -1
!        else return error_setInpError(ERR_NAME, tok(1))
!    }
!
!    ! --- find index of inflow time series (if supplied) in data base
!    if ( strlen(tok(2)) > 0 )
!    {
!        tseries = project_findObject(TSERIES, tok(2))
!        if ( tseries < 0 ) return error_setInpError(ERR_NAME, tok(2))
!        Tseries(tseries).refersTo = EXTERNAL_INFLOW                           !(5.0.019 - LR)
!    }
!
!    ! --- assign type & cf values for a FLOW inflow
!    if ( param == -1 )
!    {
!        type = FLOW_INFLOW
!        cf = 1.0/UCF(FLOW)
!    }
!
!    ! --- do the same for a pollutant inflow
!    else if ( ntoks >= 4 )
!    {
!        if      ( match(tok(3), w_CONCEN) ) type = CONCEN_INFLOW
!        else if ( match(tok(3), w_MASS) )   type = MASS_INFLOW
!        else    return error_setInpError(ERR_KEYWORD, tok(3))
!        if ( ntoks >= 5 .and. type == MASS_INFLOW )
!        {
!            if ( ! getDouble(tok(4), &cf) )
!            {
!                return error_setInpError(ERR_NUMBER, tok(4))
!            }
!            if ( cf <= 0.0 ) return error_setInpError(ERR_NUMBER, tok(4))
!        }
!    }
!
!    ! --- get sf and baseline values
!    if ( ntoks >= 6 )
!    {
!        if ( ! getDouble(tok(5), &sf) )
!        {
!            return error_setInpError(ERR_NUMBER, tok(5))
!        }
!    }
!    if ( ntoks >= 7 )
!    {
!        if ( ! getDouble(tok(6), &baseline) )
!        {
!            return error_setInpError(ERR_NUMBER, tok(6))
!        }
!    }
!
!    ! --- get baseline time pattern                                           !(5.0.014 - LR)
!    if ( ntoks >= 8 )                                                          !(5.0.014 - LR)
!    {                                                                          !(5.0.014 - LR)
!        basePat = project_findObject(TIMEPATTERN, tok(7))                     !(5.0.014 - LR)
!        if ( basePat < 0 ) return error_setInpError(ERR_NAME, tok(7))         !(5.0.014 - LR)
!    }                                                                          !(5.0.014 - LR)
!
!    ! --- include LperFT3 term in conversion factor for MASS_INFLOW
!    if ( type == MASS_INFLOW ) cf /= LperFT3
!
!    ! --- check if an external inflow object for this constituent already exists
!    inflow = Node(j).extInflow
!    while ( inflow )
!    {
!        if ( inflow->param == param ) break
!        inflow = inflow->next
!    }
!
!    ! --- if it doesn't exist, then create it
!    if ( inflow == NULL )
!    {
!        inflow = (TExtInflow *) malloc(sizeof(TExtInflow))
!        if ( inflow == NULL ) return error_setInpError(ERR_MEMORY, "")
!        inflow->next = Node(j).extInflow
!        Node(j).extInflow = inflow
!    }
!
!    ! --- assign property values to the inflow object
!    inflow->param    = param
!    inflow->type     = type
!    inflow->tSeries  = tseries
!    inflow->cFactor  = cf
!    inflow->sFactor  = sf
!    inflow->baseline = baseline
!    inflow->basePat  = basePat                                                !(5.0.014 - LR)
!    return 0
!}
!
!!=============================================================================
!
!void inflow_deleteExtInflows(int j)
!!
!!  Input:   j = node index
!!  Output:  none
!!  Purpose: deletes all time series inflow data for a node.
!!
!{
!    TExtInflow* inflow1
!    TExtInflow* inflow2
!    inflow1 = Node(j).extInflow
!    while ( inflow1 )
!    {
!        inflow2 = inflow1->next
!        free(inflow1)
!        inflow1 = inflow2 
!    }
!}
!
!=============================================================================

real(kind=dp) function inflow_getExtInflow(inflow, aDate)
!
!  Input:   inflow = external inflow data structure
!           aDate = current simulation date/time
!  Output:  returns current value of external inflow parameter
!  Purpose: retrieves the value of an external inflow at a specific
!           date and time.
!
    use headers
    use modDateTime
    use swmm5futil
    !use modTable
    implicit none
    type(TExtInflow), intent(in) :: inflow
    real(kind=dp), intent(in) :: aDate
    
    integer ::    month, day, hour    !(5.0.014 - LR)
    integer ::    p      ! baseline pattern                       !(5.0.014 - LR)
    integer ::    k      ! time series index
    real(kind=dp) :: cfm    ! units conversion factor
    real(kind=dp) :: sfm    ! scaling factor
    real(kind=dp) :: blv   ! baseline value
    real(kind=dp) :: tsv   ! time series value
    
    !real(kind=dp) :: table_tseriesLookup2 !a function

    p = inflow%basePat
    k = inflow%tSeries
    cfm = inflow%cFactor
    sfm = inflow%sFactor
    blv = inflow%baseline
    tsv = 0.0

!    if ( p >= 0 )                                             !(5.0.014 - LR)
!    {                                                         !(5.0.014 - LR)
!        month = datetime_monthOfYear(aDate) - 1              !(5.0.014 - LR)
!        day   = datetime_dayOfWeek(aDate) - 1                !(5.0.014 - LR)
!        hour  = datetime_hourOfDay(aDate)                    !(5.0.014 - LR)
!        blv   = blv * inflow_getPatternFactor(p, month, day, hour) !(5.0.019 - LR)
!    }                                                                          !(5.0.014 - LR)

!    if ( k >= 0 ) tsv = table_tseriesLookup2(Tseries(k), aDate, .FALSE.) * sfm
    if ( k > 0 ) then
      tsv = table_tserLookup2(oTsers(k)%odates, oTsers(k)%ovalues, &
                                &size(oTsers(k)%odates, 1), aDate, .FALSE.)
    end if
    inflow_getExtInflow = cfm * (tsv * sfm + blv)
    !write(24,*) 'in inflow ',inflow_getExtInflow,cfm,tsv,sfm,blv
end function inflow_getExtInflow
!
!!=============================================================================
!
!int inflow_readDwfInflow(char* tok(), int ntoks)
!!
!!  Input:   tok() = array of string tokens
!!           ntoks = number of tokens
!!  Output:  returns an error message
!!  Purpose: reads dry weather inflow parameters from line of input data.
!!
!!  Format of data line is:
!!    nodeID  FLOW/pollutID  avgValue  (pattern1 pattern2  ... pattern4)
!!
!{
!    int    i
!    int    j                          ! node index
!    int    k                          ! pollutant index (-1 for flow)
!    int    m                          ! time pattern index
!    int    pats(4)                    ! time pattern index array
!    double x                          ! avg. DWF value
!    TDwfInflow* inflow                ! dry weather flow inflow object
!
!    ! --- find index of node receiving the inflow
!    if ( ntoks < 3 ) return error_setInpError(ERR_ITEMS, "")
!    j = project_findObject(NODE, tok(0))
!    if ( j < 0 ) return error_setInpError(ERR_NAME, tok(0))
!
!    ! --- find index of inflow pollutant (-1 for FLOW) 
!    k = project_findObject(POLLUT, tok(1))
!    if ( k < 0 )
!    {
!        if ( match(tok(1), w_FLOW) ) k = -1
!        else return error_setInpError(ERR_NAME, tok(1))
!    }
!
!    ! --- get avg. value of DWF inflow
!    if ( !getDouble(tok(2), &x) )
!        return error_setInpError(ERR_NUMBER, tok(2))
!    if ( k == -1 ) x /= UCF(FLOW)
!
!    ! --- get time patterns assigned to the inflow
!    for (i=0 i<4 i++) pats(i) = -1
!    for (i=3 i<7 i++)
!    {
!        if ( i >= ntoks ) break
!        if ( strlen(tok(i)) == 0 ) continue
!        m = project_findObject(TIMEPATTERN, tok(i))
!        if ( m < 0 ) return error_setInpError(ERR_NAME, tok(i))
!        pats(i-3) = m
!    }
!
!    ! --- check if inflow for this constituent already exists
!    inflow = Node(j).dwfInflow
!    while ( inflow )
!    {
!        if ( inflow->param == k ) break
!        inflow = inflow->next
!    }
!
!    ! --- if it doesn't exist, then create it
!    if ( inflow == NULL )
!    {
!        inflow = (TDwfInflow *) malloc(sizeof(TDwfInflow))
!        if ( inflow == NULL ) return error_setInpError(ERR_MEMORY, "")
!        inflow->next = Node(j).dwfInflow
!        Node(j).dwfInflow = inflow
!    }
!
!    ! --- assign property values to the inflow object
!    inflow->param = k
!    inflow->avgValue = x
!    for (i=0 i<4 i++) inflow->patterns(i) = pats(i)
!    return 0
!}
!
!!=============================================================================
!
!void inflow_deleteDwfInflows(int j)
!!
!!  Input:   j = node index
!!  Output:  none
!!  Purpose: deletes all dry weather inflow data for a node.
!!
!{
!    TDwfInflow* inflow1
!    TDwfInflow* inflow2
!    inflow1 = Node(j).dwfInflow
!    while ( inflow1 )
!    {
!        inflow2 = inflow1->next
!        free(inflow1)
!        inflow1 = inflow2 
!    }
!}
!
!!=============================================================================
!
!double inflow_getDwfInflow(TDwfInflow* inflow, int month, int day, int hour)
!!
!!  Input:   inflow = dry weather inflow data structure
!!           month = current month of year of simulation
!!           day = current day of week of simulation
!!           hour = current hour of day of simulation
!!  Output:  returns value of dry weather inflow parameter
!!  Purpose: computes dry weather inflow value at a specific point in time.
!!
!{
!    int    i,                          ! pattern type index
!           p                          ! pattern index
!    double f = 1.0                    ! pattern factor
!
!    for (i=0 i<4 i++)
!    {
!        p = inflow->patterns(i)
!        if ( p >= 0 ) f *= inflow_getPatternFactor(p, month, day, hour)       !(5.0.019 - LR)
!    }
!    return f * inflow->avgValue
!}
!
!!=============================================================================
!
!void inflow_initDwfPattern(int j)
!!
!!  Input:   j = time pattern index
!!  Output:  none
!!  Purpose: initialzes a dry weather inflow time pattern.
!!
!{
!    int i
!    for (i=0 i<24 i++) Pattern(j).factor(i) = 1.0
!    Pattern(j).count = 0
!    Pattern(j)%datatype  = -1
!    Pattern(j).ID    = NULL
!}
!
!!=============================================================================
!
!int inflow_readDwfPattern(char* tok(), int ntoks)
!!
!!  Input:   tok() = array of string tokens
!!           ntoks = number of tokens
!!  Output:  returns an error message
!!  Purpose: reads values of a time pattern from a line of input data.
!!
!!  Format of data line is:
!!    patternID  patternType  value(1) value(2) ...
!!    patternID  value(n)  value(n+1) ....          (for continuation lines)
!{
!    int i, j, k, n = 1
!
!    ! --- check for minimum number of tokens
!    if ( ntoks < 2 ) return error_setInpError(ERR_ITEMS, "")
!
!    ! --- check that pattern exists in database
!    j = project_findObject(TIMEPATTERN, tok(0))
!    if ( j < 0 ) return error_setInpError(ERR_NAME, tok(0))
!
!    ! --- check if this is first line of pattern
!    !     (ID pointer will not have been assigned yet)
!    if ( Pattern(j).ID == NULL )
!    {
!        ! --- assign ID pointer & pattern type
!        Pattern(j).ID = project_findID(TIMEPATTERN, tok(0))
!        k = findmatch(tok(1), PatternTypeWords)
!        if ( k < 0 ) return error_setInpError(ERR_KEYWORD, tok(1))
!        Pattern(j)%datatype = k
!        n = 2
!    }
!
!    ! --- start reading pattern factors from rest of line
!    while ( ntoks > n .and. Pattern(j).count < 24 )
!    {
!        i = Pattern(j).count
!        if ( !getDouble(tok(n), &Pattern(j).factor(i)) )
!            return error_setInpError(ERR_NUMBER, tok(n))
!        Pattern(j).count++
!        n++
!    }
!    return 0
!}
!
!!=============================================================================
!
!double inflow_getPatternFactor(int p, int month, int day, int hour)            !(5.0.019 - LR)
!!
!!  Input:   p = time pattern index
!!           month = current month of year of simulation
!!           day = current day of week of simulation
!!           hour = current hour of day of simulation
!!  Output:  returns value of a time pattern multiplier
!!  Purpose: computes time pattern multiplier for a specific point in time.
!{
!    select case ( Pattern(p)%datatype )
!    {
!      case MONTHLY_PATTERN:
!        if ( month >= 0 .and. month < 12 ) return Pattern(p).factor(month)
!        break
!      case DAILY_PATTERN:
!        if ( day >= 0 .and. day < 7 ) return Pattern(p).factor(day)
!        break
!      case HOURLY_PATTERN:
!        if ( hour >= 0 .and. hour < 24 ) return Pattern(p).factor(hour)
!        break
!      case WEEKEND_PATTERN:
!        if ( day == 0 .or. day == 6 )
!        {
!            if ( hour >= 0 .and. hour < 24 ) return Pattern(p).factor(hour)
!        }
!        break
!    }
!    return 1.0
!}
!
!!=============================================================================
!
real(kind=kind(1.d0)) function table_interpolate(x, x1, y1, x2, y2)
!
!  Input:   x = x value being interpolated
!           x1, x2 = x values on either side of x
!           y1, y2 = y values corrresponding to x1 and x2, respectively
!  Output:  returns the y value corresponding to x
!  Purpose: interpolates a y value for a given x value.
!
    implicit none
    integer, parameter :: dpt = kind(1.d0)
    real(kind=dpt), intent(in) :: x, x1, y1, x2, y2
    real(kind=dpt) :: dx
    dx = x2 - x1
    if ( abs(dx) < 1.0e-20 ) then
       table_interpolate = (y1 + y2) / 2.
    else
       table_interpolate = y1 + (x - x1) * (y2 - y1) / dx
    end if
end function table_interpolate
!
real(kind=kind(1.d0)) function table_tserLookup2(tablex, tabley, nItems, x, extend)
!
!  Input:   table = pointer to a TTable structure
!           x = a date/time value
!           extend = TRUE if time series extended on either end
!  Output:  returns a y-value
!  Purpose: retrieves the y-value corresponding to a time series date,
!           using interploation if necessary.
!
!  NOTE: if extend is FALSE and date x is outside the range of the table
!        then 0 is returned; if TRUE then the first or last value is
!        returned.
!
! my own version just for testing DW routing

    implicit none

    integer, parameter :: dpt = kind(1.d0)
    real(kind=dpt), dimension(:), intent(inout) :: tablex, tabley
    real(kind=dpt), intent(in) :: x
    integer, intent(in) :: nItems
    logical, intent(in) :: extend
    real(kind=dpt) :: lyVal
    integer :: i
    real(kind=dpt) :: table_interpolate  !a function
            
    if (x < tablex(1)) Then
      if (extend) then
         lyVal = tabley(1)
      else
         lyVal = 0.0
      end if
    elseif (x > tablex(nItems)) then
      if (extend) then
         lyVal = tabley(nItems)
      else
         lyVal = 0.0
      end if
    else
      do i=1, nItems -1
         if ( abs(x-tablex(i)) < tiny(1.0)) then
            lyVal = tabley(i)
            exit
         elseif (x > tablex(i) .and. x < tablex(i+1)) then
            lyVal = table_interpolate(x, tablex(i), tabley(i), tablex(i+1), tabley(i+1))
            exit
         end if
      end do
    end if
    table_tserLookup2 = lyVal 
end function table_tserLookup2
end module