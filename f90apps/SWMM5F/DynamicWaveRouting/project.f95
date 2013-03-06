!=============================================================================
subroutine project_open(f1, f2, f3)
!
!  Input:   f1 = pointer to name of input file
!           f2 = pointer to name of report file
!           f3 = pointer to name of binary output file
!  Output:  none
!  Purpose: opens a new SWMM project.
!
   character(*), intent(in) :: f1, f2, f3

    call initPointers
    call setDefaults
    call openFiles(f1, f2, f3)
end subroutine project_open

!=============================================================================
subroutine initPointers()
!
!  Input:   none
!  Output:  none
!  Purpose: assigns NULL to all dynamic arrays for a new project.
!
    use headers
    integer :: stat
    deallocate(Gage,stat=stat)
    
    deallocate(Subcatch, stat=stat)
    deallocate(Node    , stat=stat)
    deallocate(Outfall , stat=stat)
    deallocate(Divider , stat=stat)
    deallocate(Storage , stat=stat)
    deallocate(arrLink    , stat=stat)
    deallocate(Conduit , stat=stat)
    deallocate(Pump    , stat=stat)
    deallocate(Orifice , stat=stat)
    deallocate(Weir    , stat=stat)
    deallocate(Outlet  , stat=stat)
    deallocate(Pollut  , stat=stat)
    deallocate(Landuse , stat=stat)
    deallocate(Pattern , stat=stat)
    deallocate(Curve   , stat=stat)
    deallocate(Tseries , stat=stat)
    deallocate(Transect, stat=stat)
    deallocate(Shape   , stat=stat)       !(5.0.010 - LR)  
!   deallocate( HortInfil , stat=stat)               !(5.0.019 - LR)
!   deallocate( GAInfil   , stat=stat)               !(5.0.019 - LR)
!   deallocate( CNInfil   , stat=stat)               !(5.0.019 - LR)
    deallocate(Aquifer   , stat=stat)
    deallocate(UnitHyd   , stat=stat)
    deallocate(Snowmelt  , stat=stat)
!    MemPoolAllocated = FALSE
end subroutine initPointers

!=============================================================================
subroutine setDefaults
!
!  Input:   none
!  Output:  none
!  Purpose: assigns default values to project variables.
!
   use headers
   integer :: i, j

   ! Project title & temp. file path
   do i = 1, MAXTITLE
      Title(i) = ''
   end do
   TmpDir = ''

   ! Interface files
   Frain%mode      = SCRATCH_FILE     ! Use scratch rainfall file
   Fclimate%mode   = NO_FILE 
   Frunoff%mode    = NO_FILE
   Frdii%mode      = NO_FILE
   Fhotstart1%mode = NO_FILE
   Fhotstart2%mode = NO_FILE
   Finflows%mode   = NO_FILE
   Foutflows%mode  = NO_FILE
   Frain%fileHandle      = -1
   Fclimate%fileHandle   = -1
   Frunoff%fileHandle    = -1
   Frdii%fileHandle      = -1
   Fhotstart1%fileHandle = -1
   Fhotstart2%fileHandle = -1
   Finflows%fileHandle   = -1
   Foutflows%fileHandle  = -1
   Fout%fileHandle       = -1
   Fout%mode       = NO_FILE

   ! Analysis options
   UnitSystem      = US               ! US unit system
   FlowUnits       = CFS              ! CFS flow units
   !InfilModel      = HORTON           ! Horton infiltration method
   RouteModel      = DW               ! Dyn wave flow routing method
   AllowPonding    = .false.            ! No ponding at nodes
   InertDamping    = SOME             ! Partial inertial damping
   NormalFlowLtd   = BOTH             ! Default normal flow limitation       !(5.0.010 - LR)
   ForceMainEqn    = H_W              ! Hazen-Williams eqn. for force mains  !(5.0.010 - LR)
   LinkOffsets     = DEPTH_OFFSET     ! Use depth for link offsets           !(5.0.012 - LR)
   LengtheningStep = 0                ! No lengthening of conduits
   CourantFactor   = 0.0              ! No variable time step 
   MinSurfArea     = 0.0              ! Use default min. nodal surface area
   SkipSteadyState = .FALSE.            ! Do flow routing in steady state periods 
   IgnoreRainfall  = .FALSE.            ! Analyze rainfall/runoff
   IgnoreSnowmelt  = .FALSE.            ! Analyze snowmelt                     !(5.0.014 - LR)
   IgnoreGwater    = .FALSE.            ! Analyze groundwater                  !(5.0.014 - LR)
   IgnoreRouting   = .FALSE.            ! Analyze flow routing                 !(5.0.014 - LR)
   IgnoreQuality   = .FALSE.            ! Analyze water quality                !(5.0.014 - LR)
   WetStep         = 300              ! Runoff wet time step (secs)
   DryStep         = 3600             ! Runoff dry time step (secs)
   RouteStep       = 300.0            ! Routing time step (secs)
   ReportStep      = 900              ! Reporting time step (secs)
   StartDryDays    = 0.0              ! Antecedent dry days

   ! Deprecated options                                                       !(5.0.010 - LR)
   SlopeWeighting  = .TRUE.             ! Use slope weighting                  !(5.0.010 - LR)
   Compatibility   = SWMM4            ! Use SWMM 4 up/dn weighting method    !(5.0.010 - LR)

   ! Starting & ending date/time
   StartDate       = datetime_encodeDate(2004, 1, 1)
   StartTime       = datetime_encodeTime(0,0,0)
   StartDateTime   = StartDate + StartTime
   EndDate         = StartDate
   EndTime         = 0.0                                                      !(5.0.012 - LR)
   ReportStartDate = NO_DATE
   ReportStartTime = NO_DATE
   SweepStart      = 1
   SweepEnd        = 365

   ! Reporting options
   RptFlags%input         = .FALSE.
   RptFlags%continuity    = .TRUE.
   RptFlags%flowStats     = .TRUE.
   RptFlags%controls      = .FALSE.
   RptFlags%subcatchments = .FALSE.
   RptFlags%nodes         = .FALSE.
   RptFlags%links         = .FALSE.
   RptFlags%nodeStats     = .FALSE.

   ! Temperature data
   Temp%dataSource  = NO_TEMP
   Temp%tSeries     = -1
   Temp%ta          = 70.0
   Temp%elev        = 0.0
   Temp%anglat      = 40.0
   Temp%dtlong      = 0.0
   Temp%tmax        = MISSING

   ! Wind speed data
   Wind%datatype = MONTHLY_WIND
   do i=1, 12
    Wind%aws(i) = 0.0
   end do
   ! Snowmelt parameters
   Snow%snotmp      = 34.0
   Snow%tipm        = 0.5
   Snow%rnm         = 0.6

   ! Snow areal depletion curves for pervious and impervious surfaces
   do i=1, 2
     do j= 1, 10
         Snow%adc(i, j) = 1.0 
     end do
   end do

   ! Evaporation rates
   Evap%datatype = CONSTANT_EVAP
   do i=1, 12
       Evap%monthlyEvap(i) = 0.0
       Evap%panCoeff(i)    = 1.0
   end do
   Evap%recoveryPattern = -1           !(5.0.014 - LR)
   Evap%recoveryFactor  = 1.0          !(5.0.014 - LR)
   Evap%tSeries = -1                   !(5.0.019 - LR)
   Evap%dryOnly = .FALSE.              !(5.0.019 - LR)
end subroutine setDefaults

!=============================================================================
subroutine openFiles(f1, f2, f3)
!
!  Input:   f1 = name of input file
!           f2 = name of report file
!           f3 = name of binary output file
!  Output:  none
!  Purpose: opens a project's input and report files.
!
    use headers
    character(len=MAXFNAME), intent(in) :: f1, f2, f3
    integer :: errcode
    
    ! --- initialize file pointers to NULL
    Finp%fileHandle = -1
    Frpt%fileHandle = -1
    Fout%fileHandle = -1

    ! --- save file names
    Finp%name = f1
    Frpt%name = f2
    Fout%name = f3

    ! --- check that file names are not identical
    if (f1 .eq. f2 .or. f1 .eq. f3 .or. f2 .eq. f3) then
        call writecon(FMT11)
        ErrorCode = ERR_FILE_NAME
        return
    end if

    ! --- open input and report files
    open(8, file=f1, iostat=errcode)
    if (errcode > 0) then !opening error 
        call writecon(FMT12)
        call writecon(f1)
        ErrorCode = ERR_INP_FILE
        return
    end if
    open(9, file=f2, status='replace', iostat=errcode)
    if (errcode > 0) then
       call writecon(FMT13)
       ErrorCode = ERR_RPT_FILE
       return
    end if
end subroutine openFiles