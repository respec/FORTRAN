!=============================================================================
subroutine project_open(f1, f2, f3)
!
!  Input:   f1 = pointer to name of input file
!           f2 = pointer to name of report file
!           f3 = pointer to name of binary output file
!  Output:  none
!  Purpose: opens a new SWMM project.
!
   implicit none
   character(*), intent(in) :: f1, f2, f3

    call initPointers
    call setDefaults
    call openFiles(f1, f2, f3)
end subroutine project_open

!=============================================================================
subroutine initPointers 
!
!  Input:   none
!  Output:  none
!  Purpose: assigns NULL to all dynamic arrays for a new project.
!
    use consts
    use enums
    use headers
    implicit none
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
   use consts
   use enums
   use headers
   use modDateTime
   implicit none
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
   RptFlags%subcatchments = 0 !.FALSE.
   RptFlags%nodes         = 0 !.FALSE.
   RptFlags%links         = 0 !.FALSE.
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
    use consts
    use enums
    use headers
    implicit none
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

!=============================================================================

subroutine project_readInput()
!
!  Input:   none
!  Output:  none
!  Purpose: retrieves project data from input file.
!

    use consts
    use enums
    use headers
    implicit none
    ! --- create hash tables for fast retrieval of objects by ID names
!    createHashTables()

    ! --- count number of objects in input file and create them
!    input_countObjects()
!    createObjects()

    ! --- read project data from input file
!    input_readData()
!    if ( ErrorCode ) return

    ! --- establish starting & ending date/time
    StartDateTime = StartDate + StartTime
    EndDateTime   = EndDate + EndTime
    ReportStart   = ReportStartDate + ReportStartTime
    ReportStart   = MAX(ReportStart, StartDateTime)

    ! --- check for valid starting & ending date/times
    if ( EndDateTime <= StartDateTime ) then
        call report_writeErrorMsg(ERR_START_DATE, '')
    else if ( EndDateTime <= ReportStart ) then
        call report_writeErrorMsg(ERR_REPORT_DATE, '')
    else
        ! --- compute total duration of simulation in milliseconds
        !     (add on 1 msec to account for any roundoff)
        TotalDuration = (EndDateTime - StartDateTime) * MSECperDAY
        TotalDuration = TotalDuration + 1.0

        ! --- reporting step must be <= total duration
        if ( ReportStep * 1.0 > TotalDuration/1000.0 ) then
            ReportStep = int(TotalDuration/1000.0)
        end if
        if ( ReportStep * 1.0 < RouteStep ) then
           call report_writeErrorMsg(ERR_REPORT_STEP, "")
        end if
    end if
end subroutine project_readInput

!=============================================================================

!!  This function was significantly modified for release 5.0.018.  !!      !(5.0.018 - LR)

subroutine project_validate()
!
!  Input:   none
!  Output:  none
!  Purpose: checks validity of project data.
!

    use headers
    use modLink
    implicit none
    integer :: i, j, err
    double precision :: table_validate
    ! --- validate Curves and TimeSeries
    do i=1, Nobjects(E_CURVE)
         err = table_validate(Curve(i))
         if ( err /= 0 ) call report_writeErrorMsg(ERR_CURVE_SEQUENCE, Curve(i)%ID)
    end do
    do i=1, Nobjects(E_TSERIES)
        err = table_validate(Tseries(i))
        if ( err /= 0 ) call report_writeTseriesErrorMsg(Tseries(i))
    end do

    ! --- validate hydrology objects
    !     (NOTE: order is important !!!!)                                     !(5.0.019 - LR)
    !climate_validate()
    !lid_validate()                                                            !(5.0.019 - LR)
    if ( Nobjects(E_SNOWMELT) == 0 ) IgnoreSnowmelt = .TRUE.                      !(5.0.019 - LR)
    if ( Nobjects(E_AQUIFER)  == 0 ) IgnoreGwater   = .TRUE.                      !(5.0.019 - LR)
    do i=1, Nobjects(E_GAGE)
       call gage_validate(i)
    end do
    do i=1, Nobjects(E_AQUIFER)
       call gwater_validateAquifer(i)
    end do
    do i=1, Nobjects(E_SUBCATCH)
       call subcatch_validate(i)
    end do
    do i=1, Nobjects(E_SNOWMELT)
       call snow_validateSnowmelt(i)
    end do

    ! --- compute geometry tables for each shape curve
    j = 0
    do i=1, Nobjects(E_CURVE)
        if ( Curve(i)%curveType == SHAPE_CURVE ) then
            Curve(i)%refersTo = j
            Shape(j)%curve = i
!            if ( .not. shape_validate(Shape(j), Curve(i))) &
!               &call report_writeErrorMsg(ERR_CURVE_SEQUENCE, Curve(i)%ID)
            j = j + 1
        end if
    end do

    ! --- validate links before nodes, since the latter can
    !     result in adjustment of node depths
    do i=1, Nobjects(E_NODE)
       Node(i)%oldDepth = Node(i)%fullDepth
    end do
    do i=1, Nobjects(LINK)
       call link_validate(i)
    end do
    do i=1, Nobjects(E_NODE)
       call node_validate(i)
    end do

    ! --- adjust time steps if necessary
    if ( DryStep < WetStep ) then
        call report_writeWarningMsg(WARN06, '')
        DryStep = WetStep
    end if
    if ( RouteStep > WetStep ) then
        call report_writeWarningMsg(WARN07, '')
        RouteStep = WetStep
    end if

    ! --- adjust individual reporting flags to match global reporting flag
    if ( RptFlags%subcatchments == E_ALL ) then
        do i=1, Nobjects(E_SUBCATCH)
           Subcatch(i)%rptFlag = .TRUE.
        end do
    end if
    if ( RptFlags%nodes == E_ALL ) then
        do i=1, Nobjects(E_NODE)
           Node(i)%rptFlag = .TRUE.
        end do
    end if
    if ( RptFlags%links == E_ALL ) then
        do i=1, Nobjects(LINK)
           arrLink(i)%rptFlag = .TRUE.
        end do
    end if
end subroutine project_validate

!=============================================================================

integer function project_addObject(objtype, id, n)
!
!  Input:   objtype = object type
!           id   = object ID string
!           n    = object index
!  Output:  returns 0 if object already added, 1 if not, -1 if hashing fails
!  Purpose: adds an object ID to a hash table
!

    implicit none
    integer, intent(in) :: objtype, n
    character(*), intent(in) :: id
    integer ::  mresult
    integer ::  strlen
    character(20) :: newID
!    integer :: HTinsert
!    type(HTtable) :: Htable
!    ! --- do nothing if object already placed in hash table
!    if ( project_findObject(objtype, id) >= 0 ) then
!        project_addObject = 0
!        return
!    end if
!
!    ! --- use memory from the hash tables' common memory pool to store
!    !     a copy of the object's ID string
!    strlen = len(id) + 1
!    newID = id
!
!    ! --- insert object's ID into the hash table for that type of object
!    mresult = HTinsert(Htable(objtype), newID, n)
!    if ( mresult == 0 ) mresult = -1
!    project_addObject = mresult
end function project_addObject

!=============================================================================

subroutine createObjects()
!
!  Input:   none
!  Output:  none
!  Purpose: allocates memory for project's objects.
!
!  NOTE: number of each type of object has already been determined in
!        project_readInput().
!
    use consts
    use enums
    use headers
    implicit none
    integer :: j, k

    ! --- allocate memory for each category of object
    if ( ErrorCode /= 0 ) return
    allocate(Gage(Nobjects(E_GAGE)))
!   allocate(Subcatch(Nobjects(SUBCATCH)))
    allocate(Node(Nobjects(E_NODE)))
    allocate(Outfall(Nnodes(E_OUTFALL)))
!   allocate(Divider(Nnodes(DIVIDER)))
!   allocate(Storage(Nnodes(STORAGE)))
    allocate(arrLink(Nobjects(LINK)))
    allocate(Conduit(Nlinks(E_CONDUIT)))
!   allocate(Pump(Nlinks(PUMP)))
!   allocate(Orifice(Nlinks(ORIFICE)))
!   allocate(Weir(Nlinks(WEIR)))
!   allocate(Outlet(Nlinks(OUTLET)))
!   allocate(Pollut(Nobjects(E_POLLUT)))
!   allocate(Landuse(Nobjects(LANDUSE)))
!   allocate(Pattern(Nobjects(TIMEPATTERN)))
!   allocate(Curve(Nobjects(CURVE)))
!   allocate(Tseries(Nobjects(TSERIES)))
!   allocate(Aquifer(Nobjects(AQUIFER)))
!   allocate(UnitHyd(Nobjects(UNITHYD)))
!   allocate(Snowmelt(Nobjects(SNOWMELT)))
!   allocate(Shape(Nobjects(SHAPE)))

    ! --- create LID objects                                                  !(5.0.019 - LR)
    !lid_create(Nobjects(LID), Nobjects(SUBCATCH))                             !(5.0.019 - LR)

    ! --- create control rules
    !ErrorCode = controls_create(Nobjects(CONTROL))
    !if ( ErrorCode ) return

    ! --- create cross section transects
!   ErrorCode = transect_create(Nobjects(TRANSECT))
!   if ( ErrorCode ) return

    ! --- allocate memory for infiltration data
    !infil_create(Nobjects(SUBCATCH), InfilModel)                              !(5.0.019 - LR)

    ! --- allocate memory for water quality state variables
    !in fortran, it is not standard conforming to have allocatable in a derived type
    !so, just assume there are 6 pollutants, which is probably enough
    !so, the following is not needed
!    if (Nobjects(E_POLLUT) > 0) then
!       do j = 1, Nobjects(E_SUBCATCH)
!           allocate(Subcatch(j)%initBuildup(Nobjects(E_POLLUT)))
!           allocate(Subcatch(j)%oldQual(Nobjects(E_POLLUT)))
!           allocate(Subcatch(j)%newQual(Nobjects(E_POLLUT)))
!           allocate(Subcatch(j)%pondedQual(Nobjects(E_POLLUT)))
!           allocate(Subcatch(j)%totalLoad(Nobjects(E_POLLUT)))
!       end do
!   end if
   do j =1, Nobjects(E_NODE)
!       if (Nobjects(E_POLLUT) > 0) then
!           allocate(Node(j)%oldQual(Nobjects(E_POLLUT)))
!           allocate(Node(j)%newQual(Nobjects(E_POLLUT)))
!       end if

       !Node(j).wStored = (double *) calloc(Nobjects(E_POLLUT), sizeof(double)) !(5.0.018 - LR)

       nullify(Node(j)%extInflow)
       nullify(Node(j)%dwfInflow)
       nullify(Node(j)%rdiiInflow)
       nullify(Node(j)%treatment)
   end do
!   if (Nobjects(E_POLLUT) > 0) then
!       do j =1, Nobjects(LINK)
!           allocate(arrLink(j)%oldQual(Nobjects(E_POLLUT)))
!           allocate(arrLink(j)%newQual(Nobjects(E_POLLUT)))
!       end do
!   end if

!   ! --- allocate memory for land use buildup/washoff functions
!   for (j = 0 j < Nobjects(LANDUSE) j++)
!   {
!       Landuse(j).buildupFunc =
!           (TBuildup *) calloc(Nobjects(E_POLLUT), sizeof(TBuildup))
!       Landuse(j).washoffFunc =
!           (TWashoff *) calloc(Nobjects(E_POLLUT), sizeof(TWashoff))
!   }

    ! --- allocate memory for subcatchment landuse factors
!   for (j = 0 j < Nobjects(SUBCATCH) j++)
!   {
!       Subcatch(j).landFactor =
!           (TLandFactor *) calloc(Nobjects(LANDUSE), sizeof(TLandFactor))
!       for (k = 0 k < Nobjects(LANDUSE) k++)
!       {
!           Subcatch(j).landFactor(k).buildup =
!               (double *) calloc(Nobjects(E_POLLUT), sizeof(double))
!       }
!   }

    ! --- initialize buildup & washoff functions
!   for (j = 0 j < Nobjects(LANDUSE) j++)
!   {
!       for (k = 0 k < Nobjects(E_POLLUT) k++)
!       {
!           Landuse(j).buildupFunc(k).funcType = NO_BUILDUP
!           Landuse(j).buildupFunc(k).normalizer = PER_AREA
!           Landuse(j).washoffFunc(k).funcType = NO_WASHOFF
!       }
!   }

    ! --- initialize rain gage properties
    do j = 1, Nobjects(E_GAGE)
        Gage(j)%tSeries = -1
        Gage(j)%fname = ''
    end do

    ! --- initialize subcatchment properties
!   for (j = 0 j < Nobjects(SUBCATCH) j++)
!   {
!       Subcatch(j).outSubcatch = -1
!       Subcatch(j).outNode     = -1
!       Subcatch(j).infil       = -1
!       Subcatch(j).groundwater = NULL
!       Subcatch(j).snowpack    = NULL
!       Subcatch(j).lidArea     = 0.0                                         !(5.0.019 - LR)
!       for (k = 0 k < Nobjects(E_POLLUT) k++)
!       {
!           Subcatch(j).initBuildup(k) = 0.0
!       }
!   }

    ! --- initialize RDII unit hydrograph properties
!   for ( j = 0 j < Nobjects(UNITHYD) j++ ) rdii_initUnitHyd(j)

    ! --- initialize snowmelt properties
!   for ( j = 0 j < Nobjects(SNOWMELT) j++ ) snow_initSnowmelt(j)

    ! --- initialize storage node properties                                  !(5.0.015 - LR)
!   for (j = 0 j < Nnodes(STORAGE) j++) Storage(j).infil = NULL             !(5.0.015 - LR)

    ! --- initialize link properties
    do j = 1, Nobjects(LINK)
        arrLink(j)%xsect%datatype   = -1
        arrLink(j)%cLossInlet   = 0.0
        arrLink(j)%cLossOutlet  = 0.0
        arrLink(j)%cLossAvg     = 0.0
        arrLink(j)%hasFlapGate  = .FALSE.
    end do
!   for (j = 0 j < Nlinks(PUMP) j++) Pump(j).pumpCurve  = -1

    ! --- initialize reporting flags
!   for (j = 0 j < Nobjects(SUBCATCH) j++) Subcatch(j).rptFlag = .FALSE.
    do j = 1, Nobjects(E_NODE)
       Node(j)%rptFlag = .FALSE.
    end do
    do j = 1, Nobjects(LINK)
       arrLink(j)%rptFlag = .FALSE.
    end do

    !  --- initialize curves, time series, and time patterns
!   for (j = 0 j < Nobjects(CURVE) j++)   table_init(&Curve(j))
!   for (j = 0 j < Nobjects(TSERIES) j++) table_init(&Tseries(j))
!   for (j = 0 j < Nobjects(TIMEPATTERN) j++) inflow_initDwfPattern(j)
end subroutine createObjects

!=============================================================================

subroutine deleteObjects()
!
!  Input:   none
!  Output:  none
!  Purpose: frees memory allocated for a project's objects.
!
!  NOTE: care is taken to first free objects that are properties of another
!        object before the latter is freed (e.g., we must free a
!        subcatchment's land use factors before freeing the subcatchment).
!

    use consts
    use enums
    use headers
    implicit none
    integer :: j, k

    ! --- free memory for landuse factors & groundwater
!   if ( Subcatch ) for (j = 0 j < Nobjects(SUBCATCH) j++)
!   {
!       for (k = 0 k < Nobjects(LANDUSE) k++)
!       {
!           FREE(Subcatch(j).landFactor(k).buildup)
!       }
!       FREE(Subcatch(j).landFactor)
!       FREE(Subcatch(j).groundwater)
!       FREE(Subcatch(j).snowpack)                                            !(5.0.015 - LR)
!   }

    ! --- free memory for buildup/washoff functions
!   if ( Landuse ) for (j = 0 j < Nobjects(LANDUSE) j++)
!   {
!       FREE(Landuse(j).buildupFunc)
!       FREE(Landuse(j).washoffFunc)
!   }

    ! --- free memory for water quality state variables
!   if ( Subcatch ) for (j = 0 j < Nobjects(SUBCATCH) j++)
!   {
!       FREE(Subcatch(j).initBuildup)
!       FREE(Subcatch(j).oldQual)
!       FREE(Subcatch(j).newQual)
!       FREE(Subcatch(j).pondedQual)
!       FREE(Subcatch(j).totalLoad)
!   }
!   if ( Node ) for (j = 0 j < Nobjects(NODE) j++)
!   {
!       FREE(Node(j).oldQual)
!       FREE(Node(j).newQual)
!   }
!   if ( Link ) for (j = 0 j < Nobjects(LINK) j++)
!   {
!       FREE(Link(j).oldQual)
!       FREE(Link(j).newQual)
!   }

    ! --- free memory used for infiltration                                   !(5.0.019 - LR)
!   infil_delete()                                                            !(5.0.019 - LR)
!   if ( Node ) for (j = 0 j < Nnodes(STORAGE) j++) FREE(Storage(j).infil)  !(5.0.015 - LR)

    ! --- free memory used for nodal inflows & treatment functions
!   if ( Node ) for (j = 0 j < Nobjects(NODE) j++)
!   {
!       inflow_deleteExtInflows(j)
!       inflow_deleteDwfInflows(j)
!       rdii_deleteRdiiInflow(j)
!       treatmnt_delete(j)
!   }

    ! --- delete table entries for curves and time series
!   if ( Tseries ) for (j = 0 j < Nobjects(TSERIES) j++)
!       table_deleteEntries(&Tseries(j))
!   if ( Curve ) for (j = 0 j < Nobjects(CURVE) j++)
!       table_deleteEntries(&Curve(j))

    ! --- delete cross section transects
!   transect_delete()

    ! --- delete control rules
!   controls_delete()

    ! --- delete LIDs                                                         !(5.0.019 - LR)
!   lid_delete()                                                              !(5.0.019 - LR)

    ! --- now free each major category of object
    deallocate(Gage)
    deallocate(Subcatch)
    deallocate(Node)
    deallocate(Outfall)
    deallocate(Divider)
    deallocate(Storage)
    deallocate(arrLink)
    deallocate(Conduit)
    deallocate(Pump)
    deallocate(Orifice)
    deallocate(Weir)
    deallocate(Outlet)
    deallocate(Pollut)
    deallocate(Landuse)
    deallocate(Pattern)
    deallocate(Curve)
    deallocate(Tseries)

!    deallocate(HortInfil)                                                         !(5.0.019 - LR)
!    deallocate(GAInfil)                                                           !(5.0.019 - LR)
!    deallocate(CNInfil)                                                           !(5.0.019 - LR)

    deallocate(Aquifer)
    deallocate(UnitHyd)
    deallocate(Snowmelt)
    deallocate(Shape)                                                               !(5.0.010 - LR)
end subroutine deleteObjects

!!=============================================================================
!void createHashTables()
!!
!!  Input:   none
!!  Output:  returns error code
!!  Purpose: allocates memory for object ID hash tables
!!
!{   int j
!    MemPoolAllocated = .FALSE.
!    for (j = 0 j < MAX_OBJ_TYPES  j++)
!    {
!         Htable(j) = HTcreate()
!         if ( Htable(j) == NULL ) report_writeErrorMsg(ERR_MEMORY, "")
!    }
!
!    ! --- initialize memory pool used to store object ID's
!    if ( AllocInit() == NULL ) report_writeErrorMsg(ERR_MEMORY, "")
!    else MemPoolAllocated = .TRUE.
!}
!
!!=============================================================================
!void deleteHashTables()
!!
!!  Input:   none
!!  Output:  none
!!  Purpose: frees memory allocated for object ID hash tables
!!
!{
!    int j
!    for (j = 0 j < MAX_OBJ_TYPES j++)
!    {
!        if ( Htable(j) != NULL ) HTfree(Htable(j))
!    }
!
!    ! --- free object ID memory pool
!    if ( MemPoolAllocated ) AllocFreePool()
!}
!
!!=============================================================================

!=============================================================================

integer function project_init()
!
!  Input:   none
!  Output:  returns an error code
!  Purpose: initializes the internal state of all objects.
! 
    use headers
    use modLink
    use modClimate
    implicit none
    
    integer :: j
    call climate_initState()
    !call lid_initState()                                  !(5.0.019 - LR)
    do j=1, Nobjects(E_TSERIES)
       call table_tseriesInit(Tseries(j))
    end do
    do j=1, Nobjects(E_GAGE)
       call gage_initState(j)
    end do
    do j=1, Nobjects(E_SUBCATCH)
       call subcatch_initState(j)
    end do
    do j=1, Nobjects(E_NODE)
       call node_initState(j)
    end do
    do j=1, Nobjects(LINK)
       call link_initState(j)
    end do
    project_init = ErrorCode
end function project_init
