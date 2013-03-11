module swmm5f

integer, parameter :: MAX_EXCEPTIONS = 100            ! max. number of exceptions handled

!-----------------------------------------------------------------------------
!  Unit conversion factors
!-----------------------------------------------------------------------------
double precision, dimension(10,2), parameter :: arrUcf = &                !(5.0.010 - LR)
    &reshape((/43200.0,   1097280.0 , &         ! RAINFALL (in/hr, mm/hr --> ft/sec)
      &12.0,      304.8     , &         ! RAINDEPTH (in, mm --> ft)
      &1036800.0, 26334720.0, &         ! EVAPRATE (in/day, mm/day --> ft/sec)
      &1.0,       0.3048    , &         ! LENGTH (ft, m --> ft)
      &2.2956e-5, 0.92903e-5, &         ! LANDAREA (ac, ha --> ft2)
      &1.0,       0.02832   , &         ! VOLUME (ft3, m3 --> ft3)
      &1.0,       1.608     , &         ! WINDSPEED (mph, km/hr --> mph)
      &1.0,       1.8       , &         ! TEMPERATURE (deg F, deg C --> deg F)
      &2.203e-6,  1.0e-6    , &         ! MASS (lb, kg --> mg)
      &43560.0,   3048.0   /), &        ! GWFLOW (cfs/ac, cms/ha --> ft/sec)   !(5.0.010 - LR)
      &(/10, 2/))

!      {!  US      SI
!      {43200.0,   1097280.0 },         ! RAINFALL (in/hr, mm/hr --> ft/sec)
!      {12.0,      304.8     },         ! RAINDEPTH (in, mm --> ft)
!      {1036800.0, 26334720.0},         ! EVAPRATE (in/day, mm/day --> ft/sec)
!      {1.0,       0.3048    },         ! LENGTH (ft, m --> ft)
!      {2.2956e-5, 0.92903e-5},         ! LANDAREA (ac, ha --> ft2)
!      {1.0,       0.02832   },         ! VOLUME (ft3, m3 --> ft3)
!      {1.0,       1.608     },         ! WINDSPEED (mph, km/hr --> mph)
!      {1.0,       1.8       },         ! TEMPERATURE (deg F, deg C --> deg F)
!      {2.203e-6,  1.0e-6    },         ! MASS (lb, kg --> mg)
!      {43560.0,   3048.0    }          ! GWFLOW (cfs/ac, cms/ha --> ft/sec)   !(5.0.010 - LR)
!      };
double precision, dimension(6), parameter :: Qcf = &                 ! Flow Conversion Factors:
     &(/ 1.0,     448.831, 0.64632, &     ! cfs, gpm, mgd --> cfs
       &0.02832, 28.317,  2.4466/)    ! cms, lps, mld --> cfs

!-----------------------------------------------------------------------------
!  Shared variables
!-----------------------------------------------------------------------------
logical :: IsOpenFlag           ! TRUE if a project has been opened
logical :: IsStartedFlag        ! TRUE if a simulation has been started
logical :: SaveResultsFlag      ! TRUE if output to be saved to binary file
integer :: ExceptionCount       ! number of exceptions handled
logical :: DoRunoff             ! TRUE if runoff is computed                !(5.0.018 - LR)
logical :: DoRouting            ! TRUE if flow routing is computed          !(5.0.018 - LR)

!-----------------------------------------------------------------------------

!program  main
!!
!!  Input:   argc = number of command line arguments
!!           argv = array of command line arguments
!!  Output:  returns error status
!!  Purpose: processes command line arguments.
!!
!!  Command line for stand-alone operation is: swmm5 f1  f2  f3
!!  where f1 = name of input file, f2 = name of report file, and
!!  f3 = name of binary output file if saved (or blank if not saved).
!!
!    character(80) :: inputFile;
!    character(80) :: reportFile;
!    character(80) :: binaryFile;
!    char blank[] = "";
!    time_t start;
!    double runTime;
!
!    ! --- initialize flags
!    IsOpenFlag = FALSE;
!    IsStartedFlag = FALSE;
!    SaveResultsFlag = TRUE;
!
!    ! --- check for proper number of command line arguments
!    start = time(0);
!    if (argc < 3) writecon(FMT01);
!    else
!    {
!        ! --- extract file names from command line arguments
!        inputFile = argv[1];
!        reportFile = argv[2];
!        if (argc > 3) binaryFile = argv[3];
!        else          binaryFile = blank;
!        writecon(FMT02);
!
!        ! --- run SWMM
!        swmm_run(inputFile, reportFile, binaryFile);
!
!        ! Display closing status on console
!        runTime = difftime(time(0), start);
!        sprintf(Msg, "\n\n... EPA-SWMM completed in %.2f seconds.", runTime);
!        writecon(Msg);
!        if      ( ErrorCode   ) writecon(FMT03);
!        else if ( WarningCode ) writecon(FMT04);
!        else                    writecon(FMT05);
!    }
!
!! --- Use the code below if you need to keep the console window visible
!/* 
!    writecon("    Press Enter to continue...");
!    getchar();
!*/
!end program main

contains

integer function swmm_run(f1, f2, f3)
!
!  Input:   f1 = name of input file
!           f2 = name of report file
!           f3 = name of binary output file
!  Output:  returns error code
!  Purpose: runs a SWMM simulation.
!
    use headers
    character(*), intent(in) :: f1, f2, f3
    integer(kind=K4) :: newHour, oldHour
    integer(kind=K4) :: theDay, theHour
    double precision :: elapsedTime
    logical :: flag

    newHour = 0 ; oldHour = 0
    elapsedTime = 0.0
    ! --- open the files & read input data
    ErrorCode = 0
    !swmm_open(f1, f2, f3)

    ! --- run the simulation if input data OK
    if ( ErrorCode == 0 ) then
        ! --- initialize values
        flag = .true.
        ErrorCode = swmm_start(flag)

        ! --- execute each time step until elapsed time is re-set to 0
        if ( ErrorCode == 0 ) then
!            writecon("\n o  Simulating day: 0     hour:  0")
            do while(.true.)
                ErrorCode = swmm_step(elapsedTime)
                newHour = elapsedTime * 24.0
                if ( newHour > oldHour ) then
                    theDay = elapsedTime
                    theHour = (elapsedTime - floor(elapsedTime)) * 24.0
!                    writecon("\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
!                    sprintf(Msg, "%-5d hour: %-2d", theDay, theHour)
!!                    call writecon(Msg)
                    oldHour = newHour
                end if
                if ( elapsedTime > 0.0 .and. ErrorCode==0 ) exit
            end do
!            writecon("\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
!                     "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
!            writecon("Simulation complete           ");
        end if

        ! --- clean up
        call swmm_end()
    end if

    ! --- report results
    !if ( Fout.mode == SCRATCH_FILE ) swmm_report();                            !(5.0.016 - LR)

    ! --- close the system
    !swmm_close()
    swmm_run = ErrorCode
end function swmm_run

integer function swmm_start(saveResults)
!
!  Input:   saveResults = TRUE if simulation results saved to binary file 
!  Output:  returns an error code
!  Purpose: starts a SWMM simulation.
!
    use globals
    use enums
    logical, intent(in) :: saveResults
    ! --- check that a project is open & no run started
    if ( ErrorCode /= 0 ) then
       swmm_start = ErrorCode
       return
    end if
    if ( .not.IsOpenFlag .or. IsStartedFlag ) then
        call report_writeErrorMsg(ERR_NOT_OPEN, "")
        swmm_start = ErrorCode
        return
    end if
    ExceptionCount = 0

    ! --- initialize runoff, routing & reporting time (in milliseconds)
    NewRunoffTime = 0.0
    NewRoutingTime = 0.0
    ReportTime =   (1000 * ReportStep) * 1.0
    StepCount = 0
    IsStartedFlag = .TRUE.

    ! --- initialize global continuity errors
    RunoffError = 0.0
    GwaterError = 0.0
    FlowError = 0.0
    QualError = 0.0

    ! --- open rainfall processor (creates/opens a rainfall
    !     interface file and generates any RDII flows)
    if ( .not. IgnoreRainfall ) call rain_open()
    if ( ErrorCode /= 0 ) then
       swmm_start = ErrorCode
       return
    end if
    ! --- initialize state of each major system component
    call project_init()

    !!  Following code segment was moved to here for release 5.0.018.  !!      !(5.0.018 - LR)
    ! --- see if runoff & routing needs to be computed
    if ( Nobjects(E_SUBCATCH) > 0 ) then
       DoRunoff = .TRUE.
    else 
       DoRunoff = .FALSE.
    end if
    
    if ( Nobjects(E_NODE) > 0 .and. .not. IgnoreRouting ) then                            !(5.0.014 - LR)
        DoRouting = .TRUE.
    else 
        DoRouting = .FALSE.
    end if

    ! --- open all computing systems (order is important!)
    call output_open()
    if ( DoRunoff ) call runoff_open()                                         !(5.0.018 - LR)
    if ( DoRouting ) call routing_open(RouteModel)                             !(5.0.018 - LR)
    call massbal_open()
    call stats_open()

    ! --- write Control Actions heading to report file
    if ( RptFlags%controls ) call report_writeControlActionsHeading()

    ! --- save saveResults flag to global variable
    SaveResultsFlag = saveResults
    
    swmm_start = ErrorCode
end function swmm_start

integer function swmm_step(elapsedTime)
!
!  Input:   elapsedTime = current elapsed time in decimal days
!  Output:  updated value of elapsedTime,
!           returns error code
!  Purpose: advances the simulation by one routing time step.
!
    double precision, intent(inout) :: elapsedTime
    ! --- check that simulation can proceed
    if ( ErrorCode /=0 ) then
       swmm_step = ErrorCode
       return
    end if
    if ( .not.IsOpenFlag .or. .not.IsStartedFlag  ) then
        call report_writeErrorMsg(ERR_NOT_OPEN, "")
        swmm_step = ErrorCode
        return
    end if

    !below was inside a Try-Catch block
    ! --- if routing time has not exceeded total duration
    if ( NewRoutingTime < TotalDuration ) then
        ! --- route flow & WQ through drainage system
        !     (runoff will be calculated as needed)
        !     (NewRoutingTime is updated)
        call execRouting(elapsedTime)
    end if

    ! --- save results at next reporting time
    if ( NewRoutingTime >= ReportTime ) then
        if ( SaveResultsFlag ) call output_saveResults(ReportTime)
        ReportTime = ReportTime + (1000 * ReportStep) * 1.0
    end if

    ! --- update elapsed time (days)
    if ( NewRoutingTime < TotalDuration ) then
        elapsedTime = NewRoutingTime / MSECperDAY
    ! --- otherwise end the simulation
    else 
        elapsedTime = 0.0
    end if
    swmm_step = ErrorCode
end function swmm_step

subroutine execRouting(elapsedTime)
!
!  Input:   elapsedTime = current elapsed time in decimal days
!  Output:  none
!  Purpose: routes flow & WQ through drainage system over a single time step.
!
    double precision, intent(in) :: elapsedTime
    double precision ::  nextRoutingTime          ! updated elapsed routing time (msec)
    double precision ::  routingStep              ! routing time step (sec)

!#ifdef WINDOWS
!    ! --- begin exception handling loop here
!    __try
!#endif
!    {
        ! --- determine when next routing time occurs
        StepCount = StepCount + 1
        if ( .not.DoRouting ) then
           routingStep = MIN(WetStep, ReportStep)             !(5.0.019 - LR)
        else 
           routingStep = routing_getRoutingStep(RouteModel, RouteStep)
        end if
        if ( routingStep <= 0.0 ) then
            ErrorCode = ERR_TIMESTEP
            return
        end if
        nextRoutingTime = NewRoutingTime + 1000.0 * routingStep

        ! --- compute runoff until next routing time reached or exceeded
!        if ( DoRunoff ) while ( NewRunoffTime < nextRoutingTime )              !(5.0.018 - LR)
!        {
!            runoff_execute();
!            if ( ErrorCode ) return;
!        }
  
        ! --- route flows through drainage system over current time step
        if ( DoRouting ) then 
           call routing_execute(RouteModel, routingStep)             !(5.0.010 - LR)
        else 
           NewRoutingTime = nextRoutingTime       !(5.0.010 - LR)
        end if
!    }

!#ifdef WINDOWS
!    ! --- end of try loop; handle exception here
!    __except(xfilter(GetExceptionCode(), elapsedTime, StepCount))
!    {
!        ErrorCode = ERR_SYSTEM;
!        return;
!    }
!#endif
end subroutine execRouting

!=============================================================================
!   General purpose functions
!=============================================================================

double precision function UCF(u)
!
!  Input:   u = integer code of quantity being converetd
!  Output:  returns a units conversion factor
!  Purpose: computes a conversion factor from SWMM's internal
!           units to user's units
!
    use DataSizeSpecs
    use globals
    integer(kind=K2), intent(in) :: u
    if ( u < FLOW ) then
        UCF = arrUcf(u, UnitSystem)
    else            
        UCF = Qcf(FlowUnits)
    end if
end function UCF

end module swmm5f