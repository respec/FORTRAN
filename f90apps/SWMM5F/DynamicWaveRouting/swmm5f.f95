program  main
!
!  Input:   argc = number of command line arguments
!           argv = array of command line arguments
!  Output:  returns error status
!  Purpose: processes command line arguments.
!
!  Command line for stand-alone operation is: swmm5 f1  f2  f3
!  where f1 = name of input file, f2 = name of report file, and
!  f3 = name of binary output file if saved (or blank if not saved).
!
    character(80) :: inputFile;
    character(80) :: reportFile;
    character(80) :: binaryFile;
    char blank[] = "";
    time_t start;
    double runTime;

    ! --- initialize flags
    IsOpenFlag = FALSE;
    IsStartedFlag = FALSE;
    SaveResultsFlag = TRUE;

    ! --- check for proper number of command line arguments
    start = time(0);
    if (argc < 3) writecon(FMT01);
    else
    {
        ! --- extract file names from command line arguments
        inputFile = argv[1];
        reportFile = argv[2];
        if (argc > 3) binaryFile = argv[3];
        else          binaryFile = blank;
        writecon(FMT02);

        ! --- run SWMM
        swmm_run(inputFile, reportFile, binaryFile);

        ! Display closing status on console
        runTime = difftime(time(0), start);
        sprintf(Msg, "\n\n... EPA-SWMM completed in %.2f seconds.", runTime);
        writecon(Msg);
        if      ( ErrorCode   ) writecon(FMT03);
        else if ( WarningCode ) writecon(FMT04);
        else                    writecon(FMT05);
    }

! --- Use the code below if you need to keep the console window visible
/* 
    writecon("    Press Enter to continue...");
    getchar();
*/
end program main

integer function swmm_run(char* f1, char* f2, char* f3)
!
!  Input:   f1 = name of input file
!           f2 = name of report file
!           f3 = name of binary output file
!  Output:  returns error code
!  Purpose: runs a SWMM simulation.
!
    long newHour, oldHour = 0;
    long theDay, theHour;
    DateTime elapsedTime = 0.0;

    ! --- open the files & read input data
    ErrorCode = 0;
    swmm_open(f1, f2, f3);

    ! --- run the simulation if input data OK
    if ( .not.ErrorCode )
    {
        ! --- initialize values
        swmm_start(TRUE);

        ! --- execute each time step until elapsed time is re-set to 0
        if ( .not.ErrorCode )
        {
            writecon("\n o  Simulating day: 0     hour:  0");
            do
            {
                swmm_step(&elapsedTime);
                newHour = (long)(elapsedTime * 24.0);
                if ( newHour > oldHour )
                {
                    theDay = (long)elapsedTime;
                    theHour = (long)((elapsedTime - floor(elapsedTime)) * 24.0);
                    writecon("\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
                    sprintf(Msg, "%-5d hour: %-2d", theDay, theHour);
                    writecon(Msg);
                    oldHour = newHour;
                }
            } while ( elapsedTime > 0.0 && !ErrorCode );
            writecon("\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
                     "\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b");
            writecon("Simulation complete           ");
        }

        ! --- clean up
        swmm_end();
    }

    ! --- report results
    if ( Fout.mode == SCRATCH_FILE ) swmm_report();                            !(5.0.016 - LR)

    ! --- close the system
    swmm_close();
    return ErrorCode;
end function swmm_run

integer function swmm_start(int saveResults)
!
!  Input:   saveResults = TRUE if simulation results saved to binary file 
!  Output:  returns an error code
!  Purpose: starts a SWMM simulation.
!
    ! --- check that a project is open & no run started
    if ( ErrorCode ) return ErrorCode;
    if ( .not.IsOpenFlag .or. IsStartedFlag )
    {
        report_writeErrorMsg(ERR_NOT_OPEN, "");
        return ErrorCode;
    }
    ExceptionCount = 0;

    ! --- initialize runoff, routing & reporting time (in milliseconds)
    NewRunoffTime = 0.0;
    NewRoutingTime = 0.0;
    ReportTime =   (double)(1000 * ReportStep);
    StepCount = 0;
    IsStartedFlag = TRUE;

    ! --- initialize global continuity errors
    RunoffError = 0.0;
    GwaterError = 0.0;
    FlowError = 0.0;
    QualError = 0.0;

    ! --- open rainfall processor (creates/opens a rainfall
    !     interface file and generates any RDII flows)
    if ( !IgnoreRainfall ) rain_open();
    if ( ErrorCode ) return ErrorCode;

    ! --- initialize state of each major system component
    project_init();

    !!  Following code segment was moved to here for release 5.0.018.  !!      !(5.0.018 - LR)
    ! --- see if runoff & routing needs to be computed
    if ( Nobjects[SUBCATCH] > 0 ) DoRunoff = TRUE;
    else DoRunoff = FALSE;
    if ( Nobjects[NODE] > 0 && !IgnoreRouting )                            !(5.0.014 - LR)
        DoRouting = TRUE;
    else DoRouting = FALSE;

    ! --- open all computing systems (order is important!)
    output_open();
    if ( DoRunoff ) runoff_open();                                         !(5.0.018 - LR)
    if ( DoRouting ) routing_open(RouteModel);                             !(5.0.018 - LR)
    massbal_open();
    stats_open();

    ! --- write Control Actions heading to report file
    if ( RptFlags.controls ) report_writeControlActionsHeading();

    ! --- save saveResults flag to global variable
    SaveResultsFlag = saveResults;    
    return ErrorCode;
end function swmm_start

integer function swmm_step(DateTime* elapsedTime)
!
!  Input:   elapsedTime = current elapsed time in decimal days
!  Output:  updated value of elapsedTime,
!           returns error code
!  Purpose: advances the simulation by one routing time step.
!
    ! --- check that simulation can proceed
    if ( ErrorCode ) return ErrorCode;
    if ( .not.IsOpenFlag .or. .not.IsStartedFlag  )
    {
        report_writeErrorMsg(ERR_NOT_OPEN, "");
        return ErrorCode;
    }

    !below was inside a Try-Catch block
    ! --- if routing time has not exceeded total duration
    if ( NewRoutingTime < TotalDuration )
    {
        ! --- route flow & WQ through drainage system
        !     (runoff will be calculated as needed)
        !     (NewRoutingTime is updated)
        execRouting(*elapsedTime);
    }

    ! --- save results at next reporting time
    if ( NewRoutingTime >= ReportTime )
    {
        if ( SaveResultsFlag ) output_saveResults(ReportTime);
        ReportTime = ReportTime + (double)(1000 * ReportStep);
    }

    ! --- update elapsed time (days)
    if ( NewRoutingTime < TotalDuration )
        *elapsedTime = NewRoutingTime / MSECperDAY;

    ! --- otherwise end the simulation
    else *elapsedTime = 0.0;

    return ErrorCode;
end function swmm_step

subroutine execRouting(DateTime elapsedTime)
!
!  Input:   elapsedTime = current elapsed time in decimal days
!  Output:  none
!  Purpose: routes flow & WQ through drainage system over a single time step.
!
    double   nextRoutingTime;          ! updated elapsed routing time (msec)
    double   routingStep;              ! routing time step (sec)

!#ifdef WINDOWS
!    ! --- begin exception handling loop here
!    __try
!#endif
!    {
        ! --- determine when next routing time occurs
        StepCount++;
        if ( .not.DoRouting ) routingStep = MIN(WetStep, ReportStep);              !(5.0.019 - LR)
        else routingStep = routing_getRoutingStep(RouteModel, RouteStep);
        if ( routingStep <= 0.0 )
        {
            ErrorCode = ERR_TIMESTEP;
            return;
        }
        nextRoutingTime = NewRoutingTime + 1000.0 * routingStep;

        ! --- compute runoff until next routing time reached or exceeded
        if ( DoRunoff ) while ( NewRunoffTime < nextRoutingTime )              !(5.0.018 - LR)
        {
            runoff_execute();
            if ( ErrorCode ) return;
        }
  
        ! --- route flows through drainage system over current time step
        if ( DoRouting ) routing_execute(RouteModel, routingStep);             !(5.0.010 - LR)
        else NewRoutingTime = nextRoutingTime;                                 !(5.0.010 - LR)
    }

!#ifdef WINDOWS
!    ! --- end of try loop; handle exception here
!    __except(xfilter(GetExceptionCode(), elapsedTime, StepCount))
!    {
!        ErrorCode = ERR_SYSTEM;
!        return;
!    }
!#endif
end subroutine execRouting