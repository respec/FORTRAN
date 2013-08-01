subroutine TongMain
!     + + + END SPECIFICATIONS + + +
!
!     variables to pass in (need to be added to the osv):
!     DTS           -- integer, dynamic wave time step
!
!     note: assume last one in list is the outfall, type is free
!     NNODE         -- integer, number of nodes
!     NODEX(NNODE)  -- integer, node index
!     NELEV(NNODE)  -- real, invert elevation
!     NDMAX(NNODE)  -- real, max depth for this node
!     NDINIT(NNODE) -- real, initial node depth
!     NDSURC(NNODE) -- real, surcharge depth for this node
!     NPONDA(NNODE) -- real, ponded area for this node
!     NLOCIN(NNODE) -- real, local inflow percentage to this node
!
!     NCOND         -- integer, number of conduits
!     CNODE1(NCOND) -- integer, upstream conduit node index 
!     CNODE2(NCOND) -- integer, downstream conduit node index
!     CLEN(NCOND)   -- real, conduit length
!     CMANN(NCOND)  -- real, conduit mannings n
!     COFF1(NCOND)  -- real, inlet offset
!     COFF2(NCOND)  -- real, outlet offset
!     CQ0(NCOND)    -- real, initial flow
!     CSHAPE(NCOND) -- integer, conduit shape type
!     CGEOM1(NCOND) -- real, first item of shape geometry
!     CGEOM2(NCOND) -- real, second item of shape geometry
!     CGEOM3(NCOND) -- real, third item of shape geometry
!     CGEOM4(NCOND) -- real, fourth item of shape geometry
!
!     conduit types
!     DUMMY: 0
!     CIRCULAR: 1
!     FORCE_MAIN: 2                                                          
!     FILLED_CIRCULAR: 3
!     EGGSHAPED: 4
!     HORSESHOE: 5
!     GOTHIC: 6
!     CATENARY: 7
!     SEMIELLIPTICAL: 8
!     BASKETHANDLE: 9
!     SEMICIRCULAR: 10
!     RECT_CLOSED: 11
!     RECT_OPEN: 12
!     RECT_TRIANG: 13
!     RECT_ROUND: 14
!     MOD_BASKET: 15
!     TRAPEZOIDAL: 16
!     TRIANGULAR: 17
!     PARABOLIC: 18
!     POWERFUNC: 19
!     HORIZ_ELLIPSE: 20
!     VERT_ELLIPSE: 21
!     ARCH: 22
!
      use headers
      use consts
      use enums
      use swmm5f
      use swmm5futil
      use modXsect
      use modLink
      use output
      implicit none
       
      integer, parameter :: NNODE = 10
      integer, parameter :: NCOND = 9
      integer :: J, LTYPE, ITS, NTS, DTS, DELTS
      integer :: k !subindex for each node type
      integer :: CNODE1, CNODE2
      integer :: messu
      integer :: datim(5)
      logical :: isOK
      
      integer :: lErrorCode, lN
      
      integer, parameter :: dpmain = kind(1.d0)
      real(kind=dpm) :: ROVOL, OVOL, VOLT, VOL, ROS, OS, RO, O
      
      real(kind=dpmain), dimension(6) :: XN
      real(kind=dpmain), dimension(6) :: XC
      real(kind=dpmain), dimension(4) :: XX
      real(kind=dpmain), dimension(NNODE) :: NDINIT = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
      real(kind=dpmain), dimension(NNODE) :: NELEV = (/124.6,118.3,128.2,117.5,112.3,101.6,111.5,102.0,102.8,89.9 /)
      real(kind=dpmain), dimension(NNODE) :: NDMAX = (/ 13.4, 16.7,  8.8, 12.5, 42.7,  9.4, 13.5, 18.0, 22.2, 0.0 /) 
     !real, dimension(NNODE) :: NDMAX = (/138.0,135.0,137.0,130.0,155.0,111.0,125.0,120.0,125.0,99.0 /) 
      real(kind=dpmain), dimension(NNODE) :: NDSURC = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
      real(kind=dpmain), dimension(NNODE) :: NPONDA = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)

      real(kind=dpmain), dimension(NCOND) :: CLEN = (/ 1800, 2075, 5100, 3500, 4500, 5000, 500, 300, 5000 /)
      real(kind=dpmain), dimension(NCOND) :: CMANN = (/ 0.015, 0.015, 0.015, 0.015, 0.016, 0.0154, 0.015, 0.015, 0.034 /)
      real(kind=dpmain), dimension(NCOND) :: COFF1 = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
      real(kind=dpmain), dimension(NCOND) :: COFF2 = (/0.0, 2.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
      real(kind=dpmain), dimension(NCOND) :: CQ0 = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
      
      integer, dimension(NCOND) :: CSHAPE = (/ CIRCULAR, CIRCULAR, CIRCULAR, CIRCULAR, &
                                           &TRAPEZOIDAL, CIRCULAR, CIRCULAR, TRAPEZOIDAL, CIRCULAR /) 
      real(kind=dpmain), dimension(NCOND) :: CGEOM1 = (/4.0, 4.0, 4.5, 4.5, 9.0, 5.5, 6.0, 9.0, 5.0 /) 
      real(kind=dpmain), dimension(NCOND) :: CGEOM2 = (/0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)
      real(kind=dpmain), dimension(NCOND) :: CGEOM3 = (/0.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 3.0, 0.0 /)
      real(kind=dpmain), dimension(NCOND) :: CGEOM4 = (/0.0, 0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 3.0, 0.0 /)
      
      type(TExtInflow), dimension(NNODE-1), target :: inflows
      real(kind=dpmain), dimension(NNODE-1) :: disFrac
      real(kind=dpmain) :: INVOL
      
      UnitSystem = US
      
      Nobjects(E_NODE) = 10
      Nnodes(E_OUTFALL) = 1
      Nobjects(LINK) = 9
      Nlinks(E_CONDUIT) = 9
      
      Nobjects(E_GAGE) = 0 !I think it will need some rain???
      Nobjects(E_POLLUT) = 0 !assume 6 pollutants maximum in objects.f95
      Nobjects(E_TSERIES) = 0

      call initPointers  !project
      call createObjects !project
      call setDefaults   !project
      
      k = 0
      DO 10 J= 1,NNODE
        LTYPE = JUNCTION ! junction
        IF (J.EQ.NNODE) THEN
!         this is the outfall
          LTYPE = E_OUTFALL ! outfall
        END IF
        !hard-code node counters
        if (lTYPE == JUNCTION) then
           k = k + 1
           XN(1) = NELEV(J)
           XN(2) = NDMAX(J)
           XN(3) = NDINIT(J)
           XN(4) = NDSURC(J)
           XN(5) = NPONDA(J)
        else if (LTYPE == E_OUTFALL) then
           k = 1 ! only one outfall
           XN(1) = NELEV(J)
           XN(2) = FREE_OUTFALL !outfall type (could be problem)
           XN(3) = NDINIT(J) + NELEV(J) !fixedStage
           XN(4) = 0 !tideCurve, index of tidal stage curve
           XN(5) = 0 !stageSeries, index of outfall stage time series
           XN(6) = 0 !hasFlapGate, true(ie 1) if contains flap gate, false(ie 0) no
        end if
        call node_setParams(J, LTYPE, k, XN)
        Node(J)%rptFlag = .true. !this is done in report_readoption
 10   CONTINUE

      !TODO: both conduits or first conduit and second: weir or outlet???
      LTYPE = E_CONDUIT
      DO 20 J= 1,NCOND
        XC(1) = CLEN(J)
        XC(2) = CMANN(J)
        XC(3) = COFF1(J)
        XC(4) = COFF2(J)
        XC(5) = CQ0(J)
        XC(6) = 0.0
        
        !hard code nodal-link schema
        select case (J)
          case (1)
             CNODE1 = 1
             CNODE2 = 2
          case (2)
             CNODE1 = 2
             CNODE2 = 5
          case (3)
             CNODE1 = 3
             CNODE2 = 4
          case (4)
             CNODE1 = 4
             CNODE2 = 7
          case (5)
             CNODE1 = 6
             CNODE2 = 10
          case (6)
             CNODE1 = 7
             CNODE2 = 8
          case (7)
             CNODE1 = 9
             CNODE2 = 8
          case (8)
             CNODE1 = 8
             CNODE2 = 6
          case (9)
             CNODE1 = 5
             CNODE2 = 9
        end select
        call link_setParams(J, LTYPE, CNODE1, CNODE2, J, XC)
        XX(1) = CGEOM1(J)
        XX(2) = CGEOM2(J)
        XX(3) = CGEOM3(J)
        XX(4) = CGEOM4(J)
        
        !call xsect_setParams(J, CSHAPE(J), 1, XX, 0.0)   !haven't quite understood this yet, this is for outlet link
        isOK = xsect_setParams(arrLink(j)%xsect, CSHAPE(J), XX, UCF(LENGTH))   !for normal conduit
        arrLink(J)%rptFlag = .true. !this is done in report_readoption
 20   CONTINUE

      !set options, ref: project.c->project_readOption()
      !the following could be incorporated into setDefaults() routine above
      
      RouteModel = DW
      StartDate = datetime_encodeDate(2002, 1, 1) !datetime_strToDate
      StartTime = datetime_encodeTime(0, 0, 0) !datetime_strToTime
      
      EndDate = datetime_encodeDate(2002, 1, 1) !datetime_strToDate
      EndTime = datetime_encodeTime(8, 0, 0) !datetime_strToTime
      
!      ReportStartDate = datetime_encodeDate(ryr, rmon, rday)
!      ReportStartTime = datetime_encodeTime(rhr, rmin, rsec)
      
      StartDryDays = 0 !number of antecedent dry days
      
      AllowPonding    = .false.
      SlopeWeighting  = .true.
      SkipSteadyState = .false. !could be problem
      IgnoreRainfall  = .false.
      IgnoreSnowmelt  = .true.
      IgnoreGwater    = .true.
      IgnoreRouting   = .false.
      IgnoreQuality   = .true.
      InertDamping = NO_DAMPING !InertDampingWords, w_PARTIAL, w_FULL
      NormalFlowLtd = SLOPE !NormalFlowType !0, means NO

      !set routing step size in seconds
      WetStep         = 900              ! Runoff wet time step (secs)
      DryStep         = 3600             ! Runoff dry time step (secs)
      !RouteStep       = 300.0            ! Routing time step (secs)
      RouteStep       = 20.0 !<-- change here
      ReportStep      = 900              ! Reporting time step (secs)
      StartDryDays    = 0.0              ! Antecedent dry days, DRY_DAYS
      
      
      ForceMainEqn = D_W !ForceMainType
      
      LinkOffsets = DEPTH_OFFSET !OffsetType
      
      Compatibility = SWMM5
      !RouteStep is set to 5 minutes in setDefault()
      !LengtheningStep = MAX(0.0, RouteStep) !if 0, then no lengthening of conduit for DW routing
      
      ! --- safety factor applied to variable time step estimates under
      !     dynamic wave flow routing (value of 0 indicates that variable
      !     time step option not used)
      CourantFactor = 0.0 !variable time step option not used, needs to be >= 0.0 and <= 2.0
      
      ! --- minimum surface area (ft2 or sq. meters) associated with nodes
      !     under dynamic wave flow routing 
      MinSurfArea = 0.0
      
      !minimum conduit slope, needs to be >= 0.0 and < 1; SWMM input file enters %
      MinSlope = 0 !0.05
      
      TmpDir = 'C:\Temp'
      
      !the following settings are from link->link_readXsectParams(toks, ntoks)
      !supposed to be read from input files, but we will specify here as pertinent
      
      do J = 1, NCOND
         !assume each conduit has only one barrels
         Conduit(arrLink(J)%subIndex)%barrels = 1
         !--- assume link is not a culvert    !(5.0.014 - LR)
         arrLink(J)%xsect%culvertCode = 0
      end do
      
!
! construct external inflow to each node in the network
!
    !distribute flow to all nodes except the outlet
    !assume last node is always the outlet
!    allocate(inflows(NNODE))
!    if (allocated(inflows)) then
!    end if
!    INVOL = 100.0 !cfs in flow in the first upstream node
!    disFrac = (/0.5, 0.5/)
!    
    allocate(oTsers(3))
    do J=1, 3
      oTsers(J)%datatype = E_TSERIES
      allocate(oTsers(J)%odates(5))
      allocate(oTsers(J)%ovalues(5))
      if (associated(oTsers(J)%odates) .and. &
         &associated(oTsers(J)%ovalues)) then
         oTsers(J)%odates(1) = StartDate + StartTime
         oTsers(J)%odates(2) = oTsers(J)%odates(1) + 0.25 / 24.0
         oTsers(J)%odates(3) = oTsers(J)%odates(1) + 3.00 / 24.0
         oTsers(J)%odates(4) = oTsers(J)%odates(1) + 3.25 / 24.0
         oTsers(J)%odates(5) = oTsers(J)%odates(1) + 12.0 / 24.0
         oTsers(J)%ovalues(1) = 0.0
         oTsers(J)%ovalues(4) = 0.0
         oTsers(J)%ovalues(5) = 0.0
         select case (J)
           case (1)
             oTsers(J)%ovalues(2) = 40.0
             oTsers(J)%ovalues(3) = 40.0
           case (2)
             oTsers(J)%ovalues(2) = 45.0
             oTsers(J)%ovalues(3) = 45.0
           case (3)
             oTsers(J)%ovalues(2) = 50.0
             oTsers(J)%ovalues(3) = 50.0
         end select
      end if
    end do
    
    do J = 1, NNODE - 1
       inflows(J)%param = -1 !flow
       inflows(J)%datatype = FLOW_INFLOW !or EXTERNAL_INFLOW, user-supplied external inflow
       !inflows(J)%tseries = J
       select case (J)
         case (1)
           inflows(J)%tseries = 2
         case (3)
           inflows(J)%tseries = 3
         case (5)
           inflows(J)%tseries = 1
         case default
           inflows(J)%tseries = -1
       end select
       inflows(J)%basePat = 1
       inflows(J)%cFactor = 1.0
       inflows(J)%sFactor = 1.0
       inflows(J)%baseline = 0.0
       nullify(inflows(J)%next)
       if (j.eq.1 .or. j.eq.3 .or. j.eq.5) then
          Node(J)%extInflow => inflows(J)
       else
          Nullify(Node(J)%extInflow)
       end if
    end do
      
!      NTS = DELTS/DTS
!      DO 100 ITS = 1,NTS
!         call dynwave_execute(links,DTS)
! 100  CONTINUE
 
!      call project_validate !in project.f95, ensure all is well before try running
!      if (ErrorCode == 0) IsOpenFlag = .true.
!      
!      lErrorCode = swmm_start(.true.)
!      if (ErrorCode /= 0) stop

      lErrorCode = swmm_run('', '', '', messu, datim)
      
      !The resulting nodes' and links' flow, depth, and volume outputs are
      !saved in the onodes array and olinks array. 
      !Each element of the onodes or olinks array has 3 arrays, oflow, odepth, and ovolume
      !that contain output values for each reporting steps in TSDateTime
      !For example, if you want to see the depth of 2nd node at reporting step number 4, access it as below
      !  onodes(2)%odepth(4)
      !             if you want to see the flow of 1st conduit at reporting step number 3, access it as below
      !  olinks(1)%oflow(3)
!      do J=1, OutputSize
!         write(*,*) TSDateTime(J), ",", TSOutletVals(J)
!         write(*,*) TSDateTime(J), ",", onodes(1)%oflow(J), ",", onodes(2)%odepth(J), ",", onodes(3)%ovolume(J)
!         write(*,*) TSDateTime(J), ",", olinks(1)%oflow(J), ",", olinks(2)%odepth(J), ",", olinks(1)%ovolume(J)
!      end do
      open(8, file='swmm5fout.txt', status='replace')
      write(8,*) '****************************'
      write(8,*) '*      Node Outputs        *'
      write(8,*) '****************************'
      write(8,'(1A5,1A15,3A20)') 'Node', 'Date', 'Flow(cfs)', 'Depth(ft)', 'Volume(ft3)'
      do lN=1, NNODE
        do J=1, OutputSize
          if (TSDateTime(J) > 0.0 .or. &
             &onodes(lN)%oflow(J) > 0.0 .or. &
             &onodes(lN)%odepth(J) > 0.0 .or. &
             &onodes(lN)%ovolume(J) > 0.0) then
             write(8,'(1I5,1F15.5,3F20.5)') lN, TSDateTime(J), onodes(lN)%oflow(J), onodes(lN)%odepth(J), onodes(lN)%ovolume(J)
          end if
        end do
      end do
      write(8,*) ''
      write(8,*) '****************************'
      write(8,*) '*      Link Outputs        *'
      write(8,*) '****************************'
      write(8,'(1A5,1A15,3A20)') 'Link', 'Date', 'Flow(cfs)', 'Depth(ft)', 'Volume(ft3)'
      do lN=1, NCOND
        do J=1, OutputSize
          if (TSDateTime(J) > 0 .or. &
             &olinks(lN)%oflow(J) > 0 .or. &
             &olinks(lN)%odepth(J) > 0 .or. &
             &olinks(lN)%ovolume(J) > 0) then
             write(8,'(1I5,1F15.5,3F20.5)') lN, TSDateTime(J), olinks(lN)%oflow(J), olinks(lN)%odepth(J), olinks(lN)%ovolume(J)
          end if
        end do
      end do
      close(8)
      call output_close
      STOP
!
!     have to figure out how to get the output back here
!      O = OS
!      RO = ROS
!      VOL = VOLT
!      OVOL = 0.0
!      ROVOL = OVOL
end subroutine TongMain
