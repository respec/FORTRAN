program main
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
       
      integer, parameter :: NNODE = 3
      integer, parameter :: NCOND = 2
      integer :: J, LTYPE, ITS, NTS, DTS, DELTS
      integer :: k !subindex for each node type
      integer :: CNODE1, CNODE2
      logical :: isOK
      
      integer :: lErrorCode
      
      real :: ROVOL, OVOL, VOLT, VOL, ROS, OS, RO, O
      
      real, dimension(6) :: XN
      double precision, dimension(6) :: XC
      double precision, dimension(4) :: XX
      real, dimension(NNODE) :: NDINIT = (/0.05, 0.05, 0.016/)
      real, dimension(NNODE) :: NELEV = (/10.14, 10.14, 9.23/)
      real, dimension(NNODE) :: NDMAX = (/11.14, 11.14, 10.23/)
      real, dimension(NNODE) :: NDSURC = (/12.14, 12.14, 11.23/)
      real, dimension(NNODE) :: NPONDA = (/0, 0, 0/)

      real, dimension(NCOND) :: CLEN = (/10.14, 9.23/)
      real, dimension(NCOND) :: CMANN = (/0.05, 0.016/)
      real, dimension(NCOND) :: COFF1 = (/0.0, 0.0/)
      real, dimension(NCOND) :: COFF2 = (/0.0, 0.0/)
      real, dimension(NCOND) :: CQ0 = (/0.01, 0.01/)
      
      integer, dimension(NCOND) :: CSHAPE = (/TRAPEZOIDAL, CIRCULAR/)
      real, dimension(NCOND) :: CGEOM1 = (/3.0, 2.25/)
      real, dimension(NCOND) :: CGEOM2 = (/5.0, 0.0/)
      real, dimension(NCOND) :: CGEOM3 = (/5.0, 0.0/)
      real, dimension(NCOND) :: CGEOM4 = (/5.0, 0.0/)
      
      UnitSystem = US
      
      Nobjects(E_NODE) = 3
      Nnodes(E_OUTFALL) = 1
      Nobjects(LINK) = 2
      Nlinks(E_CONDUIT) = 2
      
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
           XN(2) = FIXED_OUTFALL !outfall type, the easiest at this point
           XN(3) = NDINIT(J) + NELEV(J) !fixedStage
           XN(4) = 0 !tideCurve, index of tidal stage curve
           XN(5) = 0 !stageSeries, index of outfall stage time series
           XN(6) = 0 !hasFlapGate, true(ie 1) if contains flap gate, false(ie 0) no
        end if
        call node_setParams(J, LTYPE, k, XN)
        Node(J)%rptFlag = .true. !this is was done in report_readoption
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
        
        !hard code nodal schema
        if (J == 1) then
           CNODE1 = 1
           CNODE2 = 2
        else
           CNODE1 = 2
           CNODE2 = 3
        end if
        call link_setParams(J, LTYPE, CNODE1, CNODE2, J, XC)
        XX(1) = CGEOM1(J)
        XX(2) = CGEOM2(J)
        XX(3) = CGEOM3(J)
        XX(4) = CGEOM4(J)
        
        !call xsect_setParams(J, CSHAPE(J), 1, XX, 0.0)   !haven't quite understood this yet, this is for outlet link
        isOK = xsect_setParams(arrLink(j)%xsect, CSHAPE(J), XX, UCF(LENGTH))   !for normal conduit
        arrLink(J)%rptFlag = .true. !this is was done in report_readoption
 20   CONTINUE

      !set options, ref: project.c->project_readOption()
      !the following could be incorporated into setDefaults() routine above
      
      RouteModel = DW
      StartDate = datetime_encodeDate(2013, 3, 12) !datetime_strToDate
      StartTime = datetime_encodeTime(3, 0, 0) !datetime_strToTime
      
      EndDate = datetime_encodeDate(2013, 3, 12) !datetime_strToDate
      EndTime = datetime_encodeTime(4, 0, 0) !datetime_strToTime
      
      !ReportStartDate = datetime_encodeDate(ryr, rmon, rday)
!      ReportStartTime = datetime_encodeTime(rhr, rmin, rsec)
      
      StartDryDays = 0 !number of antecedent dry days
      
      InertDamping = w_NONE !InertDampingWords, w_PARTIAL, w_FULL
      
      AllowPonding    = .true.
      SlopeWeighting  = .true.
      SkipSteadyState = .true.
      IgnoreRainfall  = .false.
      IgnoreSnowmelt  = .true.
      IgnoreGwater    = .true.
      IgnoreRouting   = .false.
      IgnoreQuality   = .true.
      
      NormalFlowLtd = SLOPE !NormalFlowType
      
      ForceMainEqn = D_W !ForceMainType
      
      LinkOffsets = DEPTH_OFFSET !OffsetType
      
      Compatibility = SWMM5
      !RouteStep is set to 5 minutes in setDefault()
      LengtheningStep = MAX(0.0, RouteStep) !if 0, then no lengthening of conduit for DW routing
      
      ! --- safety factor applied to variable time step estimates under
      !     dynamic wave flow routing (value of 0 indicates that variable
      !     time step option not used)
      CourantFactor = 0.0 !variable time step option not used, needs to be >= 0.0 and <= 2.0
      
      ! --- minimum surface area (ft2 or sq. meters) associated with nodes
      !     under dynamic wave flow routing 
      MinSurfArea = 0.0
      
      !minimum conduit slope, needs to be >= 0.0 and < 1; SWMM input file enters %
      MinSlope = 0.05
      
      TmpDir = 'C:\Temp'
      
      !the following settings are from link->link_readXsectParams(toks, ntoks)
      !supposed to be read from input files, but we will specify here as pertinent
      
      do J = 1, NCOND
         !assume each conduit has only one barrels
         Conduit(arrLink(J)%subIndex)%barrels = 1
         !--- assume link is not a culvert    !(5.0.014 - LR)
         arrLink(J)%xsect%culvertCode = 0
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

      lErrorCode = swmm_run('', '', '')
      
      do J=1, OutputSize
         write(*,*) TSDateTime(J), ",", TSOutletVals(J)
      end do
      STOP
!
!     have to figure out how to get the output back here
!      O = OS
!      RO = ROS
!      VOL = VOLT
!      OVOL = 0.0
!      ROVOL = OVOL
      
      

end program