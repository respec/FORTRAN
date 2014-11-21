module enums
implicit none
!-----------------------------------------------------------------------------
!   enums.h !TZ: should be combined with const.f95
!
!   Project: EPA SWMM5
!   Version: 5.0
!   Date:    6/19/07   (Build 5.0.010)
!            2/8/08    (Build 5.0.012)
!            3/11/08   (Build 5.0.013)
!            1/21/09   (Build 5.0.014)
!            6/22/09   (Build 5.0.016)
!            07/30/10  (Build 5.0.019)
!   Author:  L. Rossman
!
!   Enumerated variables
!-----------------------------------------------------------------------------
private K2  !, K4, K8
integer, parameter :: K2 = selected_int_kind(2) !kind= 1
!integer, parameter :: K4 = selected_int_kind(4) !kind= 2
!integer, parameter :: K8 = selected_int_kind(8)  !kind =4

public
!-------------------------------------
! Names of major object types
!-------------------------------------
!enum ObjectType {

integer(kind=K2), parameter :: E_GAGE = 1            ! rain gage
integer(kind=K2), parameter :: E_SUBCATCH = 2        ! subcatchment
integer(kind=K2), parameter :: E_NODE = 3            ! conveyance system node
integer(kind=K2), parameter :: LINK = 4            ! conveyance system link
integer(kind=K2), parameter :: E_POLLUT = 5          ! pollutant
integer(kind=K2), parameter :: E_LANDUSE = 6         ! land use category
integer(kind=K2), parameter :: TIMEPATTERN = 7     ! dry weather flow time pattern
integer(kind=K2), parameter :: E_CURVE = 8           ! generic table of values
integer(kind=K2), parameter :: E_TSERIES = 9         ! generic time series of values
integer(kind=K2), parameter :: CONTROL = 10         ! conveyance system control rules
integer(kind=K2), parameter :: E_TRANSECT = 11       ! irregular channel cross-section
integer(kind=K2), parameter :: E_AQUIFER = 12        ! groundwater aquifer
integer(kind=K2), parameter :: E_UNITHYD = 13        ! RDII unit hydrograph
integer(kind=K2), parameter :: E_SNOWMELT = 14       ! snowmelt parameter set
integer(kind=K2), parameter :: E_SHAPE = 15          ! custom conduit shape                 !(5.0.010 - LR)
integer(kind=K2), parameter :: LID = 16            ! LID treatment units                  !(5.0.019 - LR)
integer(kind=K2), parameter :: MAX_OBJ_TYPES = 17  !(5.0.019 - LR)
!     };                                                          

!-------------------------------------
! Names of Node sub-types
!-------------------------------------
integer(kind=K2), parameter :: MAX_NODE_TYPES = 4
!enum NodeType {
integer(kind=K2), parameter :: JUNCTION = 1
integer(kind=K2), parameter :: E_OUTFALL = 2
integer(kind=K2), parameter :: E_STORAGE = 3
integer(kind=K2), parameter :: E_DIVIDER = 4
!     };

!-------------------------------------
! Names of Link sub-types
!-------------------------------------
integer(kind=K2), parameter :: MAX_LINK_TYPES = 5
!enum LinkType {
integer(kind=K2), parameter :: E_CONDUIT = 1
integer(kind=K2), parameter :: E_PUMP = 2
integer(kind=K2), parameter :: E_ORIFICE = 3
integer(kind=K2), parameter :: E_WEIR = 4
integer(kind=K2), parameter :: E_OUTLET = 5
!     };

!-------------------------------------
! File types
!-------------------------------------
!enum FileType {
integer(kind=K2), parameter :: RAINFALL_FILE = 1    ! rainfall file
integer(kind=K2), parameter :: RUNOFF_FILE = 2      ! runoff file
integer(kind=K2), parameter :: HOTSTART_FILE = 3    ! hotstart file
integer(kind=K2), parameter :: RDII_FILE = 4        ! RDII file
integer(kind=K2), parameter :: INFLOWS_FILE = 5     ! inflows interface file
integer(kind=K2), parameter :: OUTFLOWS_FILE = 6    ! outflows interface file
!     };

!-------------------------------------
! File usage types
!-------------------------------------
!enum FileUsageType {
integer(kind=K2), parameter :: NO_FILE = 1       ! no file usage
integer(kind=K2), parameter :: SCRATCH_FILE = 2  ! use temporary scratch file
integer(kind=K2), parameter :: USE_FILE = 3      ! use previously saved file
integer(kind=K2), parameter :: SAVE_FILE = 4     ! save file currently in use
!     };

!-------------------------------------
! Rain gage data types
!-------------------------------------
!enum GageDataType {
integer(kind=K2), parameter :: RAIN_TSERIES = 1  ! rainfall from user-supplied time series
integer(kind=K2), parameter :: RAIN_FILE = 2     ! rainfall from external file
!     };

!-------------------------------------
! Cross section shape types
!-------------------------------------
!enum XsectType {
integer(kind=K2), parameter :: DUMMY = 1            ! 0      
integer(kind=K2), parameter :: CIRCULAR = 2         ! 1      closed
integer(kind=K2), parameter :: FILLED_CIRCULAR = 3  ! 2      closed
integer(kind=K2), parameter :: RECT_CLOSED = 4      ! 3      closed
integer(kind=K2), parameter :: RECT_OPEN = 5        ! 4       
integer(kind=K2), parameter :: TRAPEZOIDAL = 6      ! 5       
integer(kind=K2), parameter :: TRIANGULAR = 7       ! 6       
integer(kind=K2), parameter :: PARABOLIC = 8        ! 7
integer(kind=K2), parameter :: POWERFUNC = 9        ! 8      
integer(kind=K2), parameter :: RECT_TRIANG = 10      ! 9       
integer(kind=K2), parameter :: RECT_ROUND = 11      ! 10
integer(kind=K2), parameter :: MOD_BASKET = 12      ! 11      
integer(kind=K2), parameter :: HORIZ_ELLIPSE = 13   ! 12     closed
integer(kind=K2), parameter :: VERT_ELLIPSE = 14    ! 13     closed
integer(kind=K2), parameter :: ARCH = 15            ! 14     closed
integer(kind=K2), parameter :: EGGSHAPED = 16       ! 15     closed
integer(kind=K2), parameter :: HORSESHOE = 17       ! 16     closed
integer(kind=K2), parameter :: GOTHIC = 18          ! 17     closed
integer(kind=K2), parameter :: CATENARY = 19        ! 18     closed
integer(kind=K2), parameter :: SEMIELLIPTICAL = 20  ! 19     closed
integer(kind=K2), parameter :: BASKETHANDLE = 21    ! 20     closed
integer(kind=K2), parameter :: SEMICIRCULAR = 22    ! 21     closed
integer(kind=K2), parameter :: IRREGULAR = 23       ! 22
integer(kind=K2), parameter :: CUSTOM = 24          ! 23     closed    !(5.0.010 - LR)
integer(kind=K2), parameter :: FORCE_MAIN = 25      ! 24     closed    !(5.0.010 - LR)
!     };

!-------------------------------------
! Measurement units types
!-------------------------------------
!enum UnitsType {
integer(kind=K2), parameter :: US = 1    ! US units
integer(kind=K2), parameter :: SI = 2    ! SI (metric) units
!     };

!enum FlowUnitsType {
integer(kind=K2), parameter :: CFS = 1   ! cubic feet per second
integer(kind=K2), parameter :: GPM = 2   ! gallons per minute
integer(kind=K2), parameter :: MGD = 3   ! million gallons per day
integer(kind=K2), parameter :: CMS = 4   ! cubic meters per second
integer(kind=K2), parameter :: LPS = 5   ! liters per second
integer(kind=K2), parameter :: MLD = 6   ! million liters per day
!     };

!enum ConcUnitsType {
integer(kind=K2), parameter :: MG = 1      ! Milligrams / L
integer(kind=K2), parameter :: UG = 2      ! Micrograms / L
integer(kind=K2), parameter :: E_COUNT = 3  ! Counts / L
!     };

!--------------------------------------
! Quantities requiring unit conversions
!--------------------------------------
!enum ConversionType {
integer(kind=K2), parameter :: RAINFALL = 1
integer(kind=K2), parameter :: RAINDEPTH = 2
integer(kind=K2), parameter :: EVAPRATE = 3
integer(kind=K2), parameter :: LENGTH = 4
integer(kind=K2), parameter :: LANDAREA = 5
integer(kind=K2), parameter :: VOLUME = 6
integer(kind=K2), parameter :: WINDSPEED = 7
integer(kind=K2), parameter :: TEMPERATURE = 8
integer(kind=K2), parameter :: MASS = 9
integer(kind=K2), parameter :: GWFLOW = 10        !(5.0.010 - LR)
integer(kind=K2), parameter :: FLOW = 11         ! Flow must always be listed last
!     };

!-------------------------------------
! Computed subcatchment quantities
!-------------------------------------
!#define MAX_SUBCATCH_RESULTS 7
integer(kind=K2), parameter :: MAX_SUBCATCH_RESULTS = 7
!enum SubcatchResultType {
integer(kind=K2), parameter :: SUBCATCH_RAINFALL = 1    ! rainfall intensity
integer(kind=K2), parameter :: SUBCATCH_SNOWDEPTH = 2   ! snow depth
integer(kind=K2), parameter :: SUBCATCH_LOSSES = 3      ! total losses (evap + infil)
integer(kind=K2), parameter :: SUBCATCH_RUNOFF = 4      ! runoff flow rate
integer(kind=K2), parameter :: SUBCATCH_GW_FLOW = 5     ! groundwater flow rate to node
integer(kind=K2), parameter :: SUBCATCH_GW_ELEV = 6     ! elevation of saturated gw table
integer(kind=K2), parameter :: SUBCATCH_WASHOFF = 7     ! pollutant washoff concentration
!     };

!-------------------------------------
! Computed node quantities
!-------------------------------------
!#define MAX_NODE_RESULTS 7 
integer(kind=K2), parameter :: MAX_NODE_RESULTS = 7 
!enum NodeResultType {
integer(kind=K2), parameter :: NODE_DEPTH = 1      ! water depth above invert
integer(kind=K2), parameter :: NODE_HEAD = 2       ! hydraulic head
integer(kind=K2), parameter :: NODE_VOLUME = 3     ! volume stored & ponded
integer(kind=K2), parameter :: NODE_LATFLOW = 4    ! lateral inflow rate
integer(kind=K2), parameter :: NODE_INFLOW = 5     ! total inflow rate
integer(kind=K2), parameter :: NODE_OVERFLOW = 6   ! overflow rate
integer(kind=K2), parameter :: NODE_QUAL = 7       ! concentration of each pollutant
!     };

!-------------------------------------
! Computed link quantities
!-------------------------------------
!#define MAX_LINK_RESULTS 6
integer(kind=K2), parameter :: MAX_LINK_RESULTS = 6
!enum LinkResultType {
integer(kind=K2), parameter :: LINK_FLOW = 1       ! flow rate
integer(kind=K2), parameter :: LINK_DEPTH = 2      ! flow depth
integer(kind=K2), parameter :: LINK_VELOCITY = 3   ! flow velocity
integer(kind=K2), parameter :: LINK_FROUDE = 4     ! Froude number
integer(kind=K2), parameter :: LINK_CAPACITY = 5   ! ratio of depth to full depth
integer(kind=K2), parameter :: LINK_QUAL = 6       ! concentration of each pollutant
!     };

!-------------------------------------
! System-wide flow quantities
!-------------------------------------
!#define MAX_SYS_RESULTS 14                                                     !(5.0.016 - LR)
integer(kind=K2), parameter :: MAX_SYS_RESULTS = 14   !(5.0.016 - LR)
!enum SysFlowType {
integer(kind=K2), parameter :: SYS_TEMPERATURE = 1   ! air temperature
integer(kind=K2), parameter :: SYS_RAINFALL = 2      ! rainfall intensity
integer(kind=K2), parameter :: SYS_SNOWDEPTH = 3     ! snow depth
integer(kind=K2), parameter :: SYS_LOSSES = 4        ! evap + infil
integer(kind=K2), parameter :: SYS_RUNOFF = 5        ! runoff flow
integer(kind=K2), parameter :: SYS_DWFLOW = 6        ! dry weather inflow
integer(kind=K2), parameter :: SYS_GWFLOW = 7        ! ground water inflow
integer(kind=K2), parameter :: SYS_IIFLOW = 8        ! RDII inflow
integer(kind=K2), parameter :: SYS_EXFLOW = 9        ! external inflow
integer(kind=K2), parameter :: SYS_INFLOW = 10        ! total lateral inflow
integer(kind=K2), parameter :: SYS_FLOODING = 11     ! flooding outflow
integer(kind=K2), parameter :: SYS_OUTFLOW = 12      ! outfall outflow
integer(kind=K2), parameter :: SYS_STORAGE = 13      ! storage volume
integer(kind=K2), parameter :: SYS_EVAPORATION = 14  ! evaporation                          !(5.0.016 - LR)
!    };

!-------------------------------------
! Conduit flow classifications
!-------------------------------------
!#define MAX_FLOW_CLASSES 7
integer(kind=K2), parameter :: MAX_FLOW_CLASSES = 7
!enum FlowClassType {
integer(kind=K2), parameter :: DRY = 1              ! dry conduit
integer(kind=K2), parameter :: UP_DRY = 2           ! upstream end is dry
integer(kind=K2), parameter :: DN_DRY = 3           ! downstream end is dry
integer(kind=K2), parameter :: SUBCRITICAL = 4      ! sub-critical flow
integer(kind=K2), parameter :: SUPCRITICAL = 5      ! super-critical flow
integer(kind=K2), parameter :: UP_CRITICAL = 6      ! free-fall at upstream end
integer(kind=K2), parameter :: DN_CRITICAL = 7      ! free-fall at downstream end
!     };

!-------------------------------------
! Surface pollutant loading categories
!-------------------------------------
!enum LoadingType {
integer(kind=K2), parameter :: BUILDUP_LOAD = 1      ! pollutant buildup load
integer(kind=K2), parameter :: DEPOSITION_LOAD = 2   ! rainfall deposition load
integer(kind=K2), parameter :: SWEEPING_LOAD = 3     ! load removed by sweeping
integer(kind=K2), parameter :: BMP_REMOVAL_LOAD = 4  ! load removed by BMPs
integer(kind=K2), parameter :: INFIL_LOAD = 5        ! runon load removed by infiltration
integer(kind=K2), parameter :: RUNOFF_LOAD = 6       ! load removed by runoff
!     };

!-------------------------------------
! Input data options
!-------------------------------------
!enum RainfallType {
integer(kind=K2), parameter :: RAINFALL_INTENSITY = 1   ! rainfall expressed as intensity
integer(kind=K2), parameter :: RAINFALL_VOLUME = 2      ! rainfall expressed as volume
integer(kind=K2), parameter :: CUMULATIVE_RAINFALL = 3  ! rainfall expressed as cumulative volume
!     };

!enum TempType {
integer(kind=K2), parameter :: NO_TEMP = 1       ! no temperature data supplied
integer(kind=K2), parameter :: TSERIES_TEMP = 2  ! temperatures come from time series
integer(kind=K2), parameter :: FILE_TEMP = 3     ! temperatures come from file
!     };

!enum  WindType {
integer(kind=K2), parameter :: MONTHLY_WIND = 1  ! wind speed varies by month
integer(kind=K2), parameter :: FILE_WIND = 2     ! wind speed comes from file
!     };

!enum EvapType {
integer(kind=K2), parameter :: CONSTANT_EVAP = 1    ! constant evaporation rate
integer(kind=K2), parameter :: MONTHLY_EVAP = 2     ! evaporation rate varies by month
integer(kind=K2), parameter :: TIMESERIES_EVAP = 3  ! evaporation supplied by time series
integer(kind=K2), parameter :: TEMPERATURE_EVAP = 4 ! evaporation from daily temperature   !(5.0.016 - LR)
integer(kind=K2), parameter :: FILE_EVAP = 5        ! evaporation comes from file
integer(kind=K2), parameter :: RECOVERY = 6         ! soil recovery pattern                !(5.0.014 - LR)
integer(kind=K2), parameter :: DRYONLY = 7          ! evap. allowed only in dry periods    !(5.0.019 - LR)
!     };

!enum NormalizerType {
integer(kind=K2), parameter :: PER_AREA = 1  ! buildup is per unit or area
integer(kind=K2), parameter :: PER_CURB = 2  ! buildup is per unit of curb length
!     };


!enum BuildupType {
integer(kind=K2), parameter :: NO_BUILDUP = 1       ! no buildup
integer(kind=K2), parameter :: POWER_BUILDUP = 2    ! power function buildup equation
integer(kind=K2), parameter :: EXPON_BUILDUP = 3    ! exponential function buildup equation
integer(kind=K2), parameter :: SATUR_BUILDUP = 4    ! saturation function buildup equation
integer(kind=K2), parameter :: EXTERNAL_BUILDUP = 5 ! external time series buildup         !(5.0.019 - LR)
!     };

!enum WashoffType {
integer(kind=K2), parameter :: NO_WASHOFF = 1       ! no washoff
integer(kind=K2), parameter :: EXPON_WASHOFF = 2    ! exponential washoff equation
integer(kind=K2), parameter :: RATING_WASHOFF = 3   ! rating curve washoff equation
integer(kind=K2), parameter :: EMC_WASHOFF = 4      ! event mean concentration washoff
!     };

 !!  InfilType now defined in infil.h  !!                                  !(5.0.019 - LR)

!enum  SubAreaType {
integer(kind=K2), parameter :: IMPERV0 = 1    ! impervious w/o depression storage
integer(kind=K2), parameter :: IMPERV1 = 2    ! impervious w/ depression storage
integer(kind=K2), parameter :: PERV = 3       ! pervious
!     };

!enum RunoffRoutingType {
integer(kind=K2), parameter :: TO_OUTLET = 1  ! perv & imperv runoff goes to outlet
integer(kind=K2), parameter :: TO_IMPERV = 2  ! perv runoff goes to imperv area
integer(kind=K2), parameter :: TO_PERV = 3    ! imperv runoff goes to perv subarea
!     };

!enum RouteModelType {
integer(kind=K2), parameter :: NO_ROUTING = 1 ! no routing                           !(5.0.010 - LR)
integer(kind=K2), parameter :: SF = 2         ! steady flow model
integer(kind=K2), parameter :: KW = 3         ! kinematic wave model
integer(kind=K2), parameter :: EKW = 4        ! extended kin. wave model
integer(kind=K2), parameter :: DW = 5         ! dynamic wave model
!     };

!enum ForceMainType {                                                          !(5.0.010 - LR)
integer(kind=K2), parameter :: H_W = 1  ! Hazen-Williams eqn.                  !(5.0.010 - LR)
integer(kind=K2), parameter :: D_W = 2  ! Darcy-Weisbach eqn.                  !(5.0.010 - LR)
!     };

!enum OffsetType {                                                             !(5.0.012 - LR)
integer(kind=K2), parameter :: DEPTH_OFFSET = 1  ! offset measured as depth             !(5.0.012 - LR)
integer(kind=K2), parameter :: ELEV_OFFSET = 2   ! offset measured as elevation         !(5.0.012 - LR)

!enum KinWaveMethodType {
integer(kind=K2), parameter :: NORMAL = 1   ! normal method
integer(kind=K2), parameter :: MODIFIED = 2 ! modified method

!enum CompatibilityType {
integer(kind=K2), parameter :: SWMM5 = 1    ! SWMM 5 weighting for area & hyd. radius
integer(kind=K2), parameter :: SWMM3 = 2    ! SWMM 3 weighting
integer(kind=K2), parameter :: SWMM4 = 3    ! SWMM 4 weighting

!enum NormalFlowType {                                                         !(5.0.010 - LR)
integer(kind=K2), parameter :: SLOPE = 1    ! based on slope only                  !(5.0.010 - LR)
integer(kind=K2), parameter :: FROUDE = 2   ! based on Fr only                     !(5.0.010 - LR)
integer(kind=K2), parameter :: BOTH = 3     ! based on slope & Fr                  !(5.0.010 - LR)

!enum InertialDampingType {
integer(kind=K2), parameter :: NO_DAMPING = 1      ! no inertial damping                  !(5.0.013 - LR)
integer(kind=K2), parameter :: PARTIAL_DAMPING = 2 ! partial damping                      !(5.0.013 - LR)
integer(kind=K2), parameter :: FULL_DAMPING = 3    ! full damping                         !(5.0.013 - LR) 

!enum InflowType {
integer(kind=K2), parameter :: EXTERNAL_INFLOW = 1     ! user-supplied external inflow
integer(kind=K2), parameter :: DRY_WEATHER_INFLOW = 2  ! user-supplied dry weather inflow
integer(kind=K2), parameter :: WET_WEATHER_INFLOW = 3  ! computed runoff inflow
integer(kind=K2), parameter :: GROUNDWATER_INFLOW = 4  ! computed groundwater inflow
integer(kind=K2), parameter :: RDII_INFLOW = 5         ! computed I&I inflow
integer(kind=K2), parameter :: FLOW_INFLOW = 6         ! inflow parameter is flow
integer(kind=K2), parameter :: CONCEN_INFLOW = 7       ! inflow parameter is pollutant concen.
integer(kind=K2), parameter :: MASS_INFLOW = 8         ! inflow parameter is pollutant mass

!enum PatternType {
integer(kind=K2), parameter :: MONTHLY_PATTERN = 1  ! DWF multipliers for each month
integer(kind=K2), parameter :: DAILY_PATTERN = 2    ! DWF multipliers for each day of week
integer(kind=K2), parameter :: HOURLY_PATTERN = 3   ! DWF multipliers for each hour of day
integer(kind=K2), parameter :: WEEKEND_PATTERN = 4  ! hourly multipliers for week end days

!enum OutfallType {
integer(kind=K2), parameter :: FREE_OUTFALL = 1        ! critical depth outfall condition
integer(kind=K2), parameter :: NORMAL_OUTFALL = 2      ! normal flow depth outfall condition
integer(kind=K2), parameter :: FIXED_OUTFALL = 3       ! fixed depth outfall condition
integer(kind=K2), parameter :: TIDAL_OUTFALL = 4       ! variable tidal stage outfall condition
integer(kind=K2), parameter :: TIMESERIES_OUTFALL = 5  ! variable time series outfall depth

!enum StorageType {
integer(kind=K2), parameter :: TABULAR = 1    ! area v. depth from table
integer(kind=K2), parameter :: FUNCTIONAL = 2 ! area v. depth from power function

!enum ReactorType {
integer(kind=K2), parameter :: CSTR = 1 ! completely mixed reactor
integer(kind=K2), parameter :: PLUG = 2 ! plug flow reactor

!enum TreatmentType {
integer(kind=K2), parameter :: REMOVAL = 1 ! treatment stated as a removal
integer(kind=K2), parameter :: CONCEN = 2  ! treatment stated as effluent concen.

!enum DividerType {
integer(kind=K2), parameter :: CUTOFF_DIVIDER = 1   ! diverted flow is excess of cutoff flow
integer(kind=K2), parameter :: TABULAR_DIVIDER = 2  ! table of diverted flow v. inflow
integer(kind=K2), parameter :: WEIR_DIVIDER = 3     ! diverted flow proportional to excess flow
integer(kind=K2), parameter :: OVERFLOW_DIVIDER = 4 ! diverted flow is flow > full conduit flow

!enum PumpCurveType {
integer(kind=K2), parameter :: TYPE1_PUMP = 1  ! flow varies stepwise with wet well volume
integer(kind=K2), parameter :: TYPE2_PUMP = 2  ! flow varies stepwise with inlet depth 
integer(kind=K2), parameter :: TYPE3_PUMP = 3  ! flow varies with head delivered
integer(kind=K2), parameter :: TYPE4_PUMP = 4  ! flow varies with inlet depth
integer(kind=K2), parameter :: IDEAL_PUMP = 5  ! outflow equals inflow                !(5.0.010 - LR)

!enum OrificeType {
integer(kind=K2), parameter :: SIDE_ORIFICE = 1    ! side orifice
integer(kind=K2), parameter :: BOTTOM_ORIFICE = 2  ! bottom orifice

!enum WeirType {
integer(kind=K2), parameter :: TRANSVERSE_WEIR = 1   ! transverse weir
integer(kind=K2), parameter :: SIDEFLOW_WEIR = 2     ! side flow weir
integer(kind=K2), parameter :: VNOTCH_WEIR = 3       ! V-notch (triangular) weir
integer(kind=K2), parameter :: TRAPEZOIDAL_WEIR = 4  ! trapezoidal weir

!enum CurveType {
integer(kind=K2), parameter :: STORAGE_CURVE = 1     ! surf. area v. depth for storage node
integer(kind=K2), parameter :: DIVERSION_CURVE = 2   ! diverted flow v. inflow for divider node
integer(kind=K2), parameter :: TIDAL_CURVE = 3       ! water elev. v. hour of day for outfall
integer(kind=K2), parameter :: RATING_CURVE = 4      ! flow rate v. head for outlet link
integer(kind=K2), parameter :: CONTROL_CURVE = 5     ! control setting v. controller variable
integer(kind=K2), parameter :: SHAPE_CURVE = 6       ! width v. depth for custom x-section  !(5.0.010 - LR)
integer(kind=K2), parameter :: PUMP1_CURVE = 7       ! flow v. wet well volume for pump
integer(kind=K2), parameter :: PUMP2_CURVE = 8       ! flow v. depth for pump (discrete)
integer(kind=K2), parameter :: PUMP3_CURVE = 9       ! flow v. head for pump (continuous)
integer(kind=K2), parameter :: PUMP4_CURVE = 10       ! flow v. depth for pump (continuous)

!enum InputSectionType {
integer(kind=K2), parameter :: s_TITLE = 1
integer(kind=K2), parameter :: s_OPTION = 2
integer(kind=K2), parameter :: s_FILE = 3
integer(kind=K2), parameter :: s_RAINGAGE = 4
integer(kind=K2), parameter :: s_TEMP = 5
integer(kind=K2), parameter :: s_EVAP = 5
integer(kind=K2), parameter :: s_SUBCATCH = 7
integer(kind=K2), parameter :: s_SUBAREA = 8
integer(kind=K2), parameter :: s_INFIL = 9
integer(kind=K2), parameter :: s_AQUIFER = 10
integer(kind=K2), parameter :: s_GROUNDWATER = 11
integer(kind=K2), parameter :: s_SNOWMELT = 12
integer(kind=K2), parameter :: s_JUNCTION = 13
integer(kind=K2), parameter :: s_OUTFALL = 14
integer(kind=K2), parameter :: s_STORAGE = 15
integer(kind=K2), parameter :: s_DIVIDER = 16
integer(kind=K2), parameter :: s_CONDUIT = 17
integer(kind=K2), parameter :: s_PUMP = 18
integer(kind=K2), parameter :: s_ORIFICE = 19
integer(kind=K2), parameter :: s_WEIR = 20
integer(kind=K2), parameter :: s_OUTLET = 21
integer(kind=K2), parameter :: s_XSECTION = 22
integer(kind=K2), parameter :: s_TRANSECT = 23
integer(kind=K2), parameter :: s_LOSSES = 24
integer(kind=K2), parameter :: s_CONTROL = 25
integer(kind=K2), parameter :: s_POLLUTANT = 26
integer(kind=K2), parameter :: s_LANDUSE = 27
integer(kind=K2), parameter :: s_BUILDUP = 28
integer(kind=K2), parameter :: s_WASHOFF = 29
integer(kind=K2), parameter :: s_COVERAGE = 30
integer(kind=K2), parameter :: s_INFLOW = 31
integer(kind=K2), parameter :: s_DWF = 32
integer(kind=K2), parameter :: s_PATTERN = 33
integer(kind=K2), parameter :: s_RDII = 34
integer(kind=K2), parameter :: s_UNITHYD = 35
integer(kind=K2), parameter :: s_LOADING = 36
integer(kind=K2), parameter :: s_TREATMENT = 37
integer(kind=K2), parameter :: s_CURVE = 38
integer(kind=K2), parameter :: s_TIMESERIES = 39
integer(kind=K2), parameter :: s_REPORT = 40
integer(kind=K2), parameter :: s_COORDINATE = 41
integer(kind=K2), parameter :: s_VERTICES = 42
integer(kind=K2), parameter :: s_POLYGON = 43
integer(kind=K2), parameter :: s_LABEL = 44
integer(kind=K2), parameter :: s_SYMBOL = 45
integer(kind=K2), parameter :: s_BACKDROP = 46
integer(kind=K2), parameter :: s_TAG = 47
integer(kind=K2), parameter :: s_PROFILE = 48
integer(kind=K2), parameter :: s_MAP = 49
integer(kind=K2), parameter :: s_LID_CONTROL = 50
integer(kind=K2), parameter :: s_LID_USAGE = 51     !(5.0.019 - LR)

!enum InputOptionType {
integer(kind=K2), parameter :: FLOW_UNITS = 1
integer(kind=K2), parameter :: INFIL_MODEL = 2
integer(kind=K2), parameter :: ROUTE_MODEL = 3
integer(kind=K2), parameter :: START_DATE = 4
integer(kind=K2), parameter :: START_TIME = 5
integer(kind=K2), parameter :: END_DATE = 6
integer(kind=K2), parameter :: END_TIME = 7
integer(kind=K2), parameter :: REPORT_START_DATE = 8
integer(kind=K2), parameter :: REPORT_START_TIME = 9
integer(kind=K2), parameter :: SWEEP_START = 10
integer(kind=K2), parameter :: SWEEP_END = 11
integer(kind=K2), parameter :: START_DRY_DAYS = 12
integer(kind=K2), parameter :: WET_STEP = 13
integer(kind=K2), parameter :: DRY_STEP = 14
integer(kind=K2), parameter :: ROUTE_STEP = 15
integer(kind=K2), parameter :: REPORT_STEP = 16
integer(kind=K2), parameter :: ALLOW_PONDING = 17
integer(kind=K2), parameter :: INERT_DAMPING = 18
integer(kind=K2), parameter :: SLOPE_WEIGHTING = 19
integer(kind=K2), parameter :: VARIABLE_STEP = 20
integer(kind=K2), parameter :: NORMAL_FLOW_LTD = 21
integer(kind=K2), parameter :: LENGTHENING_STEP = 22
integer(kind=K2), parameter :: MIN_SURFAREA = 23
integer(kind=K2), parameter :: E_COMPATIBILITY = 24
integer(kind=K2), parameter :: SKIP_STEADY_STATE = 25
integer(kind=K2), parameter :: TEMPDIR = 26
integer(kind=K2), parameter :: IGNORE_RAINFALL = 27
integer(kind=K2), parameter :: FORCE_MAIN_EQN = 28
integer(kind=K2), parameter :: LINK_OFFSETS = 29
integer(kind=K2), parameter :: MIN_SLOPE = 30
integer(kind=K2), parameter :: IGNORE_SNOWMELT = 31
integer(kind=K2), parameter :: IGNORE_GWATER = 32
integer(kind=K2), parameter :: IGNORE_ROUTING = 33
integer(kind=K2), parameter :: IGNORE_QUALITY = 34

!enum  NoYesType {
integer(kind=K2), parameter :: NO = 1
integer(kind=K2), parameter :: YES = 2

!enum  NoneAllType {
integer(kind=K2), parameter :: E_NONE = 1
integer(kind=K2), parameter :: E_ALL = 2
integer(kind=K2), parameter :: SOME = 3
end module

