include 'DataSizeSpecs.f95'
module enums
use DataSizeSpecs
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

!-------------------------------------
! Names of major object types
!-------------------------------------
!enum ObjectType {

integer(kind=K2), parameter :: E_GAGE = 0            ! rain gage
integer(kind=K2), parameter :: E_SUBCATCH = 1        ! subcatchment
integer(kind=K2), parameter :: E_NODE = 2            ! conveyance system node
integer(kind=K2), parameter :: LINK = 3            ! conveyance system link
integer(kind=K2), parameter :: E_POLLUT = 4          ! pollutant
integer(kind=K2), parameter :: E_LANDUSE = 5         ! land use category
integer(kind=K2), parameter :: TIMEPATTERN = 6     ! dry weather flow time pattern
integer(kind=K2), parameter :: E_CURVE = 7           ! generic table of values
integer(kind=K2), parameter :: E_TSERIES = 8         ! generic time series of values
integer(kind=K2), parameter :: CONTROL = 9         ! conveyance system control rules
integer(kind=K2), parameter :: E_TRANSECT = 10       ! irregular channel cross-section
integer(kind=K2), parameter :: E_AQUIFER = 11        ! groundwater aquifer
integer(kind=K2), parameter :: E_UNITHYD = 12        ! RDII unit hydrograph
integer(kind=K2), parameter :: E_SNOWMELT = 13       ! snowmelt parameter set
integer(kind=K2), parameter :: E_SHAPE = 14          ! custom conduit shape                 !(5.0.010 - LR)
integer(kind=K2), parameter :: LID = 15            ! LID treatment units                  !(5.0.019 - LR)
integer(kind=K2), parameter :: MAX_OBJ_TYPES = 16  !(5.0.019 - LR)
!     };                                                          

!-------------------------------------
! Names of Node sub-types
!-------------------------------------
integer(kind=K2), parameter :: MAX_NODE_TYPES = 4
!enum NodeType {
integer(kind=K2), parameter :: JUNCTION = 0
integer(kind=K2), parameter :: E_OUTFALL = 1
integer(kind=K2), parameter :: E_STORAGE = 2
integer(kind=K2), parameter :: E_DIVIDER = 3
!     };

!-------------------------------------
! Names of Link sub-types
!-------------------------------------
integer(kind=K2), parameter :: MAX_LINK_TYPES = 5
!enum LinkType {
integer(kind=K2), parameter :: E_CONDUIT = 0
integer(kind=K2), parameter :: E_PUMP = 1
integer(kind=K2), parameter :: E_ORIFICE = 2
integer(kind=K2), parameter :: E_WEIR = 3
integer(kind=K2), parameter :: E_OUTLET = 4
!     };

!-------------------------------------
! File types
!-------------------------------------
!enum FileType {
integer(kind=K2), parameter :: RAINFALL_FILE = 0    ! rainfall file
integer(kind=K2), parameter :: RUNOFF_FILE = 1      ! runoff file
integer(kind=K2), parameter :: HOTSTART_FILE = 2    ! hotstart file
integer(kind=K2), parameter :: RDII_FILE = 3        ! RDII file
integer(kind=K2), parameter :: INFLOWS_FILE = 4     ! inflows interface file
integer(kind=K2), parameter :: OUTFLOWS_FILE = 5    ! outflows interface file
!     };

!-------------------------------------
! File usage types
!-------------------------------------
!enum FileUsageType {
integer(kind=K2), parameter :: NO_FILE = 0       ! no file usage
integer(kind=K2), parameter :: SCRATCH_FILE = 1  ! use temporary scratch file
integer(kind=K2), parameter :: USE_FILE = 2      ! use previously saved file
integer(kind=K2), parameter :: SAVE_FILE = 3     ! save file currently in use
!     };

!-------------------------------------
! Rain gage data types
!-------------------------------------
!enum GageDataType {
integer(kind=K2), parameter :: RAIN_TSERIES = 0  ! rainfall from user-supplied time series
integer(kind=K2), parameter :: RAIN_FILE = 1     ! rainfall from external file
!     };

!-------------------------------------
! Cross section shape types
!-------------------------------------
!enum XsectType {
integer(kind=K2), parameter :: DUMMY = 0            ! 0      
integer(kind=K2), parameter :: CIRCULAR = 1         ! 1      closed
integer(kind=K2), parameter :: FILLED_CIRCULAR = 2  ! 2      closed
integer(kind=K2), parameter :: RECT_CLOSED = 3      ! 3      closed
integer(kind=K2), parameter :: RECT_OPEN = 4        ! 4       
integer(kind=K2), parameter :: TRAPEZOIDAL = 5      ! 5       
integer(kind=K2), parameter :: TRIANGULAR = 6       ! 6       
integer(kind=K2), parameter :: PARABOLIC = 7        ! 7
integer(kind=K2), parameter :: POWERFUNC = 8        ! 8      
integer(kind=K2), parameter :: RECT_TRIANG = 9      ! 9       
integer(kind=K2), parameter :: RECT_ROUND = 10      ! 10
integer(kind=K2), parameter :: MOD_BASKET = 11      ! 11      
integer(kind=K2), parameter :: HORIZ_ELLIPSE = 12   ! 12     closed
integer(kind=K2), parameter :: VERT_ELLIPSE = 13    ! 13     closed
integer(kind=K2), parameter :: ARCH = 14            ! 14     closed
integer(kind=K2), parameter :: EGGSHAPED = 15       ! 15     closed
integer(kind=K2), parameter :: HORSESHOE = 16       ! 16     closed
integer(kind=K2), parameter :: GOTHIC = 17          ! 17     closed
integer(kind=K2), parameter :: CATENARY = 18        ! 18     closed
integer(kind=K2), parameter :: SEMIELLIPTICAL = 19  ! 19     closed
integer(kind=K2), parameter :: BASKETHANDLE = 20    ! 20     closed
integer(kind=K2), parameter :: SEMICIRCULAR = 21    ! 21     closed
integer(kind=K2), parameter :: IRREGULAR = 22       ! 22
integer(kind=K2), parameter :: CUSTOM = 23          ! 23     closed    !(5.0.010 - LR)
integer(kind=K2), parameter :: FORCE_MAIN = 24      ! 24     closed    !(5.0.010 - LR)
!     };

!-------------------------------------
! Measurement units types
!-------------------------------------
!enum UnitsType {
integer(kind=K2), parameter :: US = 0    ! US units
integer(kind=K2), parameter :: SI = 1    ! SI (metric) units
!     };

!enum FlowUnitsType {
integer(kind=K2), parameter :: CFS = 0   ! cubic feet per second
integer(kind=K2), parameter :: GPM = 1   ! gallons per minute
integer(kind=K2), parameter :: MGD = 2   ! million gallons per day
integer(kind=K2), parameter :: CMS = 3   ! cubic meters per second
integer(kind=K2), parameter :: LPS = 4   ! liters per second
integer(kind=K2), parameter :: MLD = 5   ! million liters per day
!     };

!enum ConcUnitsType {
integer(kind=K2), parameter :: MG = 0      ! Milligrams / L
integer(kind=K2), parameter :: UG = 1      ! Micrograms / L
integer(kind=K2), parameter :: pCOUNT = 2  ! Counts / L
!     };

!--------------------------------------
! Quantities requiring unit conversions
!--------------------------------------
!enum ConversionType {
integer(kind=K2), parameter :: RAINFALL = 0
integer(kind=K2), parameter :: RAINDEPTH = 1
integer(kind=K2), parameter :: EVAPRATE = 2
integer(kind=K2), parameter :: LENGTH = 3
integer(kind=K2), parameter :: LANDAREA = 4
integer(kind=K2), parameter :: VOLUME = 5
integer(kind=K2), parameter :: WINDSPEED = 6
integer(kind=K2), parameter :: TEMPERATURE = 7
integer(kind=K2), parameter :: MASS = 8
integer(kind=K2), parameter :: GWFLOW = 9        !(5.0.010 - LR)
integer(kind=K2), parameter :: FLOW = 10         ! Flow must always be listed last
!     };

!-------------------------------------
! Computed subcatchment quantities
!-------------------------------------
!#define MAX_SUBCATCH_RESULTS 7
integer(kind=K2), parameter :: MAX_SUBCATCH_RESULTS = 7
!enum SubcatchResultType {
integer(kind=K2), parameter :: SUBCATCH_RAINFALL = 0    ! rainfall intensity
integer(kind=K2), parameter :: SUBCATCH_SNOWDEPTH = 1   ! snow depth
integer(kind=K2), parameter :: SUBCATCH_LOSSES = 2      ! total losses (evap + infil)
integer(kind=K2), parameter :: SUBCATCH_RUNOFF = 3      ! runoff flow rate
integer(kind=K2), parameter :: SUBCATCH_GW_FLOW = 4     ! groundwater flow rate to node
integer(kind=K2), parameter :: SUBCATCH_GW_ELEV = 5     ! elevation of saturated gw table
integer(kind=K2), parameter :: SUBCATCH_WASHOFF = 6     ! pollutant washoff concentration
!     };

!-------------------------------------
! Computed node quantities
!-------------------------------------
!#define MAX_NODE_RESULTS 7 
integer(kind=K2), parameter :: MAX_NODE_RESULTS = 7 
!enum NodeResultType {
integer(kind=K2), parameter :: NODE_DEPTH = 0      ! water depth above invert
integer(kind=K2), parameter :: NODE_HEAD = 1       ! hydraulic head
integer(kind=K2), parameter :: NODE_VOLUME = 2     ! volume stored & ponded
integer(kind=K2), parameter :: NODE_LATFLOW = 3    ! lateral inflow rate
integer(kind=K2), parameter :: NODE_INFLOW = 4     ! total inflow rate
integer(kind=K2), parameter :: NODE_OVERFLOW = 5   ! overflow rate
integer(kind=K2), parameter :: NODE_QUAL = 6       ! concentration of each pollutant
!     };

!-------------------------------------
! Computed link quantities
!-------------------------------------
!#define MAX_LINK_RESULTS 6
integer(kind=K2), parameter :: MAX_LINK_RESULTS = 6
!enum LinkResultType {
integer(kind=K2), parameter :: LINK_FLOW = 0       ! flow rate
integer(kind=K2), parameter :: LINK_DEPTH = 1      ! flow depth
integer(kind=K2), parameter :: LINK_VELOCITY = 2   ! flow velocity
integer(kind=K2), parameter :: LINK_FROUDE = 3     ! Froude number
integer(kind=K2), parameter :: LINK_CAPACITY = 4   ! ratio of depth to full depth
integer(kind=K2), parameter :: LINK_QUAL = 5       ! concentration of each pollutant
!     };

!-------------------------------------
! System-wide flow quantities
!-------------------------------------
!#define MAX_SYS_RESULTS 14                                                     !(5.0.016 - LR)
integer(kind=K2), parameter :: MAX_SYS_RESULTS = 14   !(5.0.016 - LR)
!enum SysFlowType {
integer(kind=K2), parameter :: SYS_TEMPERATURE = 0   ! air temperature
integer(kind=K2), parameter :: SYS_RAINFALL = 1      ! rainfall intensity
integer(kind=K2), parameter :: SYS_SNOWDEPTH = 2     ! snow depth
integer(kind=K2), parameter :: SYS_LOSSES = 3        ! evap + infil
integer(kind=K2), parameter :: SYS_RUNOFF = 4        ! runoff flow
integer(kind=K2), parameter :: SYS_DWFLOW = 5        ! dry weather inflow
integer(kind=K2), parameter :: SYS_GWFLOW = 6        ! ground water inflow
integer(kind=K2), parameter :: SYS_IIFLOW = 7        ! RDII inflow
integer(kind=K2), parameter :: SYS_EXFLOW = 8        ! external inflow
integer(kind=K2), parameter :: SYS_INFLOW = 9        ! total lateral inflow
integer(kind=K2), parameter :: SYS_FLOODING = 10     ! flooding outflow
integer(kind=K2), parameter :: SYS_OUTFLOW = 11      ! outfall outflow
integer(kind=K2), parameter :: SYS_STORAGE = 12      ! storage volume
integer(kind=K2), parameter :: SYS_EVAPORATION = 13  ! evaporation                          !(5.0.016 - LR)
!    };

!-------------------------------------
! Conduit flow classifications
!-------------------------------------
!#define MAX_FLOW_CLASSES 7
integer(kind=K2), parameter :: MAX_FLOW_CLASSES = 7
!enum FlowClassType {
integer(kind=K2), parameter :: DRY = 0              ! dry conduit
integer(kind=K2), parameter :: UP_DRY = 1           ! upstream end is dry
integer(kind=K2), parameter :: DN_DRY = 2           ! downstream end is dry
integer(kind=K2), parameter :: SUBCRITICAL = 3      ! sub-critical flow
integer(kind=K2), parameter :: SUPCRITICAL = 4      ! super-critical flow
integer(kind=K2), parameter :: UP_CRITICAL = 5      ! free-fall at upstream end
integer(kind=K2), parameter :: DN_CRITICAL = 6      ! free-fall at downstream end
!     };

!-------------------------------------
! Surface pollutant loading categories
!-------------------------------------
!enum LoadingType {
integer(kind=K2), parameter :: BUILDUP_LOAD = 0      ! pollutant buildup load
integer(kind=K2), parameter :: DEPOSITION_LOAD = 1   ! rainfall deposition load
integer(kind=K2), parameter :: SWEEPING_LOAD = 2     ! load removed by sweeping
integer(kind=K2), parameter :: BMP_REMOVAL_LOAD = 3  ! load removed by BMPs
integer(kind=K2), parameter :: INFIL_LOAD = 4        ! runon load removed by infiltration
integer(kind=K2), parameter :: RUNOFF_LOAD = 5       ! load removed by runoff
!     };

!-------------------------------------
! Input data options
!-------------------------------------
!enum RainfallType {
integer(kind=K2), parameter :: RAINFALL_INTENSITY = 0   ! rainfall expressed as intensity
integer(kind=K2), parameter :: RAINFALL_VOLUME = 1      ! rainfall expressed as volume
integer(kind=K2), parameter :: CUMULATIVE_RAINFALL = 2  ! rainfall expressed as cumulative volume
!     };

!enum TempType {
integer(kind=K2), parameter :: NO_TEMP = 0       ! no temperature data supplied
integer(kind=K2), parameter :: TSERIES_TEMP = 1  ! temperatures come from time series
integer(kind=K2), parameter :: FILE_TEMP = 2     ! temperatures come from file
!     };

!enum  WindType {
integer(kind=K2), parameter :: MONTHLY_WIND = 0  ! wind speed varies by month
integer(kind=K2), parameter :: FILE_WIND = 1     ! wind speed comes from file
!     };

!enum EvapType {
integer(kind=K2), parameter :: CONSTANT_EVAP = 0    ! constant evaporation rate
integer(kind=K2), parameter :: MONTHLY_EVAP = 1     ! evaporation rate varies by month
integer(kind=K2), parameter :: TIMESERIES_EVAP = 2  ! evaporation supplied by time series
integer(kind=K2), parameter :: TEMPERATURE_EVAP = 3 ! evaporation from daily temperature   !(5.0.016 - LR)
integer(kind=K2), parameter :: FILE_EVAP = 4        ! evaporation comes from file
integer(kind=K2), parameter :: RECOVERY = 5         ! soil recovery pattern                !(5.0.014 - LR)
integer(kind=K2), parameter :: DRYONLY = 6          ! evap. allowed only in dry periods    !(5.0.019 - LR)
!     };

!enum NormalizerType {
integer(kind=K2), parameter :: PER_AREA = 0  ! buildup is per unit or area
integer(kind=K2), parameter :: PER_CURB = 1  ! buildup is per unit of curb length
!     };


!enum BuildupType {
integer(kind=K2), parameter :: NO_BUILDUP = 0       ! no buildup
integer(kind=K2), parameter :: POWER_BUILDUP = 1    ! power function buildup equation
integer(kind=K2), parameter :: EXPON_BUILDUP = 2    ! exponential function buildup equation
integer(kind=K2), parameter :: SATUR_BUILDUP = 3    ! saturation function buildup equation
integer(kind=K2), parameter :: EXTERNAL_BUILDUP = 4 ! external time series buildup         !(5.0.019 - LR)
!     };

!enum WashoffType {
integer(kind=K2), parameter :: NO_WASHOFF = 0       ! no washoff
integer(kind=K2), parameter :: EXPON_WASHOFF = 1    ! exponential washoff equation
integer(kind=K2), parameter :: RATING_WASHOFF = 2   ! rating curve washoff equation
integer(kind=K2), parameter :: EMC_WASHOFF = 3      ! event mean concentration washoff
!     };

 !!  InfilType now defined in infil.h  !!                                  !(5.0.019 - LR)

!enum  SubAreaType {
integer(kind=K2), parameter :: IMPERV0 = 0    ! impervious w/o depression storage
integer(kind=K2), parameter :: IMPERV1 = 1    ! impervious w/ depression storage
integer(kind=K2), parameter :: PERV = 2       ! pervious
!     };

!enum RunoffRoutingType {
integer(kind=K2), parameter :: TO_OUTLET = 0  ! perv & imperv runoff goes to outlet
integer(kind=K2), parameter :: TO_IMPERV = 1  ! perv runoff goes to imperv area
integer(kind=K2), parameter :: TO_PERV = 2    ! imperv runoff goes to perv subarea
!     };

!enum RouteModelType {
integer(kind=K2), parameter :: NO_ROUTING = 0 ! no routing                           !(5.0.010 - LR)
integer(kind=K2), parameter :: SF = 1         ! steady flow model
integer(kind=K2), parameter :: KW = 2         ! kinematic wave model
integer(kind=K2), parameter :: EKW = 3        ! extended kin. wave model
integer(kind=K2), parameter :: DW = 4         ! dynamic wave model
!     };

!enum ForceMainType {                                                          !(5.0.010 - LR)
integer(kind=K2), parameter :: H_W = 0  ! Hazen-Williams eqn.                  !(5.0.010 - LR)
integer(kind=K2), parameter :: D_W = 1  ! Darcy-Weisbach eqn.                  !(5.0.010 - LR)
!     };

!enum OffsetType {                                                             !(5.0.012 - LR)
integer(kind=K2), parameter :: DEPTH_OFFSET = 0  ! offset measured as depth             !(5.0.012 - LR)
integer(kind=K2), parameter :: ELEV_OFFSET = 1   ! offset measured as elevation         !(5.0.012 - LR)

!enum KinWaveMethodType {
integer(kind=K2), parameter :: NORMAL = 0   ! normal method
integer(kind=K2), parameter :: MODIFIED = 1 ! modified method

!enum CompatibilityType {
integer(kind=K2), parameter :: SWMM5 = 0    ! SWMM 5 weighting for area & hyd. radius
integer(kind=K2), parameter :: SWMM3 = 1    ! SWMM 3 weighting
integer(kind=K2), parameter :: SWMM4 = 2    ! SWMM 4 weighting

!enum NormalFlowType {                                                         !(5.0.010 - LR)
integer(kind=K2), parameter :: SLOPE = 0    ! based on slope only                  !(5.0.010 - LR)
integer(kind=K2), parameter :: FROUDE = 1   ! based on Fr only                     !(5.0.010 - LR)
integer(kind=K2), parameter :: BOTH = 2     ! based on slope & Fr                  !(5.0.010 - LR)

!enum InertialDampingType {
integer(kind=K2), parameter :: NO_DAMPING = 0      ! no inertial damping                  !(5.0.013 - LR)
integer(kind=K2), parameter :: PARTIAL_DAMPING = 1 ! partial damping                      !(5.0.013 - LR)
integer(kind=K2), parameter :: FULL_DAMPING = 2    ! full damping                         !(5.0.013 - LR) 

!enum InflowType {
integer(kind=K2), parameter :: EXTERNAL_INFLOW = 0     ! user-supplied external inflow
integer(kind=K2), parameter :: DRY_WEATHER_INFLOW = 1  ! user-supplied dry weather inflow
integer(kind=K2), parameter :: WET_WEATHER_INFLOW = 2  ! computed runoff inflow
integer(kind=K2), parameter :: GROUNDWATER_INFLOW = 3  ! computed groundwater inflow
integer(kind=K2), parameter :: RDII_INFLOW = 4         ! computed I&I inflow
integer(kind=K2), parameter :: FLOW_INFLOW = 5         ! inflow parameter is flow
integer(kind=K2), parameter :: CONCEN_INFLOW = 6       ! inflow parameter is pollutant concen.
integer(kind=K2), parameter :: MASS_INFLOW = 7         ! inflow parameter is pollutant mass

!enum PatternType {
integer(kind=K2), parameter :: MONTHLY_PATTERN = 0  ! DWF multipliers for each month
integer(kind=K2), parameter :: DAILY_PATTERN = 1    ! DWF multipliers for each day of week
integer(kind=K2), parameter :: HOURLY_PATTERN = 2   ! DWF multipliers for each hour of day
integer(kind=K2), parameter :: WEEKEND_PATTERN = 3  ! hourly multipliers for week end days

!enum OutfallType {
integer(kind=K2), parameter :: FREE_OUTFALL = 0        ! critical depth outfall condition
integer(kind=K2), parameter :: NORMAL_OUTFALL = 1      ! normal flow depth outfall condition
integer(kind=K2), parameter :: FIXED_OUTFALL = 2       ! fixed depth outfall condition
integer(kind=K2), parameter :: TIDAL_OUTFALL = 3       ! variable tidal stage outfall condition
integer(kind=K2), parameter :: TIMESERIES_OUTFALL = 4  ! variable time series outfall depth

!enum StorageType {
integer(kind=K2), parameter :: TABULAR = 0    ! area v. depth from table
integer(kind=K2), parameter :: FUNCTIONAL = 1 ! area v. depth from power function

!enum ReactorType {
integer(kind=K2), parameter :: CSTR = 0 ! completely mixed reactor
integer(kind=K2), parameter :: PLUG = 1 ! plug flow reactor

!enum TreatmentType {
integer(kind=K2), parameter :: REMOVAL = 0 ! treatment stated as a removal
integer(kind=K2), parameter :: CONCEN = 1  ! treatment stated as effluent concen.

!enum DividerType {
integer(kind=K2), parameter :: CUTOFF_DIVIDER = 0   ! diverted flow is excess of cutoff flow
integer(kind=K2), parameter :: TABULAR_DIVIDER = 1  ! table of diverted flow v. inflow
integer(kind=K2), parameter :: WEIR_DIVIDER = 2     ! diverted flow proportional to excess flow
integer(kind=K2), parameter :: OVERFLOW_DIVIDER = 3 ! diverted flow is flow > full conduit flow

!enum PumpCurveType {
integer(kind=K2), parameter :: TYPE1_PUMP = 0  ! flow varies stepwise with wet well volume
integer(kind=K2), parameter :: TYPE2_PUMP = 1  ! flow varies stepwise with inlet depth 
integer(kind=K2), parameter :: TYPE3_PUMP = 2  ! flow varies with head delivered
integer(kind=K2), parameter :: TYPE4_PUMP = 3  ! flow varies with inlet depth
integer(kind=K2), parameter :: IDEAL_PUMP = 4  ! outflow equals inflow                !(5.0.010 - LR)

!enum OrificeType {
integer(kind=K2), parameter :: SIDE_ORIFICE = 0    ! side orifice
integer(kind=K2), parameter :: BOTTOM_ORIFICE = 1  ! bottom orifice

!enum WeirType {
integer(kind=K2), parameter :: TRANSVERSE_WEIR = 0   ! transverse weir
integer(kind=K2), parameter :: SIDEFLOW_WEIR = 1     ! side flow weir
integer(kind=K2), parameter :: VNOTCH_WEIR = 2       ! V-notch (triangular) weir
integer(kind=K2), parameter :: TRAPEZOIDAL_WEIR = 3  ! trapezoidal weir

!enum CurveType {
integer(kind=K2), parameter :: STORAGE_CURVE = 0     ! surf. area v. depth for storage node
integer(kind=K2), parameter :: DIVERSION_CURVE = 1   ! diverted flow v. inflow for divider node
integer(kind=K2), parameter :: TIDAL_CURVE = 2       ! water elev. v. hour of day for outfall
integer(kind=K2), parameter :: RATING_CURVE = 3      ! flow rate v. head for outlet link
integer(kind=K2), parameter :: CONTROL_CURVE = 4     ! control setting v. controller variable
integer(kind=K2), parameter :: SHAPE_CURVE = 5       ! width v. depth for custom x-section  !(5.0.010 - LR)
integer(kind=K2), parameter :: PUMP1_CURVE = 6       ! flow v. wet well volume for pump
integer(kind=K2), parameter :: PUMP2_CURVE = 7       ! flow v. depth for pump (discrete)
integer(kind=K2), parameter :: PUMP3_CURVE = 8       ! flow v. head for pump (continuous)
integer(kind=K2), parameter :: PUMP4_CURVE = 9       ! flow v. depth for pump (continuous)

!enum InputSectionType {
integer(kind=K2), parameter :: s_TITLE = 0
integer(kind=K2), parameter :: s_OPTION = 1
integer(kind=K2), parameter :: s_FILE = 2
integer(kind=K2), parameter :: s_RAINGAGE = 3
integer(kind=K2), parameter :: s_TEMP = 4
integer(kind=K2), parameter :: s_EVAP = 5
integer(kind=K2), parameter :: s_SUBCATCH = 6
integer(kind=K2), parameter :: s_SUBAREA = 7
integer(kind=K2), parameter :: s_INFIL = 8
integer(kind=K2), parameter :: s_AQUIFER = 9
integer(kind=K2), parameter :: s_GROUNDWATER = 10
integer(kind=K2), parameter :: s_SNOWMELT = 11
integer(kind=K2), parameter :: s_JUNCTION = 12
integer(kind=K2), parameter :: s_OUTFALL = 13
integer(kind=K2), parameter :: s_STORAGE = 14
integer(kind=K2), parameter :: s_DIVIDER = 15
integer(kind=K2), parameter :: s_CONDUIT = 16
integer(kind=K2), parameter :: s_PUMP = 17
integer(kind=K2), parameter :: s_ORIFICE = 18
integer(kind=K2), parameter :: s_WEIR = 19
integer(kind=K2), parameter :: s_OUTLET = 20
integer(kind=K2), parameter :: s_XSECTION = 21
integer(kind=K2), parameter :: s_TRANSECT = 22
integer(kind=K2), parameter :: s_LOSSES = 23
integer(kind=K2), parameter :: s_CONTROL = 24
integer(kind=K2), parameter :: s_POLLUTANT = 25
integer(kind=K2), parameter :: s_LANDUSE = 26
integer(kind=K2), parameter :: s_BUILDUP = 27
integer(kind=K2), parameter :: s_WASHOFF = 28
integer(kind=K2), parameter :: s_COVERAGE = 29
integer(kind=K2), parameter :: s_INFLOW = 30
integer(kind=K2), parameter :: s_DWF = 31
integer(kind=K2), parameter :: s_PATTERN = 32
integer(kind=K2), parameter :: s_RDII = 33
integer(kind=K2), parameter :: s_UNITHYD = 34
integer(kind=K2), parameter :: s_LOADING = 35
integer(kind=K2), parameter :: s_TREATMENT = 36
integer(kind=K2), parameter :: s_CURVE = 37
integer(kind=K2), parameter :: s_TIMESERIES = 38
integer(kind=K2), parameter :: s_REPORT = 39
integer(kind=K2), parameter :: s_COORDINATE = 40
integer(kind=K2), parameter :: s_VERTICES = 41
integer(kind=K2), parameter :: s_POLYGON = 42
integer(kind=K2), parameter :: s_LABEL = 43
integer(kind=K2), parameter :: s_SYMBOL = 44
integer(kind=K2), parameter :: s_BACKDROP = 45
integer(kind=K2), parameter :: s_TAG = 46
integer(kind=K2), parameter :: s_PROFILE = 47
integer(kind=K2), parameter :: s_MAP = 48
integer(kind=K2), parameter :: s_LID_CONTROL = 49
integer(kind=K2), parameter :: s_LID_USAGE = 50     !(5.0.019 - LR)

!enum InputOptionType {
integer(kind=K2), parameter :: FLOW_UNITS = 0
integer(kind=K2), parameter :: INFIL_MODEL = 1
integer(kind=K2), parameter :: ROUTE_MODEL = 2
integer(kind=K2), parameter :: START_DATE = 3
integer(kind=K2), parameter :: START_TIME = 4
integer(kind=K2), parameter :: END_DATE = 5
integer(kind=K2), parameter :: END_TIME = 6
integer(kind=K2), parameter :: REPORT_START_DATE = 7
integer(kind=K2), parameter :: REPORT_START_TIME = 8
integer(kind=K2), parameter :: SWEEP_START = 9
integer(kind=K2), parameter :: SWEEP_END = 10
integer(kind=K2), parameter :: START_DRY_DAYS = 11
integer(kind=K2), parameter :: WET_STEP = 12
integer(kind=K2), parameter :: DRY_STEP = 13
integer(kind=K2), parameter :: ROUTE_STEP = 14
integer(kind=K2), parameter :: REPORT_STEP = 15
integer(kind=K2), parameter :: ALLOW_PONDING = 16
integer(kind=K2), parameter :: INERT_DAMPING = 17
integer(kind=K2), parameter :: SLOPE_WEIGHTING = 18
integer(kind=K2), parameter :: VARIABLE_STEP = 19
integer(kind=K2), parameter :: NORMAL_FLOW_LTD = 20
integer(kind=K2), parameter :: LENGTHENING_STEP = 21
integer(kind=K2), parameter :: MIN_SURFAREA = 22
integer(kind=K2), parameter :: COMPATIBILITY = 23
integer(kind=K2), parameter :: SKIP_STEADY_STATE = 24
integer(kind=K2), parameter :: TEMPDIR = 25
integer(kind=K2), parameter :: IGNORE_RAINFALL = 26
integer(kind=K2), parameter :: FORCE_MAIN_EQN = 27
integer(kind=K2), parameter :: LINK_OFFSETS = 28
integer(kind=K2), parameter :: MIN_SLOPE = 29
integer(kind=K2), parameter :: IGNORE_SNOWMELT = 30
integer(kind=K2), parameter :: IGNORE_GWATER = 31
integer(kind=K2), parameter :: IGNORE_ROUTING = 32
integer(kind=K2), parameter :: IGNORE_QUALITY = 33

!enum  NoYesType {
integer(kind=K2), parameter :: NO = 0
integer(kind=K2), parameter :: YES = 1

!enum  NoneAllType {
integer(kind=K2), parameter :: pNONE = 0
integer(kind=K2), parameter :: pALL = 1
integer(kind=K2), parameter :: SOME = 2
end module

