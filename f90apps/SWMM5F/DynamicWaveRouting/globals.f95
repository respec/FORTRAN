include 'objects.f95'
module globals
!-----------------------------------------------------------------------------
!   globals.h
!
!   Project: EPA SWMM5
!   Version: 5.0
!   Date:    6/19/07   (Build 5.0.010)
!            2/4/08    (Build 5.0.012)
!            1/21/09   (Build 5.0.014)
!            07/30/10  (Build 5.0.019)
!   Author:  L. Rossman
!
!   Global Variables
!-----------------------------------------------------------------------------

use objects

type(TFile) :: Finp                     ! Input file
type(TFile) :: Fout                     ! Output file
type(TFile) :: Frpt                     ! Report file
type(TFile) :: Fclimate                 ! Climate file
type(TFile) :: Frain                    ! Rainfall file
type(TFile) :: Frunoff                  ! Runoff file
type(TFile) :: Frdii                    ! RDII inflow file
type(TFile) :: Fhotstart1               ! Hotstart input file
type(TFile) :: Fhotstart2               ! Hotstart output file
type(TFile) :: Finflows                 ! Inflows routing file
type(TFile) :: Foutflows                ! Outflows routing file

!EXTERN long
integer :: Nperiods                 ! Number of reporting periods
integer :: StepCount                ! Number of routing steps used

!EXTERN char
character(len=MAXMSG+1) :: Msg            ! Text of output message
character(len=MAXMSG+1), dimension(MAXTITLE) :: Title ! Project title
character(len=MAXFNAME+1) :: TmpDir       ! Temporary file directory

type(TRptFlags) :: RptFlags                 ! Reporting options

integer, dimension(MAX_OBJ_TYPES) :: Nobjects  ! Number of each object type
integer, dimension(MAX_NODE_TYPES) :: Nnodes   ! Number of each node sub-type
integer, dimension(MAX_LINK_TYPES) :: Nlinks   ! Number of each link sub-type
integer :: UnitSystem               ! Unit system
integer :: FlowUnits                ! Flow units
integer :: InfilModel               ! Infiltration method
integer :: RouteModel               ! Flow routing method
integer :: ForceMainEqn             ! Flow equation for force mains   !(5.0.010 - LR)
integer :: LinkOffsets              ! Link offset convention          !(5.0.012 - LR)
logical :: AllowPonding             ! Allow water to pond at nodes
integer :: InertDamping             ! Degree of inertial damping
integer :: NormalFlowLtd            ! Normal flow limited
logical :: SlopeWeighting           ! Use slope weighting
integer :: Compatibility            ! SWMM 5/3/4 compatibility
logical :: SkipSteadyState          ! Skip over steady state periods
logical :: IgnoreRainfall           ! Ignore rainfall/runoff
logical :: IgnoreSnowmelt           ! Ignore snowmelt                 !(5.0.014 - LR)
logical :: IgnoreGwater             ! Ignore groundwater              !(5.0.014 - LR)
logical :: IgnoreRouting            ! Ignore flow routing             !(5.0.014 - LR)
logical :: IgnoreQuality            ! Ignore water quality            !(5.0.014 - LR)
integer :: ErrorCode                ! Error code number
integer :: WarningCode              ! Warning code number
integer :: WetStep                  ! Runoff wet time step (sec)
integer :: DryStep                  ! Runoff dry time step (sec)
integer :: ReportStep               ! Reporting time step (sec)
integer :: SweepStart               ! Day of year when sweeping starts
integer :: SweepEnd                 ! Day of year when sweeping ends

double precision :: RouteStep                ! Routing time step (sec)
double precision :: LengtheningStep          ! Time step for lengthening (sec)
double precision :: StartDryDays             ! Antecedent dry days
double precision :: CourantFactor            ! Courant time step factor
double precision :: MinSurfArea              ! Minimum nodal surface area
double precision :: MinSlope                 ! Minimum conduit slope
double precision :: RunoffError              ! Runoff continuity error
double precision :: GwaterError              ! Groundwater continuity error
double precision :: FlowError                ! Flow routing error
double precision :: QualError                ! Quality routing error

!EXTERN DateTime !TODO: need to deal with this DateTime with HSPF's
double precision :: StartDate                ! Starting date
double precision :: StartTime                ! Starting time
double precision :: StartDateTime            ! Starting Date+Time
double precision :: EndDate                  ! Ending date
double precision :: EndTime                  ! Ending time
double precision :: EndDateTime              ! Ending Date+Time
double precision :: ReportStartDate          ! Report start date
double precision :: ReportStartTime          ! Report start time
double precision :: ReportStart              ! Report start Date+Time
!
double precision :: ReportTime               ! Current reporting time (msec)
double precision :: OldRunoffTime            ! Previous runoff time (msec)
double precision :: NewRunoffTime            ! Current runoff time (msec)
double precision :: OldRoutingTime           ! Previous routing time (msec)
double precision :: NewRoutingTime           ! Current routing time (msec)
double precision :: TotalDuration            ! Simulation duration (msec)

type(TTemp) :: Temp                     ! Temperature data
type(TEvap) :: Evap                     ! Evaporation data
type(TWind) :: Wind                     ! Wind speed data
type(TSnow) :: Snow                     ! Snow melt data

type(TSnowmelt), dimension(:), allocatable ::  Snowmelt                 ! Array of snow melt objects
type(TGage), dimension(:), allocatable ::      Gage                     ! Array of rain gages
type(TSubcatch), dimension(:), allocatable ::  Subcatch                 ! Array of subcatchments
type(TAquifer), dimension(:), allocatable ::   Aquifer                  ! Array of groundwater aquifers
type(TUnitHyd), dimension(:), allocatable ::   UnitHyd                  ! Array of unit hydrographs
type(TNode), dimension(:), allocatable ::      Node                     ! Array of nodes
type(TOutfall), dimension(:), allocatable ::   Outfall                  ! Array of outfall nodes
type(TDivider), dimension(:), allocatable ::   Divider                  ! Array of divider nodes
type(TStorage), dimension(:), allocatable ::   Storage                  ! Array of storage nodes
type(TLink), dimension(:), allocatable, target ::      arrLink          ! Array of links
type(TConduit), dimension(:), allocatable ::   Conduit                  ! Array of conduit links
type(TPump), dimension(:), allocatable ::      Pump                     ! Array of pump links
type(TOrifice), dimension(:), allocatable ::   Orifice                  ! Array of orifice links
type(TWeir), dimension(:), allocatable ::      Weir                     ! Array of weir links
type(TOutlet), dimension(:), allocatable ::    Outlet                   ! Array of outlet device links
type(TPollut), dimension(:), allocatable ::    Pollut                   ! Array of pollutants
type(TLanduse), dimension(:), allocatable ::   Landuse                  ! Array of landuses
type(TPattern), dimension(:), allocatable ::   Pattern                  ! Array of time patterns
type(TTable), dimension(:), allocatable ::     Curve                    ! Array of curve tables
type(TTable), dimension(:), allocatable ::     Tseries                  ! Array of time series tables
type(TTransect), dimension(:), allocatable ::  Transect                 ! Array of transect data
type(TShape), dimension(:), allocatable ::     Shape                    ! Array of custom conduit shapes  !(5.0.010 - LR)

!EXTERN THorton*   HortInfil;              ! Horton infiltration data        !(5.0.019 - LR)
!EXTERN TGrnAmpt*  GAInfil;                ! Green-Ampt infiltration data    !(5.0.019 - LR)
!EXTERN TCurveNum* CNInfil;                ! Curve No. infiltration data     !(5.0.019 - LR)

!-----------------------------------------------------------------------------                  
!  Shared variables used only in node.f95
!-----------------------------------------------------------------------------                  
integer ::  Kstar                  ! storage unit index
double precision :: Vstar                  ! storage unit volume (ft3)

end module
