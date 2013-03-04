include 'consts.f95'
include 'enums.f95'

module objects
!  --------------------------------------------------
!  Silverfrost FTN95 for Microsoft Visual Studio
!  Free Format FTN95 Source File
!  --------------------------------------------------
!-----------------------------------------------------------------------------
!   objects.h
!
!   Project: EPA SWMM5
!   Version: 5.0
!   Date:    6/19/07   (Build 5.0.010)
!            2/4/08    (Build 5.0.012)
!            3/11/08   (Build 5.0.013)
!            1/21/09   (Build 5.0.014)
!            4/10/09   (Build 5.0.015)
!            10/7/09   (Build 5.0.017)
!            11/18/09  (Build 5.0.018)
!            07/30/10  (Build 5.0.019)
!            04/20/11  (Build 5.0.022)
!   Author:  L. Rossman (EPA)
!            R. Dickinson (CDM)
!
!   Definitions of data structures.
!
!   Most SWMM 5 "objects" are represented as C data structures.
!
!   NOTE: the units shown next to each structure's properties are SWMM's
!         internal units and may be different than the units required
!         for the property as it appears in the input file.
!
!   NOTE: in many structure definitions, a blank line separates the set of
!         input properties from the set of computed output properties.
! ISSUES
! -DateTime data type replacement
! -Array in a derived type must be fixed size !TODO: dim fixed
! -commented out the use of MathExpr and TGrnAmpt type
!-----------------------------------------------------------------------------

!#include "mathexpr.h"
!#include "infil.h"
use consts
use enums
implicit none

!-----------------
! FILE INFORMATION
!-----------------
type TFile
   character(len=MAXFNAME+1) :: name     ! file name
   integer(kind=K2) :: mode              ! NO_FILE, SCRATCH, USE, or SAVE
   integer(kind=K2) :: state             ! current state (OPENED, CLOSED)
   !FILE*         file                 ! FILE structure pointer
   integer(kind=K4) :: fileHandle        ! FILE structure pointer
end type TFile

!-----------------------------------------
! LINKED LIST ENTRY FOR TABLES/TIME SERIES
!-----------------------------------------
type TTableEntry
   double precision :: x
   double precision :: y
   type(TTableEntry), pointer :: next
end type TTableEntry
!typedef struct TableEntry TTableEntry
!type(TableEntry) :: TTableEntry


!-------------------------
! CURVE/TIME SERIES OBJECT
!-------------------------
type TTable
   !char*         ID              ! Table/time series ID
   character*(20) :: ID              ! Table/time series ID
   integer(kind=K4) :: curveType       ! type of curve tabulated
   integer(kind=K4) :: refersTo        ! reference to some other object            !(5.0.010 - LR)
   double precision :: dxMin           ! smallest x-value interval                 !(5.0.014 - LR)
   double precision :: lastDate        ! last input date for time series
   double precision :: x1, x2          ! current bracket on x-values
   double precision :: y1, y2          ! current bracket on y-values
   type(TTableEntry) ::  firstEntry      ! first data point
   type(TTableEntry) ::  lastEntry       ! last data point
   type(TTableEntry) ::  thisEntry       ! current data point
   type(TFile) ::  extfile            ! external data file                        !(5.0.014 - LR)
end type TTable


!-----------------
! RAIN GAGE OBJECT
!-----------------
type tGage
   character(len=20) :: ID              ! raingage name
   integer(kind=K4) :: dataSource      ! data from time series or file 
   integer(kind=K4) :: tSeries         ! rainfall data time series index
   character(len=MAXFNAME +1) :: fname ! name of rainfall data file
   character(len=MAXMSG +1) :: staID ! station number
!   DateTime      startFileDate   ! starting date of data read from file
!   DateTime      endFileDate     ! ending date of data read from file
   integer(kind=K4) :: rainType        ! intensity, volume, cumulative
   integer(kind=K4) :: rainInterval    ! recording time interval (seconds)
   integer(kind=K4) :: rainUnits       ! rain depth units (US or SI)
   double precision :: snowFactor      ! snow catch deficiency correction

   integer(kind=K8) :: startFilePos    ! starting byte position in Rain file
   integer(kind=K8) :: endFilePos      ! ending byte position in Rain file
   integer(kind=K8) :: currentFilePos  ! current byte position in Rain file
   double precision :: rainAccum       ! cumulative rainfall
   double precision :: unitsFactor     ! units conversion factor (to inches or mm)
   !DateTime      startDate       ! start date of current rainfall
!   DateTime      endDate         ! end date of current rainfall
!   DateTime      nextDate        ! next date with recorded rainfall
   double precision :: rainfall        ! current rainfall (in/hr or mm/hr)
   double precision :: nextRainfall    ! next rainfall (in/hr or mm/hr)
   double precision :: reportRainfall  ! rainfall value used for reported results
   integer(kind=K4) :: coGage          ! index of gage with same rain timeseries
   !integer :: isUsed          ! TRUE if gage used by any subcatchment
   !integer :: isCurrent       ! TRUE if gage's rainfall is current        !(5.0.012 - RD)
   logical(kind=K2) :: isUsed          ! TRUE if gage used by any subcatchment
   logical(kind=K2):: isCurrent       ! TRUE if gage's rainfall is current        !(5.0.012 - RD)
end type  TGage

!-------------------
! TEMPERATURE OBJECT
!-------------------
type TTemp
   integer(kind=K4) ::           dataSource      ! data from time series or file 
   integer(kind=K4) ::           tSeries         ! temperature data time series index
!   DateTime      fileStartDate   ! starting date of data read from file
   double precision ::        elev            ! elev. of study area (ft)
   double precision ::        anglat          ! latitude (degrees)
   double precision ::        dtlong          ! longitude correction (hours)

   double precision ::        ta              ! air temperature (deg F)
   double precision ::        tmax            ! previous day's max. temp. (deg F)
   double precision ::        ea              ! saturation vapor pressure (in Hg)
   double precision ::        gamma           ! psychrometric constant
   double precision ::        tanAnglat       ! tangent of latitude angle
end type TTemp


!-----------------
! WINDSPEED OBJECT
!-----------------
type TWind
   integer(kind=K4) :: Datatype             ! monthly or file data
   double precision, dimension(12) :: aws          ! monthly avg. wind speed (mph)
   double precision :: ws              ! wind speed (mph)
end type  TWind


!------------
! SNOW OBJECT
!------------
type TSnow
    double precision ::        snotmp          ! temp. dividing rain from snow (deg F)
    double precision ::        tipm            ! antecedent temp. index parameter
    double precision ::        rnm             ! ratio of neg. melt to melt coeff.
    double precision, dimension(2,10) :: adc      ! areal depletion curves (pervious & 
                                  ! imperv. area curves w/ 10 pts.each)
    double precision ::        season          ! snowmelt season
    double precision ::        removed         ! total snow plowed out of system (ft3)
end type TSnow


!-------------------
! EVAPORATION OBJECT
!-------------------
type TEvap
    integer(kind=K4) :: Datatype            ! type of evaporation data
    integer(kind=K4) :: tSeries         ! time series index
    double precision, dimension(12) :: monthlyEvap ! monthly evaporation values
    double precision, dimension(12) :: panCoeff    ! monthly pan coeff. values
    integer(kind=K4) :: recoveryPattern ! soil recovery factor pattern              !(5.0.014 - LR)
    !integer :: dryOnly         ! true if evaporation only in dry periods   !(5.0.019 - LR)
    logical(kind=K2) :: dryOnly         ! true if evaporation only in dry periods   !(5.0.019 - LR)
    double precision :: rate            ! current evaporation rate (ft/sec)
    double precision :: recoveryFactor  ! current soil recovery factor              !(5.0.014 - LR) 
end type TEvap

!--------------------------------------------------------  
!  Infiltration objects are now declared in infil.h                           !(5.0.019 - LR)
!--------------------------------------------------------  

!-------------------
! AQUIFER OBJECT
!-------------------
type TAquifer
    character(len=20) ::       ID               ! aquifer name
    double precision ::      porosity         ! soil porosity
    double precision ::      wiltingPoint     ! soil wilting point
    double precision ::      fieldCapacity    ! soil field capacity
    double precision ::      conductivity     ! soil hyd. conductivity (ft/sec)
    double precision ::      conductSlope     ! slope of conductivity v. moisture curve
    double precision ::      tensionSlope     ! slope of tension v. moisture curve
    double precision ::      upperEvapFrac    ! evaporation available in upper zone
    double precision ::      lowerEvapDepth   ! evap depth existing in lower zone (ft)
    double precision ::      lowerLossCoeff   ! coeff. for losses to deep GW (ft/sec)
    double precision ::      bottomElev       ! elevation of bottom of aquifer (ft)
    double precision ::      waterTableElev   ! initial water table elevation (ft)
    double precision ::      upperMoisture    ! initial moisture content of unsat. zone
end type TAquifer


!------------------------
! GROUNDWATER OBJECT
!------------------------
type TGroundwater
    integer(kind=K4) ::           aquifer        ! index of associated gw aquifer 
    integer(kind=K4) ::           node           ! index of node receiving gw flow
    double precision ::        surfElev       ! elevation of ground surface (ft)
    double precision ::        a1, b1         ! ground water outflow coeff. & exponent
    double precision ::        a2, b2         ! surface water outflow coeff. & exponent
    double precision ::        a3             ! surf./ground water interaction coeff.
    double precision ::        fixedDepth     ! fixed surface water water depth (ft)
    double precision ::        nodeElev       ! elevation of receiving node invert (ft)
    double precision ::        theta          ! upper zone moisture content
    double precision ::        lowerDepth     ! depth of saturated zone (ft)
    double precision ::        oldFlow        ! gw outflow from previous time period (cfs)
    double precision ::        newFlow        ! gw outflow from current time period (cfs)
    double precision ::        maxInfilVol    ! max. infil. upper zone can accept (ft)    !(5.0.019 - LR)
end type TGroundwater


!----------------
! SNOWMELT OBJECT
!----------------
! Snowmelt objects contain parameters that describe the melting
! process of snow packs on 3 different types of surfaces:
!   1 - plowable impervious area
!   2 - non-plowable impervious area
!   3 - pervious area
type TSnowmelt
   character(len=20) ::         ID              ! snowmelt parameter set name
   double precision ::        snn             ! fraction of impervious area plowable
   double precision, dimension(3) :: si           ! snow depth for 100% cover
   double precision, dimension(3) :: dhmin        ! min. melt coeff. for each surface (ft/sec-F)
   double precision, dimension(3) :: dhmax        ! max. melt coeff. for each surface (ft/sec-F)
   double precision, dimension(3) :: tbase        ! base temp. for melting (F)
   double precision, dimension(3) :: fwfrac       ! free water capacity / snow depth
   double precision, dimension(3) :: wsnow        ! initial snow depth on each surface (ft)
   double precision, dimension(3) :: fwnow        ! initial free water in snow pack (ft)
   double precision :: weplow          ! depth at which plowing begins (ft)
   double precision, dimension(5) :: sfrac        ! fractions moved to other areas by plowing
   integer(kind=K4) :: toSubcatch      ! index of subcatch receiving plowed snow
   double precision, dimension(3) :: dhm          ! melt coeff. for each surface (ft/sec-F)
end type TSnowmelt


!----------------
! SNOWPACK OBJECT
!----------------
! Snowpack objects describe the state of the snow melt process on each
! of 3 types of snow surfaces.
type TSnowpack
   integer(kind=K4) ::           snowmeltIndex   ! index of snow melt parameter set
   double precision, dimension(3) :: fArea        ! fraction of total area of each surface
   double precision, dimension(3) :: wsnow        ! depth of snow pack (ft)
   double precision, dimension(3) :: fw           ! depth of free water in snow pack (ft)
   double precision, dimension(3) :: coldc        ! cold content of snow pack
   double precision, dimension(3) :: ati          ! antecedent temperature index (deg F)
   double precision, dimension(3) :: sba          ! initial ASC of linear ADC
   double precision, dimension(3) :: awe          ! initial AWESI of linear ADC
   double precision, dimension(3) :: sbws         ! final AWESI of linear ADC
   double precision, dimension(3) :: imelt        ! immediate melt (ft)
   end type TSnowpack

!---------------
! SUBAREA OBJECT
!---------------
! An array of 3 subarea objects is associated with each subcatchment object.
! They describe the runoff process on 3 types of surfaces:
!   1 - impervious with no depression storage
!   2 - impervious with depression storage
!   3 - pervious
type TSubarea
   integer(kind=K4) ::           routeTo         ! code indicating where outflow is sent
   double precision :: fOutlet         ! fraction of outflow to outlet
   double precision :: N               ! Manning's n
   double precision :: fArea           ! fraction of total area
   double precision :: dStore          ! depression storage (ft)
   double precision :: alpha           ! overland flow factor
   double precision :: inflow          ! inflow rate (ft/sec)
   double precision :: runoff          ! runoff rate (ft/sec)
   double precision :: depth           ! depth of surface runoff (ft)
   end type TSubarea

!-------------------------
! LAND AREA LANDUSE FACTOR
!-------------------------
type TLandFactor
   double precision ::       fraction        ! fraction of land area with land use
   !double*       buildup         ! array of buildups for each pollutant
   !double precision, dimension(:) :: buildup         ! array of buildups for each pollutant
   double precision, dimension(12) :: buildup         ! array of buildups for each pollutant, TODO:dim fixed
!   DateTime      lastSwept       ! date/time of last street sweeping
   end type TLandFactor


!--------------------
! SUBCATCHMENT OBJECT
!--------------------
type TSubcatch
   character(len=20) ::         ID              ! subcatchment name
   character*1 ::          rptFlag         ! reporting flag
   integer(kind=K4) ::           gage            ! raingage index
   integer(kind=K4) ::           outNode         ! outlet node index
   integer(kind=K4) ::           outSubcatch     ! outlet subcatchment index
   integer(kind=K4) ::           infil           ! infiltration object index
   type(TSubarea), dimension(3) :: subArea      ! sub-area data
   double precision ::        width           ! overland flow width (ft)
   double precision ::        area            ! area (ft2)
   double precision ::        fracImperv      ! fraction impervious
   double precision ::        slope           ! slope (ft/ft)
   double precision ::        curbLength      ! total curb length (ft)
   !double*       initBuildup     ! initial pollutant buildup (mass/ft2)
   double precision ::       initBuildup     ! initial pollutant buildup (mass/ft2)

   !TLandFactor*  landFactor      ! array of land use factors
   !TGroundwater* groundwater     ! associated groundwater data
   !TSnowpack*    snowpack        ! associated snow pack data
   type(TLandFactor), dimension(12) ::  landFactor      ! array of land use factors !TODO:dim fixed
   type(TGroundwater), dimension(12) :: groundwater     ! associated groundwater data !TODO:dim fixed
   type(TSnowpack), dimension(12) ::    snowpack        ! associated snow pack data !TODO:dim fixed

   double precision ::        lidArea         ! area devoted to LIDs (ft2)                !(5.0.019 - LR)
   double precision ::        rainfall        ! current rainfall (ft/sec)
   double precision ::        losses          ! current infil + evap losses (ft/sec)
   double precision ::        runon           ! runon from other subcatchments (cfs)
   double precision ::        oldRunoff       ! previous runoff (cfs)
   double precision ::        newRunoff       ! current runoff (cfs)
   double precision ::        oldSnowDepth    ! previous snow depth (ft)
   double precision ::        newSnowDepth    ! current snow depth (ft)

   !double*       oldQual         ! previous runoff quality (mass/L)
   !double*       newQual         ! current runoff quality (mass/L)
   !double*       pondedQual      ! ponded surface water quality (mass/ft3)
   !double*       totalLoad       ! total washoff load (lbs or kg)
   double precision :: oldQual         ! previous runoff quality (mass/L)
   double precision :: newQual         ! current runoff quality (mass/L)
   double precision :: pondedQual      ! ponded surface water quality (mass/ft3)
   double precision :: totalLoad       ! total washoff load (lbs or kg)

end type  TSubcatch


!-----------------------
! TIME PATTERN DATA
!-----------------------
type TPattern
   character(len=20) ::        ID               ! time pattern name
   integer(kind=K4) ::          Datatype             ! time pattern type code
   integer(kind=K4) ::          count            ! number of factors
   double precision, dimension(24) :: factor       ! time pattern factors
end type TPattern


!------------------------------
! DIRECT EXTERNAL INFLOW OBJECT
!------------------------------
type TExtInflow
   integer(kind=K4) ::            param         ! pollutant index (flow = -1)
   integer(kind=K4) ::            Datatype          ! CONCEN or MASS
   integer(kind=K4) ::            tSeries       ! index of inflow time series
   integer(kind=K4) ::            basePat       ! baseline time pattern                      !(5.0.014 - LR)
   double precision ::         cFactor       ! units conversion factor for mass inflow
   double precision ::         baseline      ! constant baseline value
   double precision ::         sFactor       ! time series scaling factor
   !struct ExtInflow* next       ! pointer to next inflow data object
   type(TExtInflow), pointer :: next       ! pointer to next inflow data object
end type TExtInflow 

!-------------------------------
! DRY WEATHER FLOW INFLOW OBJECT
!-------------------------------
type TDwfInflow
   integer(kind=K4) :: param          ! pollutant index (flow = -1)
   double precision :: avgValue       ! average value (cfs or concen.)
   integer(kind=K4), dimension(4) :: patterns   ! monthly, daily, hourly, weekend time patterns
   !struct DwfInflow* next        ! pointer to next inflow data object
   type(TDwfInflow), pointer :: next        ! pointer to next inflow data object
end type TDwfInflow
!typedef struct DwfInflow TDwfInflow

!-------------------
! RDII INFLOW OBJECT
!-------------------
type TRdiiInflow
   integer(kind=K4) :: unitHyd         ! index of unit hydrograph
   double precision :: area            ! area of sewershed (ft2)
end type TRdiiInflow


!-----------------------------
! UNIT HYDROGRAPH GROUP OBJECT
!-----------------------------
type TUnitHyd
   character(len=20) :: ID     ! name of the unit hydrograph object
   integer(kind=K4) :: rainGage        ! index of rain gage
   double precision, dimension(12, 3) :: iaMax   ! max. initial abstraction (IA) (in or mm)  !(5.0.015 - LR)
   double precision, dimension(12, 3) :: iaRecov ! IA recovery rate (in/day or mm/day)       !(5.0.015 - LR)
   double precision, dimension(12, 3) :: iaInit  ! starting IA (in or mm)                    !(5.0.015 - LR)
   double precision, dimension(12, 3) :: r       ! fraction of rainfall becoming I&I
   integer(kind=K8), dimension(12, 3) :: tBase    ! time base of each UH in each month (sec)
   integer(kind=K8), dimension(12, 3) :: tPeak    ! time to peak of each UH in each month (sec)
end type TUnitHyd

!-----------------
! TREATMENT OBJECT
!-----------------
type TTreatment
    integer(kind=K4) :: treatType       ! treatment equation type: REMOVAL/CONCEN
    !MathExpr*    equation        ! treatment eqn. as tokenized math terms    !(5.0.010 - LR)
    !type(MathExpr) :: equation        ! treatment eqn. as tokenized math terms    !(5.0.010 - LR) !TODO: add this later
end type TTreatment


!------------
! NODE OBJECT
!------------
type TNode
   character(len=20) ::         ID              ! node ID
   integer(kind=K2) ::           datatype            ! node type code
   integer(kind=K4) ::           subIndex        ! index of node's sub-category
   character*1 ::          rptFlag         ! reporting flag
   double precision ::        invertElev      ! invert elevation (ft)
   double precision ::        initDepth       ! initial storage level (ft)
   double precision ::        fullDepth       ! dist. from invert to surface (ft)
   double precision ::        surDepth        ! added depth under surcharge (ft)
   double precision ::        pondedArea      ! area filled by ponded water (ft2)

   !TExtInflow*   extInflow       ! pointer to external inflow data
   !TDwfInflow*   dwfInflow       ! pointer to dry weather flow inflow data
   !TRdiiInflow*  rdiiInflow      ! pointer to RDII inflow data
   !TTreatment*   treatment       ! array of treatment data
   type(TExtInflow), pointer ::   extInflow       ! pointer to external inflow data
   type(TDwfInflow), pointer ::   dwfInflow       ! pointer to dry weather flow inflow data
   type(TRdiiInflow), pointer ::  rdiiInflow      ! pointer to RDII inflow data
   type(TTreatment), pointer ::   treatment       ! array of treatment data

   integer(kind=K4) ::           degree          ! number of outflow links
   !character*1 ::          updated         ! true if state has been updated
   logical(kind=K2) ::          updated         ! true if state has been updated
   double precision ::        crownElev       ! top of highest connecting conduit (ft)
   double precision ::        inflow          ! total inflow (cfs)
   double precision ::        outflow         ! total outflow (cfs)
   double precision ::        oldVolume       ! previous volume (ft3)
   double precision ::        newVolume       ! current volume (ft3)
   double precision ::        fullVolume      ! max. storage available (ft3)
   double precision ::        overflow        ! overflow rate (cfs)
   double precision ::        oldDepth        ! previous water depth (ft)
   double precision ::        newDepth        ! current water depth (ft)
   double precision ::        oldLatFlow      ! previous lateral inflow (cfs)
   double precision ::        newLatFlow      ! current lateral inflow (cfs)

   !double*       oldQual         ! previous quality state
   !double*       newQual         ! current quality state
   double precision ::       oldQual         ! previous quality state
   double precision ::       newQual         ! current quality state

   !double*       wStored  originally commented out    !(5.0.018 - LR)

   double precision ::        oldFlowInflow   ! previous flow inflow
   double precision ::        oldNetInflow    ! previous net inflow
end type TNode


!---------------
! OUTFALL OBJECT
!---------------
type TOutfall
   integer(kind=K4) ::        datatype               ! outfall type code
   logical(kind=K2) ::       hasFlapGate        ! true if contains flap gate
   double precision ::     fixedStage         ! fixed outfall stage (ft)
   integer(kind=K4) ::        tideCurve          ! index of tidal stage curve
   integer(kind=K4) ::        stageSeries        ! index of outfall stage time series
end type TOutfall


!--------------------
! STORAGE UNIT OBJECT
!--------------------
type TStorage
   double precision ::      fEvap             ! fraction of evaporation realized
   double precision ::      aConst            ! surface area at zero height (ft2)
   double precision ::      aCoeff            ! coeff. of area v. height curve
   double precision ::      aExpon            ! exponent of area v. height curve
   integer(kind=K4) ::         aCurve            ! index of tabulated area v. height curve
   !TGrnAmpt*   infil             ! ptr. to infiltration object               !(5.0.015 - LR)
   !type(TGrnAmpt) :: infil        ! ptr. to infiltration object               !(5.0.015 - LR) !TODO: add this later

   double precision ::      hrt               ! hydraulic residence time (sec)
   double precision ::      evapLoss          ! evaporation loss (ft3)                    !(5.0.019 - LR)
   double precision ::      losses            ! evap + infil losses (ft3)                 !(5.0.018 - LR)
end type TStorage


!--------------------
! FLOW DIVIDER OBJECT
!--------------------
type TDivider
   integer(kind=K4) ::         link              ! index of link with diverted flow
   integer(kind=K4) ::         Datatype              ! divider type code
   double precision ::      qMin              ! minimum inflow for diversion (cfs)
   double precision ::      qMax              ! flow when weir is full (cfs)
   double precision ::      dhMax             ! height of weir (ft)
   double precision ::      cWeir             ! weir discharge coeff.
   integer(kind=K4) ::         flowCurve         ! index of inflow v. diverted flow curve
end type TDivider


!-----------------------------
! CROSS SECTION DATA STRUCTURE
!-----------------------------
type TXsect
   integer(kind=K2) ::           datatype            ! type code of cross section shape
   integer(kind=K4) ::           culvertCode     ! type of culvert (if any)                  !(5.0.014 - LR)
   integer(kind=K4) ::           transect        ! index of transect/shape (if applicable)   !(5.0.010 - LR)
   double precision ::        yFull           ! depth when full (ft)
   double precision ::        wMax            ! width at widest point (ft)
   double precision ::        aFull           ! area when full (ft2)
   double precision ::        rFull           ! hyd. radius when full (ft)
   double precision ::        sFull           ! section factor when full (ft^4/3)
   double precision ::        sMax            ! section factor at max. flow (ft^4/3)

   ! These variables have different meanings depending on section shape
   double precision ::        yBot            ! depth of bottom section
   double precision ::        aBot            ! area of bottom section
   double precision ::        sBot            ! slope of bottom section
   double precision ::        rBot            ! radius of bottom section
end type TXsect


!--------------------------------------
! CROSS SECTION TRANSECT DATA STRUCTURE
!--------------------------------------
integer, parameter ::  N_TRANSECT_TBL =  51       ! size of transect geometry tables          !(5.0.010 - LR)
type TTransect
    character(len=20) ::        ID                        ! section ID
    double precision ::       yFull                     ! depth when full (ft)
    double precision ::       aFull                     ! area when full (ft2)
    double precision ::       rFull                     ! hyd. radius when full (ft)
    double precision ::       wMax                      ! width at widest point (ft)
    double precision ::       sMax                      ! section factor at max. flow (ft^4/3)
    double precision ::       aMax                      ! area at max. flow (ft2)
    double precision ::       lengthFactor              ! floodplain / channel length     !(5.0.015 - LR)

    double precision ::       roughness                 ! Manning's n
    double precision, dimension(N_TRANSECT_TBL) :: areaTbl   ! table of area v. depth
    double precision, dimension(N_TRANSECT_TBL) :: hradTbl   ! table of hyd. radius v. depth
    double precision, dimension(N_TRANSECT_TBL) :: widthTbl  ! table of top width v. depth
    integer(kind=K4) ::          nTbl                      ! size of geometry tables
end type TTransect


!-------------------------------------                                        !(5.0.010 - LR)
! CUSTOM CROSS SECTION SHAPE STRUCTURE                                        !(5.0.010 - LR)
!-------------------------------------                                        !(5.0.010 - LR)
integer, parameter :: N_SHAPE_TBL = 51           ! size of shape geometry tables             !(5.0.010 - LR)
type TShape
    integer(kind=K4) ::          curve                     ! index of shape curve            !(5.0.010 - LR) 
    integer(kind=K4) ::          nTbl                      ! size of geometry tables         !(5.0.010 - LR)
    double precision ::       aFull                     ! area when full                  !(5.0.010 - LR)
    double precision ::       rFull                     ! hyd. radius when full           !(5.0.010 - LR)
    double precision ::       wMax                      ! max. width                      !(5.0.010 - LR)
    double precision ::       sMax                      ! max. section factor             !(5.0.010 - LR)
    double precision ::       aMax                      ! area at max. section factor     !(5.0.010 - LR)
    double precision, dimension(N_SHAPE_TBL) :: areaTbl      ! table of area v. depth          !(5.0.010 - LR)
    double precision, dimension(N_SHAPE_TBL) :: hradTbl      ! table of hyd. radius v. depth   !(5.0.010 - LR)
    double precision, dimension(N_SHAPE_TBL) :: widthTbl     ! table of top width v. depth     !(5.0.010 - LR)
end type TShape


!------------
! LINK OBJECT
!------------
type TLink
   character(len=20) ::        ID              ! link ID
   integer(kind=K4) ::           Datatype            ! link type code
   integer(kind=K4) ::           subIndex        ! index of link's sub-category
   logical(kind=K2) ::          rptFlag         ! reporting flag
   integer(kind=K4) ::           node1           ! start node index
   integer(kind=K4) ::           node2           ! end node index
   double precision ::        offset1         ! ht. above start node invert (ft)          !(5.0.012 - LR)
   double precision ::        offset2         ! ht. above end node invert (ft)            !(5.0.012 - LR)
   type(TXsect) ::        xsect           ! cross section data
   double precision ::        q0              ! initial flow (cfs)
   double precision ::        qLimit          ! constraint on max. flow (cfs)
   double precision ::        cLossInlet      ! inlet loss coeff.
   double precision ::        cLossOutlet     ! outlet loss coeff.
   double precision ::        cLossAvg        ! avg. loss coeff.
   !integer ::           hasFlapGate     ! true if flap gate present
   logical(kind=K2) ::           hasFlapGate     ! true if flap gate present

   double precision :: oldFlow         ! previous flow rate (cfs)
   double precision :: newFlow         ! current flow rate (cfs)
   double precision :: oldDepth        ! previous flow depth (ft)
   double precision :: newDepth        ! current flow depth (ft)
   double precision :: oldVolume       ! previous flow volume (ft3)
   double precision :: newVolume       ! current flow volume (ft3)
   double precision :: qFull           ! flow when full (cfs)
   double precision :: setting         ! current control setting                   !(5.0.010 - LR)
   double precision :: targetSetting   ! target control setting                    !(5.0.010 - LR)
   double precision :: froude          ! Froude number
   !double*       oldQual         ! previous quality state
   !double*       newQual         ! current quality state
   double precision :: oldQual         ! previous quality state
   double precision :: newQual         ! current quality state
   integer(kind=K4) :: flowClass       ! flow classification
   double precision ::        dqdh            ! change in flow w.r.t. head (ft2/sec)
   !signed char   direction       ! flow direction flag
   integer(kind=K4) ::   direction       ! flow direction flag
   logical ::          isClosed        ! flap gate closed flag
end type TLink


!---------------
! CONDUIT OBJECT
!---------------
type TConduit
   double precision ::        length          ! conduit length (ft)
   double precision ::        roughness       ! Manning's n
   !char          barrels         ! number of barrels
   integer(kind=K4) :: barrels         ! number of barrels !kind = 1

   double precision :: modLength       ! modified conduit length (ft)
   double precision :: roughFactor     ! roughness factor for DW routing
   double precision :: slope           ! slope
   double precision :: beta            ! discharge factor
   double precision :: qMax            ! max. flow (cfs)
   double precision :: a1, a2          ! upstream & downstream areas (ft2)
   double precision :: q1, q2          ! upstream & downstream flows per barrel (cfs)
   double precision :: q1Old, q2Old    ! previous values of q1 & q2 (cfs)
! double        aMid            ! average flow area (ft2)                   !(5.0.013 - LR)
   logical(kind=K2) :: capacityLimited ! capacity limited flag                     !(5.0.012 - LR)
   logical(kind=K2) :: superCritical   ! super-critical flow flag
   logical(kind=K2) :: hasLosses       ! local losses flag
end type TConduit


!------------
! PUMP OBJECT
!------------
type TPump
   integer(kind=K4) ::           datatype            ! pump type
   integer(kind=K4) ::           pumpCurve       ! pump curve table index
   double precision ::        initSetting     ! initial speed setting                     !(5.0.010 - LR)
   double precision ::        yOn             ! startup depth (ft)                        !(5.0.010 - LR)
   double precision ::        yOff            ! shutoff depth (ft)                        !(5.0.010 - LR)
   double precision ::        xMin            ! minimum pt. on pump curve                 !(5.0.012 - LR)
   double precision ::        xMax            ! maximum pt. on pump curve                 !(5.0.012 - LR)
end type TPump


!---------------
! ORIFICE OBJECT
!---------------
type TOrifice
   integer(kind=K4) ::           datatype            ! orifice type code
   integer(kind=K4) ::           shape           ! orifice shape code
   double precision :: cDisch          ! discharge coeff.
   double precision :: orate           ! time to open/close (sec)

   double precision :: cOrif           ! coeff. for orifice flow (ft^2.5/sec)      !(5.0.012 - LR)
   double precision :: hCrit           ! inlet depth where weir flow begins (ft)   !(5.0.012 - LR)
   double precision :: cWeir           ! coeff. for weir flow (cfs)                !(5.0.012 - LR)
   double precision :: length          ! equivalent length (ft)
   double precision :: surfArea        ! equivalent surface area (ft2)
end type TOrifice


!------------
! WEIR OBJECT
!------------
type TWeir
   integer(kind=K4) ::           datatype            ! weir type code
   integer(kind=K4) ::           shape           ! weir shape code
   !double        crestHt         ! crest height above node invert (ft)     !(5.0.012 - LR)
   double precision ::        cDisch1         ! discharge coeff.
   double precision ::        cDisch2         ! discharge coeff. for ends
   double precision ::        endCon          ! end contractions

   double precision ::        cSurcharge      ! cDisch for equiv. orifice under surcharge
   double precision ::        length          ! equivalent length (ft)
   double precision ::        slope           ! slope for Vnotch & Trapezoidal weirs
   double precision ::        surfArea        ! equivalent surface area (ft2)
end type TWeir


!---------------------
! OUTLET DEVICE OBJECT
!---------------------
type TOutlet
    !double       crestHt         ! crest ht. above node invert (ft)        !(5.0.012 - LR)
    double precision ::       qCoeff          ! discharge coeff.
    double precision ::       qExpon          ! discharge exponent
    integer(kind=K4) ::          qCurve          ! index of discharge rating curve
    integer(kind=K4) ::          curveType       ! rating curve type                         !(5.0.014 - LR)
end type TOutlet


!-----------------
! POLLUTANT OBJECT
!-----------------
type TPollut
   character(len=20) ::        ID              ! Pollutant ID
   integer(kind=K4) ::           units           ! units
   double precision ::        mcf             ! mass conversion factor
   double precision ::        dwfConcen       ! dry weather sanitary flow concen.         !(5.0.017 - LR)
   double precision ::        pptConcen       ! precip. concen.
   double precision ::        gwConcen        ! groundwater concen.
   double precision ::        rdiiConcen      ! RDII concen.
   double precision ::        kDecay          ! decay constant (1/sec)
   integer(kind=K4) ::           coPollut        ! co-pollutant index
   double precision ::        coFraction      ! co-pollutant fraction
   !int           snowOnly        ! TRUE if buildup occurs only under snow
   logical :: snowOnly        ! TRUE if buildup occurs only under snow
end type TPollut


!------------------------
! BUILDUP FUNCTION OBJECT
!------------------------
type TBuildup
   integer(kind=K4) :: normalizer      ! normalizer code (area or curb length)
   integer(kind=K4) :: funcType        ! buildup function type code
   double precision, dimension(3) :: coeff        ! coeffs. of buildup function
   double precision ::        maxDays         ! time to reach max. buildup (days)
end type TBuildup


!------------------------
! WASHOFF FUNCTION OBJECT
!------------------------
type TWashoff
   integer(kind=K4) ::           funcType        ! washoff function type code
   double precision ::        coeff           ! function coeff.
   double precision ::        expon           ! function exponent
   double precision ::        sweepEffic      ! street sweeping fractional removal
   double precision ::        bmpEffic        ! best mgt. practice fractional removal
end type TWashoff


!---------------
! LANDUSE OBJECT
!---------------
type TLanduse
   character(len=20) ::         ID              ! landuse name
   double precision ::        sweepInterval   ! street sweeping interval (days)
   double precision ::        sweepRemoval    ! fraction of buildup available for sweeping
   double precision ::        sweepDays0      ! days since last sweeping at start

!  TODO: might need to use pointer to point to an literal array!!!
!  TBuildup*     buildupFunc     ! array of buildup functions for pollutants
!  TWashoff*     washoffFunc     ! array of washoff functions for pollutants
   type(TBuildup), dimension(12) ::     buildupFunc     ! array of buildup functions for pollutants !TODO: dim fixed
   type(TWashoff), dimension(12) ::     washoffFunc     ! array of washoff functions for pollutants !TODO: dim fixed
end type TLanduse


!--------------------------
! REPORTING FLAGS STRUCTURE
!--------------------------
type TRptFlags
!  char          report          ! TRUE if results report generated
!  char          input           ! TRUE if input summary included
!  char          subcatchments   ! TRUE if subcatchment results reported
!  char          nodes           ! TRUE if node results reported
!  char          links           ! TRUE if link results reported
!  char          continuity      ! TRUE if continuity errors reported
!  char          flowStats       ! TRUE if routing link flow stats. reported
!  char          nodeStats       ! TRUE if routing node depth stats. reported
!  char          controls        ! TRUE if control actions reported

   logical(kind=K2) :: report          ! TRUE if results report generated
   logical(kind=K2) :: input           ! TRUE if input summary included
   logical(kind=K2) :: subcatchments   ! TRUE if subcatchment results reported
   logical(kind=K2) :: nodes           ! TRUE if node results reported
   logical(kind=K2) :: links           ! TRUE if link results reported
   logical(kind=K2) :: continuity      ! TRUE if continuity errors reported
   logical(kind=K2) :: flowStats       ! TRUE if routing link flow stats. reported
   logical(kind=K2) :: nodeStats       ! TRUE if routing node depth stats. reported
   logical(kind=K2) :: controls        ! TRUE if control actions reported
   integer(kind=K4) :: linesPerPage    ! number of lines printed per page
end type TRptFlags


!-------------------------------
! CUMULATIVE RUNOFF TOTALS
!-------------------------------
type TRunoffTotals
        ! All volume totals are in ft.
    double precision ::        rainfall        ! rainfall volume 
    double precision ::        evap            ! evaporation loss
    double precision ::        infil           ! infiltration loss
    double precision ::        runoff          ! runoff volume
    double precision ::        initStorage     ! inital surface storage
    double precision ::        finalStorage    ! final surface storage
    double precision ::        initSnowCover   ! initial snow cover
    double precision ::        finalSnowCover  ! final snow cover
    double precision ::        snowRemoved     ! snow removal
    double precision ::        pctError        ! continuity error (%)
end type TRunoffTotals


!--------------------------
! CUMULATIVE LOADING TOTALS
!--------------------------
type TLoadingTotals
! All loading totals are in lbs.
    double precision ::        initLoad        ! initial loading
    double precision ::        buildup         ! loading added from buildup
    double precision ::        deposition      ! loading added from wet deposition
    double precision ::        sweeping        ! loading removed by street sweeping
    double precision ::        bmpRemoval      ! loading removed by BMPs
    double precision ::        infil           ! loading removed by infiltration
    double precision ::        runoff          ! loading removed by runoff
    double precision ::        finalLoad       ! final loading
    double precision ::        pctError        ! continuity error (%)
end type TLoadingTotals


!------------------------------
! CUMULATIVE GROUNDWATER TOTALS
!------------------------------
type TGwaterTotals
! All GW flux totals are in feet.
   double precision ::        infil           ! surface infiltration
   double precision ::        upperEvap       ! upper zone evaporation loss
   double precision ::        lowerEvap       ! lower zone evaporation loss
   double precision ::        lowerPerc       ! percolation out of lower zone
   double precision ::        gwater          ! groundwater flow
   double precision ::        initStorage     ! initial groundwater storage
   double precision ::        finalStorage    ! final groundwater storage
   double precision ::        pctError        ! continuity error (%)
end type TGwaterTotals


!----------------------------
! CUMULATIVE ROUTING TOTALS
!----------------------------
type TRoutingTotals
   ! All routing totals are in ft3.
   double precision :: dwInflow         ! dry weather inflow
   double precision :: wwInflow         ! wet weather inflow
   double precision :: gwInflow         ! groundwater inflow
   double precision :: iiInflow         ! RDII inflow
   double precision :: exInflow         ! direct inflow
   double precision :: flooding         ! internal flooding
   double precision :: outflow          ! external outflow
   double precision :: reacted          ! reaction losses
   double precision :: initStorage      ! initial storage volume
   double precision :: finalStorage     ! final storage volume
   double precision :: pctError         ! continuity error
end type TRoutingTotals

!-----------------------
! SYSTEM-WIDE STATISTICS
!-----------------------
type TSysStats
    double precision ::        minTimeStep
    double precision ::        maxTimeStep
    double precision ::        avgTimeStep
    double precision ::        avgStepCount
    double precision ::        steadyStateCount
end type TSysStats


!--------------------
! RAINFALL STATISTICS
!--------------------
type TRainStats
   !DateTime    startDate
!   DateTime    endDate
   integer(kind=K8) ::        periodsRain
   integer(kind=K8) ::        periodsMissing
   integer(kind=K8) ::        periodsMalfunc
end type TRainStats

!------------------------
! SUBCATCHMENT STATISTICS
!------------------------
type TSubcatchStats
    double precision ::       precip
    double precision ::       runon
    double precision ::       evap
    double precision ::       infil
    double precision ::       runoff
    double precision ::       maxFlow         
end type TSubcatchStats


!----------------
! NODE STATISTICS
!----------------
type TNodeStats
   double precision ::        avgDepth
   double precision ::        maxDepth
   !DateTime      maxDepthDate
   double precision ::        maxDepthChange                                               !(5.0.012 - LR)
   double precision ::        volFlooded
   double precision ::        timeFlooded
   double precision ::        timeSurcharged                                               !(5.0.012 - LR)
   double precision ::        timeCourantCritical
   double precision ::        totLatFlow                                                   !(5.0.012 - LR)
   double precision ::        maxLatFlow
   double precision ::        maxInflow
   double precision ::        maxOverflow
   double precision ::        maxPondedVol                                                 !(5.0.012 - LR)
   !DateTime      maxInflowDate
!   DateTime      maxOverflowDate
end type TNodeStats


!-------------------
! STORAGE STATISTICS
!-------------------
type TStorageStats
   double precision ::        avgVol
   double precision ::        maxVol
   double precision ::        maxFlow
   double precision ::        losses                                                       !(5.0.018 - LR)
   !DateTime      maxVolDate
end type TStorageStats


!-------------------
! OUTFALL STATISTICS
!-------------------
type TOutfallStats
   double precision ::       avgFlow
   double precision ::       maxFlow
   !double*      totalLoad   
   double precision ::      totalLoad   
   integer(kind=K4) ::          totalPeriods
end type TOutfallStats


!----------------                                                             !(5.0.012 - LR)
! PUMP STATISTICS                                                             !(5.0.012 - LR)
!----------------                                                             !(5.0.012 - LR)
type TPumpStats
   double precision :: utilized                                                      !(5.0.012 - LR)
   double precision :: minFlow                                                       !(5.0.022 - LR)
   double precision :: avgFlow                                                       !(5.0.012 - LR)
   double precision :: maxFlow                                                       !(5.0.012 - LR)
   double precision :: volume                                                        !(5.0.012 - LR)
   double precision :: energy                                                        !(5.0.012 - LR)
   double precision :: offCurveLow                                                   !(5.0.022 - LR)
   double precision :: offCurveHigh                                                  !(5.0.022 - LR)
   integer(kind=K4) :: startUps                                                      !(5.0.022 - LR)
   integer(kind=K4) :: totalPeriods                                                  !(5.0.012 - LR)
end type TPumpStats


!----------------
! LINK STATISTICS
!----------------
type TLinkStats
   double precision :: maxFlow
   !DateTime      maxFlowDate
   double precision :: maxVeloc
   !DateTime      maxVelocDate
   double precision :: maxDepth
   double precision :: avgFlowChange
   double precision :: avgFroude
   double precision :: timeSurcharged
   double precision :: timeFullUpstream                                             !(5.0.012 - LR)
   double precision :: timeFullDnstream                                             !(5.0.012 - LR)
   double precision :: timeFullFlow                                                 !(5.0.012 - LR)
   double precision :: timeCapacityLimited                                          !(5.0.012 - LR)
   double precision, dimension(MAX_FLOW_CLASSES) :: timeInFlowClass
   double precision :: timeCourantCritical
   integer(kind=K8) :: flowTurns                                                    !(5.0.010 - LR)
   integer(kind=K4) :: flowTurnSign                                                 !(5.0.010 - LR)
end type TLinkStats


!-------------------------
! MAXIMUM VALUE STATISTICS
!-------------------------
type TMaxStats
   integer(kind=K4) :: objType         ! either NODE or LINK
   integer(kind=K4) :: index           ! node or link index
   double precision :: value           ! value of node or link statistic
end type TMaxStats 


!------------------
! REPORT FIELD INFO
!------------------
type TRptField
   character*80 :: FieldName        ! name of reported variable 
   character*80 :: Units       ! units of reported variable
   !char          Enabled         ! TRUE if appears in report table
   logical :: Enabled         ! TRUE if appears in report table
   integer(kind=K4) :: FieldPrecision       ! number of decimal places when reported
end type TRptField

end module
