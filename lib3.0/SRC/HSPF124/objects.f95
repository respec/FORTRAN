!include 'consts.f95'
!include 'enums.f95'
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
! -DateTime data type replacement, it is just a double number
! -Array in a derived type must be fixed size !TODO: dim fixed
! -commented out the use of MathExpr and TGrnAmpt type
!-----------------------------------------------------------------------------

!#include "mathexpr.h"
!#include "infil.h"
use consts
use enums
use infil
implicit none

integer, parameter :: dpo = kind(1.d0)
private K2, K4, K8 !, MAXFNAME,MAXMSG, MAX_FLOW_CLASSES
integer, parameter :: K2 = selected_int_kind(2) !kind= 1
integer, parameter :: K4 = selected_int_kind(4) !kind= 2
integer, parameter :: K8 = selected_int_kind(8) !kind =4
!integer, parameter :: MAXFNAME = 259            ! Max. # characters in file name
!integer, parameter :: MAXMSG = 1024           ! Max. # characters in message text
!integer(kind=K2), parameter :: MAX_FLOW_CLASSES = 7
!

public
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
   real(kind=dpo) :: x
   real(kind=dpo) :: y
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
   real(kind=dpo) :: dxMin           ! smallest x-value interval                 !(5.0.014 - LR)
   real(kind=dpo) :: lastDate        ! last input date for time series
   real(kind=dpo) :: x1, x2          ! current bracket on x-values
   real(kind=dpo) :: y1, y2          ! current bracket on y-values
   type(TTableEntry) ::  firstEntry      ! first data point
   type(TTableEntry) ::  lastEntry       ! last data point
   type(TTableEntry) ::  thisEntry       ! current data point
   type(TFile) ::  extfile            ! external data file                        !(5.0.014 - LR)
end type TTable


!-----------------
! RAIN GAGE OBJECT
!-----------------
type TGage
   character(len=20) :: ID              ! raingage name
   integer(kind=K4) :: dataSource      ! data from time series or file 
   integer(kind=K4) :: tSeries         ! rainfall data time series index
   character(len=MAXFNAME +1) :: fname ! name of rainfall data file
   character(len=MAXMSG +1) :: staID ! station number
   real(kind=dpo) :: startFileDate   ! starting date of data read from file
   real(kind=dpo) :: endFileDate     ! ending date of data read from file
   integer(kind=K4) :: rainType        ! intensity, volume, cumulative
   integer(kind=K4) :: rainInterval    ! recording time interval (seconds)
   integer(kind=K4) :: rainUnits       ! rain depth units (US or SI)
   real(kind=dpo) :: snowFactor      ! snow catch deficiency correction

   integer(kind=K8) :: startFilePos    ! starting byte position in Rain file
   integer(kind=K8) :: endFilePos      ! ending byte position in Rain file
   integer(kind=K8) :: currentFilePos  ! current byte position in Rain file
   real(kind=dpo) :: rainAccum       ! cumulative rainfall
   real(kind=dpo) :: unitsFactor     ! units conversion factor (to inches or mm)
   real(kind=dpo) :: startDate       ! start date of current rainfall
   real(kind=dpo) :: endDate         ! end date of current rainfall
   real(kind=dpo) :: nextDate        ! next date with recorded rainfall
   real(kind=dpo) :: rainfall        ! current rainfall (in/hr or mm/hr)
   real(kind=dpo) :: nextRainfall    ! next rainfall (in/hr or mm/hr)
   real(kind=dpo) :: reportRainfall  ! rainfall value used for reported results
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
   real(kind=dpo) :: fileStartDate   ! starting date of data read from file
   real(kind=dpo) ::        elev            ! elev. of study area (ft)
   real(kind=dpo) ::        anglat          ! latitude (degrees)
   real(kind=dpo) ::        dtlong          ! longitude correction (hours)

   real(kind=dpo) ::        ta              ! air temperature (deg F)
   real(kind=dpo) ::        tmax            ! previous day's max. temp. (deg F)
   real(kind=dpo) ::        ea              ! saturation vapor pressure (in Hg)
   real(kind=dpo) ::        gamma           ! psychrometric constant
   real(kind=dpo) ::        tanAnglat       ! tangent of latitude angle
end type TTemp


!-----------------
! WINDSPEED OBJECT
!-----------------
type TWind
   integer(kind=K4) :: Datatype             ! monthly or file data
   real(kind=dpo), dimension(12) :: aws          ! monthly avg. wind speed (mph)
   real(kind=dpo) :: ws              ! wind speed (mph)
end type  TWind


!------------
! SNOW OBJECT
!------------
type TSnow
    real(kind=dpo) ::        snotmp          ! temp. dividing rain from snow (deg F)
    real(kind=dpo) ::        tipm            ! antecedent temp. index parameter
    real(kind=dpo) ::        rnm             ! ratio of neg. melt to melt coeff.
    real(kind=dpo), dimension(2,10) :: adc      ! areal depletion curves (pervious & 
                                  ! imperv. area curves w/ 10 pts.each)
    real(kind=dpo) ::        season          ! snowmelt season
    real(kind=dpo) ::        removed         ! total snow plowed out of system (ft3)
end type TSnow


!-------------------
! EVAPORATION OBJECT
!-------------------
type TEvap
    integer(kind=K4) :: Datatype            ! type of evaporation data
    integer(kind=K4) :: tSeries         ! time series index
    real(kind=dpo), dimension(12) :: monthlyEvap ! monthly evaporation values
    real(kind=dpo), dimension(12) :: panCoeff    ! monthly pan coeff. values
    integer(kind=K4) :: recoveryPattern ! soil recovery factor pattern              !(5.0.014 - LR)
    !integer :: dryOnly         ! true if evaporation only in dry periods   !(5.0.019 - LR)
    logical(kind=K2) :: dryOnly         ! true if evaporation only in dry periods   !(5.0.019 - LR)
    real(kind=dpo) :: rate            ! current evaporation rate (ft/sec)
    real(kind=dpo) :: recoveryFactor  ! current soil recovery factor              !(5.0.014 - LR) 
end type TEvap

!--------------------------------------------------------  
!  Infiltration objects are now declared in infil.h                           !(5.0.019 - LR)
!--------------------------------------------------------  

!-------------------
! AQUIFER OBJECT
!-------------------
type TAquifer
    character(len=20) ::       ID               ! aquifer name
    real(kind=dpo) ::      porosity         ! soil porosity
    real(kind=dpo) ::      wiltingPoint     ! soil wilting point
    real(kind=dpo) ::      fieldCapacity    ! soil field capacity
    real(kind=dpo) ::      conductivity     ! soil hyd. conductivity (ft/sec)
    real(kind=dpo) ::      conductSlope     ! slope of conductivity v. moisture curve
    real(kind=dpo) ::      tensionSlope     ! slope of tension v. moisture curve
    real(kind=dpo) ::      upperEvapFrac    ! evaporation available in upper zone
    real(kind=dpo) ::      lowerEvapDepth   ! evap depth existing in lower zone (ft)
    real(kind=dpo) ::      lowerLossCoeff   ! coeff. for losses to deep GW (ft/sec)
    real(kind=dpo) ::      bottomElev       ! elevation of bottom of aquifer (ft)
    real(kind=dpo) ::      waterTableElev   ! initial water table elevation (ft)
    real(kind=dpo) ::      upperMoisture    ! initial moisture content of unsat. zone
end type TAquifer


!------------------------
! GROUNDWATER OBJECT
!------------------------
type TGroundwater
    integer(kind=K4) ::           aquifer        ! index of associated gw aquifer 
    integer(kind=K4) ::           node           ! index of node receiving gw flow
    real(kind=dpo) ::        surfElev       ! elevation of ground surface (ft)
    real(kind=dpo) ::        a1, b1         ! ground water outflow coeff. & exponent
    real(kind=dpo) ::        a2, b2         ! surface water outflow coeff. & exponent
    real(kind=dpo) ::        a3             ! surf./ground water interaction coeff.
    real(kind=dpo) ::        fixedDepth     ! fixed surface water water depth (ft)
    real(kind=dpo) ::        nodeElev       ! elevation of receiving node invert (ft)
    real(kind=dpo) ::        theta          ! upper zone moisture content
    real(kind=dpo) ::        lowerDepth     ! depth of saturated zone (ft)
    real(kind=dpo) ::        oldFlow        ! gw outflow from previous time period (cfs)
    real(kind=dpo) ::        newFlow        ! gw outflow from current time period (cfs)
    real(kind=dpo) ::        maxInfilVol    ! max. infil. upper zone can accept (ft)    !(5.0.019 - LR)
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
   real(kind=dpo) ::        snn             ! fraction of impervious area plowable
   real(kind=dpo), dimension(3) :: si           ! snow depth for 100% cover
   real(kind=dpo), dimension(3) :: dhmin        ! min. melt coeff. for each surface (ft/sec-F)
   real(kind=dpo), dimension(3) :: dhmax        ! max. melt coeff. for each surface (ft/sec-F)
   real(kind=dpo), dimension(3) :: tbase        ! base temp. for melting (F)
   real(kind=dpo), dimension(3) :: fwfrac       ! free water capacity / snow depth
   real(kind=dpo), dimension(3) :: wsnow        ! initial snow depth on each surface (ft)
   real(kind=dpo), dimension(3) :: fwnow        ! initial free water in snow pack (ft)
   real(kind=dpo) :: weplow          ! depth at which plowing begins (ft)
   real(kind=dpo), dimension(5) :: sfrac        ! fractions moved to other areas by plowing
   integer(kind=K4) :: toSubcatch      ! index of subcatch receiving plowed snow
   real(kind=dpo), dimension(3) :: dhm          ! melt coeff. for each surface (ft/sec-F)
end type TSnowmelt


!----------------
! SNOWPACK OBJECT
!----------------
! Snowpack objects describe the state of the snow melt process on each
! of 3 types of snow surfaces.
type TSnowpack
   integer(kind=K4) ::           snowmeltIndex   ! index of snow melt parameter set
   real(kind=dpo), dimension(3) :: fArea        ! fraction of total area of each surface
   real(kind=dpo), dimension(3) :: wsnow        ! depth of snow pack (ft)
   real(kind=dpo), dimension(3) :: fw           ! depth of free water in snow pack (ft)
   real(kind=dpo), dimension(3) :: coldc        ! cold content of snow pack
   real(kind=dpo), dimension(3) :: ati          ! antecedent temperature index (deg F)
   real(kind=dpo), dimension(3) :: sba          ! initial ASC of linear ADC
   real(kind=dpo), dimension(3) :: awe          ! initial AWESI of linear ADC
   real(kind=dpo), dimension(3) :: sbws         ! final AWESI of linear ADC
   real(kind=dpo), dimension(3) :: imelt        ! immediate melt (ft)
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
   real(kind=dpo) :: fOutlet         ! fraction of outflow to outlet
   real(kind=dpo) :: N               ! Manning's n
   real(kind=dpo) :: fArea           ! fraction of total area
   real(kind=dpo) :: dStore          ! depression storage (ft)
   real(kind=dpo) :: alpha           ! overland flow factor
   real(kind=dpo) :: inflow          ! inflow rate (ft/sec)
   real(kind=dpo) :: runoff          ! runoff rate (ft/sec)
   real(kind=dpo) :: depth           ! depth of surface runoff (ft)
   end type TSubarea

!-------------------------
! LAND AREA LANDUSE FACTOR
!-------------------------
type TLandFactor
   real(kind=dpo) ::       fraction        ! fraction of land area with land use
   !double*       buildup         ! array of buildups for each pollutant
   !real(kind=dpo), dimension(:) :: buildup         ! array of buildups for each pollutant
   real(kind=dpo), dimension(12) :: buildup         ! array of buildups for each pollutant, TODO:dim fixed
   real(kind=dpo) :: lastSwept       ! date/time of last street sweeping
   end type TLandFactor


!--------------------
! SUBCATCHMENT OBJECT
!--------------------
type TSubcatch
   character(len=20) ::         ID              ! subcatchment name
   logical ::          rptFlag         ! reporting flag
   integer(kind=K4) ::           gage            ! raingage index
   integer(kind=K4) ::           outNode         ! outlet node index
   integer(kind=K4) ::           outSubcatch     ! outlet subcatchment index
   integer(kind=K4) ::           infil           ! infiltration object index
   type(TSubarea), dimension(3) :: subArea      ! sub-area data
   real(kind=dpo) ::        width           ! overland flow width (ft)
   real(kind=dpo) ::        area            ! area (ft2)
   real(kind=dpo) ::        fracImperv      ! fraction impervious
   real(kind=dpo) ::        slope           ! slope (ft/ft)
   real(kind=dpo) ::        curbLength      ! total curb length (ft)
   !double*       initBuildup     ! initial pollutant buildup (mass/ft2)
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) ::       initBuildup     ! initial pollutant buildup (mass/ft2)

   !TLandFactor*  landFactor      ! array of land use factors
   !TGroundwater* groundwater     ! associated groundwater data
   !TSnowpack*    snowpack        ! associated snow pack data
   type(TLandFactor), dimension(12) ::  landFactor      ! array of land use factors !TODO:dim fixed
   type(TGroundwater), dimension(12) :: groundwater     ! associated groundwater data !TODO:dim fixed
   type(TSnowpack), dimension(12) ::    snowpack        ! associated snow pack data !TODO:dim fixed

   real(kind=dpo) ::        lidArea         ! area devoted to LIDs (ft2)                !(5.0.019 - LR)
   real(kind=dpo) ::        rainfall        ! current rainfall (ft/sec)
   real(kind=dpo) ::        losses          ! current infil + evap losses (ft/sec)
   real(kind=dpo) ::        runon           ! runon from other subcatchments (cfs)
   real(kind=dpo) ::        oldRunoff       ! previous runoff (cfs)
   real(kind=dpo) ::        newRunoff       ! current runoff (cfs)
   real(kind=dpo) ::        oldSnowDepth    ! previous snow depth (ft)
   real(kind=dpo) ::        newSnowDepth    ! current snow depth (ft)

   !double*  oldQual         ! previous runoff quality (mass/L)
   !double*  newQual         ! current runoff quality (mass/L)
   !double*  pondedQual      ! ponded surface water quality (mass/ft3)
   !double*  totalLoad       ! total washoff load (lbs or kg)
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) :: oldQual         ! previous runoff quality (mass/L)
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) :: newQual         ! current runoff quality (mass/L)
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) :: pondedQual      ! ponded surface water quality (mass/ft3)
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) :: totalLoad       ! total washoff load (lbs or kg)

end type  TSubcatch


!-----------------------
! TIME PATTERN DATA
!-----------------------
type TPattern
   character(len=20) ::        ID               ! time pattern name
   integer(kind=K4) ::          Datatype             ! time pattern type code
   integer(kind=K4) ::          count            ! number of factors
   real(kind=dpo), dimension(24) :: factor       ! time pattern factors
end type TPattern


!------------------------------
! DIRECT EXTERNAL INFLOW OBJECT
!------------------------------
type TExtInflow
   integer(kind=K4) ::            param         ! pollutant index (flow = -1)
   integer(kind=K4) ::            Datatype          ! CONCEN or MASS
   integer(kind=K4) ::            tSeries       ! index of inflow time series
   integer(kind=K4) ::            basePat       ! baseline time pattern                      !(5.0.014 - LR)
   real(kind=dpo) ::         cFactor       ! units conversion factor for mass inflow
   real(kind=dpo) ::         baseline      ! constant baseline value
   real(kind=dpo) ::         sFactor       ! time series scaling factor
   !struct ExtInflow* next       ! pointer to next inflow data object
   type(TExtInflow), pointer :: next       ! pointer to next inflow data object
end type TExtInflow 

!-------------------------------
! DRY WEATHER FLOW INFLOW OBJECT
!-------------------------------
type TDwfInflow
   integer(kind=K4) :: param          ! pollutant index (flow = -1)
   real(kind=dpo) :: avgValue       ! average value (cfs or concen.)
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
   real(kind=dpo) :: area            ! area of sewershed (ft2)
end type TRdiiInflow


!-----------------------------
! UNIT HYDROGRAPH GROUP OBJECT
!-----------------------------
type TUnitHyd
   character(len=20) :: ID     ! name of the unit hydrograph object
   integer(kind=K4) :: rainGage        ! index of rain gage
   real(kind=dpo), dimension(12, 3) :: iaMax   ! max. initial abstraction (IA) (in or mm)  !(5.0.015 - LR)
   real(kind=dpo), dimension(12, 3) :: iaRecov ! IA recovery rate (in/day or mm/day)       !(5.0.015 - LR)
   real(kind=dpo), dimension(12, 3) :: iaInit  ! starting IA (in or mm)                    !(5.0.015 - LR)
   real(kind=dpo), dimension(12, 3) :: r       ! fraction of rainfall becoming I&I
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
   logical(kind=K2) :: rptFlag         ! reporting flag
   real (kind=dpo) ::        invertElev      ! invert elevation (ft)
   real(kind=dpo) ::        initDepth       ! initial storage level (ft)
   real(kind=dpo) ::        fullDepth       ! dist. from invert to surface (ft)
   real(kind=dpo) ::        surDepth        ! added depth under surcharge (ft)
   real(kind=dpo) ::        pondedArea      ! area filled by ponded water (ft2)

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
   real(kind=dpo) ::        crownElev       ! top of highest connecting conduit (ft)
   real(kind=dpo) ::        inflow          ! total inflow (cfs)
   real(kind=dpo) ::        outflow         ! total outflow (cfs)
   real(kind=dpo) ::        oldVolume       ! previous volume (ft3)
   real(kind=dpo) ::        newVolume       ! current volume (ft3)
   real(kind=dpo) ::        fullVolume      ! max. storage available (ft3)
   real(kind=dpo) ::        overflow        ! overflow rate (cfs)
   real(kind=dpo) ::        oldDepth        ! previous water depth (ft)
   real(kind=dpo) ::        newDepth        ! current water depth (ft)
   real(kind=dpo) ::        oldLatFlow      ! previous lateral inflow (cfs)
   real(kind=dpo) ::        newLatFlow      ! current lateral inflow (cfs)

   !double*       oldQual         ! previous quality state
   !double*       newQual         ! current quality state
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) :: oldQual         ! previous quality state
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) :: newQual         ! current quality state

   !double*       wStored  originally commented out    !(5.0.018 - LR)

   real(kind=dpo) ::        oldFlowInflow   ! previous flow inflow
   real(kind=dpo) ::        oldNetInflow    ! previous net inflow
end type TNode


!---------------
! OUTFALL OBJECT
!---------------
type TOutfall
   integer(kind=K4) ::        datatype               ! outfall type code
   logical(kind=K2) ::       hasFlapGate        ! true if contains flap gate
   real(kind=dpo) ::     fixedStage         ! fixed outfall stage (ft)
   integer(kind=K4) ::        tideCurve          ! index of tidal stage curve
   integer(kind=K4) ::        stageSeries        ! index of outfall stage time series
end type TOutfall


!--------------------
! STORAGE UNIT OBJECT
!--------------------
type TStorage
   real(kind=dpo) ::      fEvap             ! fraction of evaporation realized
   real(kind=dpo) ::      aConst            ! surface area at zero height (ft2)
   real(kind=dpo) ::      aCoeff            ! coeff. of area v. height curve
   real(kind=dpo) ::      aExpon            ! exponent of area v. height curve
   integer(kind=K4) ::         aCurve            ! index of tabulated area v. height curve
   !TGrnAmpt*   infil             ! ptr. to infiltration object               !(5.0.015 - LR)
   type(TGrnAmpt) :: infil        ! ptr. to infiltration object               !(5.0.015 - LR) !TODO: add this later
   real(kind=dpo) ::      hrt               ! hydraulic residence time (sec)
   real(kind=dpo) ::      evapLoss          ! evaporation loss (ft3)                    !(5.0.019 - LR)
   real(kind=dpo) ::      losses            ! evap + infil losses (ft3)                 !(5.0.018 - LR)
end type TStorage


!--------------------
! FLOW DIVIDER OBJECT
!--------------------
type TDivider
   integer(kind=K4) ::         link              ! index of link with diverted flow
   integer(kind=K4) ::         Datatype              ! divider type code
   real(kind=dpo) ::      qMin              ! minimum inflow for diversion (cfs)
   real(kind=dpo) ::      qMax              ! flow when weir is full (cfs)
   real(kind=dpo) ::      dhMax             ! height of weir (ft)
   real(kind=dpo) ::      cWeir             ! weir discharge coeff.
   integer(kind=K4) ::         flowCurve         ! index of inflow v. diverted flow curve
end type TDivider


!-----------------------------
! CROSS SECTION DATA STRUCTURE
!-----------------------------
type TXsect
   integer(kind=K2) ::           datatype            ! type code of cross section shape
   integer(kind=K4) ::           culvertCode     ! type of culvert (if any)                  !(5.0.014 - LR)
   integer(kind=K4) ::           transect        ! index of transect/shape (if applicable)   !(5.0.010 - LR)
   real(kind=dpo) ::        yFull           ! depth when full (ft)
   real(kind=dpo) ::        wMax            ! width at widest point (ft)
   real(kind=dpo) ::        aFull           ! area when full (ft2)
   real(kind=dpo) ::        rFull           ! hyd. radius when full (ft)
   real(kind=dpo) ::        sFull           ! section factor when full (ft^4/3)
   real(kind=dpo) ::        sMax            ! section factor at max. flow (ft^4/3)

   ! These variables have different meanings depending on section shape
   real(kind=dpo) ::        yBot            ! depth of bottom section
   real(kind=dpo) ::        aBot            ! area of bottom section
   real(kind=dpo) ::        sBot            ! slope of bottom section
   real(kind=dpo) ::        rBot            ! radius of bottom section
end type TXsect


!--------------------------------------
! CROSS SECTION TRANSECT DATA STRUCTURE
!--------------------------------------
integer, parameter ::  N_TRANSECT_TBL =  51       ! size of transect geometry tables          !(5.0.010 - LR)
type TTransect
    character(len=20) ::        ID                        ! section ID
    real(kind=dpo) ::       yFull                     ! depth when full (ft)
    real(kind=dpo) ::       aFull                     ! area when full (ft2)
    real(kind=dpo) ::       rFull                     ! hyd. radius when full (ft)
    real(kind=dpo) ::       wMax                      ! width at widest point (ft)
    real(kind=dpo) ::       sMax                      ! section factor at max. flow (ft^4/3)
    real(kind=dpo) ::       aMax                      ! area at max. flow (ft2)
    real(kind=dpo) ::       lengthFactor              ! floodplain / channel length     !(5.0.015 - LR)

    real(kind=dpo) ::       roughness                 ! Manning's n
    real(kind=dpo), dimension(N_TRANSECT_TBL) :: areaTbl   ! table of area v. depth
    real(kind=dpo), dimension(N_TRANSECT_TBL) :: hradTbl   ! table of hyd. radius v. depth
    real(kind=dpo), dimension(N_TRANSECT_TBL) :: widthTbl  ! table of top width v. depth
    integer(kind=K4) ::          nTbl                      ! size of geometry tables
end type TTransect


!-------------------------------------                                        !(5.0.010 - LR)
! CUSTOM CROSS SECTION SHAPE STRUCTURE                                        !(5.0.010 - LR)
!-------------------------------------                                        !(5.0.010 - LR)
integer, parameter :: N_SHAPE_TBL = 51           ! size of shape geometry tables             !(5.0.010 - LR)
type TShape
    integer(kind=K4) ::          curve                     ! index of shape curve            !(5.0.010 - LR) 
    integer(kind=K4) ::          nTbl                      ! size of geometry tables         !(5.0.010 - LR)
    real(kind=dpo) ::       aFull                     ! area when full                  !(5.0.010 - LR)
    real(kind=dpo) ::       rFull                     ! hyd. radius when full           !(5.0.010 - LR)
    real(kind=dpo) ::       wMax                      ! max. width                      !(5.0.010 - LR)
    real(kind=dpo) ::       sMax                      ! max. section factor             !(5.0.010 - LR)
    real(kind=dpo) ::       aMax                      ! area at max. section factor     !(5.0.010 - LR)
    real(kind=dpo), dimension(N_SHAPE_TBL) :: areaTbl      ! table of area v. depth          !(5.0.010 - LR)
    real(kind=dpo), dimension(N_SHAPE_TBL) :: hradTbl      ! table of hyd. radius v. depth   !(5.0.010 - LR)
    real(kind=dpo), dimension(N_SHAPE_TBL) :: widthTbl     ! table of top width v. depth     !(5.0.010 - LR)
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
   real(kind=dpo) ::        offset1         ! ht. above start node invert (ft)          !(5.0.012 - LR)
   real(kind=dpo) ::        offset2         ! ht. above end node invert (ft)            !(5.0.012 - LR)
   type(TXsect) ::        xsect           ! cross section data
   real(kind=dpo) ::        q0              ! initial flow (cfs)
   real(kind=dpo) ::        qLimit          ! constraint on max. flow (cfs)
   real(kind=dpo) ::        cLossInlet      ! inlet loss coeff.
   real(kind=dpo) ::        cLossOutlet     ! outlet loss coeff.
   real(kind=dpo) ::        cLossAvg        ! avg. loss coeff.
   !integer ::           hasFlapGate     ! true if flap gate present
   logical(kind=K2) ::           hasFlapGate     ! true if flap gate present

   real(kind=dpo) :: oldFlow         ! previous flow rate (cfs)
   real(kind=dpo) :: newFlow         ! current flow rate (cfs)
   real(kind=dpo) :: oldDepth        ! previous flow depth (ft)
   real(kind=dpo) :: newDepth        ! current flow depth (ft)
   real(kind=dpo) :: oldVolume       ! previous flow volume (ft3)
   real(kind=dpo) :: newVolume       ! current flow volume (ft3)
   real(kind=dpo) :: qFull           ! flow when full (cfs)
   real(kind=dpo) :: setting         ! current control setting                   !(5.0.010 - LR)
   real(kind=dpo) :: targetSetting   ! target control setting                    !(5.0.010 - LR)
   real(kind=dpo) :: froude          ! Froude number
   !double*       oldQual         ! previous quality state
   !double*       newQual         ! current quality state
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) :: oldQual         ! previous quality state
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) :: newQual         ! current quality state
   integer(kind=K4) :: flowClass       ! flow classification
   real(kind=dpo) ::        dqdh            ! change in flow w.r.t. head (ft2/sec)
   !signed char   direction       ! flow direction flag
   integer(kind=K4) ::   direction       ! flow direction flag
   logical ::          isClosed        ! flap gate closed flag
end type TLink


!---------------
! CONDUIT OBJECT
!---------------
type TConduit
   real(kind=dpo) ::        clength          ! conduit length (ft)
   real(kind=dpo) ::        roughness       ! Manning's n
   !char          barrels         ! number of barrels
   integer(kind=K4) :: barrels         ! number of barrels !kind = 1

   real(kind=dpo) :: modLength       ! modified conduit length (ft)
   real(kind=dpo) :: roughFactor     ! roughness factor for DW routing
   real(kind=dpo) :: slope           ! slope
   real(kind=dpo) :: beta            ! discharge factor
   real(kind=dpo) :: qMax            ! max. flow (cfs)
   real(kind=dpo) :: a1, a2          ! upstream & downstream areas (ft2)
   real(kind=dpo) :: q1, q2          ! upstream & downstream flows per barrel (cfs)
   real(kind=dpo) :: q1Old, q2Old    ! previous values of q1 & q2 (cfs)
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
   real(kind=dpo) ::        initSetting     ! initial speed setting                     !(5.0.010 - LR)
   real(kind=dpo) ::        yOn             ! startup depth (ft)                        !(5.0.010 - LR)
   real(kind=dpo) ::        yOff            ! shutoff depth (ft)                        !(5.0.010 - LR)
   real(kind=dpo) ::        xMin            ! minimum pt. on pump curve                 !(5.0.012 - LR)
   real(kind=dpo) ::        xMax            ! maximum pt. on pump curve                 !(5.0.012 - LR)
end type TPump


!---------------
! ORIFICE OBJECT
!---------------
type TOrifice
   integer(kind=K4) ::           datatype            ! orifice type code
   integer(kind=K4) ::           shape           ! orifice shape code
   real(kind=dpo) :: cDisch          ! discharge coeff.
   real(kind=dpo) :: orate           ! time to open/close (sec)

   real(kind=dpo) :: cOrif           ! coeff. for orifice flow (ft^2.5/sec)      !(5.0.012 - LR)
   real(kind=dpo) :: hCrit           ! inlet depth where weir flow begins (ft)   !(5.0.012 - LR)
   real(kind=dpo) :: cWeir           ! coeff. for weir flow (cfs)                !(5.0.012 - LR)
   real(kind=dpo) :: length          ! equivalent length (ft)
   real(kind=dpo) :: surfArea        ! equivalent surface area (ft2)
end type TOrifice


!------------
! WEIR OBJECT
!------------
type TWeir
   integer(kind=K4) ::           datatype            ! weir type code
   integer(kind=K4) ::           shape           ! weir shape code
   !double        crestHt         ! crest height above node invert (ft)     !(5.0.012 - LR)
   real(kind=dpo) ::        cDisch1         ! discharge coeff.
   real(kind=dpo) ::        cDisch2         ! discharge coeff. for ends
   real(kind=dpo) ::        endCon          ! end contractions

   real(kind=dpo) ::        cSurcharge      ! cDisch for equiv. orifice under surcharge
   real(kind=dpo) ::        length          ! equivalent length (ft)
   real(kind=dpo) ::        slope           ! slope for Vnotch & Trapezoidal weirs
   real(kind=dpo) ::        surfArea        ! equivalent surface area (ft2)
end type TWeir


!---------------------
! OUTLET DEVICE OBJECT
!---------------------
type TOutlet
    !double       crestHt         ! crest ht. above node invert (ft)        !(5.0.012 - LR)
    real(kind=dpo) ::       qCoeff          ! discharge coeff.
    real(kind=dpo) ::       qExpon          ! discharge exponent
    integer(kind=K4) ::          qCurve          ! index of discharge rating curve
    integer(kind=K4) ::          curveType       ! rating curve type                         !(5.0.014 - LR)
end type TOutlet


!-----------------
! POLLUTANT OBJECT
!-----------------
type TPollut
   character(len=20) ::        ID              ! Pollutant ID
   integer(kind=K4) ::           units           ! units
   real(kind=dpo) ::        mcf             ! mass conversion factor
   real(kind=dpo) ::        dwfConcen       ! dry weather sanitary flow concen.         !(5.0.017 - LR)
   real(kind=dpo) ::        pptConcen       ! precip. concen.
   real(kind=dpo) ::        gwConcen        ! groundwater concen.
   real(kind=dpo) ::        rdiiConcen      ! RDII concen.
   real(kind=dpo) ::        kDecay          ! decay constant (1/sec)
   integer(kind=K4) ::           coPollut        ! co-pollutant index
   real(kind=dpo) ::        coFraction      ! co-pollutant fraction
   !int           snowOnly        ! TRUE if buildup occurs only under snow
   logical :: snowOnly        ! TRUE if buildup occurs only under snow
end type TPollut


!------------------------
! BUILDUP FUNCTION OBJECT
!------------------------
type TBuildup
   integer(kind=K4) :: normalizer      ! normalizer code (area or curb length)
   integer(kind=K4) :: funcType        ! buildup function type code
   real(kind=dpo), dimension(3) :: coeff        ! coeffs. of buildup function
   real(kind=dpo) ::        maxDays         ! time to reach max. buildup (days)
end type TBuildup


!------------------------
! WASHOFF FUNCTION OBJECT
!------------------------
type TWashoff
   integer(kind=K4) ::           funcType        ! washoff function type code
   real(kind=dpo) ::        coeff           ! function coeff.
   real(kind=dpo) ::        expon           ! function exponent
   real(kind=dpo) ::        sweepEffic      ! street sweeping fractional removal
   real(kind=dpo) ::        bmpEffic        ! best mgt. practice fractional removal
end type TWashoff


!---------------
! LANDUSE OBJECT
!---------------
type TLanduse
   character(len=20) ::         ID              ! landuse name
   real(kind=dpo) ::        sweepInterval   ! street sweeping interval (days)
   real(kind=dpo) ::        sweepRemoval    ! fraction of buildup available for sweeping
   real(kind=dpo) ::        sweepDays0      ! days since last sweeping at start

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
   integer(kind=K2) :: subcatchments   ! TRUE if subcatchment results reported
   integer(kind=K2) :: nodes           ! TRUE if node results reported
   integer(kind=K2) :: links           ! TRUE if link results reported
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
    real(kind=dpo) ::        rainfall        ! rainfall volume 
    real(kind=dpo) ::        evap            ! evaporation loss
    real(kind=dpo) ::        infil           ! infiltration loss
    real(kind=dpo) ::        runoff          ! runoff volume
    real(kind=dpo) ::        initStorage     ! inital surface storage
    real(kind=dpo) ::        finalStorage    ! final surface storage
    real(kind=dpo) ::        initSnowCover   ! initial snow cover
    real(kind=dpo) ::        finalSnowCover  ! final snow cover
    real(kind=dpo) ::        snowRemoved     ! snow removal
    real(kind=dpo) ::        pctError        ! continuity error (%)
end type TRunoffTotals


!--------------------------
! CUMULATIVE LOADING TOTALS
!--------------------------
type TLoadingTotals
! All loading totals are in lbs.
    real(kind=dpo) ::        initLoad        ! initial loading
    real(kind=dpo) ::        buildup         ! loading added from buildup
    real(kind=dpo) ::        deposition      ! loading added from wet deposition
    real(kind=dpo) ::        sweeping        ! loading removed by street sweeping
    real(kind=dpo) ::        bmpRemoval      ! loading removed by BMPs
    real(kind=dpo) ::        infil           ! loading removed by infiltration
    real(kind=dpo) ::        runoff          ! loading removed by runoff
    real(kind=dpo) ::        finalLoad       ! final loading
    real(kind=dpo) ::        pctError        ! continuity error (%)
end type TLoadingTotals


!------------------------------
! CUMULATIVE GROUNDWATER TOTALS
!------------------------------
type TGwaterTotals
! All GW flux totals are in feet.
   real(kind=dpo) ::        infil           ! surface infiltration
   real(kind=dpo) ::        upperEvap       ! upper zone evaporation loss
   real(kind=dpo) ::        lowerEvap       ! lower zone evaporation loss
   real(kind=dpo) ::        lowerPerc       ! percolation out of lower zone
   real(kind=dpo) ::        gwater          ! groundwater flow
   real(kind=dpo) ::        initStorage     ! initial groundwater storage
   real(kind=dpo) ::        finalStorage    ! final groundwater storage
   real(kind=dpo) ::        pctError        ! continuity error (%)
end type TGwaterTotals


!----------------------------
! CUMULATIVE ROUTING TOTALS
!----------------------------
type TRoutingTotals
   ! All routing totals are in ft3.
   real(kind=dpo) :: dwInflow         ! dry weather inflow
   real(kind=dpo) :: wwInflow         ! wet weather inflow
   real(kind=dpo) :: gwInflow         ! groundwater inflow
   real(kind=dpo) :: iiInflow         ! RDII inflow
   real(kind=dpo) :: exInflow         ! direct inflow
   real(kind=dpo) :: flooding         ! internal flooding
   real(kind=dpo) :: outflow          ! external outflow
   real(kind=dpo) :: reacted          ! reaction losses
   real(kind=dpo) :: initStorage      ! initial storage volume
   real(kind=dpo) :: finalStorage     ! final storage volume
   real(kind=dpo) :: pctError         ! continuity error
end type TRoutingTotals

!-----------------------
! SYSTEM-WIDE STATISTICS
!-----------------------
type TSysStats
    real(kind=dpo) ::        minTimeStep
    real(kind=dpo) ::        maxTimeStep
    real(kind=dpo) ::        avgTimeStep
    real(kind=dpo) ::        avgStepCount
    real(kind=dpo) ::        steadyStateCount
end type TSysStats


!--------------------
! RAINFALL STATISTICS
!--------------------
type TRainStats
   real(kind=dpo) :: startDate
   real(kind=dpo) :: endDate
   integer(kind=K8) ::        periodsRain
   integer(kind=K8) ::        periodsMissing
   integer(kind=K8) ::        periodsMalfunc
end type TRainStats

!------------------------
! SUBCATCHMENT STATISTICS
!------------------------
type TSubcatchStats
    real(kind=dpo) ::       precip
    real(kind=dpo) ::       runon
    real(kind=dpo) ::       evap
    real(kind=dpo) ::       infil
    real(kind=dpo) ::       runoff
    real(kind=dpo) ::       maxFlow         
end type TSubcatchStats


!----------------
! NODE STATISTICS
!----------------
type TNodeStats
   real(kind=dpo) ::        avgDepth
   real(kind=dpo) ::        maxDepth
   real(kind=dpo) :: maxDepthDate
   real(kind=dpo) ::        maxDepthChange                                               !(5.0.012 - LR)
   real(kind=dpo) ::        volFlooded
   real(kind=dpo) ::        timeFlooded
   real(kind=dpo) ::        timeSurcharged                                               !(5.0.012 - LR)
   real(kind=dpo) ::        timeCourantCritical
   real(kind=dpo) ::        totLatFlow                                                   !(5.0.012 - LR)
   real(kind=dpo) ::        maxLatFlow
   real(kind=dpo) ::        maxInflow
   real(kind=dpo) ::        maxOverflow
   real(kind=dpo) ::        maxPondedVol                                                 !(5.0.012 - LR)
   real(kind=dpo) :: maxInflowDate
   real(kind=dpo) :: maxOverflowDate
end type TNodeStats


!-------------------
! STORAGE STATISTICS
!-------------------
type TStorageStats
   real(kind=dpo) ::        avgVol
   real(kind=dpo) ::        maxVol
   real(kind=dpo) ::        maxFlow
   real(kind=dpo) ::        losses                                                       !(5.0.018 - LR)
   real(kind=dpo) :: maxVolDate
end type TStorageStats


!-------------------
! OUTFALL STATISTICS
!-------------------
type TOutfallStats
   real(kind=dpo) ::       avgFlow
   real(kind=dpo) ::       maxFlow
   real(kind=dpo), dimension(MAX_NUM_POLLUTANTS) :: totalLoad !allocatable
   integer(kind=K4) ::          totalPeriods
end type TOutfallStats


!----------------                                                             !(5.0.012 - LR)
! PUMP STATISTICS                                                             !(5.0.012 - LR)
!----------------                                                             !(5.0.012 - LR)
type TPumpStats
   real(kind=dpo) :: utilized                                                      !(5.0.012 - LR)
   real(kind=dpo) :: minFlow                                                       !(5.0.022 - LR)
   real(kind=dpo) :: avgFlow                                                       !(5.0.012 - LR)
   real(kind=dpo) :: maxFlow                                                       !(5.0.012 - LR)
   real(kind=dpo) :: volume                                                        !(5.0.012 - LR)
   real(kind=dpo) :: energy                                                        !(5.0.012 - LR)
   real(kind=dpo) :: offCurveLow                                                   !(5.0.022 - LR)
   real(kind=dpo) :: offCurveHigh                                                  !(5.0.022 - LR)
   integer(kind=K4) :: startUps                                                      !(5.0.022 - LR)
   integer(kind=K4) :: totalPeriods                                                  !(5.0.012 - LR)
end type TPumpStats


!----------------
! LINK STATISTICS
!----------------
type TLinkStats
   real(kind=dpo) :: maxFlow
   real(kind=dpo) :: maxFlowDate
   real(kind=dpo) :: maxVeloc
   real(kind=dpo) :: maxVelocDate
   real(kind=dpo) :: maxDepth
   real(kind=dpo) :: avgFlowChange
   real(kind=dpo) :: avgFroude
   real(kind=dpo) :: timeSurcharged
   real(kind=dpo) :: timeFullUpstream                                             !(5.0.012 - LR)
   real(kind=dpo) :: timeFullDnstream                                             !(5.0.012 - LR)
   real(kind=dpo) :: timeFullFlow                                                 !(5.0.012 - LR)
   real(kind=dpo) :: timeCapacityLimited                                          !(5.0.012 - LR)
   real(kind=dpo), dimension(MAX_FLOW_CLASSES) :: timeInFlowClass
   real(kind=dpo) :: timeCourantCritical
   integer(kind=K8) :: flowTurns                                                    !(5.0.010 - LR)
   integer(kind=K4) :: flowTurnSign                                                 !(5.0.010 - LR)
end type TLinkStats


!-------------------------
! MAXIMUM VALUE STATISTICS
!-------------------------
type TMaxStats
   integer(kind=K4) :: objType         ! either NODE or LINK
   integer(kind=K4) :: index           ! node or link index
   real(kind=dpo) :: value           ! value of node or link statistic
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
