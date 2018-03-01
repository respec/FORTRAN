module text
!-----------------------------------------------------------------------------
!   text.h
!
!   Project: EPA SWMM5
!   Version: 5.0
!   Date:    6/19/07   (Build 5.0.010)
!            7/16/07   (Build 5.0.011)
!            2/4/08    (Build 5.0.012)
!            3/11/08   (Build 5.0.013)
!            1/21/09   (Build 5.0.014)
!            4/10/09   (Build 5.0.015)
!            6/22/09   (Build 5.0.016)
!            10/7/09   (Build 5.0.017)
!            11/18/09  (Build 5.0.018)
!            07/30/10  (Build 5.0.019)
!            08/23/10  (Build 5.0.020)
!            09/27/10  (Build 5.0.021)
!            04/20/11  (Build 5.0.022)
!   Author:  L. Rossman
!
!   Text strings
!-----------------------------------------------------------------------------

character*(*), parameter ::  FMT01 = 'NL Correct syntax is:NL swmm5 <input file> <report file> <output file>NL'
character*(*), parameter ::  FMT02 = 'NL... EPA-SWMM 5.0 (Build 5.0.022)NL' !(5.0.022 - LR)

character*(*), parameter ::  FMT03 = ' There are errors.NL'
character*(*), parameter ::  FMT04 = ' There are warnings.NL'
character*(*), parameter ::  FMT05 = 'NL     '
character*(*), parameter ::  FMT06 = 'NL o  Retrieving project data'
character*(*), parameter ::  FMT07 = 'NL o  Writing output report'
character*(*), parameter ::  FMT08 = 'NL  EPA STORM WATER MANAGEMENT MODEL - VERSION 5.0 (Build 5.0.022)'    !(5.0.022 - LR)
character*(*), parameter ::  FMT09 = 'NL  --------------------------------------------------------------'
character*(*), parameter ::  FMT10 = 'NL     '
character*(*), parameter ::  FMT11 = 'NL    Cannot use duplicate file names.'
character*(*), parameter ::  FMT12 = 'NL    Cannot open input file '
character*(*), parameter ::  FMT13 = 'NL    Cannot open report file '
character*(*), parameter ::  FMT14 = 'NL    Cannot open output file '
character*(*), parameter ::  FMT15 = 'NL    Cannot open temporary output file'
character*(*), parameter ::  FMT16 = 'NL  ERROR %d detected. Execution halted.'
character*(*), parameter ::  FMT17 = 'at line %d of input file:'
character*(*), parameter ::  FMT18 = 'at line %d of %s] section:'
character*(*), parameter ::  FMT19 = 'NL  Maximum error count exceeded.'
character*(*), parameter ::  FMT20 = 'NLNL  Analysis begun on:  %s'
character*(*), parameter ::  FMT20a = '  Analysis ended on:  %s'    !(5.0.011 - LR)
character*(*), parameter ::  FMT21 = '  Total elapsed time: '

! Warning messages   !(5.0.015 - LR)
character*(*), parameter :: WARN01 = 'WARNING 01: wet weather time step reduced to recording interval for Rain Gage' 
character*(*), parameter :: WARN02 = 'WARNING 02: maximum depth increased for Node'
character*(*), parameter :: WARN03 = 'WARNING 03: negative offset ignored for Link'
character*(*), parameter :: WARN04 = 'WARNING 04: minimum elevation drop used for Conduit'
character*(*), parameter :: WARN05 = 'WARNING 05: minimum slope used for Conduit'
character*(*), parameter :: WARN06 = 'WARNING 06: dry weather time step increased to the wet weather time step'
character*(*), parameter :: WARN07 = 'WARNING 07: routing time step reduced to the wet weather time step'
character*(*), parameter :: WARN08 = 'WARNING 08: elevation drop exceeds length for Conduit'         !(5.0.018 - LR)
character*(*), parameter :: WARN09 = 'WARNING 09: time series interval greater than recording interval for Rain Gage' !(5.0.018 - LR)

! Analysis Option Keywords
character*(*), parameter :: w_FLOW_UNITS        = 'FLOW_UNITS         '
character*(*), parameter :: w_INFIL_MODEL       = 'INFILTRATION       '
character*(*), parameter :: w_ROUTE_MODEL       = 'FLOW_ROUTING       '
character*(*), parameter :: w_START_DATE        = 'START_DATE         '
character*(*), parameter :: w_START_TIME        = 'START_TIME         '
character*(*), parameter :: w_END_DATE          = 'END_DATE           '
character*(*), parameter :: w_END_TIME          = 'END_TIME           '
character*(*), parameter :: w_REPORT_START_DATE = 'REPORT_START_DATE  '
character*(*), parameter :: w_REPORT_START_TIME = 'REPORT_START_TIME  '
character*(*), parameter :: w_SWEEP_START       = 'SWEEP_START        '
character*(*), parameter :: w_SWEEP_END         = 'SWEEP_END          '
character*(*), parameter :: w_START_DRY_DAYS    = 'DRY_DAYS           '
character*(*), parameter :: w_WET_STEP          = 'WET_STEP           '
character*(*), parameter :: w_DRY_STEP          = 'DRY_STEP           '
character*(*), parameter :: w_ROUTE_STEP        = 'ROUTING_STEP       '
character*(*), parameter :: w_REPORT_STEP       = 'REPORT_STEP        '
character*(*), parameter :: w_ALLOW_PONDING     = 'ALLOW_PONDING      '
character*(*), parameter :: w_INERT_DAMPING     = 'INERTIAL_DAMPING   '
character*(*), parameter :: w_SLOPE_WEIGHTING   = 'SLOPE_WEIGHTING    '
character*(*), parameter :: w_VARIABLE_STEP     = 'VARIABLE_STEP      '
character*(*), parameter :: w_NORMAL_FLOW_LTD   = 'NORMAL_FLOW_LIMITED'
character*(*), parameter :: w_LENGTHENING_STEP  = 'LENGTHENING_STEP   '
character*(*), parameter :: w_MIN_SURFAREA      = 'MIN_SURFAREA       '
character*(*), parameter :: w_COMPATIBILITY     = 'COMPATIBILITY      '
character*(*), parameter :: w_SKIP_STEADY_STATE = 'SKIP_STEADY_STATE  '
character*(*), parameter :: w_TEMPDIR           = 'TEMPDIR            '
character*(*), parameter :: w_IGNORE_RAINFALL   = 'IGNORE_RAINFALL    '
character*(*), parameter :: w_FORCE_MAIN_EQN    = 'FORCE_MAIN_EQUATION'
character*(*), parameter :: w_LINK_OFFSETS      = 'LINK_OFFSETS       ' !(5.0.012 - LR)
character*(*), parameter :: w_MIN_SLOPE         = 'MIN_SLOPE          ' !(5.0.014 - LR)
character*(*), parameter :: w_IGNORE_SNOWMELT   = 'IGNORE_SNOWMELT    ' !(5.0.014 - LR)
character*(*), parameter :: w_IGNORE_GWATER     = 'IGNORE_GROUNDWATER ' !(5.0.014 - LR)
character*(*), parameter :: w_IGNORE_ROUTING    = 'IGNORE_ROUTING     ' !(5.0.014 - LR)
character*(*), parameter :: w_IGNORE_QUALITY    = 'IGNORE_QUALITY     ' !(5.0.014 - LR)

! Flow Units
character*(*), parameter :: w_CFS               = 'CFS                '
character*(*), parameter :: w_GPM               = 'GPM                '
character*(*), parameter :: w_MGD               = 'MGD                '
character*(*), parameter :: w_CMS               = 'CMS                '
character*(*), parameter :: w_LPS               = 'LPS                '
character*(*), parameter :: w_MLD               = 'MLD                '

! Flow Routing Methods
character*(*), parameter :: w_NF                = 'NF                 '
character*(*), parameter :: w_KW                = 'KW                 '
character*(*), parameter :: w_EKW               = 'EKW                '
character*(*), parameter :: w_DW                = 'DW                 '

character*(*), parameter :: w_STEADY            = 'STEADY             '
character*(*), parameter :: w_KINWAVE           = 'KINWAVE            '
character*(*), parameter :: w_XKINWAVE          = 'XKINWAVE           '
character*(*), parameter :: w_DYNWAVE           = 'DYNWAVE            '

! Infiltration Methods
character*(*), parameter :: w_HORTON            = 'HORTON             '
character*(*), parameter :: w_GREEN_AMPT        = 'GREEN_AMPT         '
character*(*), parameter :: w_CURVE_NUMEBR      = 'CURVE_NUMBER       '

! Normal Flow Criteria                                                        !(5.0.010 - LR)
character*(*), parameter :: w_SLOPE             = 'SLOPE              '                                           !(5.0.010 - LR)
character*(*), parameter :: w_FROUDE            = 'FROUDE             '                                          !(5.0.010 - LR)
character*(*), parameter :: w_BOTH              = 'BOTH               '                                            !(5.0.010 - LR)

! Snowmelt Data Keywords
character*(*), parameter :: w_WINDSPEED         = 'WINDSPEED          '
character*(*), parameter :: w_SNOWMELT          = 'SNOWMELT           '
character*(*), parameter :: w_ADC               = 'ADC                '
character*(*), parameter :: w_PLOWABLE          = 'PLOWABLE           '

! Evaporation Data Options
character*(*), parameter :: w_CONSTANT          = 'CONSTANT           '
character*(*), parameter :: w_TIMESERIES        = 'TIMESERIES         '
character*(*), parameter :: w_TEMPERATURE       = 'TEMPERATURE        '                                     !(5.0.016 - LR)
character*(*), parameter :: w_FILE              = 'FILE               '
character*(*), parameter :: w_RECOVERY          = 'RECOVERY           '                                        !(5.0.014 - LR)
character*(*), parameter :: w_DRYONLY           = 'DRY_ONLY           '                                        !(5.0.019 - LR)

! DWF Time Pattern Types
character*(*), parameter :: w_MONTHLY           = 'MONTHLY            '
character*(*), parameter :: w_DAILY             = 'DAILY              '
character*(*), parameter :: w_HOURLY            = 'HOURLY             '
character*(*), parameter :: w_WEEKEND           = 'WEEKEND            '

! Rainfall Record Types
character*(*), parameter :: w_INTENSITY         = 'INTENSITY          '
character*(*), parameter :: w_VOLUME            = 'VOLUME             '
character*(*), parameter :: w_CUMULATIVE        = 'CUMULATIVE         '

! Unit Hydrograph Types                                                       !(5.0.015 - LR)
character*(*), parameter :: w_SHORT             = 'SHORT              '                                           !(5.0.015 - LR)
character*(*), parameter :: w_MEDIUM            = 'MEDIUM             '                                          !(5.0.015 - LR)
character*(*), parameter :: w_LONG              = 'LONG               '                                            !(5.0.015 - LR)

! Internal Runoff Routing Options
character*(*), parameter :: w_OUTLET            = 'OUTLET             '
character*(*), parameter :: w_IMPERV            = 'IMPERV             '
character*(*), parameter :: w_PERV              = 'PERV               '

! Outfall Node Types
character*(*), parameter :: w_FREE              = 'FREE               '
character*(*), parameter :: w_FIXED             = 'FIXED              '
!character*(*), parameter :: w_TIDAL             = 'TIDAL              '
character*(*), parameter :: w_CRITICAL          = 'CRITICAL           '
character*(*), parameter :: w_NORMAL            = 'NORMAL             '

! Flow Divider Node Types
character*(*), parameter :: w_FUNCTIONAL        = 'FUNCTIONAL         '
character*(*), parameter :: w_TABULAR           = 'TABULAR            '
character*(*), parameter :: w_CUTOFF            = 'CUTOFF             '
character*(*), parameter :: w_OVERFLOW          = 'OVERFLOW           '

! Pump Curve Types
character*(*), parameter :: w_TYPE1             = 'TYPE1              '
character*(*), parameter :: w_TYPE2             = 'TYPE2              '
character*(*), parameter :: w_TYPE3             = 'TYPE3              '
character*(*), parameter :: w_TYPE4             = 'TYPE4              '
character*(*), parameter :: w_IDEAL             = 'IDEAL              '                                           !(5.0.010 - LR)

! Pump Curve Variables
!character*(*), parameter :: w_VOLUME            = 'VOLUME             '
character*(*), parameter :: w_DEPTH             = 'DEPTH              '
character*(*), parameter :: w_HEAD              = 'HEAD               '

! Orifice Types
character*(*), parameter :: w_SIDE              = 'SIDE               '
character*(*), parameter :: w_BOTTOM            = 'BOTTOM             '

! Weir Types
character*(*), parameter :: w_TRANSVERSE        = 'TRANSVERSE         '
character*(*), parameter :: w_SIDEFLOW          = 'SIDEFLOW           '
character*(*), parameter :: w_VNOTCH            = 'V-NOTCH            '

! Conduit Cross-Section Shapes
character*(*), parameter :: w_DUMMY             = 'DUMMY              '
character*(*), parameter :: w_CIRCULAR          = 'CIRCULAR           '
character*(*), parameter :: w_FILLED_CIRCULAR   = 'FILLED_CIRCULAR    '
character*(*), parameter :: w_RECT_CLOSED       = 'RECT_CLOSED        '
character*(*), parameter :: w_RECT_OPEN         = 'RECT_OPEN          '
character*(*), parameter :: w_TRAPEZOIDAL       = 'TRAPEZOIDAL        '
character*(*), parameter :: w_TRIANGULAR        = 'TRIANGULAR         '
character*(*), parameter :: w_PARABOLIC         = 'PARABOLIC          '
character*(*), parameter :: w_POWERFUNC         = 'POWER              '
character*(*), parameter :: w_RECT_TRIANG       = 'RECT_TRIANGULAR    '
character*(*), parameter :: w_RECT_ROUND        = 'RECT_ROUND         '
character*(*), parameter :: w_MOD_BASKET        = 'MODBASKETHANDLE    '
character*(*), parameter :: w_HORIZELLIPSE      = 'HORIZ_ELLIPSE      '
character*(*), parameter :: w_VERTELLIPSE       = 'VERT_ELLIPSE       '
character*(*), parameter :: w_ARCH              = 'ARCH               '
character*(*), parameter :: w_EGGSHAPED         = 'EGG                '
character*(*), parameter :: w_HORSESHOE         = 'HORSESHOE          '
character*(*), parameter :: w_GOTHIC            = 'GOTHIC             '
character*(*), parameter :: w_CATENARY          = 'CATENARY           '
character*(*), parameter :: w_SEMIELLIPTICAL    = 'SEMIELLIPTICAL     '
character*(*), parameter :: w_BASKETHANDLE      = 'BASKETHANDLE       '
character*(*), parameter :: w_SEMICIRCULAR      = 'SEMICIRCULAR       '
character*(*), parameter :: w_IRREGULAR         = 'IRREGULAR          '
character*(*), parameter :: w_CUSTOM            = 'CUSTOM             '                                          !(5.0.010 - LR)
character*(*), parameter :: w_FORCE_MAIN        = 'FORCE_MAIN         '                                      !(5.0.010 - LR)
character*(*), parameter :: w_H_W               = 'H-W                '                                             !(5.0.010 - LR)
character*(*), parameter :: w_D_W               = 'D-W                '                                             !(5.0.010 - LR)

! Link Offset Options                                                         !(5.0.012 - LR)
character*(*), parameter :: w_ELEVATION         = 'ELEVATION          '                                       !(5.0.012 - LR)
         ! w_DEPTH defined previously.                                        !(5.0.012 - LR)

! Transect Data Input Codes
character*(*), parameter :: w_NC                = 'NC                 '
character*(*), parameter :: w_X1                = 'X1                 '
character*(*), parameter :: w_GR                = 'GR                 '

! Rain Volume Units
character*(*), parameter :: w_INCHES            = 'IN                 '
character*(*), parameter :: w_MMETER            = 'MM                 '

! Flow Volume Units                                                           !(5.0.012 - LR)
character*(*), parameter :: w_MGAL              = '10^6 gal           '                                        !(5.0.014 - LR)
character*(*), parameter :: w_MLTRS             = '10^6 ltr           '                                        !(5.0.014 - LR)

! Ponded Depth Units                                                          !(5.0.019 - LR)
character*(*), parameter :: w_PONDED_FEET       = 'Feet               '                                            !(5.0.019 - LR)
character*(*), parameter :: w_PONDED_METERS     = 'Meters             '                                          !(5.0.019 - LR)

! Concentration Units
character*(*), parameter :: w_MGperL            = 'MG/L               '
character*(*), parameter :: w_UGperL            = 'UG/L               '
character*(*), parameter :: w_COUNTperL         = '#/L                '

! Mass Units
character*(*), parameter :: w_MG                = 'MG                 '
character*(*), parameter :: w_UG                = 'UG                 '
character*(*), parameter :: w_COUNT             = '#                  '

! Load Units
character*(*), parameter :: w_LBS               = 'lbs                '
character*(*), parameter :: w_KG                = 'kg                 '
character*(*), parameter :: w_LOGN              = 'LogN               '

! Pollutant Buildup Functions
character*(*), parameter :: w_POW               = 'POW                '
character*(*), parameter :: w_EXP               = 'EXP                '
character*(*), parameter :: w_SAT               = 'SAT                '
character*(*), parameter :: w_EXT               = 'EXT                '       !(5.0.019 - LR)

! Normalizing Variables for Pollutant Buildup
character*(*), parameter ::   w_PER_AREA        = 'AREA               '
character*(*), parameter ::   w_PER_CURB        = 'CURB               '

! Pollutant Washoff Functions
! (EXP function defined above)
character*(*), parameter ::   w_RC              = 'RC                 '
character*(*), parameter ::   w_EMC             = 'EMC                '

! Treatment Keywords
character*(*), parameter ::   w_REMOVAL         = 'REMOVAL            '
character*(*), parameter ::   w_RATE            = 'RATE               '
character*(*), parameter ::   w_HRT             = 'HRT                '
character*(*), parameter ::   w_DT              = 'DT                 '
character*(*), parameter ::   w_AREA            = 'AREA               '

! Curve Types
!define  w_STORAGE (defined below)
character*(*), parameter ::   w_DIVERSION       = 'DIVERSION          '
character*(*), parameter ::   w_TIDAL           = 'TIDAL              '
character*(*), parameter ::   w_RATING          = 'RATING             '
character*(*), parameter ::   w_SHAPE           = 'SHAPE              '                                           !(5.0.010 - LR)
character*(*), parameter ::   w_PUMP1           = 'PUMP1              '
character*(*), parameter ::   w_PUMP2           = 'PUMP2              '
character*(*), parameter ::   w_PUMP3           = 'PUMP3              '
character*(*), parameter ::   w_PUMP4           = 'PUMP4              '

! Reporting Options
character*(*), parameter ::   w_INPUT           = 'INPUT              '
character*(*), parameter ::   w_CONTINUITY      = 'CONTINUITY         '
character*(*), parameter ::   w_FLOWSTATS       = 'FLOWSTATS          '
character*(*), parameter ::   w_CONTROLS        = 'CONTROL            '
character*(*), parameter ::   w_NODESTATS       = 'NODESTATS          '

! Interface File Types
character*(*), parameter ::   w_RAINFALL        = 'RAINFALL           '
character*(*), parameter ::   w_RUNOFF          = 'RUNOFF             '
character*(*), parameter ::   w_HOTSTART        = 'HOTSTART           '
character*(*), parameter ::   w_RDII            = 'RDII               '
character*(*), parameter ::   w_ROUTING         = 'ROUTING            '
character*(*), parameter ::   w_INFLOWS         = 'INFLOWS            '
character*(*), parameter ::   w_OUTFLOWS        = 'OUTFLOWS           '

! Miscellaneous Keywords
character*(*), parameter ::   w_OFF             = 'OFF                '
character*(*), parameter ::   w_ON              = 'ON                 '
character*(*), parameter ::   w_NO              = 'NO                 '
character*(*), parameter ::   w_YES             = 'YES                '
character*(*), parameter ::   w_NONE            = 'NONE               '
character*(*), parameter ::   w_ALL             = 'ALL                '
character*(*), parameter ::   w_SCRATCH         = 'SCRATCH            '
character*(*), parameter ::   w_USE             = 'USE                '
character*(*), parameter ::   w_SAVE            = 'SAVE               '
character*(*), parameter ::   w_FULL            = 'FULL               '
character*(*), parameter ::   w_PARTIAL         = 'PARTIAL            '

! Major Object Types
character*(*), parameter ::   w_GAGE            = 'RAINGAGE           '
character*(*), parameter ::   w_SUBCATCH        = 'SUBCATCH           '
character*(*), parameter ::   w_NODE            = 'NODE               '
character*(*), parameter ::   w_LINK            = 'LINK               '
character*(*), parameter ::   w_POLLUT          = 'POLLUTANT          '
character*(*), parameter ::   w_LANDUSE         = 'LANDUSE            '
character*(*), parameter ::   w_TSERIES         = 'TIME SERIES        '
character*(*), parameter ::   w_TABLE           = 'TABLE              '
character*(*), parameter ::   w_UNITHYD         = 'HYDROGRAPH         '

! Node Sub-Types
character*(*), parameter ::   w_JUNCTION        = 'JUNCTION           '
character*(*), parameter ::   w_OUTFALL         = 'OUTFALL            '
character*(*), parameter ::   w_STORAGE         = 'STORAGE            '
character*(*), parameter ::   w_DIVIDER         = 'DIVIDER            '

! Link Sub-Types
character*(*), parameter ::   w_CONDUIT         = 'CONDUIT            '
character*(*), parameter ::   w_PUMP            = 'PUMP               '
character*(*), parameter ::   w_ORIFICE         = 'ORIFICE            '
character*(*), parameter ::   w_WEIR            = 'WEIR               '

! Control Rule Keywords
character*(*), parameter ::   w_RULE            = 'RULE               '
character*(*), parameter ::   w_IF              = 'IF                 '
character*(*), parameter ::   w_AND             = 'AND                '
character*(*), parameter ::   w_OR              = 'OR                 '
character*(*), parameter ::   w_THEN            = 'THEN               '
character*(*), parameter ::   w_ELSE            = 'ELSE               '
character*(*), parameter ::   w_PRIORITY        = 'PRIORITY           '

! External Inflow Types
character*(*), parameter ::   w_FLOW            = 'FLOW               '
character*(*), parameter ::   w_CONCEN          = 'CONCEN             '
character*(*), parameter ::   w_MASS            = 'MASS               '

! Variable Units
character*(*), parameter ::   w_FEET            = 'FEET               '
character*(*), parameter ::   w_METERS          = 'METERS             '
character*(*), parameter ::   w_FPS             = 'FT/SEC             '
character*(*), parameter ::   w_MPS             = 'M/SEC              '
character*(*), parameter ::   w_PCNT            = 'PERCENT            '
character*(*), parameter ::   w_ACRE            = 'acre               '
character*(*), parameter ::   w_HECTARE         = 'hectare            '

! Input File Sections
character*(*), parameter ::   ws_TITLE          = '[TITLE             '
character*(*), parameter ::   ws_OPTION         = '[OPTION            '
character*(*), parameter ::   ws_FILE           = '[FILE              '
character*(*), parameter ::   ws_RAINGAGE       = '[RAINGAGE          '
character*(*), parameter ::   ws_TEMP           = '[TEMPERATURE       '
character*(*), parameter ::   ws_EVAP           = '[EVAP              '
character*(*), parameter ::   ws_SUBCATCH       = '[SUBCATCHMENT      '
character*(*), parameter ::   ws_SUBAREA        = '[SUBAREA           '
character*(*), parameter ::   ws_INFIL          = '[INFIL             '
character*(*), parameter ::   ws_AQUIFER        = '[AQUIFER           '
character*(*), parameter ::   ws_GROUNDWATER    = '[GROUNDWATER       '
character*(*), parameter ::   ws_SNOWMELT       = '[SNOWPACK          '
character*(*), parameter ::   ws_JUNCTION       = '[JUNC              '
character*(*), parameter ::   ws_OUTFALL        = '[OUTFALL           '
character*(*), parameter ::   ws_STORAGE        = '[STORAGE           '
character*(*), parameter ::   ws_DIVIDER        = '[DIVIDER           '
character*(*), parameter ::   ws_CONDUIT        = '[CONDUIT           '
character*(*), parameter ::   ws_PUMP           = '[PUMP              '
character*(*), parameter ::   ws_ORIFICE        = '[ORIFICE           '
character*(*), parameter ::   ws_WEIR           = '[WEIR              '
character*(*), parameter ::   ws_OUTLET         = '[OUTLET            '
character*(*), parameter ::   ws_XSECTION       = '[XSECT             '
character*(*), parameter ::   ws_TRANSECT       = '[TRANSECT          '
character*(*), parameter ::   ws_LOSS           = '[LOSS              '
character*(*), parameter ::   ws_CONTROL        = '[CONTROL           '
character*(*), parameter ::   ws_POLLUTANT      = '[POLLUT            '
character*(*), parameter ::   ws_LANDUSE        = '[LANDUSE           '
character*(*), parameter ::   ws_BUILDUP        = '[BUILDUP           '
character*(*), parameter ::   ws_WASHOFF        = '[WASHOFF           '
character*(*), parameter ::   ws_COVERAGE       = '[COVERAGE          '
character*(*), parameter ::   ws_INFLOW         = '[INFLOW            '
character*(*), parameter ::   ws_DWF            = '[DWF               '
character*(*), parameter ::   ws_PATTERN        = '[PATTERN           '
character*(*), parameter ::   ws_RDII           = '[RDII              '
character*(*), parameter ::   ws_UNITHYD        = '[HYDROGRAPH        '
character*(*), parameter ::   ws_LOADING        = '[LOADING           '
character*(*), parameter ::   ws_TREATMENT      = '[TREATMENT         '
character*(*), parameter ::   ws_CURVE          = '[CURVE             '
character*(*), parameter ::   ws_TIMESERIES     = '[TIMESERIES        '
character*(*), parameter ::   ws_REPORT         = '[REPORT            '
character*(*), parameter ::   ws_MAP            = '[MAP               '
character*(*), parameter ::   ws_COORDINATE     = '[COORDINATE        '
character*(*), parameter ::   ws_VERTICES       = '[VERTICES          '
character*(*), parameter ::   ws_POLYGON        = '[POLYGON           '
character*(*), parameter ::   ws_SYMBOL         = '[SYMBOL            '
character*(*), parameter ::   ws_LABEL          = '[LABEL             '
character*(*), parameter ::   ws_BACKDROP       = '[BACKDROP          '
character*(*), parameter ::   ws_TAG            = '[TAG               '
character*(*), parameter ::   ws_PROFILE        = '[PROFILE           '
character*(*), parameter ::   ws_LID_CONTROL    = '[LID_CONTROL       '                                    !(5.0.019 - LR)
character*(*), parameter ::   ws_LID_USAGE      = '[LID_USAGE         '                                      !(5.0.019 - LR)

end module text
