module error
!-----------------------------------------------------------------------------
!   error.h
!
!   Project: EPA SWMM5
!   Version: 5.0
!   Date:    04/20/11  (Build 5.0.022)
!   Author:  L. Rossman
!
!   Error codes
!
!   NOTE: The error code enumeration was re-ordered sequentially for
!         release 5.0.022 and all previous update comments were removed.
!-----------------------------------------------------------------------------

!enum  ErrorType {

  !... Runtime Errors
integer, parameter :: ERR_NONE = 0                 !     0
integer, parameter :: ERR_MEMORY = 1               !101  1
integer, parameter :: ERR_KINWAVE = 2              !103  2
integer, parameter :: ERR_ODE_SOLVER = 3           !105  3
integer, parameter :: ERR_TIMESTEP = 4             !107  4

  !... Subcatchment/Aquifer Errors
integer, parameter :: ERR_SUBCATCH_OUTLET = 5      !108  5
integer, parameter :: ERR_AQUIFER_PARAMS = 6       !109  6
integer, parameter :: ERR_GROUND_ELEV = 7          !110  7

  !... Conduit/Pump Errors
integer, parameter :: ERR_LENGTH = 8               !111  8
integer, parameter :: ERR_ELEV_DROP = 9            !112  9
integer, parameter :: ERR_ROUGHNESS = 10            !113  10
integer, parameter :: ERR_BARRELS = 11              !114  11
integer, parameter :: ERR_SLOPE = 12                !115  12
integer, parameter :: ERR_NO_XSECT = 13             !117  13
integer, parameter :: ERR_XSECT = 14                !119  14
integer, parameter :: ERR_NO_CURVE = 15             !121  15

  !... Topology Errors
integer, parameter :: ERR_LOOP = 16                 !131  16
integer, parameter :: ERR_MULTI_OUTLET = 17         !133  17
integer, parameter :: ERR_MULTI_DUMMY_LINK = 18     !134  18

  !... Node Errors
integer, parameter :: ERR_DIVIDER = 19              !135  19
integer, parameter :: ERR_DIVIDER_LINK = 20         !136  20
integer, parameter :: ERR_WEIR_DIVIDER = 21         !137  21
integer, parameter :: ERR_NODE_DEPTH = 22           !138  22
integer, parameter :: ERR_REGULATOR = 23            !139  23
integer, parameter :: ERR_OUTFALL = 24              !141  24
integer, parameter :: ERR_REGULATOR_SHAPE = 25      !143  25
integer, parameter :: ERR_NO_OUTLETS = 26           !145  26

  !... RDII Errors
integer, parameter :: ERR_UNITHYD_TIMES = 27        !151  27
integer, parameter :: ERR_UNITHYD_RATIOS = 28       !153  28
integer, parameter :: ERR_RDII_AREA = 29            !155  29

  !... Rain Gage Errors
integer, parameter :: ERR_RAIN_FILE_CONFLICT = 30   !156  30
integer, parameter :: ERR_RAIN_GAGE_FORMAT = 31     !157  31
integer, parameter :: ERR_RAIN_GAGE_TSERIES = 32    !158  32
integer, parameter :: ERR_RAIN_GAGE_INTERVAL = 33   !159  33

  !... Treatment Function Error
integer, parameter :: ERR_CYCLIC_TREATMENT = 34     !161  34

  !... Curve/Time Series Errors
integer, parameter :: ERR_CURVE_SEQUENCE = 35       !171  35
integer, parameter :: ERR_TIMESERIES_SEQUENCE = 36  !173  36

  !... Snowmelt Errors
integer, parameter :: ERR_SNOWMELT_PARAMS = 37      !181  37
integer, parameter :: ERR_SNOWPACK_PARAMS = 38      !182  38

  !... LID Errors
integer, parameter :: ERR_LID_TYPE = 39             !183  39
integer, parameter :: ERR_LID_LAYER = 40            !184  40
integer, parameter :: ERR_LID_PARAMS = 41           !185  41
integer, parameter :: ERR_SUBCATCH_LID = 42         !186  42
integer, parameter :: ERR_LID_AREAS = 43            !187  43
integer, parameter :: ERR_LID_CAPTURE_AREA = 44     !188  44

  !... Simulation Date/Time Errors
integer, parameter :: ERR_START_DATE = 45           !191  45
integer, parameter :: ERR_REPORT_DATE = 46          !193  46
integer, parameter :: ERR_REPORT_STEP = 47          !195  47

  !... Input Parser Errors
integer, parameter :: ERR_INPUT = 48                !200  48
integer, parameter :: ERR_LINE_LENGTH = 49          !201  49
integer, parameter :: ERR_ITEMS = 50                !203  50
integer, parameter :: ERR_KEYWORD = 51              !205  51
integer, parameter :: ERR_DUP_NAME = 52             !207  52
integer, parameter :: ERR_NAME = 53                 !209  53
integer, parameter :: ERR_NUMBER = 54               !211  54
integer, parameter :: ERR_DATETIME = 55             !213  55
integer, parameter :: ERR_RULE = 56                 !217  56
integer, parameter :: ERR_TRANSECT_UNKNOWN = 57     !219  57
integer, parameter :: ERR_TRANSECT_SEQUENCE = 58    !221  58
integer, parameter :: ERR_TRANSECT_TOO_FEW = 59     !223  59
integer, parameter :: ERR_TRANSECT_TOO_MANY = 60    !225  60
integer, parameter :: ERR_TRANSECT_MANNING = 61     !227  61
integer, parameter :: ERR_TRANSECT_OVERBANK = 62    !229  62
integer, parameter :: ERR_TRANSECT_NO_DEPTH = 63    !231  63
integer, parameter :: ERR_TREATMENT_EXPR = 64       !233  64

  !... File Name/Opening Errors
integer, parameter :: ERR_FILE_NAME = 65            !301  65
integer, parameter :: ERR_INP_FILE = 66             !303  66
integer, parameter :: ERR_RPT_FILE = 67             !305  67
integer, parameter :: ERR_OUT_FILE = 68             !307  68
integer, parameter :: ERR_OUT_WRITE = 69            !309  69
integer, parameter :: ERR_OUT_READ = 70             !311  70

  !... Rain File Errors
integer, parameter :: ERR_RAIN_FILE_SCRATCH = 71    !313  71
integer, parameter :: ERR_RAIN_FILE_OPEN = 72       !315  72
integer, parameter :: ERR_RAIN_FILE_DATA = 73       !317  73
integer, parameter :: ERR_RAIN_FILE_SEQUENCE = 74   !318  74 
integer, parameter :: ERR_RAIN_FILE_FORMAT = 75     !319  75
integer, parameter :: ERR_RAIN_FILE_GAGE = 76       !321  76

  !... Runoff File Errors
integer, parameter :: ERR_RUNOFF_FILE_OPEN = 77    !323  77
integer, parameter :: ERR_RUNOFF_FILE_FORMAT = 78    !325  78
integer, parameter :: ERR_RUNOFF_FILE_END = 79       !327  79
integer, parameter :: ERR_RUNOFF_FILE_READ = 80      !329  80

  !... Hotstart File Errors
integer, parameter :: ERR_HOTSTART_FILE_NAMES = 81  !330  81
integer, parameter :: ERR_HOTSTART_FILE_OPEN = 82   !331  82
integer, parameter :: ERR_HOTSTART_FILE_FORMAT = 83 !333  83
integer, parameter :: ERR_HOTSTART_FILE_READ = 84   !335  84

  !... Climate File Errors
integer, parameter :: ERR_NO_CLIMATE_FILE = 85      !336  85
integer, parameter :: ERR_CLIMATE_FILE_OPEN = 86    !337  86
integer, parameter :: ERR_CLIMATE_FILE_READ = 87    !338  87
integer, parameter :: ERR_CLIMATE_END_OF_FILE = 88  !339  88

  !... RDII File Errors
integer, parameter :: ERR_RDII_FILE_SCRATCH = 89    !341  89
integer, parameter :: ERR_RDII_FILE_OPEN = 90       !343  90
integer, parameter :: ERR_RDII_FILE_FORMAT = 91     !345  91
      
  !... Routing File Errors
integer, parameter :: ERR_ROUTING_FILE_OPEN = 92    !351  92
integer, parameter :: ERR_ROUTING_FILE_FORMAT = 93  !353  93
integer, parameter :: ERR_ROUTING_FILE_NOMATCH = 94 !355  94
integer, parameter :: ERR_ROUTING_FILE_NAMES = 95   !357  95

  !... Time Series File Errors
integer, parameter :: ERR_TABLE_FILE_OPEN = 96      !361  96
integer, parameter :: ERR_TABLE_FILE_READ = 97      !363  97

  !... Runtime Errors
integer, parameter :: ERR_SYSTEM = 98               !401  98
integer, parameter :: ERR_NOT_CLOSED = 99           !402  99
integer, parameter :: ERR_NOT_OPEN = 100             !403  100
integer, parameter :: ERR_FILE_SIZE = 101            !405  101

integer, parameter :: MAXERRMSG = 102
!     };

!below is content from error.c

character(len=*) :: ERR101 
character(len=*) :: ERR103 
character(len=*) :: ERR105 
character(len=*) :: ERR107 
character(len=*) :: ERR108 
character(len=*) :: ERR109 
character(len=*) :: ERR110 
character(len=*) :: ERR111 
character(len=*) :: ERR112 
character(len=*) :: ERR113 
character(len=*) :: ERR114 
character(len=*) :: ERR115 
character(len=*) :: ERR117 
character(len=*) :: ERR119 
character(len=*) :: ERR121 
character(len=*) :: ERR131 
character(len=*) :: ERR133 
character(len=*) :: ERR134 
character(len=*) :: ERR135 
character(len=*) :: ERR136 
character(len=*) :: ERR137 
character(len=*) :: ERR138 
character(len=*) :: ERR139 
character(len=*) :: ERR141 
character(len=*) :: ERR143 
character(len=*) :: ERR145 
character(len=*) :: ERR151 
character(len=*) :: ERR153 
character(len=*) :: ERR155 
character(len=*) :: ERR156 
character(len=*) :: ERR157 
character(len=*) :: ERR158 
character(len=*) :: ERR159 
character(len=*) :: ERR161 
character(len=*) :: ERR171 
character(len=*) :: ERR173 
character(len=*) :: ERR181 
character(len=*) :: ERR182 
character(len=*) :: ERR183 
character(len=*) :: ERR184 
character(len=*) :: ERR185 
character(len=*) :: ERR186 
character(len=*) :: ERR187 
character(len=*) :: ERR188 
character(len=*) :: ERR191 
character(len=*) :: ERR193 
character(len=*) :: ERR195 
character(len=*) :: ERR200 
character(len=*) :: ERR201 
character(len=*) :: ERR203 
character(len=*) :: ERR205 
character(len=*) :: ERR207 
character(len=*) :: ERR209 
character(len=*) :: ERR211 
character(len=*) :: ERR213 
character(len=*) :: ERR217 
character(len=*) :: ERR219 
character(len=*) :: ERR221 
character(len=*) :: ERR223 
character(len=*) :: ERR225 
character(len=*) :: ERR227 
character(len=*) :: ERR229 
character(len=*) :: ERR231 
character(len=*) :: ERR233 
character(len=*) :: ERR301 
character(len=*) :: ERR303 
character(len=*) :: ERR305 
character(len=*) :: ERR307 
character(len=*) :: ERR309 
character(len=*) :: ERR311 
character(len=*) :: ERR313 
character(len=*) :: ERR315 
character(len=*) :: ERR317 
character(len=*) :: ERR318 
character(len=*) :: ERR319 
character(len=*) :: ERR321 
character(len=*) :: ERR323 
character(len=*) :: ERR325 
character(len=*) :: ERR327 
character(len=*) :: ERR329 
character(len=*) :: ERR330 
character(len=*) :: ERR331 
character(len=*) :: ERR333 
character(len=*) :: ERR335 
character(len=*) :: ERR336 
character(len=*) :: ERR337 
character(len=*) :: ERR338 
character(len=*) :: ERR339 
character(len=*) :: ERR341 
character(len=*) :: ERR343 
character(len=*) :: ERR345 
character(len=*) :: ERR351 
character(len=*) :: ERR353 
character(len=*) :: ERR355 
character(len=*) :: ERR357 
character(len=*) :: ERR361 
character(len=*) :: ERR363 
character(len=*) :: ERR401 
character(len=*) :: ERR402 
character(len=*) :: ERR403 
character(len=*) :: ERR405 

!use the following as FMT string and reconstruct to replace %s with , A, ...
parameter (ERR101 = 'ERROR 101: memory allocation error.')
parameter (ERR103 = 'ERROR 103: cannot solve KW equations for Link %s.')
parameter (ERR105 = 'ERROR 105: cannot open ODE solver.')
parameter (ERR107 = 'ERROR 107: cannot compute a valid time step.')
parameter (ERR108 = 'ERROR 108: ambiguous outlet ID name for Subcatchment %s.')
parameter (ERR109 = 'ERROR 109: invalid parameter values for Aquifer %s.')
parameter (ERR110 = 'ERROR 110: ground elevation is below water table for Subcatchment %s.')
parameter (ERR111 = 'ERROR 111: invalid length for Conduit %s.')
parameter (ERR112 = 'ERROR 112: elevation drop exceeds length for Conduit %s.')
parameter (ERR113 = 'ERROR 113: invalid roughness for Conduit %s.')
parameter (ERR114 = 'ERROR 114: invalid number of barrels for Conduit %s.')
parameter (ERR115 = 'ERROR 115: adverse slope for Conduit %s.')
parameter (ERR117 = 'ERROR 117: no cross section defined for Link %s.')
parameter (ERR119 = 'ERROR 119: invalid cross section for Link %s.')
parameter (ERR121 = 'ERROR 121: missing or invalid pump curve assigned to Pump %s.')
parameter (ERR131 = 'ERROR 131: the following links form cyclic loops in the drainage system:')
parameter (ERR133 = 'ERROR 133: Node %s has more than one outlet link.')
parameter (ERR134 = 'ERROR 134: Node %s has illegal DUMMY link connections.')
parameter (ERR135 = 'ERROR 135: Divider %s does not have two outlet links.')
parameter (ERR136 = 'ERROR 136: Divider %s has invalid diversion link.')
parameter (ERR137 = 'ERROR 137: Weir Divider %s has invalid parameters.')
parameter (ERR138 = 'ERROR 138: Node %s has initial depth greater than maximum depth.')
parameter (ERR139 = 'ERROR 139: Regulator %s is the outlet of a non-storage node.')
parameter (ERR141 = 'ERROR 141: Outfall %s has more than 1 inlet link or an outlet link.')
parameter (ERR143 = 'ERROR 143: Regulator %s has invalid cross-section shape.')
parameter (ERR145 = 'ERROR 145: Drainage system has no acceptable outlet nodes.')
parameter (ERR151 = 'ERROR 151: a Unit Hydrograph in set %s has invalid time base.')
parameter (ERR153 = 'ERROR 153: a Unit Hydrograph in set %s has invalid response ratios.')
parameter (ERR155 = 'ERROR 155: invalid sewer area for RDII at node %s.')
parameter (ERR156 = 'ERROR 156: inconsistent data file name for Rain Gage %s.')
parameter (ERR157 = 'ERROR 157: inconsistent rainfall format for Rain Gage %s.')
parameter (ERR158 = 'ERROR 158: time series for Rain Gage %s is also used by another object.')
parameter (ERR159 = 'ERROR 159: recording interval greater than time series interval for Rain Gage %s.')
parameter (ERR161 = 'ERROR 161: cyclic dependency in treatment functions at node %s.')
parameter (ERR171 = 'ERROR 171: Curve %s has invalid or out of sequence data.')
parameter (ERR173 = 'ERROR 173: Time Series %s has its data out of sequence.')
parameter (ERR181 = 'ERROR 181: invalid Snow Melt Climatology parameters.')
parameter (ERR182 = 'ERROR 182: invalid parameters for Snow Pack %s.')
parameter (ERR183 = 'ERROR 183: no type specified for LID %s.')
parameter (ERR184 = 'ERROR 184: missing layer for LID %s.')
parameter (ERR185 = 'ERROR 185: invalid parameter value for LID %s.')
parameter (ERR186 = 'ERROR 186: invalid parameter value for LID placed in Subcatchment %s.')
parameter (ERR187 = 'ERROR 187: LID area exceeds total area for Subcatchment %s.')
parameter (ERR188 = 'ERROR 188: LID capture area exceeds total impervious area for Subcatchment %s.')
parameter (ERR191 = 'ERROR 191: simulation start date comes after ending date.')
parameter (ERR193 = 'ERROR 193: report start date comes after ending date.')
parameter (ERR195 = 'ERROR 195: reporting time step or duration is less than routing time step.')
parameter (ERR200 = 'ERROR 200: one or more errors in input file.')
parameter (ERR201 = 'ERROR 201: too many characters in input line ')
parameter (ERR203 = 'ERROR 203: too few items ')
parameter (ERR205 = 'ERROR 205: invalid keyword %s ')
parameter (ERR207 = 'ERROR 207: duplicate ID name %s ')
parameter (ERR209 = 'ERROR 209: undefined object %s ')
parameter (ERR211 = 'ERROR 211: invalid number %s ')
parameter (ERR213 = 'ERROR 213: invalid date/time %s ')
parameter (ERR217 = 'ERROR 217: control rule clause out of sequence ')
parameter (ERR219 = 'ERROR 219: data provided for unidentified transect ')
parameter (ERR221 = 'ERROR 221: transect station out of sequence ')
parameter (ERR223 = 'ERROR 223: Transect %s has too few stations.')
parameter (ERR225 = 'ERROR 225: Transect %s has too many stations.')
parameter (ERR227 = 'ERROR 227: Transect %s has no Mannings N.')
parameter (ERR229 = 'ERROR 229: Transect %s has invalid overbank locations.')
parameter (ERR231 = 'ERROR 231: Transect %s has no depth.')
parameter (ERR233 = 'ERROR 233: invalid treatment function expression ')
parameter (ERR301 = 'ERROR 301: files share same names.')
parameter (ERR303 = 'ERROR 303: cannot open input file.')
parameter (ERR305 = 'ERROR 305: cannot open report file.')
parameter (ERR307 = 'ERROR 307: cannot open binary results file.')
parameter (ERR309 = 'ERROR 309: error writing to binary results file.')
parameter (ERR311 = 'ERROR 311: error reading from binary results file.')
parameter (ERR313 = 'ERROR 313: cannot open scratch rainfall interface file.')
parameter (ERR315 = 'ERROR 315: cannot open rainfall interface file %s.')
parameter (ERR317 = 'ERROR 317: cannot open rainfall data file %s.')
parameter (ERR318 = 'ERROR 318: date out of sequence in rainfall data file %s.')
parameter (ERR319 = 'ERROR 319: invalid format for rainfall interface file.')
parameter (ERR321 = 'ERROR 321: no data in rainfall interface file for gage %s.')
parameter (ERR323 = 'ERROR 323: cannot open runoff interface file %s.')
parameter (ERR325 = 'ERROR 325: incompatible data found in runoff interface file.')
parameter (ERR327 = 'ERROR 327: attempting to read beyond end of runoff interface file.')
parameter (ERR329 = 'ERROR 329: error in reading from runoff interface file.')
parameter (ERR330 = 'ERROR 330: hotstart interface files have same names.')
parameter (ERR331 = 'ERROR 331: cannot open hotstart interface file %s.')
parameter (ERR333 = 'ERROR 333: incompatible data found in hotstart interface file.')
parameter (ERR335 = 'ERROR 335: error in reading from hotstart interface file.')
parameter (ERR336 = 'ERROR 336: no climate file specified for evaporation and/or wind speed.')
parameter (ERR337 = 'ERROR 337: cannot open climate file %s.')
parameter (ERR338 = 'ERROR 338: error in reading from climate file %s.')
parameter (ERR339 = 'ERROR 339: attempt to read beyond end of climate file %s.')
parameter (ERR341 = 'ERROR 341: cannot open scratch RDII interface file.')
parameter (ERR343 = 'ERROR 343: cannot open RDII interface file %s.')
parameter (ERR345 = 'ERROR 345: invalid format for RDII interface file.')
parameter (ERR351 = 'ERROR 351: cannot open routing interface file %s.')
parameter (ERR353 = 'ERROR 353: invalid format for routing interface file %s.')
parameter (ERR355 = 'ERROR 355: mis-matched names in routing interface file %s.')
parameter (ERR357 = 'ERROR 357: inflows and outflows interface files have same name.')
parameter (ERR361 = 'ERROR 361: could not open external file used for Time Series %s.')
parameter (ERR363 = 'ERROR 363: invalid data in external file used for Time Series %s.')
parameter (ERR401 = 'ERROR 401: general system error.')
parameter (ERR402 = 'ERROR 402: cannot open new project while current project still open.')
parameter (ERR403 = 'ERROR 403: project not open or last run not ended.')
parameter (ERR405 = 'ERROR 405: amount of output produced will exceed maximum file size; &
                    &either reduce Ending Date or increase Reporting Time Step.') !405 cont'd

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  NOTE: Need to update ErrorMsgs[], ErrorCodes[], and ErrorType
!        (in error.h) whenever a new error message is added.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!/

!char* ErrorMsgs[] =
!character(len=80), dimension(102) :: ErrorMsgs = &
!     (/'',    ERR101, ERR103, ERR105, ERR107, ERR108, ERR109, ERR110, ERR111, &
!      ERR112, ERR113, ERR114, ERR115, ERR117, ERR119, ERR121, ERR131, ERR133, &
!      ERR134, ERR135, ERR136, ERR137, ERR138, ERR139, ERR141, ERR143, ERR145, &
!      ERR151, ERR153, ERR155, ERR156, ERR157, ERR158, ERR159, ERR161, ERR171, &
!      ERR173, ERR181, ERR182, ERR183, ERR184, ERR185, ERR186, ERR187, ERR188, &
!      ERR191, ERR193, ERR195, ERR200, ERR201, ERR203, ERR205, ERR207, ERR209, &
!      ERR211, ERR213, ERR217, ERR219, ERR221, ERR223, ERR225, ERR227, ERR229, &
!      ERR231, ERR233, ERR301, ERR303, ERR305, ERR307, ERR309, ERR311, ERR313, &
!      ERR315, ERR317, ERR318, ERR319, ERR321, ERR323, ERR325, ERR327, ERR329, &
!      ERR330, ERR331, ERR333, ERR335, ERR336, ERR337, ERR338, ERR339, ERR341, &
!      ERR343, ERR345, ERR351, ERR353, ERR355, ERR357, ERR361, ERR363, ERR401, &
!      ERR402, ERR403, ERR405 /)

!int ErrorCodes[] =
integer, dimension(102) ::  ErrorCodes = &
  &(/ 0,      101,    103,    105,    107,    108,    109,    110,    111, &
     &112,    113,    114,    115,    117,    119,    121,    131,    133, &
     &134,    135,    136,    137,    138,    139,    141,    143,    145, &
     &151,    153,    155,    156,    157,    158,    159,    161,    171, &
     &173,    181,    182,    183,    184,    185,    186,    187,    188, &
     &191,    193,    195,    200,    201,    203,    205,    207,    209, &
     &211,    213,    217,    219,    221,    223,    225,    227,    229, &
     &231,    233,    301,    303,    305,    307,    309,    311,    313, &
     &315,    317,    318,    319,    321,    323,    325,    327,    329, &
     &330,    331,    333,    335,    336,    337,    338,    339,    341, &
     &343,    345,    351,    353,    355,    357,    361,    363,    401, &
     &402,    403,    405 /)

character(len=256) ::  ErrString

contains

!char* error_getMsg(int i)
character(len=80) function error_getMsg(i)
    integer, parameter :: K2 = selected_int_kind(2) !kind= 1
    integer(kind=K2), intent(in) :: i
!   if ( i >= 0 .and. i < MAXERRMSG ) then
!           error_getMsg = ErrorMsgs(i)
!   else 
!           error_getMsg = ErrorMsgs(0)
!   end if
!   return
   
    select case (i)
       case (0); error_getMsg = ''
       case (1); error_getMsg = ERR101
       case (2); error_getMsg = ERR103
       case (3); error_getMsg = ERR105
       case (4); error_getMsg = ERR107
       case (5); error_getMsg = ERR108
       case (6); error_getMsg = ERR109
       case (7); error_getMsg = ERR110
       case (8); error_getMsg = ERR111
       case (9); error_getMsg = ERR112
       case (10); error_getMsg = ERR113
       case (11); error_getMsg = ERR114
       case (12); error_getMsg = ERR115
       case (13); error_getMsg = ERR117
       case (14); error_getMsg = ERR119
       case (15); error_getMsg = ERR121
       case (16); error_getMsg = ERR131
       case (17); error_getMsg = ERR133
       case (18); error_getMsg = ERR134
       case (19); error_getMsg = ERR135
       case (20); error_getMsg = ERR136
       case (21); error_getMsg = ERR137
       case (22); error_getMsg = ERR138
       case (23); error_getMsg = ERR139
       case (24); error_getMsg = ERR141
       case (25); error_getMsg = ERR143
       case (26); error_getMsg = ERR145
       case (27); error_getMsg = ERR151
       case (28); error_getMsg = ERR153
       case (29); error_getMsg = ERR155
       case (30); error_getMsg = ERR156
       case (31); error_getMsg = ERR157
       case (32); error_getMsg = ERR158
       case (33); error_getMsg = ERR159
       case (34); error_getMsg = ERR161
       case (35); error_getMsg = ERR171
       case (36); error_getMsg = ERR173
       case (37); error_getMsg = ERR181
       case (38); error_getMsg = ERR182
       case (39); error_getMsg = ERR183
       case (40); error_getMsg = ERR184
       case (41); error_getMsg = ERR185
       case (42); error_getMsg = ERR186
       case (43); error_getMsg = ERR187
       case (44); error_getMsg = ERR188
       case (45); error_getMsg = ERR191
       case (46); error_getMsg = ERR193
       case (47); error_getMsg = ERR195
       case (48); error_getMsg = ERR200
       case (49); error_getMsg = ERR201
       case (50); error_getMsg = ERR203
       case (51); error_getMsg = ERR205
       case (52); error_getMsg = ERR207
       case (53); error_getMsg = ERR209
       case (54); error_getMsg = ERR211
       case (55); error_getMsg = ERR213
       case (56); error_getMsg = ERR217
       case (57); error_getMsg = ERR219
       case (58); error_getMsg = ERR221
       case (59); error_getMsg = ERR223
       case (60); error_getMsg = ERR225
       case (61); error_getMsg = ERR227
       case (62); error_getMsg = ERR229
       case (63); error_getMsg = ERR231
       case (64); error_getMsg = ERR233
       case (65); error_getMsg = ERR301
       case (66); error_getMsg = ERR303
       case (67); error_getMsg = ERR305
       case (68); error_getMsg = ERR307
       case (69); error_getMsg = ERR309
       case (70); error_getMsg = ERR311
       case (71); error_getMsg = ERR313
       case (72); error_getMsg = ERR315
       case (73); error_getMsg = ERR317
       case (74); error_getMsg = ERR318
       case (75); error_getMsg = ERR319
       case (76); error_getMsg = ERR321
       case (77); error_getMsg = ERR323
       case (78); error_getMsg = ERR325
       case (79); error_getMsg = ERR327
       case (80); error_getMsg = ERR329
       case (81); error_getMsg = ERR330
       case (82); error_getMsg = ERR331
       case (83); error_getMsg = ERR333
       case (84); error_getMsg = ERR335
       case (85); error_getMsg = ERR336
       case (86); error_getMsg = ERR337
       case (87); error_getMsg = ERR338
       case (88); error_getMsg = ERR339
       case (89); error_getMsg = ERR341
       case (90); error_getMsg = ERR343
       case (91); error_getMsg = ERR345
       case (92); error_getMsg = ERR351
       case (94); error_getMsg = ERR353
       case (95); error_getMsg = ERR355
       case (96); error_getMsg = ERR357
       case (97); error_getMsg = ERR361
       case (98); error_getMsg = ERR363
       case (99); error_getMsg = ERR401
       case (100); error_getMsg = ERR402
       case (101); error_getMsg = ERR403
       case (102); error_getMsg = ERR405
    end select
    return
end function error_getMsg

!int  error_getCode(int i)
integer function error_getCode(i)
    integer, intent(in) :: i
    if ( i >= 0 .and. i < MAXERRMSG ) then
            error_getCode = ErrorCodes(i)
    else 
            error_getCode = 0
    end if
    return
end function error_getCode

integer function error_setInpError(errcode, s)
    integer, intent(in) :: errcode
    character(len=*), intent(in) :: s
    ErrString = s
    error_setInpError = errcode
    return
end function error_setInpError

end module error
