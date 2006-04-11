C
C     data structure for uci creation
C
      INTEGER    MXRCH,MXCONN,MXLAND,MXMET,MXEXTS
      INTEGER    MAXRAT, MAXROW
C
      PARAMETER (MXRCH  = 2000)
      PARAMETER (MXCONN = 2000)
      PARAMETER (MXLAND = 10)
      PARAMETER (MXMET  = 20)
      PARAMETER (MXEXTS = 100)
      PARAMETER (MAXRAT=100)
      PARAMETER (MAXROW=25)
C
C     MXRCH  - maximum number of reaches
C     MXCONN - maximum number of reach to reach connections
C     MXLAND - maximum number of land uses
C     MXMET  - maximum number of met segment records
C     MXEXTS - maximum number of external source flows
C     MAXRAT - maximum number of rating curves that can be used
C     MAXROW - maximum number of rows in a rating curve
C
      COMMON /CGISIN/ NREACH,NRCONN,NMETSG,NEVAP,NLANDU,
     1                R2RCON,RCHMET,LANDPI,AREALR,PRCEVP,METDSN,RCHDSN,
     2                FTABNO,FOREST,LZSN,INFILT,LSUR,SLSUR,OUTSIT,
     3                KVARY,AGWRC,CEPSC,UZSN,NSUR,INTFW,IRC,LZETP,
     4                RETSC,LEN,DELTH,STCOR,KS,DB50,UPAREA,UPAREL,
     5                NUMEXT,EXTSRC,RAEXT,R2HYDR,R2PYR,R3NEXI,R3LKFG,
     6                R4HYFG,LAREA,NDSNIN,DSNIN,AREAIN,
     7                UPELEV,DNELEV,UPLAT,UPLON,DNLAT,DNLON,
     8                OUTLEV,RESMFG,RUNFG,SPOUT,UNIT,SDATIM,EDATIM,
     9                NUMRAT,NUMROW,RAT,AREA,WDMSFL

      INTEGER   NREACH,NRCONN,NMETSG,NEVAP,NLANDU,NUMEXT,
     1          R2RCON(MXCONN,2),RCHMET(MXRCH),OUTSIT(MXRCH),
     2          LANDPI(MXLAND),PRCEVP(MXMET),METDSN(MXMET),
     3          RCHDSN(MXRCH,10),FTABNO(MXRCH),EXTSRC(MXEXTS,2),
     4          R2HYDR(MXRCH),R2PYR(MXRCH),R3NEXI(MXRCH),R3LKFG(MXRCH),
     5          R4HYFG(19,MXRCH),NDSNIN,DSNIN(MXRCH),
     6          OUTLEV,RESMFG,RUNFG,SPOUT,UNIT,SDATIM(5),EDATIM(5),
     7          NUMRAT,NUMROW(MAXRAT),WDMSFL
      REAL      AREALR(MXLAND,MXRCH),FOREST(MXLAND,MXMET),
     1          LZSN(MXLAND,MXMET),INFILT(MXLAND,MXMET),
     2          LSUR(MXLAND,MXMET),SLSUR(MXLAND,MXMET),
     3          KVARY(MXLAND,MXMET),AGWRC(MXLAND,MXMET),
     4          CEPSC(MXLAND,MXMET),UZSN(MXLAND,MXMET),
     5          NSUR(MXLAND,MXMET),INTFW(MXLAND,MXMET),
     6          IRC(MXLAND,MXMET),LZETP(MXLAND,MXMET),
     7          RETSC(MXLAND,MXMET),LEN(MXRCH),DELTH(MXRCH),
     8          STCOR(MXRCH),KS(MXRCH),DB50(MXRCH),UPAREA(MXRCH),
     8          UPAREL(MXLAND,MXRCH,MXMET),
     9          RAEXT(MXEXTS),LAREA(MXRCH),AREAIN(MXRCH),
     1          UPELEV(MXRCH),DNELEV(MXRCH),UPLAT(MXRCH),
     2          UPLON(MXRCH),DNLAT(MXRCH),DNLON(MXRCH),
     3          RAT(MAXRAT,MAXROW,4),AREA(MAXRAT)
C
      COMMON /CGISC/ CLAND,CRCH,CMETS,CTSTYP,CEXTSR,RNINFO,CSCEN,WDMNAM
      CHARACTER*4    CTSTYP(MXMET)
      CHARACTER*8    CMETS(MXMET),CEXTSR(MXEXTS),CSCEN
      CHARACTER*12   CLAND(MXLAND),CRCH(MXRCH)
      CHARACTER*64   WDMNAM
      CHARACTER*80   RNINFO
C
C     key: U - user enters directly
C          I - interpreted but requires user confirmation
C          C - calculated from values entered directly
C          D - default values used
C
C     NREACH - U number of reaches in network
C     NRCONN - U number of reach to reach connections
C     NMETSG - U number of met segments (precip stations)
C     NEVAP  - U number of evap stations
C     NLANDU - U number of land uses
C     R2RCON - U array of reach to reach connections
C     RCHMET - U array of met segments assoc with each reach
C     LANDPI - U array of perv/imperv for each land use
C     AREALR - U array of areas of each land use contrib to each reach
C     PRCEVP - U array of precip/evap flags for each met station
C     METDSN - I array of met station data set numbers for each station
C     RCHDSN - I array of output dsns for each reach
C     FOREST - D (hspf parm) fraction of segment covered by forest
C     LZSN   - D (hspf parm) lower zone nominal storage
C     INFILT - D (hspf parm) index to infiltration capacity of soil
C     LSUR   - D (hspf parm) length of assumed overland flow plane
C     SLSUR  - D (hspf parm) slope of assumed overland flow plane
C     KVARY  - D (hspf parm) groundwater recession parameter
C     AGWRC  - D (hspf parm) groundwater recession rate
C     CEPSC  - D (hspf parm) interception storage capacity
C     UZSN   - D (hspf parm) upper zone nominal storage
C     NSUR   - D (hspf parm) mannings n for assumed overland flow plane
C     INTFW  - D (hspf parm) interflow infow parameter
C     IRC    - D (hspf parm) interflow recession parameter
C     LZETP  - D (hspf parm) lower zone et parameter
C     RETSC  - D (hspf parm) retention storage capacity of surface
C     FTABNO - D (hspf parm) ftable number for this reach
C     LEN    - D (hspf parm) length of each reach
C     DELTH  - D (hspf parm) diff in elevation for each reach
C     STCOR  - D (hspf parm) correction to the rchres depth to calculate stage
C     KS     - D (hspf parm) weighting factor for hydrologic routing
C     DB50   - D (hspf parm) median diameter of bed sediment
C     R2HYDR - D (hspf parm) hydr printout level flag
C     R2PYR  - D (hspf parm) pyr printout level flag
C     R3NEXI - D (hspf parm) number of exits
C     R3LKFG - D (hspf parm) lake flag
C     R4HYFG - D (hspf parm) rchres hydr flags
C     UPAREA - C total land area upstream of each reach
C     UPAREL - C area of each land use and met segment upstream of each reach
C     NUMEXT - I number of external source flows
C     EXTSRC - I connection of external source flow to reach
C     RAEXT  - I area of each external source flow
C     LAREA  - C area contributing to each reach from all land uses
C     NDSNIN - I number of candidate datasets to use as local inflows
C     DSNIN  - I array of candidate datasets to use as local inflows
C     AREAIN - I array of upstream areas for each candidate inflow dataset
C     UPELEV - U elevation at upstream end of each reach
C     DNELEV - U elevation at downstream end of each reach
C     UPLAT  - U latitude of upstream end of each reach
C     UPLON  - U longitude of upstream end of each reach
C     DNLAT  - U latitude of downstream end of each reach
C     DNLON  - U longitude of downstream end of each reach
C     OUTLEV - U hspf output level
C     RESMFG - U resume flag
C     RUNFG  - U run flag
C     SPOUT  - U special actions output level
C     UNIT   - U unit system
C     SDATIM - U start date
C     EDATIM - U end date
C     OUTSIT - I does user want output at this reach
C     RAT    - U array of values in each rating curve
C     NUMRAT - U number of rating curves available
C     AREA   - U drainage areas for each rating curve
C     NUMROW - U number of rows in each rating curve
C     WDMSFL -   file unit number of active wdm file
C     CSCEN  -   new scenario name
C     WDMNAM -   wdm file name
C
C     CLAND  - U array of land use names
C     CRCH   - U array of reach names
C     CMETS  - U array of met station names
C     CTSTYP - I array of tstype attributes for met data sets
C     CEXTSR - I array of names of external source flow locations
C     RNINFO - U run info
