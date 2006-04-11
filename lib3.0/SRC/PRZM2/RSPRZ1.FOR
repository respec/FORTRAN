C
C
C
      SUBROUTINE   PRZM
     I                 (RSTFG, NUMFIL, MCARLO, SEPTON, NITRON,
     I                  MODID, RSDAT, REDAT, LPRZRS,
     I                  LPRZOT, LPRZIN, LWDMS,
     I                  LMETEO, LSPTIC, LNITAD,
     I                  LTMSRS, SRNFG, BASEND, IPRZM, ITSAFT, NLDLT)
C
C     +  +  + PURPOSE +  +  +
C     called by EXESUP to execute PRZM
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     SRNFG,BASEND,RSTFG,NUMFIL,IPRZM,ITSAFT,NLDLT
      INTEGER     RSDAT(3),REDAT(3),LPRZRS,LPRZOT,
     1            LPRZIN,LMETEO,LSPTIC,LNITAD,LTMSRS,LWDMS
      LOGICAL     MCARLO,SEPTON,NITRON,APPLY
      CHARACTER*3 MODID(NUMFIL)
      REAL        CURVN
      INTEGER*4   RODPTH
C
C     +  +  + ARGUMENT DEFINITIONS +  +  +
C     RSTFG  - restart starting flag
C     NUMFIL - max. number of open files
C     MCARLO - flag for Monte Carlo on
C     SEPTON - septic effluent on flag
C     NITRON - nitrogen modeling on flag
C     MODID  - model id (pest,conc,water)
C     RSDAT  - restart starting date
C     REDAT  - restart ending date
C     LPRZRS - unit number for przm restart file
C     LPRZOT - unit number for przm output file
C     LPRZIN - unit number for przm input file
C     LMETEO - unit number for meteorlogical file
C     LSPTIC - unit number for septic effluent file
C     LNITAD - unit number for nitrogen atmospheric deposition
C     LTMSRS - unit number for time series file
C     LWDMS  - unit number for WDM file
C     SRNFG  - starting run flag
C     BASEND - base node for PRZM
C     IPRZM  - current przm zone
C     ITSAFT - current time step
C     NLDLT  - maximum days in a time step (31)
C
C     +  +  + PARAMETERS +  +  +
C
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXPDT.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXZON.INC'
C
C     +  +  + COMMON BLOCKS +  +  +
C
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CVMISC.INC'
      INCLUDE 'CPRZST.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CIRGT.INC'
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CPTAP.INC'
      INCLUDE 'CFILEX.INC'
      INCLUDE 'CBIO.INC'
      INCLUDE 'EXAM.INC'
      INCLUDE 'CNITR.INC'
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      INTEGER      J,I,LDAY,FDAY,JP1,MNTHP1,EYRFG,
     1             K, NMCDAY
      INTEGER      FLPS,FLCN
      REAL         ATEMP(2),PWIND(2),R0,
     1             OLDKH(NCMPTS),ZCH,URH,ZRH,TOTCR
      REAL*8       DKBIO(3,NCMPTS)
      CHARACTER*4  YEAR,MNTH,DAY,CONC
      INTEGER      ILDLT,IERROR
      LOGICAL      MCTFLG,IRDAY,FATAL
      CHARACTER*80 MESAGE
C
C     +  +  + INTRINSICS  +  +  + 
C      
      INTRINSIC MOD
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL SUBIN,RSTGET,RSTGT1,KHCORR,ACTION,GETMET,PLGROW
      EXTERNAL IRRIG,HYDROL,EVPOTR,HYDR1,HYDR2,EROSN,SLTEMP,FARM
      EXTERNAL PZSCRN,PESTAP,PLPEST,CANOPY,BIODEG,SLPST0,SLPST1
      EXTERNAL MOC,MASBAL,OUTCNC,OUTRPT,OUTPST,OUTHYD,OUTTSR
      EXTERNAL MCPRZ,RSTPUT,RSTPT1,SUBOUT,PRZEXM,ERRCHK
      EXTERNAL SEPTIN,NITR,NITRAP,NITBAL,OUTCNI,OUTNIT,ZIPR
C
C     +  +  + DATA INITIALIZATIONS +  +  +
C
      DATA YEAR /'YEAR'/
      DATA MNTH /'MNTH'/
      DATA DAY  /' DAY'/
      DATA CONC /'CONC'/
C
C     +  +  + OUTPUT FORMATS +  +  +
2000  FORMAT('Application [',I2,'] chem [',I1,
     1      '] on julday [',I3,'] year [',I2,'] zone [',I2,']')
2001  FORMAT('Application [',I2,'] chem [',I1,
     1      '] on julday [',I3,'] year [',I2,'] zone [',I2,']')
2002  FORMAT('ERROR, Application [',I2,'] failed ideal soil conditions')
2010  FORMAT('Nitrogen application [',I2,'] on julday [',I3,'] year [',
     $       I2,'] zone [',I2,']')
2020  FORMAT('ERROR, Nitrogen application [',I2,'] failed ideal soil ',
     $       'conditions')
C
C     +  +  + END SPECIFICATIONS +  +  +
C
      R0 = 0.0
      APPLY = .FALSE.
      MESAGE = 'PRZM'
      CALL SUBIN(MESAGE)
C     get unit numbers used for input and output
      FLPS= LPRZOT
      FLCN= LPRZOT
C
C     in restart mode
      CALL RSTGET (LPRZRS,IPRZM)
      CALL RSTGT1 (RSTFG,LPRZRS,IPRZM)
C
C     use dates passed as input rather than on input file
      ISTYR = RSDAT(1)
      ISMON = RSDAT(2)
      ISDAY = RSDAT(3)
      IEYR  = REDAT(1)
      IEMON = REDAT(2)
      IEDAY = REDAT(3)
C
C     check temperature simulation flag
      IF (ITFLAG .EQ. 1) THEN
        DO 178 K=1,NCHEM
          CALL KHCORR(SPT,HENRYK(K),ENPY(K),NCOM2,OLDKH)
          DO 177 I=1,NCOM2
            OKH(K,I) = OLDKH(I)
177       CONTINUE
178     CONTINUE
      ELSE
        DO 189 K=1,NCHEM
          DO 188 I=1,NCOM2
            OKH(K,I)   = HENRYK(K)
            KH(K,I)    = HENRYK(K)
188       CONTINUE
189     CONTINUE
      ENDIF
C
      NMCDAY = (ITSAFT-1)*NLDLT
      DO 200 IY=ISTYR,IEYR
        IF (MOD(IY,4) .NE. 0 .OR. MOD(IY,100) .EQ. 0) THEN
          LEAP=1
          LDAY=365
        ELSE
          LEAP=2
          LDAY=366
        ENDIF
        IF (IY .EQ. IEYR) LDAY=IEDAY+CNDMO(LEAP,IEMON)
        FDAY=1
        IF (IY .EQ. ISTYR) FDAY=ISDAY+CNDMO(LEAP,ISMON)
        EYRFG = 0
C
C       counter for VADOFT link
        ILDLT = 0
C       set input accumulator for GLOMAS
        DO 39 K = 1,NCHEM
          PTAP(K) = 0.
  39    CONTINUE
C
C       begin daily loop
        DO 100 JULDAY=FDAY,LDAY
          NMCDAY = NMCDAY + 1
          ILDLT = ILDLT + 1
          IF (JULDAY .EQ. LDAY) THEN
            EYRFG = 1
          ENDIF
          IF (LEAP .EQ. 2 .AND. DAYCNT .EQ. 366) DAYCNT = 0
          IF (LEAP .EQ. 1 .AND. DAYCNT .EQ. 365) DAYCNT = 0
          DAYCNT = DAYCNT + 1
C
          DO 40 J=1,12
            JP1= J+ 1
            IF (JULDAY.GT.CNDMO(LEAP,J) .AND.
     1         JULDAY.LE.CNDMO(LEAP,JP1)) MONTH = J
40        CONTINUE
          DOM=JULDAY-CNDMO(LEAP,MONTH)
          MNTHP1 = MONTH + 1
          SSFLAG = 0
          IF (IY.EQ.SAYR .AND. JULDAY.EQ.SAVAL) THEN
C           time for a special action
            CALL ACTION (LPRZIN,LPRZOT,MODID(3))
          END IF
C         get met data
          CALL GETMET(
     I      IY,JULDAY,MONTH,DOM,LMETEO,LSPTIC,LNITAD,FWDMS,
     I      LDAY,RSTFG,NITRON,SEPTON,
     O      RETCOD)

C         grow some crops
          CALL PLGROW(IRDAY)
C
          APDEP  = 0.0
          AINF(1)= 0.0
          THRUFL = 0.0
          IF (IRTYPE .GT. 0 .AND. IRNONE .NE. 4 .AND. RZI .EQ. 1) THEN
C           need to do irrigation
            CALL IRRIG
          ELSE
            IF (IRNONE .EQ. 4) THEN
              IF (IRDAY) THEN
                CALL IRRIG
              ELSE
                GOTO 555
              ENDIF
            ENDIF
          ENDIF
C
C         calculate surface hydrology factors
555   CONTINUE
          CALL HYDROL (LPRZOT,MODID(3),RODPTH,CURVN)
C
C         calculate et
          CALL EVPOTR
C
          IF (SEPTON) THEN
C           introduce septic effluent into soil column
            CALL SEPTIN
          END IF
C
          IF (HSWZT .EQ. 0) THEN
C           hydraulics with unrestricted drainage
            CALL HYDR1
          ELSEIF (HSWZT .EQ. 1) THEN
C           hydraulics with restricted drainage
            CALL HYDR2
          ENDIF
C
          SEDL= 0.0
          ELTT= 0.0
          IF (RUNOF .GT. 0.0 .AND. ERFLAG .EQ. 1) THEN
C           calc loss of chem due to erosion
            CALL EROSN
          END IF
C
          IF (NITRON) THEN
C           perform nitrogen simulation
            CALL SLTEMP (LPRZOT,MODID(3))
            CALL ZIPR (3*NCOM2,R0,SOILAP)
            IF (JULDAY.EQ.IAPDY(NAPPC) .AND. IY.EQ.IAPYR(NAPPC)) THEN
C             need to perform ag nitrogen application
              IF (FRMFLG .EQ. 1) THEN
C               check for appropriate soil moisture
                CALL FARM (RODPTH,APPLY,CURVN)
                IF (APPLY) THEN
C                 make ag nitrogen application
                  WRITE(MESAGE,2010) NAPPC,IAPDY(NAPPC),
     $                               IAPYR(NAPPC),IPRZM
                  CALL PZSCRN(1,MESAGE)
                  CALL NITRAP (FECHO)
                  NAPPC= NAPPC+ 1
                  WIN  = 0
                ELSE
C                 soil moisture not right for application, try again tomorrow
                  WIN = WIN + 1
                  IF (WIN .GT. WINDAY(NAPPC)) THEN
C                   beyond window of opportunity
                    WRITE(MESAGE,2020) NAPPC
                    IERROR= 2150
                    FATAL = .TRUE.
                    CALL ERRCHK(IERROR,MESAGE,FATAL)
                  ELSE
C                   try to apply tomorrow
                    IAPDY(NAPPC) = IAPDY(NAPPC) + 1
                  ENDIF
                ENDIF
              ELSE
                WRITE(MESAGE,2010) NAPPC,IAPDY(NAPPC),
     $                             IAPYR(NAPPC),IPRZM
                CALL PZSCRN(1,MESAGE)
                CALL NITRAP (FECHO)
                NAPPC= NAPPC+ 1
              ENDIF
            ENDIF
            IF (MCARLO) THEN
              CALL NITR (IY,MONTH,DOM,FECHO,IPRZM,MODID(13))
            ELSE
              CALL NITR (IY,MONTH,DOM,LPRZOT,IPRZM,MODID(13))
            END IF
C           perform mass balance for nitrogen constituents
            CALL NITBAL (APDEP,IPRZM)
            IF (ECHOLV .GE. 3) THEN
              CALL OUTHYD (LPRZOT,LTMSRS,MODID(3),MODID(5),SEPTON)
              IF (ITEM3 .EQ. CONC .AND. (STEP3 .EQ. DAY .OR. (STEP3
     1          .EQ. MNTH .AND. JULDAY .EQ. CNDMO(LEAP,MNTHP1)) .OR.
     2          (STEP3 .EQ. YEAR .AND. JULDAY .EQ. CNDMO(LEAP,13)))
     3          .AND. FLCN.GT.0) CALL OUTCNI (LPRZOT,MODID(6))
              CALL OUTNIT (FLPS,MODID(13),SEPTON)
              IF (NPLOTS .GT. 0) THEN
C               output time-series
                HEADER = HEADER + 1
                IF (HEADER .EQ. 1) SRNFG = 1
                CALL OUTTSR (SRNFG,EYRFG,LPRZOT,LTMSRS,LWDMS,
     I                       MODID(3),MODID(5),HEIGHT)
              ENDIF
            ENDIF
C           store PRZM nitrogen fluxes for vadoft, start w/ammonia
            PRZMPF(IPRZM,ILDLT,1) = PRZMPF(IPRZM,ILDLT,1) +
     $                              NCFX2(BASEND,1)/1.0E5
C           nitrate
            PRZMPF(IPRZM,ILDLT,2) = PRZMPF(IPRZM,ILDLT,2) +
     $                              NCFX4(BASEND,1)/1.0E5
C           combine the two organic species
            PRZMPF(IPRZM,ILDLT,3) = PRZMPF(IPRZM,ILDLT,3) +
     $                              (NCFX13(BASEND,1) +
     $                               NCFX15(BASEND,1))/1.0E5
          ELSE
C           perform pesticide simulation
            DO 95 K=1, NCHEM
              ELTERM(K) = ELTT*KD(K,1)
              DO 74 I=1,NCOM2
                SOILAP(K,I) = 0.0
                DKBIO(K,I)  = 0.0
74            CONTINUE
C
              IF (ITFLAG .EQ. 1) THEN
                IF (K .EQ. 1) CALL SLTEMP (LPRZOT,MODID(3))
                CALL KHCORR (SPT,HENRYK(K),ENPY(K),NCOM2,OLDKH)
                DO 75 I=1, NCOM2
                  KH(K,I) = OLDKH(I)
75              CONTINUE
              ENDIF
C
              PLNTAP(K) = 0.0
C
              IF (JULDAY.EQ.IAPDY(NAPPC) .AND. IY.EQ.IAPYR(NAPPC))THEN
                IF (FRMFLG .EQ. 1) THEN
C                 added new statement for farm option -jam 4/24/91
                  CALL FARM (RODPTH,APPLY,CURVN)
                  IF (APPLY) THEN
                    WRITE(MESAGE,2000) NAPPC,K,IAPDY(NAPPC),
     $                                 IAPYR(NAPPC),IPRZM
                    CALL PZSCRN(1,MESAGE)
                    CALL PESTAP(K)
                    PTAP(K) = PTAP(K) + TAPP(K,NAPPC) - PLNTAP(K)
                  ENDIF
                ELSE
                  WRITE(MESAGE,2001) NAPPC,K,IAPDY(NAPPC),
     $                               IAPYR(NAPPC),IPRZM
                  CALL PZSCRN(1,MESAGE)
                  CALL PESTAP(K)
C                 global mass balance
                  PTAP(K) = PTAP(K) + TAPP(K,NAPPC) - PLNTAP(K)
                ENDIF
              ENDIF
C
              IF (FAM .EQ. 2 .OR. FAM .EQ. 3) CALL PLPEST(K)
C
              IF (K .EQ. 1) THEN
                CNDBDY = DAIR/0.5
                CONDUC = CNDBDY
              ENDIF
C
C             When canopy develops, resistance type approach is used
C             to estimate the volatilization flux and concentration
C             retains in the canopy
C
              IF (HEIGHT .GT. 5.0) THEN
                ZCH = HEIGHT/100.0
                IF (ITFLAG .EQ. 0) THEN
                  ATEMP(1)= 15.0
                ELSE
                  ATEMP(1)= UBT
                ENDIF
                ATEMP(2)= TEMP
                PWIND(1)= 0.0
                PWIND(2)= WIND*36.0*24.0
                URH     = PWIND(2)
                IF (ITFLAG .EQ. 1) THEN
                  ZRH= ZWIND
                ELSE
                  ZRH= 2.0
                ENDIF
C
C               CONDUC was being calculated after the following
C               if then statement.  It should be calculated right
C               after the call CANOPY statement.  Change made by
C               PV @ AQUA TERRA Consultants, 10/93
C
                IF(HENRYK(K).GT.0.0.AND.URH.GT.0.0)THEN
                  CALL CANOPY(ATEMP,PWIND,ZRH,ZCH,URH,TOTCR,CRCNC)
                  CONDUC = 1.0 / (1.0/CNDBDY + TOTCR)
                ELSE
                  TOTCR=0.0
                ENDIF
C                CONDUC = 1.0 / (1.0/CNDBDY + TOTCR)
              ENDIF
C
C             Include calls to biodegradation subroutines here
C
              IF (BIOFLG .EQ. 1) THEN
                CALL BIODEG(K,DKBIO)
              ENDIF
C
C             end of biodegradation
C
              IF (MCFLAG.EQ.0 .OR. VLFLAG.EQ.0) THEN
                CALL SLPST0 (LPRZOT, MODID(3), K, DKBIO)
              ELSE
                CALL MOC(K)
                CALL SLPST1 (LPRZOT,MODID(3),K, DKBIO)
              END IF
C
C             calculate correction for dissolved to total solute conc.
              CALL MASBAL (APDEP,K,IPRZM)
C
              IF (MCOFLG .EQ. 0 .AND. ECHOLV .GE.3) THEN
C
                IF (ITEM3 .EQ. CONC .AND. (STEP3 .EQ. DAY .OR. (STEP3
     1            .EQ. MNTH .AND. JULDAY .EQ. CNDMO(LEAP,MNTHP1)) .OR.
     2            (STEP3 .EQ. YEAR .AND. JULDAY .EQ. CNDMO(LEAP,13)))
     3            .AND. FLCN.GT.0) CALL OUTCNC (LPRZOT,MODID(6),K)
C
C               Determine if a write to files MODOUT.DAT or SNAPSHOT.DAT
C               is required
                CALL OUTRPT (LPRZOT,MODID(7),MODID(8),K)
              ENDIF
C
              IF (ECHOLV .GE. 3) THEN
                IF (K .EQ. 1) CALL OUTHYD (
     I                           LPRZOT,LTMSRS,MODID(3),MODID(5),SEPTON)
                CALL OUTPST (
     I                       FLPS,MODID(4),K)
              ENDIF
              PRZMPF(IPRZM,ILDLT,K) = PRZMPF(IPRZM,ILDLT,K) +
     1          DFFLUX(K,BASEND) + ADFLUX(K,BASEND)
CPRH              DAFLUX(IPRZM,1,ILDLT,K) = DFFLUX(K,1) + ADFLUX(K,1) +
CPRH   1            PVFLUX(K,1)
C
              DO 90 I=1,NCOM2
CPRH                DAFLUX(IPRZM,I+1,ILDLT,K) = DFFLUX(K,I) + ADFLUX(K,I) +
CPRH   1              PVFLUX(K,I)
                SPESTR(K,I)=X(I)
C
C               store SPESTR for this zone (for use w/ MASCOR)
                PESTR(K,I)=SPESTR(K,I)*(THETN(I)+KD(K,I)*BD(I)+
     1                   (THETAS(I)-THETN(I))*KH(K,I))/THETN(I)
90            CONTINUE
C
C             last value of DAFLUX and ZPESTR is same as
C             in last compartment
CPRH              DAFLUX(IPRZM,NCOM2+2,ILDLT,K) = DFFLUX(K,NCOM2) +
CPRH   1            ADFLUX(K,NCOM2) + PVFLUX(K,NCOM2)
C
              IF (NPLOTS .GT. 0 .AND. K .EQ. NCHEM) THEN
                 HEADER = HEADER + 1
                 IF (HEADER .EQ. 1) SRNFG = 1
                 IF(ECHOLV .GE.3)CALL OUTTSR
     1             (SRNFG,EYRFG,LPRZOT,LTMSRS,LWDMS,MODID(3),MODID(5),
     2              HEIGHT)
              ENDIF
C
C             new code added for EXAMS
              IF (ERFLAG.EQ.1 .AND. IPRZM.EQ.1) THEN
                IF ((EXMFLG.EQ.1) .AND. (K.EQ.NCHEM)) CALL PRZEXM(K)
              ENDIF
C             end of code added for EXAMS
C
              SRNFG = 0
              IF (ITFLAG .EQ. 1) THEN
                DO 92 I=1,NCOM2
                  OKH(K,I) = KH(K,I)
92              CONTINUE
              ENDIF
95          CONTINUE
            IF (FRMFLG .EQ. 1) THEN
              IF (APPLY) THEN
                IF (JULDAY.EQ.IAPDY(NAPPC) .AND.
     $                  IY.EQ.IAPYR(NAPPC)) THEN
                  NAPPC= NAPPC+ 1
                  WIN = 0
                ENDIF
              ELSE
                IF (JULDAY.EQ.IAPDY(NAPPC) .AND.
     $                  IY.EQ.IAPYR(NAPPC)) THEN
                  WIN = WIN + 1
                  IF (WIN .GT. WINDAY(NAPPC)) THEN
                    WRITE(MESAGE,2002)NAPPC
                    IERROR = 2150
                    FATAL = .TRUE.
                    CALL ERRCHK(IERROR,MESAGE,FATAL)
                  ELSE
                    IAPDY(NAPPC) = IAPDY(NAPPC) + 1
                  ENDIF
                ENDIF
              ENDIF
            ELSE
              IF (JULDAY.EQ.IAPDY(NAPPC) .AND. IY.EQ.IAPYR(NAPPC))THEN
                NAPPC= NAPPC+ 1
              ENDIF
            ENDIF
          END IF
C
C         water flux to EXESUP
          PRZMWF(IPRZM,ILDLT) = PRZMWF(IPRZM,ILDLT) + AINF(BASEND)
C
C         transfer results to Monte Carlo arrays
          IF(MCARLO) THEN
            MCTFLG = .TRUE.
            CALL MCPRZ(
     I        MCTFLG,IPRZM,NMCDAY)
          ENDIF
C
100     CONTINUE
200   CONTINUE
      IF (RSTFG .EQ. 1 .OR. RSTFG .EQ. 2) THEN
C
C       Save state of system for next execution
        CALL RSTPUT (LPRZRS,IPRZM)
        CALL RSTPT1 (LPRZRS,IPRZM)
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   KHCORR
     I                   (STEMP,HENRY,ENP,NUMB,
     O                    NEWK)
C
C     + + + PURPOSE + + +
C     to correct Henry's constant using Clausius-Clapeyron equation
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NUMB
      REAL      STEMP(NUMB),NEWK(NUMB)
      REAL      HENRY,ENP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NUMB  - ???
C     STEMP - ???
C     NEWK  - ???
C     HENRY - ???
C     ENP   - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL      TMPK,TCORR,HENRY2
      INTEGER   I
C
C     + + + INTRINSICS + + +
      INTRINSIC LOG10
C
C     + + + END SPECIFICATIONS + + +
C
      IF (HENRY .GT. 0.0) THEN
C       perform corrections
        DO 10 I=1, NUMB
          TMPK=273.16+STEMP(I)
          TCORR=(298.16-TMPK)/(298.16*TMPK)/(2.302585*1.98718)
          HENRY2=LOG10(HENRY)
          NEWK(I)=HENRY2-1000.0*ENP*TCORR
          NEWK(I)=10**NEWK(I)
 10     CONTINUE
      ELSE
C       not a valid Henry's value, don't correct
        DO 20 I = 1,NUMB
          NEWK(I) = 0.0
 20     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
C
      SUBROUTINE   ACTION (LPRZIN,LPRZOT,MODID)
C
C     + + + PURPOSE + + +
C     identifies any special actions requested in the przm input file
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LPRZIN,LPRZOT
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LPRZIN - ???
C     LPRZOT - ???
C     MODID  - ???
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CMISC.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
C
C     + + + LOCAL VARIABLES + + +
      DIMENSION    ACTS(7)
      REAL         FRAC,RMULT1(10),RMULT3(10)
      INTEGER      MATCH,I,J,JT10,JP1,JP1T10,FLAG,SALEAP,JCOM
      CHARACTER*8  ACTS
      CHARACTER*80 MESAGE
C
C     +  +  + INTRINSICS +  +  +
C
      INTRINSIC NINT,MOD
C
C     +  +  + EXTERANLS +  +  +
C
      EXTERNAL SUBIN,SUBOUT
C
C     +  +  + DATA INITIALIZATIONS +  +  +
C
      DATA  ACTS / 'BD      ','CN      ','DSRATE  ','DWRATE  ',
     1       'KD      ','SNAPSHOT','USLEC   ' /
      DATA  RMULT1 /0.40,0.45,0.50,0.55,0.62,0.67,0.73,0.79,0.87,1.00/
      DATA  RMULT3 /2.22,1.85,1.67,1.50,1.40,1.30,1.21,1.14,1.07,1.00/
C
C     + + + OUTPUT FORMATS + + +
1000  FORMAT (1X,A3,/,1X,A3,1X,100(1H*),/,1X,A3,/,1X,A3,10X,
     1        'THE SPECIAL ACTION CALLED ''',A8,
     2        ''' DOES NOT EXIST FOR JULIAN DATE ',I3,',',I4,
     2          '.',/,1X,A3,/,1X,A3,1X,100(1H*))
1010  FORMAT (2X,3I2,1X,A8,1X,I3,3F8.0)
1020  FORMAT (1X,A3,/,1X,A3,1X,100(1H*),/,1X,A3,/,1X,A3,10X,
     1        'ERROR UPON A SPECIAL ACTION READ, VALUES DISPLAYED ',
     2        'BELOW:',/,1X,A3,/,1X,A3,10X,3I2,4X,A8,4X,I4,3G10.4,
     3        /,1X,A3,/,1X,A3,1X,100(1H*))
1030  FORMAT (1X,A3,/,1X,A3,1X,100(1H*),/,1X,A3,/,1X,A3,10X,
     1        'A SPECIAL ACTION READ FOUND DATES OUT OF ORDER, ',
     2        'VALUES DISPLAYED BELOW:',/,1X,A3,/,1X,A3,10X,3I2,4X,
     3        A8,4X,I4,3G10.4,/,1X,A3,/,1X,A3,10X,
     4        'CURRENT YEAR/JULIAN DATE: ',I2,'/',I3,
     4          '   NOT JULIAN DATE: ',I3,/,1X,A3,/,1X,A3,1X,100(1H*))
1040  FORMAT (1X,A3,/,1X,A3,1X,100(1H*),/,1X,A3,/,1X,A3,10X,3I2,4X,
     1        'THE VARIABLE ',A8,' CAN NOT USE A DIMENSION OF ',I4,
     2        /,1X,A3,/,1X,A3,1X,100(1H*))
1050  FORMAT (1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,/,1X,A3,1X,20A4,/)
1060  FORMAT (1X,A3,/,1X,A3,4X,3I2,'  VARIABLE ',A8,' FOR HORIZON #',
     1          I3,' NOW HAS A VALUE OF ',G10.4)
1061  FORMAT (1X,A3,/,1X,A3,4X,3I2,'  VARIABLE ',A8,' FOR HORIZON #',
     1          I3,' NOW HAS VALUES OF ',3G10.4)
1070  FORMAT (1X,A3,/,1X,A3,4X,3I2,'  VARIABLE ',A8,' FOR CROP #',I3,
     1          ' NOW HAS VALUES OF ',3I8)
1080  FORMAT (1X,A3,/,1X,A3,4X,3I2,'  VARIABLE ',A8,' FOR CROP #',I3,
     1          ' NOW HAS VALUES OF ',3G10.4)
1090  FORMAT (1X,A3,/,1X,A3,4X,3I2,'  SNAPSHOT REQUESTED')
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'ACTION'
      CALL SUBIN(MESAGE)
C     assign values in temporary locations
      FLAG   = 0
   4  IF  (SAYR - IY)     820, 6,900
   6  IF (SAVAL - JULDAY) 820,10,900
C
  10  CONTINUE
C     determine which special action was requested
      DO 200 MATCH = 1, 7
        IF (ACTS(MATCH) .EQ. SPACT) THEN
C
C         we have a match for a variable
          IF (MATCH .EQ. 1) THEN
C
C           match for the variable BD
            IF (NACTS .LT. 1 .OR. NACTS .GT. NHORIZ) GO TO 260
            DO 113 JCOM = NCOMBE(NACTS),NCOMEN(NACTS)
              BD(JCOM) = SPACTS(1)
  113       CONTINUE
            I = MATCH
            GO TO 210
          ELSEIF (MATCH .EQ. 2) THEN
C
C           match for the variable CN
            IF (NACTS .LT. 1 .OR. NACTS .GT. NDC) GO TO 260
            DO 50 I = 1, 3
              CN(NACTS,I,2) = NINT(SPACTS(I))
              DO 46 J = 1, 9
                JT10   = J * 10
                JP1    = J + 1
                JP1T10 = JP1 * 10
                IF (CN(NACTS,I,2) .GT. JT10 .AND. CN(NACTS,I,2)
     1            .LE. JP1T10) THEN
                  FRAC = (CN(NACTS,I,2) - JT10) / 10.00
                  CN(NACTS,I,1) = ((RMULT1(JP1)-RMULT1(J))*FRAC
     1                            + RMULT1(J)) * CN(NACTS,I,2)
                  CN(NACTS,I,3) = ((RMULT3(JP1)-RMULT3(J))*FRAC
     1                            +RMULT3(J)) * CN(NACTS,I,2)
                ENDIF
  46          CONTINUE
  50        CONTINUE
            I = MATCH
            GO TO 210
C
          ELSEIF (MATCH .EQ. 3) THEN
C           match for the variable DSRATE
            IF (NACTS .LT. 1 .OR. NACTS .GT. NHORIZ) GO TO 260
            DO 114 JCOM = NCOMBE(NACTS),NCOMEN(NACTS)
              DO 51 I = 1, NCHEM
                DSRATE(I,JCOM) = SPACTS(I)
   51         CONTINUE
  114       CONTINUE
            I = MATCH
            GO TO 210
C
          ELSEIF (MATCH .EQ. 4) THEN
C           match for the variable DWRATE
            IF (NACTS .LT. 1 .OR. NACTS .GT. NHORIZ) GO TO 260
            DO 116 JCOM = NCOMBE(NACTS),NCOMEN(NACTS)
              DO 52 I = 1, NCHEM
                DWRATE(I,JCOM) = SPACTS(I)
  52          CONTINUE
  116       CONTINUE
            I = MATCH
            GO TO 210
C
          ELSEIF (MATCH .EQ. 5) THEN
C           match for the variable KD
            IF (NACTS .LT. 1 .OR. NACTS .GT. NHORIZ) GO TO 260
            DO 117 JCOM = NCOMBE(NACTS),NCOMEN(NACTS)
              DO 47 I= 1, NCHEM
                KD(I,JCOM) = SPACTS(I)
  47          CONTINUE
  117       CONTINUE
            I = MATCH
            GO TO 210
C
          ELSEIF (MATCH .EQ. 6) THEN
C           match for the variable SNAPSHOT
            SSFLAG = 1
            I = MATCH
            GO TO 210
C
          ELSEIF (MATCH .EQ. 7) THEN
C           match for the variable USLEC
            IF (NACTS .LT. 1 .OR. NACTS .GT. NDC) GO TO 260
            DO 150 I = 1, NDC
              USLEC(NACTS,I) = SPACTS(I)
 150        CONTINUE
            I = MATCH
            GO TO 210
          ENDIF
        ENDIF
 200  CONTINUE
      IF (MCOFLG .EQ. 0) THEN
        WRITE(LPRZOT,1000) (MODID,J=1,4),SPACT,SAVAL,SAYR,
     1                   (MODID,J=1,2)
      ENDIF
      GO TO 220
C
C     write a summary of changes made
 210  CONTINUE
      IF (MCOFLG .EQ. 0) THEN
        IF (FLAG .EQ. 0) WRITE(LPRZOT,1050) (MODID,J=1,5),ATITLE
      ENDIF
      FLAG = 1
      IF (MCOFLG .EQ. 0) THEN
        IF (I .EQ. 2) THEN
          WRITE(LPRZOT,1070) (MODID,J=1,2),SADAY,SAMON,SAYR,SPACT,NACTS,
     1                     (CN(NACTS,JP1,2),JP1=1,3)
C          WRITE(LPRZOT,1075) ((MODID,CN(NACTS,JP1,J),J=1,3),JP1=1,3)
        ELSEIF (I .EQ. 7) THEN
          WRITE(LPRZOT,1080) (MODID,J=1,2),SADAY,SAMON,SAYR,SPACT,
     1                      NACTS,(SPACTS(I),I=1,3)
        ELSEIF (I .EQ. 3) THEN
          WRITE(LPRZOT,1061) (MODID,J=1,2),SADAY,SAMON,SAYR,SPACT,
     1                      NACTS,(SPACTS(I),I=1,3)
        ELSEIF (I .EQ. 4) THEN
          WRITE(LPRZOT,1061) (MODID,J=1,2),SADAY,SAMON,SAYR,SPACT,
     1                      NACTS,(SPACTS(I),I=1,3)
        ELSEIF (I .EQ. 5) THEN
          WRITE(LPRZOT,1061) (MODID,J=1,2),SADAY,SAMON,SAYR,SPACT,
     1                      NACTS,(SPACTS(I),I=1,3)
        ELSEIF (I .EQ. 6) THEN
          WRITE(LPRZOT,1090) (MODID,J=1,2),SADAY,SAMON,SAYR
        ELSE
          WRITE(LPRZOT,1060) (MODID,J=1,2),SADAY,SAMON,SAYR,SPACT,
     1                      NACTS,SPACTS(1)
        ENDIF
      ENDIF
C
C     read the next special action in
 220  CONTINUE
      READ(LPRZIN,1010,END=240,ERR=800)
     1        SADAY,SAMON,SAYR,SPACT,NACTS,(SPACTS(I),I=1,3)
C
C     modification to correct for leap year
      SALEAP=1
      IF(MOD(SAYR,4).EQ.0) SALEAP=2
      SAVAL=SADAY+CNDMO(SALEAP,SAMON)
C
C     end change
      GO TO 4
C
C     end of file encountered,
C     set special action's date artificially high
 240  SAVAL = 9999
      SAYR  = 9999
      GO TO 900
C
C     the variable NACTS contains an unacceptable range
 260  IF (MCOFLG .EQ. 0) THEN
        WRITE(LPRZOT,1040) (MODID,J=1,4),SADAY,SAMON,SAYR,SPACT,NACTS,
     1                   (MODID,J=1,2)
      ENDIF
      GO TO 220
C
C     error reading data in
C     display error message and read next value in
 800  IF (MCOFLG .EQ. 0) THEN
        WRITE(LPRZOT,1020) (MODID,J=1,6),SADAY,SAMON,SAYR,SPACT,
     1                    NACTS,SPACTS,(MODID,J=1,2)
      ENDIF
      GO TO 220
C
 820  IF (MCOFLG .EQ. 0) THEN
        WRITE(LPRZOT,1030) (MODID,J=1,6),SADAY,SAMON,SAYR,SPACT,
     1                    NACTS,SPACTS,(MODID,J=1,2),IY,JULDAY,SAVAL,
     2                    (MODID,J=1,2)
      ENDIF
      GO TO 220
C
C     return to calling program
 900  CONTINUE
      CALL SUBOUT
      RETURN
C
      END
C
C
C
      SUBROUTINE   GETMET
     I                   (IY,JULDAY,MONTH,DOM,LMETEO,LSPTIC,LNITAD,
     I                    LWDMS,LDAY,RSTFG,NITRON,SEPTON,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     gets met data for the specified day from old met file or wdmsfl
C     Modification date: 2/14/92 JAM
C     Further modifications were made at AQUA TERRA Consultants 9/93
C     in two areas:
C       Increased the size of the buffer used to read precipitation data,
C     from a WDM file, to store a whole year of data.  This eliminates
C     the need to perform a WDM read from disk for every month being
C     simulated. Reads from disk are now performed only once per year.
C       Also, the capability to read average monthly evaporation values
C     stored as attributes on the same WDM dataset as the precipitation
C     data has been added. These monthly values are then divided by the
C     number of days in the month to generate daily values to be used
C     in the simulation for the appropriate month.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     IY,MONTH,DOM,LMETEO,LSPTIC,LNITAD,RETCOD,
     1            RSTFG,LWDMS,JULDAY,LDAY
      LOGICAL     NITRON,SEPTON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IY     - ???
C     MONTH  - ???
C     DOM    - ???
C     JULDAY - ???
C     LDAY   - ???
C     LMETEO - unit number for meteorologic file
C     LSPTIC - unit number for septic effluent file
C     LNITAD - unit number for nitrogen atmospheric deposition file
C     LWDMS  - unit number of wdms file
C     RSTFG  - restart flag 1-first time thru, 2-later times thru
C     NITRON - nitrogen simulation on flag
C     SEPTON - septic effluent on flag
C     RETCOD - ???
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CMET.INC'
      INCLUDE 'CSPTIC.INC'
      INCLUDE 'CNITR.INC'
C
C     + + + SAVES + + +
      INTEGER      DELT,DTRAN(15),QUALFG,TUNITS,CURYR,CMTDSN(5),
     $             CSPDSN(4),CNDDSN(6),READFG
      SAVE         DELT,DTRAN,QUALFG,TUNITS,CURYR,CMTDSN,
     $             CSPDSN,CNDDSN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,MM,MD,MY,DATES(6),NVAL,IDY,IMO,
     1             IERROR,SAIND(12),SALEN,NDAYS,NXTMON
      REAL         R0,RSAVAL
      LOGICAL      FATAL
      CHARACTER*60 CADBUF
      CHARACTER*80 MESAGE
      CHARACTER*2  CDSN
      CHARACTER*4  CRCODE
C
C     + + + FUNCTIONS + + +
      INTEGER      DAYMON
      REAL         DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL SUBIN,ERRCHK,WDTGET,SUBOUT,WDBSGR,ZIPR,DAYMON,DAYVAL
C
C     + + + DATA INITIALIZATIONS + + +
      DATA SAIND /135,136,137,138,139,140,141,142,143,132,133,134/
      DATA DELT,DTRAN,QUALFG,TUNITS /1,1,1,0,0,1,10*0,30,4/
      DATA CURYR,CMTDSN,CSPDSN,CNDDSN/0,5*0,4*0,6*0/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(1X,3(I2),5F10.0)
 1010 FORMAT(1X,3I2,A60)
 1020 FORMAT(F10.0)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Simulation date (',I2,2('/',I2),
     1       ') ,meterological date (',I2,2('/',I2),') do not match')
 2010 FORMAT(I2)
 2020 FORMAT(I4)
C
C     + + + END SPECIFICATIONS + + +
C
      R0 = 0.0
      SALEN  = 1
      MESAGE = 'GETMET'
      CALL SUBIN(MESAGE)
      RETCOD= 0
      IMO= 2
      IF (DAYMON(DATES(1),IMO).EQ.29) THEN
C       leap year
        NVAL= 366
      ELSE
C       normal year
        NVAL= 365
      END IF
      NDAYS = DAYMON(DATES(1),IMO)
      IF (MONTH.LT.12) THEN
        NXTMON= MONTH + 1
      ELSE
        NXTMON= 1
      END IF
C
      IF (METDSN(1).EQ.0) THEN
C       read from old meteorolgical file
        IF (RSTFG.EQ.1) THEN
C         first read from met file, skip as needed
10        CONTINUE
            READ(LMETEO,1000,END=20) MM,MD,MY,PRECIP,PEVP,TEMP,
     1                               WIND,SOLRAD
          IF (MY.LT.IY.OR.MM.LT.MONTH.OR.MD.LT.DOM) GO TO 10
        ELSE
C         further read and check of meteorlogical data
          READ(LMETEO,1000,END=20) MM,MD,MY,PRECIP,PEVP,TEMP,
     1                             WIND,SOLRAD
          IF (MY.NE.IY.OR.MM.NE.MONTH.OR.MD.NE.DOM) THEN
            IERROR = 2000
            FATAL = .TRUE.
            WRITE(MESAGE,2000) DOM,MONTH,IY,MD,MM,MY
            CALL ERRCHK(
     I                  IERROR, MESAGE, FATAL)
            RETCOD= 1
          END IF
        END IF
        GO TO 30
20      CONTINUE
C
C         end of file reading metdata
          RETCOD= 2
30      CONTINUE
      ELSE
C       read from wdms file
        DATES(1)= IY + 1900
        DATES(2)= MONTH
        DATES(3)= DOM
        DATES(4)= 0
        DATES(5)= 0
        DATES(6)= 0
        DO 40 I = 1,5
          IF (CMTDSN(I).NE.METDSN(I) .OR. CURYR.NE.DATES(1)) THEN
C           time to read another year of data
            IF (METDSN(I).GT.0) THEN
C             retrieve time-series data from wdm file
              CALL WDTGET (LWDMS,METDSN(I),DELT,DATES,NVAL,
     I                     DTRAN(I),QUALFG,TUNITS,
     O                     DBUFF(1,I),RETCOD)
            ELSE IF (METDSN(I).EQ.-1) THEN
C             retrieve attribute values for evap from dataset
              IDY= 1
              DO 35 IMO= 1,12
C               get average monthly values for each month of year
                CALL WDBSGR (LWDMS,METDSN(1),SAIND(IMO),SALEN,
     O                       RSAVAL,RETCOD)
C               daily value is monthly average divided by days in month
C               also convert from inches to cms
                RSAVAL= RSAVAL/NDAYS
C               fill daily buffer with value
                CALL ZIPR (NDAYS,RSAVAL,DBUFF(IDY,I))
                IDY= IDY+ NDAYS
 35           CONTINUE
            ELSE IF (METDSN(I).EQ.0) THEN
C             this process not being modeled, no need to retrieve values
              CALL ZIPR (NVAL,R0,DBUFF(1,I))
            END IF
            IF (RETCOD .NE. 0) THEN
C             problem reading wdmsfl
              FATAL = .TRUE.
              WRITE(CDSN,2010)   METDSN(I)
              WRITE(CRCODE,2020) RETCOD
              MESAGE = 'Error reading WDMS file, data set number ['//
     1          CDSN//'] , return code ['//CRCODE//']'
              IERROR = 1295
              CALL ERRCHK(IERROR,MESAGE,FATAL)
            ENDIF
C           save current data-set number
            CMTDSN(I)= METDSN(I)
          END IF
  40    CONTINUE
C       get the current day's value
        PRECIP= DBUFF(JULDAY,1)
        PEVP  = DBUFF(JULDAY,2)
        TEMP  = DBUFF(JULDAY,3)
        WIND  = DBUFF(JULDAY,4)
        SOLRAD= DBUFF(JULDAY,5)
      END IF
C
      IF (SEPTON .AND. SEPDSN(1).EQ.0) THEN
C       read septic effluent from flat file
        IF (RSTFG.EQ.1) THEN
C         first read from flat file, skip as needed
50        CONTINUE
            READ(LSPTIC,1000,END=60) MM,MD,MY,INFLOW,AMMON,NITR,ORGN
          IF (MY.LT.IY.OR.MM.LT.MONTH.OR.MD.LT.DOM) GO TO 50
        ELSE
C         further read and check of meteorlogical data
          READ(LSPTIC,1000,END=60) MM,MD,MY,INFLOW,AMMON,NITR,ORGN
          IF (MY.NE.IY.OR.MM.NE.MONTH.OR.MD.NE.DOM) THEN
            IERROR = 2000
            FATAL = .TRUE.
            WRITE(MESAGE,2000) DOM,MONTH,IY,MD,MM,MY
            CALL ERRCHK(
     I                  IERROR, MESAGE, FATAL)
            RETCOD= 1
          END IF
        END IF
        GO TO 70
60      CONTINUE
C
C         end of file reading metdata
          RETCOD= 2
70      CONTINUE
      ELSE IF (SEPTON) THEN
C       read septic effluent from wdms file
        DATES(1)= IY + 1900
        DATES(2)= MONTH
        DATES(3)= DOM
        DATES(4)= 0
        DATES(5)= 0
        DATES(6)= 0
        DO 80 I = 1,4
          IF (CSPDSN(I).NE.SEPDSN(I) .OR. CURYR.NE.DATES(1)) THEN
C           time to read another year of data
            IF (SEPDSN(I).GT.0) THEN
C             retrieve time-series data from wdm file
              CALL WDTGET (LWDMS,SEPDSN(I),DELT,DATES,NVAL,
     I                     DTRAN(I+5),QUALFG,TUNITS,
     O                     SBUFF(1,I),RETCOD)
            ELSE
C             this process not being modeled, no need to retrieve values
              CALL ZIPR (NVAL,R0,SBUFF(1,I))
            END IF
            IF (RETCOD .NE. 0) THEN
C             problem reading wdmsfl
              FATAL = .TRUE.
              WRITE(CDSN,2010)   SEPDSN(I)
              WRITE(CRCODE,2020) RETCOD
              MESAGE = 'Error reading WDMS file, data set number ['//
     1          CDSN//'] , return code ['//CRCODE//']'
              IERROR = 1295
              CALL ERRCHK(IERROR,MESAGE,FATAL)
            ENDIF
C           save current data-set number
            CSPDSN(I)= SEPDSN(I)
          END IF
80      CONTINUE
C       get the current day's values
        INFLOW= SBUFF(JULDAY,1)
        AMMON = SBUFF(JULDAY,2)
        NITR  = SBUFF(JULDAY,3)
        ORGN  = SBUFF(JULDAY,4)
      END IF
C
      IF (NITRON) THEN
C       get any needed nitrogen atmospheric deposition values
        READFG = 0
        DO 200 I = 1,6
C         check next flag
          IF (NIADFG(I).EQ.-2) THEN
C           atmospheric depostion values input as monthly
            IF (I.LE.3) THEN
C             dry monthly value
              NIADDR(I)= DAYVAL(NIAFXM(MONTH,I),
     I                          NIAFXM(NXTMON,I),DOM,NDAYS)
            ELSE
C             generate wet value from input concentration and current precip
              NIADWT(I)= PRECIP*DAYVAL(NIACNM(MONTH,I),
     I                                 NIACNM(NXTMON,I),DOM,NDAYS)
            END IF
          ELSE IF (NIADFG(I).EQ.-1) THEN
C           read this atmospheric deposition value from flat file
            IF (RSTFG.EQ.1 .AND. READFG.EQ.0) THEN
C             first read from flat file, skip as needed
 150          CONTINUE
                READ(LNITAD,1010,END=160) MM,MD,MY,CADBUF
              IF (MY.LT.IY.OR.MM.LT.MONTH.OR.MD.LT.DOM) GO TO 150
              READFG = 1
            ELSE IF (READFG.EQ.0) THEN
C             further read and check of nitrogaen atmospheric deposition data
              READ(LNITAD,1010,END=160) MM,MD,MY,CADBUF
              IF (MY.NE.IY.OR.MM.NE.MONTH.OR.MD.NE.DOM) THEN
                IERROR = 2000
                FATAL = .TRUE.
                WRITE(MESAGE,2000) DOM,MONTH,IY,MD,MM,MY
                CALL ERRCHK(
     I                      IERROR, MESAGE, FATAL)
                RETCOD= 1
              END IF
              READFG = 1
            END IF
C           read atmospheric deposition value from text buffer
            IF (I.LE.3) THEN
C             read dry deposition value
              READ(CADBUF,1020) NIADDR(I)
            ELSE
C             read concentration value, generate deposition using precip
              READ(CADBUF,1020) NIADWT(I-3)
              NIADWT(I-3) = NIADWT(I-3) * PRECIP
            END IF
            CADBUF = CADBUF(11:60)
            GO TO 170
 160        CONTINUE
C
C             end of file reading metdata
              RETCOD= 2
 170        CONTINUE
          ELSE IF (NIADFG(I).GT.0) THEN
C           read septic effluent from wdms file
            DATES(1)= IY + 1900
            DATES(2)= MONTH
            DATES(3)= DOM
            DATES(4)= 0
            DATES(5)= 0
            DATES(6)= 0
            IF (CNDDSN(I).NE.NIADFG(I) .OR. CURYR.NE.DATES(1)) THEN
C             time to retrieve another year of time-series data from wdm file
              CALL WDTGET (LWDMS,NIADFG(I),DELT,DATES,NVAL,
     I                     DTRAN(I+9),QUALFG,TUNITS,
     O                     NBUFF(1,I),RETCOD)
              IF (RETCOD .NE. 0) THEN
C               problem reading wdmsfl
                FATAL = .TRUE.
                WRITE(CDSN,2010)   NIADFG(I)
                WRITE(CRCODE,2020) RETCOD
                MESAGE = 'Error reading WDMS file, data set number ['//
     1            CDSN//'] , return code ['//CRCODE//']'
                IERROR = 1295
                CALL ERRCHK(IERROR,MESAGE,FATAL)
              ENDIF
C             save current data-set number
              CNDDSN(I)= NIADFG(I)
            END IF
C           get the current day's values
            IF (I.LE.3) THEN
              NIADDR(I) = NBUFF(JULDAY,I)
            ELSE
              NIADWT(I-3) = NBUFF(JULDAY,I) * PRECIP
            END IF
          END IF
 200    CONTINUE
      END IF
C     save current year
      CURYR= DATES(1)
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   PLGROW(IRDAY)
C
C     + + + PURPOSE + + +
C     determines plant growth parameters for use in other routines
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      LOGICAL      IRDAY
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     IRDAY   - determines if today is an irrigation day
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMISC.INC'
C
C     + + + LOCAL VARIABLES + + +
      REAL         FRAC,DDLN
      INTEGER      I,IEXDAY,NEXDAY,NBYR,NEYR,J,NDYRS,NRZCOM,ILIN
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + + 
C
      INTRINSIC MOD,FLOAT,AMIN1,AMAX1
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,SUBOUT
C
C     + + + END SPECIFICATIONS + + +
C
C     determine which crop is in season. if a new crop is beginning,
C     determine the crop type and the length of the growing season.
C
      MESAGE = 'PLGROW'
      CALL SUBIN(MESAGE)
C
      DO 60 I= 1, NCPDS
        IF (JULDAY .EQ. IEMER(I) .AND. IY .EQ. IYREM(I)) THEN
          IRDAY = .TRUE.
          ISCOND=2
          RZI=1
          NDCNT=0
          DO 10 J= 1, NDC
            IF (INCROP(I) .EQ. ICNCN(J)) NCROP=J
10        CONTINUE
          NCP  = I
          NDYRS= IYRMAT(I)- IYREM(I)
          IF (NDYRS .LE. 0) THEN
            TNDGS(I)= MAT(I)- IEMER(I)
          ELSE
            NEXDAY = 0
            IF (MOD(IYRMAT(I),4) .EQ. 0) NEXDAY= NEXDAY+1
            IF (MOD(IYREM(I),4) .EQ. 0)  NEXDAY= NEXDAY+1
            IF (NDYRS .LE. 1) THEN
              TNDGS(I)= 365- IEMER(I)+ MAT(I)+ NEXDAY
            ELSE
              NBYR  = IYREM(I)+1
              NEYR  = IYRMAT(I)-1
              IEXDAY= 0
              DO 40 J=NBYR, NEYR
                IF (MOD(J,4) .EQ. 0) IEXDAY= IEXDAY+1
40            CONTINUE
              TNDGS(I)= MAT(I)- IEMER(I)+(365*NDYRS)+ IEXDAY+ NEXDAY
            ENDIF
          ENDIF
        ENDIF
60    CONTINUE
C
C     determine fraction of time elapsed between crop emergence and
C     maturation and compute crop growth parameters
      FRAC  = 0.
      COVER = 0.
      WEIGHT= 0.
      HEIGHT= 0.
      NCOM1 = NCOM0
      DIN   = 0.
      IF (NCP .NE. 0 .AND. RZI .NE. 0) THEN
        NDCNT = NDCNT+1
        FRAC  = AMIN1(1.0,FLOAT(NDCNT)/FLOAT(TNDGS(NCP)))
        COVER = COVMAX(NCROP)*FRAC
        WEIGHT= WFMAX(NCROP)*FRAC
        HEIGHT= HTMAX(NCROP)*FRAC
C
        ILIN = 0
        DDLN = 0.0
70      CONTINUE
          ILIN = ILIN + 1
          DDLN = DDLN + DELX(ILIN)
        IF (AMXDR(NCROP)*FRAC .GT. DDLN) GO TO 70
        NRZCOM = ILIN
        NCOM1 = AMAX1 (FLOAT(NRZCOM),FLOAT(NCOM0))+ 0.5
        DIN   = CINTCP(NCROP)*COVER
C
C       set soil surface condition after harvest and
C       turn off root zone flag
        IF (IY .EQ. IYRHAR(NCP) .AND. JULDAY .EQ. IHAR(NCP)) THEN
          IRDAY = .FALSE.
          ISCOND= ICNAH(NCROP)
          RZI   = 0
          IFSCND(NCROP) = IPSCND
          COVER = 0.0
          HEIGHT= 0.0
          WEIGHT= 0.0
        ENDIF
C
C       set uptake flags if crop is growing
        IF (RZI .NE. 0) THEN
          DO 120 I=1,NRZCOM
            GAMMA(I)= UPTKF
120       CONTINUE
        ENDIF
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   IRRIG
C
C     + + + PURPOSE + + +
C     determines soil moisture deficit, decides
C     if irrigation is needed, and calculates irrigation
C     depths.
C     Modification date: 2/14/92 JAM
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CCROP.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CIRGT.INC'
      INCLUDE 'CMET.INC'
C
C     + + + LOCAL VARIABLES + + +
      REAL         SMCRIT,SMAVG,FCAVG,XLOC,SLOPE,DDLN
      INTEGER      I,ILIN
      CHARACTER*80 MESAGE
C
C     local variable definitions:
C          SMCRIT...soil moisture level where irrigation begins
C                   (fraction).
C          SMAVG....average root zone soil moisture level (fraction).
C          FCAVG....average root zone field capacity (fraction).
C          XLOC.....location in furrow (fraction of length)
C          SLOPE....slope in infiltration - distance curve for furrow
C
C     + + + INTRINSICS + + +
C
      INTRINSIC MIN
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,FURROW,SUBOUT
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'IRRIG'
      CALL SUBIN(MESAGE)
C
C     initialization
      APDEP = 0.0
      SMDEF = 0.0
C
C     compute average soil moisture and porosity for root zone
      DW = 0.0
      SMCRIT = 0.0
      SMAVG = 0.0
      FCAVG = 0.0
      DO 10 I=1,NCOMRZ
        SMCRIT = SMCRIT + (PCDEPL*(THEFC(I)-THEWP(I))+THEWP(I))/NCOMRZ
        SMAVG = SMAVG + THETO(I)/NCOMRZ
        FCAVG = FCAVG + THEFC(I)/NCOMRZ
        DW = DW + (THETAS(I)-THETO(I))/NCOMRZ
   10 CONTINUE
C
C     check if irrigation is needed
      IF(SMAVG.GE.SMCRIT .OR. PRECIP.GT.0.0) GO TO 800
C
      DDLN = 0.0
      DO 321 ILIN = 1, NCOMRZ
        DDLN = DDLN +DELX(ILIN)
321   CONTINUE
      SMDEF = (FCAVG  - SMAVG)*(1.0 + FLEACH)*DDLN
C
C     flood irrigation
C
      IF(IRTYPE .EQ. 1)THEN
        APDEP   = SMDEF
        AINF(1) = APDEP
C
C     furrow irrigation
      ELSE IF(IRTYPE .EQ. 2)THEN
C
C       compute infiltration down the furrow
        CALL FURROW
C
C       use infiltration at a specific location in furrow
C       (XFRAC greater than 0.0)
C
        IF(XFRAC .GE. 0.0)THEN
C
C         find infiltration at location XFRAC in the furrow
          XLOC = 0.0
          DO 20 I=2,NSPACE
            XLOC = XLOC + DX/XL
            IF(XLOC .GE. XFRAC)THEN
              SLOPE = (FS(I)-FS(I-1))/DX
              APDEP = 100.*(FS(I) - SLOPE*XL*(XLOC - XFRAC))
              AINF(1) = APDEP
              GO TO 800
            END IF
   20     CONTINUE
C
C       use average furrow infiltration depth (XFRAC less than 0.0)
        ELSE
          DO 30 I=1,NSPACE
             APDEP = APDEP + 100.*FS(I)/NSPACE
   30     CONTINUE
          AINF(1) = APDEP
        END IF
C
C     Over-Canopy Sprinkler Irrigation;  irrigation applied above
C     the canopy as precipitation
      ELSE IF(IRTYPE .EQ. 3)THEN
         PRECIP = MIN(RATEAP*24.0 , SMDEF+DIN-CINT)
C
C     Under-Canopy Sprinkler Irrigation; irrigation applied as
C     under-canopy throughfall
      ELSE
        THRUFL = MIN(RATEAP*24.0 , SMDEF)
C       for sprinkler M-C, generate value of THRUFL here
C       from normal distribution, mean = APMEAN, std. dev. = APSTD
C
      END IF
C
 800  CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   FURROW
C
C     + + + PURPOSE + + +
C     computes flow and infiltration down a furrow using
C     a Kinematic Wave with Green-Ampt infiltration.
C     flow equation is approximated by a backwards-time and -space
C     finite difference, solved by iteration.
C     Modification date: 2/14/92 JAM
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CIRGT.INC'
C
C     + + + LOCAL VARIABLES + + +
      REAL         FERR,DT,TEND,TIME,X,XADV,Q,QEND,Q1,Q2,VOLIN,FVOL,
     1             VCHAN,VINF,S,DVOL,A,A0,AUS,A1,A2,AOLD,B,B0,B2,BUS,
     2             Y1,Y2,Y0,YAVG,Y,YUS,DEPTH,P1,P2,R1,R2,EM,ALPH,
     3             DEL,DEL2,DELQ,FQ,FQ2,DA,D2A,DB,D2B,DFQ,D2FQ,QOLD,
     4             RATIO,TERM,OMEGA,ERROR,DTREM,DVOLUS,BOLD,FSOLD
      INTEGER      I,N,IX,NTIME,ITER,NTREM,IT
      CHARACTER*80 MESAGE
      REAL         LNCHK
      REAL*8       SQRCHK
C
C     Local variable definitions; units are SI (m,s):
C           FERR...error tolerance for iteration, as fraction of OMEGA
C           OMEGA...known terms in finite difference equation
C           FQ....unknowns in finite difference equation
C           DT....time step for flow computations (s)
C           DTREM...time step after furrow is flowing full
C
C           First letter naming conventions:
C                       Q   = flow rates (m3/s)
C                       A   = cross-sectional areas (m2)
C                       B   = flow top widths (m)
C                       Y   = flow depths (m)
C                       D** = first derivative of **
C                       D2** = second derivative of **
C                       VOL = volumes (m3)
C
C     + + + INTRINSICS + + +
C
      INTRINSIC NINT,REAL,DBLE,ABS
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,LNCHK,INFIL,SQRCHK,SUBOUT
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'FURROW'
      CALL SUBIN(MESAGE)
C
C     initialization
      FERR  = .01
      DX    = 10.
      DT    = 60.
      TEND  = 86400.
      TIME  = 0.0
      X     = 0.0
      VOLIN = 0.0
      NSPACE= NINT(XL/DX + 1)
      NTIME =  NINT(TEND/DT)
      DO 100 I=1,NSPACE
        QS(I) = 0.0
        FS(I) = 0.0
  100 CONTINUE
C
C     Compute cross section Area-Flow relationship
C         A = ALPH * Q**EM
      Y1 = .01
      Y2 = .10
      A1 = BT*Y1 + ZRS*Y1**2.
      A2 = BT*Y2 + ZRS*Y2**2.
      P1 = BT + REAL(SQRCHK(DBLE(1.+ZRS**2.)))*2.*Y1
      P2 = BT + REAL(SQRCHK(DBLE(1.+ZRS**2.)))*2.*Y2
      R1 = A1/P1
      R2 = A2/P2
      Q1 = A1*(R1**.66667)*(SF**.5)/EN
      Q2 = A2*(R2**.66667)*(SF**.5)/EN
      EM = (LNCHK(A2)-LNCHK(A1))/(LNCHK(Q2)-LNCHK(Q1))
      ALPH = A1/(Q1**EM)
C
C     compute conditions at head of furrow
      QS(1) = Q0
      QEND  = Q0
      A0 = ALPH*Q0**EM
      B0 = REAL(SQRCHK(DBLE(BT**2 + 4.0*A0*ZRS)))
      Y0 = (B0 - BT)/(2.0*ZRS)
      YAVG = A0/B0
C
C     finite - difference time loop
C
      DO 200 N=1,NTIME
        X = 0.0
        FVOL = 0.0
        TIME = TIME + DT
C
C       compute infiltration at head of furrow and set upstream
C       boundary conditions
        CALL INFIL(KS,YAVG,HF,DW,DT,FERR,FS(1))
        VOLIN = VOLIN + Q0*DT
        DVOLUS = 0.0
        Q = Q0
        DEPTH = Y0
        A = A0
        B = B0
C
C       finite - difference space loop -- solve for flow rate at
C       each downstream station by iteration
        DO 150 I= 2,NSPACE
          X = X + DX
C
C         Calculate current infiltration;   compute finite difference
C         terms for known upstream and previous conditions:
C
          AOLD = ALPH*QS(I)**EM
          BOLD = REAL(SQRCHK(DBLE(BT**2 + 4.*AOLD*ZRS)))
          FSOLD = FS(I)
          Y = AOLD/BOLD
          CALL INFIL(KS,Y,HF,DW,DT,FERR,FS(I))
          OMEGA = (DT*Q/DX) + AOLD + BOLD*FSOLD
          ERROR = ABS(FERR*OMEGA)
          AUS = A
          BUS = B
          YUS = DEPTH
C
C         Compute finite difference terms for first guess of new Q
C         (first guess = upstream flow rate):
C
          FQ = (DT*Q/DX) + A + B*FS(I)
C
C         Iteration using 2nd order Taylor series expansion to
C         compute next guess of Q; maximum of 10 iterations allowed:
C
          DO 120 ITER = 1,10
C
C           Compute derivatives with respect to Q:
C
            DA  = ALPH*EM*Q**(EM-1.0)
            D2A = ALPH*EM*(EM-1.0)*Q**(EM-2.0)
            DB  = 2.*ZRS*DA/B
            D2B = 2.*ZRS*D2A/B - 4.*(ZRS**2)*(DA**2)/(B**3)
            DFQ = DT/DX + DA + DB*FS(I)
            D2FQ= D2A + D2B*FS(I)
C
C           Compute new guesses of Q (two roots):
C
            RATIO= DFQ/D2FQ
            TERM = RATIO**2 - 2.*(FQ-OMEGA)/D2FQ
            IF(TERM .LE. 0.0)TERM=0.0
            TERM = REAL(SQRCHK(DBLE(TERM)))
            QOLD = Q
C
            Q    = QOLD - RATIO + TERM
            IF(Q .LE. 0.0)Q=Q0/100.
            A    = ALPH*Q**EM
            B    = REAL(SQRCHK(DBLE(BT**2 + 4.*A*ZRS)))
            FQ   = (DT*Q/DX) + A + B*FS(I)
            DEL  = ABS(FQ - OMEGA)
C
            Q2   = QOLD - RATIO - TERM
            IF(Q2 .LE. 0.0)Q2=Q0/100.
            A2   = ALPH*Q2**EM
            B2   = REAL(SQRCHK(DBLE(BT**2 + 4.*A2*ZRS)))
            FQ2  = (DT*Q2/DX) + A2 + B2*FS(I)
            DEL2 = ABS(FQ2 - OMEGA)
C
C           Choose Q with smallest error DEL; check for convergence:
C
            IF (DEL2 .LT. DEL) THEN
              Q  = Q2
              A  = A2
              B  = B2
              FQ = FQ2
              DEL= DEL2
            END IF
            IF (DEL .LE. ERROR) GO TO 125
  120     CONTINUE
C
C         If no convergence after 10 iterations, truncate wave front
C         (exit finite-difference spatial loop):
C
          GO TO 199
C
C         Convergence; end iteration loop and compute volumes
C         in furrow channel and ground:
C
125       CONTINUE
          S    = (DEPTH - YUS)/DX
          VCHAN= AUS*DX + S*BUS*(DX**2)/2. + ZRS*(S**2)*(DX**3)/3.
          VINF = (BUS*FS(I-1) + B*FS(I))*DX/2.0
          FVOL = FVOL + VCHAN + VINF
C
C         Store new value of flow rate Q and check volume balance;
C         If volume in channel + volume infiltrated is greater than
C         total inflow volume, truncate wave front (exit F-D space loop):
C
          DVOL = ABS(FVOL - VOLIN)
          IF (FVOL .LT. VOLIN .OR. DVOL .LT. DVOLUS) THEN
            QS(I) = Q
            DEPTH = (B-BT)/(2.*ZRS)
            DVOLUS = DVOL
            XADV = X
          ELSE
            GO TO 199
          END IF
  150   CONTINUE
C
C       Check if change in flow at end of furrow is significant
C       (greater than 5 percent). If not, discontinue finite
C       difference flow computations.
C
  199   IF (ABS(XADV-XL).LT.1.0E-5) THEN
          IF (FS(NSPACE) .GE. SMDEF/100.) GO TO 800
          DELQ = (QEND-Q)/QEND
          IF (DELQ .LE. .05) GO TO 299
          QEND = Q
        END IF
  200 CONTINUE
C
C     End of Finite Difference Flow computation loops;
C     Compute infiltration for remainder of day assuming
C     constant flow in the channel:
C
  299 CONTINUE
      NTREM = (NTIME - N)/60  + 1
      DTREM = DT*10.
      DO 400 IT = 1,NTREM
        DO 300 IX = 1,NSPACE
          A = ALPH*QS(IX)**EM
          B = REAL(SQRCHK(DBLE(BT**2 + 4.*A*ZRS)))
          Y = A/B
          CALL INFIL(KS,Y,HF,DW,DTREM,FERR,FS(IX))
  300   CONTINUE
        IF (FS(NSPACE) .GE. SMDEF/100.) GO TO 800
  400 CONTINUE
C
 800  CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE INFIL (KS,D,HF,DW,DT,FERR,FC)
C
C     Computes Green-Ampt infiltration assuming
C     a constant depth over time step DT.  The integrated
C     Green-Ampt equation is solved by iteration for
C     the new value of cumulative infiltration FC.
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      REAL KS,D,HF,DW,DT,FERR,FC
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C             KS = saturated hydraulic conductivity (L/T)
C             D  = depth of surface water (L)
C             HF = Green-ampt suction parameter (L)
C             DW = porosity (unitless)
C             DT = time interval
C             FERR = iteration convergence criteria, as fraction
C             FC = cumulative infiltration (L)
C
C     + + + LOCAL VARIABLES + + +          
C
      REAL         H,OMEGA,ERROR,FPREV,FINIT,FI,DFI,D2FI,RATIO,
     1             TERM,FCOLD,DEL,FC2,FI2,DEL2
      INTEGER      ITER
      CHARACTER*80 MESAGE
      REAL         LNCHK
      REAL*8       SQRCHK
C
C     + + + INTRINSICS + + + 
C      
      INTRINSIC ABS,REAL,DBLE
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,LNCHK,SQRCHK,SUBOUT
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'INFIL'
      CALL SUBIN(MESAGE)
C
C     Compute terms known from conditions at start of DT:
C
      H = (D+HF)*DW
      OMEGA = KS*DT
      ERROR = ABS(FERR*OMEGA)
      FPREV = FC -H*LNCHK(H+FC)
      FINIT = FC
C
C     Initial guess of new cumulative inf. FC based on old infiltration:
C
      FC = FC + KS*DT
C
C     Iteration using 2nd order Taylor series to compute next
C     guess of FC:
C
      FI = FC - H*LNCHK(H+FC) - FPREV
      DO 100 ITER = 1,10
        DFI = 1. - H/(H+FC)
        D2FI = H/(H+FC)**2
        RATIO = DFI/D2FI
        TERM = RATIO**2 - 2.*(FI-OMEGA)/D2FI
        IF(TERM .LE. 0.0)TERM = 0.0
        TERM = REAL(SQRCHK(DBLE(TERM)))
        FCOLD = FC
C
C       Two roots:  choose root with the smallest error:
C
        FC = FCOLD - RATIO + TERM
        IF(FC.LE.0.0)FC=0.0
        FI = FC - H*LNCHK(H+FC) - FPREV
        DEL = ABS(FI-OMEGA)
        FC2 = FCOLD - RATIO - TERM
        IF(FC2.LE.0.0)FC2=0.0
        FI2 = FC2 - H*LNCHK(H+FC2) - FPREV
        DEL2 = ABS(FC2-OMEGA)
        IF(DEL2 .LT. DEL)THEN
          FC = FC2
          FI = FI2
          DEL = DEL2
        END IF
C
C       Check for convergence
C
        IF(DEL .LE. ERROR)GO TO 111
  100 CONTINUE
  111 IF(ITER .GE. 10)FC = FINIT
C
      CALL SUBOUT
C
      RETURN
      END
C
      SUBROUTINE HYDROL (LPRZOT,MODID,RODPTH,CURVN)
C
C     This subroutine calculates snowmelt, crop interceptoin,
C     runoff, and infiltration from the soil surface
C     Modification date: 2/7/92 JAM
C     Further modified by PV @ AQUA TERRA Consultants 9/93 to
C     hard-wire the depth of runoff calculation to 10 cm
C
C     +  +  + DUMMY ARGUMENTS +  +  + 
C
      INTEGER     LPRZOT
      CHARACTER*3 MODID
C
C     +  +  + ARGUMENT DEFINITIONS +  +  +
C
C     LPRZOT  - Fortran unit number for output file LPRZOT
C     MODID - character string for output file identification
C
C     +  +  + PARAMETERS +  +  +  
C
      INCLUDE 'PPARM.INC'
C
C     +  +  + COMMON BLOCKS +  +  +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
C
C     +  +  + LOCAL VARIABLES +  +  +  
C
      REAL         TWLVL,CURVN,WLVL,AAA,DDLNJ
      INTEGER      I,RODPTH,MLIN
      CHARACTER*80 MESAGE
C
C     +  +  + INTRINSICS +  +  +
C
      INTRINSIC AMIN1,AMAX1
C
C     +  +  + EXTERNALS +  +  + 
C      
      EXTERNAL SUBIN,SUBOUT
C
C     +  +  + OUTPUT FORMATS +  +  + 
C
2000  FORMAT (1X,A3,1X,110(1H*),/,1X,A3,1X,110(1H*),/,1X,A3,/,1X,A3,50X,
     1        'E R R O R',/,1X,A3,/,1X,A3,10X,'YOU ARE FORCING MORE ',
     2        'RUNOFF TO OCCUR THAN THE AMOUNT OF THROUGH FALL ',
     3        'AVAILABLE - THE',/,1X,A3,10X,'CANOPY COVERAGE AND THE ',
     4        'RUNOFF CURVE NUMBER INPUT VALUES ARE PROBABLY ',
     5        'INCONSISTENT',/,1X,A3,/,1X,A3,/,1X,A3,1X,110(1H*))
C
C     +  +  + END SPECIFICATIONS +  +  +
C
      MESAGE = 'HYDROL'
      CALL SUBIN(MESAGE)
C
      RUNOF = 0.0
      SMELT = 0.0
      SNOWFL= 0.0
      CINTB = CINT
      OSNOW = SNOW
      IF (SFAC .GT. 0.0) THEN
C
C       Compute snowmelt and accumulation
C
        IF (TEMP .LE. 0.0) THEN
          SNOWFL= PRECIP
          PRECIP= 0.0
          SNOW  = SNOW+ SNOWFL
        ELSE
          SMELT = AMIN1(SFAC*TEMP,SNOW)
          SNOW  = SNOW- SMELT
        ENDIF
      ENDIF
C
C     Compute interception
C     throughfall due to under-canopy sprinkler irrigation
C
      IF (THRUFL .GT. 0.0 .AND. PRECIP .LE. 0.0) THEN
        PRECIP = THRUFL
      ELSE
        THRUFL= AMAX1(0.0,PRECIP-(DIN-CINT))
        IF (PRECIP .GT. DIN-CINT) CINT = DIN
        IF (PRECIP .LE. DIN-CINT) CINT = CINT + PRECIP
      ENDIF
C
C     Compute runoff
C     hard-wiring the depth of runoff calculation to 10 cm or the
C     surface compartment thickness, whichever is greater
C      AAA   = AMAX1(THKNS(1),10.)
      AAA   = AMAX1(DELX(1),10.)
      MLIN  = 0
      DDLNJ = 0.0
10    CONTINUE
        MLIN  = MLIN + 1
        DDLNJ = DDLNJ + DELX(MLIN)
      IF (AAA .GT. DDLNJ) GO TO 10
      RODPTH= MLIN
      WLVL  = 0.0
      DO 30 I= 1, RODPTH
        WLVL= WLVL+ SW(I)
30    CONTINUE
      TWLVL = WLVL/ DDLNJ
      CURVN = CN(NCROP,ISCOND,2)+ (TWLVL-THETH)* (CN(NCROP,ISCOND,3)-
     1        CN(NCROP,ISCOND,2))/ THETH
      IF (CURVN .GT. CN(NCROP,ISCOND,3)) CURVN = CN(NCROP,ISCOND,3)
      IF (TWLVL .LT. THETH) CURVN= CN(NCROP,ISCOND,1)+ TWLVL*
     1        (CN(NCROP,ISCOND,2)- CN(NCROP,ISCOND,1))/ THETH
C     the constant .508 is derived from 0.2 * 2.54 cm/in
C     and 0.2 is from INABS = 0.2 * S, where S is (1000./CURVN-10.) below
      INABS = AMAX1(0.508* (1000./CURVN-10.),PRECIP-THRUFL)
      IF (PRECIP+SMELT .GT. 0.0) THEN
        IF (PRECIP+SMELT .GT. INABS) RUNOF= (PRECIP+ SMELT- INABS)**2/
     1                                      (PRECIP+ SMELT+ (4* INABS))
        IF (THRUFL+SMELT .LT. RUNOF) THEN
          RUNOF = THRUFL+SMELT
          WRITE(LPRZOT,2000) (MODID,I=1,10)
        ENDIF
      ENDIF
C
C     Compute infiltration for first soil compartment
C
      AINF(1) = AINF(1) + THRUFL+ SMELT- RUNOF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE EVPOTR
C
C     Computes daily potential evapotranspiration,
C     canopy evaporation, and actual evapotranspiration from each
C     soil layer
C     Modification date: 2/14/92 JAM
C
C     + + + PARAMETERS + + +   
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CMISC.INC'
C
C     + + + LOCAL VARIABLES + + +            
C
      REAL         PET,ES(40),PETP,ANUM,DENOM,AW,FRAC(NCMPTS),TSW,
     1             TWP,TFRAC,RNSUM,STDELX,EDEPTH
      INTEGER      ITEMP,I,NSUM
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + + 
C      
      INTRINSIC INT,FLOAT,AMAX1,AMIN1
C
C     + + + EXTERNALS + + +
C
      EXTERNAL SUBIN,SUBOUT
C
C     + + + DATA INITIALIZTIONS + + +           
C
      DATA ES/ 4.926, 5.294, 5.685, 6.101, 6.543, 7.013, 7.513, 8.015,
     1         8.609, 9.209, 9.844,10.518,11.231,11.987,12.788,13.634,
     2        14.530,15.477,16.477,17.535,18.650,19.827,21.068,22.377,
     3        23.756,25.209,26.739,28.349,30.043,31.824,33.695,35.663,
     4        37.729,39.898,42.175,44.453,47.067,49.692,52.442,55.324/
C
C     + + + END SPECIFICATIONS + + +               
C
      MESAGE = 'EVPOTR'
      CALL SUBIN(MESAGE)
C
C     Compute potential evapotranspiration
C
      ITEMP= INT(TEMP)
      IF (ITEMP.LT.1)  ITEMP= 1
      IF (ITEMP.GT.40) ITEMP= 40
      IF (IPEIND .EQ. 0) THEN
        PET= PEVP*PFAC
      ELSEIF (IPEIND .EQ. 1) THEN
        PET= 0.021*ES(ITEMP)*DT(MONTH)**2.0/(FLOAT(ITEMP)+273.)
      ELSEIF (IPEIND .EQ. 2) THEN
        IF (PEVP .GT. 0.0) THEN
          PET= PEVP*PFAC
        ELSEIF (TEMP .GT. -99.) THEN
          PET= 0.021*ES(ITEMP)*DT(MONTH)**2.0/(FLOAT(ITEMP)+273.)
        ELSE
          PET= 0.0
        ENDIF
      ENDIF
C
C     Subtract canopy evaporation from potential evapotranspiration
C
      PETP= AMAX1(0.0,PET-CINT)
      IF (PET .GT. CINT) THEN
        CEVAP= CINT
      ELSE
        CEVAP= PET
      ENDIF
      CINT= CINT-CEVAP
C
C     Compute evapotranspiration from each soil layer
C
      EDEPTH = 5.0
      STDELX = DELX(1)
      STTDET = 0.0
C
C     End of modification               
C
      TDET = 0.0
      DO 10 I= 1, NCOM2
        ET(I)= 0.0
10    CONTINUE
      ANUM = 0.0
      DENOM= 0.0
      DO 20 I= 1, NCOM1
        ANUM = ANUM + AMAX1(0.0,SW(I)- WP(I))
        DENOM= DENOM+ AMAX1(0.0,FC(I)- WP(I))
20    CONTINUE
      AW   = ANUM/DENOM
      IF (AW.LT.0.6) PETP= AW*PETP/0.6
      NSUM = 0
      TSW  = 0.0
      TWP  = 0.0
      DO 30 I= 1, NCOM1
        NSUM = NSUM + I
        TSW  = TSW + SW(I)
        TWP  = TWP + WP(I)
30    CONTINUE
      RNSUM= FLOAT(NSUM)
      IF (RNSUM .GT. 0.00 .AND. TSW .GT. TWP) THEN
        TFRAC= 0.0
        DO 40 I= 1, NCOM1
          FRAC(I)=FLOAT(NCOM1-I+1)*(SW(I)-WP(I))/(RNSUM*(TSW-TWP))
          TFRAC  =TFRAC + FRAC(I)
40      CONTINUE
        DO 50 I = 1, NCOM1
          ET(I) = AMIN1((SW(I)-WP(I)),PETP*FRAC(I)/TFRAC)
          ET(I) = AMAX1(ET(I),0.0)
          TDET  = TDET + ET(I)
C
C         This code is added by C.S.Raju to estimate the evaporation
C         through the top 5cm depth of soil.
C
          IF (STDELX .LE. EDEPTH)THEN
            STTDET = STTDET + ET(I)
            STDELX = STDELX + DELX(I)
          END IF
C
C
50      CONTINUE
C
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE HYDR1
C
C     Performs hydraulic calculations assuming a uniform soil
C     profile with unrestricted drainage 
C     (drainage occurs instantaneously)
C     Modification date: 2/18/92 JAM
C
C     +  +  + PARAMETERS +  +  +
C      
      INCLUDE 'PPARM.INC'
C
C     +  +  + COMMON BLOCKS +  +  +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CSPTIC.INC'
C
C     +  +  + LOCAL VARIABLES +  +  +               
C
      REAL         R(3,NCMPTS)
      INTEGER      I,K
      CHARACTER*80 MESAGE
C
C     +  +  + EXTERNALS +  +  + 
C      
      EXTERNAL SUBIN,SUBOUT
C
C     +  +  + END SPECIFICATIONS +  +  +              
C
      MESAGE = 'HYDR1'
      CALL SUBIN(MESAGE)
C
      DO 20 I=1,NCOM2
        THETO(I) = SW(I)/DELX(I)
        THETN(I) = (SW(I)+ AINF(I)+ LINF(I)- ET(I))/ DELX(I)
        AINF(I+1)= 0.0
        IF (THETN(I) .GT. THEFC(I)) THEN
          AINF(I+1)= (THETN(I)- THEFC(I))* DELX(I)
          THETN(I) = THEFC(I)
        ENDIF
        VEL(I)= AINF(I+1)/THETN(I)
        IF (MCFLAG.EQ.1) THEN
          DO 21 K = 1,3
            R(K,I) = 1 + (BD(I)*KD(K,I)/THETN(I))
C            VM(K,I) = VEL(I)/R(K,I)
21        CONTINUE
        ENDIF
        SW(I) = THETN(I)*DELX(I)
20    CONTINUE
C
      VLFLAG = 0
      DO 30 I = 1, NCOM2
        IF (VEL(I) .GT. 0.0) VLFLAG = 1
30    CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE HYDR2
C
C     Performs soil hydraulic calculations. Incoming water
C     fills successive soil compartments to field capacity during infil-
C     tration events until incoming water is depleted. Percolation continues
C     according to assigned drainage parameters (AD) until redistribution
C     is complete. Allows for the presence of restrictive layers in soil
C     profile.
C     Modfication date: 2/7/92 JAM
C
C     +  +  + PARAMETERS +  +  +  
C
      INCLUDE 'PPARM.INC'
C
C     +  +  + COMMON BLOCKS +  +  +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CSPTIC.INC'
C
C     +  +  + LOCAL VARIABLES +  +  +                 
C
      REAL         R(3,NCMPTS)
      REAL         TS(NCMPTS),AVSTOR,EXTRA,F1,F2,F3,T,LNCHK
      INTEGER      I,K,IB,NDEX,NDEXM1
      CHARACTER*80 MESAGE
      REAL*8       EXPCHK
C
C     +  +  + INTRINSICS +  +  + 
C      
      INTRINSIC AMAX1,AMIN1,DBLE,REAL
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL SUBIN,SUBOUT,LNCHK,EXPCHK
C
C     +  +  + END SPECIFICATIONS +  +  +                
C
      MESAGE = 'HYDR2'
      CALL SUBIN(MESAGE)
C
C     Subtract evapotranspired water from soil profile
C
      DO 10 I=1,NCOM2
        OUTFLO(I) = 0.0
        THETO(I)=SW(I)/DELX(I)
        TS(I)   =THETO(I)+ (LINF(I)-ET(I))*DELT/DELX(I)
10    CONTINUE
C
      IF (AINF(1) .GT. 0.0) THEN
C
C       Route water during infiltration event
C
        DO 20 I=1, NCOM2
          AVSTOR   = (THETAS(I)- TS(I))* DELX(I)/DELT
          THETN(I) = AMIN1(AINF(I),AVSTOR)* DELT/DELX(I)+ TS(I)
          AINF(I+1)= AINF(I)- (THETN(I)-TS(I))* DELX(I)/DELT
          IF (AINF(I+1).LE.0.0) AINF(I+1)= 0.0
20      CONTINUE
C
C       If there is an infiltration event on the current dat, then water
C       movement calculations are finished - jump to end of routine
C
      ELSE
C
C       If there is no infiltration event on current, then water
C       movement calculations are made here
C
        AINF(1) = 0.0
        DO 40 I=1,NCOM2
          TS(I)   = TS(I)+ AINF(I)/DELX(I)* DELT
          AINF(I+1) = 0.0
          IF (TS(I) .GT. THETAS(I)) THEN
            F1 = THETAS(I) - THEFC(I)
            F2 = TS(I) - THEFC(I)
            F3 = LNCHK(F1/F2) / (-AD(I)-ADL(I))
            T  = AMIN1(1.0,F3)
            TS(I) = THEFC(I) + F2 * 
     1                  REAL(EXPCHK(DBLE((-AD(I)-ADL(I))*T)))
            AINF(I+1) = AD(I) * F2 * DELX(I) / (ADL(I) + AD(I)) *
     1              (1.0 - REAL(EXPCHK(DBLE((-AD(I)-ADL(I))*T))))
            OUTFLO(I) = ADL(I) * F2 * DELX(I) / (ADL(I) + AD(I)) *
     1             (1.0 - REAL(EXPCHK(DBLE((-AD(I)-ADL(I))*T))))
            IF (T .LT. 1.0) THEN
              F1 = REAL(EXPCHK(DBLE(-AD(I)*(1.0-T))))
              F2 = TS(I) - THEFC(I)
              TS(I) = THEFC(I) + F2 * F1
              AINF(I+1) = AINF(I+1) + F2 * DELX(I) * (1.0 - F1)
            ENDIF
          ELSEIF (TS(I) .GT. THEFC(I)) THEN
            F1 = REAL(EXPCHK(DBLE(-AD(I) * DELT)))
            F2 = TS(I) - THEFC(I)
            TS(I) = THEFC(I) + F2 * F1
            AINF(I+1) = F2 * DELX(I) * (1.0 - F1)
          ENDIF
          THETN(I)= TS(I)
40      CONTINUE
C
50      CONTINUE
          NDEX= 0
          I   = NCOM2
60        CONTINUE
            IF (THETN(I).GT.THETAS(I)) NDEX= I
            I = I- 1
          IF (I.GE.1.AND.NDEX.EQ.0) GO TO 60
C
          IF (NDEX .GT. 1) THEN
C
C           Redistribute water into overlying compartments
            NDEXM1     = NDEX- 1
            EXTRA      = THETN(NDEX)- THETAS(NDEX)
            THETN(NDEX)= THETAS(NDEX)
            IB= NDEXM1
70          CONTINUE
              AVSTOR    = AMAX1(0.0,THETAS(IB)-THETN(IB))
              THETN(IB) = AMIN1(EXTRA,AVSTOR)+ THETN(IB)
              AINF(IB+1)= AINF(IB+1)- EXTRA * DELX(IB)/DELT
              EXTRA     = EXTRA- AMIN1(EXTRA,AVSTOR)
              IB= IB- 1
            IF (EXTRA .GT. 0.0 .AND. IB .GE. 1) GO TO 70
C
C           Look for oversaturation again
          ENDIF
        IF (NDEX .NE. 0) GO TO 50
      ENDIF
C
      DO 120 I=1,NCOM2
        VEL(I)= AINF(I+1)/THETN(I)
        IF (MCFLAG.EQ.1) THEN
          DO 22 K = 1,3
            R(K,I) = 1 + (BD(I)*KD(K,I)/THETN(I))
C            VM(K,I) = VEL(I)/R(K,I)
22        CONTINUE
        ENDIF
        SW(I) = THETN(I)*DELX(I)
120   CONTINUE
C
      VLFLAG = 0
      DO 130 I = 1, NCOM2
        IF (VEL(I) .GT. 0.0) VLFLAG = 1
130   CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE EROSN
C
C     + + + PURPOSE + + +
C     Determines loss of pesticide due to erosion by the MUSLE method
C     and an enrichment ratio.  It sets up a sink term (ELTERM) for
C     the pesticide balance in the surface layer.
C     Modification date: 2/14/92 JAM
C
C     + + + PARAMETERS + + +    
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMET.INC'
C
C     + + + LOCAL VARIABLES + + +           
C
      REAL         Q,RNUM,DENOM,QQP,SLKGHA,ENRICH,LNCHK
      CHARACTER*80 MESAGE
      REAL*8       EXPCHK
C
C     + + + INTRINSICS + + +
C
      INTRINSIC REAL,DBLE
C
C     + + + EXTERNAL + + +
C
      EXTERNAL SUBIN,LNCHK,EXPCHK,SUBOUT
C
C     + + + END SPECIFICATIONS + + +          
C
      MESAGE = 'EROSN'
      CALL SUBIN(MESAGE)
C
C     Determine runoff energy factor
C
      Q    = RUNOF* AFIELD* 100.
      RNUM = AFIELD* (PRECIP+SMELT)* RUNOF
      DENOM= TR* (PRECIP+SMELT-INABS)
      QQP  = RNUM/DENOM* Q* 0.0278
C
C     Compute enrichment ratio
C
      SEDL  = 11.8* QQP**0.56
      SEDL  = SEDL* USLEK* USLELS* USLEC(NCROP,ISCOND)* USLEP
      SLKGHA= SEDL* 1000./AFIELD
      ENRICH= 2.0- (0.2* LNCHK(SLKGHA))
      ENRICH= REAL(EXPCHK(DBLE(ENRICH)))
C
C     Compute loss term for pesticide balance
C
      ELTT= SEDL* ENRICH/(AFIELD*100.*DELX(1))
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE SLTEMP(
     I  LPRZOT,MODID)
C
C     + + + PURPOSE + + +
C     Calculates the soil temperature profile using
C     air temperatures, solar radiation, surface albedo, wind velocity,
C     evaporation, soil water content, and soil physical properties as
C     input data. This procedure is based on the methods of
C     Thebodeaux (1979);
C     Van Bavel and  Hillel (1975); de Vries (1963);
C     and Hanks et al. (1971).
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +             
C
      INTEGER     LPRZOT
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     LPRZOT....Fortran unit number for output file
C     MODID...character string for identification of output file LPRZOT
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CCROP.INC'
      INCLUDE 'CMISC.INC'
C
C     + + + LOCAL VARIABLES + + +                                      
C
      INTEGER      I,J,K,L,N,NUMDYS
      REAL         AIRDEN,Z0,D,ZCH,HTC,QC1,QEVF,QLW1,QLW2,QSWR,
     1             QGHF,TEMPK,STK,FX1,FX2,DELTA,EVAP,AAA,BBB,
     2             THKLY1,ABSOIL
      REAL         XVOL(5,NCMPTS),VOLCOR,LBTEMP,LNCHK,LOGCHK,
     1             DIFFCO(NCMPTS),ALAMDA(0:5),GEE(5,3),AKAY(5),
     2             THZERO(2),GFLD,SIGMA0,SIGMA1,SIGMA2,VAPLMD,AIRLMD,
     3             TA(NCMPTS),TB(NCMPTS),TC(NCMPTS),TF(NCMPTS)
C
C     + + + INTRINSICS + + +
C
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
C
      EXTERNAL LOGCHK,LNCHK,TRDIA1,SUBOUT
C
C     + + + DATA INITIALIZATIONS + + +
C
      DATA GEE(1,1),GEE(1,2),GEE(1,3)/2*0.125,0.750/
      DATA GEE(2,1),GEE(2,2),GEE(2,3)/2*0.125,0.750/
      DATA GEE(3,1),GEE(3,2),GEE(3,3)/2*0.5,0.0/
      DATA GEE(4,1),GEE(4,2),GEE(4,3)/3*0.333/
      DATA ALAMDA/122.7,1762.6,604.8,51.8,122.7,5.3/
      DATA VAPLMD/15.2/
C
C     + + + END SPECIFICATIONS + + +             
C
C     Interpolation of daily values from neighboring monthly values of 
C     soil surface albedo and bottom boundary temperature
C
      BBT(13) = BBT(1)
      NUMDYS = CNDMO(LEAP,MONTH+1) - CNDMO(LEAP,MONTH)
      LBTEMP = BBT(MONTH) + (BBT(MONTH+1)-BBT(MONTH))*DOM/NUMDYS
      ALBEDO(13) = ALBEDO(1)
      ABSOIL = ALBEDO(MONTH)+(ALBEDO(MONTH+1)-ALBEDO(MONTH))*DOM/NUMDYS
C
C     Estimation of surface albedo from canopy albedo and
C     soil surface albedo
C
      IF (SNOW .LT. 0.5)THEN
        ABSOIL = 0.23*COVER + ABSOIL*(1.-COVER)
      ELSE
        ABSOIL = 0.8
      END IF
C
C     Evaporation correction for crop canopy
C
      EVAP = STTDET*(1.-COVER)
C
      AIRLMD  = 5.3
C
C     If thermal conductivity and heat capacities are supplied,
C     bypass this sec.
C
      IF (IDFLAG .EQ. 0)GOTO 110
C
C     This portion of the routine estimates the Thermal Diffusivity of
C     soil compartment as the soil water content changes with time and
C     depth, using the procedure of de Vries (1963).
C
      DO 100 L =1,NCOM2
C
C     The vol fractions of sand,clay,and OM are adjusted so that their
C     total value equals to (1-porosity)
C
        VOLCOR=(1.0-THETAS(L))/((SAND(L)/2.65+CLAY(L)/2.65+
     1         OC(L)*1.724/1.3)*BD(L))
C
C       Conversion of Wt percents of soil constituents to vol fractions
C
        XVOL(1,L) = SAND(L)*BD(L)/2.65*VOLCOR
        XVOL(3,L) = OC(L)*1.724*BD(L)/1.30*VOLCOR
        XVOL(2,L) = 1.0-THETAS(L)-XVOL(1,L)-XVOL(3,L)
C
C       Defining water content and air in the soil pores
C
        XVOL(4,L) = THETO(L)
        IF(THETO(L) .LT. THEWP(L))XVOL(4,L)=THEWP(L)
        IF(THETO(L) .GT. THETAS(L))XVOL(4,L)=THETAS(L)
        XVOL(5,L) = THETAS(L) - XVOL(4,L)
C
C       Estimation of 'G' parameter when W.C is greater than F.C.
C
        IF (XVOL(4,L) .GT. THEFC(L))THEN
          GEE(5,1) = 0.333 - XVOL(5,L)/THETAS(L)*(0.333-0.035)
          ALAMDA(5) = AIRLMD + VAPLMD
C
C       Estimation of 'G' parameter when water content is less than F.C.
C
        ELSE
          GFLD = 0.333 - (THETAS(L)-THEFC(L))/THETAS(L)*(0.333-0.035)
          GEE(5,1) = 0.013 + XVOL(4,L)/THEFC(L)*(GFLD-0.013)
          ALAMDA(5) = AIRLMD + XVOL(4,L)/THEFC(L)*VAPLMD
        END IF
        GEE(5,2)=GEE(5,1)
        GEE(5,3)=1.-2*GEE(5,1)
        ALAMDA(0) = ALAMDA(4)
C
C       Estimation of thermal conductivity
C
        K = 0
 10     K = K+1
        SIGMA1 = 0.0
        SIGMA2 = 0.0
        DO 20 I = 1,5
          SIGMA0 = 0.0
C
C         Estimation of 'K' parameter
C
          DO 30 J = 1, 3
            SIGMA0 = SIGMA0+1./(1.+(ALAMDA(I)/ALAMDA(0)-1.)*GEE(I,J))
 30       CONTINUE
          AKAY(I) = SIGMA0/3.
          SIGMA1 = SIGMA1 + AKAY(I)*XVOL(I,L)*ALAMDA(I)
          SIGMA2 = SIGMA2 + AKAY(I)*XVOL(I,L)
 20     CONTINUE
C
C       Thermal Conductivity in cal/cm-day-C
C
        THZERO(K) =SIGMA1/SIGMA2
        IF (THETO(L) .LT. THEWP(L) .AND. K .LT. 2)THEN
          XVOL(4,L) = 0.0
          XVOL(5,L) = THETAS(L)
          ALAMDA(5) = AIRLMD
          ALAMDA(0) = ALAMDA(5)
          GOTO 10
        END IF
C
C       Interpolation of thermal cond. when W.C. is less than
C       critical point
C
        IF (THETO(L) .LT. THEWP(L))THEN
          THZERO(2) = 1.25*THZERO(2)
          THCOND(L)=THZERO(2)+THETO(L)/THEWP(L)*(THZERO(1)-THZERO(2))
        ELSE
          THCOND(L)=THZERO(1)
        END IF
C
C       Volumetric Heat Capacity of the soil layer, cal/cm3-C
C
        VHTCAP(L)=0.46*(XVOL(1,L)+XVOL(2,L)) + 0.6*XVOL(3,L) + THETO(L)
C
100   CONTINUE
110   CONTINUE
C
      DO 120 L = 1, NCOM2
C
C       Diffusion coefficient, cm2/day
C
        DIFFCO(L) = THCOND(L)/VHTCAP(L)
 120  CONTINUE
C
C     This portion of the subroutine estimates the Upper 
C     Boundary Temperature using Energy-Balance at the air/soil
C     interface. The fourth order equation in terms of soil surface
C     temperature is solved by Newton-Raphson method for upper
C     boundary temperature.
C
C     Air Density(from Thibodeaux), gm/cm3
C
      AIRDEN = (-0.0042*TEMP +1.292)*1.E-3
      ZCH = HEIGHT/100.
C
C     If the crop height is less than 5 cm, the roughness height
C     is set equal 0.006605m (at 5 cm crop height)
C
      IF(HEIGHT .LT. 5.0)THEN
        Z0 = 0.006605
        D = 0.0
      ELSE
C
C       Zero displacement height, m
C
        D = 10.**(0.9793*LOGCHK(ZCH)-0.1536)
C
C       Roughness Height, m
C
        Z0 = 10.**(0.997*LOGCHK(ZCH)-0.883)
      END IF
C
C     Heat Transfer coefficient at air-surface interface, cm/day
C     Wind speed in cm/sec
C
      HTC = 0.1681*WIND*86400/((LNCHK((ZWIND-D)/Z0))**2)
C
C     Sensible air Heat Flux term, cal/cm2-K-day
C
      QC1= AIRDEN*0.2402*HTC
C
C     Evaporation heat flux term, cal/cm2-day, EVAP in cm/day
C
      QEVF = 580.*EVAP*1.0
C
C     Atmospheric Longwave radiation component term, cal/cm2-K-day
C
      TEMPK = TEMP + 273.18
      QLW1 = EMMISS*0.936E-5*(TEMPK**2)*11.7E-8
C
C     Longwave radiation component emitted by the soil
C     term, cal/cm2-K-day
C
      QLW2 = EMMISS*11.7E-8
C
C     Short Wave radiation term, cal/cm2-day
C
      QSWR = (1.- ABSOIL)*SOLRAD
C
C     Calculation of Soil Heat flux term, cal/cm-C-day
C     Estimation of average temp gradient in the top 5cm of soil
C
      N = 1
      THKLY1 = DELX(1)
      AAA = 1.0/DELX(1)
      BBB = (SPT(1) + 273.18)/DELX(1)
35    IF(THKLY1 .LT. 5.0) THEN
        N = N +1
        THKLY1 = THKLY1 + DELX(N)
        BBB = BBB + (SPT(N) - SPT(N-1))/DELX(N)
      GOTO 35
      ENDIF
      AAA = AAA/N
      BBB = BBB/N
      QGHF= THCOND(1)
C
C     Initializing the soil surface temperature
C
      STK = TEMPK
      DELTA = 0.0
C
C     Newton-Raphson method to solve the 4th order equation for UBT
C
 40   STK = STK - DELTA
      FX1 = STK**4 + (QC1 + QGHF*AAA)/QLW2*STK - (QLW1*TEMPK**4.- QEVF
     1      + QC1*TEMPK + QSWR + QGHF*BBB)/QLW2
      FX2 = 4.*STK**3 + (QC1 + QGHF*AAA)/QLW2
      DELTA = FX1/FX2
C
C     Convergence criteria, 0.1 deg C
C
      IF(ABS(DELTA) .GT. 0.1)GOTO 40
      UBT = STK - 273.18
C
C     This portion of the routine simulates the Soil Temperature Profile
C     when Upper Boundary, Bottom Boundary, and Initial temperatures are
C     provided. Top Boundary Layer:
C
      TA(1)=0.0
      TC(1)=-(DIFFCO(1)+DIFFCO(2))*.5/(DELX(1)*(DELX(1)+DELX(2))*0.5)
      TB(1)=1.0+DIFFCO(1)/(DELX(1)**2)-TC(1)
      TF(1)=SPT(1)+DIFFCO(1)/(DELX(1)**2)*UBT
C
C       Non Boundary Layer:
C
        DO 50 I= 2, NCOM2-1
          TA(I) = -(DIFFCO(I-1)+DIFFCO(I))*0.5/(DELX(I)*(DELX(I)+
     1             DELX(I-1))*0.5)
          TC(I) = -(DIFFCO(I)+DIFFCO(I+1))*0.5/(DELX(I)*(DELX(I)+
     1             DELX(I+1))*0.5)
          TB(I) = 1.0 - (TA(I)+TC(I))
          TF(I) = SPT(I)
50      CONTINUE
C
C        Bottom Boundary Layer:
C
         TA(NCOM2) = -(DIFFCO(NCOM2-1)+DIFFCO(NCOM2))*0.5/
     1                (DELX(NCOM2)*(DELX(NCOM2)+DELX(NCOM2-1))*0.5)
         TB(NCOM2) = 1.0 -TA(NCOM2)+DIFFCO(NCOM2)/(DELX(NCOM2)**2)
         TC(NCOM2) = 0.0
         TF(NCOM2) =SPT(NCOM2)+DIFFCO(NCOM2)/(DELX(NCOM2)**2)*
     1              LBTEMP
C
          CALL TRDIA1(TA,TB,TC,SPT,TF,NCOM2,LPRZOT,MODID)
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE FARM (RODPTH,APPLY,CURVN)
C
C     + + + PURPOSE + + +
C     new subroutine added to insure pesticide application is
C     applied during adequate moisture conditions
C     Modification date: 2/14/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
C
      LOGICAL   APPLY
      INTEGER*4 RODPTH
      REAL      CURVN
C
C     + + + ARGUMENT DEFINITIONS + + +
C
C     APPLY   - logical to flag  pesticide application
C     RODPTH  - number of runoff compartments (top of PRZM)
C     CURVN   - generated curve number from subroutine HYDROL 
C
C
C     + + + PARAMETERS + + +
C
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
C
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CCROP.INC'
C
C     + + + LOCAL VARIABLES + + + 
C
      INTEGER*4 I
      REAL      JEFAVG,JEFAG1
      REAL      THETAL,THETAH,THETAV,THEAV1
C
C     + + + END SPECIFICATIONS + + + 
C      
      THETAL  = 0.0
      THETAH  = 0.0
      JEFAVG = 0.0
      DO 70 I=1,RODPTH 
        JEFAVG  =  JEFAVG + THETO(I)
        THETAL  =  THETAL + THEWP(I)
        THETAH  =  THETAH + THEFC(I)
70    CONTINUE
        JEFAG1  =  JEFAVG/RODPTH
        THETAV  =  (THETAL/RODPTH) 
        THEAV1  =  (THETAH/RODPTH) * .9
C 
      IF ((JEFAG1 .GE. THETAV .AND. JEFAG1 .LE. THEAV1).AND.
     1    (CURVN .LT. CN(NCROP,ISCOND,3) .AND. 
     2     CURVN .GT. CN(NCROP,ISCOND,1))) THEN
        APPLY = .TRUE.
      ELSE
        APPLY = .FALSE.
      ENDIF
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   SEPTIN
C
C     + + + PURPOSE + + +
C     Introduce septic effluent into PRZM soil column.
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CSPTIC.INC'
      INCLUDE 'CNITR.INC'
      INCLUDE 'CPEST.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,LNC
C
C     + + + END SPECIFICATIONS + + +
C
      LNC = NCOMEN(SEPHZN) - NCOMBE(SEPHZN) + 1
      DO 10 I = NCOMBE(SEPHZN),NCOMEN(SEPHZN)
C       introduce water volume from septic effluent into column
        LINF(I)  = INFLOW/LNC
        AMMINF(I)= AMMON/LNC * 1.0E5
        NITINF(I)= NITR/LNC * 1.0E5
        ORGINF(I)= ORGN/LNC * 1.0E5
        NIT(1,I) = NIT(1,I) + ORGINF(I) * (1.0 - ORGRFC)
        NIT(2,I) = NIT(2,I) + AMMINF(I)
        NIT(4,I) = NIT(4,I) + NITINF(I)
        NIT(7,I) = NIT(7,I) + ORGINF(I) * ORGRFC
 10   CONTINUE
C
      RETURN
      END
