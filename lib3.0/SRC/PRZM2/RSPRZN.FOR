C
C
C
      SUBROUTINE   NITR
     I                 (YEAR,MON,DAY,LPRZOT,IPRZM,MODID)
C
C     + + + PURPOSE + + +
C     Simulate nitrogen behavior in detail
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     YEAR,MON,DAY,LPRZOT,IPRZM
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YEAR   - year of simulation
C     MON    - month of simulation
C     DAY    - day of month in simulation
C     LPRZOT - Fortran unit number of PRZM2 main output file
C     IPRZM  - PRZM-2 zone number
C     MODID  - model id (pest,conc,water)
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CNITR.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CMET.INC'
      INCLUDE 'CCROP.INC'
C
C     + + + SAVES + + +
      INTEGER     NDAY(12,2)
      SAVE        NDAY
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I1,NDAYS,NXTMON,DATIM(5),NCP1
      REAL        R0,R24,TTHKNS,RETLN,SLMASS,SLMOIS,SLWILT,WS,
     $            DUM(4),TMPKDN
      CHARACTER*4 LAYID
C
C     + + + FUNCTIONS + + +
      REAL        DAYVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    DAYVAL,NITRXN,CRDYFR,YUPTGT,YUPINI,ZIPR,NITMOV,JDMODY
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDAY / 31,28,31,30,31,30,31,31,30,31,30,31,
     $            31,29,31,30,31,30,31,31,30,31,30,31 /
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      R0 = 0.0
      R24= 24.0
      NCP1 = NCOM2 + 1
      DATIM(1) = YEAR
      DATIM(2) = MON
      DATIM(3) = DAY
      DATIM(4) = 0
      DATIM(5) = 0
      NDAYS = NDAY(MON,LEAP)
      IF (MON.LT.12) THEN
        NXTMON= MON + 1
      ELSE
        NXTMON= 1
      END IF
C
      IF (NC1.EQ.0) THEN
C       1st time through, determine number of compartments in 1st horizon
        TTHKNS = 0.0
 10     CONTINUE
C         check next thickness
          NC1 = NC1 + 1
          TTHKNS = TTHKNS + DELX(NC1)
        IF (TTHKNS.LT.THKNS(1)) GO TO 10
C
        IF (NUPTFG.EQ.1) THEN
C         determine crop days and fractions
C         assign PRZM crop dates to HSPF crop dates
          IF (NDC.GT.3) THEN
C           HSPF code can only handle 3 crops
            NCRP = 3
          ELSE
            NCRP = NDC
          END IF
          IF (NCRP.GT.1) THEN
C           determine start/end of multiple cropping periods
            DO 15 I = 1,NCRP
              CALL JDMODY (IYREM(I),IEMER(I),
     O                     CRPDAT(1,I),CRPDAT(2,I))
              CALL JDMODY (IYRHAR(1),IHAR(1),
     O                     CRPDAT(3,I),CRPDAT(4,I))
 15         CONTINUE
          ELSE
C           only one cropping period
            CALL JDMODY (IYREM(1),IEMER(1),
     O                   CRPDAT(1,1),CRPDAT(2,1))
            CALL JDMODY (IYRHAR(1),IHAR(1),
     O                   CRPDAT(3,1),CRPDAT(4,1))
          END IF
          CALL CRDYFR (NCRP,CRPDAT,NDAY(1,LEAP),
     O                 CRPDAY,CRPFRC)
C         get initial values of previous month's final daily uptake target
          CALL YUPINI (R24,YEAR,MON,DAY,NDAY(1,LEAP),NCRP,CRPDAT,CRPDAY,
     I                 CRPFRC,NUPTGT,NUPTFM,NCOM2,NUPTM,
     O                 PNUTG)
        END IF
        IF (VNPRFG.EQ.0 .AND. ALPNFG.EQ.1) THEN
C         distribute above ground plant return rates through top horizon
          DO 20 I = 1,NC1
            KRETAN(I) = KRETAN(1)
 20       CONTINUE
        END IF
      END IF
C
      IF (VNPRFG .EQ. 1) THEN
C       plant return rate parameters are allowed to vary throughout
C       the year - interpolate for daily value
        DO 50 I= 1, NCOM2
          KRETBN(I)= DAYVAL(KRBNM(MON,I),KRBNM(NXTMON,I),DAY,NDAYS)
 50     CONTINUE
        BGNPRF= DAYVAL(BNPRFM(MON),BNPRFM(NXTMON),DAY,NDAYS)
        IF (ALPNFG .EQ. 1) THEN
C         above-ground compartments being simulated
          DO 60 I = 1,NC1
C           only into top horizon's compartments
            KRETAN(I)= DAYVAL(KRLNM(MON),KRLNM(NXTMON),DAY,NDAYS)
 60       CONTINUE
          AGKPRN= DAYVAL(KRANM(MON),KRANM(NXTMON),DAY,NDAYS)
          LINPRF= DAYVAL(LNPRFM(MON),LNPRFM(NXTMON),DAY,NDAYS)
        END IF
      END IF
      IF (VNUTFG .EQ. 1) THEN
C       plant uptake parameters are allowed to vary throughout the year
C       interpolate for the daily value
        IF (ALPNFG .EQ. 1) THEN
C         above-ground fractions
          DO 70 I= 1, NCOM2
            ANUTF(I)= DAYVAL(ANUFM(MON,I),ANUFM(NXTMON,I),DAY,NDAYS)
 70       CONTINUE
        END IF
        IF (NUPTFG .EQ. 0) THEN
C         first order plant uptake
          DO 80 I= 1, NCOM2
            KPLN(I)= DAYVAL(KPLNM(MON,I),KPLNM(NXTMON,I),DAY,NDAYS)
 80       CONTINUE
        END IF
      ELSE
C       plant uptake parmameters for nitrogen do not vary
      END IF
C
      IF (NUPTFG .EQ. 1) THEN
C       yield-based plant uptake parameters vary monthly, and
C       daily targets must be calculated from a trapezoidal
C       function
        CALL YUPTGT (R24,YEAR,MON,DAY,NDAYS,NCRP,CRPDAT,CRPDAY,
     I               CRPFRC,NUPTGT,NUPTFM,NCOM2,NUPTM,
     M               PNUTG,NDFC,
     O               NUPTG)
C
      END IF
C
      IF (ALPNFG .EQ. 1) THEN
C       calculate above-ground and litter compartment plant returns
C
C       recalculate reaction fluxes
        RETAGN= AGKPRN*AGPLTN
        RTLLN(NCP1) = 0.0
        RTRLN(NCP1) = 0.0
        DO 100 I = 1,NC1
C         for compartments in first horizon
          RTLLN(I)= KRETAN(I)*LITTRN*(1.0- LINPRF)
          RTRLN(I)= KRETAN(I)*LITTRN*LINPRF
          RTLLN(NCP1)= RTLLN(NCP1)+ RTLLN(I)
          RTRLN(NCP1)= RTRLN(NCP1)+ RTRLN(I)
 100    CONTINUE
        RETLN= RTLLN(NCP1)+ RTRLN(NCP1)
C
C       update above-ground storage
        IF (RETAGN .GT. AGPLTN) THEN
C         reduce plant return to make storage non-negative
          RETAGN= AGPLTN
          AGPLTN= 0.0
        ELSE
C         use calculated flux
          AGPLTN= AGPLTN- RETAGN
        END IF
C
C       update litter storage
        IF (RETLN .GT. LITTRN) THEN
C         reduce plant returns to make storage non-negative
          DO 110 I = 1,NC1
C           for compartments in first horizon
            RTLLN(I)= RTLLN(I)*LITTRN/RETLN
            RTRLN(I)= RTRLN(I)*LITTRN/RETLN
 110      CONTINUE
          LITTRN= 0.0
        ELSE
C         use calculated flux
          LITTRN= LITTRN- RETLN
        END IF
C
C       update target storages
        LITTRN= LITTRN+ RETAGN
        DO 120 I = 1,NC1
C         for compartments in first horizon
          NIT(1,I)= NIT(1,I)+ RTLLN(I)
          NIT(7,I)= NIT(7,I)+ RTRLN(I)
 120    CONTINUE
      ELSE
C       no above-ground compartments - zero fluxes
        RETAGN=   0.0
        CALL ZIPR (NCP1,R0,RTLLN)
        CALL ZIPR (NCP1,R0,RTRLN)
      END IF
C
      DO 140 I = 1,NC1
C       update storages for atmospheric deposition to first horizon
        NIT(1,I)= NIT(1,I)+ NIADDR(3)/NC1+ NIADWT(3)/NC1
        NIT(3,I)= NIT(3,I)+ NIADDR(1)/NC1+ NIADWT(1)/NC1
        NIT(4,I)= NIT(4,I)+ NIADDR(2)/NC1+ NIADWT(2)/NC1
 140  CONTINUE
C
C     transport nitrogen species
      CALL NITMOV (LPRZOT,MODID)
C
      DO 150 I = 1,NCOM2
C       perform nitrogen reactions per compartment
        WRITE(LAYID,2000) I
C       convert from cm to kg/ha
        SLMOIS = SW(I) * 1.0E5
C       convert from g/cm3 to kg/ha
        SLMASS = BD(I) * DELX(I) * 1.0E5
C       convert from cm to inches
        SLWILT = WP(I) / 2.54
        WS     = SW(I) / 2.54
C       create temporary value for denitrification rate based on threshold
        TMPKDN = NPM(5,I)
        IF (THETN(I).GT.DNTHRS(I)) THEN
C         determine rate for denitrification based on water content
          NPM(5,I) = (THETN(I)-DNTHRS(I))/
     $               (THETAS(I)-DNTHRS(I)) * NPM (5,I)
        ELSE
C         water content below threshold for denitrification
          NPM(5,I) = 0.0
        END IF
        CALL NITRXN(IPRZM,I1,DATIM,LPRZOT,ITMAXA,GNPM,I1,
     I              I1,FORAFG,SPT(I),SLMOIS,SLMASS,NPM(1,I),LAYID,
     I              KPLN(I),NUPTFG,FIXNFG,ALPNFG,AMVOFG,NUPTG(I),NMXRAT,
     I              SLWILT,ORNPM(1,I),KVOL(I),THVOL,TRFVOL,DUM,DUM,
     I              KRETBN(I),BGNPRF,ANUTF(I),WS,
     M              NDFC(I),NWCNT,NECNT,AGPLTN,NIT(1,I),NRXF(1,I))
C       restore input value for denitrification rate
        NPM(5,I) = TMPKDN
 150  CONTINUE
C
C     find total nitrogen outflows due to overland flow erosion
C      SOSEDN= SEDN(1)+ SEDN(2)+ SEDN(3)
C
C     find total outflows from the pervious land segment
C      PONO3 = TSNO3(1)+ TSNO3(5)+ SSNO3(3)
C      PONH4 = TSAMS(1)+ TSAMS(5)+ SSAMS(3)+ SEDN(2)
C      POORN = TSSLN(1)+ TSSLN(5)+ SSSLN(3)+ SEDN(1)+
C     $        TSSRN(1)+ TSSRN(5)+ SSSRN(3)+ SEDN(3)
C      PONITR= PONO3+ PONH4+ POORN
C
C     store reaction fluxes for printout
      OSAMS(NCP1) = 0.0
      OSNO3(NCP1) = 0.0
      ORNMN(NCP1) = 0.0
      AMIMB(NCP1) = 0.0
      AMUPB(NCP1) = 0.0
      NIIMB(NCP1) = 0.0
      NIUPB(NCP1) = 0.0
      AMNIT(NCP1) = 0.0
      DENIF(NCP1) = 0.0
      AMVOL(NCP1) = 0.0
      REFRON(NCP1)= 0.0
      RTLBN(NCP1) = 0.0
      RTRBN(NCP1) = 0.0
      AMUPA(NCP1) = 0.0
      NIUPA(NCP1) = 0.0
      NFIXFX(NCP1)= 0.0
      NDFC(NCP1)  = 0.0
      I = 8
      CALL ZIPR (I,R0,TNIT)
      DO 200 I = 1,NCOM2
C       store values for each layer
        ORNMN(I) = NRXF(3,I)
        AMIMB(I) = NRXF(4,I)
        AMUPB(I) = NRXF(5,I)
        NIIMB(I) = NRXF(6,I)
        NIUPB(I) = NRXF(7,I)
        AMNIT(I) = NRXF(8,I)
        DENIF(I) = NRXF(9,I)
        AMVOL(I) = NRXF(10,I)
        REFRON(I)= NRXF(11,I)
        RTLBN(I) = NRXF(12,I)
        RTRBN(I) = NRXF(13,I)
        AMUPA(I) = NRXF(14,I)
        NIUPA(I) = NRXF(15,I)
        NFIXFX(I)= NRXF(16,I)
        OSAMS(NCP1) = OSAMS(NCP1) + OSAMS(I)
        OSNO3(NCP1) = OSNO3(NCP1) + OSNO3(I)
        ORNMN(NCP1) = ORNMN(NCP1) + ORNMN(I)
        AMIMB(NCP1) = AMIMB(NCP1) + AMIMB(I)
        AMUPB(NCP1) = AMUPB(NCP1) + AMUPB(I)
        NIIMB(NCP1) = NIIMB(NCP1) + NIIMB(I)
        NIUPB(NCP1) = NIUPB(NCP1) + NIUPB(I)
        AMNIT(NCP1) = AMNIT(NCP1) + AMNIT(I)
        DENIF(NCP1) = DENIF(NCP1) + DENIF(I)
        OSSLN(NCP1) = OSSLN(NCP1) + OSSLN(I)
        OSSRN(NCP1) = OSSRN(NCP1) + OSSRN(I)
        AMVOL(NCP1) = AMVOL(NCP1) + AMVOL(I)
        REFRON(NCP1)= REFRON(NCP1) + REFRON(I)
        RTLBN(NCP1) = RTLBN(NCP1) + RTLBN(I)
        RTRBN(NCP1) = RTRBN(NCP1) + RTRBN(I)
        AMUPA(NCP1) = AMUPA(NCP1) + AMUPA(I)
        NIUPA(NCP1) = NIUPA(NCP1) + NIUPA(I)
        NFIXFX(NCP1)= NFIXFX(NCP1) + NFIXFX(I)
        DO 180 J = 1,8
C         find the totals of nitrogen in soil storage
          TNIT(J)= TNIT(J) + NIT(J,I)
 180    CONTINUE
        IF (NUPTFG .EQ. 1) THEN
C         find the total N uptake deficit
          NDFC(NCP1)= NDFC(NCP1) + NDFC(I)
        END IF
 200  CONTINUE
C
C     find the totals of nitrogen in soil storage
CPRH      TN(1)= SN(1)+ UN(1)+ LN(1)+ AN(1)
CPRH      TN(2)= SN(2)+ UN(2)+ LN(2)+ AN(2)
CPRH      TN(3)= SN(3)+ UN(3)+ IN(1)+ LN(3)+ AN(3)
CPRH      TN(4)= SN(4)+ UN(4)+ IN(2)+ LN(4)+ AN(4)
CPRH      TN(6)= SN(6)+ UN(6)+ IN(3)+ LN(6)+ AN(6)
CPRH      TN(7)= SN(7)+ UN(7)+ LN(7)+ AN(7)
CPRH      TN(8)= SN(8)+ UN(8)+ IN(4)+ LN(8)+ AN(8)
C
C     find the total nitrogen in plant storage
CPRH      TN(5)= SN(5)+ UN(5)+ LN(5)+ AN(5)
CPRH   *** don't need to include AG and Litter N in total for output ***
CPRH      IF (ALPNFG .EQ. 1) THEN
CPRHC       above-ground compartments being simulated
CPRH        TNIT(5)= TNIT(5)+ AGPLTN+ LITTRN
CPRH      END IF
C
C     total nitrogen in storage
      TOTNIT= TNIT(1)+ TNIT(2)+ TNIT(3)+ TNIT(4)+
     $        TNIT(5)+ TNIT(6)+ TNIT(7)+ TNIT(8)+ AGPLTN+ LITTRN
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITRXN
     I                   (LSNO,MSGFL,DATIM,MESSU,ITMAXA,GNPM,BRXNFG,
     I                    CRXNFG,FORAFG,TMP,MOISTM,SOILM,NPM,LAYID,
     I                    KPLN,NUPTFG,FIXNFG,ALPNFG,AMVOFG,NUPTG,NMXRAT,
     I                    WILTPT,ORNPM,KVOL,THVOL,TRFVOL,KSAT,CSAT,
     I                    KRET,KRETF,ANUTF,SMST,
     M                    NDEFC,NWCNT,NECNT,AGPLTN,NIT,NITRXF)
C
C     + + + PURPOSE + + +
C     Perform reactions on nitrogen forms
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LSNO,MSGFL,DATIM(5),MESSU,ITMAXA,BRXNFG,CRXNFG,FORAFG,
     $            NUPTFG,FIXNFG,ALPNFG,AMVOFG,NWCNT(6),NECNT(1)
      REAL        GNPM(11),TMP,MOISTM,SOILM,NPM(11),KPLN,NUPTG,NMXRAT,
     $            WILTPT,ORNPM(4),KVOL,THVOL,TRFVOL,KSAT(4),CSAT(4),
     $            KRET,KRETF,ANUTF,SMST,NDEFC,AGPLTN,NIT(8),NITRXF(16)
      CHARACTER*4 LAYID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LSNO   - land segment number of PERLND
C     MSGFL  - fortran unit number of HSPF message file
C     DATIM  - date and time of day
C     MESSU  - ftn unit no. to be used for printout of messages
C     ITMAXA - maximum number of iterations allowed for convergence of
C              Freundlich method for ammonia adsorption/desorption
C     GNPM   - general nitrogen parameters
C     BRXNFG - flag indicating whether biological reaction fluxes are
C              recalculated this interval
C     CRXNFG - flag indicating whether chemical reaction fluxes are
C              recalculated this interval (adsorption/desorption)
C     FORAFG - flag indicating which method is used to calculate adsorption/
C              desorption - 1: first-order rates; 2: single-valued Freundlich
C     TMP    - soil temperature in this layer
C     MOISTM - soil moisture in this layer (lb or kg)
C     SOILM  - soil mass of this layer (lb or kg)
C     NPM    - nitrogen parameters for this layer
C     LAYID  - character identifier for this layer
C     KPLN   - first-order plant-uptake parameter for this layer
C     NUPTFG - flag indicating which method is used to calculate plant uptake
C              0: first-order rate; 1: yield-based algorithm;
C              2 or -2: half-saturation (Michaelis-Menton) kinetics
C     FIXNFG - flag to turn on/off nitrogen fixation (NUPTFG= 1 only)
C     ALPNFG - flag to turn on/off above-ground and litter n compartments
C     AMVOFG - flag to turn on/off ammonia volatilization
C     NUPTG  - plant uptake target for this layer (NUPTFG= 1)
C     NMXRAT - ratio of maximum plant uptake to target uptake (NUPTFG= 1)
C     WILTPT - wilting point: soil moisture cutoff for plant uptake for this
C              layer (NUPTFG= 1)
C     ORNPM  - organic n parameters for this layer
C     KVOL   - first-order ammonia volatilization rate for this layer
C     THVOL  - temperature correction coefficient for ammonia volatilization
C     TRFVOL - reference temperature for ammonia volatilization
C     KSAT   - max rate parameters for half-saturation kinetics for this layer
C     CSAT   - half-saturation constants for this layer
C     KRET   - plant N return rate for this layer
C     KRETF  - refractory fraction of plant N return
C     ANUTF  - above-ground plant uptake fraction
C     SMST   - soil moisture storage in inches
C     NDEFC  - cumulative plant uptake deficit for this layer (NUPTFG= 1)
C     NWCNT  - warning counts
C     NECNT  - error count
C     AGPLTN - above-ground plant n storage
C     NIT    - storages of each species of nitrogen in this layer
C     NITRXF - current reaction fluxes for this layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I4,SCLU,SGRP
      REAL        NO3UTF,NH4UTF,THPLN,THKDSA,THKADA,THKIMN,THKAM,THKDNI,
     $            THKNI,THKIMA,CMAXAM,KDSAM,KADAM,KIMNI,KAM,KDNI,KNI,
     $            KIMAM,XFIXAM,XMAXAM,KF1AM,N1IAM,KLON,KRON,KONLR,
     $            THNLR,KSUNI,KSUAM,KSINI,KSIAM,CSUNI,CSUAM,CSINI,CSIAM,
     $            PLON,AMAD,AMSU,NO3,PLTN,SLON,PRON,SRON,ADSAM,DESAM,
     $            AMMIF,IMMAM,UTAM,IMMNI,UTNI,NITRF,DENI,AMVO,RFON,
     $            PRETL,PRETR,UTAMA,UTNIA,NFIX,UTNTOT,TNH4,AMCY,DIF35,
     $            KDNIK,KIMAMK,KIMNIK,KAMK,KPLNK,KNIK,KONLRK,KSUNIK,
     $            KSUAMK,KSINIK,KSIAMK,KVOLK,PRET,MAXUPT,FRAC,TFRAC,
     $            TPLON,TAMAD,TNO3,TAMSU,TPLTN,UTAMAB,AMSULO,AMDEFC,
     $            UTAMFR,TLON,TRON,UTNACT,AGUTF,NICONC,AMCONC
      CHARACTER*4 NH4ID(5),CHSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL    FIRORD,SV,OMSTR,OMSTC,OMSTD,OMSTI,OMSG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA       NH4ID/'AMMO','NIUM','    ','    ','    '/
C
C     + + + END SPECIFICATIONS + + +
C
      I4    = 4
      SCLU  = 310
C     assign values to local variables where necessary
C     general parameters
      NO3UTF= GNPM(1)
      NH4UTF= GNPM(2)
      THPLN = GNPM(3)
      THKDSA= GNPM(4)
      THKADA= GNPM(5)
      THKIMN= GNPM(6)
      THKAM = GNPM(7)
      THKDNI= GNPM(8)
      THKNI = GNPM(9)
      THKIMA= GNPM(10)
      CMAXAM= GNPM(11)
C
C     layer specific parameters
      KDSAM = NPM(1)
      KADAM = NPM(2)
      KIMNI = NPM(3)
      KAM   = NPM(4)
      KDNI  = NPM(5)
      KNI   = NPM(6)
      KIMAM = NPM(7)
      XFIXAM= NPM(8)
      XMAXAM= NPM(9)
      KF1AM = NPM(10)
      N1IAM = NPM(11)
C
C     organic nitrogen parameters
      KLON= ORNPM(1)
      KRON= ORNPM(2)
      KONLR= ORNPM(3)
      THNLR= ORNPM(4)
C
C     saturation kinetics parameters
      KSUNI= KSAT(1)
      KSUAM= KSAT(2)
      KSINI= KSAT(3)
      KSIAM= KSAT(4)
      CSUNI= CSAT(1)
      CSUAM= CSAT(2)
      CSINI= CSAT(3)
      CSIAM= CSAT(4)
C
C     layer specific storages of nitrogen
      PLON= NIT(1)
      AMAD= NIT(2)
      AMSU= NIT(3)
      NO3 = NIT(4)
      PLTN= NIT(5)
      SLON= NIT(6)
      PRON= NIT(7)
      SRON= NIT(8)
C
C     layer specific reaction fluxes
      ADSAM= NITRXF(1)
      DESAM= NITRXF(2)
      AMMIF= NITRXF(3)
      IMMAM= NITRXF(4)
      UTAM = NITRXF(5)
      IMMNI= NITRXF(6)
      UTNI = NITRXF(7)
      NITRF= NITRXF(8)
      DENI = NITRXF(9)
      AMVO = NITRXF(10)
      RFON = NITRXF(11)
      PRETL= NITRXF(12)
      PRETR= NITRXF(13)
      UTAMA= NITRXF(14)
      UTNIA= NITRXF(15)
      NFIX = NITRXF(16)
C
C     set other needed local variables
      UTNTOT= UTNI+ UTAM
      IF (ALPNFG .EQ. 1) THEN
C       above-ground and litter compartments being simulated
        AGUTF= ANUTF
      ELSE
C       no above-ground plant uptake
        AGUTF= 0.0
      END IF
C
      IF (CRXNFG .EQ. 1) THEN
C       chemical (adsorption/desorption) fluxes are
C       recalculated this interval
        IF (FORAFG .NE. 1) THEN
C         ammonium is adsorbed/desorbed by first order kinetics
C         with this method the adsorption/desorption fluxes are
C         calculated every cnumn intervals in units of the basic
C         simulation interval (mass/area-ivl); the updating of the
C         storages is done every interval
C
          CALL FIRORD (TMP,MOISTM,KDSAM,KADAM,THKDSA,THKADA,
     I                 AMSU,AMAD,
     O                 ADSAM,DESAM)
        ELSE
C         ammonium is adsorbed/desorbed using the single value
C         freundlich method
C         with this method the adsorption/desorption is instantaneous
C         and is done every cnumn intervals.  because this method is
C         instantaneous, no updating of the storages is done during
C         intermediate intervals
C
C         total ammonium
          TNH4= AMAD+ AMSU
C
          CALL SV (MOISTM,SOILM,TNH4,XFIXAM,CMAXAM,XMAXAM,KF1AM,
     I             N1IAM,LSNO,MESSU,MSGFL,DATIM,
     I             ITMAXA,NH4ID,LAYID,
     M             AMSU,NECNT(1),
     O             AMCY,AMAD)
C
C         zero fluxes since this method is based on
C         instantaneous equilibrium
          ADSAM= 0.0
          DESAM= 0.0
C
C         any crystalline ammonium formed is considered adsorbed
          AMAD= AMAD+ AMCY
C
        END IF
C
      END IF
C
      IF (BRXNFG .EQ. 1) THEN
C       biochemical transformation fluxes are recalculated
C       this interval
C
        IF ( (TMP .GT. 4.0) .AND. (MOISTM .GT. 100.0) ) THEN
C         there is sufficient soil layer temperature (in deg c)
C         and moisture for biochemical transformations to occur
C
          IF (TMP .LT. 35.0) THEN
C           soil layer temperature in deg c is less than
C           optimum, modify inputted first order reaction rates
C           decrease the inputted first order reaction rates
C           by the modified arrhenius equation
            DIF35 = TMP- 35.0
            KDNIK = KDNI*THKDNI**DIF35
            KIMAMK= KIMAM*THKIMA**DIF35
            KIMNIK= KIMNI*THKIMN**DIF35
            KAMK  = KAM*THKAM**DIF35
            IF (NUPTFG .EQ. 0) THEN
C             first order plant uptake
              KPLNK = KPLN*THPLN**DIF35
            END IF
            KNIK  = KNI*THKNI**DIF35
            KONLRK= KONLR*THNLR**DIF35
            IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C             max rates for saturation kinetics
              KSUNIK= KSUNI*THPLN**DIF35
              KSUAMK= KSUAM*THPLN**DIF35
              KSINIK= KSINI*THKIMN**DIF35
              KSIAMK= KSIAM*THKIMA**DIF35
            ELSE
C             zero rates
              KSUNIK= 0.0
              KSUAMK= 0.0
              KSINIK= 0.0
              KSIAMK= 0.0
            END IF
          ELSE
C           soil layer temperature in deg c is at optimum,
C           use inputted first order reaction rates
            KDNIK = KDNI
            KIMAMK= KIMAM
            KIMNIK= KIMNI
            KAMK  = KAM
            IF (NUPTFG .EQ. 0) THEN
C             first order plant uptake
              KPLNK = KPLN
            END IF
            KNIK  = KNI
            KONLRK= KONLR
            IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C             max rates for saturation kinetics
              KSUNIK= KSUNI
              KSUAMK= KSUAM
              KSINIK= KSINI
              KSIAMK= KSIAM
            ELSE
C             zero rates
              KSUNIK= 0.0
              KSUAMK= 0.0
              KSINIK= 0.0
              KSIAMK= 0.0
            END IF
          END IF
C
          IF (AMVOFG .EQ. 1) THEN
C           temperature correction for ammonia volatilization
            KVOLK = KVOL*THVOL**(TMP-TRFVOL)
          ELSE
C           zero rate
            KVOLK= 0.0
          END IF
C
C         recompute transformation fluxes.  this is done every
C         bnumn intervals in units of the basic simulation
C         interval (mass/area-ivl); however, the updating
C         of the storages is done every interval;  the exception
C         is nitrogen fixation, which is recomputed every interval
C
C         nitrification
          NITRF= AMSU*KNIK
C
C         denitrification
          DENI = NO3*KDNIK
C
C         organic nitrogen ammonification
          AMMIF= PLON*KAMK
C
C         ammonia volatilization
          AMVO= AMSU*KVOLK
C
C         conversion of particulate labile organic n to refractory
          RFON= PLON*KONLRK
C
C         plant nitrogen return - not temperature-adjusted
          PRET= PLTN*KRET
          PRETR= PRET* KRETF
          PRETL= PRET- PRETR
C
          IF ( (NUPTFG .EQ. 0) .OR. (NUPTFG .EQ. 1) ) THEN
C           immobilization is first-order
C
C           nitrate immobilization
            IMMNI= NO3*KIMNIK
C
C           ammonium immobilization
            IMMAM= AMSU*KIMAMK
C
          ELSE IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C           immobilization is half-saturation kinetics
C
C           compute current concentrations
            NICONC= NO3/MOISTM*1.0E6
            AMCONC= AMSU/MOISTM*1.0E6
C
C           nitrate immobilization
            IF (NICONC .LE. 0.0) THEN
C             no nitrate present
              IMMNI= 0.0
            ELSE
C             nitrate present
              IMMNI= KSINIK*NICONC/(CSINI+ NICONC)
              IMMNI= IMMNI*MOISTM/1.0E6
            END IF
C
C           ammonium immobilization
            IF (AMCONC .LE. 0.0) THEN
C             no solution ammonia present
              IMMAM= 0.0
            ELSE
C             solution ammonia present
              IMMAM= KSIAMK*AMCONC/(CSIAM+ AMCONC)
              IMMAM= IMMAM*MOISTM/1.0E6
            END IF
C
          END IF
C
          IF (NUPTFG .EQ. 0) THEN
C           plant uptake is first-order
C
C           plant uptake of nitrate
            UTNI= NO3*KPLNK*NO3UTF
C
C           plant uptake of ammonium
            UTAM= AMSU*KPLNK*NH4UTF
C
          ELSE IF (NUPTFG .EQ. 1) THEN
C           plant uptake is yield-based
C
            IF (SMST .GE. WILTPT) THEN
C             soil moisture is at or above wilting point
C
C             try to take up optimum target plus seasonal deficit
              UTNTOT= NUPTG+ NDEFC
C
C             make sure maximum rate is not exceeded
              MAXUPT= NUPTG*NMXRAT
              IF (UTNTOT .GT. MAXUPT) THEN
C               reduce to maximum rate
                UTNTOT= MAXUPT
              END IF
            ELSE
C             soil moisture is below wilting point
              UTNTOT= 0.0
            END IF
C
C           divide total target between nitrate and ammonia
            UTNI= UTNTOT*NO3UTF
            UTAM= UTNTOT*NH4UTF
C           
          ELSE IF ( (NUPTFG .EQ. 2) .OR. (NUPTFG .EQ. -2) ) THEN
C           plant uptake uses saturation kinetics
C
C           nitrate
            IF (NICONC .LE. 0.0) THEN
C             no nitrate present
              UTNI= 0.0
            ELSE
C             nitrate present
              UTNI= KSUNIK*NICONC/(CSUNI+ NICONC)
              UTNI= UTNI*MOISTM/1.0E6
            END IF
C
C           ammonia
            IF (AMCONC .LE. 0.0) THEN
C             no solution ammonia present
              UTAM= 0.0
            ELSE
C             solution ammonia present
              UTAM= KSUAMK*AMCONC/(CSUAM+ AMCONC)
              UTAM= UTAM*MOISTM/1.0E6
            END IF
C
          END IF
C
          IF (ALPNFG .EQ. 1) THEN
C           divide plant uptake into above- and below-ground
            UTAMA= UTAM* AGUTF
            UTAM= UTAM- UTAMA
            UTNIA= UTNI* AGUTF
            UTNI= UTNI- UTNIA
          END IF
        ELSE
C         there are no biochemical transformations occurring due
C         to either low temperatures or low moisture
C         zero fluxes
          DENI = 0.0
          IMMNI= 0.0
          AMMIF= 0.0
          IMMAM= 0.0
          UTNI = 0.0
          NITRF= 0.0
          UTAM = 0.0
          AMVO = 0.0
          RFON = 0.0
          PRETL= 0.0
          PRETR= 0.0
          UTAMA= 0.0
          UTNIA= 0.0
          UTNTOT= 0.0
C
        END IF
      END IF
C
C     update all storages to account for fluxes - done every
C     interval; check and fix any storages that may be negative
C
C     initalize the fraction used to change any negative storages
C     that may have been computed; frac also acts as a flag
C     indicating negative storages were projected (when < 1.0)
      FRAC= 1.0
C
C     calculate temporary particulate labile organic nitrogen in storage
      TPLON= PLON- AMMIF+ IMMAM+ IMMNI+ PRETL- RFON
C
      IF (TPLON .LT. 0.0) THEN
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        FRAC= PLON/(PLON-TPLON)
C
C       write a warning that the organic nitrogen value will
C       be fixed up so that it does not go negative
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (PLON)
        CALL OMSTR (TPLON)
        CALL OMSTR (AMMIF)
        CALL OMSTR (IMMAM)
        CALL OMSTR (IMMNI)
        CALL OMSTR (PRETL)
        CALL OMSTR (RFON)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 2
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             NWCNT(3))
C
      END IF
C
C     calculate temporary adsorbed ammonium in storage
      TAMAD= AMAD- DESAM+ ADSAM
C
      IF (TAMAD .LT. 0.0) THEN
C
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        TFRAC= AMAD/(AMAD-TAMAD)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
C
C       write a warning that the adsorbed value of ammonium will
C       be fixed up so that it does not go negative
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (AMAD)
        CALL OMSTR (TAMAD)
        CALL OMSTR (ADSAM)
        CALL OMSTR (DESAM)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 3
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             NWCNT(4))
C
      END IF
C
C     calculate temporary nitrate in storage
      TNO3= NO3+ NITRF- (IMMNI+DENI+UTNI+UTNIA)
C
      IF (TNO3 .LT. 0.0) THEN
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        TFRAC= NO3/(NO3-TNO3)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
C
C       write a warning that the value of nitrate in storage will
C       be fixed up so that it does not go negative
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (NO3)
        CALL OMSTR (TNO3)
        CALL OMSTR (NITRF)
        CALL OMSTR (IMMNI)
        CALL OMSTR (DENI)
        CALL OMSTR (UTNI)
        CALL OMSTR (UTNIA)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             NWCNT(2))
C
      END IF
C
C     calculate temporary solution ammonium in storage
      TAMSU= AMSU+ DESAM+ AMMIF- (ADSAM+IMMAM+UTAM+UTAMA+NITRF+AMVO)
C
      IF (NUPTFG .EQ. 1) THEN
C       set a threshold value for solution ammonium in storage
        AMSULO= 0.001
        UTAMAB= UTAM+ UTAMA
        IF ( (TAMSU .LE. AMSULO) .AND. (UTAMAB .GT. 0.0)) THEN
C         determine reduction
          AMDEFC= AMSULO- TAMSU
          IF ( AMDEFC .GE. UTAMAB) THEN
C           shut off uptake completely
            TAMSU= TAMSU+ UTAMAB
            UTAM= 0.0
            UTAMA= 0.0
          ELSE
C           prorate reduction
            UTAMFR= AMDEFC/UTAMAB
            TAMSU= TAMSU+ (UTAMFR*UTAMAB)
            UTAM= UTAM*(1.0- UTAMFR)
            UTAMA= UTAMA*(1.0- UTAMFR)
          END IF
        END IF
      END IF
C
      IF (TAMSU .LT. 0.0) THEN
C       negative storage value is unrealistic
C       write a warning that the solution value of ammonium will
C       be fixed up so that it does not go negative
        TFRAC= AMSU/(AMSU-TAMSU)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
C
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CALL OMSTR (FRAC)
        CALL OMSTR (AMSU)
        CALL OMSTR (TAMSU)
        CALL OMSTR (ADSAM)
        CALL OMSTR (DESAM)
        CALL OMSTR (AMMIF)
        CALL OMSTR (IMMAM)
        CALL OMSTR (NITRF)
        CALL OMSTR (UTAM)
        CALL OMSTR (UTAMA)
        CALL OMSTR (AMVO)
        CHSTR = LAYID
        CALL OMSTC (I4,CHSTR1)
        SGRP = 4
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             NWCNT(5))
C
      END IF
C
C     calculate temporary below-ground plant n in storage
      TPLTN= PLTN+ UTNI+ UTAM- PRETL- PRETR
C
      IF (TPLTN .LT. 0.0) THEN
C       negative storage value is unrealistic
C       calculate that fraction of the flux that is
C       needed to make the storage zero
        TFRAC= PLTN/(PLTN-TPLTN)
C
C       keep the smaller fraction; the smaller fraction
C       of the fluxes will make all the storages either zero
C       or positive
        IF (TFRAC.LT.FRAC)  FRAC= TFRAC
      END IF
C
      IF (FRAC .GE. 1.0) THEN
C       no storages have gone negative; use the temporary values
        PLON= TPLON
        AMAD= TAMAD
        NO3 = TNO3
        AMSU= TAMSU
        PLTN= TPLTN
      ELSE
C       at least one of the storages has gone negative
C       use frac to adjust the fluxes to make all the storages
C       zero or positive
        FRAC = FRAC*0.9999
        ADSAM= ADSAM*FRAC
        DESAM= DESAM*FRAC
        AMMIF= AMMIF*FRAC
        IMMAM= IMMAM*FRAC
        UTAM = UTAM*FRAC
        IMMNI= IMMNI*FRAC
        UTNI = UTNI*FRAC
        NITRF= NITRF*FRAC
        DENI = DENI*FRAC
        AMVO = AMVO*FRAC
        RFON = RFON*FRAC
        PRETL= PRETL*FRAC
        PRETR= PRETR*FRAC
        UTAMA= UTAMA*FRAC
        UTNIA= UTNIA*FRAC
C
C       recalculate the storages
        PLON = PLON- AMMIF+ IMMAM+ IMMNI+ PRETL- RFON
        AMAD = AMAD- DESAM+ ADSAM
        NO3  = NO3+ NITRF- (IMMNI+DENI+UTNI+UTNIA)
        AMSU = AMSU+ DESAM+ AMMIF- (ADSAM+IMMAM+UTAM+UTAMA+NITRF+AMVO)
        PLTN= PLTN+ UTAM+ UTNI- PRETL- PRETR
C
      END IF
C
C     add converted organic n and plant return to refractory storage
      PRON= PRON+ RFON+ PRETR
C
C     partition particulate and solution fractions of organic n storages
C
C     labile
      TLON= PLON+ SLON
      IF (KLON .GT. 1.0E15) THEN
C       infinite paritition coefficient for labile
        SLON= 0.0
      ELSE
C       partition labile
        SLON= TLON/(KLON+ 1.0)
      END IF
      PLON= TLON- SLON
C
C     refractory
      TRON= PRON+ SRON
      IF (KRON .GT. 1.0E15) THEN
C       infinite paritition coefficient for labile
        SRON= 0.0
      ELSE
C       partition labile
        SRON= TRON/(KRON+ 1.0)
      END IF
      PRON= TRON- SRON
C
      IF (ALPNFG .EQ. 1) THEN
C       above-ground compartment simulated
        AGPLTN= AGPLTN+ UTAMA+ UTNIA
      END IF
C
C     calculate nitrogen fixation every interval
      NFIX= 0.0
      IF ( (NUPTFG .EQ. 1) .AND. (FIXNFG .EQ. 1) ) THEN
C       see if there is still unsatisfied demand for uptake
        IF ( (TMP .GT. 4.0) .AND. (MOISTM .GT. 100.0) ) THEN
C         there is sufficient soil layer temperature (in deg c)
C         and moisture for biochemical transformations to occur
          IF (SMST .GE. WILTPT) THEN
C           soil moisture is at or above wilting point
            UTNACT= UTNI+ UTAM+ UTNIA+ UTAMA
            IF (UTNTOT .GT. UTNACT) THEN
C             still unsatisfied plant uptake demand
              NFIX= UTNTOT- UTNACT
              PLTN= PLTN+ NFIX
            END IF
          END IF
        END IF
      END IF
C
      IF (NUPTFG .EQ. 1) THEN
C       accumulate any deficit
        NDEFC= NDEFC+ NUPTG- UTAM- UTNI- UTAMA- UTNIA- NFIX
        IF (NDEFC .LT. 1.0E-06) THEN
C         deficit has been erased
          NDEFC= 0.0
        END IF
      END IF
C
C     reassign storages to "permanent" array
      NIT(1)= PLON
      NIT(2)= AMAD
      NIT(3)= AMSU
      NIT(4)= NO3
      NIT(5)= PLTN
      NIT(6)= SLON
      NIT(7)= PRON
      NIT(8)= SRON
C
C     reassign fluxes to "permanent" array
      NITRXF(1)= ADSAM
      NITRXF(2)= DESAM
      NITRXF(3)= AMMIF
      NITRXF(4)= IMMAM
      NITRXF(5)= UTAM
      NITRXF(6)= IMMNI
      NITRXF(7)= UTNI
      NITRXF(8)= NITRF
      NITRXF(9)= DENI
      NITRXF(10)= AMVO
      NITRXF(11)= RFON
      NITRXF(12)= PRETL
      NITRXF(13)= PRETR
      NITRXF(14)= UTAMA
      NITRXF(15)= UTNIA
      NITRXF(16)= NFIX
C
      RETURN
      END
C
C
C
      SUBROUTINE   SV
     I               (MOISTM,SOILM,TCM,XFIX,CMAX,XMAX,KF1,N1I,
     I                LSNO,MESSU,MSGFL,DATIM,
     I                ITMAX,CMID,LAYID,
     M                CMSU,ECNT,
     O                CMCY,CMAD)
C
C     + + + PURPOSE + + +
C     Calculate the adsorption/desorption of chemicals by the
C     single value freundlich method
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ECNT,MSGFL,ITMAX,LSNO,MESSU,DATIM(5)
      REAL        CMAD,CMAX,CMCY,CMSU,KF1,MOISTM,
     $            N1I,SOILM,TCM,XFIX,XMAX
      CHARACTER*4 LAYID,CMID(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MOISTM - ???
C     SOILM  - ???
C     TCM    - ???
C     XFIX   - ???
C     CMAX   - ???
C     XMAX   - ???
C     KF1    - ???
C     N1I    - ???
C     LSNO   - land surface id number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of HSPF message file
C     ITMAX  - ???
C     CMID   - ???
C     LAYID  - ???
C     CMSU   - ???
C     ECNT   - ???
C     CMCY   - ???
C     CMAD   - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      REAL       C,FIXCAP,MAXAD,MAXSU,REMCM,X
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITER
C
C     + + + END SPECIFICATIONS + + +
C
      IF (MOISTM.GE.0.001) THEN
C       there is sufficient moisture for adsorption/desorption to occur
C
C       determine the capacity of soil to fix the chemical in mass/area
        FIXCAP= XFIX*SOILM*1.0E-06
C
        IF (TCM.GT.FIXCAP) THEN
C         there is more chemical than the fixed capacity, so
C         determine where the surplus resides
C
C         calculate the maximum soluble and adsorbed chemical -
C         units are mass/area
          MAXSU= CMAX*MOISTM*1.0E-06
          MAXAD= XMAX*SOILM*1.0E-06
C
C         determine if maximum adsorption capacity and solubility
C         have been reached
          REMCM= TCM- MAXAD- MAXSU
C
          IF (REMCM .GE. 0.0) THEN
C           maximum adsorption capacity and solubilty have been
C           reached, so solution and adsorbed forms are at capacity;
C           the remaining chemical is considered to be in the
C           crystalline form
            CMAD= MAXAD
            CMSU= MAXSU
            CMCY= REMCM
          ELSE
C           total amount is less than amount needed to reach capacity.
C           therefore, no crystalline form exists and adsorption/
C           desorption amounts must be determined from
C           the freundlich isotherm
            CMCY= 0.0
C
C           make initial estimate of the freundlich value for chemical
C           concentration in solution(c) - units are ppm in solution
            IF (CMSU .GT. 0.0) THEN
C             use current concentration
              C= (CMSU*1.0E06)/MOISTM
CTHJ              C= CMSU/MOISTM*1.0E06
            ELSE
C             use maximum
              C= CMAX
            END IF
C
C           find values on freundlich isotherm
            CALL ITER (TCM,MOISTM,SOILM,KF1,N1I,XFIX,ITMAX,CMID,
     I                 LAYID,LSNO,MESSU,MSGFL,DATIM,FIXCAP,
     M                 C,ECNT,
     O                 X)
C
C           convert the freundlich isotherm concentration
C           to mass/area units
            CMAD= X*SOILM*1.0E-06
            IF (CMAD.GT.TCM) CMAD= TCM
            CMSU= TCM-CMAD
          END IF
        ELSE
C         there is insufficient chemical to fullfill the fixed capacity
C         the fixed portion is part of the adsorbed phase
          CMAD= TCM
          CMCY= 0.0
          CMSU= 0.0
        END IF
      ELSE
C       insufficient moisture for adsorption/desorption to occur
        MAXAD= XMAX*SOILM*1.0E-06
        IF (TCM .GT. MAXAD) THEN
          CMAD= MAXAD
          CMCY= TCM-MAXAD
        ELSE
          CMAD= TCM
          CMCY= 0.0
        END IF
        CMSU= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ITER
     I                 (TCM,MOISTM,SOILM,KF,NI,XFIX,ITMAX,CMID,LAYID,
     I                  LSNO,MESSU,MSGFL,DATIM,FIXCAP,
     M                  C,ECNT,
     O                  X)
C
C     + + + PURPOSE + + +
C     Iterate until a sufficiently close approximation for the adsorbed
C     and solution values on the freundlich isotherm is reached
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ECNT,MSGFL,ITMAX,LSNO,MESSU,DATIM(5)
      REAL        C,FIXCAP,KF,MOISTM,NI,SOILM,TCM,X,XFIX
      CHARACTER*4 LAYID,CMID(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TCM    - ???
C     MOISTM - ???
C     SOILM  - ???
C     KF     - ???
C     NI     - ???
C     XFIX   - ???
C     ITMAX  - ???
C     CMID   - ???
C     LAYID  - ???
C     BLK    - current block number
C     LSNO   - land surface id number
C     MESSU  - ftn unit no. to be used for printout of messages
C     MSGFL  - fortran unit number of error message file
C     FIXCAP - ???
C     C      - ???
C     ECNT   - ???
C     X      - ???
C     DATIM  - date and time of day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      COUNT,I4,I20,SGRP,SCLU,J
      REAL         RCLOSE,FRAC,DENOM
      CHARACTER*4  CHSTR,CHSTR4(5)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CHSTR,CHSTR1)
      CHARACTER*1  CHSTR1(4)
      EQUIVALENCE (CHSTR4,CHSTR2)
      CHARACTER*1  CHSTR2(20)
C
C     + + + EXTERNALS + + +
      EXTERNAL   OMSTD,OMSTI,OMSTC,OMSTR,OMSG
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     error/warn message cluster
      SCLU=  308
      I4=      4
      I20=    20
      COUNT=   0
C
C     dountil
 10   CONTINUE
C
C       estimate adsorbed phase value
        X= KF*(C**NI)+ XFIX
C
C       recalculate the fraction which compares the freundlich
C       estimates with the total chemical
        DENOM= (X*SOILM+ C*MOISTM)*1.0E-06- FIXCAP
        IF (DENOM .LE. 0.0) THEN
C         denominator too small - perturb to make positive
          FRAC= 2.0
          WRITE (MESSU,*) 'WARNING - ITER: DENOM',DENOM,' COUNT',
     #                     COUNT,' FRAC 2.0'
        ELSE
C         denominator is ok - compute next iteration
          FRAC= (TCM-FIXCAP)/DENOM
        END IF
C
C       determine if these estimates are acceptable
        RCLOSE= FRAC- 1.0
C
        IF ( (ABS(RCLOSE) .GT. 0.01) .AND. (COUNT .LE. ITMAX) ) THEN
C         get ready for new iteration
C
          COUNT= COUNT+ 1
C
C         estimate solution phase value
          C= C*FRAC
        END IF
      IF ( (ABS(RCLOSE) .GT. 0.01) .AND. (COUNT .LE. ITMAX) ) GO TO 10
C
      IF (COUNT .GE. ITMAX) THEN
C       iterative freundlich solution did not converge
C       before reaching the iteration limit - error
        CALL OMSTD (DATIM)
        CALL OMSTI (LSNO)
        CHSTR= LAYID
        CALL OMSTC (I4,CHSTR1)
        DO 5 J= 1,5
C         put char*4 arg into local for equivalencing
          CHSTR4(J)= CMID(J)
  5     CONTINUE
        CALL OMSTC (I20,CHSTR2)
        CALL OMSTR (FRAC)
        CALL OMSTR (TCM)
        CALL OMSTR (X)
        CALL OMSTR (SOILM)
        CALL OMSTR (C)
        CALL OMSTR (MOISTM)
        SGRP= 1
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECNT)
      END IF
C     adjust c and x to account for tolerance in the iterative
C     process
      C= C*FRAC
      X= XFIX+ (X- XFIX)*FRAC
C
      RETURN
      END
C
C
C
      SUBROUTINE   FIRORD
     I                   (TMP,MOISTM,KDS,KAD,THKDS,THKAD,
     I                    CMSU,CMAD,
     O                    ADS,DES)
C
C     + + + PURPOSE + + +
C     Calculate the adsorption and desorption fluxes using
C     temperature dependent first order kinetics
C     internal units for first order reaction rate parameters
C     (kds,kad) are per interval
C
C     + + + DUMMY ARGUMENTS + + +
      REAL       ADS,CMAD,CMSU,DES,KAD,KDS,MOISTM,THKAD,THKDS,TMP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TMP    - ???
C     MOISTM - ???
C     KDS    - ???
C     KAD    - ???
C     THKDS  - ???
C     THKAD  - ???
C     CMSU   - ???
C     CMAD   - ???
C     ADS    - ???
C     DES    - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL       DIF35,KADK,KDSK
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TMP.GE.0.0 .AND. MOISTM.GE.0.001) THEN
C       soil layer temperature in deg c is warm enough, and soil
C       moisture in mass/area is sufficient to adsorb/desorb
C
        IF (TMP .LT. 35.0) THEN
C         soil layer temperature is less than optimum,
C         modify inputted reaction rate parameter
C         decrease inputted reaction rate parameter by use of the
C         modified arrenhius equation
          DIF35= TMP- 35.0
          KDSK = KDS*THKDS**DIF35
          KADK = KAD*THKAD**DIF35
        ELSE
C         temperature is optimum,use inputted reaction rate parameter
          KDSK= KDS
          KADK= KAD
        END IF
C
C       calculate the actual adsorption and desorption fluxes - units
C       are mass/area-ivl
        ADS= CMSU*KADK
        DES= CMAD*KDSK
      ELSE
C       either soil temperature is too cold or the soil layer is
C       too dry, zero fluxes
        ADS= 0.0
        DES= 0.0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CRDYFR
     I                   (NCRP,CRPDAT,NDAY,
     O                    CRPDAY,CRPFRC)
C
C     + + + PURPOSE + + +
C     Determine number of days in month each crop is growing and
C     fraction of monthly target plant uptake for each crop.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NCRP,CRPDAT(4,3),NDAY(12),CRPDAY(13,3)
      REAL      CRPFRC(13,3)
C
C     NCRP   - number of crops per year
C     CRPDAT - month/day of planting and harvesting for each crop
C     NDAY   - number of days in each month
C     CRPDAY - number of days in month that each crop is growing
C     CRPFRC - fraction of monthly target plant uptake for each crop
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I0,TCDAY(13),NC,MTH
      REAL       R0
C
C     + + + INTRINSICS + + +
      INTRINSIC  FLOAT
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI, ZIPR
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      R0 = 0.0
      I= 13
      CALL ZIPI (I,I0,
     O           TCDAY)
      I= 39
      CALL ZIPI (I,I0,
     O           CRPDAY)
      CALL ZIPR (I,R0,
     O           CRPFRC)
C
C     compute crop days in month
      DO 80 NC= 1, NCRP
        MTH= CRPDAT(1,NC)- 1
C
C       do-until loop - start with planting month
 70     CONTINUE
          MTH= MTH+ 1
          IF (MTH .GT. 12) THEN
C           wrap around end of year
            MTH= 1
          END IF
C
          IF (MTH .EQ. CRPDAT(3,NC)) THEN
C           month is harvest month - only go to end of season
            CRPDAY(MTH,NC)= CRPDAT(4,NC)
            IF (MTH .EQ. 2) THEN
C             compute leap year february as month 13
              CRPDAY(13,NC)= CRPDAY(2,NC)
            END IF
          ELSE
C           season goes to end of month
            CRPDAY(MTH,NC)= NDAY(MTH)
            IF (MTH .EQ. 2) THEN
C             compute leap year february as month 13
              CRPDAY(13,NC)= 29
            END IF
          END IF
          IF (MTH .EQ. CRPDAT(1,NC)) THEN
C           month is planting month - subtract days before planting
            CRPDAY(MTH,NC)= CRPDAY(MTH,NC)- CRPDAT(2,NC)+ 1
            IF (MTH .EQ. 2) THEN
C             compute leap year february as month 13
              CRPDAY(13,NC)= CRPDAY(2,NC)- CRPDAT(2,NC)+ 1
            END IF
          END IF
C
C         accumulate total crop days per month
          TCDAY(MTH)= TCDAY(MTH)+ CRPDAY(MTH,NC)
          IF (MTH .EQ. 2) THEN
C           compute leap year february as month 13
            TCDAY(13)= TCDAY(13)+ CRPDAY(13,NC)
          END IF
C
C       end of do-until loop - stop when reach harvest month
        IF (MTH .NE. CRPDAT(3,NC)) GO TO 70
 80   CONTINUE
C
C     compute fractions
      DO 100 NC= 1, NCRP
        DO 90 MTH= 1, 13
          IF (TCDAY(MTH) .EQ. 0) THEN
C           no uptake for month
            CRPFRC(MTH,NC)= 0.0
          ELSE
C           compute fraction
            CRPFRC(MTH,NC)= FLOAT (CRPDAY(MTH,NC)) / FLOAT (TCDAY(MTH))
          END IF
 90     CONTINUE
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   YUPINI
     I                    (DELT60,YR,MON,DAY,NDAY,NCRP,CRPDAT,CRPDAY,
     I                     CRPFRC,TUPTGT,UPTFM,NCMPT,UPTM,
     O                     PUTG)
C
C     + + + PURPOSE + + +
C     Calculate initial values of the daily plant uptake target on
C     last day of previous month.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER YR,MON,DAY,NDAY(12),NCRP,CRPDAT(4,3),CRPDAY(13,3),NCMPT
      REAL    DELT60,CRPFRC(13,3),TUPTGT,UPTFM(12),UPTM(12,NCMPT),
     $        PUTG(NCMPT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT60 - simulation time interval in hours
C     YR     - current year
C     MON    - current month
C     DAY    - current day of month
C     NDAY   - number of days in each month
C     NCRP   - number of crops per year
C     CRPDAT - month/day of planting and harvesting for each crop
C     CRPDAY - number of days in month that each crop is growing
C     CRPFRC - fraction of monthly target plant uptake for each crop
C     TUPTGT - total annual target plant uptake
C     UPTFM  - fraction of annual target plant uptake applied to each month
C     NCMPT  - number of layers (compartments) in soil column
C     SUPTM  - fraction of monthly target plant uptake from each soil layer
C     SPUTG  - daily target plant uptake on last day of previous month for
C              each soil layer
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER ICROP,YEAR,MTH,LDAY,LPYRFG,PMON,PDAY
      REAL    R0,UPTG(NCMPTS),DFC(NCMPTS+1)
C
C     + + + EXTERNALS + + +
      EXTERNAL CRPSEL,YUPTGT,LPYEAR,ZIPR
C
C     + + + END SPECIFICATIONS + + +
C
      R0 = 0.0
C
C     deficits and previous targets are all zero during off-season
      CALL ZIPR (NCMPT,R0,PUTG)
      CALL ZIPR (NCMPT+1,R0,DFC)
C
C     determine which crop is in effect at beginning of run
      CALL CRPSEL (MON,DAY,CRPDAT,NCRP,
     O             ICROP)
C
      IF (ICROP .GT. 0) THEN
C       run starts during a crop season - trace uptake targets
C       from beginning of season
C
        PMON= CRPDAT(1,ICROP)
        PDAY= CRPDAT(2,ICROP)
        IF ( (MON .GT. PMON) .OR.
     $       ( (MON .EQ. PMON) .AND. (DAY .GE. PDAY) ) ) THEN
C         season began same calendar year
          YEAR= YR
        ELSE
C         season began previous calendar year
          YEAR= YR- 1
        END IF
        MTH= PMON- 1
C       determine if current year is a leap year
        CALL LPYEAR (YEAR,
     O               LPYRFG)
C       do-until loop
 80     CONTINUE
          MTH= MTH+ 1
          IF (MTH .GT. 12) THEN
C           wrap around end of year
            MTH= 1
            YEAR= YEAR+ 1
C           determine if current year is a leap year
            CALL LPYEAR (YEAR,
     O                   LPYRFG)
          END IF
          IF (MTH .NE. MON) THEN
C           calculate interval targets for last day of month
            IF (MTH .EQ. 2) THEN
C             february is special case
              IF (LPYRFG .EQ. 1) THEN
C               long february
                LDAY= 29
              ELSE
C               regular february
                LDAY= 28
              END IF
            ELSE
C             use natural end of month
              LDAY= NDAY(MTH)
            END IF
            CALL YUPTGT (DELT60,YEAR,MTH,LDAY,LDAY,NCRP,CRPDAT,CRPDAY,
     I                   CRPFRC,TUPTGT,UPTFM,NCMPT,UPTM,
     M                   PUTG,DFC,
     O                   UPTG)
          END IF
C       end do-until
        IF (MTH .NE. MON) GO TO 80
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CRPSEL
     I                    (MON,DAY,CRPDAT,NCRP,
     O                     ICROP)
C
C     + + + PURPOSE + + +
C     Determines which, if any, of the current crop seasons
C     includes the current day and month.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MON,DAY,CRPDAT(4,3),NCRP,ICROP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MON    - current month
C     DAY    - current day of month
C     CRPDAT - month/day of planting and harvesting for each crop
C     NCRP   - number of crops per year
C     ICROP  - index of current crop; or zero if none is current
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,PMON,PDAY,HMON,HDAY
C
C     + + + END SPECIFICATIONS + + +
C
      ICROP= 0
C
      IF (NCRP .GT. 0) THEN
C       check which crop is active
        I= 0
 10     CONTINUE
          I= I+ 1
          PMON= CRPDAT(1,I)
          PDAY= CRPDAT(2,I)
          HMON= CRPDAT(3,I)
          HDAY= CRPDAT(4,I)
          IF ( (PMON .LT. HMON) .OR.
     $         ( (PMON .EQ. HMON) .AND.
     $           (PDAY .LT. HDAY) ) ) THEN
C           season does not cross year boundary
            IF ( (MON .GT. PMON) .AND. (MON .LT. HMON) ) THEN
C             whole current month is in season
              ICROP= I
            ELSE IF ( (MON .EQ. PMON) .AND. (MON .EQ. HMON) ) THEN
C             whole season is in current month
              IF ( (DAY .GE. PDAY) .AND. (DAY .LE. HDAY) ) THEN
C               current day is in season
                ICROP= I
              END IF
            ELSE IF ( (MON .EQ. PMON) .AND. (DAY .GE. PDAY) ) THEN
C             current day is after planting this month
              ICROP= I
            ELSE IF ( (MON .EQ. HMON) .AND. (DAY .LE. HDAY) ) THEN
C             current day is before harvesting this month
              ICROP= I
            END IF
          ELSE
C           season crosses year boundary
            IF ( (MON .GT. PMON) .OR. (MON .LT. HMON) ) THEN
C             whole current month is in season
              ICROP= I
            ELSE IF ( (MON .EQ. PMON) .AND. (MON .EQ. HMON) ) THEN
C             whole off-season is in current month
              IF ( (DAY .GE. PDAY) .OR. (DAY .LE. HDAY) ) THEN
C               current day is in season
                ICROP= I
              END IF
            ELSE IF ( (MON .EQ. PMON) .AND. (DAY .GE. PDAY) ) THEN
C             current day is after planting this month
              ICROP= I
            ELSE IF ( (MON .EQ. HMON) .AND. (DAY .LE. HDAY) ) THEN
C             current day is before harvesting this month
              ICROP= I
            END IF
          END IF
C       end do-until loop
        IF ( (ICROP .EQ. 0) .AND. (I .LT. NCRP) ) GO TO 10
C
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   YUPTGT
     I                    (DELT60,YR,MON,DAY,NDAYS,NCRP,CRPDAT,CRPDAY,
     I                     CRPFRC,TUPTGT,UPTFM,NCMPT,UPTM,
     M                     PUTG,DFC,
     O                     UPTG)
C
C     + + + PURPOSE + + +
C     Calculates daily yield-based plant uptake targets for each soil
C     layer based on user-specified monthly fractions of the annual
C     target and a trapezoidal function to interpolate between months.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER YR,MON,DAY,NDAYS,NCRP,CRPDAT(4,3),CRPDAY(13,3),NCMPT
      REAL    DELT60,CRPFRC(13,3),TUPTGT,UPTFM(12),UPTM(12,NCMPT),
     $        PUTG(NCMPT),DFC(NCMPT+1),UPTG(NCMPT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT60 - simulation time interval in hours
C     YR     - current year
C     MON    - current month
C     DAY    - current day of month
C     NDAYS  - number of days in current month
C     NCRP   - number of crops per year
C     CRPDAT - month/day of planting and harvesting for each crop
C     CRPDAY - number of days in month that each crop is growing
C     CRPFRC - fraction of monthly target plant uptake for each crop
C     TUPTGT - total annual target plant uptake
C     UPTFM  - fraction of annual target plant uptake applied to each month
C     NCMPT  - number of layers (compartments) in soil column
C     UPTM   - fraction of monthly target plant uptake from each soil layer
C     PUTG   - daily target plant uptake on last day of previous month for
C              each soil layer
C     DFC    - cumulative plant uptake deficit from each soil layer,
C              position NCMPT+1 is total deficit
C     UPTG   - current interval target plant uptake from each soil layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,ICROP,LPYRFG,CURDAY
      REAL       R0
C
C     + + + EXTERNALS + + +
      EXTERNAL   CRPSEL,LPYEAR,YUPLAY,ZIPR
C
C     + + + END SPECIFICATIONS + + +
C
      R0 = 0.0
C
C     determine current crop
      CALL CRPSEL (MON,DAY,CRPDAT,NCRP,
     O             ICROP)
C
      IF (ICROP .EQ. 0) THEN
C       no active crop - reset previous and current targets and deficits
        CALL ZIPR (NCMPT,R0,PUTG)
        CALL ZIPR (NCMPT,R0,UPTG)
        CALL ZIPR (NCMPT+1,R0,DFC)
      ELSE
C       compute interval targets
C
C       find current day of season this month
        IF (MON .EQ. CRPDAT(1,ICROP)) THEN
C         planting took place earlier this month
          CURDAY= DAY- CRPDAT(2,ICROP)+ 1
        ELSE
C         season began before current month
          CURDAY= DAY
        END IF
C
C       determine if current year is a leap year
        CALL LPYEAR (YR,
     O               LPYRFG)
C
        DO 10 I = 1,NCMPT
C         find target for each layer
          CALL YUPLAY (DELT60,LPYRFG,MON,DAY,CURDAY,NDAYS,ICROP,CRPDAT,
     I                 CRPDAY,CRPFRC,TUPTGT,UPTFM,UPTM(1,I),
     M                 PUTG(I),
     O                 UPTG(I))
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   YUPLAY
     I                    (DELT60,LPYRFG,MON,DAY,CURDAY,NDAYS,ICROP,
     I                     CRPDAT,CRPDAY,CRPFRC,TUPTGT,UPTFM,
     I                     UPTM,
     M                     PUTG,
     O                     UPTG)
C
C     + + + PURPOSE + + +
C     Calculates daily yield-based plant uptake targets for a soil
C     layer based on user-specified monthly fractions of the annual
C     target and a trapezoidal function to interpolate between months.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER LPYRFG,MON,DAY,CURDAY,NDAYS,ICROP,CRPDAT(4,3),
     $        CRPDAY(13,3)
      REAL    DELT60,CRPFRC(13,3),TUPTGT,UPTFM(12),UPTM(12),PUTG,UPTG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DELT60 - simulation time interval in hours
C     MON    - current month
C     DAY    - current day of month
C     CURDAY - current day of season this month
C     NDAYS  - number of days in current month
C     ICROP  - index of current crop
C     CRPDAT - month/day of planting and harvesting for each crop
C     CRPDAY - number of days in month that each crop is growing
C     CRPFRC - fraction of monthly target plant uptake for each crop
C     TUPTGT - total annual target plant uptake
C     UPTFM  - fraction of annual target plant uptake applied to each month
C     UPTM   - fraction of monthly target plant uptake from soil layer
C     PUTG   - daily target plant uptake on last day of previous month for
C              soil layer
C     UPTG   - current interval target plant uptake from soil layer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER MTH,LNDAYS,SUMDAY(31)
      REAL    MONTGT,DELTGT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA SUMDAY/1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,
     #        171,190,210,231,253,276,300,325,351,378,406,435,465,496/
C
C     + + + END SPECIFICATIONS + + +
C
C     calculate monthly target
      MONTGT= TUPTGT* UPTFM(MON)* UPTM(MON)* CRPFRC(MON,ICROP)
C
C     find daily change in monthly target
      IF ( (LPYRFG .EQ. 1) .AND. (MON .EQ. 2) ) THEN
C       use month 13 for 29-day february
        MTH= 13
        LNDAYS= 29
      ELSE
C       use current month
        MTH= MON
        LNDAYS= NDAYS
      END IF
      DELTGT= (MONTGT- PUTG*CRPDAY(MTH,ICROP)) /
     #         SUMDAY(CRPDAY(MTH,ICROP))
C
C     find daily target
      UPTG= PUTG+ CURDAY*DELTGT
      IF (UPTG .LT. 0.0) THEN
C       cut off uptake
        UPTG= 0.0
      END IF
C
C     update daily target at end of previous month
      IF ( (MON .EQ. CRPDAT(3,ICROP)) .AND.
     $     (DAY .EQ. CRPDAT(4,ICROP)) ) THEN
C       today is harvest day - reset previous target to zero
        PUTG= 0.0
      ELSE IF (DAY .EQ. LNDAYS) THEN
C       today is last day of month - this becomes previous target
        PUTG= UPTG
      END IF
C
C     convert from daily target to interval target
      UPTG= UPTG* DELT60/24.0
C
      RETURN
      END
C
C
C
      SUBROUTINE   LPYEAR
     I                  (YEAR,
     O                  LPYRFG)
C
C     + + + PURPOSE + + +
C     Returns a leap year flag, lpyrfg, that is on if the year is a
C     leap year.
C
C     + + + KEYWORDS + + +
C     ???
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4  LPYRFG,YEAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YEAR   - ???
C     LPYRFG - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4  I4,I100,I400
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + END SPECIFICATIONS + + +
C
      I4   = 4
      I100 = 100
      I400 = 400
C
      IF ( MOD(YEAR,I100) .EQ. 0) THEN
C       on a century boundary
        IF ( MOD(YEAR,I400) .EQ. 0) THEN
C         on a 400 year boundary
          LPYRFG= 1
        ELSE
          LPYRFG= 0
        END IF
      ELSE
        IF ( MOD(YEAR,I4) .EQ. 0) THEN
C         leap year
          LPYRFG= 1
        ELSE
          LPYRFG= 0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRZNRD
     I                   (LPRZIN,FRSTRD,SEPTON,
     O                    LINCOD)
C
C     + + + PURPOSE + + +
C     Read nitrogen input parameters for PRZM nitrogen simulation.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LPRZIN
      LOGICAL      FRSTRD,SEPTON
      CHARACTER*80 LINCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LPRZIN - Fortran unit number for PRZM-2 input file
C     FRSTRD - ???
C     SEPTON - septic effluent on flag
C     LINCOD - character string for record number being read
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CNITR.INC'
      INCLUDE 'CSPTIC.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,ISTRT,IEND,IERROR,APM,APD
      REAL         NUPSUM
      CHARACTER*80 MESAGE
      LOGICAL      FATAL
C
C     + + + FUNCTIONS + + +
      INTEGER      DYJDY
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     DYJDY, ECHORD, ERRCHK
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(8I5)
 1010 FORMAT(I5,F5.0,4I5)
 1020 FORMAT(10F8.0)
 1030 FORMAT(12F5.0)
 1040 FORMAT(A78)
 1050 FORMAT(2X,3I2,I8,5F8.0)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('The horizon number specified to receive septic influent ',
     $       '[',I2,'] does not exist.')
 2010 FORMAT('If FIXNFG is 1, NUPTFG must be 1.  As NUPTFG is 0,',
     $       ' FIXNFG will be set to 0.')
 2020 FORMAT('Sum of monthly plant uptake fractions over the year [',
     $        F5.2,'] do not sum to 1.')
 2030 FORMAT('Sum of layered plant uptake fractions [',F5.2,
     $       '] do not sum to 1. in month [',I2,']')
 2040 FORMAT('Sum of fraction of nitrogen uptake from nitrate & ',
     $       'ammonium [',F5.2,'] is not 1.')
C
C     + + + END SPECIFICATIONS + + +
C
C     init number of compartments in 1st horizon so that it will
C     be calculated first time through NITR
      NC1 = 0
C
      LINCOD = 'N1.0'
      CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O             MESAGE)
      READ(MESAGE,1040,END=910,ERR=920) NTITLE
C
      IF (SEPTON) THEN
C       septic effluent parameters
        LINCOD = 'N2.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1010,END=910,ERR=920) SEPHZN,ORGRFC,
     $                                   (SEPDSN(I),I=1,4)
        IF (SEPHZN.LT.1 .OR. SEPHZN.GT.NHORIZ) THEN
C         invalid horizon specified for septic effluent
          IERROR = 4000
          WRITE(MESAGE,2000) SEPHZN
          FATAL  = .TRUE.
          CALL ERRCHK(IERROR,MESAGE,FATAL)
        END IF
      END IF
C
C     nitrogen flags
      LINCOD = 'N3.0'
      CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O             MESAGE)
      READ(MESAGE,1000,END=910,ERR=920) VNUTFG,FORAFG,ITMAXA,NUPTFG,
     $                                  FIXNFG,AMVOFG,ALPNFG,VNPRFG
C
      IF (FIXNFG.EQ.1 .AND. NUPTFG.NE.1) THEN
        IERROR = 4010
        WRITE(MESAGE,2010)
        FATAL  = .FALSE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      END IF
C
C     atmospheric deposition flags
      LINCOD = 'N4.0'
      CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O             MESAGE)
      READ(MESAGE,1000,END=910,ERR=920) (NIADFG(I),I=1,6)
      DO 5 I = 1,6
C       see if any deposition input as monthly values
        IF (NIADFG(I).EQ.-2) THEN
C         this constituent input as monthly deposition values
          LINCOD = 'N5.0'
          CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                 MESAGE)
          IF (I.LE.3) THEN
            READ(MESAGE,1030,END=910,ERR=920) (NIAFXM(J,I),J=1,12)
          ELSE
            READ(MESAGE,1030,END=910,ERR=920) (NIACNM(J,I),J=1,12)
          END IF
        END IF
 5    CONTINUE
C
C     number of ag applications
      LINCOD = 'N6.0'
      CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O             MESAGE)
      READ(MESAGE,1000,END=910,ERR=920) NAPS,FRMFLG
C
      IF (NAPS.GT.0) THEN
C       read nitrogen application records
        LINCOD = 'N7.0'
        DO 7 I = 1,NAPS
C         read record for each application
          CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                 MESAGE)
          READ(MESAGE,1050,END=910,ERR=920) APD,APM,IAPYR(I),WINDAY(I),
     $                                      DEPI(1,I),(TAPP(J,I),J=1,3),
     $                                      NAPFRC(I)
C         determine julian application date
          IAPDY(I) = DYJDY(IAPYR(I),APM,APD)
 7      CONTINUE
      END IF
C
      IF (NUPTFG.EQ.0) THEN
C       first order plant uptake being used
        IF (VNUTFG.EQ.0) THEN
C         nitrogen plant uptake rate parameters do not vary
          LINCOD = 'N8.0'
          DO 10 ISTRT = 1, NHORIZ, 8
C           read up to 8 horizon values per record
            IEND = ISTRT + 7
            IF (IEND.GT.NHORIZ) IEND = NHORIZ
            CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                   MESAGE)
            READ(MESAGE,1020,END=910,ERR=920) (KPLN(I),I=ISTRT,IEND)
 10       CONTINUE
        ELSE
C         nitrogen plant uptake rate parameters vary throughout year
          LINCOD = 'N9.0'
          DO 20 I = 1,NHORIZ
C           get monthly values for each horizon
            CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                   MESAGE)
            READ(MESAGE,1030,END=910,ERR=920) (KPLNM(J,I),J=1,12)
 20       CONTINUE
        END IF
      ELSE IF (NUPTFG.EQ.1) THEN
C       yield-based plant uptake
        LINCOD = 'N10.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1020,END=910,ERR=920) NUPTGT,NMXRAT
C       monthly fractions for yield-based plant uptake of nitrogen
        LINCOD = 'N11.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1030,END=910,ERR=920) (NUPTFM(I),I=1,12)
        NUPSUM = 0.0
        DO 25 I = 1,12
C         sum monthly values over year
          NUPSUM = NUPSUM + NUPTFM(I)
 25     CONTINUE
        IF (ABS(NUPSUM-1.0).GT.1.0E-6) THEN
C         values do not sum to unity
          IERROR = 4020
          WRITE(MESAGE,2020) NUPSUM
          FATAL  = .TRUE.
          CALL ERRCHK(IERROR,MESAGE,FATAL)
        END IF
C       monthly fractions of plant uptake per soil layer
        LINCOD = 'N12.0'
        DO 30 I = 1,NHORIZ
C         get monthly values for each horizon
          CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                 MESAGE)
          READ(MESAGE,1030,END=910,ERR=920) (NUPTM(J,I),J=1,12)
 30     CONTINUE
        DO 37 J = 1,12
          NUPSUM = 0.0
          DO 35 I = 1,NHORIZ
C           sum monthly values over year
            NUPSUM = NUPSUM + NUPTM(J,I)
 35       CONTINUE
          IF (ABS(NUPSUM-1.0).GT.1.0E-6) THEN
C           values do not sum to unity
            IERROR = 4030
            WRITE(MESAGE,2030) NUPSUM,J
            FATAL  = .TRUE.
            CALL ERRCHK(IERROR,MESAGE,FATAL)
          END IF
 37     CONTINUE
      END IF
C
      IF (ALPNFG.EQ.1) THEN
C       above ground plant nitrogen being simulated
        IF (VNUTFG.EQ.0) THEN
C         above ground plant uptake fractions do not vary
          LINCOD = 'N13.0'
          DO 40 ISTRT = 1, NHORIZ, 8
C           read up to 8 horizon values per record
            IEND = ISTRT + 7
            IF (IEND.GT.NHORIZ) IEND = NHORIZ
            CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                   MESAGE)
            READ(MESAGE,1020,END=910,ERR=920) (ANUTF(I),I=ISTRT,IEND)
 40       CONTINUE
        ELSE
C         above ground plant uptake fractions vary throughout year
          LINCOD = 'N14.0'
          DO 50 I = 1,NHORIZ
C           get monthly values for each horizon
            CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                   MESAGE)
            READ(MESAGE,1030,END=910,ERR=920) (ANUFM(J,I),J=1,12)
 50       CONTINUE
        END IF
      END IF
C
C     first order general parameters
      LINCOD = 'N15.0'
      CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O             MESAGE)
      READ(MESAGE,1020,END=910,ERR=920) (GNPM(I),I=1,10)
      IF (ABS(GNPM(1)+GNPM(2)-1.0).GT.1.0E-6) THEN
C       sum of nitrate and ammonium nitrogne uptake is not unity
        IERROR = 4040
        WRITE(MESAGE,2040) GNPM(1)+GNPM(2)
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      END IF
C     first order reaction parameters for all horizons
      LINCOD = 'N16.0'
      DO 60 I = 1,NHORIZ
C       read values for each horizon
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1020,END=910,ERR=920) (NPM(J,I),J=1,5),DNTHRS(I),
     $                                     NPM(6,I),NPM(7,I)
 60   CONTINUE
C
      IF (FORAFG.EQ.1) THEN
C       single value Freundlich isotherm method for ammonium adsorp/desorp
C       max solubility of ammonium in water
        LINCOD = 'N17.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1020,END=910,ERR=920) GNPM(11)
C       adsorption/desorption parameters
        LINCOD = 'N18.0'
        DO 70 I = 1,NHORIZ
C         read values for each horizon
          CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                 MESAGE)
          READ(MESAGE,1020,END=910,ERR=920) NPM(8,I),NPM(10,I),NPM(11,I)
C         calculate max adsorption capacity
          NPM(9,I) = NPM(10,I)*(GNPM(11)**NPM(11,I))+ NPM(8,I)
 70     CONTINUE
      END IF
C
      IF (AMVOFG.EQ.1) THEN
C       ammonia volatilization parameters
        LINCOD = 'N19.0'
        DO 80 ISTRT = 1, NHORIZ+2, 8
C         read up to 8 horizon values per record
          IEND = ISTRT + 5
          IF (IEND.GT.NHORIZ) IEND = NHORIZ
          CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                 MESAGE)
          IF (ISTRT.EQ.1) THEN
C           read temp parameters
            READ(MESAGE,1020,END=910,ERR=920) THVOL,TRFVOL,
     $                                       (KVOL(I),I=ISTRT,IEND)
          ELSE
C           continue to read horizon values
            READ(MESAGE,1020,END=910,ERR=920) (KVOL(I),I=ISTRT-2,IEND)
          END IF
 80     CONTINUE
      END IF
C
C     organic nitrogen transformation parameters
      LINCOD = 'N20.0'
      DO 90 I = 1,NHORIZ
C       read values for each horizon
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1020,END=910,ERR=920) (ORNPM(J,I),J=1,4)
 90   CONTINUE
C
      IF (VNPRFG.EQ.0) THEN
C       plant return rates do not vary
C       below ground return rates for all horizons
        LINCOD = 'N21.0'
        DO 100 ISTRT = 1, NHORIZ+1, 8
C         read up to 8 horizon values per record
          IEND = ISTRT + 6
          IF (IEND.GT.NHORIZ) IEND = NHORIZ
          CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                 MESAGE)
          IF (IEND.EQ.NHORIZ) THEN
C           last record in series, need to get refractory fraction
            IF (IEND.GE.ISTRT) THEN
C             more horizon values to read
              READ(MESAGE,1020,END=910,ERR=920)(KRETBN(I),I=ISTRT,IEND),
     $                                          BGNPRF
            ELSE
C             only refractory fraction parameter left to read
              READ(MESAGE,1020,END=910,ERR=920) BGNPRF
            END IF
          ELSE
C           continue reading horizon values
            READ(MESAGE,1020,END=910,ERR=920) (KRETBN(I),I=ISTRT,IEND+1)
          END IF
 100    CONTINUE
C       above ground return rate for top horizon
        LINCOD = 'N22.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1020,END=910,ERR=920) AGKPRN,KRETAN(1),LINPRF
      ELSE
C       plant nitrogen return rates vary throughout the year
C       below ground plant return rates
        LINCOD = 'N23.0'
        DO 110 I = 1,NHORIZ
C         get monthly values for each horizon
          CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O                 MESAGE)
          READ(MESAGE,1030,END=910,ERR=920) (KRBNM(J,I),J=1,12)
 110    CONTINUE
C       refractory fractions for below ground plant N return
        LINCOD = 'N24.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1030,END=910,ERR=920) (BNPRFM(I),I=1,12)
C       above ground plant N return rates to litter N
        LINCOD = 'N25.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1030,END=910,ERR=920) (KRANM(I),I=1,12)
C       litter N return rate to 1st horizon
        LINCOD = 'N26.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1030,END=910,ERR=920) (KRLNM(I),I=1,12)
C       refractory fractions for litter N return
        LINCOD = 'N27.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1030,END=910,ERR=920) (LNPRFM(I),I=1,12)
      END IF
C
C     initial storage of constituents
      TONIT0 = 0.0
      LINCOD = 'N28.0'
      DO 120 I = 1,NHORIZ
C       read values for each horizon
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1020,END=910,ERR=920) NIT(1,I),(NIT(J,I),J=6,8),
     $                                   (NIT(J,I),J=2,5)
C       initial nitrogen in storage
        TONIT0= TONIT0 + NIT(1,I) + NIT(2,I) + NIT(3,I) + NIT(4,I) +
     $          NIT(5,I) + NIT(6,I) + NIT(7,I) + NIT(8,I)
 120  CONTINUE
      IF (ALPNFG.EQ.1) THEN
C       initial storage of above ground plant and litter N
        LINCOD = 'N29.0'
        CALL ECHORD (LPRZIN,LINCOD,FRSTRD,
     O               MESAGE)
        READ(MESAGE,1020,END=910,ERR=920) AGPLTN,LITTRN
        TONIT0 = TONIT0 + AGPLTN + LITTRN
      END IF
C
 910  CONTINUE
C       get here on a read error
C
 920  CONTINUE
C       get here on end of file
C
C     init current total nitrogen to initial total nitrogen
      TOTNIT = TONIT0
C
      RETURN
      END
C
C
C
      SUBROUTINE   OMSG
     I                 (MESSU,MESSFL,SCLU,SGRP,
     M                  COUNT)
C
C     + + + PURPOSE + + +
C     output an error or warning message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSU,MESSFL,SCLU,SGRP,COUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSU  - unit number to write message to
C     MESSFL - unit number containing text of message
C     SCLU   - cluster on message file containing message text
C     SGRP   - group on message file containing message text
C     COUNT  - count of messages written
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,MAXCNT,MAXACT
      CHARACTER*3 PID
C
C     + + + EXTERNALS + + +
      EXTERNAL    OMSINI
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A3,/,1X,A3,1X,100(1H*),/,1X,A3,/)
 2010 FORMAT (1X,A3,10X,'DATE: ',I4,'/',I2,'/',I2,/,1X,A3,/,
     $        1X,A3,' PRZM-2 zone: ',I3,7X,'Compartment: ',4A1,/,1X,A3,/
     $        1X,A3,' A negative value is projected for nitrate.  ',
     $              'To avoid the problem,',/,
     $        1X,A3,' fluxes will be arbitrarily reduced.  ',
     $              'Relevant data are:',/,1X,A3,/,
     $        1X,A3,'      FRAC       NO3      TNO3     NITRF',
     $              '     IMMNI      DENI      UTNI     UTNIA',/,
     $        1X,A3,8F10.2,/,1X,A3,/,
     $        1X,A3,' See PRZM-2 manual for definitions of terms.',/)
 2020 FORMAT (1X,A3,10X,'DATE: ',I4,'/',I2,'/',I2,/,1X,A3,/,
     $        1X,A3,' PRZM-2 zone: ',I3,7X,'Compartment: ',4A1,/,1X,A3,/
     $        1X,A3,' A negative value is projected for particulate ',
     $              'labile organic nitrogen.',/,
     $        1X,A3,' To avoid the problem, fluxes will be ',
     $              'arbitrarily reduced.  Relevant data',/,
     $        1X,A3,' are:',/,1X,A3,/,
     $        1X,A3,'      FRAC      ORGN     TORGN     AMMIF',
     $              '     IMMAM     IMMNI     PRETL      RFON',/,
     $        1X,A3,8F10.2,/,1X,A3,/,
     $        1X,A3,' See PRZM-2 manual for definitions of terms.',/)
 2030 FORMAT (1X,A3,10X,'DATE: ',I4,'/',I2,'/',I2,/,1X,A3,/,
     $        1X,A3,' PRZM-2 zone: ',I3,7X,'Compartment: ',4A1,/,1X,A3,/
     $        1X,A3,' A negative value is projected for adsorbed ',
     $              'ammonium.  To avoid the',/,
     $        1X,A3,' problem, fluxes will be arbitrarily ',
     $              'reduced.  Relevant data are:',/,
     $        1X,A3,' are:',/,1X,A3,/,
     $        1X,A3,'      FRAC      AMAD     TAMAD     ADSAM',
     $              '     DESAM',/,
     $        1X,A3,5F10.2,/,1X,A3,/,
     $        1X,A3,' See PRZM-2 manual for definitions of terms.',/)
 2040 FORMAT (1X,A3,10X,'DATE: ',I4,'/',I2,'/',I2,/,1X,A3,/,
     $        1X,A3,' PRZM-2 zone: ',I3,7X,'Compartment: ',4A1,/,1X,A3,/
     $        1X,A3,' A negative value is projected for solution ',
     $              'ammonium.  To avoid the problem,',/,
     $        1X,A3,' fluxes will be arbitrarily reduced.  ',
     $              'Relevant data are:',/,1X,A3,/,
     $        1X,A3,'      FRAC      AMSU     TAMSU     ADSAM',
     $              '     DESAM     AMMIF     IMMAM     NITRF',
     $              '      UTAM     UTAMA      AMVO',/,
     $        1X,A3,11F10.2,/,1X,A3,/,
     $        1X,A3,' See PRZM-2 manual for definitions of terms.',/)
 2050 FORMAT (1X,A3,10X,'DATE: ',I4,'/',I2,'/',I2,/,1X,A3,/,
     $        1X,A3,' PRZM-2 zone: ',I3,7X,'Compartment: ',4A1,/,1X,A3,/
     $        1X,A3,' The iterative technique (subroutine iter) used ',
     $              'to solve the Freundlich',/,
     $        1X,A3,' adsorption/desorption equation did not converge ',
     $              'within the allowed no. of',/,
     $        1X,A3,' iterations.  Relevant data are:',/,1X,A3,/,
     $        1X,A3,'  Pesticide',/,
     $        1X,A3,20A1,/,1X,A3,/,
     $        1X,A3,'      Frac       TCM         X     Soilm',
     $              '         C    MOISTM',/,
     $        1X,A3,6F10.2,/,1X,A3,/,
     $        1X,A3,' See PRZM-2 manual for definitions of terms.',/)
 2100 FORMAT (1X,A3,/,1X,A3,1X,100(1H*))
 2150 FORMAT (1X,A3,/,1X,A3,' The count for the WARNING printed ',
     $        'above has reached its maximum.',/,1X,A3,/,1X,A3,
     $        ' If the condition is encountered again the ',
     $        'message will not be repeated.')
 2160 FORMAT (1X,A3,/,1X,A3,' The count for the ERROR printed ',
     $        'above has reached its maximum.',/,1X,A3,/,1X,A3,
     $        ' The RUN has been terminated.')
C
C     + + + END SPECIFICATIONS + + +
C
      PID = 'WTR'
C
C     increment counter for this message
      COUNT= COUNT+ 1
C
      IF (COUNT .LE. 50) THEN
C       how many will we accept and what do we do when max is reached
        IF (SCLU.EQ.310) THEN
          MAXCNT = 10
          MAXACT = 0
        ELSE IF (SCLU.EQ.308) THEN
          MAXCNT = 20
          MAXACT = 1
        END IF
      ELSE
C       assume we dont want this again
        MAXCNT= 1
      END IF
C
      IF (COUNT .LE. MAXCNT) THEN
C       write detailed error message
C       first write line of asterisks as separator
        WRITE (MESSU,2000) (PID,I=1,3)
        IF (SCLU.EQ.310 .AND. SGRP.EQ.1) THEN
          WRITE (MESSU,2010) PID,(DATIM(I),I=1,3),PID,PID,IMSVL(1),
     $                      (CMSVL(I),I=1,4),(PID,I=1,6),
     $                      (RMSVL(I),I=1,8),PID,PID
        ELSE IF (SCLU.EQ.310 .AND. SGRP.EQ.2) THEN
          WRITE (MESSU,2020) PID,(DATIM(I),I=1,3),PID,PID,IMSVL(1),
     $                      (CMSVL(I),I=1,4),(PID,I=1,7),
     $                      (RMSVL(I),I=1,8),PID,PID
        ELSE IF (SCLU.EQ.310 .AND. SGRP.EQ.3) THEN
          WRITE (MESSU,2030) PID,(DATIM(I),I=1,3),PID,PID,IMSVL(1),
     $                      (CMSVL(I),I=1,4),(PID,I=1,7),
     $                      (RMSVL(I),I=1,5),PID,PID
        ELSE IF (SCLU.EQ.310 .AND. SGRP.EQ.4) THEN
          WRITE (MESSU,2040) PID,(DATIM(I),I=1,3),PID,PID,IMSVL(1),
     $                      (CMSVL(I),I=1,4),(PID,I=1,6),
     $                      (RMSVL(I),I=1,11),PID,PID
        ELSE IF (SCLU.EQ.308 .AND. SGRP.EQ.1) THEN
          WRITE (MESSU,2050) PID,(DATIM(I),I=1,3),PID,PID,IMSVL(1),
     $                      (CMSVL(I),I=1,4),(PID,I=1,7),
     $                      (CMSVL(I),I=5,24),(PID,I=1,3),
     $                      (RMSVL(I),I=1,6),PID,PID
        END IF
C       write bottom of message separator
        WRITE (MESSU,2100) PID,PID
      END IF
C
      IF (COUNT .EQ. MAXCNT) THEN
C       print last time message
        IF (SCLU.EQ.308) THEN
C         last error message
          WRITE (MESSU,2160) (PID,I=1,4)
        ELSE
C         last warning message
          WRITE (MESSU,2150) (PID,I=1,4)
        END IF
C       write line of asterisks as separator to messu
        WRITE (MESSU,2100) PID,PID
        IF (MAXACT .EQ. 1) THEN
C         this is fatal!
          STOP
        END IF
      END IF
C
C     reset storages
      CALL OMSINI
C
      RETURN
      END
C
C
C
      SUBROUTINE   OMSINI
C
C     + + + PURPOSE + + +
C     reset assoc parms to don't write
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      ICNT= 0
      RCNT= 0
      CCNT= 0
      DO 10 I= 1,5
        DATIM(I)= 0
 10   CONTINUE
C
      RETURN
      END
C
C
C
C
      SUBROUTINE   OMSTI
     I                  (IVAL)
C
C     + + + PURPOSE + + +
C     save an integer value to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL   - value to save
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     increment counter of values saved
      ICNT= ICNT+ 1
      IF (ICNT .LE. MXMSI) THEN
C       save value
        IMSVL(ICNT)= IVAL
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OMSTR
     I                  (RVAL)
C
C     + + + PURPOSE + + +
C     save an real value to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      REAL   RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RVAL   - value to save
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + END SPECIFICATIONS + + +
C
C     increment counter of values saved
      RCNT= RCNT+ 1
      IF (RCNT .LE. MXMSR) THEN
C       save value
        RMSVL(RCNT)= RVAL
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   OMSTC
     I                  (CLEN,CVAL)
C
C     + + + PURPOSE + + +
C     save character value to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CLEN
      CHARACTER*1 CVAL(CLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLEN   - length of character string
C     CVAL   - character string
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,CLEN
C       increment counter of values saved
        CCNT= CCNT+ 1
        IF (CCNT .LE. MXMSC) THEN
C         save value
          CMSVL(CCNT)= CVAL(I)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   OMSTD
     I                  (DATE)
C
C     + + + PURPOSE + + +
C     save a date to output with a hspf message
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DATE(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - date to save
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'phmsg.inc'
      INCLUDE 'chmsg.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYI
C
C     + + + END SPECIFICATIONS + + +
C
C     save date
      I = 5
      CALL COPYI (I,DATE,DATIM)
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   DAYVAL
     I                        (MVAL1,MVAL2,DAY,NDAYS)
C
C     + + + PURPOSE + + +
C     Linearly interpolate a value for this day (DAYVAL), given
C     values for the start of this month and next month (MVAL1 and
C     MVAL2).  ndays is the number of days in this month.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    DAY,NDAYS
      REAL       MVAL1,MVAL2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MVAL1  - ???
C     MVAL2  - ???
C     DAY    - day of month
C     NDAYS  - no. of days in this month
C
C     + + + LOCAL VARIABLES + + +
      REAL       RDAY,RNDAYS
C
C     + + + INTRINSICS + + +
      INTRINSIC  FLOAT
C
C     + + + END SPECIFICATIONS + + +
C
      RDAY  = FLOAT(DAY)
      RNDAYS= FLOAT(NDAYS)
      DAYVAL= MVAL1 + (MVAL2 - MVAL1)*(RDAY - 1)/RNDAYS
C
      RETURN
      END
C
C
C
      SUBROUTINE    NITMOV
     I                    (LPRZOT,MODID)
C
C     + + + PURPOSE + + +
C     Sets up the coefficient matrix for the solution of the
C     Creation date: 9/12/95 BRBicknell
C
C     + + + DUMMY ARGUMENTS + + +              
      INTEGER     LPRZOT
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LPRZOT - Fortran unit number for output file LPRZOT
C     MODID  - character string for output file identification
C
C     + + + PARAMETERS + + +   
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CNITR.INC'
      INCLUDE 'CMISC.INC'
C
C     + + + LOCAL VARIABLES + + +            
      INTEGER      I,K
      REAL         ENRICH,SLKGHA
      REAL*8       X(NCMPTS)
      CHARACTER*80 MESAGE
C
C     + + + FUNCTIONS + + +
      REAL         LNCHK
      REAL*8       EXPCHK
C
C     + + + INTRINSICS + + +
      INTRINSIC    REAL,DBLE
C
C     +  +  + EXTERNALS +  +  + 
      EXTERNAL SUBIN,SUBOUT,SLNIT,LNCHK,EXPCHK
C
C     + + + END SPECIFICATIONS + + +          
C
      MESAGE = 'NITMOV'
      CALL SUBIN(MESAGE)
C
C     ammonia
      K= 3
C     compute current concentrations (this may not be necessary)
      DO 10 I= 1, NCOM2
        CNIT(K,I)= NIT(K,I)/(THETO(I)*DELX(I)*1.0E5)
 10   CONTINUE
C
      CALL SLNIT
     I           (LPRZOT,MODID,K,
     O            X)
C
C     recalculate storages and compute current transport fluxes
      DO 20 I= 1, NCOM2
        CNIT(K,I)= X(I)
        NIT(K,I)= CNIT(K,I)*(THETN(I)*DELX(I)*1.0E5)
        PSAMS(I)= VEL(I)*X(I)*THETN(I)*1.0E5
        OSAMS(I)= OUTFLO(I)*X(I)*1.0E5
 20   CONTINUE
      OSAMS(1)= 0.0
      NCFX1(4,1)= RUNOF*X(1)
C     ? WHAT ABOUT ROOT ZONE FLUX
C     ? WHAT ABOUT TOTAL NCFX3 (I.E., LATERAL/INTERFLOW)
C
C
C     nitrate
      K= 4
C     compute current concentrations (this may not be necessary)
      DO 30 I= 1, NCOM2
        CNIT(K,I)= NIT(K,I)/(THETO(I)*DELX(I)*1.0E5)
 30   CONTINUE
C
      CALL SLNIT
     I           (LPRZOT,MODID,K,
     O            X)
C
C     recalculate storages and compute current transport fluxes
      DO 40 I= 1, NCOM2
        CNIT(K,I)= X(I)
        NIT(K,I)= CNIT(K,I)*(THETN(I)*DELX(I)*1.0E5)
        PSNO3(I)= VEL(I)*X(I)*THETN(I)*1.0E5
        OSNO3(I)= OUTFLO(I)*X(I)*1.0E5
 40   CONTINUE
      OSNO3(1)= 0.0
      NCFX1(5,1)= RUNOF*X(1)
C
C
C     labile organic N
      K= 6
C     compute current concentrations (this may not be necessary)
      DO 50 I= 1, NCOM2
        CNIT(K,I)= NIT(K,I)/(THETO(I)*DELX(I)*1.0E5)
 50   CONTINUE
C
      CALL SLNIT
     I           (LPRZOT,MODID,K,
     O            X)
C
C     recalculate storages and compute current transport fluxes
      DO 60 I= 1, NCOM2
        CNIT(K,I)= X(I)
        NIT(K,I)= CNIT(K,I)*(THETN(I)*DELX(I)*1.0E5)
        PSSLN(I)= VEL(I)*X(I)*THETN(I)*1.0E5
        OSSLN(I)= OUTFLO(I)*X(I)*1.0E5
 60   CONTINUE
      OSSLN(1)= 0.0
      NCFX1(6,1)= RUNOF*X(1)
C
C
C     refractory organic N
      K= 8
C     compute current concentrations (this may not be necessary)
      DO 70 I= 1, NCOM2
        CNIT(K,I)= NIT(K,I)/(THETO(I)*DELX(I)*1.0E5)
 70   CONTINUE
C
      CALL SLNIT
     I           (LPRZOT,MODID,K,
     O            X)
C
C     recalculate storages and compute current transport fluxes
      DO 80 I= 1, NCOM2
        CNIT(K,I)= X(I)
        NIT(K,I)= CNIT(K,I)*(THETN(I)*DELX(I)*1.0E5)
        PSSRN(I)= VEL(I)*X(I)*THETN(I)*1.0E5
        OSSRN(I)= OUTFLO(I)*X(I)*1.0E5
 80   CONTINUE
      OSSRN(1)= 0.0
      NCFX1(7,1)= RUNOF*X(1)
C
      IF (ERFLAG.EQ.1) THEN
C       handle erosion losses of particulates
C
C       calculate enrichment ratio
        SLKGHA= SEDL* 1000./AFIELD
        ENRICH= 2.0- (0.2* LNCHK(SLKGHA))
        ENRICH= REAL(EXPCHK(DBLE(ENRICH)))
C
C       adsorbed ammonium
        SEDN(2)= SEDL*ENRICH/(BD(1)*AFIELD*DELX(1)*100.)*NIT(2,1)
        NIT(2,1)= NIT(2,1) - SEDN(2)
C
C       particulate labile organic N
        SEDN(1)= SEDL*ENRICH/(BD(1)*AFIELD*DELX(1)*100.)*NIT(1,1)
        NIT(1,1)= NIT(1,1) - SEDN(1)
C
C       particulate refractory organic N
        SEDN(3)= SEDL*ENRICH/(BD(1)*AFIELD*DELX(1)*100.)*NIT(7,1)
        NIT(7,1)= NIT(7,1) - SEDN(3)
      ELSE
C       no erosion fluxes
        SEDN(1) = 0.0
        SEDN(2) = 0.0
        SEDN(3) = 0.0
      END IF
C
      CALL SUBOUT      
C
      RETURN
      END
C
C
C
      SUBROUTINE    SLNIT
     I                   (LPRZOT,MODID,K,
     O                    X)
C
C     + + + PURPOSE + + +
C     Sets up the coefficient matrix for the solution of the
C     soil transport equation for nitrogen species. It then  
C     calls an equation solver for the tridiagonal matrix.
C     Creation date: 9/13/95 BRB
C
C     + + + DUMMY ARGUMENTS + + +              
      INTEGER     K,LPRZOT
      REAL*8      X(200)
      CHARACTER*3 MODID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LPRZOT - Fortran unit number for output file LPRZOT
C     MODID  - character string for output file identification
C     K      - nitrogen species number being simulated (3,4,6,8)
C
C     + + + PARAMETERS + + +   
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CNITR.INC'
      INCLUDE 'CMISC.INC'
C
C     + + + LOCAL VARIABLES + + +            
      INTEGER      I
      REAL         KDNIT(8,NCMPTS)
      REAL*8       A(NCMPTS),B(NCMPTS),C(NCMPTS),F(NCMPTS)
      CHARACTER*80 MESAGE
C
C     +  +  + EXTERNALS +  +  + 
      EXTERNAL SUBIN,TRDIAG,SUBOUT
C
C     + + + END SPECIFICATIONS + + +          
C
      MESAGE = 'SLNIT'
      CALL SUBIN(MESAGE)
C
      DO 10 I= 1, NCOM2
        KDNIT(K,I)= 0.0
 10   CONTINUE
C
C     Set up coefficients for surface layer
C
      A(1)= 0.0
      B(1)= (VEL(1)*THETN(1)/DELX(1)
     1       +RUNOF/DELX(1))*DELT
     2       +THETN(1) + KDNIT(K,1)*BD(1)
C?   3       +(ELTERM(K))*DELT
      C(1)= 0.0
      F(1)= (THETO(1)+KDNIT(K,1)*BD(1))*CNIT(K,1)
C
C     Calculate coefficient of non-boundary soil layers
      DO 20 I=2,NCOM2M
        A(I)= (-VEL(I-1)*THETN(I-1)/DELX(I))*DELT
        B(I)= (VEL(I)*THETN(I)/DELX(I)
     1        +OUTFLO(I)/DELX(I)) *DELT
     2        +THETN(I) + KDNIT(K,I)*BD(I)
        C(I)= 0.0
        F(I)= (THETO(I)+KDNIT(K,I)*BD(I))*CNIT(K,I)
 20   CONTINUE
C
C     Calculate coefficients of bottom layer
C
      A(NCOM2)=(-VEL(NCOM2M)*THETN(NCOM2M)/DELX(NCOM2))*DELT
      B(NCOM2)= (VEL(NCOM2)*THETN(NCOM2)/DELX(NCOM2)
     1          + OUTFLO(NCOM2)/DELX(NCOM2))*DELT
     2          + THETN(NCOM2) + KDNIT(K,NCOM2)*BD(NCOM2)
      C(NCOM2)= 0.0
      F(NCOM2)= (THETO(NCOM2)+KDNIT(K,NCOM2)*BD(NCOM2))*CNIT(K,NCOM2)
C
C     Call equation solver
      CALL TRDIAG (A,B,C,X,F,NCOM2,LPRZOT,MODID)
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   NITECH
     I                   (LECHO,LMODID,SEPTON)
C
C     + + + PURPOSE + + +
C     Echo user input nitrogen simulation parameters.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LECHO
      CHARACTER*3 LMODID
      LOGICAL     SEPTON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LECHO  - local fortran unit number for file FECHO
C     LMODID - character string for output file identification
C     SEPTON - septic effluent on flag
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CNITR.INC'
      INCLUDE 'CSPTIC.INC'
      INCLUDE 'CMISC.INC'
      INCLUDE 'CPEST.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,APD,APM,ISTRT,IEND
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A3,/,1X,A3,/,1X,A3,' ',A78,/,1X,A3,/,1X,A3,1X,
     $        'NITROGEN SIMULATION FLAGS',/,1X,A3,1X,25('-'),/,1X,A3,/,
     $        1X,A3,1X,'VNUTFG - MONTHLY VARYING PLANT UPTAKE (0=NO, ',
     $        '1=YES)',T65,I5,/,1X,A3,1X,'FORAFG - AMMONIA ADS/DES ',
     $        'METHOD (0=1ST ORDER, 1=FREUNDLICH)',T65,I5,/,1X,A3,1X,
     $        'ITMAXA - MAX NUMBER ITERATIONS FOR FREUNDLICH SOLUTION',
     $        T65,I5,/,1X,A3,1X,'NUPTFG - PLANT UPTAKE OPTION (0=1ST ',
     $        'ORDER, 1=YIELD-BASED)',T65,I5,/,1X,A3,1X,'FIXNFG - ',
     $        'NITROGEN FIXATION (0=NO, 1=YES)',T65,I5,/,1X,A3,1X,
     $        'AMVOFG - AMMONIA VOLATILIZATION (0=NO, 1=YES)',T65,I5,/,
     $        1X,A3,1X,'ALPNFG - ABOVE-GROUND AND LITTER (0=NO, 1=YES)',
     $        T65,I5,/,1X,A3,1X,'VNPRFG - MONTHLY VARYING PLANT RETURN',
     $        ' (0=NO, 1=YES)',T65,I5)
 2010 FORMAT (1X,A3,/,1X,A3,1X,'SEPTIC EFFLUENT INTRODUCED INTO ',
     $        'HORIZON NUMBER',T65,I5,/,1X,A3,1X,'FRACTION OF ORGANIC',
     $        'WHICH BECOMES REFRACTORY',T65,F5.2)
 2011 FORMAT (1X,A3,1X,'SEPTIC EFFLUENT DATA-SET NUMBERS (H20, AMM, ',
     $        'NIT, ORG N)  ',4I5)
 2020 FORMAT (1X,A3,/,1X,A3,1X,'ATMOSPHERIC DEPOSITION FLAGS:',T40,
     $        'AMMONIA',T50,'NITRATE',T60,'ORGANIC N',/,1X,A3,T31,
     $        'DRY:',T40,I5,T50,I5,T60,I5,/,1X,A3,T31,'WET:',T40,I5,
     $        T50,I5,T60,I5)
 2030 FORMAT (1X,A3,/,1X,A3,1X,'AGRICULTURAL',T22,'APPLICATION',T35,
     $        'AMMONIA',T50,'NITRATE',T65,'ORGANIC N',T80,'REFRACTORY',
     $        /,1X,A3,1X,'APPLICATION',T22,'DATE',T35,'(KG/HA)',T50,
     $        '(KG/HA)',T65,'(KG/HA)',T80,'FRACTION')
 2031 FORMAT (1X,A3,T25,I2,1X,A4,', ',I2,T30,4(5X,G10.4))
 2040 FORMAT (1X,A3,1X,/,1X,A3,1X,'1ST ORDER PLANT UPTAKE RATES ',
     $        '(8 HORIZON VALUES PER LINE)')
 2041 FORMAT (1X,A3,1X,8G10.4)
 2050 FORMAT (1X,A3,1X,/,1X,A3,1X,'MONTHLY 1ST ORDER PLANT UPTAKE ',
     $        'RATES (PER HORIZON)',/,1X,A3,1X,50('-'),/,1X,A3,1X,
     $        'HZN   JAN   FEB   MAR   APR   MAY   JUN   JUL   AUG   ',
     $        'SEP   OCT   NOV   DEC')
 2051 FORMAT (1X,A3,1X,I3,1X,12F6.3)
 2060 FORMAT (1X,A3,/,1X,A3,1X,'YIELD-BASED UPTAKE TARGET',T40,G10.4,/,
     $        1X,A3,1X,'RATIO OF MAX UPTAKE TO TARGET',T40,G10.4)
 2070 FORMAT (1X,A3,/,1X,A3,1X,'MONTHLY FRACTIONS OF YIELD-BASED ',
     $        'UPTAKE  (JAN - DEC)',/,1X,A3,1X,12F6.3)
 2080 FORMAT (1X,A3,/,1X,A3,1X,'MONTHLY HORIZON FRACTIONS OF YIELD-',
     $        'BASED UPTAKE',/,1X,A3,1X,47('-'),/,1X,A3,1X,
     $        'HZN   JAN   FEB   MAR   APR   MAY   JUN   JUL   AUG   ',
     $        'SEP   OCT   NOV   DEC')
 2090 FORMAT (1X,A3,/,1X,A3,1X,'ABOVE-GROUND PLANT UPTAKE RATES ',
     $        '(8 HORIZON VALUES PER LINE)')
 2100 FORMAT (1X,A3,/,1X,A3,1X,'MONTHLY ABOVE-GROUND PLANT UPTAKE ',
     $        'RATES (PER HORIZON)',/,1X,A3,1X,53('-'),/,1X,A3,1X,
     $        'HZN   JAN   FEB   MAR   APR   MAY   JUN   JUL   AUG   ',
     $        'SEP   OCT   NOV   DEC')
 2110 FORMAT (1X,A3,/,1X,A3,1X,'GENERAL NITROGEN PARAMETERS',/,1X,A3,1X,
     $        27('-'),/,1X,A3,1X,'NITRATE FRACTION OF TOTAL N PLANT ',
     $        'UPTAKE',T50,G10.4,/,1X,A3,1X,'AMMONIUM FRACTION OF ',
     $        'TOTAL N PLANT UPTAKE',T50,G10.4,/,1X,A3,1X,'TEMPERATURE',
     $        ' COEFFICIENTS FOR FIRST ORDER:',/,1X,A3,3X,'PLANT ',
     $        'UPTAKE',T50,G10.4,/,1X,A3,3X,'AMMONIA DESORPTION',T50,
     $        G10.4,/,1X,A3,3X,'AMMONIA ADSORPTION',T50,G10.4,/,1X,A3,
     $        3X,'NITRATE IMMOBILIZATION',T50,G10.4,/,1X,A3,3X,'ORG N ',
     $        'AMMONIFICATION',T50,G10.4,/,1X,A3,3X,'DENITRIFICATION',
     $        T50,G10.4,/,1X,A3,3X,'NITRIFICATION',T50,G10.4,/,1X,A3,3X,
     $        'AMMONIA IMMOBLIZATION',T50,G10.4)
 2120 FORMAT (1X,A3,/,1X,A3,1X,'FIRST ORDER REACTION RATES (PER ',
     $        'HORIZON)',/,1X,A3,1X,40('-'),/,1X,A3,1X,'HOR-  AMMONIUM',
     $        '  AMMONIUM   NITRATE  AMMONIF- DENITRIF- DENITRIF.  ',
     $        'NITRIF-  AMMONIUM',/,1X,A3,1X,'IZON   DESORP.   ',
     $        'ADSORP.  IMMOBIL.  ICATION   ICATION  THRESHOLD  ',
     $        'ICATION  IMMOBIL.')
 2121 FORMAT (1X,A3,1X,I3,1X,8G10.4)
 2130 FORMAT (1X,A3,/,1X,A3,1X,'SINGLE VALUE FREUNDLICH PARAMETERS',
     $        /,1X,A3,1X,34('-'),/,1X,A3,1X,'MAX SOLUBILITY ',
     $        'OF AMMONIUM IN WATER',T50,G10.4,/,1X,A3,/,1X,A3,1X,
     $        'HOR-     FIXED     FREUNDLICH',/,1X,A3,1X,'IZON   ',
     $        'AMMONIUM        K           1/N1')
 2131 FORMAT (1X,A3,1X,I3,1X,3(2X,G10.4))
 2140 FORMAT (1X,A3,/,1X,A3,1X,'AMMONIA VOLATILIZATION PARAMETERS',/,
     $        1X,A3,1X,33('-'),/,1X,A3,1X,'TEMPERATURE CORRECTION ',
     $        'COEFFICIENT',T50,G10.4,/,1X,A3,1X,'REFERENCE ',
     $        'TEMPERATURE FOR CORRECTION',T50,G10.4,/,1X,A3,/,1X,A3,
     $        1X,'FIRST ORDER RATES (8 HORIZON VALUES PER LINE)')
 2150 FORMAT (1X,A3,/,1X,A3,1X,'ORGANIC NITROGEN PARAMETERS',/,1X,A3,1X,
     $        29('-'),/,1X,A3,10X,'PARTITION COEFFICIENTS:',6X,
     $        'LABILE/REFRACTORY CONVERSION:',/,1X,A3,1X,'HORIZON    ',
     $        'LABILE    REFRACTORY          RATE     TEMP COEFF.')
 2151 FORMAT (1X,A3,1X,I5,2X,2(2X,G10.4),6X,2(2X,G10.4))
 2160 FORMAT (1X,A3,/,1X,A3,1X,'BELOW-GROUND PLANT RETURN RATES',/,1X,
     $        A3,1X,31('-'),/,1X,A3,1X,'FRACTION TO REFRACTORY',
     $        T50,G10.4,/,1X,A3,/,1X,A3,1X,'FIRST ORDER RATES (8 ',
     $        'HORIZON VALUES PER LINE)')
 2170 FORMAT (1X,A3,/,1X,A3,1X,'ABOVE-GROUND PLANT RETURN RATES',/,1X,
     $        A3,1X,31('-'),/,1X,A3,1X,'FIRST ORDER ABOVE-GROUND ',
     $        'RETURN RATE',T50,G10.4,/,1X,A3,1X,'FIRST ORDER ',
     $        'LITTER RETURN RATE (TO 1ST HORIZON)',T50,G10.4,/,1X,A3,
     $        1X,'FRACTION TO REFRACTORY',T50,G10.4)
 2180 FORMAT (1X,A3,/,1X,A3,1X,'MONTHLY BELOW-GROUND RETURN RATES ',
     $        '(PER HORIZON)',/,1X,A3,1X,47('-'),/,1X,A3,1X,
     $        'HZN   JAN   FEB   MAR   APR   MAY   JUN   JUL   AUG   ',
     $        'SEP   OCT   NOV   DEC')
 2190 FORMAT (1X,A3,/,1X,A3,1X,'MONTHLY FRACTIONS TO REFRACTORY FOR ',
     $        'BELOW-GROUND PLANT RETURN (JAN - DEC)',/,1X,A3,1X,12F6.3)
 2200 FORMAT (1X,A3,/,1X,A3,1X,'MONTHLY ABOVE-GROUND PLANT TO LITTER ',
     $        'RETURN RATES (JAN - DEC)',/,1X,A3,1X,12F6.3)
 2210 FORMAT (1X,A3,/,1X,A3,1X,'MONTHLY LITTER RETURN RATES TO FIRST ',
     $        'HORIZON (JAN - DEC)',/,1X,A3,1X,12F6.3)
 2220 FORMAT (1X,A3,/,1X,A3,1X,'MONTHLY FRACTIONS TO REFRACTORY FOR ',
     $        'LITTER RETURN (JAN - DEC)',/,1X,A3,1X,12F6.3)
 2230 FORMAT (1X,A3,/,1X,A3,1X,'INITIAL STORAGES OF NITROGEN ',
     $        'CONSTITUENTS BY HORIZON',/,1X,A3,1X,52('-'),/,1X,A3,1X,
     $        'HOR-  <--LABILE ORANIC N--->  <REFRACTORY ORGANIC N>   ',
     $        ' ADSORBED   SOLUTION               PLANT',/,1X,A3,1X,
     $        'IZON  PARTICULATE   SOLUTION  PARTICULATE   SOLUTION   ',
     $        ' AMMONIUM   AMMONIUM     NITRATE   NITROGEN')
 2231 FORMAT (1X,A3,1X,I4,1X,4(2X,G10.4,1X,G10.4))
 2240 FORMAT (1X,A3,/,1X,A3,1X,'INITIAL STORAGES OF ABOVE-GROUND ',
     $        'NITROGEN',/,1X,A3,1X,41('-'),/,1X,A3,1X,'ABOVE-',
     $        'GROUND PLANT NITROGEN',T50,G10.4,/,1X,A3,1X,'LITTER ',
     $        'NITROGEN',T50,G10.4)
C
C     + + + END SPECIFICATIONS + + +
C
C     nitrogen header
      WRITE (LECHO,2000) (LMODID,I=1,3),NTITLE,(LMODID,I=1,5),VNUTFG,
     $                   LMODID,FORAFG,LMODID,ITMAXA,LMODID,NUPTFG,
     $                   LMODID,FIXNFG,LMODID,AMVOFG,LMODID,ALPNFG,
     $                   LMODID,VNPRFG
C
      IF (SEPTON) THEN
C       septic effluent introduction depth
        WRITE (LECHO,2010) LMODID,LMODID,SEPHZN,LMODID,ORGRFC
        IF (SEPDSN(1).GT.0) THEN
          WRITE (LECHO,2011) LMODID,SEPDSN
        END IF
      END IF
C
      WRITE (LECHO,2020) (LMODID,I=1,3),(NIADFG(I),I=1,3),
     $                    LMODID,(NIADFG(I),I=4,6)
C
      IF (NAPS.GT.0) THEN
C       application information
        WRITE (LECHO,2030) (LMODID,I=1,3)
        DO 20 I = 1,NAPS
          LEAP = 1
          IF (MOD(IAPYR(I),4) .EQ. 0) LEAP = 2
          DO 10 J = 1,12
            IF (IAPDY(I) .GT. CNDMO(LEAP,J) .AND. IAPDY(I) .LE.
     $        CNDMO(LEAP,J+1)) APM = J
 10       CONTINUE
          APD = IAPDY(I) - CNDMO(LEAP,APM)
          WRITE(LECHO,2031) LMODID,APD,CMONTH(APM),IAPYR(I),TAPP(1,I),
     $                      TAPP(2,I),TAPP(3,I),NAPFRC(I)
 20     CONTINUE
      END IF
C
      IF (NUPTFG.EQ.0) THEN
C       first order plant uptake being used
        IF (VNUTFG.EQ.0) THEN
C         nitrogen plant uptake rate parameters do not vary
          WRITE (LECHO,2040) LMODID,LMODID
          DO 30 ISTRT = 1, NHORIZ, 8
C           write up to 8 horizon values per record
            IEND = ISTRT + 7
            IF (IEND.GT.NHORIZ) IEND = NHORIZ
            WRITE (LECHO,2041) LMODID,(KPLN(I),I=ISTRT,IEND)
 30       CONTINUE
        ELSE
C         nitrogen plant uptake rate parameters vary throughout year
          WRITE (LECHO,2050) (LMODID,I=1,4)
          DO 40 I = 1,NHORIZ
C           write monthly values for each horizon
            WRITE (LECHO,2051) LMODID,I,(KPLNM(J,I),J=1,12)
 40       CONTINUE
        END IF
      ELSE IF (NUPTFG.EQ.1) THEN
C       yield-based plant uptake
        WRITE (LECHO,2060) LMODID,LMODID,NUPTGT,LMODID,NMXRAT
C       monthly fractions for yield-based plant uptake of nitrogen
        WRITE (LECHO,2070) (LMODID,I=1,3),(NUPTFM(I),I=1,12)
C       monthly fractions of plant uptake per soil layer
        WRITE (LECHO,2080) (LMODID,I=1,4)
        DO 50 I = 1,NHORIZ
C         write monthly values for each horizon
          WRITE (LECHO,2051) LMODID,I,(NUPTM(J,I),J=1,12)
 50     CONTINUE
      END IF
C
      IF (ALPNFG.EQ.1) THEN
C       above ground plant nitrogen being simulated
        IF (VNUTFG.EQ.0) THEN
C         above ground plant uptake fractions do not vary
          WRITE (LECHO,2090) LMODID,LMODID
          DO 60 ISTRT = 1, NHORIZ, 8
C           read up to 8 horizon values per record
            IEND = ISTRT + 7
            IF (IEND.GT.NHORIZ) IEND = NHORIZ
            WRITE (LECHO,2041) LMODID,(ANUTF(I),I=ISTRT,IEND)
 60       CONTINUE
        ELSE
C         above ground plant uptake fractions vary throughout year
          WRITE (LECHO,2100) (LMODID,I=1,4)
          DO 70 I = 1,NHORIZ
C           get monthly values for each horizon
            WRITE (LECHO,2051) LMODID,I,(ANUFM(J,I),J=1,12)
 70       CONTINUE
        END IF
      END IF
C
C     first order general parameters
      WRITE (LECHO,2110) (LMODID,I=1,4),GNPM(1),LMODID,GNPM(2),LMODID,
     $                   (LMODID,GNPM(I),I=3,10)
C     first order reaction parameters for all horizons
      WRITE (LECHO,2120) (LMODID,I=1,5)
      DO 80 I = 1,NHORIZ
C       write values for each horizon
        WRITE (LECHO,2121) LMODID,I,(NPM(J,I),J=1,5),DNTHRS(I),
     $                     NPM(6,I),NPM(7,I)
 80   CONTINUE
C
      IF (FORAFG.EQ.1) THEN
C       single value Freundlich isotherm method for ammonium adsorp/desorp
C       max solubility of ammonium in water
        WRITE (LECHO,2130) (LMODID,I=1,4),GNPM(11),(LMODID,I=1,3)
C       adsorption/desorption parameters
        DO 90 I = 1,NHORIZ
C         write values for each horizon
          WRITE (LECHO,2131) LMODID,I,NPM(8,I),NPM(10,I),NPM(11,I)
 90     CONTINUE
      END IF
C
      IF (AMVOFG.EQ.1) THEN
C       ammonia volatilization parameters
        WRITE(LECHO,2140) (LMODID,I=1,4),THVOL,LMODID,TRFVOL,
     $                     LMODID,LMODID
        DO 100 ISTRT = 1, NHORIZ, 8
C         read up to 8 horizon values per record
          IEND = ISTRT + 7
          IF (IEND.GT.NHORIZ) IEND = NHORIZ
C         write horizon values
          WRITE (LECHO,2041) LMODID,(KVOL(I),I=ISTRT,IEND)
 100    CONTINUE
      END IF
C
C     organic nitrogen transformation parameters
      WRITE (LECHO,2150) (LMODID,I=1,5)
      DO 110 I = 1,NHORIZ
C       write values for each horizon
        WRITE (LECHO,2151) LMODID,I,(ORNPM(J,I),J=1,4)
 110  CONTINUE
C
      IF (VNPRFG.EQ.0) THEN
C       plant return rates do not vary
C       below ground return rates for all horizons
        WRITE (LECHO,2160) (LMODID,I=1,4),BGNPRF,LMODID,LMODID
        DO 120 ISTRT = 1, NHORIZ, 8
C         write up to 8 horizon values per record
          IEND = ISTRT + 7
          IF (IEND.GT.NHORIZ) IEND = NHORIZ
C         more horizon values to write
          WRITE (LECHO,2041) LMODID,(KRETBN(I),I=ISTRT,IEND)
 120    CONTINUE
C       above ground return rate for top horizon
        WRITE (LECHO,2170) (LMODID,I=1,4),AGKPRN,LMODID,KRETAN(1),
     $                      LMODID,LINPRF
      ELSE
C       plant nitrogen return rates vary throughout the year
C       below ground plant return rates
        WRITE (LECHO,2180) (LMODID,I=1,4)
        DO 130 I = 1,NHORIZ
C         write monthly values for each horizon
          WRITE (LECHO,2051) LMODID,I,(KRBNM(J,I),J=1,12)
 130    CONTINUE
C       refractory fractions for below ground plant N return
        WRITE (LECHO,2190) (LMODID,I=1,3),(BNPRFM(I),I=1,12)
C       above ground plant N return rates to litter N
        WRITE (LECHO,2200) (LMODID,I=1,3),(KRANM(I),I=1,12)
C       litter N return rate to 1st horizon
        WRITE (LECHO,2210) (LMODID,I=1,3),(KRLNM(I),I=1,12)
C       refractory fractions for litter N return
        WRITE (LECHO,2220) (LMODID,I=1,3),(LNPRFM(I),I=1,12)
      END IF
C
C     initial storages of constituents
      WRITE (LECHO,2230) (LMODID,I=1,5)
      DO 140 I = 1,NHORIZ
C       write values for each horizon
        WRITE (LECHO,2231) LMODID,I,NIT(1,I),(NIT(J,I),J=6,8),
     $                                       (NIT(J,I),J=2,5)
 140  CONTINUE
      IF (ALPNFG.EQ.1) THEN
C       initial storage of above ground plant and litter N
        WRITE (LECHO,2240) (LMODID,I=1,4),AGPLTN,LMODID,LITTRN
      END IF
C
      RETURN
      END
