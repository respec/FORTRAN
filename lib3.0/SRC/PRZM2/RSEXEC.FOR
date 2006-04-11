C
C
C
      SUBROUTINE   EXESUP
     I                   (MCARLO,PRZMON,YRSTEP,VADFON,TRNSIM,
     I                    SEPTON,NITRON,SRNFG,IDNODE,MODID,
     I                    NPZONE,NVZONE,
     I                    ISDAY,ISMON,ISTYR,
     I                    NLDLT,NTSAFT,LLSTS,
     I                    NCHEM,IPARNT,IRUN,NRUNS)
C
C     + + + PURPOSE + + +
C     called from MAIN to control simulation calls
C     Modification date: 7/2/92 JAM
C
      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PMXZON.INC'
C
C     + + + DUMMY ARGUMENTS + + +
C
      LOGICAL      MCARLO,PRZMON,YRSTEP,VADFON,TRNSIM,SEPTON,NITRON
      INTEGER      ISDAY,ISMON,ISTYR,
     1             NLDLT,NTSAFT,LLSTS,NPZONE,NVZONE,
     2             IRUN,NRUNS,NCHEM,IPARNT(3),
     3             SRNFG,IDNODE(MXZONE)
      CHARACTER*3  MODID(NMXFIL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MCARLO - monte carlo on flag
C     PRZMON - przm on flag
C     VADFON - vadoft flow on flag
C     YRSTEP - run PRZM at annual increment flag
C     TRNSIM - vadoft transport on flag
C     SEPTON - septic effluent on flag
C     NITRON - nitrogen modeling on flag
C     MODID  - model id
C     ISDAY  - starting day of simulation
C     ISMON  - starting month of simulation
C     ISTYR  - starting year of simulation
C     NLDLT  - number of days in a vadoft time step
C     NTSAFT - number of time steps in simlation
C     LLSTS  - number of days left in final time step
C     NPZONE - number of przm zones
C     NVZONE - number of vadoft zones
C     IRUN   - current number of run
C     NRUNS  - total number of runs
C     NCHEM  - number of chemicals applied
C     IPARNT - chemical number corresponding to parent 
C     SRNFG  - starting run flag
C     IDNODE - base node for PRZM for transfer to top of VADOFT
C
C     + + + PARAMETERS + + +
C      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXTIM.INC'
      INCLUDE 'PMXOWD.INC'
C      INCLUDE 'PMXZON.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CVMISC.INC'
      INCLUDE 'CTRNBC.INC'
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CADISC.INC'
      INCLUDE 'CCUMUL.INC'
      INCLUDE 'CFILEX.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       CNDM1(2,12),IFIL,LEAP,
     2              ITSAFT,IHOLD,
     3              IDAY0,IMON0,IYR0,NDAYS,NUDAY,NUMON,NUYR,
     4              M,I,LNCHEM,LPARNT(3)
      CHARACTER*80  MESAGE,SIMTYM
      LOGICAL       FLOSIM
      CHARACTER*2   VDUM
C
C     + + + FUNCTIONS + + +
      INTEGER       DYJDY
C
C     + + + EXTERNALS + + +
      EXTERNAL      SUBIN,ENDDAY,PZDSPL,XPRZM,XFLOW,DYJDY,
     1              XTRANS,DONBAR,SUBOUT,VADGET,VADPUT,OUTTAB
C
C     + + + INTRINSICS + + +
      INTRINSIC     MOD
C
C     + + + DATA INITIALIZATIONS + + +
        DATA  CNDM1/31,31,28,29,31,31,30,30,31,31,30,30,
     1          31,31,31,31,30,30,31,31,30,30,31,31/
C
C     + + + OUTPUT FORMATS + + +
C
 2200 FORMAT(I2)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'EXESUP'
      CALL SUBIN(MESAGE)
C
C     initialize
      TMVEX1 = 0.0
      TMVEX2 = 0.0
      TMACCW = 0.0
      TMACCP = 0.0
      TMDCYV = 0.0
      NLDLT  = 31
      DO 9 I = 1,NVZONE
        ANNCCW(I) = 0.0
        ANNWIF(I) = 0.0
        ANNWEF(I) = 0.0
        DO 10 M = 1,3
          ANNMEF(I,M) = 0.0
          ANNCYV(I,M) = 0.0
          ANNMIF(I,M) = 0.0
          ANNCCP(I,M) = 0.0
10      CONTINUE
 9    CONTINUE
C
      IF (NPZONE .GT. NVZONE) THEN
        I = NPZONE
      ELSE
        I = NVZONE
      ENDIF
C
C     loop through month periods
      DO 100 ITSAFT = 1, NTSAFT
C       calculate start and end dates
        IF (ITSAFT .EQ. 1) THEN
C         first time thru
          IDAY0 = ISDAY
          IMON0 = ISMON
          IYR0  = ISTYR
        ELSE
C         carry on where we left off
          IDAY0 = NUDAY
          IMON0 = NUMON
          IYR0  = NUYR
        ENDIF
C
C       check for leap year
        IF (MOD(IYR0,4).EQ.0 .AND. MOD(IYR0,100).NE.0) THEN
C         leap year
          LEAP = 2
        ELSE
C         not leap year
          LEAP = 1
        END IF
C
        IF (VADFON .OR. .NOT. YRSTEP) THEN
C         number of days this month
          NLDLT = CNDM1(LEAP,IMON0) - IDAY0 + 1
        ELSE
C         number of days this year
          NLDLT = DYJDY(IYR0,12,31)- DYJDY(IYR0,IMON0,IDAY0)+ 1
        ENDIF
C
        IF ((ITSAFT .EQ. NTSAFT) .AND. (LLSTS .NE. 0)) THEN
          NDAYS = LLSTS - 1
        ELSE
          NDAYS = NLDLT - 1
        ENDIF
C     
        CALL ENDDAY(
     I              IDAY0, IMON0, IYR0, NDAYS,
     O              NUDAY, NUMON, NUYR, SIMTYM)
C         
        IF (.NOT. MCARLO) THEN
          MESAGE = 'Simulating from ' // SIMTYM(1:22)
          IHOLD = ECHOLV
          ECHOLV = 3
C         write time, regardless of selected echo level
          CALL PZDSPL(FECHO,MESAGE)
          ECHOLV = IHOLD
        ENDIF
C         
        IF (PRZMON) THEN
C         do przm simulation
          CALL XPRZM(
     I               MCARLO,SEPTON,NITRON,NDAYS+1,NCHEM,ITSAFT,
     I               IYR0,IMON0,IDAY0,NUYR,NUMON,NUDAY,SIMTYM,
     I               MODID,SRNFG,IDNODE,NPZONE)
        ENDIF
C
C    ADDED TO OUTPUT SUMMARY TABLE, ATC, MOUNTAIN VIEW (8/93)
C
              IF (ITSAFT.EQ.NTSAFT) THEN
                 CALL OUTTAB
              ENDIF
C
        IF (VADFON) THEN
C         flow simulation
          DO 90 I = 1,NVZONE
C           loop for each zone
            IF (TRNSIM) THEN
C             rewind tape 10 file
              IFIL = FVTP10(I)
              REWIND IFIL
C             rewind vadoft transport restart file
              IFIL = FVRSTT(I)
              REWIND IFIL
            ENDIF
C           rewind vadoft flow restart file
            IFIL = FVRSTF(I)
            REWIND IFIL
            FLOSIM = .TRUE.
C           display VADOFT time step to screen and echo file
            WRITE(VDUM,2200) I
            MESAGE = 'Running VADOFT from ' // SIMTYM(1:22) // 
     1               ' for zone [ ' // VDUM // ' ]'
            CALL PZDSPL(FECHO,MESAGE)
C           read vadoft input for xflow from restart file
            CALL VADGET(FVRSTF(I),I,FLOSIM)
            CALL XFLOW(
     I               PRZMON,TRNSIM,MCARLO,
     I               NPZONE,NDAYS+1,ITSAFT,SIMTYM,I)
C           rewind vadoft flow restart file
            REWIND IFIL
            CALL VADPUT(FVRSTF(I),I,FLOSIM)
C
C           transport simulations
            IF (TRNSIM) THEN
              FLOSIM = .FALSE.
              IFIL = FVTP10(I)
              REWIND IFIL
              IFIL = FVRSTT(I)
              REWIND IFIL
              IF (NITRON) THEN
C               simulate three nitrogen constituents
                LNCHEM = 3
                LPARNT(1) = 0
                LPARNT(2) = 0
                LPARNT(3) = 0
              ELSE
C               simulate chemical(s) in execution supervisor
                LNCHEM = NCHEM
                LPARNT(1) = IPARNT(1)
                LPARNT(2) = IPARNT(2)
                LPARNT(3) = IPARNT(3)
              END IF
C
C             read vadoft input for xtrans from restart file
              CALL VADGET(FVRSTT(I),I,FLOSIM)
              CALL XTRANS(
     I                    PRZMON,TRNSIM,MCARLO,
     I                    NPZONE,NDAYS+1,LNCHEM,ITSAFT,LPARNT,
     I                    I,SIMTYM)
              REWIND IFIL
              CALL VADPUT(FVRSTT(I),I,FLOSIM)
            ENDIF
  90      CONTINUE
        ENDIF
C
C       global mass balance
        CALL ENDDAY(
     I              IDAY0,IMON0,IYR0,NDAYS+1,
     O              NUDAY,NUMON,NUYR,SIMTYM)
C
        IF (MCARLO) THEN
C         show percent complete
          IF (NRUNS .LE.  5) THEN
            CALL DONBAR(1,NTSAFT*NRUNS,ITSAFT+(IRUN-1)*NTSAFT)
          ENDIF
        ELSE
          CALL DONBAR(1,NTSAFT,ITSAFT)
        ENDIF
C    
 100  CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
C
      SUBROUTINE   ENDDAY
     I                   (IDAY0,IMON0,IYR0,NDAYS,
     O                    NUDAY,NUMON,NUYR,SIMTYM)
C
C     +  +  + PURPOSE +  +  +
C     used to determine JULDAY, simulation time
C     Modification date: 2/7/92 JAM
C
C     +  +  + DUMMY ARGUMENTS +  +  +
C
      INTEGER      IDAY0,IMON0,IYR0,NDAYS,NUDAY,NUMON,NUYR
      CHARACTER*80 SIMTYM
C
C     +  +  + ARGUMENT DEFINITIONS +  +  +
C
C     IDAY0  - incoming day
C     IMON0  - incoming month
C     IYR0   - incoming year
C     NDAYS  - number of days in time step
C     NUDAY  - new day
C     NUMON  - new month
C     NUYR   - new year
C     SIMTYM - simulation time for each time step
C
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      INTEGER*4   CNDMO(2,13),JDAY0,JDAYN,LEAP,I
      CHARACTER*3 MONTH(12)
C
C     +  +  + INTRINSICS +  +  +
C
      INTRINSIC   MOD
C
C     +  +  + DATA INITIALIZATIONS +  +  +
C
      DATA  CNDMO/0,0,31,31,59,60,90,91,120,121,151,152,181,182,
     1          212,213,243,244,273,274,304,305,334,335,365,366/
      DATA  MONTH/ 'Jan','Feb','Mar','Apr','May','Jun',
     1             'Jul','Aug','Sep','Oct','Nov','Dec'/
C
C     +  +  + OUTPUT FORMATS +  +  +
C
 2000 FORMAT(I2,1X,A3,1X,I2,' to ',I2,1X,A3,1X,I2)
C
C     +  +  + END SPECIFICATIONS +  +  +
C
      NUYR = IYR0
C
C     find year and Julian day
      IF ((MOD(NUYR,4) .EQ. 0) .AND. (MOD(NUYR,100) .NE. 0)) THEN
        LEAP = 2
      ELSE
        LEAP = 1
      ENDIF
      JDAY0 = IDAY0 + CNDMO(LEAP,IMON0)
      JDAYN = JDAY0 + NDAYS
C
 10   CONTINUE
      IF (JDAYN .GT. CNDMO(LEAP,13)) THEN
        JDAYN = JDAYN - CNDMO(LEAP,13)
        NUYR  = NUYR + 1
        IF ((MOD(NUYR,4) .EQ. 0) .AND. (MOD(NUYR,100) .NE. 0)) THEN
          LEAP = 2
        ELSE
          LEAP = 1
        ENDIF
        GO TO 10
      ENDIF
C
C     find month
      I = 13
 20   CONTINUE
      IF (JDAYN .LE. CNDMO(LEAP,I)) THEN
        I = I - 1
        GO TO 20
      ENDIF
C
      NUMON = I
      NUDAY = JDAYN - CNDMO(LEAP,NUMON)
C
      WRITE(SIMTYM,2000) IDAY0, MONTH(IMON0), IYR0,
     1                   NUDAY, MONTH(NUMON), NUYR
C
      RETURN
      END
C
C
C
      SUBROUTINE   XPRZM(
     I                   MCARLO,SEPTON,NITRON,NLDLT,NCHEM,ITSAFT,
     I                   IYR0,IMON0,IDAY0,NUYR,NUMON,NUDAY,SIMTYM,
     I                   MODID,SRNFG,IDNODE,NPZONE)
C
C     + + + PURPOSE + + +
C     przm execution
C     Modfication date: 2/18/92 JAM
C
      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PMXZON.INC'
C
C     + + + DUMMY ARGUMENTS + + +
C
      LOGICAL      MCARLO,SEPTON,NITRON
      INTEGER      NLDLT,NCHEM,ITSAFT,
     2             IYR0,IMON0,IDAY0,
     3             SRNFG,NPZONE,IDNODE(MXZONE),
     4             NUDAY,NUMON,NUYR
      CHARACTER*80 SIMTYM
      CHARACTER*3  MODID(NMXFIL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MCARLO - monte carlo on flag
C     SEPTON - septic effluent on flag
C     NITRON - nitrogen modeling on flag
C     NLDLT  - maximum number of days in a VADOFT time step (31)
C     NCHEM  - number of chemicals applied
C     ITSAFT - current number of simulation run
C     IYR0   - beginning year for current time step
C     IMON0  - beginning month for current time step
C     IDAY0  - beginning day for current time step
C     SRNFG  - starting run flag 
C     NPZONE - number of przms
C     IDNODE - base node of PRZM
C     SIMTYM - displays current simualtion dates
C     MODID  - model id (pest,conc,water)
C     NUYR   - ending year for current time step
C     NUMON  - ending month for current time step
C     NUDAY  - ending day for current time step
C
C     + + + PARAMETERS + + +
C      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXPDT.INC'
C      INCLUDE 'PMXZON.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CPRZST.INC'
      INCLUDE 'CFILEX.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RSTFG,RSDAT(3),REDAT(3),
     1             BASEND,ICHEM,ILDLT,IPZONE,K,LCHEM
      CHARACTER*80 MESAGE
      CHARACTER*2  CDUM
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,PZDSPL,PRZM,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' EXESUP: PRZM WATER FLUX, CHEM FLUX 1, 2, 3')
 2100 FORMAT(' ',I5,4G10.3)
 2200 FORMAT(I2)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'XPRZM'
      CALL SUBIN(MESAGE)
C
      IF (ITSAFT .EQ. 1) THEN
        RSTFG = 1
      ELSE
        RSTFG = 2
      ENDIF
C
C     initialize PRZM input
      RSDAT(1) = IYR0
      RSDAT(2) = IMON0
      RSDAT(3) = IDAY0
      REDAT(1) = NUYR
      REDAT(2) = NUMON
      REDAT(3) = NUDAY
C       
C     loop through root zones
      DO 50 IPZONE = 1, NPZONE
        WRITE(CDUM,2200)IPZONE
        MESAGE = 'Running PRZM from ' // SIMTYM(1:22) // '  for zone ['
     1    // CDUM // ']'
        CALL PZDSPL(FECHO,MESAGE)
C       base node index for retrieving TOTFLX
        BASEND = IDNODE(IPZONE)
        IF (NITRON) THEN
C         simulate 3 nitrogen constituents
          LCHEM = 3
        ELSE
C         simulate number of chemicals in execution supervisor
          LCHEM = NCHEM
        END IF
C       zero flux array
        DO 20 ILDLT = 1, NLDLT
          PRZMWF(IPZONE,ILDLT) = 0.0
          DO 10 ICHEM = 1, LCHEM
            PRZMPF(IPZONE,ILDLT,ICHEM) = 0.0
 10       CONTINUE
 20     CONTINUE
        CALL PRZM(
     I            RSTFG,NMXFIL,MCARLO,SEPTON,NITRON,
     I            MODID,RSDAT,REDAT,FPRZRS(IPZONE),
     I            FPRZOT(IPZONE),FPRZIN(IPZONE),FWDMS,
     I            FMETEO(IPZONE),FSPTIC(IPZONE),FNITAD(IPZONE),
     I            FTMSRS(IPZONE),SRNFG,BASEND,IPZONE,ITSAFT,NLDLT)
        IF (ECHOLV .GE. 7) THEN
C         echo przm fluxes
          WRITE(FECHO,2000)
        ENDIF
C         
        DO 30 ILDLT = 1, NLDLT
          IF (ECHOLV .GE. 7) THEN
            WRITE(FECHO,2100) ILDLT, PRZMWF(IPZONE,ILDLT),
     1      (PRZMPF(IPZONE,ILDLT,K),K=1,NCHEM)
          ENDIF
 30     CONTINUE
C
 50   CONTINUE
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   XFLOW
     I                  (PRZMON,TRNSIM,MCARLO,
     I                   NPZONE,NLDLT,ITSAFT,SIMTYM,IVZONE)
C
C     + + + PURPOSE + + +
C     vadoft flow execution
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      LOGICAL      PRZMON,TRNSIM,MCARLO
      INTEGER      NPZONE,NLDLT,ITSAFT,IVZONE
      CHARACTER*80 SIMTYM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PRZMON - przm on flag
C     TRNSIM - vadoft transport on flag
C     MCARLO - monte carlo on flag
C     NPZONE - number of przm zones
C     NLDLT  - maximum number of days in a time step (31)
C     ITSAFT - current time step
C     SIMTYM - current date of simualtion
C     IVZONE - current zone number
C
C     + + + PARAMETERS + + +
      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PMXPDT.INC'
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXZON.INC'
      INCLUDE 'PMXOWD.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CPRZST.INC'
      INCLUDE 'CVADST.INC'
      INCLUDE 'CZNWHT.INC'
      INCLUDE 'CIN2VD.INC'
      INCLUDE 'CFILEX.INC'
      INCLUDE 'CVMISC.INC'
      INCLUDE 'CVNTR1.INC'
      INCLUDE 'CCONTR.INC'
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL      FLOSIM,MOUTFL
      INTEGER      ICHEM,IPZONE,ILDLT,INODE,I,PARENT
      REAL         WGHT,SAVTMP(MXNOD)
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,PZDSPL,VADOFT,MCVAD,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Running VADOFT flow simulation, zone [',I2,']')
 2010 FORMAT(' EXESUP: NODE, PINT'/(I10,G10.3))
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'XFLOW'
      CALL SUBIN(MESAGE)
C
C     For flow, number of chemicals is always 1.
      ICHEM  = 1
C
      FLOSIM = .TRUE.
      IF (PRZMON) THEN
C                 
C       zero top flux array before summing up PRZM contributions
        DO 10 ILDLT = 1, NLDLT
          TOPFLX(ILDLT) = 0.0
 10     CONTINUE
C
        DO 30 IPZONE = 1, NPZONE
          WGHT = P2VWHT(IPZONE,IVZONE)
C         skip if no contribution from PRZM zone IPZONE
C
          IF (WGHT .GT. 0.0) THEN
            DO 20 ILDLT=1,NLDLT
              TOPFLX(ILDLT) = TOPFLX(ILDLT) +
     1          WGHT * PRZMWF(IPZONE,ILDLT)
 20         CONTINUE
          ENDIF
 30     CONTINUE
C
      ENDIF
C
      IF (ITSAFT .NE. 1) THEN
        DO 40 INODE = 1, NPV
          PINT(INODE) = SAVHED(IVZONE,INODE)
 40     CONTINUE
      ELSE
        DO 50 INODE = 1, NPV
          PINT(INODE) = CPINT(INODE,1)
 50     CONTINUE
      ENDIF
      IF (ECHOLV .GE. 8) THEN
        WRITE(FECHO,2010) (I,PINT(I),I=1,NPV)
      ENDIF
C
      WRITE(MESAGE,2000) IVZONE
      CALL PZDSPL(FECHO,MESAGE)
      CALL VADOFT(
     I            FLOSIM,TRNSIM,PRZMON,TOPFLX,TOWFLX,
     I            FVADOT(IVZONE),FVTP10(IVZONE),PINT,NLDLT,IVZONE,
     I            NVREAD,IBTND1,IBTNDN,VALND1,VALNDN,ICHEM,
     I            PARENT,SIMTYM,
     O            SAVTMP)
C             
      DO 60 INODE = 1, NPV
        SAVHED(IVZONE,INODE) = SAVTMP(INODE)
 60   CONTINUE
C
C     zero VADOFT flow cumulative variables here
C
      TMACCW = 0.0
      CUWVEF = 0.0
      CUWVIF = 0.0
C
C     transfer VADOFT outputs to Monte-Carlo arrays
      IF (MCARLO) THEN
        MOUTFL = .TRUE.
        CALL MCVAD(
     I             FLOSIM,MOUTFL,IVZONE,ITSAFT)
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   XTRANS
     I                   (PRZMON,TRNSIM,MCARLO,
     I                    NPZONE,NLDLT,NCHEM,ITSAFT,IPARNT,
     I                    IVZONE,SIMTYM)
C
C     + + + PURPOSE + + +
C     VADOFT transport execution
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      LOGICAL      PRZMON,TRNSIM,MCARLO
      INTEGER      NPZONE,NLDLT,NCHEM,ITSAFT,
     1             IPARNT(3),IVZONE
      CHARACTER*80 SIMTYM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PRZMON - przm on flag
C     TRNSIM - vadoft transport on flag
C     MCARLO - monte carlo on flag
C     NPZONE - number of przms
C     NLDLT  - ???
C     NCHEM  - number of chemicals applied
C     ITSAFT - ???
C     IPARNT - ???
C     SIMTYM - ???
C     IVZONE - current zone number
C
C     + + + PARAMETERS + + +
      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PMXPDT.INC'
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXZON.INC'
      INCLUDE 'PMXOWD.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CPRZST.INC'
      INCLUDE 'CVADST.INC'
      INCLUDE 'CZNWHT.INC'
      INCLUDE 'CIN2VD.INC'
      INCLUDE 'CVNTR1.INC'
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CFILEX.INC'
      INCLUDE 'CVMISC.INC'
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL      FLOSIM,MOUTFL
      INTEGER      IPZONE,ICHEM,PARENT,ILDLT,INODE,IFIL
      REAL         SAVTMP(MXNOD),CTMP,WGHT,TOTFLX
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,INITDK,VADCHM,PZDSPL,VADOFT,MCVAD,SUBOUT
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT('Running VADOFT transport simulation, zone [',I2,
     1       '], chemical [',I1,']')
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'XTRANS'
      CALL SUBIN(MESAGE)
C
C     zero chemical decay array
      FLOSIM = .FALSE.
      CALL INITDK(
     I            NLDLT, NPV)
C
C     loop through chemicals
      DO 80 ICHEM = 1, NCHEM
C       store VADOFT corrected concentrations (initial conditions)
C       for this time step
        IF (ITSAFT .EQ. 1) THEN
          DO 10 INODE = 1, NPV
            PINT(INODE) = CPINT(INODE,ICHEM)
 10       CONTINUE
        ELSE
          DO 20 INODE = 1, NPV
            CTMP        = SAVCNC(IVZONE,INODE,ICHEM)
            PINT(INODE) = CTMP
C           CPINT used in VADCHM for initial conditions of parent
            CPINT(INODE,ICHEM) = CTMP
 20       CONTINUE
        ENDIF
C
C       set material properties by chemical
        CALL VADCHM(
     I              ICHEM, IPARNT,
     O              PARENT)
C
C       run VADOFT for NLDLT days
        WRITE(MESAGE,2000) IVZONE, ICHEM
        CALL PZDSPL(FECHO,MESAGE)
C
        IF(PRZMON) THEN
          DO 30 ILDLT = 1, NLDLT
            TOPFLX(ILDLT) = 0.0
            TOWFLX(ILDLT) = 0.0
 30       CONTINUE
C
          DO 50 IPZONE = 1, NPZONE
            WGHT = P2VWHT(IPZONE,IVZONE)
          TOTFLX = 0.0
            IF (WGHT .GT. 0.0) THEN
C
              DO 40 ILDLT=1,NLDLT
                TOPFLX(ILDLT) = TOPFLX(ILDLT) +
     1            WGHT * 1.0E6 * PRZMPF(IPZONE,ILDLT,ICHEM)
              TOTFLX = TOTFLX + TOPFLX(ILDLT)
                TOWFLX(ILDLT) = TOWFLX(ILDLT) +
     1            WGHT * PRZMWF(IPZONE,ILDLT)
  40          CONTINUE
C
C             end of time loop
            ENDIF
  50      CONTINUE
        ENDIF
C
C       end of PRZM check
        IFIL = FVTP10(IVZONE)
        REWIND IFIL
C
        CALL VADOFT(
     I              FLOSIM,TRNSIM,PRZMON,TOPFLX,TOWFLX,
     I              FVADOT(IVZONE),FVTP10(IVZONE),PINT,NLDLT,IVZONE,
     I              NVREAD,IBTND1,IBTNDN,VALND1,VALNDN,ICHEM,
     I              PARENT,SIMTYM,
     O              SAVTMP)
C
        DO 60 INODE = 1, NPV
          SAVCNC(IVZONE,INODE,ICHEM) = SAVTMP(INODE)
 60     CONTINUE
C
C       zero VADOFT cumulative chemical totals here
        CUSMEF = 0.0
        CUSMIF = 0.0
        TMACCP = 0.0
        TMDCYV = 0.0
C
C     end of chemical loop
 80   CONTINUE
C
C     transfer VADOFT variables to Monte Carlo arrays
      IF(MCARLO) THEN
        MOUTFL = .TRUE.
        CALL MCVAD(
     I             FLOSIM,MOUTFL,IVZONE,ITSAFT)
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   DONBAR
     I                   (MIN, MAX, NOW)
C
C     + + + PURPOSE + + +
C     calculate percent complete for screen
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MIN, MAX, NOW
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MIN - minimum number of iterations
C     MAX - maximum number of iterations
C     NOW - current number of iterations completed
C
C     + + + PARAMETERS + + +
      INCLUDE 'PCMPLR.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CECHOT.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      MAXLN, LNG, I
      CHARACTER*80 MESAGE
C
C     + + + INTRINSICS + + +
      INTRINSIC    INT, REAL, CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL     PZSCRN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MAXLN / 75/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ECHOLV .GE. 1) THEN
        LNG =  INT(0.5 + REAL((NOW - MIN + 1) * MAXLN) /
     1        REAL((MAX - MIN + 1)))
C
C       Ex.: 2 iterations, min = 1, max = 2. at end of run 1, now = 1
C       and lng = ifix((1 * maxln / 2) + 0.5), eg. halfway there
C
        IF ((LNG .GT. 0) .AND. (LNG .LE. MAXLN)) THEN
          MESAGE = ' '
          IF (WINDOW) THEN
            IF (NONPC) THEN
              DO 11 I = 1, LNG
                MESAGE(I:I) = '*'
 11           CONTINUE
            ELSE
              DO 12 I = 1, LNG
                MESAGE(I:I) = CHAR(219)
 12           CONTINUE
            ENDIF
            CALL PZSCRN(5,MESAGE)
          ENDIF
        ENDIF
      ENDIF
C

      RETURN
      END
C
C
C
      SUBROUTINE   INITDK
     I                   (NLDLT, NP)
C
C     + + + PURPOSE + + +
C     initializes the amount of pesticide decay of each
C     chemical which could have daughter products.
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      NLDLT, NP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NLDLT - ???
C     NP    - ???
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXVDT.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CVCHMK.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ICHEM, ILDLT, I
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN, SUBOUT
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'INITDK'
      CALL SUBIN(MESAGE)
C
      DO 30 ICHEM = 1, 2
        DO 20 ILDLT = 1, NLDLT
          DO 10 I = 1, NP
            TRTERM(I,ILDLT,ICHEM) = 0.0
 10       CONTINUE
 20     CONTINUE
 30   CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   VADCHM
     I                   (ICHEM, IPARNT,
     O                    PARENT)
C
C     + + + PURPOSE + + +
C     transfers chemical specific data to VADOFT variables
C     Modification date: 2/18/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ICHEM, IPARNT(3), PARENT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ICHEM  - ???
C     IPARNT - ???
C     PARENT - ???
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXMAT.INC'
      INCLUDE 'PMXNOD.INC'
      INCLUDE 'PMXOWD.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CIN2VD.INC'
      INCLUDE 'CCONTR.INC'
      INCLUDE 'CMDATA.INC'
      INCLUDE 'CVWRKM.INC'
      INCLUDE 'CVVLM.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I, J, IMAT, NPM1
C
C     + + + END SPECIFICATIONS + + +
C
      PARENT = IPARNT(ICHEM)
C
C     load chemical/material properties
      DO 20 I = 1, NMAT
        DO 10 J = 3, 5
          PROP(I,J) = CPROP(I,J,ICHEM)
 10     CONTINUE
C
        FRACMP(I) = CRACMP(I,ICHEM)
        IF (PARENT .NE. 0) THEN
C
C         retardation coef. of parent
          RCOFP(I)  = CPROP(I,3,PARENT)
C         decay coef. of parent compound
C
        ENDIF
 20   CONTINUE
C
C     decay coef. of current compound by node
      NPM1 = NP -1
      DO 30 I = 1, NPM1
        IMAT = IPROP(I)
        DLAMND(I) = CLAMDI(IMAT,ICHEM)
 30   CONTINUE
C
      RETURN
      END

