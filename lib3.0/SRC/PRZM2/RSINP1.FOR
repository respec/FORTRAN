C
C
C
      SUBROUTINE   INITEM
     I                   ( VERSON,
     O                     MCARLO,PRZMON,YRSTEP,VADFON,TRNSIM,NVZONE,
     O                     ISDAY,ISMON,ISTYR,IEDAY,IEMON,IEYR,
     O                     NLDLT,NTSAFT,LLSTS,NPZONE,
     O                     NCHEM,IPARNT,SEPTON,NITRON )
C
C     + + + PURPOSE + + +
C     called from main,
C     reads supervisor file to determine options and run dates,
C     opens all files necessary to perform simulation.
C     Modication date: 1/24/92 JAM
C
C     Date: Tuesday, 12 January 1993.  Time: 10:33:51.
C     Added version number (VERSON) to output screen with
C     program header line - DWD/CSC
C
C     + + + DUMMY ARGUMENTS + + +
      LOGICAL      MCARLO,PRZMON,YRSTEP,VADFON,TRNSIM,SEPTON,NITRON
      INTEGER*4    ISDAY,ISMON,ISTYR,IEDAY,IEMON,IEYR,IPARNT(3),
     1             NLDLT,NTSAFT,LLSTS,NPZONE,NVZONE,NCHEM
      REAL         VERSON
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MCARLO  - monte carlo on flag
C     PRZMON  - przm on flag
C     YRSTEP  - run PRZM at annual increments flag
C     VADFON  - vadoft flow on flag
C     TRNSIM  - vadoft transport on flag
C     ISDAY   - day on which to start simulation
C     ISMON   - month on which to start simulation
C     ISTYR   - year on which to start simulation
C     IEDAY   - day on which to end simulation
C     IEMON   - month on which to end simulation
C     IEYR    - year on which to end simulation
C     IPARNT  - chemical number corresponding to parent
C     NLDLT   - maximum number of days in a vadoft run
C     NTSAFT  - number of months in simulation
C     LLSTS   - number of days left in last month time step
C     NVZONE  - number of vadoft zones
C     NPZONE  - number of przm zones
C     NCHEM   - number of chemicals being applied (up to 3)
C     VERSON  - current version number
C     SEPTON  - septic effluent on flag
C     NITRON  - nitrogen modeling on flag
C
C          + + + PARAMETERS + + +
      INCLUDE 'PIOUNI.INC'
      INCLUDE 'PCMPLR.INC'
      INCLUDE 'PMXNSZ.INC'
      INCLUDE 'PMXVDT.INC'
      INCLUDE 'PMXZON.INC'
      INCLUDE 'PPARM.INC'
      INCLUDE 'PMXYRS.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'TABLE.INC'
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CZNWHT.INC'
      INCLUDE 'CTRACE.INC'
      INCLUDE 'CECHOT.INC'
      INCLUDE 'CFILEX.INC'
C
C     + + + LOCAL VARIABLES + + +
      LOGICAL      STDAOK,ENDAOK,NCHMOK,PRNTOK(3),FATAL,EOF,EDRUN,
     1             DONGLO,DONFIL,WHTSON
      INTEGER*4    IERROR,IGNCNT,TNDAYS,LCOUNT,MCOUNT,KCOUNT,
     1             ICHEM,I,J,LNG,ISTR,FTYP,EJULDY,SJULDY
      REAL         RTMP(MXNSZO),SUM
      CHARACTER*80 FNAME,PATH,TEST,FNAME1,FNAME2,MESAGE
      CHARACTER*76 IGNORE(20)
      CHARACTER*56 STATS
      CHARACTER*18 ANAME,FILID(25)
      CHARACTER*7  FSTAT
      CHARACTER*2  ZSTR,ICHAR
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR, DYJDY
C
C     + + + INTRINSICS + + +
      INTRINSIC    ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL     CLEAR,PZSCRN,COMRD,NAMFIX,OPECHO,ECHOF,
     1             ERRCHK,VALDAT,ECHOGD,PRZDAY,TDCALC,COMRD2,
     2             LNGSTR,FILOPN,FILCHK,DYJDY
C
C     + + + DATA INITIALIZATIONS + + +
      DATA FILID/'                  ','                  ',
     1           '                  ','WDM FILE          ',
     2           'MCIN              ','MCOUT             ',
     3           'MCOUT2            ','                  ',
     4           '                  ','                  ',
     5           'PRZM INPUT        ','                  ',
     6           'PRZM OUTPUT       ','PRZM RESTART      ',
     7           '                  ','TIME SERIES       ',
     8           'METEOROLOGY       ','SEPTIC EFFLUENT   ',
     9           'NITROGEN DEPOSIT  ','                  ',
     *           'VADOFT INPUT      ','VADOFT OUTPUT     ',
     1           'VADOFT TAPE10     ','VADOFT FLOW RST   ',
     2           'VADOFT TRANS RST  '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(F4.2)
 1100 FORMAT(A18,6X,A56)
 1110 FORMAT(A18,1X,A2,1X,A56)
 1120 FORMAT(I2)
 1200 FORMAT(BN,I56)
 1300 FORMAT(10F8.2)
 1400 FORMAT(BN,I25)
 1600 FORMAT(3I2)
 1700 FORMAT(I2)
 1800 FORMAT(I8)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(1X,79('*')//
     1       12X,' P R Z M - 2  Model',
     2           ' - Release',F5.2,' - September,  1994')
 2010 FORMAT(   //,6X,'DEVELOPED FOR:',
     3          //,12X,'              R. F. Carsel',
     4           /,12X,'  U.S. Environmental Protection Agency',
     5           /,12X,'   Office of Research and Development',
     6           /,12X,'Athens Environmental Research Laboratory',/,
     B       ' ',79('*'),///,
     1       ' ','Selected options:'/)
 2020 FORMAT(' '/
     1       ' ','Files:'/)
 2025 FORMAT(' '/
     1       ' ','Global data:'/)
 2027 FORMAT(1X,'Przm to Vadoft weight matrix:'/73X,'Vadoft')
 2028 FORMAT(1X,A79)
 2030 FORMAT('Error in file name input, line with...',A24)
 2110 FORMAT('Number of PRZM zones is [',I2,'] ')
 2115 FORMAT('Number of VADOFT zones is [',I2,'] ')
 2120 FORMAT('ERROR, too many zones, maximum is [',I2,'] ')
 2150 FORMAT(I2,2('/',I2))
 2200 FORMAT(I5)
 2250 FORMAT('Parent of chemical [',I1,']')
 2300 FORMAT(1X,'Total days:',I7,'    Time steps:',I4)
 2350 FORMAT(/' One PRZM will contribute 100% of the flux to one ',
     1       'VADOFT')
 2400 FORMAT('Bad value [',I4,'] for number of chemicals')
 2450 FORMAT('Bad index [',I4,'] of chemical')
 2500 FORMAT('Bad value [',I4,'] for chemical parent species')
 2550 FORMAT(/,' The following lines of the input RUN file ',
     1       'were ignored: '//)
 2600 FORMAT(1X,I2,'. ',A76)
 2700 FORMAT('ENDDATA before the parent of chemical',I2,' was provided')
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize trace level, subroutine level, and run file params
      SUBLVL = 0
      TRCLVL = 0
      PATH   = ' '
      PRZMON = .TRUE.
      WHTSON = .FALSE.
      VADFON = .FALSE.
      MCARLO = .FALSE.
      TRNSIM = .FALSE.
      SEPTON = .FALSE.
      NITRON = .FALSE.
      IGNCNT = 0
      NLDLT  = 31
C
C     default number of zones
      NPZONE = 1
      NVZONE = 1
      NCHEM  = 1
C
C     read flags
      STDAOK = .FALSE.
      ENDAOK = .FALSE.
      NCHMOK = .FALSE.
C
C     user can input IPARNT(1) but it must always be zero
      PRNTOK(1) = .TRUE.
      IPARNT(1) = 0
      PRNTOK(2) = .FALSE.
      PRNTOK(3) = .FALSE.
C
C     clear the screen before execution
      CALL CLEAR
C
C     hard-wire names of RUN file and ECHO file
      FNAME1 = 'PRZM2.RUN'
      FNAME2 = 'KECHO.PRN'
C
C
C     draw output screen with program header
C      MESAGE = ' P  R  Z  M  -  2    Execution Supervisor'
      MESAGE = ' PRZM-2 Version x.xx Execution Supervisor'
      WRITE (MESAGE(17:20),1000) VERSON
      CALL PZSCRN(0,MESAGE)
C
      FTYP   = 3
      MESAGE = FILID(FTYP)
      FSTAT  = 'OLD'
C     open run file
      CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME1, FSTAT, 0,
     M            FILOPC,
     O            FCON )
C
      FTYP   = 1
      MESAGE = FILID(FTYP)
      FSTAT  = 'UNKNOWN'
C     open echo file to default path
      CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME2, FSTAT, 0,
     M            FILOPC,
     O            FECHO )
      KECHOT = FECHO
C
C     determine which routines are to be run
      MESAGE = 'Determine user options'
      CALL PZSCRN(1,MESAGE)
      EDRUN = .FALSE.
C
 100  CONTINUE
C
C       read a line of input
        CALL COMRD(FCON,MESAGE)
        READ(MESAGE,1100) ANAME, STATS
C
C       capitalize and left justify
        TEST   = ANAME
        CALL NAMFIX(TEST)
        MESAGE = STATS
        CALL NAMFIX(MESAGE)
        STATS  = MESAGE(1:56)
C
C       check which options are on
        IF (TEST(1:7) .EQ. 'PRZM   ') THEN
C         przm may be on
          PRZMON = STATS(1:2) .EQ. 'ON'
          IF (PRZMON) THEN
C           see if running at an annual increment
            MESAGE = STATS(3:56)
            CALL NAMFIX (MESAGE)
            YRSTEP = MESAGE .EQ. 'YEAR'
          ELSE
C           don't run an annual increment
            YRSTEP = .FALSE.
          END IF
        ELSE IF (TEST(1:9) .EQ. 'VADOFT   ') THEN
C         vadoft flow may be on
          VADFON = STATS .EQ. 'ON'
        ELSE IF (TEST(1:9) .EQ. 'TRANSPORT') THEN
C         vadoft transport may be on
          TRNSIM = STATS .EQ. 'ON'
        ELSE IF (TEST(1:8) .EQ. 'NITROGEN') THEN
C         nitrogen simulation may be on
          NITRON = STATS .EQ. 'ON'
        ELSE IF (TEST(1:11) .EQ. 'MONTE CARLO') THEN
C         monte carlo information
          MCARLO = .NOT. (STATS(1:3) .EQ. 'OFF')
          IF (MCARLO) THEN
            MESAGE = 'Monte Carlo is ON'
            MCOFLG = 1
            CALL PZSCRN(1,MESAGE)
          ELSE
            MESAGE = 'Monte Carlo is OFF'
            MCOFLG = 0
            CALL PZSCRN(1,MESAGE)
          ENDIF
        ELSE IF (TEST(1:10) .EQ. 'PRZM ZONES') THEN
C         how many przm zones?
          READ(STATS,'(I2)',ERR=110) NPZONE
          WRITE(MESAGE,2110) NPZONE
          CALL PZSCRN(1,MESAGE)
          IF (NPZONE .GT. MXZONE) THEN
C           too many przm zones
            WRITE(MESAGE,2120) MXZONE
            IERROR= 1110
            FATAL = .TRUE.
            CALL ERRCHK(IERROR,MESAGE,FATAL)
          END IF
          GOTO 115
 110      CONTINUE
            MESAGE= 'ERROR reading number of przm zones'
            IERROR= 1120
            FATAL = .TRUE.
            CALL ERRCHK(IERROR,MESAGE,FATAL)
 115      CONTINUE
        ELSE IF (TEST(1:12) .EQ. 'VADOFT ZONES') THEN
C         how many vadoft zones?
          READ(STATS,'(I2)',ERR=120) NVZONE
          WRITE(MESAGE,2115) NVZONE
          CALL PZSCRN(1,MESAGE)
          IF (NVZONE .GT. MXNSZO) THEN
C           too many vadoft zones
            WRITE(MESAGE,2120) MXNSZO
            IERROR= 1110
            FATAL = .TRUE.
            CALL ERRCHK(IERROR,MESAGE,FATAL)
          END IF
          GOTO 125
 120      CONTINUE
            MESAGE= 'ERROR reading number of vadoft zones'
            IERROR= 1120
            FATAL = .TRUE.
            CALL ERRCHK(IERROR,MESAGE,FATAL)
 125      CONTINUE
        ELSE IF (TEST(1:6) .EQ. 'ENDRUN') THEN
C         echo requested options
          WRITE(FECHO,2000) VERSON
          WRITE(FECHO,2010)
          MESAGE = 'Number of PRZM zones'
          WRITE(TEST,2200) NPZONE
          CALL ECHOGD(FECHO,TEST,MESAGE)
          MESAGE = 'Number of VADOFT zones'
          WRITE(TEST,2200) NVZONE
          CALL ECHOGD(FECHO,TEST,MESAGE)
          MESAGE = 'Monte Carlo'
          CALL OPECHO(FECHO,MESAGE,MCARLO)
          MESAGE = 'PRZM'
          CALL OPECHO(FECHO,MESAGE,PRZMON)
          MESAGE = 'VADOFT'
          CALL OPECHO(FECHO,MESAGE,VADFON)
          MESAGE = 'Transport simulation'
          CALL OPECHO(FECHO,MESAGE,TRNSIM)
          WRITE(FECHO,2020)
C
          IF (PRZMON .AND. VADFON) THEN
C           initialize przm to vadoft weight array
            WHTSON = .FALSE.
            DO 20 I = 1, NPZONE
              DO 15 J = 1, NVZONE
                IF (J .EQ. I) THEN
                  P2VWHT(I,J) = 1.0
                ELSE
                  P2VWHT(I,J) = 0.0
                ENDIF
   15         CONTINUE
   20       CONTINUE
          ENDIF
C
          EDRUN  = .TRUE.
        ENDIF
C         
      IF (.NOT.(EDRUN)) GOTO 100
C
      DONFIL = .FALSE.
C     loop to open files
 300  CONTINUE
C       ???
        CALL COMRD(FCON,MESAGE)
        READ(MESAGE,1110) ANAME,ZSTR,FNAME
C
C       capitalize and left justify
        TEST = ANAME
        CALL NAMFIX(TEST)
        CALL NAMFIX(FNAME)
C
C       check for valid zones
        READ(ZSTR,1120,ERR=310) ISTR
        GOTO 320
  310   CONTINUE
C         read error on zone number -- force error
          ISTR = -1
  320   CONTINUE
        IF (ISTR .EQ. 0) THEN
C         default to zone 1
          ISTR = 1
        ELSE IF (ISTR .LT. 0 .OR. (ISTR .GT. MXZONE .AND.
     1      ISTR .GT. MXNSZO)) THEN
          MESAGE = 'Bad zone # '// ZSTR // ' for file ' // ANAME
          IERROR = 1130
          FATAL  = .TRUE.
          CALL ERRCHK (IERROR,MESAGE,FATAL)
        ENDIF
C
        IF (TEST (1:4) .EQ. 'PATH') THEN
          PATH = FNAME
C
        ELSE IF (TEST (1:18) .EQ. FILID(11)) THEN
C         przm input
          IF (PRZMON .AND. ISTR .LE. NPZONE) THEN
            FTYP = 11
            FSTAT= 'OLD'
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FPRZIN(ISTR) )
            MESAGE = FILID(FTYP)
            CALL ECHOF(FECHO, FPRZIN(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(17)) THEN
          IF (PRZMON) THEN
            FTYP   = 17
            MESAGE = FILID(FTYP)
            FSTAT= 'OLD'
C           open meteorology file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FMETEO(ISTR) )
            CALL ECHOF(FECHO, FMETEO(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(4)) THEN
          IF (PRZMON) THEN
            FTYP   = 4
            MESAGE = FILID(FTYP)
            FSTAT= 'OLD'
C           open WDMS file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FWDMS)
            CALL ECHOF(FECHO, FWDMS, PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(18)) THEN
          IF (PRZMON) THEN
            FTYP   = 18
            MESAGE = FILID(FTYP)
            FSTAT= 'OLD'
C           open septic effluent file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FSPTIC(ISTR) )
            CALL ECHOF(FECHO, FSPTIC(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
            SEPTON = .TRUE.
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(19)) THEN
          IF (PRZMON) THEN
            FTYP   = 19
            MESAGE = FILID(FTYP)
            FSTAT= 'OLD'
C           open nitrogen atmospheric deposition file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FNITAD(ISTR) )
            CALL ECHOF(FECHO, FNITAD(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(21)) THEN
          IF (VADFON) THEN
            FTYP   = 21
            MESAGE = FILID(FTYP)
            FSTAT= 'OLD'
C           open VADOFT input file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FVADIN(ISTR) )
            CALL ECHOF(FECHO, FVADIN(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(5)) THEN
          IF (MCARLO) THEN
            FTYP   =  5
            MESAGE = FILID(FTYP)
            FSTAT= 'OLD'
C           open Monte Carlo input file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, 0,
     M                  FILOPC,
     O                  FMCIN )
            CALL ECHOF(FECHO, FMCIN, PATH, FNAME, 0,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
C       output files
C       przm
        ELSE IF (TEST (1:18) .EQ. FILID(13)) THEN
          IF (PRZMON .AND. (.NOT. MCARLO)) THEN
            FTYP   = 13
            FSTAT  = 'UNKNOWN'
            MESAGE = FILID(FTYP)
C           open HYDROLOGY file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FPRZOT(ISTR) )
            CALL ECHOF(FECHO, FPRZOT(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(16)) THEN
          IF (PRZMON .AND. (.NOT. MCARLO)) THEN
            FTYP   = 16
            MESAGE = FILID(FTYP)
            FSTAT= 'UNKNOWN'
C           open TIME SERIES file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FTMSRS(ISTR) )
            CALL ECHOF(FECHO, FTMSRS(ISTR), PATH, FNAME, 0,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(22)) THEN
          IF (VADFON) THEN
            FTYP   = 22
            MESAGE = FILID(FTYP)
            FSTAT= 'UNKNOWN'
C           open VADOFT output file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FVADOT(ISTR) )
            CALL ECHOF(FECHO, FVADOT(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(6)) THEN
          IF (MCARLO) THEN
            FTYP   = 6
            MESAGE = FILID(FTYP)
            FSTAT= 'UNKNOWN'
C           open Monte Carlo output file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, 0,
     M                  FILOPC,
     O                  FMCOUT)
            CALL ECHOF(FECHO, FMCOUT, PATH, FNAME, 0,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(7)) THEN
          IF (MCARLO) THEN
            FTYP   = 7
            MESAGE = FILID(FTYP)
            FSTAT  = 'UNKNOWN'
C           open Monte Carlo output file 2
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, 0,
     M                  FILOPC,
     O                  FMCOU2 )
            MESAGE = FILID(FTYP)
            CALL ECHOF(FECHO, FMCOU2, PATH, FNAME, 0,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
C       open scratch files
C       vadoft
        ELSE IF (TEST (1:18) .EQ. FILID(23)) THEN
          IF (VADFON .AND. TRNSIM) THEN
            FTYP   = 23
            MESAGE = FILID(FTYP)
            FSTAT  = 'UNKNOWN'
C           open TAPE10 file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FVTP10(ISTR))
            MESAGE = FILID(FTYP)
            CALL ECHOF(FECHO, FVTP10(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(14)) THEN
          IF (PRZMON) THEN
            FTYP   = 14
            FSTAT  = 'UNKNOWN'
C           open PRZM RESTART file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FPRZRS(ISTR) )
            MESAGE = FILID(FTYP)
            CALL ECHOF(FECHO, FPRZRS(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(24)) THEN
          IF (VADFON) THEN
            FTYP   = 24
            FSTAT  = 'UNKNOWN'
C           open VADOFT FLOW RESTART file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FVRSTF(ISTR) )
            MESAGE = FILID(FTYP)
            CALL ECHOF(FECHO, FVRSTF(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:18) .EQ. FILID(25)) THEN
          IF (VADFON .AND. TRNSIM) THEN
            FTYP   = 25
            FSTAT  = 'UNKNOWN'
C           open VADOFT TRANSPORT RESTART file
            CALL FILOPN(FILID(FTYP), FTYP, PATH, FNAME, FSTAT, ISTR,
     M                  FILOPC,
     O                  FVRSTT(ISTR) )
            MESAGE = FILID(FTYP)
            CALL ECHOF(FECHO, FVRSTT(ISTR), PATH, FNAME, ISTR,
     M                 MESAGE)
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST (1:8) .EQ. 'ENDFILES') THEN
C         check to see that all files necessary have been opened
          IF (PRZMON) THEN
            CALL FILCHK(FILID(11),NPZONE,FPRZIN)
            IF (FWDMS .LE. 0) THEN
C             wdm file not available
              CALL FILCHK(FILID(17),NPZONE,FMETEO)
            ENDIF
            CALL FILCHK(FILID(14),NPZONE,FPRZRS)
            IF (.NOT. MCARLO) THEN
              CALL FILCHK(FILID(13),NPZONE,FPRZOT)
              CALL FILCHK(FILID(16),NPZONE,FTMSRS)
            ENDIF
          ENDIF
C
          IF (VADFON) THEN
C           vadoft flow on
            CALL FILCHK(FILID(21),NVZONE,FVADIN)
            CALL FILCHK(FILID(24),NVZONE,FVRSTF)
            CALL FILCHK(FILID(22),NVZONE,FVADOT)
            IF (TRNSIM) THEN
C             vadoft transport on
              CALL FILCHK(FILID(23),NVZONE,FVTP10)
              CALL FILCHK(FILID(25),NVZONE,FVRSTT)
            ENDIF
          ENDIF
C
          DONFIL = .TRUE.
C
        ELSE
C         bad input
          WRITE(MESAGE,2030) TEST(1:24)
          IERROR = 1070
          FATAL  = .TRUE.
          CALL ERRCHK(IERROR,MESAGE,FATAL)
        ENDIF
C
      IF (.NOT. (DONFIL)) GOTO 300
C
C
C     global data
      DONGLO = .FALSE.
C     write header for global data
      WRITE(FECHO,2025)
C
 400  CONTINUE
        CALL COMRD(FCON,MESAGE)
        READ(MESAGE,1100) ANAME, FNAME
        TEST = ANAME
        CALL NAMFIX(TEST)
C
        IF (TEST(1:10) .EQ. 'START DATE') THEN
C         read in start date of simulation
          READ(FNAME,1600,END=4100,ERR=4300)
     1      ISDAY,ISMON,ISTYR
C         check to see if date is valid calendar date
          CALL VALDAT(TEST,ISDAY,ISMON,ISTYR)
          STDAOK = .TRUE.
          MESAGE = 'Start of simulation'
          WRITE(TEST,2150) ISDAY, ISMON, ISTYR
          CALL ECHOGD(FECHO,TEST,MESAGE)
C
        ELSE IF (TEST(1:8) .EQ. 'END DATE') THEN
C         read in ending date of simulation
          READ(FNAME,1600,END=4100,ERR=4300)
     1      IEDAY,IEMON,IEYR
C         check to see if date is valid calendar date
          CALL VALDAT(TEST,IEDAY,IEMON,IEYR)
          ENDAOK = .TRUE.
          MESAGE = 'End of simulation'
          WRITE(TEST,2150) IEDAY, IEMON, IEYR
          CALL ECHOGD(FECHO,TEST,MESAGE)
C
C   ADDED TO USE FOR SUMMARY TABLE
C
          STARTYR = ISTYR
          ENDYEAR = IEYR
C
        ELSE IF (TEST(1:16) .EQ. 'NUMBER OF CHEMIC') THEN
C         read in number of chemicals from run file
          IF (PRZMON .OR. VADFON) THEN
            READ(FNAME,1200,END=4100,ERR=4300) NCHEM
            MESAGE = 'Number of chemicals'
            WRITE(TEST,2200) NCHEM
            CALL ECHOGD(FECHO,TEST,MESAGE)
C           number of chemicals cannot be < 1 or > 3
            IF ((NCHEM .LT. 1) .OR. (NCHEM .GT. 3)) THEN
              IERROR = 1090
              WRITE(MESAGE,2400) NCHEM
              FATAL  = .TRUE.
              CALL ERRCHK(IERROR,MESAGE,FATAL)
            ENDIF
            NCHMOK = .TRUE.
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
C       if VADOFT is on, user must determine daughter products
C       if PRZM run only then daughter products are optional
        ELSE IF (TEST(1:9) .EQ. 'PARENT OF') THEN
          IF (TRNSIM .AND. (NCHEM .GT. 1)) THEN
            ANAME = TEST(10:24)
            TEST  = ANAME
            READ(TEST,1400,END=4100,ERR=4300) ICHEM
            IF ((ICHEM .LT. 1) .OR. (ICHEM .GT. NCHEM)) THEN
              IERROR = 1092
              WRITE(MESAGE,2450) ICHEM
              FATAL  = .TRUE.
              CALL ERRCHK(IERROR,MESAGE,FATAL)
            ELSE
              READ(FNAME,1200,END=4100,ERR=4300) IPARNT(ICHEM)
              PRNTOK(ICHEM) = .TRUE.
            ENDIF
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST(1:7) .EQ. 'WEIGHTS') THEN
C         przm to vadoft weights
          IF (PRZMON .AND. VADFON) THEN
            WHTSON = .TRUE.
            WRITE(FECHO,2027)
            MESAGE = ' '
            DO 50 I = 1 ,NVZONE
C             build vadoft zone message for array in echo file
              ISTR = 79-(NVZONE-I+1)*8
              WRITE(MESAGE(ISTR:ISTR+7),1800)I
   50       CONTINUE
            WRITE(FECHO,2028) MESAGE
            MESAGE = ' '
            DO 30 I = 1, NPZONE
              READ(FCON,1300,END=4100,ERR=4300) RTMP
              DO 25 J = 1, NVZONE
                P2VWHT(I,J) = RTMP(J)
   25         CONTINUE
              WRITE(ICHAR,1700) I
              MESAGE = 'Przm ['//ICHAR//']'
              WRITE(TEST,1300) (RTMP(J), J=1,NVZONE)
              CALL ECHOGD(FECHO,TEST,MESAGE)
   30       CONTINUE
C           check to see that sum of each column equals one
            DO 40 J = 1, NVZONE
              SUM = 0
              DO 35 I = 1, NPZONE
                SUM = SUM + P2VWHT(I,J)
   35         CONTINUE
              IF (ABS(SUM - 1.0) .GT. 1.0E-30) THEN
                IERROR = 1205
                MESAGE = 'P2VWHT columns must sum to 1'
                FATAL  = .TRUE.
                CALL ERRCHK(IERROR,MESAGE,FATAL)
              ENDIF
   40       CONTINUE
          ELSE
            IGNCNT = IGNCNT + 1
            IGNORE(IGNCNT) = MESAGE
          ENDIF
C
        ELSE IF (TEST(1:7) .EQ. 'ENDDATA') THEN
C         check to see if all necessary data has been read
          IERROR = 0
          IF (.NOT. STDAOK) THEN
            IERROR = 1500
            MESAGE = 'ENDDATA before starting day was provided'
          ELSE IF (.NOT. ENDAOK) THEN
            IERROR = 1510
            MESAGE = 'ENDDATA before end day was provided'
          ELSE IF (TRNSIM) THEN
            IF (.NOT. NCHMOK) THEN
              IERROR = 1530
              MESAGE = 'ENDDATA before number of chemicals was provided'
            ELSE
              IF (NCHEM .GT. 1) THEN
                DO 499 ICHEM = 1, NCHEM
                  IF (.NOT. PRNTOK(ICHEM)) THEN
                    IERROR = 1540
                    WRITE(MESAGE,2700) ICHEM
                    GO TO 500
                  ENDIF
                  IF (IPARNT(ICHEM) .EQ. 0) THEN
C                   running one chemical
                    WRITE(MESAGE,2250) ICHEM
                    TEST = 'No parent'
                    CALL ECHOGD(FECHO,TEST,MESAGE)
                  ELSE IF(IPARNT(ICHEM) .LT. ICHEM) THEN
C                   running daughter products
                    WRITE(MESAGE,2250) ICHEM
                    WRITE(TEST,2200) IPARNT(ICHEM)
                    CALL ECHOGD(FECHO,TEST,MESAGE)
                  ELSE IF (IPARNT(ICHEM) .GE. ICHEM) THEN
C                   too many parents
                    WRITE(MESAGE,2500) IPARNT(ICHEM)
                    IERROR = 1100
                    GO TO 500
C                   goto to get out of a do-while  !!!
                  ENDIF
 499            CONTINUE
C
 500            CONTINUE
              ENDIF
            ENDIF
          ENDIF
C
          IF (IEYR .LT. ISTYR) THEN
            IERROR = 1200
            MESAGE = 'End date is before start date'
          ELSE IF (IEYR .EQ. ISTYR) THEN
            IF (IEMON .LT. ISMON) THEN
              IERROR = 1200
              MESAGE = 'End date is before start date'
            ELSE IF (IEMON .EQ. ISMON) THEN
              IF (IEDAY .LT. ISDAY) THEN
                IERROR = 1200
                MESAGE = 'End date is before start date'
              ELSE IF (IEDAY .EQ. ISDAY) THEN
                IERROR = 1202
                MESAGE = 'End date and start date are the same'
              ENDIF
            ENDIF
          ENDIF
C
          IF (IERROR .NE. 0) THEN
            FATAL = .TRUE.
            CALL ERRCHK(IERROR,MESAGE,FATAL)
          ENDIF
C     
          IF(PRZMON) THEN
C           store dates in PRZM common
            CALL PRZDAY(ISDAY,ISMON,ISTYR,IEDAY,IEMON,IEYR)
          ENDIF
C
C         calculate total no. of days in simulation
          CALL TDCALC(
     I                ISDAY,ISMON,ISTYR,
     I                IEDAY,IEMON,IEYR,
     O                TNDAYS,LCOUNT,MCOUNT,KCOUNT)
C
          LLSTS  = 0
          IF (VADFON .OR. .NOT. YRSTEP) THEN
C           use monthly increment
            NTSAFT = KCOUNT
            IF ((IEMON .EQ. ISMON) .AND. (IEYR .EQ. ISTYR)) THEN
C             less than one month run
              LLSTS = IEDAY - ISDAY + 1
            ELSE
C             number of days in last month
              LLSTS = IEDAY
            END IF
          ELSE
C           use annual increment for PRZM only
            NTSAFT = IEYR- ISTYR
            EJULDY = DYJDY(IEYR,IEMON,IEDAY)
            SJULDY = DYJDY(ISTYR,ISMON,ISDAY)
            IF (IEYR .EQ. ISTYR) THEN
C             less than one year run
              LLSTS = EJULDY - SJULDY + 1
            ELSE
C             number of days in last year
              LLSTS = EJULDY
            END IF
          END IF
C
C
          IF (LLSTS .NE. 0) THEN
C           last partial increment
            NTSAFT = NTSAFT + 1
            WRITE(FECHO,2300)TNDAYS,NTSAFT
          ENDIF
          IF (IGNCNT .GT. 0) THEN
C           list out which lines were ignored
            WRITE(FECHO,2550)
            DO 511 I = 1, IGNCNT
              WRITE(FECHO,2600) I, IGNORE(I)
 511        CONTINUE
          ENDIF
          IF (.NOT. WHTSON) THEN
C           write default 'przm to vadoft weight' message
            WRITE(FECHO,2350)
          ENDIF
          DONGLO = .TRUE.
        ELSE
          IERROR = 1190
          LNG    = LNGSTR(TEST)
          MESAGE = 'Bad identifier reading global data[' //
     1      TEST(1:LNG) // ']'
          FATAL  = .TRUE.
          CALL ERRCHK(IERROR,MESAGE,FATAL)
        ENDIF
C
      IF (.NOT.(DONGLO)) THEN
C     read next global data line
      GO TO 400
      ENDIF
C
C     echo level
      CALL COMRD2(FCON,MESAGE,EOF)
      IF (EOF) GO TO 3100
      READ(MESAGE,1100) ANAME, STATS
C
C     capitalize and left justify
      TEST   = ANAME
      CALL NAMFIX(TEST)
      MESAGE = STATS
      CALL NAMFIX(MESAGE)
      STATS  = MESAGE(1:56)
      IF (TEST(1:4) .EQ. 'ECHO') THEN
        IF (STATS .EQ. 'OFF') THEN
          ECHOLV = 0
        ELSE IF (STATS .EQ. 'ON') THEN
          IF (MCARLO) THEN
            ECHOLV = 1
          ELSE
            ECHOLV = 5
          ENDIF
        ELSE
          TEST = 'Echo level'
          READ(STATS,1200,END=4100,ERR=4300) ECHOLV
          IF (MCARLO .AND. (ECHOLV .GE. 4)) THEN
            IERROR = 1570
            MESAGE = 'Monte carlo simulation - echo level reset to 1'
            FATAL  = .FALSE.
            CALL ERRCHK(IERROR,MESAGE,FATAL)
            ECHOLV = 1
          ENDIF
        ENDIF
        GO TO 3200
      ELSE
C
C       last card not echo, must be trace
        IF (MCARLO) THEN
          ECHOLV = 1
        ELSE
          ECHOLV = 5
        ENDIF
        GO TO 3800
      ENDIF
C
 3100 CONTINUE
        IERROR = 1220
        IF (MCARLO) THEN
          MESAGE = 'Echo level not defined; set to 1'
          ECHOLV = 1
        ELSE
          MESAGE = 'Echo level not defined; set to 5'
          ECHOLV = 5
        ENDIF
        FATAL  = .FALSE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
C
C       end-of-file means no info on trace either
        GO TO 4000
C
 3200 CONTINUE
C
C     trace information
      CALL COMRD2(FCON,MESAGE,EOF)
      IF (EOF) GO TO 4000
      READ(MESAGE,1100) ANAME, STATS
C
C     capialtize and left justify
      TEST   = ANAME
      CALL NAMFIX(TEST)
      MESAGE = STATS
      CALL NAMFIX(MESAGE)
      STATS  = MESAGE(1:24)
 3800 IF (TEST(1:5) .EQ. 'TRACE') THEN
C       ???
        IF (STATS .EQ. 'OFF') THEN
          TRCLVL = 0
        ELSE IF (STATS .EQ. 'ON') THEN
          TRCLVL = 3
        ELSE
          TEST = 'Trace level'
          READ(STATS,1200,END=4100,ERR=4300) TRCLVL
        ENDIF
        GO TO 8000
      ELSE
C       ???
        LNG = LNGSTR(TEST)
        IF (LNG .GT. 24) LNG = 24
        MESAGE = 'Unrecognized label [' // TEST(1:LNG) //
     1    '] while attempting to read ECHO or TRACE'
        IERROR = 1210
        FATAL  = .FALSE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      ENDIF
C
 4000 CONTINUE
      IERROR = 1230
      MESAGE = 'Trace level not defined; set to 0'
      FATAL  = .FALSE.
      CALL ERRCHK(IERROR,MESAGE,FATAL)
      TRCLVL = 0
      GO TO 8000
C
 4100 CONTINUE
      IERROR = 1240
      MESAGE = 'End of file on PRZM2 run file'
      FATAL  = .TRUE.
      CALL ERRCHK(IERROR,MESAGE,FATAL)
      GO TO 8000
C
 4300 CONTINUE
      IERROR = 1250
      MESAGE = 'Error reading PRZM2 run file data... ' // TEST
      FATAL  = .TRUE.
      CALL ERRCHK(IERROR,MESAGE,FATAL)
C
C     normal exit
 8000 CONTINUE
C
C     close run control file
C           ??? how do we want to keep track of opened files ?
      CLOSE(FCON)
C
      RETURN
      END
C
C
C
      SUBROUTINE   FILOPN
     I                   (LFILID,FILTYP,PATH,FNAME,FSTAT,ISTR,
     M                    FILOPC,FILUNI)
C
C     +  +  + PURPOSE +  +  +
C
C     open files and assign unit numbers
C     Modification date: 2/7/92 JAM
C
C     +  +  + DUMMY ARGUMENTS +  +  +
C
      INTEGER      FILTYP,ISTR,FILOPC,FILUNI
      CHARACTER*80 FNAME,PATH
      CHARACTER*7  FSTAT
      CHARACTER*18 LFILID
C
C     +  +  + ARGUMENT DEFINITIONS +  +  +
C
C     LFILID - file type as text
C     FILTYP - file type as numeric code
C     PATH   - path to proceed to named file
C     FNAME  - file name
C     FSTAT  - file status, OLD or UNKNOWN
C     ISTR   - zone associated with file
C     FILOPC - count of open files
C     FILUNI - unit number of opened file
C
C     +  +  + PARAMETERS +  +  +
C
      INCLUDE 'PIOUNI.INC'
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      INTEGER      IERROR,NKNOWN,LEN,RETCOD
      LOGICAL      FATAL,FILUSE
      CHARACTER*80 MESAGE,FILNM,MTMP
      CHARACTER*2  USTR
C
C     +  +  + FUNCTIONS +  +  +
C
      INTEGER      LNGSTR
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL     PZSCRN,ERRCHK,LNGSTR,WDBOPN
C
C     +  +  + OUTPUT FORMATS +  +  +
C
2000  FORMAT (I2)
C
C     +  +  + END SPECIFICATIONS +  +  +
C
      IERROR = 0
C     error for unknown unit id
      NKNOWN = 7000
C
      IF (FILUNI .GT. 0) THEN
C       file type already specified
        IERROR = 1260
        MESAGE = 'File type ' // LFILID // 'has already been specified.'
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR, MESAGE, FATAL)
      END IF
C
      IF (FNAME(1:1) .EQ. '@') THEN
C       if first character of fname is '@', user is suppling alternate
C       path (or not using the defined path) within fname
        FILNM = FNAME(2:LNGSTR(FNAME))
      ELSE
        IF (LNGSTR(PATH) .LE. 0) THEN
C         no path available
          FILNM = FNAME(1:LNGSTR(FNAME))
        ELSE
C         add path to start of name
          FILNM = PATH(1:LNGSTR(PATH)) // FNAME(1:LNGSTR(FNAME))
          ENDIF
      ENDIF
C     length of file name
      LEN = LNGSTR(FILNM)
      IF (LEN .GT. 40) THEN
C       only so much room
        LEN = 40
      ENDIF
C
      IF (FILOPC .GE. NMXFIL) THEN
        IERROR = 1270
        WRITE (USTR,2000) NMXFIL
        MESAGE = 'Too many files opened at once, limit is' // USTR
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR, MESAGE, FATAL)
      ELSE
C       increment count of open files
        FILUNI = FILBAS + FILOPC
        FILOPC = FILOPC + 1
      ENDIF
C
C     already open?
      INQUIRE (FILE=FILNM,OPENED=FILUSE)
      IF (FILUSE) THEN
C       problem, already in use
        IERROR = 1275
        MESAGE = 'File ' // FNAME(1:LEN) // ' is already in use.'
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR, MESAGE, FATAL)
      END IF
C
      IF (IERROR .EQ. 0) THEN
C       open the file
        IF (FILTYP .NE. 3) THEN
C         ???
          WRITE(USTR,2000) FILUNI
          MESAGE = 'Opening file [' // FILNM(1:LEN) // '] as unit [' //
     1             USTR // ']'
          IF (ISTR .GT. 0) THEN
C           file associated with zone
            WRITE(USTR,2000) ISTR
            MTMP  = MESAGE
            MESAGE= MTMP(1:LNGSTR(MTMP)) // ' for zone [' // USTR // ']'
          END IF
          CALL PZSCRN(1,MESAGE)
        ENDIF
        IF ((FILTYP .NE. 23) .AND. (FILTYP .NE. 24) .AND.
     1         (FILTYP .NE. 14) .AND. (FILTYP .NE. 25) .AND.
     2         (FILTYP .NE. 4)) THEN
          OPEN(FILUNI,FILE=FILNM,STATUS=FSTAT,IOSTAT=IERROR)
        ELSE IF (FILTYP .EQ. 4) THEN
C         special procedure to open wdm file
          CALL WDBOPN(FILUNI,FILNM,0,RETCOD)
          IF (RETCOD .NE. 0) THEN
            IERROR = 1285
            MESAGE = 'Error in attempting to open WDM file'
            FATAL  = .TRUE.
            CALL ERRCHK(IERROR,MESAGE,FATAL)
          ENDIF
        ELSE
C         unformatted file
          OPEN(FILUNI,FILE=FILNM,STATUS=FSTAT,IOSTAT=IERROR,
     1         FORM = 'UNFORMATTED')
        ENDIF
      ENDIF
C
C     check for any errors in opening file
      IF (IERROR .NE. 0) THEN
        LEN = LNGSTR(FILNM)
        IF (LEN .GT. 43) THEN
C         only so much room
          LEN = 43
        ENDIF
        IF (IERROR .EQ. NKNOWN) THEN
          MESAGE = 'Unknown unit number to open file [' //
     1      FILNM(1:LEN) //  ']'
          FATAL = .TRUE.
        ELSE
          MESAGE = 'Error in attempting to open file [' //
     1      FILNM(1:LEN) //  ']'
          FATAL = .TRUE.
        ENDIF
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ECHOF
     I                  (FECHO, KFILE, PATH, FNAME, ISTR,
     M                   MESAGE)
C
C     +  +  + PURPOSE +  +  +
C
C     used to echo names of files opened
C     Modification date: 2/7/92 JAM
C
C     +  +  + DUMMY ARGUMENTS +  +  +
C
      INTEGER      FECHO,KFILE,ISTR
      CHARACTER*80 PATH,FNAME,MESAGE
C
C     +  +  + ARGUMENT DEFINITIONS +  +  +
C
C     FECHO  - echo file unit number
C     KFILE  - unit number of file to echo
C     PATH   - path to proceed to named file
C     FNAME  - name of file to echo
C     ISTR   - zone associated with file
C     MESAGE - message written
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      INTEGER      LTOTL
      CHARACTER*80 VALUE,ADUM
      CHARACTER*1  CHR
C
C     +  +  + FUNCTIONS +  +  +
C
      INTEGER      LNGSTR
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL     ELPSE,LNGSTR
C
C     +  +  + DATA INITIALIZATIONS +  +  +
C
      DATA CHR   /'.'/
      DATA LTOTL / 79/
C
C     +  +  + OUTPUT FORMATS +  +  +
C
 2000 FORMAT(' ',A80)
 2020 FORMAT(' [',I2,']')
C
C     +  +  + END SPECIFICATIONS +  +  +
C
C     build message
      ADUM    = MESAGE(1:LNGSTR(MESAGE)) // ' FILE'
      IF (ISTR .GT. 0) THEN
C       zone info
        WRITE(VALUE,2020) ISTR
        MESAGE= ADUM
        ADUM  = MESAGE(1:LNGSTR(MESAGE)) // ' for ZONE' // VALUE
      END IF
      WRITE(VALUE,2020) KFILE
      MESAGE  = ADUM(1:LNGSTR(ADUM)) // ' uses UNIT' // VALUE
C
      IF (FNAME(1:1) .NE. '@') THEN
C       using defined path
        IF (LNGSTR(PATH) .GT. 0) THEN
C         add path to start of name
          VALUE = PATH(1:LNGSTR(PATH)) // FNAME
        ELSE
C         no path available
          VALUE = FNAME
        ENDIF
      ELSE
C       do not use filename
        VALUE = FNAME(2:LNGSTR(FNAME))
      ENDIF
C     ???
      CALL ELPSE(VALUE,CHR,LTOTL,MESAGE)
      WRITE(FECHO,2000) MESAGE
C
      RETURN
      END
C
C
C
      SUBROUTINE   VALDAT
     I                   (TEST,IDAY,IMON,IYR)
C
C     +  +  + PURPOSE +  +  +
C
C     checks to see if run date is valid calendar date
C     Modification date: 2/7/92 JAM
C
C     +  +  + DUMMY ARGUMENTS +  +  +
C
      CHARACTER*80 TEST
      INTEGER      IDAY,IMON,IYR
C
C     +  +  + ARGUMENT DEFINITIONS +  +  +
C
C     TEST - kind of date being tested
C     IDAY - day
C     IMON - month
C     IYR  - year
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      INTEGER      IERROR,LNG
      CHARACTER*80 MESAGE
      LOGICAL      FATAL
C
C     +  +  + FUNCTIONS +  +  +
C
      INTEGER      LNGSTR
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL     ERRCHK,LNGSTR
C
C     +  +  + INTRINSICS +  +  +
C
      INTRINSIC    MOD
C
C     +  +  + OUTPUT FORMATS +  +  +
C
 2000 FORMAT(I2,'/',I2,'/',I4)
C
C     +  +  + END SPECIFICATIONS +  +  +
C
C     check year
      FATAL = IYR .LT. 0
C     check month
      FATAL = FATAL .OR. (IMON .LT. 1)
      FATAL = FATAL .OR. (IMON .GT. 12)
C     check day
      FATAL = FATAL .OR. (IDAY .LT. 1)
      IF (.NOT. FATAL) THEN
C       check day
        GO TO (31,28,31,30,31,30,31,31,30,31,30,31), IMON
 28     CONTINUE
          IF ((MOD(IYR,4) .EQ. 0) .AND. (MOD(IYR,100) .NE. 0)) THEN
            FATAL = IDAY .GT. 29
          ELSE
            FATAL = IDAY .GT. 28
          ENDIF
          GO TO 200
 30     CONTINUE
          FATAL = IDAY .GT. 30
          GO TO 200
 31     CONTINUE
          FATAL = IDAY .GT. 31
 200    CONTINUE
      ENDIF
C
      IF (FATAL) THEN
C       bad date
        LNG  = LNGSTR(TEST)
        WRITE(MESAGE(1:10),2000) IDAY,IMON,IYR
        MESAGE = MESAGE(1:10) //
     1           ' - Invalid ' // TEST(1:LNG)
        IERROR = 1550
        CALL ERRCHK(IERROR,MESAGE,FATAL)
      ENDIF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ECHOGD
     I                   (FECHO, VALUE,
     M                    MESAGE)
C
C     +  +  + PURPOSE +  +  +
C
C     echoes global data input
C     Modification date: JAM
C
C     +  +  + DUMMY ARGUMENTS +  +  +
C
      INTEGER      FECHO
      CHARACTER*80 VALUE,MESAGE
C
C     +  +  + ARGUMENT DEFINITIONS +  +  +
C
C     FECHO  - echo file unit number
C     VALUE  - echo value
C     MESAGE - screen message
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      INTEGER      LTOTL
      CHARACTER*1  CHR
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL     ELPSE
C
C     +  +  + DATA INITIALIZATIONS +  +  +
C
      DATA CHR /'.'/
      DATA LTOTL / 79/
C
C     +  +  + OUTPUT FORMATS +  +  +
C
 2000 FORMAT(' ',A80)
C
C     +  +  + END SPECIFICATIONS +  +  +
C
      CALL ELPSE(VALUE,CHR,LTOTL,MESAGE)
      WRITE(FECHO,2000) MESAGE
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRZDAY
     I                   (IDAY0,IMON0,IYR0,IDAYN,IMONN,IYRN)
C
C     + + + PURPOSE + + +
C     load dates in PRZM common block
C     Modification date: 2/7/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 IDAY0, IMON0, IYR0, IDAYN, IMONN, IYRN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDAY0 - start day
C     IMON0 - start month
C     IYR0  - start year
C     IDAYN - end day
C     IMONN - end month
C     IYRN  - end year
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CMISC.INC'
C
C     + + + END SPECIFICATIONS + + +
C
      ISDAY = IDAY0
      ISMON = IMON0
      ISTYR = IYR0
      IEDAY = IDAYN
      IEMON = IMONN
      IEYR  = IYRN
C
      RETURN
      END
C
C
C
      SUBROUTINE   TDCALC
     I                   (ISDAY, ISMON, ISTYR,
     I                    IEDAY, IEMON, IEYR,
     O                    TNDAYS,LCOUNT,MCOUNT,KCOUNT)
C
C     + + + PURPOSE + + +
C     determine starting and ending days for daily loop,
C     also determine monthly KCOUNT - jam 4/16/91
C     Modification date: 2/7/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ISDAY,ISMON,ISTYR,IEDAY,IEMON,IEYR,TNDAYS,LCOUNT,KCOUNT,
     1          MCOUNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ISDAY - day on which to start simulation
C     ISMON - month on which to start simulation
C     ISTYR - year on which to start simulation
C     IEDAY - day on which to end simulation
C     IEMON - month on which to end simulation
C     IEYR  - year on which to end simulation
C     TNDAYS - total number of days of simulation
C     LCOUNT - non-leap year counter
C     MCOUNT - leap year counter
C     KCOUNT - total number of months in a simulation - 1
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CNDMO(2,13),FDAY,LDAY,
     1          ICNT,IY,LEAP
C
C     ICNT   - number of years between start year and end year
C     FDAY   - first day in simulation
C     LDAY   - last day in simulation
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  CNDMO/0,0,31,31,59,60,90,91,120,121,151,152,181,182,
     1          212,213,243,244,273,274,304,305,334,335,365,366/
C
C     + + + END SPECIFICATIONS + + +
C
      KCOUNT = 0
      ICNT   = 0
      LCOUNT = 0
      MCOUNT = 0
      TNDAYS = 0
C
      DO 200 IY = ISTYR, IEYR
        IF (MOD(IY,4) .NE. 0 .OR. MOD(IY,100) .EQ. 0) THEN
C         not leap year
          LEAP=1
          LDAY=365
        ELSE
C         leap year
          LEAP=2
          LDAY=366
        ENDIF
        IF (IY .EQ. IEYR) THEN
C         final year
          LDAY=IEDAY+CNDMO(LEAP,IEMON)
          FDAY=1
        ELSE IF (IY .EQ. ISTYR) THEN
C         first year
          FDAY=ISDAY+CNDMO(LEAP,ISMON)
        ELSE IF (IY .NE. IEYR .OR. IY .NE. ISTYR) THEN
C         intermediate year
          FDAY=CNDMO(LEAP,1)
          LDAY=LDAY-1
        ENDIF
        TNDAYS = TNDAYS + LDAY - FDAY + 1
        IF((LDAY-FDAY+1).EQ.365) THEN
C         full non-leap year
          LCOUNT = LCOUNT + 1
          IF ((IY .EQ. ISTYR) .OR. (IY .EQ. IEYR)) THEN
            ICNT = ICNT + 1
          ENDIF
        ENDIF
        IF((LDAY-FDAY+1).EQ.366) THEN
C         full leap year
          MCOUNT = MCOUNT + 1
          IF ((IY .EQ. ISTYR) .OR. (IY .EQ. IEYR)) THEN
            ICNT = ICNT + 1
          ENDIF
        ENDIF
 200  CONTINUE
C
C     new code added to make nldlt .le. 31 4-11-91 -jam 
      IF (ISTYR .EQ. IEYR) THEN
        KCOUNT = IEMON - ISMON
      ELSE
        KCOUNT = (LCOUNT+MCOUNT-ICNT) * 12 + (13 - ISMON) + IEMON - 1
      ENDIF
C     end of additions   -jam
C
      RETURN
      END
C
C
C
      SUBROUTINE   READM
     I                  (IN2,OUT,OUT2,MCMAX,NMAX,NEMP,NRMAX,NCMAX,NPMAX,
     M                   LARR,
     O                   VAR,SNAME,NDAT,MCVAR,BBT,PNAME,NVAR,NRUN,DIST,
     O                   PALPH,IND1,NAVG,INDZ)
C
C     + + + PURPOSE + + +
C     reads data required by the Monte Carlo shell
C     Modification date: 2/7/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IN2,OUT,OUT2,MCMAX,MCVAR,NVAR,NRUN,NMAX,
     1          NDAT(MCMAX),NEMP,LARR(NMAX),NRMAX,NCMAX,
     2          NPMAX,IND1(NMAX,2),NAVG(NMAX),INDZ(NMAX,2)
      REAL      VAR(MCMAX,5),DIST(NEMP,2,MCMAX),PALPH
      REAL*8    BBT(MCMAX,MCMAX)
      CHARACTER SNAME(NMAX,3)*20,PNAME(MCMAX)*20
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IN2   - ???
C     OUT   - ???
C     OUT2  - ???
C     MCMAX - ???
C     NMAX  - ???
C     NEMP  - ???
C     NRMAX - ???
C     NCMAX - ???
C     NPMAX - ???
C     LARR  - ???
C     VAR   - ???
C     SNAME - ???
C     NDAT  - ???
C     MCVAR - ???
C     BBT   - ???
C     PNAME - ???
C     NVAR  - ???
C     NRUN  - ???
C     DIST  - ???
C     PALPH - ???
C     IND1  - ???
C     NAVG  - ???
C     INDZ  - ???
C
C     + + + LOCAL VARIABLES + + +
      REAL         CORR
      INTEGER      IERR,NEVAR,NCDF,IND11,IND12,INDZ1,INDZ2,
     1             LNGL,LNGT,IERROR,I,J,IE,JE,K,JJ,KK
      CHARACTER*80 LINE,TITLE,NAME1*20,NAME2*20,MESAGE
      LOGICAL      FATAL,STATUS
C
C     + + + FUNCTIONS + + +
      INTEGER      LNGSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,COMRD3,NAMFIX,MCECHO,LFTJUS,ERRCHK,SUBOUT,
     1             LNGSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC    NINT,ABS
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A80)
 1100 FORMAT(7I5)
 1200 FORMAT(A20,2I5,5G10.0)
 1300 FORMAT(I5,F10.0)
 1400 FORMAT(A20,2I5,A20,2I5,F10.0)
 1500 FORMAT(A20,2I5,2A20,I5)
 1600 FORMAT(8F10.0)
C
C     + + + OUTPUT FORMATS + + +
 2000  FORMAT(1X,'A VARIABLE CORRELATED WITH ITSELF MUST HAVE A ',
     1          'CORRELATION COEFFICIENT OF 1. SEE ',A8)
 2100  FORMAT(1X,'THE CORRELATION COEFFICIENT MUST BE BETWEEN 0 AND 1',
     1          /,' SEE ',A8,' AND ',A8)
 2200  FORMAT(///2X,'**************ERROR IN MONTE CARLO INPUT',
     1        //5X,'THE NUMBER OF',1X,A30,1X,'IS GREATER THAN THE',
     2         /5X,'MAXIMUM OF',I5,1X,'ALLOWED BY ARRAY DIMENSIONS')
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'READM'
      CALL SUBIN(MESAGE)
C
C     initialize variables
      IERROR = 0
      DO 10 I=1,MCMAX
        DO 20 J=1,MCMAX
          BBT(I,J) = 0.
 20     CONTINUE
        BBT(I,I) = 1.
 10   CONTINUE
      STATUS = .TRUE.
      MCVAR  = 0
      IERR   = 0
C
C     read in title of run
      CALL COMRD3(IN2,LINE,STATUS)
      READ(LINE,1000,ERR=999)TITLE
C
C     read Monte Carlo control parameters
      CALL COMRD3(IN2,LINE,STATUS)
      READ(LINE,1300,ERR=999)NRUN,PALPH
      IF (NRUN .GT. NRMAX) THEN
        LINE   = 'MONTE CARLO RUNS'
        IERROR = 5050
        IERR   = NRMAX
        GO TO 997
      END IF
      IF (PALPH .LE. 0.0 .OR. PALPH .GE. 100) THEN
C       ???
        PALPH = 90
      ENDIF
C
C     read Monte Carlo parameters
      I = 0
      NEVAR = 0
   30 CONTINUE
        CALL COMRD3(IN2,LINE,STATUS)
        IF(STATUS) THEN
          I = I + 1
          IF(I .GT. MCMAX)THEN
            LINE   = 'MONTE CARLO INPUT VARIABLES'
            IERROR = 5060
            IERR   = MCMAX
            GO TO 997
          END IF
          READ(LINE,1200,ERR=999)PNAME(I),IND1(I,1),INDZ(I,1),VAR(I,1),
     1                          VAR(I,2),VAR(I,3),VAR(I,4),VAR(I,5)
C
C         check for reasonable zone index and reset if < 1
          IF (INDZ(I,1) .LT. 1) THEN
            INDZ(I,1) = 1
          ENDIF
C
          IF (NINT(VAR(I,5)) .EQ. 7) THEN
            NEVAR = NEVAR + 1
          ENDIF
C
C         left justify name
          CALL NAMFIX(LINE)
          PNAME(I) = LINE(1:20)
        ENDIF
      IF(STATUS) GOTO 30
      MCVAR = I
C
C     read empirical distributions
      IF (NEVAR .GT. 0) THEN
        DO 80 IE = 1,NEVAR
          CALL COMRD3(IN2,LINE,STATUS)
          READ(LINE,1100,ERR=999)NDAT(IE)
          IF(NDAT(IE).GT. NEMP)THEN
            LINE   = 'EMPIRICAL DIST. DATA POINTS'
            IERR = NEMP
            IERROR = 5070
            GO TO 997
          END IF
          DO 75 JE=1,NDAT(IE)
            CALL COMRD3(IN2,LINE,STATUS)
            READ(LINE,1600,ERR=999)DIST(JE,1,IE),DIST(JE,2,IE)
   75     CONTINUE
   80   CONTINUE
      END IF
C
C     read output variable names
      NVAR = 0
      NCDF = 0
   60 CONTINUE
        CALL COMRD3(IN2,LINE,STATUS)
        IF(STATUS) THEN
          NVAR = NVAR + 1
          IF (NVAR .GT. NMAX) THEN
            LINE   = 'MONTE CARLO OUTPUT VARIABLES'
            IERR = NMAX
            IERROR = 5080
            GO TO 997
          END IF
          READ(LINE,1500,END = 998,ERR=999) SNAME(NVAR,1),IND1(NVAR,2),
     1          INDZ(NVAR,2),SNAME(NVAR,2),SNAME(NVAR,3),NAVG(NVAR)
C
C         check for reasonable zone index and reset if < 1
          IF (INDZ(NVAR,2) .LT. 1) THEN
            INDZ(NVAR,2) = 1
          ENDIF
          IF(NAVG(NVAR) .GT. NPMAX)THEN
            LINE = 'DAYS IN OUTPUT AVG. PERIODS'
            IERR = NPMAX
            IERROR = 5090
            GO TO 997
          END IF
C
C         left justify and capitialize
          LINE = SNAME(NVAR,1)
          CALL NAMFIX(LINE)
          SNAME(NVAR,1) = LINE(1:20)
          LINE = SNAME(NVAR,2)
          CALL NAMFIX(LINE)
          SNAME(NVAR,2) = LINE(1:20)
          LINE = SNAME(NVAR,3)
          CALL NAMFIX(LINE)
          SNAME(NVAR,3) = LINE(1:20)
          IF(SNAME(NVAR,2)(1:3) .EQ. 'CDF') THEN
            NCDF = NCDF + 1
          ENDIF
          IF(NCDF .GT. NCMAX) THEN
            LINE   = 'REQUESTED OUTPUT CDFS'
            IERR   = NCMAX
            IERROR = 5100
            GO TO 997
          END IF
C
        ENDIF
      IF(STATUS) GOTO 60
C
C     read correlation matrix
   40 CONTINUE
        CALL COMRD3(IN2,LINE,STATUS)
        IF(STATUS) THEN
          READ(LINE,1400,ERR=999,IOSTAT=IERROR) NAME1,IND11,INDZ1,NAME2,
     1      IND12,INDZ2,CORR
C
C         check for reasonable zone index and reset if < 1
          IF (INDZ1 .LT. 1) THEN
            INDZ1 = 1
          ENDIF
          IF (INDZ2 .LT. 1) THEN
            INDZ2 = 1
          ENDIF
C
C         capitalize and left justify
          LINE  = NAME1
          CALL NAMFIX(LINE)
          NAME1 = LINE(1:20)
          LINE  = NAME2
          CALL NAMFIX(LINE)
          NAME2 = LINE(1:20)
C
          IF((NAME1 .EQ. NAME2) .AND. (IND11 .EQ. IND12) .AND.
     1                    (INDZ1.EQ.INDZ2)  .AND. CORR .LT. 1) THEN
            WRITE(OUT,2000) NAME1
            IERR = 1
          ENDIF
          IF(ABS(CORR) .GT. 1.)THEN
            WRITE(OUT,2100) NAME1,NAME2
            IERR = 1
          ENDIF
        ENDIF
C
C       organize correlation coefficient
        J = 0
        K = 0
  110   CONTINUE
          J = J + 1
          K = K+1
          IF(NAME1 .EQ. PNAME(J) .AND. IND11.EQ.IND1(J,1)
     1                           .AND. INDZ1.EQ.INDZ(J,1))THEN
            JJ = 0
            KK = 0
  120       CONTINUE
              JJ = JJ + 1
              KK = KK+1
              IF (NAME2 .EQ. PNAME(JJ) .AND. IND12.EQ.IND1(JJ,1)
     1                                .AND. INDZ2.EQ.INDZ(JJ,1)) THEN
                IF (NINT(VAR(J,5)*VAR(JJ,5)) .NE. 0) THEN
C                 if not a constant insert in correlation matrix
                  BBT(K,KK) = CORR
                  BBT(KK,K) = CORR
                ENDIF
                JJ = MCVAR
                J  = MCVAR
              ENDIF
            IF(JJ .LT. MCVAR) GOTO 120
          ENDIF
        IF(J .LT. MCVAR) GOTO 110
      IF(STATUS) GOTO 40
C
      IF(IERR .GT. 0) THEN
        WRITE(*,*) ' ERROR IN READING CORRELATION INPUTS'
      ENDIF
      CALL MCECHO(
     I            OUT,OUT2,MCVAR,VAR,SNAME,NVAR,MCMAX,NMAX,
     I            BBT,PNAME,NRUN,TITLE,LARR,IERR,IND1,NAVG,
     I            NEMP,NDAT,DIST,NEVAR,PALPH,INDZ)
      GO TO 3000
 997  CONTINUE
        WRITE(OUT,2200)LINE(1:30),IERR
        FATAL  = .TRUE.
        LNGL = LNGSTR(LINE)
C
C       reuse 'TITLE' for holding integer
        WRITE(TITLE,'(I10)') IERR
        CALL LFTJUS(TITLE)
        LNGT = LNGSTR(TITLE)
        MESAGE = 'The no. of [' // LINE(1:LNGL) //
     1    '] is greater than max. of ' // TITLE(1:LNGT)
        CALL ERRCHK(IERROR,MESAGE,FATAL)
 999  CONTINUE
        IF (IERROR .EQ. 0) IERROR = 5000
        MESAGE = 'Format error in reading Monte Carlo input file'
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
 998  CONTINUE
        IF (IERROR .EQ. 0) IERROR = 5010
        MESAGE = 'Premature end of Monte Carlo input file'
        FATAL  = .TRUE.
        CALL ERRCHK(IERROR,MESAGE,FATAL)
 3000 CONTINUE
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   MCECHO
     I                   (OUT,OUT2,MCVAR,VAR,SNAME,NVAR,MCMAX,NMAX,
     I                    BBT,PNAME,NRUN,TITLE,LARR,IERR,IND1,NAVG,
     I                    NEMP,NDAT,DIST,NEVAR,PALPH,INDZ)
C
C     + + + PURPOSE + + +
C     echoes Monte Carlo input data
C     Modification date: 2/7/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 OUT,OUT2,MCVAR,NVAR,MCMAX,NRUN,NMAX,LARR(NVAR),IERR,
     1          NEMP,NDAT(MCMAX),NEVAR,IND1(NMAX,2),
     2          NAVG(NMAX),INDZ(NMAX,2)
      REAL      VAR(MCMAX,5),DIST(NEMP,2,MCMAX),PALPH
      REAL*8    BBT(MCMAX,MCMAX)
      CHARACTER SNAME(NMAX,3)*20,PNAME(MCMAX)*20,TITLE*80
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OUT   - ???
C     OUT2  - ???
C     MCVAR - ???
C     VAR   - ???
C     SNAME - ???
C     NVAR  - ???
C     MCMAX - ???
C     NMAX  - ???
C     BBT   - ???
C     PNAME - ???
C     NRUN  - ???
C     TITLE - ???
C     LARR  - ???
C     IERR  - ???
C     IND1  - ???
C     NAVG  - ???
C     NEMP  - ???
C     NDAT  - ???
C     DIST  - ???
C     NEVAR - ???
C     PALPH - ???
C     INDZ  - ???
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER    NDIS(9)*11,LINE*360
      CHARACTER*80 MESAGE
      CHARACTER*7  DASH
      REAL         LNCHK, DUMR4,RMN,RCV
      REAL*8       EXPCHK, SQRCHK
      INTEGER*4    I,IE,JE,KOUNT,KK2,KK,KK1,ICOUNT,J
C
C     NDIS - Vector containing names of distributions
C
C     + + + EXTERNALS + + +
      EXTERNAL   SUBIN,SUBOUT,LNCHK,EXPCHK,SQRCHK
C
C     + + + INTRINSICS + + +
      INTRINSIC  NINT,INT,REAL,DBLE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA NDIS/'CONSTANT   ','NORMAL     ','LOG NORMAL ','EXPONENTIAL',
     1          'UNIFORM    ','SU         ','SB         ','EMPIRICAL  ',
     2          'TRIANGULAR '/
      DATA DASH/'-------'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(5(/),
     1             20X,'---------------- SHELL ------------------',
     2           //22X,'MONTE CARLO UNCERTAINTY ANALYSIS SHELL',
     3          ///34X,'DEVELOPED FOR',
     A           //22X,'U.S. ENVIRONMENTAL PROTECTION AGENCY',
     1            /24X,'ENVIRONMENTAL RESEARCH LABORATORY',
     2            /32X,'ATHENS, GA  30605',
     3           //35X,'MARCH 1992',
     4           //20X,'----------------------------------------',
     5              8(/),1X,A80)
 2100 FORMAT(//,T5,'I N P U T  D A T A',/,
     1          T5,'------------------')
 2150 FORMAT(//,T5,'NUMBER OF MONTE CARLO RUNS',T43,'= ',
     1I5,//,T5,'NUMBER OF MODEL INPUT PARAMETERS',/,T6,
     2'BEING VARIED',T43,'= ',I5,//,
     3     T5,'CONFIDENCE LEVEL FOR PERCENTILES',T43,'= ',F6.2,' %'/)
 2200 FORMAT('1',///,T47,'SUMMARY OF INPUT PARAMETERS',/,
     1               T47,'---------------------------',//,
     1'PARAMETER NAME ',T20,'IND1',T25,'INDZ',T30,
     1'INPUT MEAN',T41,'INPUT C.V.',
     2T53,'DIST. MEAN.', T67,'DIST. STD',T80,'MINIMUM VALUE',T95,
     3'MAXIMUM VALUE', T110,'DIST TYPE',/,
     1'--------',T20,'----',T25,'----',T30,'----------',T41,'---------'
     2,T53,'-----------',T67,'---------',T80,'-------------',T95,
     3'-------------',T110,'---------')
 2250 FORMAT(1X,A19,I3,T25,I3,T30,G10.4,T40,G10.4,T55,G11.4,T67,G10.4,
     1T80,G10.4,T97,G10.4,T110,A11)
 2300 FORMAT('1',/,/,/,10X,'CORRELATION MATRIX FOR INPUTS',/,
     1                 10X,'-----------------------------',/)
 2350 FORMAT(/)
 2370 FORMAT(1X,A8,1X,12F10.4,/)
 2400 FORMAT(////,10X,'"MODEL PARAMETERS FOR EACH MONTE CARLO RUN:"',//)
 2420 FORMAT(5X,'"MC"',1X,16(2X,'"',A7,'"'))
 2450 FORMAT(5X,'"','RUN','"',16(2X,'"',A7,'"'))
 2470 FORMAT(1X,'"-------"',16(2X,'"',A7,'"'))
 2500 FORMAT(2X,'"NUMBER"',1X,16(1X,'"',I3,':',I3,'"',1X))
 2520 FORMAT(1X,A19,I3,T25,I3,T30,G10.4,T110,A11)
 2550 FORMAT(1X,A19,I3,T25,I3,T80,G10.4,T97,G10.4,T110,A11)
 2570 FORMAT(1X,A19,I3,T25,I3,T30,G10.4,T80,G10.4,T97,G10.4,T110,A11)
 2600 FORMAT(////,T15,'OUTPUT VARIABLE     ','IND1 ','INDZ ',
     $            'STATISTICS', 14X,'OUTPUT',T77,'MOVING AVG'/,
     1            T15,'     NAMES      ',14X,'   FLAG   ',14X,' FLAG ',
     2            T77,'  PERIOD  ',/,
     3            T15,'------------------- ','----',1X,'----',1X,
     4            '----------', 14X,'------',T77,'----------')
 2630 FORMAT(T15,A20,I4,1X,I4,3X,A20,2X,A10,T80,I4)
 2660 FORMAT(T13,A120)
 2700 FORMAT(////////,17X,'M O N T E - C A R L O  O U T P U T',/
     1                17X,'----------------------------------')
 2720 FORMAT(//,5X,'********** ERROR IN INPUTING CORRELATION DATA')
 2750 FORMAT(   5X,'CHECK CORRELATION COEFFICIENTS')
 2800 FORMAT(   5X,'ONLY NORMAL AND LOGNORMAL VARIABLES CAN BE',
     1             ' CORRELATED')
 2850 FORMAT(////5X,'EMPIRICAL DISTRIBUTION NUMBER ',I5,
     1         //12X,'VALUE',13X,'CUM. PROB.')
 2900 FORMAT(10X,G10.4,10X,G10.4)
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'MCECHO'
      CALL SUBIN(MESAGE)
C
C     write title of run
      WRITE(OUT,2000) TITLE
      WRITE(OUT,2100)
C
C     write Monte Carlo control inputs
      WRITE(OUT,2150) NRUN,MCVAR,PALPH
      WRITE(OUT,2200)
C
      DO 10 I= 1,MCVAR
C
        IF (NINT(VAR(I,5)) .EQ. 1) THEN
C         normal distribution
          RMN=VAR(I,1)
          RCV=VAR(I,2)/VAR(I,1)
          WRITE(OUT,2250)PNAME(I),IND1(I,1),INDZ(I,1),RMN,RCV,VAR(I,1),
     1                  VAR(I,2),VAR(I,3),VAR(I,4),NDIS(INT(VAR(I,5)+1))
C
        ELSE IF(NINT(VAR(I,5)).EQ.2) THEN
C         log normal
          RMN=VAR(I,1)
          RCV=VAR(I,2)/VAR(I,1)
          VAR(I,2)=LNCHK(1. + (VAR(I,2)/VAR(I,1))**2)
          VAR(I,1)=LNCHK(VAR(I,1)) - .5*VAR(I,2)
          VAR(I,2) = REAL(SQRCHK(DBLE(VAR(I,2))))
          WRITE(OUT,2250)PNAME(I),IND1(I,1),INDZ(I,1),RMN,RCV,VAR(I,1),
     1                  VAR(I,2),VAR(I,3),VAR(I,4),NDIS(INT(VAR(I,5)+1))
C
        ELSE IF(NINT(VAR(I,5)).EQ.3) THEN
C         exponential
          RCV = 1.0
          WRITE(OUT,2250)PNAME(I),IND1(I,1),INDZ(I,1),VAR(I,1),RCV,
     1      VAR(I,1), VAR(I,1),VAR(I,3), VAR(I,4),NDIS(INT(VAR(I,5)+1))
C
        ELSE IF(NINT(VAR(I,5)).EQ.4) THEN
C         uniform
          WRITE(OUT,2550)PNAME(I),IND1(I,1),INDZ(I,1),VAR(I,3),
     1                  VAR(I,4),NDIS(INT(VAR(I,5)+1))
C
        ELSE IF(NINT(VAR(I,5)).EQ.5 .OR. NINT(VAR(I,5)).EQ.6) THEN
C         sb, su distribution
          RMN = VAR(I,1)
          RCV = VAR(I,2)/RMN
          WRITE(OUT,2250)PNAME(I),IND1(I,1),INDZ(I,1),RMN,RCV,VAR(I,1),
     1                VAR(I,2), VAR(I,3),VAR(I,4),NDIS(INT(VAR(I,5)+1))
C
        ELSE IF(NINT(VAR(I,5)).EQ.7) THEN
C         empirical
          WRITE(OUT,2550)PNAME(I),IND1(I,1),INDZ(I,1),VAR(I,3),
     1                  VAR(I,4),NDIS(INT(VAR(I,5)+1))
C
        ELSEIF(NINT(VAR(I,5)).EQ.8)THEN
C         triangular
          WRITE(OUT,2570)PNAME(I),IND1(I,1),INDZ(I,1),VAR(I,1),VAR(I,3),
     1                  VAR(I,4),NDIS(INT(VAR(I,5)+1))
        ELSE
          VAR(I,2) = 0.
          WRITE(OUT,2520)PNAME(I),IND1(I,1),INDZ(I,1),VAR(I,1),
     1                  NDIS(INT(VAR(I,5)+1))
        ENDIF
 10   CONTINUE
C
C     write empirical distributions
      IF (NEVAR .GT. 0)THEN
        DO 20 IE=1,NEVAR
          WRITE(OUT,2850)IE
          DO 15 JE=1,NDAT(IE)
            WRITE(OUT,2900)DIST(JE,1,IE),DIST(JE,2,IE)
   15     CONTINUE
   20   CONTINUE
      END IF
C
C     write output variables
      IF (NVAR .GT. 0) THEN
        WRITE(OUT,2600)
        DO 30 I=1,NVAR
          WRITE(OUT,2630) SNAME(I,1),IND1(I,2),INDZ(I,2),SNAME(I,2),
     1                  SNAME(I,3),  NAVG(I)
   30   CONTINUE
      ENDIF
C
C     write input correlation matrix
      LINE = ' '
      WRITE(OUT,2300)
C
C     write names of correlated parameters
      KOUNT = 1
      KK2   = -1
      DO 40 KK=1,MCVAR
        KK1 = KK2 + 3
        KK2 = KK1+7
        LINE(KK1:KK2) = PNAME(KK)
        KOUNT = KOUNT + 1
   40 CONTINUE
      WRITE(OUT,2660) LINE
      WRITE(OUT,2350)
C
C     write each row of correlation matrix
      ICOUNT = 0
      DO 50 I=1,MCVAR
        ICOUNT = ICOUNT + 1
        WRITE(OUT,2370)PNAME(I),(BBT(ICOUNT,J),J=1,KOUNT-1)
        WRITE(OUT,2350)
  50  CONTINUE
C
C     compute correlation parameters for log-normal distributions
      ICOUNT = 0
      DO 70 I=1,MCVAR
        IF(NINT(VAR(I,5)) .GT. 0) THEN
          ICOUNT = ICOUNT + 1
        ENDIF
        IF(NINT(VAR(I,5)) .EQ. 2)THEN
          KOUNT = 0
          DO 60 J=I+1,MCVAR
            IF(NINT(VAR(J,5)) .GT. 0) THEN
              KOUNT = KOUNT + 1
C
C             lognormal -- lognormal correlations
              IF(NINT(VAR(J,5)) .EQ. 2)THEN
                DUMR4 = REAL(BBT(ICOUNT,KOUNT))
                BBT(ICOUNT,KOUNT) = LNCHK(1. + DUMR4*
     1            REAL(SQRCHK(DBLE(EXPCHK(DBLE(VAR(I,2)**2)-1.))))*
     2            REAL(SQRCHK(DBLE(EXPCHK(DBLE(VAR(J,2)**2)-1.)))))/
     3            (VAR(I,2)*VAR(J,2))
                  BBT(KOUNT,ICOUNT) = BBT(ICOUNT,KOUNT)
C
C             lognormal -- normal correlations
              ELSE IF(NINT(VAR(J,5)) .EQ. 1)THEN
                BBT(ICOUNT,KOUNT) = BBT(ICOUNT,KOUNT)*
     1        REAL(SQRCHK(DBLE(EXPCHK(DBLE(VAR(I,2)**2)-1.)/VAR(I,2))))
                BBT(KOUNT,ICOUNT) = BBT(ICOUNT,KOUNT)
              ELSE
                END IF
            END IF
   60     CONTINUE
        END IF
   70 CONTINUE
C
C     write header for summary output
      WRITE(OUT,2700)
C
C     write headers for results of each Monte Carlo run
      WRITE(OUT2,2400)
      ICOUNT = 0
      DO 80 I=1,NVAR
        IF(SNAME(I,3)(1:5) .EQ. 'WRITE')THEN
          ICOUNT = ICOUNT + 1
          LARR(ICOUNT) = I
        END IF
   80 CONTINUE
      WRITE(OUT2,2420)(SNAME(LARR(J),1),J=1,ICOUNT)
      WRITE(OUT2,2450)(SNAME(LARR(J),1)(8:14),J=1,ICOUNT)
      WRITE(OUT2,2500)(IND1(LARR(J),2),INDZ(LARR(J),2),J=1,ICOUNT)
      WRITE(OUT2,2470)(DASH,J=1,ICOUNT)
C
      IF(IERR .GT. 0) THEN
        WRITE(OUT,2720)
        IF(IERR .EQ. 1) WRITE(OUT,2750)
        IF(IERR .EQ. 2) WRITE(OUT,2800)
      ENDIF
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   INITMC
     I                   (MCMAX,NVAR,NMAX,BBT,MCVAR,
     M                    STAT,CORR,
     O                    DECOM)
C
C     + + + PURPOSE + + +
C     initializes statistical summation arrays,
C     called after input of Monte Carlo data.
C     Modification date: 2/7/92 JAM
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MCVAR,MCMAX,NVAR,NMAX
      REAL*8    STAT(NMAX,6),CORR(NMAX,NMAX),
     1          BBT(MCMAX,MCMAX),DECOM(MCMAX,MCMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MCMAX - ???
C     NVAR  - ???
C     NMAX  - ???
C     BBT   - ???
C     MCVAR - ???
C     STAT  - ???
C     CORR  - ???
C     DECOM - ???
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,DECOMP,SUBOUT
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'INITMC'
      CALL SUBIN(MESAGE)
C
C     decompose input correlation matrix for Monte Carlo generation
      CALL DECOMP(
     I            MCVAR,MCMAX,
     M            BBT,DECOM)
C
C     initialize statistical summation arrays
      DO 20 I=1,NVAR
        STAT(I,1) = 0.0
        STAT(I,2) = 0.0
        STAT(I,3) = 0.0
        STAT(I,4) = 1.E20
        STAT(I,5) = -1.E20
        STAT(I,6) = 0.0
        DO 10 J=1,NVAR
          CORR(I,J) = 0.0
   10   CONTINUE
   20 CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   DECOMP
     I                   (N,NCARLO,
     M                    BBT,B)
C
C     + + + PURPOSE + + +
C     decomposes the matrix BBT (N by N) into
C     a lower triangular form using a Choleski Decomposition
C     method as modified by Lane
C     Modification date: 2/7/92 JAM
C
C     +  +  + DUMMY ARGUMENTS +  +  +
C
      INTEGER      N,NCARLO
      REAL*8       BBT(NCARLO,NCARLO),B(NCARLO,NCARLO)
C
C     +  +  + ARGUMENT DEFINITIONS +  +  +
C
C     N      - ???
C     NCARLO - ???
C     BBT    - ???
C     B      - ???
C
C     +  +  + LOCAL VARIABLES +  +  +
C
      REAL*8       SQRCHK
      REAL         H,P,HH,PP,DUMR4
      CHARACTER*80 MESAGE
      INTEGER      IERROR,I,J,L,K
      LOGICAL      FATAL
C
C     +  +  + EXTERNALS +  +  +
C
      EXTERNAL   SUBIN,ERRCHK,SUBOUT,SQRCHK
C
C     +  +  + INTRINSICS +  +  +
C
      INTRINSIC  REAL,DBLE
C
C     +  +  + OUTPUT FORMATS +  +  +
C
 2000 FORMAT(   'Subroutine DECOMP terminated, matrix BBT is not ',
     $'positive definite')
C
C     +  +  + END SPECIFICATIONS +  +  +
C
      MESAGE = 'DECOMP'
      CALL SUBIN(MESAGE)
C
      DO 101 I=1,N
        DO 100 J=1,N
          IF(I-J) 90,20,40
C           ???
 20         P=0.
            IF(I.EQ.1) GO TO 70
              L=J-1
              DO 30 K=1,L
                H=B(I,K)**2
C               ???
                P=P+H
 30           CONTINUE
 70         CONTINUE
            DUMR4= BBT(I,I)-P
            IF(BBT(I,J)-P) 72,71,71
C             ???
 71           B(I,I)= REAL(SQRCHK(DBLE(DUMR4)))
              GOTO 100
 40         HH=0.
            IF(J.EQ.1) GO TO 110
              L=J-1
              DO 60 K=1,L
                PP=  (B(J,K)*B(I,K))/B(J,J)
                HH =  HH  + PP
 60           CONTINUE
 110        B(I,J) = (BBT(I,J)/B(J,J))-HH
            GOTO 100
 90         B(I,J) = 0.
            GO TO 100
 72           CONTINUE
              IERROR = 5040
              WRITE(MESAGE,2000)
              FATAL  = .TRUE.
              CALL ERRCHK(IERROR,MESAGE,FATAL)
 100    CONTINUE
 101  CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   INIDAT
C
C     + + + PURPOSE + + +
C     Provides CNDMO and CMONTH values for COMMON BLOCK /CMISC/
C     Modification date: 2/7/92 JAM
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CMISC.INC'
C
C     + + + END SPECIFICATIONS + + +
C
      CNDMO(1,1)  = 0
      CNDMO(2,1)  = 0
      CNDMO(1,2)  = 31
      CNDMO(2,2)  = 31
      CNDMO(1,3)  = 59
      CNDMO(2,3)  = 60
      CNDMO(1,4)  = 90
      CNDMO(2,4)  = 91
      CNDMO(1,5)  = 120
      CNDMO(2,5)  = 121
      CNDMO(1,6)  = 151
      CNDMO(2,6)  = 152
      CNDMO(1,7)  = 181
      CNDMO(2,7)  = 182
      CNDMO(1,8)  = 212
      CNDMO(2,8)  = 213
      CNDMO(1,9)  = 243
      CNDMO(2,9)  = 244
      CNDMO(1,10) = 273
      CNDMO(2,10) = 274
      CNDMO(1,11) = 304
      CNDMO(2,11) = 305
      CNDMO(1,12) = 334
      CNDMO(2,12) = 335
      CNDMO(1,13) = 365
      CNDMO(2,13) = 366
C
      CMONTH(1)  = 'JAN.'
      CMONTH(2)  = 'FEB.'
      CMONTH(3)  = 'MAR.'
      CMONTH(4)  = 'APR.'
      CMONTH(5)  = 'MAY '
      CMONTH(6)  = 'JUNE'
      CMONTH(7)  = 'JULY'
      CMONTH(8)  = 'AUG.'
      CMONTH(9)  = 'SEP.'
      CMONTH(10) = 'OCT.'
      CMONTH(11) = 'NOV.'
      CMONTH(12) = 'DEC.'
C
      RETURN
      END 
C
C
C
      SUBROUTINE   FILINI
C
C     + + + PURPOSE + + +
C     initialize multiple segment file data
C     Modfification date: 2/14/92
C
C     + + + PARAMETERS + + +
      INCLUDE 'PMXZON.INC'
      INCLUDE 'PMXNSZ.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CFILEX.INC'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      FILOPC         = 0
C
      DO 10 I = 1, MXZONE
        FPRZIN(I) = -999
        FPRZRS(I) = -999
        FPRZOT(I) = -999
        FMETEO(I) = -999
        FSPTIC(I) = -999
        FTMSRS(I) = -999
   10 CONTINUE
C
      DO 20 I = 1, MXNSZO
        FVADIN(I) = -999
        FVADOT(I) = -999
        FVTP10(I) = -999
        FVRSTF(I) = -999
        FVRSTT(I) = -999
   20 CONTINUE
C
      FMCIN  = -999
      FMCOUT = -999
      FMCOU2 = -999
      FCON   = -999
      FECHO  = -999
      FWDMS  = -999
C
      RETURN
      END
C
C
C
      SUBROUTINE   THCALC
C
C     + + + PURPOSE + + +
C     computes filed capacity and wilting point for each soil layer
C     Modification date: 2/14/92 JAM
C
C     + + + PARAMETERS + + +
      INCLUDE 'PPARM.INC'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'CHYDR.INC'
      INCLUDE 'CPEST.INC'
      INCLUDE 'CMISC.INC'
C
C     + + + LOCAL VARIABLES + + +
      REAL         FCV(5),WPV(5)
      INTEGER      I
      CHARACTER*80 MESAGE
C
C     + + + EXTERNALS + + +
      EXTERNAL     SUBIN,SUBOUT
C
C     + + + DATA INITIALIZATIONS + + +
C     coefficients for linear regression equations for prediction
C     of soil water contents at -0.33 and -15.0 matric potentials
      DATA FCV/.3486, -.0018, .0039, .0228, -.0738/
      DATA WPV/.0854, -.0004, .0044, .0122, -.0182/
C
C     + + + END SPECIFICATIONS + + +
C
      MESAGE = 'THCALC'
      CALL SUBIN(MESAGE)
C
C     compute field capacity and wilting point
      DO 10 I=1,NHORIZ
        THEFC(I) = FCV(1)+ (FCV(2)*SAND(I))+ (FCV(3)*CLAY(I))+
     1             (FCV(4)*OC(I)*1.724)+ (FCV(5)*BD(I))
C
C       check that calculated THEFC is less than THETAS
        THEWP(I) = WPV(1)+ (WPV(2)*SAND(I))+ (WPV(3)*CLAY(I))+
     1             (WPV(4)*OC(I)*1.724)+ (WPV(5)*BD(I))
   10 CONTINUE
C
      CALL SUBOUT
C
      RETURN
      END

