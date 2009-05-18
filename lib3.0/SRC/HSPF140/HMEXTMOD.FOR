C
C
C
      MODULE HMEXTMOD
C
C     + + + PURPOSE + + +
C     module containing utility module EXTMOD
C
C     + + + HISTORY + + +
C     2009/05/07 PBD - initial implementation of module version of EXTMOD
C     2009/05/18 JLK - ongoing refinements
C
C     + + + MODULE VARIABLES + + +
      USE HMTABLEDEF
      USE HMTIMSERDEF
      IMPLICIT NONE
C
      INTEGER    PMXINP,PMXOUT,PMXSTA
      PARAMETER (PMXINP=20,PMXOUT=20,PMXSTA=20)
C      
      TYPE HTEXTMOD
        SEQUENCE
        INTEGER   EXTMODNO,NPT,NMN,MESSU
        REAL      DELT
        INTEGER   NDAY(12),YR,MON,DAY,HR,MIN,
     $            NDAYS,SPAFP,SPAKND,SPIVL,SPOPNO,SPOUT,UUNITS,
     $            SSMNAME(8),INNAME(8),PUNIT(2),BUNIT(2),OLEV(2),AREA,
     $            NINP,NOUT,NSTA,
     $         INPGROUP(3,PMXINP),INPNAME(3,PMXINP),INPUNITS(2,PMXINP),
     $         OUTGROUP(3,PMXOUT),OUTNAME(3,PMXOUT),OUTUNITS(2,PMXOUT),
     $         STAGROUP(3,PMXSTA),STANAME(3,PMXSTA),STAUNITS(2,PMXSTA),
     $         INPFP(PMXINP),OUTFP(PMXOUT),STAFP(PMXSTA)
        REAL      INPVAL(PMXINP,5),OUTVAL(PMXOUT,5),STAVAL(PMXSTA,5)
        INTEGER   SPNUND,EXTLST
      END TYPE HTEXTMOD
C
      CONTAINS   
C
C
C
      SUBROUTINE GETTABLEDEF_EXTMOD
     O                             (MOD_TABLE)
C             
      TYPE (HMODULE_TABLE_DEFINITION) MOD_TABLE
C
      ALLOCATE (MOD_TABLE%TABLE(8))
C
      CALL TABLE_SET("GEN-INFO",1,
     I               101,101,301,301,
     I               4,
     I               "ExtMod-opn",
     I               "    # -  #<SSM-Name----------------------> " // 
     I               "<Instance-Name-----------------> File ","","",
     I               "(1X,8A4,1X,8A4,I5)",
     I               " <SSM-Name----------------------> " //
     I               "<Instance-Name-----------------> File","","","",
     I               "General Information about each EXTMOD operation.",
     I               "","","","","","","","","",
     M               MOD_TABLE%TABLE(1))
C
      CALL TABLE_FIELD_SET(10,1,-999,-999,-999,
     I               "C",
     I               "EXTMOD operations to which these data apply.",
     M               MOD_TABLE%TABLE(1)%FIELD(1))
      CALL TABLE_FIELD_SET(32,11,-999,-999,-999,
     I               "C",
     I               "Small-scale model name.",
     M               MOD_TABLE%TABLE(1)%FIELD(2))
      CALL TABLE_FIELD_SET(32,44,-999,-999,-999,
     I               "C",
     I               "Small-scale model instance name.",
     M               MOD_TABLE%TABLE(1)%FIELD(3))
C
      CALL TABLE_SET("OUT-INFO",1,
     I               102,102,302,302,
     I               7,
     I               "ExtMod-opn",
     I               "    # -  # Engl Metr Engl Metr  Prt  Bin ",
     I               "","",
     I               "(6I5)",
     I               " Engl Metr Engl Metr  Prt  Bin ","","","",
     I               "Print and binary output units and output levels.",
     I               "","","","","","","","","",
     M               MOD_TABLE%TABLE(2))
C
      CALL TABLE_FIELD_SET(10,1,-999,-999,-999,
     I               "C",
     I               "EXTMOD operations to which these data apply.",
     M               MOD_TABLE%TABLE(2)%FIELD(1))
      CALL TABLE_FIELD_SET(5,11,1,99,0,
     I               "I",
     I               "Print output unit number English.",
     M               MOD_TABLE%TABLE(2)%FIELD(2))
      CALL TABLE_FIELD_SET(5,16,1,99,0,
     I               "I",
     I               "Print output unit number Metric.",
     M               MOD_TABLE%TABLE(2)%FIELD(3))
      CALL TABLE_FIELD_SET(5,21,1,99,0,
     I               "I",
     I               "Binary output unit number English.",
     M               MOD_TABLE%TABLE(2)%FIELD(4))
      CALL TABLE_FIELD_SET(5,26,1,99,0,
     I               "I",
     I               "Binary output unit number Metric.",
     M               MOD_TABLE%TABLE(2)%FIELD(5))
      CALL TABLE_FIELD_SET(5,31,1,10,3,
     I               "I",
     I               "Print output level.",
     M               MOD_TABLE%TABLE(2)%FIELD(6))
      CALL TABLE_FIELD_SET(5,36,1,10,3,
     I               "I",
     I               "Binary output level.",
     M               MOD_TABLE%TABLE(2)%FIELD(7))
C
      CALL TABLE_SET("AREA-INFO",1,
     I               103,103,303,303,
     I               2,
     I               "ExtMod-opn",
     I               "    # -  #      Area ",
     I               "","",
     I               "(F10.3)",
     I               "      Area ","","","",
     I               "Area associated with this instance.",
     I               "","","","","","","","","",
     M               MOD_TABLE%TABLE(3))
C
      CALL TABLE_FIELD_SET(10,1,-999,-999,-999,
     I               "C",
     I               "EXTMOD operations to which these data apply.",
     M               MOD_TABLE%TABLE(3)%FIELD(1))
      CALL TABLE_FIELD_SETR(10,11,0.0,-999.0,1.0,
     I               "R",
     I               "Area associated with this instance.",
     M               MOD_TABLE%TABLE(3)%FIELD(2))
C
      CALL TABLE_SET("TIMESERIES",1,
     I               104,104,304,304,
     I               4,
     I               "ExtMod-opn",
     I               "    x -  x  NIN NOUT NSTA","","",
     I               "(1X,3I10)",
     I               "       NIN      NOUT      NSTA","","","",
     I               "Number of time series to be used " //
     $                  "in the Extmod Operation.",
     I               "","","","","","","","","",
     M               MOD_TABLE%TABLE(4))
C
      CALL TABLE_FIELD_SET(10,1,-999,-999,-999,
     I               "C",
     I               "EXTMOD operations to which these data apply.",
     M               MOD_TABLE%TABLE(4)%FIELD(1))
      CALL TABLE_FIELD_SET(5,11,0,20,0,
     I               "I",
     I               "NIN is the number of influx time " //
     $                 "series to be used.",
     M               MOD_TABLE%TABLE(4)%FIELD(2))
      CALL TABLE_FIELD_SET(5,16,0,20,0,
     I               "I",
     I               "NOUT is the number of outflux time " //
     $                 "series to be used.",
     M               MOD_TABLE%TABLE(4)%FIELD(3))
      CALL TABLE_FIELD_SET(5,21,0,20,0,
     I               "I",
     I               "NSTA is the number of state var time " //
     $                 "series to be used.",
     M               MOD_TABLE%TABLE(4)%FIELD(4))
C
      CALL TABLE_SET("INFLUX",20,
     I               105,105,305,305,
     I               4,
     I               "ExtMod-opn",
     I           "    x -  x<--Group---> <---Name---> <Units->","","",
     I               "(1X,3A4,1X,3A4,1X,2A4)",
     I               "<--Group---> <---Name---> <Units->","","","",
     I           "Details of each input flux timeseries to be used " //
     $                  "in the Extmod Operation.",
     I               "","","","","","","","","",
     M               MOD_TABLE%TABLE(5))
C
      CALL TABLE_FIELD_SET(10,1,-999,-999,-999,
     I               "C",
     I               "EXTMOD operations to which these data apply.",
     M               MOD_TABLE%TABLE(5)%FIELD(1))
      CALL TABLE_FIELD_SET(12,11,-999,-999,-999,
     I               "C",
     I               "Input flux group.",
     M               MOD_TABLE%TABLE(5)%FIELD(2))
      CALL TABLE_FIELD_SET(12,24,-999,-999,-999,
     I               "C",
     I               "Input flux name.",
     M               MOD_TABLE%TABLE(5)%FIELD(3))
      CALL TABLE_FIELD_SET(8,37,-999,-999,-999,
     I               "C",
     I               "Input flux units.",
     M               MOD_TABLE%TABLE(5)%FIELD(4))
C
      CALL TABLE_SET("OUTFLUX",20,
     I               106,106,306,306,
     I               4,
     I               "ExtMod-opn",
     I               "    x -  x<--Group---> <---Name---> Units","","",
     I               "(1X,3A4,1X,3A4,1X,2A4)",
     I               "<--Group---> <---Name---> <Units->","","","",
     I           "Details of each output flux timeseries to be used " //
     $                  "in the Extmod Operation.",
     I               "","","","","","","","","",
     M               MOD_TABLE%TABLE(6))
C
      CALL TABLE_FIELD_SET(10,1,-999,-999,-999,
     I               "C",
     I               "EXTMOD operations to which these data apply.",
     M               MOD_TABLE%TABLE(6)%FIELD(1))
      CALL TABLE_FIELD_SET(12,11,-999,-999,-999,
     I               "C",
     I               "Output flux group.",
     M               MOD_TABLE%TABLE(6)%FIELD(2))
      CALL TABLE_FIELD_SET(12,24,-999,-999,-999,
     I               "C",
     I               "Output flux name.",
     M               MOD_TABLE%TABLE(6)%FIELD(3))
      CALL TABLE_FIELD_SET(8,37,-999,-999,-999,
     I               "C",
     I               "Output flux units.",
     M               MOD_TABLE%TABLE(6)%FIELD(4))
C
      CALL TABLE_SET("STATE",20,
     I               107,107,307,307,
     I               4,
     I               "ExtMod-opn",
     I               "    x -  x<--Group---> <---Name---> Units","","",
     I               "(1X,3A4,1X,3A4,1X,2A4)",
     I               "<--Group---> <---Name---> <Units->","","","",
     I           "Details of each state var timeseries to be used " //
     $                  "in the Extmod Operation.",
     I               "","","","","","","","","",
     M               MOD_TABLE%TABLE(7))
C
      CALL TABLE_FIELD_SET(10,1,-999,-999,-999,
     I               "C",
     I               "EXTMOD operations to which these data apply.",
     M               MOD_TABLE%TABLE(7)%FIELD(1))
      CALL TABLE_FIELD_SET(12,11,-999,-999,-999,
     I               "C",
     I               "State variable group.",
     M               MOD_TABLE%TABLE(7)%FIELD(2))
      CALL TABLE_FIELD_SET(12,24,-999,-999,-999,
     I               "C",
     I               "State variable name.",
     M               MOD_TABLE%TABLE(7)%FIELD(3))
      CALL TABLE_FIELD_SET(8,37,-999,-999,-999,
     I               "C",
     I               "State variable units.",
     M               MOD_TABLE%TABLE(7)%FIELD(4))
C
      MOD_TABLE%TABLE(8)%NAME = "END EXTMOD"
      MOD_TABLE%TABLE(8)%DIM = 0
C
      END SUBROUTINE            
C
C
C
      SUBROUTINE GETTSDEF_EXTMOD
     O                          (MOD_DEFN)
C
      TYPE (HMODULE_TIMSER_DEFINITION) MOD_DEFN   
C      
      INTEGER      BASEADDR, ITMP, SBPTR, ADDR
      TYPE(HTEXTMOD) HEXTMOD
C      
      MOD_DEFN%NAME   = "EXTMOD"
      MOD_DEFN%CLUSTER= 152
      ALLOCATE (MOD_DEFN%GROUP(3))
C
      BASEADDR = LOC(HEXTMOD%EXTMODNO)
C
      MOD_DEFN%GROUP(1)%NAME  = "INPUT"
      MOD_DEFN%GROUP(1)%OFFSET= 10
      ALLOCATE (MOD_DEFN%GROUP(1)%MEMBER(1))
C      
      ITMP = LOC(HEXTMOD%NINP)
      SBPTR= -(1+ ((ITMP - BASEADDR)/4))
      ITMP = LOC(HEXTMOD%INPFP)
      ADDR = 1+ ((ITMP - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("TIMSER",
     I                       PMXINP,SBPTR,1,2,0,0,2,0,ADDR,
     I                       "input time series",
     M                       MOD_DEFN%GROUP(1)%MEMBER(1))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(1)%MEMBER(1)%LTVAL)
C      
      MOD_DEFN%GROUP(2)%NAME  = "OUTPUT"
      MOD_DEFN%GROUP(2)%OFFSET= 20
      ALLOCATE (MOD_DEFN%GROUP(2)%MEMBER(1))
C      
      ITMP = LOC(HEXTMOD%NOUT)
      SBPTR= -(1+ ((ITMP - BASEADDR)/4))
      ITMP = LOC(HEXTMOD%OUTFP)
      ADDR = 1+ ((ITMP - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("TIMSER",
     I                       PMXOUT,SBPTR,1,2,0,0,2,0,ADDR,
     I                       "output time series",
     M                       MOD_DEFN%GROUP(2)%MEMBER(1))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(2)%MEMBER(1)%LTVAL)
C
      MOD_DEFN%GROUP(3)%NAME = "STATE"
      MOD_DEFN%GROUP(3)%OFFSET= 30
      ALLOCATE (MOD_DEFN%GROUP(3)%MEMBER(1))
C      
      ITMP = LOC(HEXTMOD%NSTA)
      SBPTR= -(1+ ((ITMP - BASEADDR)/4))
      ITMP = LOC(HEXTMOD%STAFP)
      ADDR = 1+ ((ITMP - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("TIMSER",
     I                       PMXSTA,SBPTR,1,1,0,0,0,0,ADDR,
     I                       "state variable time series",
     M                       MOD_DEFN%GROUP(3)%MEMBER(1))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(3)%MEMBER(1)%LTVAL)
C      
      MOD_DEFN%INITIALIZED = .TRUE.
C      
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   PEXTMOD
     I                   (NDELT,SDATIM,NDAMON,EMFG,TABLEDEFS, 
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the EXTMOD module
C
C     + + + HISTORY + + +
C     2009/05/07 PBD - initial implementation of module version of EXTMOD
C
C     + + + MODULES + + +
      USE       HIOOSV 
      USE HMMSG, ONLY : OMSTI,OMSG,OMSTC
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG, OSVKEY
      TYPE (HMODULE_TABLE_DEFINITION) :: TABLEDEFS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS  + + +
      INCLUDE   'crin2.inc'
C
C     + + + MODULE VARIABLES + + +
      TYPE(HTEXTMOD)  HEXTMOD     
      POINTER   (P,HEXTMOD)
      POINTER   (PDATIM,DATIM),(POSV,OSV)
      INTEGER    DATIM(5),OSV(*)
C
C     + + + LOCAL VARIABLES + + +   
      INTEGER    I,I0,I1,I2,OSVKND,OSVKST,OSVREC,BASEADDR,ADDR,
     $           IVAL(17),J,TBNO,TBSB,NVAL
      REAL       RVAL(1)
      TYPE(HMODULE_TABLE) :: TABLEDEF
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLE,RTABLE,DAYMNH,HSCKFL,HSCKFLX
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),/,' ','PROCESSING EXTMOD OPERATION NO. ',
     $        I4)
 2010 FORMAT (/,' FINISHED PROCESSING EXTMOD OPERATION NO. ',I4,
     $        /,' ',132('+'))
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
      I2= 2
C
      BASEADDR= LOC(HEXTMOD%EXTMODNO)
      ADDR    = LOC(HEXTMOD%EXTLST)
      I       = ((ADDR - BASEADDR)/4) -1
      OSVREC  = 1+ (I/500)
      OSVKST  = OSVKEY+ 1
      OSVKND  = OSVKEY+ OSVREC
C
C     point the osv to its memory location
      P     = LOC(OSVM(1,OSVKST))
      PDATIM= LOC(HEXTMOD%YR)    
      POSV  = LOC(HEXTMOD)
C     record the keys in opntab
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (RESMFG.EQ.1) THEN
C       read the general part of the osv from osvfl
C       - not implemented in this release of hspf
      ELSE
C       initialize the entire osv for this operation
        DO 10 I= 1, OSVREC*500
          OSV(I)= -999
 10     CONTINUE
        HEXTMOD%MESSU = FILE(1)
C
        IF (OUTLEV.GT.0) THEN
          WRITE (HEXTMOD%MESSU,2000) OPTNO
        END IF
C
        HEXTMOD%EXTMODNO= OPTNO
        HEXTMOD%DELT    =  NDELT
        DO 20 I= 1, 12
          HEXTMOD%NDAY(I)= NDAMON(I)
 20     CONTINUE
        DO 30 I= 1, 5
          DATIM(I)= SDATIM(I)
 30     CONTINUE
C       following value is assigned so that subroutine addtim will
C       work ok
        HEXTMOD%NDAYS = DAYMNH (HEXTMOD%YR,HEXTMOD%MON,HEXTMOD%NDAY)
        HEXTMOD%SPIVL = 0
        HEXTMOD%SPOPNO= OPNO
        HEXTMOD%UUNITS= EMFG
C
C       initialize current values - todo - make this a flux reset
        DO 40 I= 1, PMXINP
          HEXTMOD%INPVAL(I,1)= -1.0E30
 40     CONTINUE
        DO 50 I= 1, PMXOUT
          HEXTMOD%OUTVAL(I,1)= -1.0E30
 50     CONTINUE
        DO 60 I= 1, PMXSTA
          HEXTMOD%STAVAL(I,1)= -1.0E30
 60     CONTINUE
      END IF
C
C     process table - type gen-info
C       SSM name, eg 'Pat's Sediment Model'
C       instance name, eg 'Site 5'
C       unit number of SSM output time series file
      TBNO= 1
      TBSB= 1
      NVAL= 16
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M             IVAL)
C
      DO 110 J= 1,8
        HEXTMOD%SSMNAME(J)= IVAL(J)
 110  CONTINUE
      DO 120 J= 1,8
        HEXTMOD%INNAME(J)= IVAL(8+J)
 120  CONTINUE
C
C     process table - type out-info
C       unit numbers for print and binary output reports, eng and metric
C       output levels for print and binary output reports
      TBNO= 2
      TBSB= 1
      NVAL= 6
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M             IVAL)
C
      DO 140 J= 1,2
        HEXTMOD%PUNIT(J)= IVAL(J)
        HEXTMOD%BUNIT(J)= IVAL(2+J)
        HEXTMOD%OLEV(J) = IVAL(4+J)
 140  CONTINUE
C
C     check output files - if not open, open them with standard name
      DO 150 J= 1,2
        IF (HEXTMOD%PUNIT(J) .GT. 0) THEN
          CALL HSCKFL
     I                (HEXTMOD%PUNIT(J))
        END IF
        IF (HEXTMOD%BUNIT(J) .GT. 0) THEN
C         check binary output files 
          CALL HSCKFLX
     I                 (I0,HEXTMOD%PUNIT(J))
        END IF
 150  CONTINUE
C
C     process table - type area-info
C       area associated with SSM instance
      TBNO= 3
      TBSB= 1
      NVAL= 1
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      CALL RTABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M             RVAL)
C
      HEXTMOD%AREA = RVAL(1)
C
C     process table - type timeseries
C       number of inputs, outputs and state vars
      TBNO= 4
      TBSB= 1
      NVAL= 3
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M             IVAL)
      HEXTMOD%NINP= IVAL(1)
      HEXTMOD%NOUT= IVAL(2)
      HEXTMOD%NSTA= IVAL(3)
C
C     process table - type influx
C       details about inflows
      TBNO= 5
      NVAL= 8
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      DO 190 TBSB = 1, HEXTMOD%NINP
        CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M               IVAL)
        DO 170 J= 1,3
          HEXTMOD%INPGROUP(J,TBSB)= IVAL(J)
          HEXTMOD%INPNAME(J,TBSB) = IVAL(J+3)
 170    CONTINUE
        DO 180 J= 1,2
          HEXTMOD%INPUNITS(J,TBSB)= IVAL(J+6)
 180    CONTINUE    
 190  CONTINUE
C 
C     process table - type outflux
C       details about outflows
      TBNO= 6
      NVAL= 8
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      DO 220 TBSB = 1, HEXTMOD%NOUT
        CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M               IVAL)
        DO 200 J= 1,3
          HEXTMOD%OUTGROUP(J,TBSB)= IVAL(J)
          HEXTMOD%OUTNAME(J,TBSB) = IVAL(J+3)
 200    CONTINUE
        DO 210 J= 1,2
          HEXTMOD%OUTUNITS(J,TBSB)= IVAL(J+6)
 210    CONTINUE    
 220  CONTINUE
C
C     process table - type state
C       details about state variables
      TBNO= 7
      NVAL= 8
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      DO 250 TBSB = 1, HEXTMOD%NSTA
        CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M               IVAL)
        DO 230 J= 1,3
          HEXTMOD%STAGROUP(J,TBSB)= IVAL(J)
          HEXTMOD%STANAME(J,TBSB) = IVAL(J+3)
 230    CONTINUE
        DO 240 J= 1,2
          HEXTMOD%STAUNITS(J,TBSB)= IVAL(J+6)
 240    CONTINUE    
 250  CONTINUE
C
      IF (OUTLEV.GT.0) THEN
        WRITE (HEXTMOD%MESSU,2010)  OPTNO
      END IF
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   EXTMOD
     I                   (STIVL,WIDTH,OSVKST)
C
C     + + + PURPOSE + + +
C     Copy a set of point- and/or mean-valued time series
C
C     + + + HISTORY + + +
C     2009/05/07 PBD - initial implementation of module version of EXTMOD
C
C     + + + MODULES + + +
      USE       HIOOSV 
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STIVL,WIDTH,OSVKST
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STIVL  - in inpad row
C     WIDTH  - of inpad row
C     OSVKST - starting block in osv
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cmpad.inc'
      INCLUDE   'cmosv.inc'
C
C     + + + MODULE VARIABLES + + +
      TYPE(HTEXTMOD)  HEXTMOD     
      POINTER    (P,HEXTMOD)
      POINTER    (PNUMBR,NUMBR),(PDATIM,DATIM) 
      INTEGER     NUMBR(2),DATIM(5)   
C       
C     + + + LOCAL VARIABLES + + +
      INTEGER     IVL,IVL1,N,IDELT,PIVLNO,
     #            NXTMON,HRFG,DAYFG,EDAYFG,EMONFG,EPYRFG,  
     #            I1,I12
      CHARACTER*6 OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL    ADDTIM,SPECL,UPQUAN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        OPTYP/'EXTMOD'/
C
C     + + + END SPECIFICATIONS + + +
C
C     point to where the osv for this operation is in memory
      P     = LOC(OSVM(1,OSVKST))
      PDATIM= LOC(HEXTMOD%YR)    
C      
      IVL   = STIVL- 1
      IVL1  = STIVL
      IDELT = HEXTMOD%DELT
      PIVLNO= 0
      I1    = 1
      I12   = 12
C
      IF (STIVL .EQ. 1) THEN
C       TODO: is this section needed?      
        IF (HEXTMOD%NINP .GT. 0) THEN
C         put initial values of input time series into osv
          DO 10 N= 1,HEXTMOD%NINP
            IF (HEXTMOD%INPFP(N) .GT. 0) THEN
              HEXTMOD%INPVAL(N,1)= PAD(HEXTMOD%INPFP(N)+IVL1)
            END IF
 10       CONTINUE
        END IF
        IF (HEXTMOD%NOUT .GT. 0) THEN
C         put initial values of output time series into osv
          DO 20 N= 1,HEXTMOD%NOUT
            IF (HEXTMOD%OUTFP(N) .GT. 0) THEN
              HEXTMOD%OUTVAL(N,1)= PAD(HEXTMOD%OUTFP(N)+IVL1)
            END IF
 20       CONTINUE
        END IF
        IF (HEXTMOD%NSTA .GT. 0) THEN
C         put initial values of state variable time series into osv
          DO 30 N= 1,HEXTMOD%NSTA
            IF (HEXTMOD%STAFP(N) .GT. 0) THEN
              HEXTMOD%STAVAL(N,1)= PAD(HEXTMOD%STAFP(N)+IVL1)
            END IF
 30       CONTINUE
        END IF
      END IF
C
C     time loop
      DO 130 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1         = IVL1+ 1
        HEXTMOD%SPIVL= HEXTMOD%SPIVL+ 1
C
C       increment date/time
        CALL ADDTIM (IDELT,HEXTMOD%NDAY,I1,I12,
     M               DATIM,PIVLNO,
     O               HEXTMOD%NDAYS,NXTMON,HRFG,DAYFG,EDAYFG,
     O               EMONFG,EPYRFG)
C
        IF (HEXTMOD%SPAFP .GT. 0 .AND. 
     $      HEXTMOD%SPAFP .LE. HEXTMOD%SPAKND) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,HEXTMOD%EXTMODNO,
     I                HEXTMOD%SPAKND,HEXTMOD%SPOPNO,
     I                DATIM,HEXTMOD%MESSU,HEXTMOD%SPIVL,
     I                HEXTMOD%SPOUT,HEXTMOD%SPNUND,
     M                HEXTMOD%SPAFP)
        END IF
C
        IF (HEXTMOD%NINP .GT. 0) THEN
C         put values of input time series into osv
          DO 40 N= 1,HEXTMOD%NINP
            IF (HEXTMOD%INPFP(N) .GT. 0) THEN
              HEXTMOD%INPVAL(N,1)= PAD(HEXTMOD%INPFP(N)+IVL1)
            END IF
 40       CONTINUE
        END IF
        IF (HEXTMOD%NOUT .GT. 0) THEN
C         put values of output time series into osv
          DO 50 N= 1,HEXTMOD%NOUT
            IF (HEXTMOD%OUTFP(N) .GT. 0) THEN
              HEXTMOD%OUTVAL(N,1)= PAD(HEXTMOD%OUTFP(N)+IVL1)
            END IF
 50       CONTINUE
        END IF
        IF (HEXTMOD%NSTA .GT. 0) THEN
C         put values of state variable time series into osv
          DO 60 N= 1,HEXTMOD%NSTA
            IF (HEXTMOD%STAFP(N) .GT. 0) THEN
              HEXTMOD%STAVAL(N,1)= PAD(HEXTMOD%STAFP(N)+IVL1)
            END IF
 60       CONTINUE
        END IF
C
C       accumulators and reports go here
        IF (EMONFG .EQ. 1) THEN
          WRITE(HEXTMOD%PUNIT(1),*) 
     1          'EXTMOD',HEXTMOD%EXTMODNO,'DONE ', DATIM(1), DATIM(2)
        END IF
C
C       what gets passed to the rest of HSPF???        
C          just output or any input/state/output?
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (HEXTMOD%SPIVL,HEXTMOD%SPOPNO)
C
 130  CONTINUE
C
      RETURN
      END SUBROUTINE
C    
      END MODULE HMEXTMOD  
               