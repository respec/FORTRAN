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
      USE HMTABLEDEF, ONLY : TABLE_FIELD_SET, TABLE_FIELD_SETR, 
     $                       HMODULE_TABLE_DEFINITION, HMODULE_TABLE,
     $                       TABLE_SET
      USE HMTIMSERDEF
      IMPLICIT NONE
C
      INTEGER    PMXINP,PMXOUT,PMXSTA
      PARAMETER (PMXINP=20,PMXOUT=20,PMXSTA=20)
C      
      TYPE HTEXTMOD
        SEQUENCE
        INTEGER      EXTMODNO,MESSU
        REAL         DELT
        INTEGER      NDAY(12),YR,MON,DAY,HR,MIN,
     $               NDAYS,SPAFP,SPAKND,SPIVL,SPOPNO,SPOUT,PFLAG,IDUM,
     $               UUNITS,NXTMON,HRFG,DAYFG,EDAYFG,EMONFG,EPYRFG
        CHARACTER*32 SSMNAME,INNAME
        INTEGER      PUNIT(2),BUNIT(2),OLEV(2),PIVL(2),PYREND(2)
        REAL         AREA
        INTEGER      PIVLNO,BIVLNO,NINP,NOUT,NSTA
        INTEGER      UNIQUEGROUPCOUNT
        INTEGER      BFLAG(PMXINP+PMXOUT+PMXSTA)
        CHARACTER*12 UNIQUEGROUPS(PMXINP+PMXOUT+PMXSTA)
        CHARACTER*12 INPGROUP(PMXINP),INPNAME(PMXINP)
        CHARACTER*8  INPUNITS(PMXINP)
        CHARACTER*12 OUTGROUP(PMXOUT),OUTNAME(PMXOUT)
        CHARACTER*8  OUTUNITS(PMXOUT)
        CHARACTER*12 STAGROUP(PMXSTA),STANAME(PMXSTA)
        CHARACTER*8  STAUNITS(PMXSTA)
        INTEGER      FRMINFP(PMXINP),FRMOUFP(PMXOUT),FRMSTFP(PMXSTA),
     $               TOINFP(PMXINP),TOOUFP(PMXOUT),TOSTFP(PMXSTA)
        REAL         INPVAL(PMXINP,5),OUTVAL(PMXOUT,5),STAVAL(PMXSTA,5)
        INTEGER      SPNUND,EXTLAST
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
     I               9,
     I            "ExtMod-opn",
     I            "    # -  # Engl Metr Engl Metr  Prt  Bin Pivl Bivl",
     I            "","",
     I            "(8I5)",
     I            " Engl Metr Engl Metr  Prt  Bin   Pivl Bivl","","","",
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
      CALL TABLE_FIELD_SET(5,41,1,10,1,
     I               "I",
     I               "Print output interval.",
     M               MOD_TABLE%TABLE(2)%FIELD(8))
      CALL TABLE_FIELD_SET(5,46,1,10,1,
     I               "I",
     I               "Binary output interval.",
     M               MOD_TABLE%TABLE(2)%FIELD(9))
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
      ALLOCATE (MOD_DEFN%GROUP(2))
C
      BASEADDR = LOC(HEXTMOD%EXTMODNO)
C
      MOD_DEFN%GROUP(1)%NAME  = "FRMEXT"
      MOD_DEFN%GROUP(1)%OFFSET= 10
      ALLOCATE (MOD_DEFN%GROUP(1)%MEMBER(3))
C      
      ITMP = LOC(HEXTMOD%NINP)
      SBPTR= -(1+ ((ITMP - BASEADDR)/4))
      ITMP = LOC(HEXTMOD%FRMINFP)
      ADDR = 1+ ((ITMP - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("INPFLX",
     I                       PMXINP,SBPTR,1,2,0,0,2,0,ADDR,
     I                       "input time series",
     M                       MOD_DEFN%GROUP(1)%MEMBER(1))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(1)%MEMBER(1)%LTVAL)
C      
      ITMP = LOC(HEXTMOD%NOUT)
      SBPTR= -(1+ ((ITMP - BASEADDR)/4))
      ITMP = LOC(HEXTMOD%FRMOUFP)
      ADDR = 1+ ((ITMP - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("OUTFLX",
     I                       PMXOUT,SBPTR,1,2,0,0,2,0,ADDR,
     I                       "output time series",
     M                       MOD_DEFN%GROUP(1)%MEMBER(2))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(1)%MEMBER(2)%LTVAL)
C      
      ITMP = LOC(HEXTMOD%NSTA)
      SBPTR= -(1+ ((ITMP - BASEADDR)/4))
      ITMP = LOC(HEXTMOD%FRMSTFP)
      ADDR = 1+ ((ITMP - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("STATE",
     I                       PMXSTA,SBPTR,1,1,0,0,2,0,ADDR,
     I                       "state variable time series",
     M                       MOD_DEFN%GROUP(1)%MEMBER(3))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(1)%MEMBER(3)%LTVAL)
C    
      MOD_DEFN%GROUP(2)%NAME  = "TOHSPF"
      MOD_DEFN%GROUP(2)%OFFSET= 20
      ALLOCATE (MOD_DEFN%GROUP(2)%MEMBER(3))
C      
      ITMP = LOC(HEXTMOD%NINP)
      SBPTR= -(1+ ((ITMP - BASEADDR)/4))
      ITMP = LOC(HEXTMOD%TOINFP)
      ADDR = 1+ ((ITMP - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("INPFLX",
     I                       PMXINP,SBPTR,1,2,0,0,0,0,ADDR,
     I                       "input time series",
     M                       MOD_DEFN%GROUP(2)%MEMBER(1))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(2)%MEMBER(1)%LTVAL)
C      
      ITMP = LOC(HEXTMOD%NOUT)
      SBPTR= -(1+ ((ITMP - BASEADDR)/4))
      ITMP = LOC(HEXTMOD%TOOUFP)
      ADDR = 1+ ((ITMP - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("OUTFLX",
     I                       PMXOUT,SBPTR,1,2,0,0,0,0,ADDR,
     I                       "output time series",
     M                       MOD_DEFN%GROUP(2)%MEMBER(2))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(2)%MEMBER(2)%LTVAL)
C      
      ITMP = LOC(HEXTMOD%NSTA)
      SBPTR= -(1+ ((ITMP - BASEADDR)/4))
      ITMP = LOC(HEXTMOD%TOSTFP)
      ADDR = 1+ ((ITMP - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("STATE",
     I                       PMXSTA,SBPTR,1,1,0,0,0,0,ADDR,
     I                       "state variable time series",
     M                       MOD_DEFN%GROUP(2)%MEMBER(3))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(2)%MEMBER(3)%LTVAL)
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
      ADDR    = LOC(HEXTMOD%EXTLAST)
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
C
        HEXTMOD%PIVLNO= 0
        HEXTMOD%BIVLNO= 0
C        
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
      WRITE(HEXTMOD%SSMNAME,'(8A4)')(IVAL(J),J=1,8)
      WRITE(HEXTMOD%INNAME,'(8A4)') (IVAL(J),J=9,12)
C
C     process table - type out-info
C       unit numbers for print and binary output reports, eng and metric
C       output levels for print and binary output reports
      TBNO= 2
      TBSB= 1
      NVAL= 8
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M             IVAL)
C
      DO 140 J= 1,2
        HEXTMOD%PUNIT(J)= IVAL(J)
        HEXTMOD%BUNIT(J)= IVAL(2+J)
        HEXTMOD%OLEV(J) = IVAL(4+J)
        HEXTMOD%PIVL(J) = IVAL(6+J)
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
     I                 (I0,HEXTMOD%BUNIT(J))
C         this sets an entire array     
          HEXTMOD%BFLAG = HEXTMOD%OLEV(2)
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
      HEXTMOD%UNIQUEGROUPCOUNT = 0
C     process table - type influx
C       details about inflows
      TBNO= 5
      NVAL= 8
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      DO TBSB = 1, HEXTMOD%NINP
        CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M               IVAL)
        WRITE(HEXTMOD%INPGROUP(TBSB),'(3A4)') (IVAL(J),J=1,3)
        WRITE(HEXTMOD%INPNAME(TBSB),'(3A4)')  (IVAL(J),J=4,6)
        WRITE(HEXTMOD%INPUNITS(TBSB),'(2A4)') (IVAL(J),J=7,8)
      END DO
      CALL UNIQUEGROUPS(HEXTMOD%NINP,HEXTMOD%INPGROUP,
     M                  HEXTMOD%UNIQUEGROUPCOUNT,HEXTMOD%UNIQUEGROUPS)
C 
C     process table - type outflux
C       details about outflows
      TBNO= 6
      NVAL= 8
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      DO TBSB = 1, HEXTMOD%NOUT
        CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M               IVAL)
        WRITE(HEXTMOD%OUTGROUP(TBSB),'(3A4)') (IVAL(J),J=1,3)
        WRITE(HEXTMOD%OUTNAME(TBSB),'(3A4)')  (IVAL(J),J=4,6)
        WRITE(HEXTMOD%OUTUNITS(TBSB),'(2A4)') (IVAL(J),J=7,8)
      END DO
      CALL UNIQUEGROUPS(HEXTMOD%NSTA,HEXTMOD%OUTGROUP,
     M                  HEXTMOD%UNIQUEGROUPCOUNT,HEXTMOD%UNIQUEGROUPS)
C
C     process table - type state
C       details about state variables
      TBNO= 7
      NVAL= 8
      TABLEDEF = TABLEDEFS%TABLE(TBNO)
      DO TBSB = 1, HEXTMOD%NSTA
        CALL ITABLEX(TBNO,TBSB,NVAL,I1,TABLEDEF,
     M               IVAL)
        WRITE(HEXTMOD%STAGROUP(TBSB),'(3A4)') (IVAL(J),J=1,3)
        WRITE(HEXTMOD%STANAME(TBSB),'(3A4)')  (IVAL(J),J=4,6)
        WRITE(HEXTMOD%STAUNITS(TBSB),'(2A4)') (IVAL(J),J=7,8)
      END DO
      CALL UNIQUEGROUPS(HEXTMOD%NOUT,HEXTMOD%STAGROUP,
     M                  HEXTMOD%UNIQUEGROUPCOUNT,HEXTMOD%UNIQUEGROUPS)
C
      CALL EXTRST(I1,HEXTMOD)
      CALL EXTRST(I2,HEXTMOD)
      CALL EXTRST(3,HEXTMOD)
      CALL EXTRST(4,HEXTMOD)
      CALL EXTRST(5,HEXTMOD)
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
C     Import a series of external model results into HSPF with reports and accumulation
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
      INTEGER     IVL,IVL1,N,IDELT,I1,I12
      CHARACTER*6 OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL    ADDTIM2,SPECL,UPQUAN,ACCVEC
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
      I1    = 1
      I12   = 12
C
      IF (STIVL .EQ. 1) THEN
        IF (HEXTMOD%NSTA .GT. 0) THEN
C         put initial values of state variable time series into osv
          DO N= 1,HEXTMOD%NSTA
            IF (HEXTMOD%FRMSTFP(N) .GT. 0) THEN
              HEXTMOD%STAVAL(N,1)= PAD(HEXTMOD%FRMSTFP(N)+IVL1)
            END IF
          END DO
        END IF
      END IF
C
C     time loop
      DO 130 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1         = IVL1+ 1
        HEXTMOD%SPIVL= HEXTMOD%SPIVL+ 1
C
C       increment date/time
        CALL ADDTIM2(IDELT,HEXTMOD%NDAY,I1,I1,I12,
     M               DATIM,HEXTMOD%PIVLNO,HEXTMOD%BIVLNO,
     O               HEXTMOD%NDAYS,HEXTMOD%NXTMON,HEXTMOD%HRFG,
     O               HEXTMOD%DAYFG,HEXTMOD%EDAYFG,
     O               HEXTMOD%EMONFG,HEXTMOD%EPYRFG)
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
          DO N= 1,HEXTMOD%NINP
            IF (HEXTMOD%FRMINFP(N) .GT. 0) THEN
              HEXTMOD%INPVAL(N,1)= PAD(HEXTMOD%FRMINFP(N)+IVL1)
            ELSE
              HEXTMOD%INPVAL(N,1)= -1.0E30
            END IF
          END DO
        END IF
        IF (HEXTMOD%NOUT .GT. 0) THEN
C         put values of output time series into osv
          DO N= 1,HEXTMOD%NOUT
            IF (HEXTMOD%FRMOUFP(N) .GT. 0) THEN
              HEXTMOD%OUTVAL(N,1)= PAD(HEXTMOD%FRMOUFP(N)+IVL1)
            ELSE
              HEXTMOD%OUTVAL(N,1)= -1.0E30
            END IF
          END DO
        END IF
        IF (HEXTMOD%NSTA .GT. 0) THEN
C         put values of state variable time series into osv
          DO N= 1,HEXTMOD%NSTA
            IF (HEXTMOD%FRMSTFP(N) .GT. 0) THEN
              HEXTMOD%STAVAL(N,1)= PAD(HEXTMOD%FRMSTFP(N)+IVL1)
            ELSE
              HEXTMOD%STAVAL(N,1)= -1.0E30
            END IF
          END DO
        END IF
C
        CALL EXTPRINT(HEXTMOD)
C
C       output to rest of hspf
        IF (HEXTMOD%NINP .GT. 0) THEN
C         put values of input time series onto pad
          DO 70 N= 1,HEXTMOD%NINP
            IF (HEXTMOD%TOINFP(N) .GT. 0) THEN
              PAD(HEXTMOD%TOINFP(N)+IVL1)= HEXTMOD%INPVAL(N,1)
            END IF
 70       CONTINUE
        END IF
        IF (HEXTMOD%NOUT .GT. 0) THEN
C         put values of output time series onto pad
          DO 80 N= 1,HEXTMOD%NOUT
            IF (HEXTMOD%TOOUFP(N) .GT. 0) THEN
              PAD(HEXTMOD%TOOUFP(N)+IVL1)= HEXTMOD%OUTVAL(N,1)
            END IF
 80       CONTINUE
        END IF
        IF (HEXTMOD%NSTA .GT. 0) THEN
C         put values of state variable time series onto pad
          DO 90 N= 1,HEXTMOD%NSTA
            IF (HEXTMOD%TOSTFP(N) .GT. 0) THEN
              PAD(HEXTMOD%TOSTFP(N)+IVL1)= HEXTMOD%STAVAL(N,1)
            END IF
 90       CONTINUE
        END IF
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (HEXTMOD%SPIVL,HEXTMOD%SPOPNO)
C
 130  CONTINUE
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   EXTPRINT
     M                     (HEXTMOD)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes, produce printed output for module.
C
C     + + + DUMMY ARGUMENTS
      TYPE(HTEXTMOD)  HEXTMOD     
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    EXDAT(5),I1,I2,I3,I4,I5,J,PRINTU,UNITFG,BINU
      POINTER   (PDATIM,DATIM) 
      INTEGER    DATIM(5) 
      LOGICAL    PRINTNOW
C
C     + + + EXTERNALS + + +
      EXTERNAL   EXDATE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('1',//,' EXTERNAL SEGMENT NO. ',I3,6X,A32,6X,A32,6X,
     $        'REPORT FOR ',I4,' INTERVALS ENDING ',I4,'/',I2,'/',
     $        I2,I3,':',I2)
 2010 FORMAT ('1',//,' EXTERNAL SEGMENT NO. ',I3,6X,A32,6X,A32,6X,
     $        'REPORT FOR DAY ',I4,'/',I2,'/',I2)
 2020 FORMAT ('1',//,' EXTERNAL SEGMENT NO. ',I3,6X,A32,6X,A32,6X,
     $        'REPORT FOR MONTH ',I4,'/',I2)
 2030 FORMAT ('1',//,' EXTERNAL SEGMENT NO. ',I3,6X,A32,6X,A32,6X,
     $        'REPORT FOR PRINTOUT YEAR ENDING ',I4,'/',I2)
C
C     + + + END SPECIFICATIONS + + +
C
      PDATIM= LOC(HEXTMOD%YR)    
      I1= 1
      I2= 2
      I3= 3
      I4= 4
      I5= 5
C     flux accumulation
      IF (HEXTMOD%OLEV(1) .EQ. 2 .OR. HEXTMOD%OLEV(2) .EQ. 2) THEN
C       some printout at the pivl level is being produced, so
C       accumulate fluxes to this level
        CALL EXTACC
     I             (I1,I2,
     M              HEXTMOD)
      END IF
C
C     always accumulate to the daily level
      CALL EXTACC
     I           (I1,I3,
     M            HEXTMOD)
C
      IF (HEXTMOD%EDAYFG .EQ. 1) THEN
C       it's the last interval of the day - accumulate daily
C       fluxes to the month level
        CALL EXTACC
     I             (I3,I4,
     M              HEXTMOD)
C
        IF (HEXTMOD%EMONFG .EQ. 1) THEN
C         it's the last interval of the month - accumulate
C         monthly fluxes to the year level
          CALL EXTACC
     I               (I4,I5,
     M                HEXTMOD)
        END IF
      END IF
C
C     printout and continuity check
      DO 10 UNITFG= 1,2
        PRINTU= HEXTMOD%PUNIT(UNITFG)
        BINU  = HEXTMOD%BUNIT(UNITFG)
        IF (PRINTU .NE. 0 .OR. BINU .NE. 0) THEN
C         printout is required in this set of external units.
C         unitfg= 1 for english, 2 for metric.  printu is the
C         fortran logical unit no. to be used for printout.
C
          CALL EXDATE
     I               (DATIM,
     O                EXDAT)
C     
          IF ((HEXTMOD%PIVLNO .EQ. HEXTMOD%PIVL(1) .AND. 
     #         HEXTMOD%OLEV(1) .EQ. 2) .OR.
     #        (HEXTMOD%BIVLNO .EQ. HEXTMOD%PIVL(2) .AND. 
     #         HEXTMOD%OLEV(2) .EQ. 2)) THEN
C           it's time to handle any pivl level printout, and some is required
C           transform hour and minute fields in date/time to external format
            IF (PRINTU .GT. 0 .AND. HEXTMOD%OLEV(1) .EQ. 2) THEN
              WRITE (PRINTU,2000) HEXTMOD%EXTMODNO, HEXTMOD%SSMNAME,
     $                            HEXTMOD%INNAME, HEXTMOD%PIVL(1), EXDAT
              PRINTNOW = .TRUE.
            ELSE
              PRINTNOW = .FALSE.
            END IF
            CALL EXTPRT
     I                 (UNITFG,I2,PRINTU,BINU,EXDAT,PRINTNOW,
     M                  HEXTMOD)
          END IF
C
          IF (HEXTMOD%EDAYFG .EQ. 1) THEN
            IF (HEXTMOD%OLEV(1) .LE. 3 .OR. HEXTMOD%OLEV(2) .LE. 3) THEN
C             it's time to handle daily printout
              IF (PRINTU .GT. 0 .AND. HEXTMOD%OLEV(1) .LE. 3) THEN
                WRITE (PRINTU,2010) HEXTMOD%EXTMODNO, HEXTMOD%SSMNAME,
     $                              HEXTMOD%INNAME,(DATIM(J),J=1,3)
                PRINTNOW = .TRUE.
              ELSE
                PRINTNOW = .FALSE.
              END IF
              CALL EXTPRT
     I                   (UNITFG,I3,PRINTU,BINU,EXDAT,PRINTNOW,
     M                    HEXTMOD)
            END IF
C
            IF (HEXTMOD%EMONFG .EQ. 1) THEN
              IF (HEXTMOD%OLEV(1).LE.4 .OR. HEXTMOD%OLEV(2).LE.4) THEN
C               it's time to handle monthly printout
                IF (PRINTU .GT. 0 .AND. HEXTMOD%OLEV(1) .LE. 4) THEN
                  WRITE (PRINTU,2020) HEXTMOD%EXTMODNO, HEXTMOD%SSMNAME,
     $                                HEXTMOD%INNAME,(DATIM(J),J=1,2)
                  PRINTNOW = .TRUE.
                ELSE
                  PRINTNOW = .FALSE.
                END IF
                CALL EXTPRT
     I                     (UNITFG,I4,PRINTU,BINU,EXDAT,PRINTNOW,
     M                      HEXTMOD)
              END IF
C
              IF (HEXTMOD%EPYRFG .EQ. 1) THEN
C               it's time to handle yearly printout
                IF (PRINTU .GT. 0 .AND.  HEXTMOD%OLEV(1) .LE. 5) THEN
                  WRITE (PRINTU,2030) HEXTMOD%EXTMODNO, HEXTMOD%SSMNAME,
     $                                HEXTMOD%INNAME,(DATIM(J),J=1,2)
                  PRINTNOW = .TRUE.
                ELSE
                  PRINTNOW = .FALSE.
                END IF
                CALL EXTPRT
     I                     (UNITFG,I5,PRINTU,BINU,EXDAT,PRINTNOW,
     M                      HEXTMOD)
              END IF
            END IF
          END IF
        ELSE
C         printout is not required for this set of external units
        END IF
 10   CONTINUE
C
C     reset all flux accumulators and those state variables
C     used in material balance checks
      IF ((HEXTMOD%PIVLNO .EQ. HEXTMOD%PIVL(1) .AND. 
     #     HEXTMOD%OLEV(1) .EQ. 2) .OR.
     #    (HEXTMOD%BIVLNO .EQ. HEXTMOD%PIVL(2) .AND. 
     #     HEXTMOD%OLEV(2) .EQ. 2)) THEN
C       reset any pivl level variables in use
        CALL EXTRST
     I             (I2,
     M              HEXTMOD)
      END IF
C
      IF (HEXTMOD%EDAYFG .EQ. 1) THEN
C       reset any daily variables in use
        CALL EXTRST
     I             (I3,
     M              HEXTMOD)
C
        IF (HEXTMOD%EMONFG .EQ. 1) THEN
C         reset any monthly variables in use
          CALL EXTRST
     I               (I4,
     M                HEXTMOD)
C
          IF (HEXTMOD%EPYRFG .EQ. 1) THEN
C           reset any yearly variables in use
            CALL EXTRST
     I                 (I5,
     M                  HEXTMOD)
          END IF
        END IF
      END IF
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   EXTACC
     I                   (FRMROW,TOROW,
     M                    HEXTMOD)
C
C     + + + PURPOSE + + +
C     Accumulate fluxes, produce printed output for module.
C
C     + + + PURPOSE + + +
C     Accumulate fluxes for output
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        FRMROW,TOROW
      TYPE(HTEXTMOD) HEXTMOD     
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FRMROW - row containing incremental flux accumulation
C     TOROW  - flux row to be incremented
C
C     + + + EXTERNALS + + +
      EXTERNAL  ACCVEC
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ACCVEC
     I           (PMXINP,HEXTMOD%INPVAL(1,FRMROW),
     M            HEXTMOD%INPVAL(1,TOROW))
      CALL ACCVEC
     I           (PMXOUT,HEXTMOD%OUTVAL(1,FRMROW),
     M            HEXTMOD%OUTVAL(1,TOROW))
      CALL ACCVEC
     I           (PMXSTA,HEXTMOD%STAVAL(1,FRMROW),
     M            HEXTMOD%STAVAL(1,TOROW))
C
      RETURN
      END SUBROUTINE           
C    
C
C    
      SUBROUTINE   EXTPRT
     I                   (UNITFG,LEV,PRINTU,BINU,EXDAT,PRINTNOW,
     M                    HEXTMOD)
C
C     + + + PURPOSE + + +
C     Perform printout for module EXTMOD.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        UNITFG,LEV,PRINTU,BINU,EXDAT(5)
      LOGICAL        PRINTNOW
      TYPE(HTEXTMOD) HEXTMOD     
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UNITFG  - output units   1-english, 2-metric
C     LEV     - current output level (2-pivl,3-day,4-mon,5-ann)
C     PRINTU  - fortran unit number on which to print output
C     BINU    - fortran unit number on which to write binary output
C     PRINTNOW- printed output needed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      GROUPCOUNT, I, J, K, ACNT, I0, I1,
     $             BHEADLEN(PMXINP+PMXOUT+PMXSTA)
      REAL         BVALUES(PMXINP+PMXOUT+PMXSTA)
      LOGICAL      GROUPHEADERPRINTED 
      CHARACTER*16 BHEAD(PMXINP+PMXOUT+PMXSTA)
      CHARACTER*8  SHEAD
C
C     + + + EXTERNALS + + +
      EXTERNAL     BINHED,BINREC
C
C     + + + END SPECIFICATIONS + + +
C
      I0= 0
      I1= 1
C
      DO I = 1, HEXTMOD%UNIQUEGROUPCOUNT
        IF (PRINTU .GT. 0 .AND. PRINTNOW) THEN
          GROUPHEADERPRINTED = .FALSE.
          CALL PRINTSECTION(PRINTU,LEV,'STATE VARIABLES',
     I                      HEXTMOD%UNIQUEGROUPS(I), 
     I                      HEXTMOD%NSTA,HEXTMOD%STAGROUP,
     I                      HEXTMOD%STANAME,HEXTMOD%STAUNITS,
     I                      HEXTMOD%STAVAL(1,LEV),
     M                      GROUPHEADERPRINTED)
          CALL PRINTSECTION(PRINTU,LEV,'INPUTS',
     I                      HEXTMOD%UNIQUEGROUPS(I),
     I                      HEXTMOD%NINP,HEXTMOD%INPGROUP,
     I                      HEXTMOD%INPNAME,HEXTMOD%INPUNITS,
     I                      HEXTMOD%INPVAL(1,LEV),
     M                      GROUPHEADERPRINTED)
          CALL PRINTSECTION(PRINTU,LEV,'OUTPUTS',
     I                      HEXTMOD%UNIQUEGROUPS(I),
     I                      HEXTMOD%NOUT,HEXTMOD%OUTGROUP,
     I                      HEXTMOD%OUTNAME,HEXTMOD%OUTUNITS,
     I                      HEXTMOD%OUTVAL(1,LEV),
     M                      GROUPHEADERPRINTED)
        END IF
        IF (BINU .GT. 0) THEN
          ACNT = 0
          CALL BUILDBIN(HEXTMOD%BFLAG(I),
     I                  HEXTMOD%UNIQUEGROUPS(I),'S',
     I                  HEXTMOD%NSTA,HEXTMOD%STANAME, 
     I                  HEXTMOD%STAGROUP,HEXTMOD%STAVAL(1,LEV),
     M                  ACNT,BHEADLEN,BHEAD,BVALUES)
          CALL BUILDBIN(HEXTMOD%BFLAG(I),
     I                  HEXTMOD%UNIQUEGROUPS(I),'I',
     I                  HEXTMOD%NINP,HEXTMOD%INPNAME, 
     I                  HEXTMOD%INPGROUP,HEXTMOD%INPVAL(1,LEV),
     M                  ACNT,BHEADLEN,BHEAD,BVALUES)
          CALL BUILDBIN(HEXTMOD%BFLAG(I),
     I                  HEXTMOD%UNIQUEGROUPS(I),'O',
     I                  HEXTMOD%NOUT,HEXTMOD%OUTNAME, 
     I                  HEXTMOD%OUTGROUP,HEXTMOD%OUTVAL(1,LEV),
     M                  ACNT,BHEADLEN,BHEAD,BVALUES)
C
          SHEAD = HEXTMOD%UNIQUEGROUPS(I)(1:8)
          IF(HEXTMOD%BFLAG(I) .GT. 0) THEN
            CALL BINHED (BINU,'EXTMOD  ',HEXTMOD%EXTMODNO,
     1                   SHEAD,
     1                   ACNT,BHEADLEN,BHEAD)
            HEXTMOD%BFLAG(I) = -HEXTMOD%BFLAG(I)
          END IF
          CALL BINREC (BINU,'EXTMOD  ',HEXTMOD%EXTMODNO,
     1                 SHEAD,UNITFG,
     1                 LEV,EXDAT,ACNT,BVALUES)

        END IF
      END DO    
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   BUILDBIN
     I                     (BFLAG,GROUP,FTYPE,COUNT,NAMES,GROUPS,VALUES,
     M                      ACNT,BHEADLEN,BHEAD,BVALUES)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       BFLAG,COUNT,ACNT,BHEADLEN(*)
      REAL          VALUES(COUNT),BVALUES(*)
      CHARACTER*16  BHEAD(*)
      CHARACTER*12  GROUP,GROUPS(COUNT),NAMES(COUNT)
      CHARACTER*1   FTYPE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       J
      CHARACTER*16  HEADER      
C
      DO J = 1, COUNT
        IF (GROUPS(J) .EQ. GROUP) THEN
C         need this     
          ACNT = ACNT + 1     
          IF (BFLAG .GT. 0) THEN
C           need headers
            HEADER = FTYPE // ':' // NAMES(J)
            BHEADLEN(ACNT) = LEN_TRIM(HEADER)
            BHEAD(ACNT) = TRIM(HEADER)
          END IF
          BVALUES(ACNT) = VALUES(J)
        END IF
      END DO
C
      RETURN
      END SUBROUTINE     
C
C
C
      SUBROUTINE PRINTSECTION
     I                       (PRINTU,LEV,TITLE,GROUP,
     I                        COUNT,GROUPS,NAMES,UNITS,VALUES,
     M                        GROUPHEADERPRINTED)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       PRINTU,LEV,COUNT
      REAL          VALUES(COUNT)     
      LOGICAL       GROUPHEADERPRINTED
      CHARACTER*12  GROUP,GROUPS(COUNT),NAMES(COUNT)
      CHARACTER*8   UNITS(COUNT)
      CHARACTER*(*) TITLE
C      
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,OUTPOS,LEN
      CHARACTER*132 HEADSTR,UNITSSTR,VALUESTR
C      
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' *** GROUP ',A,' ***')
 2010 FORMAT (/,'    ',A,//,A132,/,A132)
 2020 FORMAT (A132)
C
      OUTPOS  = 0
      HEADSTR = ""
      UNITSSTR= ""
      VALUESTR= ""
      DO J = 1, COUNT
        IF (GROUPS(J) .EQ. GROUP) THEN
          IF (.NOT.GROUPHEADERPRINTED) THEN
            WRITE(PRINTU,2000) TRIM(GROUP)
            GROUPHEADERPRINTED = .TRUE.
          END IF      
          OUTPOS= OUTPOS+ 4   
          LEN   = LEN_TRIM(NAMES(J))
          HEADSTR(OUTPOS+13-LEN:OUTPOS+12) = TRIM(NAMES(J))
          LEN   = LEN_TRIM(UNITS(J))
          UNITSSTR(OUTPOS+13-LEN:OUTPOS+12) = TRIM(UNITS(J))
          IF (VALUES(J) .GT. 0.1 .AND. VALUES(J) .LT. 100000.0) THEN
            WRITE(VALUESTR(OUTPOS+1:OUTPOS+12),'(F12.3)') VALUES(J)
          ELSE
            WRITE(VALUESTR(OUTPOS+1:OUTPOS+12),'(1PE12.3)') VALUES(J)
          END IF
          OUTPOS = OUTPOS+ 12
        END IF
      END DO
      IF (OUTPOS .GT. 0) THEN
        WRITE(PRINTU,2010) TRIM(TITLE),HEADSTR,UNITSSTR
        WRITE(PRINTU,2020) VALUESTR
      END IF
C
      RETURN
      END SUBROUTINE      
C
C
C
      SUBROUTINE UNIQUEGROUPS
     I                       (SECTIONCOUNT,SECTIONGROUPS,
     M                        GROUPCOUNT,GROUPS)
C      
C     + + + PURPOSE + + +
C     Build an array of unique groups
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      SECTIONCOUNT,GROUPCOUNT
      CHARACTER*12 SECTIONGROUPS(*),GROUPS(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SECTIONCOUNT  - size of section
C     SECTIONGROUPS - groups in section
C     GROUPCOUNT    - unique group count
C     GROUPS        - unique group names
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I, J
      LOGICAL      GROUPFOUND
C
C     + + + END SPECIFICATIONS + + +
C
      DO I = 1, SECTIONCOUNT
        GROUPFOUND = .FALSE.
        DO J = 1, GROUPCOUNT
          IF (SECTIONGROUPS(I) .EQ. GROUPS(J)) THEN
            GROUPFOUND = .TRUE.
            EXIT 
          END IF
        END DO
        IF (.NOT. GROUPFOUND) THEN
          GROUPCOUNT = GROUPCOUNT+ 1
          GROUPS(GROUPCOUNT) = SECTIONGROUPS(I)
        END IF        
      END DO
C
      RETURN
      END SUBROUTINE       
C
C
C
      SUBROUTINE   EXTRST   
     I                   (LEV,
     M                    HEXTMOD)
C
C     + + + PURPOSE + + +
C     Reset all flux accumulators and those state variables used
C     in material balance checks.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER        LEV
      TYPE(HTEXTMOD) HEXTMOD     
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEV    - current output level (2-pivl,3-day,4-mon,5-ann)
C
C     + + + EXTERNALS + + +
      EXTERNAL  SETVEC
C
C     + + + END SPECIFICATIONS + + +
C
      CALL SETVEC
     I           (PMXINP,0.0, 
     M            HEXTMOD%INPVAL(1,LEV))
      CALL SETVEC
     I           (PMXOUT,0.0, 
     M            HEXTMOD%OUTVAL(1,LEV))
      CALL SETVEC
     I           (PMXSTA,0.0, 
     M            HEXTMOD%STAVAL(1,LEV))
C     
      RETURN
      END SUBROUTINE
C            
      END MODULE HMEXTMOD  
               