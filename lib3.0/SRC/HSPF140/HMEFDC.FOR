C
C
C
      MODULE HMEFDC
C
C     + + + PURPOSE + + +
C     module containing module EFDC
C
C     + + + HISTORY + + +
C     2009/06/22 JLK - initial implementation of module version of EFDC
C
C     + + + MODULE VARIABLES + + +
      USE HMTIMSERDEF
      USE HMTABLEDEF
      IMPLICIT NONE
C
      TYPE HTEFDC
        SEQUENCE
        INTEGER   EFDCNO,MESSU
        REAL      DELT
        INTEGER   NDAY(12),YR,MON,DAY,HR,MIN,
     $            NDAYS,SPAFP,SPAKND,SPIVL,SPOPNO,SPOUT,UUNITS
        INTEGER   SPNUND
      END TYPE HTEFDC
C
      CONTAINS 
C
C
C
      SUBROUTINE GETTABLEDEF_EFDC
     O                           (MOD_TABLE)
C             
      TYPE (HMODULE_TABLE_DEFINITION) MOD_TABLE
C

C
      END SUBROUTINE            
C
C
C
      SUBROUTINE GETTSDEF_EFDC
     O                        (MOD_DEFN)
C
      TYPE (HMODULE_TIMSER_DEFINITION) MOD_DEFN   
C      

C      
      MOD_DEFN%INITIALIZED = .TRUE.
C      
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   PEFDC
     I                  (NDELT,SDATIM,NDAMON,EMFG,TABLEDEFS, 
     M                   OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the EFDC module
C
C     + + + HISTORY + + +
C     2009/06/22 JLK - use version
C
C     + + + MODULES + + +
      USE       HIOOSV 
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
      TYPE(HTEFDC)  HEFDC     
      POINTER   (P,HEFDC)
      POINTER   (PNUMBR,NUMBR),(PDATIM,DATIM),(POSV,OSV)
      INTEGER    NUMBR(2),DATIM(5),OSV(500)
C
C     + + + LOCAL VARIABLES + + +   
      INTEGER    I,I1,I2,OSVKND,OSVKST,OSVREC,BASEADDR,ADDR
      TYPE(HMODULE_TABLE) :: TABLEDEF
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLEX,DAYMNH
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),/,' ','PROCESSING EFDC OPERATION NO. ',I4)
 2010 FORMAT (/,' FINISHED PROCESSING EFDC OPERATION NO. ',I4,
     $        /,' ',132('+'))
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      I2   = 2
C
      OSVREC= 1
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
C
C     point the osv to its memory location
      P = LOC(OSVM(1,OSVKST))
C
      PDATIM = LOC(HEFDC%YR)    
      POSV   = LOC(HEFDC)
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
        HEFDC%MESSU = FILE(1)
C
        IF (OUTLEV.GT.0) THEN
          WRITE (HEFDC%MESSU,2000) OPTNO
        END IF
C
C       place information into osv
        HEFDC%EFDCNO= OPTNO
        HEFDC%DELT  =  NDELT
        DO 20 I= 1, 12
          HEFDC%NDAY(I)= NDAMON(I)
 20     CONTINUE
        DO 30 I= 1, 5
          DATIM(I)= SDATIM(I)
 30     CONTINUE
C       following value is assigned so that subroutine addtim will
C       work ok
        HEFDC%NDAYS = DAYMNH (HEFDC%YR,HEFDC%MON,HEFDC%NDAY)
        HEFDC%SPIVL = 0
        HEFDC%SPOPNO= OPNO
        HEFDC%UUNITS= EMFG
C
      END IF
C

C
      IF (OUTLEV.GT.0) THEN
        WRITE (HEFDC%MESSU,2010)  OPTNO
      END IF
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   EFDC
     I                  (STIVL,WIDTH,OSVKST)
C
C     + + + PURPOSE + + +
C     Wrapper for EFDC model
C
C     + + + HISTORY + + +
C     2009/05/22 JLK - initial implementation
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
      TYPE(HTEFDC)  HEFDC     
      POINTER   (P,HEFDC)
      POINTER   (PNUMBR,NUMBR),(PDATIM,DATIM)
      INTEGER    NUMBR(2),DATIM(5)
C       
C     + + + LOCAL VARIABLES + + +
      INTEGER     IVL,IVL1,N,IDELT,PIVLNO,DUM1,DUM2,DUM3,DUM4,
     #            DUM5,DUM6,I1,I12
      CHARACTER*6 OPTYP
C
C     + + + EXTERNALS + + +
      EXTERNAL    ADDTIM,SPECL,UPQUAN
C
C     + + + DATA INITIALIZATIONS + + +
      DATA        OPTYP/'EFDC  '/
C
C     + + + END SPECIFICATIONS + + +
C
C     point to where the osv for this operation is in memory
      P      = LOC(OSVM(1,OSVKST))
      PDATIM = LOC(HEFDC%YR)    
C      
      IVL   = STIVL- 1
      IVL1  = STIVL
      IDELT = HEFDC%DELT
      PIVLNO= 0
      I1    = 1
      I12   = 12
C
C     time loop
      DO 130 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1       = IVL1+ 1
        HEFDC%SPIVL= HEFDC%SPIVL+ 1
C
C       increment date/time
        CALL ADDTIM (IDELT,HEFDC%NDAY,I1,I12,
     M               DATIM,PIVLNO,
     O               HEFDC%NDAYS,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6)
C
        IF (HEFDC%SPAFP .GT. 0 .AND. HEFDC%SPAFP .LE. HEFDC%SPAKND) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,HEFDC%EFDCNO,
     I                HEFDC%SPAKND,HEFDC%SPOPNO,
     I                DATIM,HEFDC%MESSU,HEFDC%SPIVL,
     I                HEFDC%SPOUT,HEFDC%SPNUND,
     M                HEFDC%SPAFP)
        END IF
C

C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (HEFDC%SPIVL,HEFDC%SPOPNO)
C
 130  CONTINUE
C
      RETURN
      END SUBROUTINE
C    
      END MODULE HMEFDC
               