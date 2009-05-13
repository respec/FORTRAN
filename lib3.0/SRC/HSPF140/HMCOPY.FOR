C
C
C
      MODULE HMCOPY
C
C     + + + PURPOSE + + +
C     module containing utility module COPY
C
C     + + + HISTORY + + +
C     2009/04/27 JLK - initial implementation of module version of COPY
C
C     + + + MODULE VARIABLES + + +
      USE HMTIMSERDEF
      USE HMTABLEDEF
      IMPLICIT NONE
C
      TYPE HTCOPY
        SEQUENCE
        INTEGER   COPYNO,NPT,NMN,MESSU
        REAL      DELT
        INTEGER   NDAY(12),YR,MON,DAY,HR,MIN,
     $            NDAYS,SPAFP,SPAKND,SPIVL,SPOPNO,SPOUT,UUNITS,
     $            IPTFP(20),IMNFP(20),OPTFP(20),OMNFP(20)
        REAL      PTVAL(20),MNVAL(20)
        INTEGER   SPNUND
      END TYPE HTCOPY
C
      CONTAINS 
C
C
C
      SUBROUTINE GETTABLEDEF_COPY
     O                           (MOD_TABLE)
C             
      TYPE (HMODULE_TABLE_DEFINITION) MOD_TABLE
C
      ALLOCATE (MOD_TABLE%TABLE(2))
      MOD_TABLE%TABLE(1)%NAME = "TIMESERIES"
      MOD_TABLE%TABLE(1)%DIM = 1
      MOD_TABLE%TABLE(2)%NAME = "END COPY"
      MOD_TABLE%TABLE(2)%DIM = 0
C
      END SUBROUTINE            
C
C
C
      SUBROUTINE GETTSDEF_COPY
     O                        (MOD_DEFN)
C
      TYPE (HMODULE_TIMSER_DEFINITION) MOD_DEFN   
C      
      INTEGER      BASEADDR, SBPTR, ADDR
      TYPE(HTCOPY) HCOPY
C      
      MOD_DEFN%NAME   = "COPY"
      MOD_DEFN%CLUSTER= 144
      ALLOCATE (MOD_DEFN%GROUP(2))
C
      BASEADDR = LOC(HCOPY%COPYNO)
C
      MOD_DEFN%GROUP(1)%NAME  = "INPUT"
      MOD_DEFN%GROUP(1)%OFFSET= 10
      ALLOCATE (MOD_DEFN%GROUP(1)%MEMBER(2))
C      
      SBPTR= -(1+ ((LOC(HCOPY%NPT) - BASEADDR)/4))
      ADDR = 1+ ((LOC(HCOPY%IPTFP) - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("POINT",
     I                       20,SBPTR,1,1,0,0,2,0,ADDR,
     I                       "Point-valued input time series",
     M                       MOD_DEFN%GROUP(1)%MEMBER(1))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(1)%MEMBER(1)%LTVAL)
C      
      SBPTR= -(1+ ((LOC(HCOPY%NMN) - BASEADDR)/4))
      ADDR = 1+ ((LOC(HCOPY%IMNFP) - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("MEAN",
     I                       20,SBPTR,1,2,0,0,2,0,ADDR,
     I                       "Mean-valued input time series",
     M                       MOD_DEFN%GROUP(1)%MEMBER(2))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(1)%MEMBER(2)%LTVAL)
C
      MOD_DEFN%GROUP(2)%NAME = "OUTPUT"
      MOD_DEFN%GROUP(2)%OFFSET= 20
      ALLOCATE (MOD_DEFN%GROUP(2)%MEMBER(2))
C      
      SBPTR= -(1+ ((LOC(HCOPY%NPT) - BASEADDR)/4))
      ADDR = 1+ ((LOC(HCOPY%OMNFP) - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("POINT",
     I                       20,SBPTR,1,1,0,0,0,0,ADDR,
     I                       "Point-valued output time series",
     M                       MOD_DEFN%GROUP(2)%MEMBER(1))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(2)%MEMBER(1)%LTVAL)
C      
      SBPTR= -(1+ ((LOC(HCOPY%NMN) - BASEADDR)/4))
      ADDR = 1+ ((LOC(HCOPY%OPTFP) - BASEADDR)/4)
      CALL TIMSER_MEMBER_SET("MEAN",
     I                       20,SBPTR,1,2,0,0,0,0,ADDR,
     I                       "Mean-valued output time series",
     M                       MOD_DEFN%GROUP(2)%MEMBER(2))
      CALL TIMSER_MEMBER_LTVAL_INIT(MOD_DEFN%GROUP(2)%MEMBER(2)%LTVAL)
C      
      MOD_DEFN%INITIALIZED = .TRUE.
C      
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   PCOPY
     I                   (NDELT,SDATIM,NDAMON,EMFG, 
     M                    OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the copy module
C
C     + + + HISTORY + + +
C     2009/04/22 JLK - use OSV module version
C     2009/04/27 JLK - initial implementation of module version of COPY
C
C     + + + MODULES + + +
      USE       HIOOSV 
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG, OSVKEY
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
      TYPE(HTCOPY)  HCOPY     
      POINTER   (P,HCOPY)
      POINTER   (PNUMBR,NUMBR),(PDATIM,DATIM),(POSV,OSV)
      INTEGER    NUMBR(2),DATIM(5),OSV(500)
C
C     + + + LOCAL VARIABLES + + +   
      INTEGER    I,I1,I2,OSVKND,OSVKST,OSVREC,BASEADDR,ADDR
C
C     + + + FUNCTIONS + + +
      INTEGER    DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL   ITABLE,DAYMNH
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),/,' ','PROCESSING COPY OPERATION NO. ',I4)
 2010 FORMAT (/,' FINISHED PROCESSING COPY OPERATION NO. ',I4,
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
      PNUMBR = LOC(HCOPY%NPT)
      PDATIM = LOC(HCOPY%YR)    
      POSV   = LOC(HCOPY)
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
        HCOPY%MESSU = FILE(1)
C
        IF (OUTLEV.GT.0) THEN
          WRITE (HCOPY%MESSU,2000) OPTNO
        END IF
C
C       place information into osv
        HCOPY%COPYNO= OPTNO
        HCOPY%DELT  =  NDELT
        DO 20 I= 1, 12
          HCOPY%NDAY(I)= NDAMON(I)
 20     CONTINUE
        DO 30 I= 1, 5
          DATIM(I)= SDATIM(I)
 30     CONTINUE
C       following value is assigned so that subroutine addtim will
C       work ok
        HCOPY%NDAYS = DAYMNH (HCOPY%YR,HCOPY%MON,HCOPY%NDAY)
        HCOPY%SPIVL = 0
        HCOPY%SPOPNO= OPNO
        HCOPY%UUNITS= EMFG
C
C       initialize current values
        DO 40 I= 1, 20
          HCOPY%PTVAL(I)= -1.0E30
          HCOPY%MNVAL(I)= -1.0E30
 40     CONTINUE
      END IF
C
C     get no. of time series to be copied -- table-type timeseries
      CALL ITABLE (I1,I1,I2,I1,
     M             NUMBR)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (HCOPY%MESSU,2010)  OPTNO
      END IF
C
      RETURN
      END SUBROUTINE
C
C
C
      SUBROUTINE   COPY
     I                  (STIVL,WIDTH,OSVKST)
C
C     + + + PURPOSE + + +
C     Copy a set of point- and/or mean-valued time series
C
C     + + + HISTORY + + +
C     2009/04/22 JLK - use OSV module version
C     2009/04/27 JLK - initial implementation of module version of COPY
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
      TYPE(HTCOPY)  HCOPY     
      POINTER    (P,HCOPY)
      POINTER    (PNUMBR,NUMBR),(PDATIM,DATIM) 
      INTEGER     NUMBR(2),DATIM(5)   
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
      DATA        OPTYP/'COPY  '/
C
C     + + + END SPECIFICATIONS + + +
C
C     point to where the osv for this operation is in memory
      P      = LOC(OSVM(1,OSVKST))
      PDATIM = LOC(HCOPY%YR)    
C      
      IVL   = STIVL- 1
      IVL1  = STIVL
      IDELT = HCOPY%DELT
      PIVLNO= 0
      I1    = 1
      I12   = 12
C
      IF ( (STIVL .EQ. 1) .AND. (HCOPY%NPT.GT.0) ) THEN
C       put initial values of point-valued time series into inpad
C       transfer from input row to temporary array
        DO 20 N= 1,HCOPY%NPT
          IF (HCOPY%IPTFP(N) .GT. 0) THEN
            IF (HCOPY%OPTFP(N) .NE. HCOPY%IPTFP(N)) THEN
              HCOPY%PTVAL(N)= PAD(HCOPY%IPTFP(N)+IVL1)
            END IF
          END IF
 20     CONTINUE
C       transfer from temporary array to output row
        DO 30 N= 1,HCOPY%NPT
          IF ( (HCOPY%OPTFP(N) .GT. 0) .AND. 
     1         (HCOPY%IPTFP(N) .GT. 0) ) THEN
            IF (HCOPY%OPTFP(N) .NE. HCOPY%IPTFP(N)) THEN
              PAD(HCOPY%OPTFP(N)+IVL1)= HCOPY%PTVAL(N)
            END IF
          END IF
 30     CONTINUE
      END IF
C
C     time loop
      DO 130 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1       = IVL1+ 1
        HCOPY%SPIVL= HCOPY%SPIVL+ 1
C
C       increment date/time
        CALL ADDTIM (IDELT,HCOPY%NDAY,I1,I12,
     M               DATIM,PIVLNO,
     O               HCOPY%NDAYS,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6)
C
        IF (HCOPY%SPAFP .GT. 0 .AND. HCOPY%SPAFP .LE. HCOPY%SPAKND) THEN
C         special actions are being taken and there is at least one left
          CALL SPECL (OPTYP,HCOPY%COPYNO,
     I                HCOPY%SPAKND,HCOPY%SPOPNO,
     I                DATIM,HCOPY%MESSU,HCOPY%SPIVL,
     I                HCOPY%SPOUT,HCOPY%SPNUND,
     M                HCOPY%SPAFP)
        END IF
C
        IF (HCOPY%NPT.GT.0) THEN
C         copy point-valued time series to osv
          DO 40 N= 1,HCOPY%NPT
            IF (HCOPY%IPTFP(N) .GT. 0) THEN
              HCOPY%PTVAL(N)= PAD(HCOPY%IPTFP(N)+IVL1)
            END IF
 40       CONTINUE
        END IF
C
        IF (HCOPY%NMN.GT.0) THEN
C         copy mean-valued time series to osv
          DO 50 N= 1,HCOPY%NMN
            IF (HCOPY%IMNFP(N) .GT. 0) THEN
              HCOPY%MNVAL(N)= PAD(HCOPY%IMNFP(N)+IVL1)
            END IF
 50       CONTINUE
        END IF
C
        IF (HCOPY%NPT.GT.0) THEN
C         copy point-valued time series from osv to output
          DO 60 N= 1,HCOPY%NPT
            IF ( (HCOPY%OPTFP(N) .GT. 0) .AND. 
     1           (HCOPY%IPTFP(N) .GT. 0) ) THEN
              IF (HCOPY%OPTFP(N) .NE. HCOPY%IPTFP(N)) THEN
                PAD(HCOPY%OPTFP(N)+IVL1)= HCOPY%PTVAL(N)
              END IF
            END IF
 60       CONTINUE
        END IF
C
        IF (HCOPY%NMN.GT.0) THEN
C         copy mean-valued time series from osv to output
          DO 70 N= 1,HCOPY%NMN
            IF ( (HCOPY%OMNFP(N) .GT. 0) .AND. 
     1           (HCOPY%IMNFP(N) .GT. 0) ) THEN
              IF (HCOPY%OMNFP(N) .NE. HCOPY%IMNFP(N)) THEN
                PAD(HCOPY%OMNFP(N)+IVL1)= HCOPY%MNVAL(N)
              END IF
            END IF
 70       CONTINUE
        END IF
C
C       update pipes for user-defined variable quantities
        CALL UPQUAN (HCOPY%SPIVL,HCOPY%SPOPNO)
C
 130  CONTINUE
C
      RETURN
      END SUBROUTINE
C    
      END MODULE HMCOPY  
               