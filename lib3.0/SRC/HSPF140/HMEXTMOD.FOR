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
C
C     + + + MODULE VARIABLES + + +
      USE HMTABLEDEF
      IMPLICIT NONE
C
      TYPE HTEXTMOD
        SEQUENCE
        INTEGER   EXTMODNO,NPT,NMN,MESSU
        REAL      DELT
        INTEGER   NDAY(12),YR,MON,DAY,HR,MIN,
     $            NDAYS,SPAFP,SPAKND,SPIVL,SPOPNO,SPOUT,UUNITS,
     $            IPTFP(20),IMNFP(20),OPTFP(20),OMNFP(20)
        REAL      PTVAL(20),MNVAL(20)
        INTEGER   SPNUND
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
      ALLOCATE (MOD_TABLE%TABLE(2))
      MOD_TABLE%TABLE(1)%NAME = "TIMESERIES"
      MOD_TABLE%TABLE(1)%DIM = 1
      MOD_TABLE%TABLE(2)%NAME = "END EXTMOD"
      MOD_TABLE%TABLE(2)%DIM = 0
C
      END SUBROUTINE            
C
C
C
      SUBROUTINE   PEXTMOD
     I                   (NDELT,SDATIM,NDAMON,EMFG, 
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
      POINTER   (PSSMNAME,SSMNAME),(PINNAME,INNAME)
      POINTER   (PUNIT,UNIT),(POLEV,OLEV),(PRAREA,RAREA)
      POINTER   (PDATIM,DATIM),(POSV,OSV)
      INTEGER    SSMNAME(5),INNAME(5),UNIT(4),DATIM(5),OSV(500)
      INTEGER    OLEV(2)
      REAL       RAREA
C
C     + + + LOCAL VARIABLES + + +   
      INTEGER    I,I0,I1,I2,OSVKND,OSVKST,OSVREC,BASEADDR,ADDR,
     $           IVAL(11),J,TBNO,TBSB,NVAL
      REAL       RVAL(1)
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
      I0   = 0
      I1   = 1
      I2   = 2
C
      OSVREC= 1
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
C
C     point the osv to its memory location
      P = LOC(OSVM(1,OSVKST))
      PDATIM = LOC(HEXTMOD%YR)    
      POSV   = LOC(HEXTMOD)
C     record the keys in opntab
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
C     flag pointer addresses
      BASEADDR = LOC(HEXTMOD%EXTMODNO)
      ADDR     = LOC(HEXTMOD%SPAFP)
      WRITE(99,*) 'address of SPAFP',1+ ((ADDR - BASEADDR)/4),BASEADDR/4
      ADDR     = LOC(HEXTMOD%IPTFP)
      WRITE(99,*) 'address of IPTFP',1+ ((ADDR - BASEADDR)/4)
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
        HEXTMOD%DELT  =  NDELT
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
C       initialize current values
        DO 40 I= 1, 20
          HEXTMOD%PTVAL(I)= -1.0E30
          HEXTMOD%MNVAL(I)= -1.0E30
 40     CONTINUE
      END IF
C
C     process table - type gen-info
C       SSM name, eg 'Pat's Sediment Model'
C       instance name, eg 'Site 5'
C       unit number of SSM output time series file
      TBNO= 1
      TBSB= 1
      NVAL= 11
      CALL ITABLE(TBNO,TBSB,NVAL,I1,
     M            IVAL)
C
      DO 110 J= 1,5
        SSMNAME(J)= IVAL(J)
 110  CONTINUE
      DO 120 J= 1,5
        INNAME(J)= IVAL(5+J)
 120  CONTINUE
      UNIT(1)= IVAL(11)
C
C     process table - type out-info
C       unit numbers for print and binary output reports, eng and metric
C       output levels for print and binary output reports
      TBNO= 1
      TBSB= 1
      NVAL= 6
      CALL ITABLE(TBNO,TBSB,NVAL,I1,
     M            IVAL)
C
      DO 130 J= 1,4
        UNIT(1+J)= IVAL(10+J)
 130  CONTINUE
      DO 140 J= 1,2
        OLEV(J) = IVAL(4+J)
 140  CONTINUE
C
C     check output files - if not open, open them with standard name
      DO 150 J= 2,3
        IF (UNIT(J) .GT. 0) THEN
          CALL HSCKFL
     I                (UNIT(J))
        END IF
 150  CONTINUE
C     check binary output files 
      DO 160 J= 4,5
        IF (UNIT(J) .GT. 0) THEN
          CALL HSCKFLX
     I                 (I0,UNIT(J))
        END IF
 160  CONTINUE
C
C     process table - type area-info
C       area associated with SSM instance
      TBNO= 1
      TBSB= 1
      NVAL= 1
      CALL RTABLE(TBNO,TBSB,NVAL,I1,
     M            RVAL)
C
      RAREA = RVAL(1)
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
      INTEGER     IVL,IVL1,N,IDELT,PIVLNO,DUM1,DUM2,DUM3,DUM4,
     #            DUM5,DUM6,I1,I12
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
      P      = LOC(OSVM(1,OSVKST))
      PDATIM = LOC(HEXTMOD%YR)    
C      
      IVL   = STIVL- 1
      IVL1  = STIVL
      IDELT = HEXTMOD%DELT
      PIVLNO= 0
      I1    = 1
      I12   = 12
C
      IF ( (STIVL .EQ. 1) .AND. (HEXTMOD%NPT.GT.0) ) THEN
C       put initial values of point-valued time series into inpad
C       transfer from input row to temporary array
        DO 20 N= 1,HEXTMOD%NPT
          IF (HEXTMOD%IPTFP(N) .GT. 0) THEN
            IF (HEXTMOD%OPTFP(N) .NE. HEXTMOD%IPTFP(N)) THEN
              HEXTMOD%PTVAL(N)= PAD(HEXTMOD%IPTFP(N)+IVL1)
            END IF
          END IF
 20     CONTINUE
C       transfer from temporary array to output row
        DO 30 N= 1,HEXTMOD%NPT
          IF ( (HEXTMOD%OPTFP(N) .GT. 0) .AND. 
     1         (HEXTMOD%IPTFP(N) .GT. 0) ) THEN
            IF (HEXTMOD%OPTFP(N) .NE. HEXTMOD%IPTFP(N)) THEN
              PAD(HEXTMOD%OPTFP(N)+IVL1)= HEXTMOD%PTVAL(N)
            END IF
          END IF
 30     CONTINUE
      END IF
C
C     time loop
      DO 130 IVL= STIVL,WIDTH+ STIVL- 1
        IVL1       = IVL1+ 1
        HEXTMOD%SPIVL= HEXTMOD%SPIVL+ 1
C
C       increment date/time
        CALL ADDTIM (IDELT,HEXTMOD%NDAY,I1,I12,
     M               DATIM,PIVLNO,
     O               HEXTMOD%NDAYS,DUM1,DUM2,DUM3,DUM4,DUM5,DUM6)
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
        IF (HEXTMOD%NPT.GT.0) THEN
C         copy point-valued time series to osv
          DO 40 N= 1,HEXTMOD%NPT
            IF (HEXTMOD%IPTFP(N) .GT. 0) THEN
              HEXTMOD%PTVAL(N)= PAD(HEXTMOD%IPTFP(N)+IVL1)
            END IF
 40       CONTINUE
        END IF
C
        IF (HEXTMOD%NMN.GT.0) THEN
C         copy mean-valued time series to osv
          DO 50 N= 1,HEXTMOD%NMN
            IF (HEXTMOD%IMNFP(N) .GT. 0) THEN
              HEXTMOD%MNVAL(N)= PAD(HEXTMOD%IMNFP(N)+IVL1)
            END IF
 50       CONTINUE
        END IF
C
        IF (HEXTMOD%NPT.GT.0) THEN
C         copy point-valued time series from osv to output
          DO 60 N= 1,HEXTMOD%NPT
            IF ( (HEXTMOD%OPTFP(N) .GT. 0) .AND. 
     1           (HEXTMOD%IPTFP(N) .GT. 0) ) THEN
              IF (HEXTMOD%OPTFP(N) .NE. HEXTMOD%IPTFP(N)) THEN
                PAD(HEXTMOD%OPTFP(N)+IVL1)= HEXTMOD%PTVAL(N)
              END IF
            END IF
 60       CONTINUE
        END IF
C
        IF (HEXTMOD%NMN.GT.0) THEN
C         copy mean-valued time series from osv to output
          DO 70 N= 1,HEXTMOD%NMN
            IF ( (HEXTMOD%OMNFP(N) .GT. 0) .AND. 
     1           (HEXTMOD%IMNFP(N) .GT. 0) ) THEN
              IF (HEXTMOD%OMNFP(N) .NE. HEXTMOD%IMNFP(N)) THEN
                PAD(HEXTMOD%OMNFP(N)+IVL1)= HEXTMOD%MNVAL(N)
              END IF
            END IF
 70       CONTINUE
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
      END MODULE HMEXTMOD  
               