C
C
C
      SUBROUTINE   PGENER
     I                    (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     Process the input for the gener module
C
C     + + + HISTORY + + +
C     2009/04/22 JLK - use OSV module version
C
C     + + + MODULES + + +
      USE       HIOOSV 
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION GENER1 + + +
      INCLUDE   'cgene.inc'
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,I1,I2,I3,I4,IVAL(1),OSVKND,OSVKST,OSVREC
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL  ITABLE,RTABLE,DAYMNH
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' PROCESSING INPUT FOR TRANSGENERATION OPERATION NO.',
     $          I5,'  TIME INTERVAL=',I5,' MINS')
 2010 FORMAT (/,' FINISHED PROCESSING TRANSGENERATION OPERATION NO.',I5,
     $        /,' ',132('+'))
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize the entire osv
      DO 10 I=1,MAXOSV
        IPAD(I)= -999
 10   CONTINUE
C
      I1   = 1
      I2   = 2
      I3   = 3
      I4   = 4
      MESSU= FILE(1)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPTNO, NDELT
      END IF
C
C     place information into osv
      GENRNO= OPTNO
      MESSU= FILE(1)
      MSGFL= FILE(15)
      DELT= NDELT
      DO 13 I= 1, 12
        NDAY(I)= NDAMON(I)
 13   CONTINUE
      DO 16 I= 1, 5
        DATIM(I)= SDATIM(I)
 16   CONTINUE
C     following value is assigned so that subroutine addtim will
C     work ok
      NDAYS= DAYMNH (YR,MON,NDAY)
      SPIVL=  0
      SPOPNO= OPNO
      UUNITS= EMFG
C
      SUM= 0.0
C     get the operation code - table-type opcode
      CALL ITABLE
     I             (I1,I1,I1,I1,
     M              IVAL)
      OPCODE= IVAL(1)
      IF (OPCODE.EQ.8) THEN
C       operation is power series - get number of terms
        CALL ITABLE
     I               (I2,I1,I1,I1,
     M                IVAL)
        NTERMS= IVAL(1)
C       get coefficients
        CALL RTABLE
     I               (I3,I1,NTERMS,I1,
     M                K)
      END IF
      IF ( (OPCODE .GE. 9) .AND. (OPCODE .LE. 11) ) THEN
C       constant needed
        I= 1
      ELSE IF ( (OPCODE .GE. 24) .AND. (OPCODE .LE. 26) ) THEN
C       constant needed
        I= 1
      ELSE
C       no constant needed
        I= 0
      END IF
C
      IF (I .EQ. 1) THEN
C       get constant
        CALL RTABLE
     I               (I4,I1,I1,I1,
     M                K)
      END IF
C
      OSVREC= 1
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV
     I            (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2010) OPTNO
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMUTSN
     I                    (NDELT,SDATIM,NDAMON,EMFG,MAXOSV,
     M                     OSVKEY)
C
C     + + + PURPOSE + + +
C     process the input for the mutsin module
C
C     + + + HISTORY + + +
C     2009/04/22 JLK - use OSV module version
C
C     + + + MODULES + + +
      USE       HIOOSV 
      USE HMMSG, ONLY : OMSTI,OMSG
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NDELT,SDATIM(5),NDAMON(12),EMFG,MAXOSV,OSVKEY
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NDELT  - simulation time interval in minutes
C     SDATIM - starting date/time
C     NDAMON - no. of days in each month of calendar year
C     EMFG   - english/metric units flag (english-1,metric-2)
C     MAXOSV - maximum size of osv
C     OSVKEY - last osv file record written
C
C     + + + COMMON BLOCKS- SCRTCH, VERSION MUTSIN1 + + +
      INCLUDE   'cmuts.inc'
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   OSVKST,OSVKND,OSVREC,I,I1,I4,SCLU,SGRP
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMNH
C
C     + + + EXTERNALS + + +
      EXTERNAL  DAYMNH,ITABLE,HSCKFL 
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (/,' ',132('+'),
     $        /,' ','PROCESSING MUTSIN OPERATION NO. ',I4)
 2030 FORMAT (/,' FINISHED PROCESSING MUTSIN OPERATION NO. ',I4,
     $        /,' ',132('+'))
C
C     + + + END SPECIFICATIONS + + +
C
      I1   = 1
      SCLU = 371
      MESSU= FILE(1)
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2000) OPTNO
      END IF
C
      IF (RESMFG.EQ.1) THEN
C       read the osv from osvfl
C       - not implemented in this release of hspf
      ELSE
C       initialize the entire osv area
        DO 30 I= 1,MAXOSV
          IPAD(I)= -999
 30     CONTINUE
      END IF
C
      MUTNO = OPTNO
      MESSU = FILE(1)
      MSGFL = FILE(15)
C
      DELT= NDELT
      DO 10 I= 1,5
        DATIM(I)= SDATIM(I)
 10   CONTINUE
      DO 15 I= 1,12
        NDAY(I)= NDAMON(I)
 15   CONTINUE
C
      STFG  = 1
      UUNITS= EMFG
      OSVREC= 1
C     following values are assigned so that subroutine addtim will
C     work ok
      PIVL  = 1
      PIVLNO= 0
      NDAYS = DAYMNH(YR,MON,NDAY)
C
C     get info for operation -- table-type mutsinfo
      I4= 5
      CALL ITABLE
     I             (I1,I1,I4,I1,
     M              MUTINF)
C
C     check output file - if not open,
C     then open it with a standard name
      IF (MUTFL .GT. 0) THEN
        CALL HSCKFL
     I              (MUTFL)
      END IF
C
      NCURV= NPT+ NMN
      IF (NCURV.LT.1.OR.NCURV.GT.20) THEN
C       total no. of curves to be plotted is not within valid range
        CALL OMSTI (NCURV)
        SGRP= 21
        CALL OMSG (MESSU,MSGFL,SCLU,SGRP,
     M             ECOUNT)
      END IF
C
      IF (MISSFG.EQ.2) THEN
        FILVAL= -1.0E30
      ELSE
        FILVAL= 0.0
      END IF
C
C     write the osv to disc and record the keys in opntab
      OSVKST= OSVKEY+ 1
      OSVKND= OSVKEY+ OSVREC
      CALL PUTOSV
     I            (OSVKST,OSVKND,MAXOSV,IPAD)
      OPNTAB(7,OPNO)= OSVKST
      OPNTAB(8,OPNO)= OSVKND
      OSVKEY        = OSVKND
C
      IF (OUTLEV.GT.0) THEN
        WRITE (MESSU,2030) OPTNO
      END IF
C
      RETURN
      END
