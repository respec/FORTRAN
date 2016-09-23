C
C
C
      PROGRAM   HSPF140Intel_Driver
C
C     + + + PURPOSE + + +
C     Driver to call new operating and run interp modules
C
C     + + + MODULES + + +
      USE HIOOSV, ONLY : INITOSVQ 
      USE HMMSG, ONLY  : INITMSGQ
      USE HMCOPY, ONLY : COPY
C
C     + + + COMMON BLOCKS  + + +
      INCLUDE   'crin2.inc'
      INCLUDE   'cmpad.inc'
      INCLUDE   'cmosv.inc'
C
C     + + + PARAMETERS + + +

C
C     + + + LOCAL VARIABLES + + +
      INTEGER  STIVL,WIDTH,OSVKST,OSVKND,OSVKEY
C
C     + + + FUNCTIONS + + +

C
C     + + + INTRINSICS + + +

C
C     + + + EXTERNALS + + +
      EXTERNAL  COPY_SET

C
C     + + + INPUT FORMATS + + +

C
C     + + + END SPECIFICATIONS + + +
C
C     initialize osv
      CALL INITOSVQ
C     initialize error/warning message module
      CALL INITMSGQ      

C     set global parameters
      STIVL = 1
      WIDTH = 100
      OSVKEY= 0

C     initialize COPY parameters (not using UCI, so no call to PCOPY)

C     size of OSV for copy
      OSVREC= 1
      
      DO 10 OPNO = 1, 3
C       COPY 1, 2, 3
        OSVKST        = OSVKEY+ 1
        CALL COPY_SET (OSVKST,OPNO)

        OSVKND        = OSVKEY+ OSVREC
        OPNTAB(7,OPNO)= OSVKST
        OPNTAB(8,OPNO)= OSVKND
        OSVKEY        = OSVKND
 10   CONTINUE

C     build dummy input timeseries
      DO 15 IVL = 1, 100
        PAD(IVL) = IVL
        PAD(IVL+100) = IVL*2
 15   CONTINUE        

C     need an operations supervisor to loop through operations
    
      DO 20 OPNO = 1, 3
        CALL COPY (STIVL,WIDTH,OPNTAB(7,OPNO))
 20   CONTINUE            

C     output timeseries
      DO 25 IVL = 1, 100
        WRITE(*,'(I4,8F10.0)') 
     1     IVL,PAD(IVL),PAD(100+IVL),
     2         PAD(200+IVL),PAD(300+IVL),PAD(400+IVL),PAD(500+IVL)
 25   CONTINUE        

      STOP
      END PROGRAM
C
C
C           
      SUBROUTINE COPY_SET
     I                   (OSVKST,OPNO)
C      
C     + + + PURPOSE + + +
C     Set COPY parameters
C      
C     + + + MODULES + + +
      USE HIOOSV, ONLY : OSVM
      USE HMCOPY, ONLY : PCOPY, HTCOPY 
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OSVKST, OPNO
C
C     + + + MODULE VARIABLES + + +
      POINTER   (P,HCOPY)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER  OMCODE
      TYPE (HTCOPY) HCOPY
C
C     + + + END SPECIFICATIONS + + +
C
C     testing copy module
      OMCODE= 4 
C
C     point the osv to its memory location
      P = LOC(OSVM(1,OSVKST))
C     set the parameter values      
      HCOPY%COPYNO= OPNO
      HCOPY%NPT   = 1
      HCOPY%NMN   = 1
      HCOPY%MESSU = 6
      HCOPY%DELT  = 60.0
      HCOPY%YR    = 2015
      HCOPY%MON   = 1
      HCOPY%DAY   = 1
      HCOPY%HR    = 1
      HCOPY%MIN   = 1
      HCOPY%NDAY  = [31,28,31,30,31,30,31,31,30,31,30,31]
      HCOPY%NDAYS = 31        
      HCOPY%UUNITS= 1
      
C     dummy flag pointers
      HCOPY%IPTFP(1) = ((OPNO-1)*200) + 1
      HCOPY%IMNFP(1) = ((OPNO-1)*200) + 101
      HCOPY%OPTFP(1) = ((OPNO-1)*200) + 201
      HCOPY%OMNFP(1) = ((OPNO-1)*200) + 301

C     special action details

      
      END SUBROUTINE COPY_SET
      