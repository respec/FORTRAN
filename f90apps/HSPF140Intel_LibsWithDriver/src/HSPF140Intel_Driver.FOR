C
C
C
      PROGRAM   HSPF140Intel_Driver
C
C     + + + PURPOSE + + +
C     Driver to call new operating and run interp modules
C
C     + + + MODULES + + +
      USE HMRINTABLE
      USE HMRINTS
      USE HMTIMSERDEF
      USE HIOOSV, ONLY : INITOSVQ
      USE HMMSG, ONLY : INITMSGQ
      USE HMCOPY, ONLY : PCOPY
C
C     + + + COMMON BLOCKS  + + +
      INCLUDE   'crin2.inc'
C
C     + + + PARAMETERS + + +

C
C     + + + LOCAL VARIABLES + + +
      INTEGER  OMCODE, NDELT, SDATIM(5), NDAMON(12), EMFG, OSVKEY
      LOGICAL  OK
      TYPE (HMODULE_TABLE_DEFINITION)  TABLEDEF
      TYPE (HMODULE_TIMSER_DEFINITION) TIMSERDEF
C
C     + + + FUNCTIONS + + +

C
C     + + + INTRINSICS + + +

C
C     + + + EXTERNALS + + +

C
C     + + + INPUT FORMATS + + +

C
C     + + + END SPECIFICATIONS + + +
C
C     initialize osv
      CALL INITOSVQ
C     initialize error/warning message module
      CALL INITMSGQ      
C
C     testing copy module
      OMCODE  = 4 
C     parameter tables
      OK      = GETMODULETABLEDEF(OMCODE)
      IF (OK) THEN
        TABLEDEF= TABLEDEFS(OMCODE)
        WRITE(*,*) TABLEDEF%INITIALIZED, TABLEDEF%NAME
C       timeseries tables
        CALL GETMODTSDEF(OMCODE)
        TIMSERDEF= TIMSERDEFS(OMCODE)
        WRITE(*,*) TIMSERDEF%INITIALIZED, TIMSERDEF%NAME
C
        NDELT    = 60
        SDATIM(1)= 2015
        SDATIM(2)= 1
        SDATIM(3)= 1
        NDAMON(1)= 31
        EMFG     = 1
        OSVKEY   = 0
C
        OPNO     = 1
        FILE(1)  = 6     
        OPTNO    = 1
        OUTLEV   = 3
C
C       need uci records
C        
        CALL PCOPY
     I            (NDELT,SDATIM,NDAMON,EMFG,TABLEDEF, 
     M             OSVKEY)
C
        WRITE(*,*) OSVKEY
      END IF
C
      STOP
      END