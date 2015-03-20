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
      USE HMCOPY, ONLY : PCOPY, HTCOPY
C
C     + + + COMMON BLOCKS  + + +
      INCLUDE   'crin2.inc'
C
C     + + + PARAMETERS + + +

C
C     + + + LOCAL VARIABLES + + +
      INTEGER  OMCODE
      LOGICAL  OK
      TYPE (HMODULE_TABLE_DEFINITION)  COPY_TABLEDEF
      TYPE (HMODULE_TIMSER_DEFINITION) COPY_TIMSERDEF
      TYPE (HTCOPY) COPY_OPERS(10), COPY_OPER 
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
C     parameter tables - think about this API
      OK      = GETMODULETABLEDEF(OMCODE)
      IF (OK) THEN
        COPY_TABLEDEF= TABLEDEFS(OMCODE)
        WRITE(*,*) COPY_TABLEDEF%INITIALIZED, COPY_TABLEDEF%NAME
C       timeseries tables
        CALL GETMODTSDEF(OMCODE)
        COPY_TIMSERDEF= TIMSERDEFS(OMCODE)
        WRITE(*,*) COPY_TIMSERDEF%INITIALIZED, COPY_TIMSERDEF%NAME
      END IF
C
      IF (OK) THEN
C       set global parameters

      END IF
C
      IF (OK) THEN            
C       initialize COPY parameters (not using UCI, so no call to PCOPY)
        COPY_OPER%COPYNO = 1
        COPY_OPER%NPT    = 1
        COPY_OPER%NMN    = 2
        COPY_OPER%MESSU  = 6
        COPY_OPER%DELT   = 60.0
        COPY_OPER%YR     = 2015
        COPY_OPER%MON    = 1
        COPY_OPER%DAY    = 1
        COPY_OPER%HR     = 1
        COPY_OPER%MIN    = 1
        COPY_OPER%NDAY   = [31,28,31,30,31,30,31,31,30,31,30,31]
        COPY_OPER%NDAYS  = 31
        
        COPY_OPER%UUNITS = 1
        
        OSVKEY   = 0
C
      END IF
C
C     timeseries?

C     need an operations supervisor
      
C
      STOP
      END