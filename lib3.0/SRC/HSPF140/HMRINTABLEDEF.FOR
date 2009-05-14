C
C
C
      MODULE HMTABLEDEF
C
C     + + + PURPOSE + + +
C     module containing module table portion of run interpreter 
C
C     + + + HISTORY + + +
C     2009/05/13 JLK - initial implementation
C
C     + + + MODULE VARIABLES + + +
      IMPLICIT NONE
C
      TYPE HMODULE_FIELD
        SEQUENCE
        INTEGER       FLEN,SCOL,IMIN,IMAX,IDEF
        REAL          RMIN,RMAX,RDEF,ADD,MULT
        CHARACTER*4   FTYP
        CHARACTER*120 HELP
      END TYPE HMODULE_FIELD
C      
      TYPE HMODULE_TABLE
        SEQUENCE
        CHARACTER*12  NAME
        INTEGER       DIM,GROUP(4),SFLD,NFLDS
        CHARACTER*80  HEAD_IN(4)
        CHARACTER*120 WRFMT,HEAD_OUT(4),HELP(10)
        TYPE (HMODULE_FIELD),
     $       ALLOCATABLE, DIMENSION(:) :: FIELD
      END TYPE HMODULE_TABLE
C      
      TYPE HMODULE_TABLE_DEFINITION
        SEQUENCE
        LOGICAL     :: INITIALIZED = .FALSE.
        CHARACTER*8 NAME
        INTEGER     CLUSTER
        TYPE (HMODULE_TABLE), 
     $       ALLOCATABLE, DIMENSION(:)  :: TABLE
      END TYPE HMODULE_TABLE_DEFINITION
C                             
      END MODULE