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
      CONTAINS
C
C
C
      SUBROUTINE TABLE_SET
     I                    (NAME,DIM,
     I                     GROUP1,GROUP2,GROUP3,GROUP4,
     I                     NFLDS,
     I                     HEADI1,HEADI2,HEADI3,HEADI4,
     I                     WRFMT,
     I                     HEADO1,HEADO2,HEADO3,HEADO4,
     I                     HELP1,HELP2,HELP3,HELP4,HELP5,
     I                     HELP6,HELP7,HELP8,HELP9,HELP10,
     M                     TABLE)
C
      CHARACTER*(*) NAME
      INTEGER       DIM,GROUP1,GROUP2,GROUP3,GROUP4,NFLDS
      CHARACTER*80  HEADI1,HEADI2,HEADI3,HEADI4
      CHARACTER*120 WRFMT,HEADO1,HEADO2,HEADO3,HEADO4,
     $              HELP1,HELP2,HELP3,HELP4,HELP5,
     $              HELP6,HELP7,HELP8,HELP9,HELP10
      TYPE (HMODULE_TABLE) TABLE
C
      TABLE%NAME    = NAME
      TABLE%DIM     = DIM
      TABLE%GROUP(1)= GROUP1
      TABLE%GROUP(2)= GROUP2
      TABLE%GROUP(3)= GROUP3
      TABLE%GROUP(4)= GROUP4
      TABLE%SFLD    = 2
      TABLE%NFLDS   = NFLDS
C
      TABLE%HEAD_IN(1) = HEADI1
      TABLE%HEAD_IN(2) = HEADI2
      TABLE%HEAD_IN(3) = HEADI3
      TABLE%HEAD_IN(4) = HEADI4
C
      TABLE%WRFMT = WRFMT
C
      TABLE%HEAD_OUT(1) = HEADO1
      TABLE%HEAD_OUT(2) = HEADO2
      TABLE%HEAD_OUT(3) = HEADO3
      TABLE%HEAD_OUT(4) = HEADO4
C
      TABLE%HELP(1) = HELP1
      TABLE%HELP(2) = HELP2
      TABLE%HELP(3) = HELP3
      TABLE%HELP(4) = HELP4
      TABLE%HELP(5) = HELP5
      TABLE%HELP(6) = HELP6
      TABLE%HELP(7) = HELP7
      TABLE%HELP(8) = HELP8
      TABLE%HELP(9) = HELP9
      TABLE%HELP(10) = HELP10
C
      ALLOCATE (TABLE%FIELD(NFLDS))
C
      RETURN
      END SUBROUTINE     
C
C
C
      SUBROUTINE TABLE_FIELD_SET
     I                          (FLEN,SCOL,IMIN,IMAX,IDEF,
     I                           FTYP,
     I                           HELP,
     M                           FIELD)

      INTEGER       FLEN,SCOL,IMIN,IMAX,IDEF
      CHARACTER*4   FTYP
      CHARACTER*120 HELP
      TYPE (HMODULE_FIELD) FIELD
C
      FIELD%FLEN = FLEN
      FIELD%SCOL = SCOL
      FIELD%IMIN = IMIN
      FIELD%IMAX = IMAX
      FIELD%IDEF = IDEF
      FIELD%FTYP = FTYP
      FIELD%HELP = HELP
C
      RETURN
      END SUBROUTINE     
C                             
      END MODULE