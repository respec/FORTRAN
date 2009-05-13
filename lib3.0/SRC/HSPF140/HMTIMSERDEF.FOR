C
C
C
      MODULE HMTIMSERDEF
C
C     + + + PURPOSE + + +
C     module containing timeseries portion of run interpreter 
C
C     + + + HISTORY + + +
C     2009/05/08 JLK - initial implementation
C
C     + + + MODULE VARIABLES + + +
      IMPLICIT NONE
C
      TYPE HMODULE_TIMSER_MEMBER
        SEQUENCE
        CHARACTER*8  NAME
        INTEGER      MDIM1,MAXSB1,MAXSB2,MKIND,SPTRN,MSECT,
     #               MIO,OSVBAS,OSVOFF
        REAL         LTVAL(2,2,2)
        CHARACTER*40 DEFINITION
      END TYPE HMODULE_TIMSER_MEMBER
C      
      TYPE HMODULE_TIMSER_GROUP
        SEQUENCE
        CHARACTER*8 NAME
        INTEGER     OFFSET
        TYPE (HMODULE_TIMSER_MEMBER),
     $       ALLOCATABLE, DIMENSION(:) :: MEMBER
      END TYPE HMODULE_TIMSER_GROUP
C
      TYPE HMODULE_TIMSER_DEFINITION
        SEQUENCE
        LOGICAL     :: INITIALIZED = .FALSE.
        CHARACTER*8 NAME
        INTEGER     CLUSTER
        TYPE (HMODULE_TIMSER_GROUP), 
     $       ALLOCATABLE, DIMENSION(:)  :: GROUP 
      END TYPE HMODULE_TIMSER_DEFINITION
C
      CONTAINS
C
C
C
      SUBROUTINE TIMSER_MEMBER_SET
     I                     (NAME,MDIM1,MAXSB1,MAXSB2,
     I                      MKIND,SPTRN,MSECT,
     I                      MIO,OSVBAS,OSVOFF,DEFINITION,
     M                      MEMBER)
C
      CHARACTER*(*) NAME
      INTEGER       MDIM1,MAXSB1,MAXSB2,MKIND,SPTRN,MSECT,
     #              MIO,OSVBAS,OSVOFF
      CHARACTER*(*) DEFINITION
      TYPE (HMODULE_TIMSER_MEMBER) MEMBER
C
      MEMBER%NAME  = NAME
      MEMBER%MDIM1 = MDIM1      
      MEMBER%MAXSB1= MAXSB1
      MEMBER%MAXSB2= MAXSB2
      MEMBER%MKIND = MKIND
      MEMBER%SPTRN = SPTRN
      MEMBER%MSECT = MSECT
      MEMBER%MIO   = MIO
      MEMBER%OSVBAS= OSVBAS
      MEMBER%OSVOFF= OSVOFF
      MEMBER%DEFINITION= DEFINITION
C
      RETURN
      END SUBROUTINE     
C
C
C  
      SUBROUTINE TIMSER_MEMBER_LTVAL_INIT
     O                                  (ALTVAL)
C
      REAL ALTVAL(2,2,2)
C
      ALTVAL(1,1,1)= 0
      ALTVAL(2,1,1)= 1
      ALTVAL(1,2,1)= 0
      ALTVAL(2,2,1)= 1
      ALTVAL(1,1,2)= 0
      ALTVAL(2,1,2)= 1
      ALTVAL(1,2,2)= 0
      ALTVAL(2,2,2)= 1
C
      END SUBROUTINE
C                             
      END MODULE