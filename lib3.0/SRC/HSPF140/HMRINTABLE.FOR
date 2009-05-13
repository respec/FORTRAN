C
C
C
      MODULE HMRINTABLE
C
C     + + + PURPOSE + + +
C     module containing module table portion of run interpreter 
C
C     + + + HISTORY + + +
C     2009/05/13 JLK - initial implementation
C
C     + + + MODULE VARIABLES + + +
      USE HMTABLEDEF
      IMPLICIT NONE
C
C     need one for each operating module type
      TYPE (HMODULE_TABLE_DEFINITION), SAVE :: TABLEDEFS(20)
C   
      CONTAINS   
C
C
C 
      LOGICAL FUNCTION GETMODULETABLEDEF
     I                                  (OMCODE)
C
      USE HMCOPY, ONLY : GETTABLEDEF_COPY
      USE HMEXTMOD, ONLY : GETTABLEDEF_EXTMOD
C      
      INTEGER OMCODE
C
      IF (.NOT. TABLEDEFS(OMCODE)%INITIALIZED) THEN
        IF (OMCODE .EQ. 4) THEN
          CALL GETTABLEDEF_COPY(TABLEDEFS(OMCODE))
          GETMODULETABLEDEF = .TRUE.
        ELSEIF (OMCODE .EQ. 12) THEN
          CALL GETTABLEDEF_EXTMOD(TABLEDEFS(OMCODE))
          GETMODULETABLEDEF = .TRUE.
        ELSE
          GETMODULETABLEDEF = .FALSE.
        END IF      
      END IF    
C
      RETURN
      END FUNCTION
C
      END MODULE