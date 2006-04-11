      MODULE TSINS

        IMPLICIT NONE

        PUBLIC :: TSINS_GET, TSINS_PUT
        PRIVATE

        TYPE TSIN
          INTEGER              :: INST(200)      ! instruction
          INTEGER (KIND=2)     :: ID             ! record number
          TYPE (TSIN), POINTER :: NEXT           ! pointer to next record
        END TYPE TSIN

        TYPE TSINF
          TYPE (TSIN), POINTER :: FIRST
        END TYPE TSINF
        TYPE (TSINF) :: TSINX             ! pointer to start of recs

      CONTAINS

        TYPE (TSIN) FUNCTION TSINS_POS (KEY) RESULT (TP)
          INTEGER,                    INTENT(IN)  :: KEY
          POINTER                                 :: TP

          IF (.NOT. ASSOCIATED(TSINX%FIRST)) THEN
            ! WRITE(*,*) 'TSINS_POS: allocate first record'
            ALLOCATE (TSINX%FIRST)
            TSINX%FIRST%ID = 0
          END IF

          TP => TSINX%FIRST
          DO
            IF (KEY == TP%ID) THEN
              ! WRITE(*,*) 'TSINS_POS: found ',TP%ID,KEY
              EXIT
            ELSE IF (.NOT. ASSOCIATED(TP%NEXT)) THEN
              ALLOCATE (TP%NEXT)
              ! WRITE(*,*) 'TSINS_POS: allocate record',KEY
              TP => TP%NEXT
              TP%ID = KEY
              EXIT
            ELSE
              TP => TP%NEXT
            END IF
          END DO

        END FUNCTION TSINS_POS


        SUBROUTINE TSINS_PUT (ILEN,INSTR,KEY)
          INTEGER, INTENT(IN)            :: ILEN
          INTEGER, INTENT(IN)            :: INSTR(ILEN)
          INTEGER, INTENT(IN)            :: KEY

          TYPE (TSIN), POINTER           :: TP

          TP => TSINS_POS(KEY)

          TP%INST(1:ILEN) = INSTR

        END SUBROUTINE TSINS_PUT

        SUBROUTINE TSINS_GET (ILEN,KEY,INSTR)
          INTEGER, INTENT(IN)            :: ILEN
          INTEGER, INTENT(IN)            :: KEY
          INTEGER, INTENT(OUT)           :: INSTR(ILEN)

          TYPE (TSIN), POINTER           :: TP

          TP => TSINS_POS(KEY)

          INSTR = TP%INST(1:ILEN)

        END SUBROUTINE TSINS_GET

      END MODULE TSINS
