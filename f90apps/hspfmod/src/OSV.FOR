      MODULE OSV

        IMPLICIT NONE

        PUBLIC :: OSV_GET, OSV_PUT
        PRIVATE

        INTEGER, PARAMETER     :: POSVWD = 500
        TYPE OSVS
          INTEGER              :: IVAL(POSVWD)   ! instruction
          INTEGER (KIND=2)     :: ID             ! record number
          TYPE (OSVS), POINTER :: NEXT          ! pointer to next record
        END TYPE OSVS

        TYPE OSVF
          TYPE (OSVS), POINTER  :: FIRST
        END TYPE OSVF
        TYPE (OSVF) :: OSVX             ! pointer to start of recs

      CONTAINS

        TYPE (OSVS) FUNCTION OSV_POS (KEY) RESULT (OP)
          INTEGER,                    INTENT(IN)  :: KEY
          POINTER                                 :: OP

          IF (.NOT. ASSOCIATED(OSVX%FIRST)) THEN
            ! WRITE(*,*) 'OSV_POS: allocate first record'
            ALLOCATE (OSVX%FIRST)
            OSVX%FIRST%ID = 0
          END IF

          OP => OSVX%FIRST
          DO
            IF (KEY == OP%ID) THEN
              ! WRITE(*,*) 'OSV_POS: found ',OP%ID,KEY
              EXIT
            ELSE IF (.NOT. ASSOCIATED(OP%NEXT)) THEN
              ALLOCATE (OP%NEXT)
              ! WRITE(*,*) 'OSV_POS: allocate record',KEY
              OP => OP%NEXT
              OP%ID = KEY
              EXIT
            ELSE
              OP => OP%NEXT
            END IF
          END DO

        END FUNCTION OSV_POS


        SUBROUTINE OSV_PUT (KEYST,KEYND,MAXOSV,OSV)
          INTEGER, INTENT(IN)            :: KEYST,KEYND,MAXOSV
          INTEGER, INTENT(IN)            :: OSV(MAXOSV)

          TYPE (OSVS), POINTER           :: OP
          INTEGER                        :: F,L,K

          OP => OSV_POS(KEYST)
          F = 1
          L = POSVWD
          K = KEYST

          DO
            ! WRITE(*,*) 'OSV_PUT: move',F,L,K
            OP%IVAL = OSV(F:L)
            IF (OP%ID == KEYND) THEN
              EXIT
            ELSE
              F = F+ POSVWD
              L = L+ POSVWD
              IF (L > MAXOSV) THEN
                L = MAXOSV
              END IF
              K = K+ 1
              IF (.NOT. ASSOCIATED(OP%NEXT)) THEN
                ALLOCATE (OP%NEXT)
                OP%NEXT%ID = K
                ! WRITE(*,*) 'OSV_PUT: allocate record',K
              END IF
              OP => OP%NEXT
            END IF
          END DO

        END SUBROUTINE OSV_PUT

        SUBROUTINE OSV_GET (KEYST,KEYND,MAXOSV,OSV)
          INTEGER, INTENT(IN)            :: KEYST,KEYND,MAXOSV
          INTEGER, INTENT(OUT)           :: OSV(MAXOSV)

          TYPE (OSVS), POINTER           :: OP
          INTEGER                        :: F,L

          OP => OSV_POS(KEYST)
          F = 1
          L = POSVWD

          DO
            ! WRITE(*,*) 'OSV_GET: move',F,L,OP%ID
            OSV(F:L) = OP%IVAL
            IF (OP%ID == KEYND) THEN
              EXIT
            ELSE
              F = F+ POSVWD
              L = L+ POSVWD
              IF (L > MAXOSV) THEN
                L = MAXOSV
              END IF
              OP => OP%NEXT
            END IF
          END DO

        END SUBROUTINE OSV_GET

      END MODULE OSV
