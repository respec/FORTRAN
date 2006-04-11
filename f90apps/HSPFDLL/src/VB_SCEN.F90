
      SUBROUTINE F90_SPIPH (HIN,HOUT)
        ! HIN is handle to pipe sending input to the fort applic
        ! HOUT is handle to pipe sending output from the fort applic
        USE SCENMOD, ONLY : M_SPIPH
        dll_export F90_SPIPH

        INTEGER,  INTENT(IN) :: HIN, HOUT

        CALL M_SPIPH(HIN,HOUT)

      END SUBROUTINE F90_SPIPH

      SUBROUTINE F90_ACTSCN (MKFILS,WDMFL,MSGFL,RETCOD,CSCEN)
!       activate scenario
        USE SCENMOD, ONLY : M_ACTSCN
        DLL_EXPORT F90_ACTSCN

        INTEGER,         INTENT(IN)  :: MKFILS
        CHARACTER(LEN=*),INTENT(IN)  :: CSCEN
        INTEGER,         INTENT(IN)  :: WDMFL(4),MSGFL
        INTEGER,         INTENT(OUT) :: RETCOD

        CALL M_ACTSCN (MKFILS,CSCEN,WDMFL,MSGFL,RETCOD)
        write(99,*) 'wdmfl',wdmfl(1),wdmfl(2),wdmfl(3),wdmfl(4)

      END SUBROUTINE F90_ACTSCN

      !scnmod
      SUBROUTINE   F90_SIMSCN (RETCOD)
!       simulate scenario
        USE SCENMOD, ONLY : M_SIMSCN
        DLL_EXPORT F90_SIMSCN

        INTEGER, INTENT(OUT)  :: RETCOD

        CALL M_SIMSCN(RETCOD)

      END SUBROUTINE F90_SIMSCN

